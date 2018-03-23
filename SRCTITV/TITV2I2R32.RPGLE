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
023100121012     C*
023200121012     C* Verifico immediatamente data/ora stato corrente => se "futura" skippo completamente
023300121012     C                   IF        stbDAS*10000+stbORS >
023400121012     C                             %int((%dec(%date():*ISO)*1000000+
023500121012     C                             %dec(%time()))/100)
023600121012     C                   ELSE
023700110929     C*
023800111103     C* Come primissima cosa verifico rottura d codice su key spedizione
023900110929     C                   IF        stbAAS = sAAS AND
024000110929     C                             stbLNP = sLNP AND
024100110929     C                             stbNRS = sNRS AND
024200110929     C                             stbNSP = sNSP
024300110929     C                   ELSE
024400110929     C                   SETOFF                                       40
024500110929     C                   EVAL      sAAS = stbAAS
024600110929     C                   EVAL      sLNP = stbLNP
024700110929     C                   EVAL      sNRS = stbNRS
024800110929     C                   EVAL      sNSP = stbNSP
024900110929     C                   ENDIF
025000031014     C*
025100030925     C* Inizializzo la DS d output con i valori di default della dichiarazione della struttura dati
025200110103     C                   RESET                   DS_Cli_Status
025300080618     C*
025400110927     C                   SETON                                        50
025500031103     C*
025600031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
025700031103     C* deve restituire al cliente nel file tradotto
025800111215     C                   IF        stbFTR = *blanks                             * ridondante
025900111215     C*
026000111215     C* Verifico (subito) se spedizione in giacenza
026100111215     C                   exsr      rtvgiac
026200110103     C*
026300111031     C* Gestisco solamente alcuni stati
026400110928     C                   IF        (stbTIS= '1' OR
026500111103     C                             (stbTIS= '9' AND stbPRS= '1'))
026600110103     C*
026700110103     C                   SETOFF                                       50
026800031103     C*
026900031103     C* Innanzitutto chaino la bolla direttamente su TITAS
027000031103     C     KEYtas30P     CHAIN     titas30c
027100031103     C                   IF        %found(titas30c)
027200110103     C*
027300110103     C                   clear                   wCAS
027400110927     C                   clear                   wDDP
027500110927     C                   clear                   wTPI
027600110929     C                   clear                   wDCR
027700110103     C*
027800110103     C* Quindi verifico subito esistenza contrassegno
027900110103     C     KEYcsb03C     CHAIN     tncsb03l
028000110103     C                   IF        %found(tncsb03l)
028100110103     C                   eval      wCAS = csbCAS
028200110927     C                   eval      wDDP = csbDDP
028300110927     C                   eval      wTPI = csbTPI
028400110103     C                   ENDIF
028500110929     C*
028600110929     C* Considerazione su eventi DIR
028700110929     C                   IF        stbCOS = 'DIR'
028800110929     C                   SETON                                        40
028900110929     C                   ENDIF
029000080618     C*
029100110103     C* Effettuo l'abbinamento tra causali standard Bartolini e causali cliente
029200110927     C                   IF         stbTIS= '1' AND
029300110927     C                             (stbPRS= '2' OR stbPRS= '3' OR stbPRS= '4')
029400110927     C*
029500110927     C                   MOVEL     *blanks       wCOS             10
029600110927     C                   MOVEL     *blanks       wCAU             10
029700110927     C                   IF        stbPRS = '3'
029800110927     C                   EVAL      wCOS = %subst(stbCOS:1:7)
029900110928     C                   EVAL      wCAU = %subst(stbCOS:8:3)
030000110927     C                   ELSE
030100110927     C                   EVAL      wCOS = stbCOS
030200110927     C                   ENDIF
030300110927     C*
030400031103     C                   Z-ADD     1             j
030500110927     C     wCOS          LOOKUP    CAUBAR(j)                              55
030600110103     C                   IF        %found AND CAUCLI(j) <> 'NON INVIARE'
030700110927     C                   EVAL      DS_CS_EveCod = CAUCLI(j)
030800110927     C                   EVAL      DS_CS_EveDes = CAUDES(j)
030900110927     C*
031000110927     C* Verifico se evento lasciato avviso
031100110927     C                   z-add     1             ii
031200110927     C     wCOS          lookup    sk2Alavv(ii)                           55
031300110927     C                   if        %found
031400110927     C                   eval      DS_CS_LavDat = stbDAS
031500110927     C                   endif
031600110927     C*
031700110927     C* Se stato d tipo giacenza
031800110927     C                   if        stbPRS = '3'
031900110927     C                   eval      DS_CS_GiacDat = stbDAS
032000110927     C*
032100110927     C* Se apertura reperisco il motivo apertura giacenza
032200110927     C                   if        stbSTS = '1'
032300110927     C                   z-add     1             i
032400110927     C     wCAU          lookup    sk2Agiac(i)                            55
032500110927     C                   if        %found
032600110927     C                   eval      DS_CS_EveDes = sk2Agiacd(i)
032700110927     C                   endif
032800111121     C                   endif
032900111121     C*
033000111121     C                   if        wGiac = 'S'
033100110927     C*
033200110927     C* Scorro tutte le note giacenza relative alla giacebza corrente
033300110927     C     KEYgnp01_P    setll     tignp01l
033400110927     C                   if        %equal(tignp01l)
033500110927     C     KEYgnp01_P    reade     tignp01l
033600110927     C                   dow       not %eof(tignp01l)
033700110927     C*** concateno note giacenza
033800110927     C                   eval      DS_CS_GiacMot =
033900110927     C                             %trim(DS_CS_GiacMot) + ' ' + gnpDMC
034000110927     C     KEYgnp01_P    reade     tignp01l
034100110927     C                   enddo
034200110927     C                   endif
034300111121     C*
034400110927     C                   endif
034500110927     C*
034600110927     C                   endif
034700110927     C*
034800031103     C                   ELSE
034900031103     C                   SETON                                        50
035000031103     C                   ENDIF
035100110927     C                   ENDIF
035200110103     C*
035300110103     C* Effettuo l'abbinamento tra causali personalizzate Bartolini e causali cliente
035400110927     C                   IF        stbTIS= '9' AND
035500110927     C                             stbPRS= '1'
035600110927     C*
035700110927     C                   SETOFF                                       50
035800110927     C*
035900110103     C                   Z-ADD     1             j
036000110927     C     stbCOS        LOOKUP    WCOD9BAR(j)                            55
036100110103     C                   IF        %found AND WCOD9CLI(j) <> 'NON INVIARE'
036200110103     C                   EVAL      DS_CS_EveCod = WCOD9CLI(j)
036300110927     C                   EVAL      DS_CS_EveDes = WCOD9DES(j)
036400110103     C                   ELSE
036500110103     C                   SETON                                        50
036600110103     C                   ENDIF
036700110103     C                   ENDIF
036800041015     C*
036900080618     C* X stati provenienti dai danni, se trattasi d tipi anomalia 'A' o 'M' o 'V'
037000110927     C                   IF        stbTIS= '1' AND
037100110927     C                             stbPRS= '5'
037200080618     C*
037300110927     C                   SETON                                        50
037400080618     C*
037500080901     C* Effettuo l'abbinamento tra causali Bartolini e causali cliente
037600080618     C                   Z-ADD     1             jTAD
037700080618     C                   MOVEL(P)  stbCOS        wTADCod
037800080618     C     wTADCod       LOOKUP    skTADCod(jTAD)                         70
037900080618     C                   IF        %found
038000110927     C                   Z-ADD     1             j
038100080618     C     skTADRag(jTAD)LOOKUP    WDCTBAR(j)                             70
038200080618     C                   IF        %found
038300110103     C                   EVAL      DS_CS_EveCod = WDCTCLI(j)
038400110927     C                   EVAL      DS_CS_EveDes = WDCTDES(j)
038500110927     C                   SETOFF                                       50
038600080618     C                   ENDIF
038700080618     C                   ENDIF
038800080618     C                   ENDIF
038900111121     C*
039000111121     C* Se evento "fittizio" e spedizione in giacenza => nn restituisco lo stato "fittizio"
039100111121     C                   z-add     1             i
039200111121     C     wCOS          lookup    sk2A_2Z                                55
039300120130     C***                if        %found and wGiac = 'S'
039400120130     C                   if        %found
039500111121     C                   SETON                                        50
039600111121     C                   endif
039700080618     C*
039800080618     C* Scarico il buffer d output
039900080618     C  N50              EXSR      WRIREC
040000110103     C*
040100110103     C                   ENDIF
040200110103     C                   ENDIF
040300110103     C                   ENDIF
040400031103     C*
040500031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
040600031103     C                   EVAL      stbFTR = 'T'
040700031103     C                   UPDATE    tistb000
040800121012     C*
040900121012     C                   ENDIF
041000991027     C*
041100110103     C     KEYstb03P     READE     tistb03l
041200031103     C                   SETOFF                                       50
041300030325     C                   ENDDO
041400031103     C                   ENDIF
041500991027     C*
041600030325     C                   EVAL      wrkesito = '0'
041700991027     C*
041800910830     C                   ENDSR
041900111121
042000111121
042100111121
042200111121     C     rtvgiac       BEGSR
042300111121     C*
042400111121     C                   movel     'N'           wGiac             1
042500111121     C*
042600111121     C* Costruisco la chiave sul file TIGCP51L
042700111121     C                   eval      gcpFRG = *zeros
042800111121     C*
042900111121     C* Reperisco le giacenze relative alla bolla corrente
043000111121     C     KEYgcp51_P    chain     tigcp51l
043100111121     C                   if        %found(tigcp51l)
043200111216     C                   if        gcpATB = *blanks AND gcpDCG = 0
043300111121     C                   movel     'S'           wGiac
043400111216     C                   endif
043500111121     C                   endif
043600111121     C*
043700111121     C                   ENDSR
043800031103
043900031103
044000031103
044100110103     C     wrirec        BEGSR
044200110928     C*
044300110928     C                   eval      wLNP = tasLNP
044400110928     C                   eval      wLNA = tasLNA
044500110928     C                   eval      wDCR = tasDCR
044600120229     C*
044700120229     C* Prendo sempre i dati più recenti da ambiente filiale
044800120229     C                   setoff                                       60
044900120229     C*
045000120229     C                   eval      pOutAnnoFI     = tasAAS
045100120229     C                   eval      pOutLineaParFI = tasLNP
045200120229     C                   eval      pOutSerieFI    = tasNRS
045300120229     C                   eval      pOutNumSpedFI  = tasNSP
045400120229     C*
045500120229     C* - 1° tentativo in filiale arrivo   (FNARB)
045600120229     C                   if        not *in60
045700120229     C     KEYarb01C     chain     fnarb01l
045800120229     C                   if        %found(fnarb01l)
045900120229     C                   seton                                        60
046000120229     C                   eval      wLNP   = arbLNP
046100120229     C                   eval      wLNA   = arbLNA
046200120229     C                   if        arbDCR > wDCR
046300120229     C                   eval      wDCR   = arbDCR
046400120229     C                   endif
046500120229     C                   endif
046600120229     C                   endif
046700120229     C*
046800120229     C* - 2° tentativo in filiale partenza (FNBLP)
046900120229     C                   if        not *in60
047000120229     C     KEYarb01C     chain     fnblp01l
047100120229     C                   if        %found(fnblp01l)
047200120229     C                   seton                                        60
047300120229     C                   eval      wLNP   = blpLNP
047400120229     C                   eval      wLNA   = blpLNA
047500120229     C                   if        blpDCR > wDCR
047600120229     C                   eval      wDCR   = blpDCR
047700120229     C                   endif
047800120229     C                   endif
047900120229     C                   endif
048000111103     C*
048100111103     C                   movel     *blanks       wEsito1           1
048200110928     C                   movel     *blanks       wEsito2           1
048300111103     C*
048400111103     C* Chiamata metodo GetLblTyp
048500111103     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
048600111103     C                                                stbAAS
048700111103     C                                               :stbLNP
048800111103     C                                               :stbNRS
048900111103     C                                               :stbNSP
049000111103     C                                               :pOutLblTyp
049100111103     C                                               :pOutAnnoBO
049200111103     C                                               :pOutLineaParBO
049300111103     C                                               :pOutSerieBO
049400111103     C                                               :pOutNumSpedBO
049500111103     C                                               :pOutDcmBO
049600111103     C                                               :pOutCcaBO
049700111103     C                                               :pOutRblBO))
049800111103     C*
049900111103     C                   if        wEsito1 = '0'
050000111103     C                   if        pOutCcaBO = '2' AND
050100111103     C                             stbDAS    > pOutDcmBO
050200111103     C                   eval      DS_CS_EveDes = %trim(DS_CS_EveDes) +
050300111103     C                             ' COMMERCITY'
050400111103     C                   endif
050500111103     C                   endif
050600111103     C*
050700111103     C* Se necessario reperisco l'ultima bolla figlia
050800111103     C                   if        *in40
050900110928     C*
051000110928     C* Chiamata metodo GetLastChild
051100110928     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
051200110928     C                                                stbAAS
051300110928     C                                               :stbLNP
051400110928     C                                               :stbNRS
051500110928     C                                               :stbNSP
051600110928     C                                               :pOutAnnoFI
051700110928     C                                               :pOutLineaParFI
051800110928     C                                               :pOutSerieFI
051900110928     C                                               :pOutNumSpedFI
052000110928     C                                               :pOutDcmFI
052100110928     C                                               :pOutCcaFI))
052200110928     C*
052300110928     C* Se esito chiamata ok => reperisco i dati della bolla figlia
052400110928     C                   if        wEsito2 = '0'
052500110929     C*
052600110929     C                   setoff                                       60
052700110928     C*
052800110928     C* - 1° tentativo in filiale arrivo   (FNARB)
052900111216     C                   if        not *in60
053000110929     C     KEYarb01C     chain     fnarb01l
053100110929     C                   if        %found(fnarb01l)
053200110929     C                   seton                                        60
053300110929     C                   eval      wLNP   = arbLNP
053400110929     C                   eval      wLNA   = arbLNA
053500110929     C                   if        arbDCR > wDCR
053600110929     C                   eval      wDCR   = arbDCR
053700110929     C                   endif
053800110929     C                   endif
053900111216     C                   endif
054000110928     C*
054100110928     C* - 2° tentativo in filiale partenza (FNBLP)
054200111216     C                   if        not *in60
054300110929     C     KEYarb01C     chain     fnblp01l
054400110929     C                   if        %found(fnblp01l)
054500110929     C                   seton                                        60
054600110929     C                   eval      wLNP   = blpLNP
054700110929     C                   eval      wLNA   = blpLNA
054800110929     C                   if        blpDCR > wDCR
054900110929     C                   eval      wDCR   = blpDCR
055000110929     C                   endif
055100110929     C                   endif
055200111216     C                   endif
055300110928     C*
055400110928     C* - 3° tentativo in sede             (TITAS)
055500110929     C                   if        not *in60
055600110929     C     KEYarb01C     chain     titas30c
055700110929     C                   if        %found(titas30c)
055800110929     C                   eval      wLNP   = tasLNP
055900110929     C                   eval      wLNA   = tasLNA
056000110929     C                   eval      wDCR   = tasDCR
056100110929     C                   endif
056200110929     C                   endif
056300110929     C*
056400110928     C                   endif
056500110929     C                   endif
056600110927     C*
056700110928     C     wLNP          chain     azorg01l
056800110927     C                   if        %found(azorg01l)
056900110927     C                   eval      DS_CS_LNP = orgDES
057000110927     C                   endif
057100110927     C*
057200110928     C     wLNA          chain     azorg01l
057300110927     C                   if        %found(azorg01l)
057400110927     C                   eval      DS_CS_LNA = orgDES
057500110927     C                   endif
057600110927     C*
057700110927     C                   eval      DS_CS_RMN     = stbRMN
057800111020     C                   eval      DS_CS_EveDaOr = stbDAS*10000+stbORS
057900110928     C                   eval      DS_CS_DCR     = wDCR
058000110929     C                   eval      DS_CS_CasImp  = wCAS
058100110927     C                   eval      DS_CS_CasDatP = wDDP
058200110927     C                   eval      DS_CS_CasTipI = wTPI
058300120507     C                   eval      DS_CS_KeySPED = %trim(%editc(tasLNP:'X'))+
058400120507     C                                             %trim(%editc(tasNRS:'X'))+
058500120507     C                                             %trim(%editc(tasNSP:'X'))
058600031103     C*
058700031103     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII e scrivo
058800110927     C                   EVAL      VAWDTA =
058900110927     C                                    %trim(%editc(DS_CS_RMN:'X'))
059000110927     C                               +'|'+%trim(DS_CS_LNP)
059100110927     C                               +'|'+%trim(DS_CS_LNA)
059200111020     C                               +'|'+%trim(%editc(DS_CS_EveDaOr:'X'))
059300110927     C                               +'|'+%trim(DS_CS_EveCod)
059400111103     C                               +'|'+%trim(DS_CS_EveDes)
059500110927     C                               +'|'+%trim(%editc(DS_CS_GiacDat:'X'))
059600110927     C                               +'|'+%trim(DS_CS_GiacMot)
059700110927     C                               +'|'+%trim(%editc(DS_CS_LavDat:'X'))
059800110927     C                               +'|'+%trim(%editc(DS_CS_DCR:'X'))
059900110927     C                               +'|'+%trim(%editc(DS_CS_CasImp:'3'))
060000110927     C                               +'|'+%trim(%editc(DS_CS_CasDatP:'X'))
060100110927     C                               +'|'+%trim(DS_CS_CasTipI)
060200120507     C                               +'|'+%trim(DS_CS_KeySPED)
060300110927     C                               +'|'
060400110927     C*
060500031103     C                   WRITE     TIVAW000
060600031103     C*
060700031103     C                   ENDSR
060800031103
060900031103
061000991027
061100991027      /TITLE Operazioni iniziali.
061200991027     C     *inzsr        BEGSR
061300991027     C*
061400991027     C     *ENTRY        PLIST
061500031103     C                   parm                    prmppi
061600991027     C     wrkesito      parm      wrkesito      prmesito
061700031103     C*
061800031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
061900031103     C                   EVAL      DSPPI = prmppi
062000030709     C*
062100030709     C* Definizione chiavi
062200110927     C*
062300110927     C* Chiave su TABEL00F - Parziale
062400110927     C     KEYtab00P     KLIST
062500110927     C                   KFLD                    tblKUT
062600110927     C                   KFLD                    tblCOD
062700030924     C*
062800110103     C* Chiave su TISTB03L - Parziale
062900110103     C     KEYstb03P     KLIST
063000031104     C                   KFLD                    wKstbKSU
063100031103     C                   KFLD                    wKstbFTR
063200031103     C*
063300031103     C* Chiave su TITAS30C - Parziale
063400031103     C     KEYtas30P     KLIST
063500031103     C                   KFLD                    stbAAS
063600031103     C                   KFLD                    stbLNP
063700031103     C                   KFLD                    stbNRS
063800031103     C                   KFLD                    stbNSP
063900110929     C*
064000110929     C* Chiave su FNARB01L / FNBLP01L - Completa
064100110929     C     KEYarb01C     KLIST
064200110929     C                   KFLD                    pOutAnnoFI
064300110929     C                   KFLD                    pOutLineaParFI
064400110929     C                   KFLD                    pOutSerieFI
064500110929     C                   KFLD                    pOutNumSpedFI
064600110103     C*
064700110103     C* Chiave su TNCSB03L - Completa
064800110103     C     KEYcsb03C     KLIST
064900110103     C                   KFLD                    stbAAS
065000110103     C                   KFLD                    stbLNP
065100110103     C                   KFLD                    stbNRS
065200110103     C                   KFLD                    stbNSP
065300110927     C*
065400110927     C* Chiave su TIGCP51L - Parziale
065500110927     C     KEYgcp51_P    KLIST
065600110927     C                   KFLD                    stbAAS
065700110927     C                   KFLD                    stbLNP
065800110927     C                   KFLD                    stbNRS
065900110927     C                   KFLD                    stbNSP
066000110927     C                   KFLD                    gcpFRG
066100110927     C*
066200110927     C* Chiave su TIGNP01L - Parziale
066300110927     C     KEYgnp01_P    KLIST
066400110927     C                   KFLD                    gcpAGC
066500110927     C                   KFLD                    gcpFGC
066600110927     C                   KFLD                    gcpNGC
066700110927     C                   KFLD                    gcpFRG
066800991027     C*
066900991027     C                   ENDSR
067000110103** CAUBAR - CAUSALI BARTOLINI
067100110103A                       L.AVVISO
067200110103ARS                     AVVISATO-RIMANDA SVINCOLO 1GG
067300110103AVV                     LASCIATO AVVISO 1GG
067400110103AV2                     LASCIATO AVVISO 2GG
067500110103AV3                     LASCIATO AVVISO 3GG
067600110103AV5                     LASCIATO AVVISO 5GG
067700110103AV7                     LASCIATO AVVISO 7GG
067800110103AV9                     LASCIATO AVVISO 9 GIORNI
067900110103A16                     L.A. RIMANDA LO SVINCOLO
068000110103A23                     L.A. CHIUSO FINO AL
068100110103CPO                     CAMBIO PORTO
068200110103DAN                     COLLO/I DANNEGGIATO/I
068300110103DIR                     DIROTTAMENTO
068400110103F                       FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
068500110103G                       GIACENZA GENERICA TEMPORANEA
068600110103GEN                     GIACENZA GENERICA
068700110103G02                     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
068800110103G03                     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
068900110103G05                     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
069000110103G09                     ATTENDERE 9 GIORNI
069100110103IDD                     CHIUSURA SPEDIZIONE CON IDD
069200110103MAN                     COLLO/I MANCANTE/I
069300110103MIC                     MESSA IN CONSEGNA
069400110103N                       NON FATTA
069500110103NAV                     COLLO SCONDIZIONATO
069600110103NIC                     TOLTA DALLA CONSEGNA
069700110103P                       CONSEGNA PARZIALE
069800110103PAT                     FESTIVITA PATRONALE
069900110103R                       RISERVA
070000110103RDC                     RIPRISTINO
070100110103RFD                     RIFIUTATO
070200110103RFM                     RIFIUTATO
070300110103RIC                     RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
070400110103RIP                     RIPRISTINO
070500110103RS                      RISERVA
070600110103T                       CHIUSO PER TURNO
070700110103WA                      AMMANCO SENZA EVENTO
070800110103WAE                     AMMANCO CON EVENTO
070900110103WM                      MANCANZA SENZA EVENTO
071000110103WME                     MANCANZA CON EVENTO
071100110103WV                      AVARIA SENZA EVENTO
071200110103WVE                     AVARIA CON EVENTO
071300110103ZAL                     ALLUVIONE
071400110103ZBS                     BLOCCO STRADALE
071500110103ZFR                     FESTIVITÀ REGIONALE
071600110103ZMM                     MARE MOLTO MOSSO
071700110103ZMP                     MANIFESTAZIONE PUBBLICA
071800110103ZNV                     NEVICATA ECCEZIONALE
071900110103ZSC                     SCIOPERO
072000110103001                     RIFIUTA SENZA SPECIFICARE IL MOTIVO
072100110103002                     MERCE NON ORDINATA O NON CONFORME
072200110103003                     MERCE SPEDITA IN RITARDO
072300110103004                     MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
072400110103005                     MERCE SPEDITA CON TROPPO ANTICIPO
072500110103006                     MERCE RESA SENZA AUTORIZZAZIONE
072600110103007                     DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
072700110103008                     DESTINATARIO NON PAGA IL C/ASSEGNO
072800110103009                     CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
072900110103010                     DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
073000110103012                     IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
073100110103013                     DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
073200110103014                     DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
073300110103016                     DESTINATARIO RIMANDA LO SVINCOLO AL
073400110103017                     DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
073500110103018                     DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
073600110103019                     DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
073700110103020                     DESTINATARIO RISULTA TRASFERITO
073800110103021                     DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
073900110103022                     INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
074000110103023                     IL DESTINATARIO E' CHIUSO FINO AL
074100110103024                     IL DESTINATARIO E' CHIUSO PER FERIE
074200110103025                     IL DESTINATARIO E' CHIUSO
074300110103026                     IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
074400110103027                     DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
074500110103028                     ESERCIZIO NON ANCORA IN ATTIVITA
074600110103029                     IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
074700110103030                     IL DESTINATARIO RIFIUTA LA MERCE
074800110103031                     IL DESTINATARIO NON INTENDE
074900110103032                     FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
075000110103033                     IL DESTINATARIO RICHIEDE
075100110103034                     SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
075200110103035                     DOCUMENTI INCOMPLETI O MANCANTI
075300110103036                     IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
075400110103037                     RIFIUTA LA CONSEGNA TASSATIVA
075500110103100                     -
075600110103101                     -
075700110103999                     -
075800110103CONSOK                  CONSEGNA OK
075900110103RESO                    RESO
076000110927                        -
076100110929GIACAPE                 APERTURA GIACENZA
076200110927GIACDIS                 RICEVUTE DISPOSIZIONI GIACENZA
076300110927GIACEND                 CHIUSURA GIACENZA
076400110927                        -
076500110927                        -
076600110927                        -
076700110927                        -
076800110927                        -
076900110927                        -
077000110927                        -
077100110103** CAUCLI - CAUSALI CLIENTE
077200110926A                       L.AVVISO
077300110926ARS                     AVVISATO-RIMANDA SVINCOLO 1GG
077400110926AVV                     LASCIATO AVVISO 1GG
077500110926AV2                     LASCIATO AVVISO 2GG
077600110926AV3                     LASCIATO AVVISO 3GG
077700110926AV5                     LASCIATO AVVISO 5GG
077800110926AV7                     LASCIATO AVVISO 7GG
077900110926AV9                     LASCIATO AVVISO 9 GIORNI
078000110926A16                     L.A. RIMANDA LO SVINCOLO
078100110926A23                     L.A. CHIUSO FINO AL
078200110926CPO                     CAMBIO PORTO
078300110926DAN                     COLLO/I DANNEGGIATO/I
078400110926DIR                     DIROTTAMENTO
078500110926F                       FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
078600110926G                       GIACENZA GENERICA TEMPORANEA
078700110926GEN                     GIACENZA GENERICA
078800110926G02                     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
078900110926G03                     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
079000110926G05                     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
079100110926G09                     ATTENDERE 9 GIORNI
079200110926IDD                     CHIUSURA SPEDIZIONE CON IDD
079300110926MAN                     COLLO/I MANCANTE/I
079400110926MIC                     MESSA IN CONSEGNA
079500110926N                       NON FATTA
079600110926NAV                     COLLO SCONDIZIONATO
079700110926NIC                     TOLTA DALLA CONSEGNA
079800110926P                       CONSEGNA PARZIALE
079900110926PAT                     FESTIVITA PATRONALE
080000110926R                       RISERVA
080100110926RDC                     RIPRISTINO
080200110926RFD                     RIFIUTATO
080300110926RFM                     RIFIUTATO
080400110926RIC                     RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
080500110926RIP                     RIPRISTINO
080600110926RS                      RISERVA
080700110926T                       CHIUSO PER TURNO
080800110926WA                      AMMANCO SENZA EVENTO
080900110926WAE                     AMMANCO CON EVENTO
081000110926WM                      MANCANZA SENZA EVENTO
081100110926WME                     MANCANZA CON EVENTO
081200110926WV                      AVARIA SENZA EVENTO
081300110926WVE                     AVARIA CON EVENTO
081400110926ZAL                     ALLUVIONE
081500110926ZBS                     BLOCCO STRADALE
081600110926ZFR                     FESTIVITÀ REGIONALE
081700110926ZMM                     MARE MOLTO MOSSO
081800110926ZMP                     MANIFESTAZIONE PUBBLICA
081900110926ZNV                     NEVICATA ECCEZIONALE
082000110926ZSC                     SCIOPERO
082100110926001                     RIFIUTA SENZA SPECIFICARE IL MOTIVO
082200110926002                     MERCE NON ORDINATA O NON CONFORME
082300110926003                     MERCE SPEDITA IN RITARDO
082400110926004                     MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
082500110926005                     MERCE SPEDITA CON TROPPO ANTICIPO
082600110926006                     MERCE RESA SENZA AUTORIZZAZIONE
082700110926007                     DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
082800110926008                     DESTINATARIO NON PAGA IL C/ASSEGNO
082900110926009                     CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
083000110926010                     DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
083100110926012                     IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
083200110926013                     DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
083300110926014                     DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
083400110926016                     DESTINATARIO RIMANDA LO SVINCOLO AL
083500110926017                     DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
083600110926018                     DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
083700110926019                     DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
083800110926020                     DESTINATARIO RISULTA TRASFERITO
083900110926021                     DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
084000110926022                     INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
084100110926023                     IL DESTINATARIO E' CHIUSO FINO AL
084200110926024                     IL DESTINATARIO E' CHIUSO PER FERIE
084300110926025                     IL DESTINATARIO E' CHIUSO
084400110926026                     IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
084500110926027                     DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
084600110926028                     ESERCIZIO NON ANCORA IN ATTIVITA
084700110926029                     IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
084800110926030                     IL DESTINATARIO RIFIUTA LA MERCE
084900110926031                     IL DESTINATARIO NON INTENDE
085000110926032                     FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
085100110926033                     IL DESTINATARIO RICHIEDE
085200110926034                     SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
085300110926035                     DOCUMENTI INCOMPLETI O MANCANTI
085400110926036                     IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
085500110926037                     RIFIUTA LA CONSEGNA TASSATIVA
085600110926NON INVIARE             -
085700110926NON INVIARE             -
085800110926NON INVIARE             -
085900110926CONSOK                  CONSEGNA OK
086000110926RESO                    RESO
086100110926
086200110927*AG                     APERTURA GIACENZA
086300110927*DG                     RICEVUTE DISPOSIZIONI GIACENZA
086400110927*CG                     CHIUSURA GIACENZA
086500110927                        -
086600110927                        -
086700110927                        -
086800110927                        -
086900110927                        -
087000110927                        -
087100110927                        -
087200110926** CAUDES - DESCRIZIONI CAUSALI CLIENTE
087300110103L.AVVISO
087400110103AVVISATO-RIMANDA SVINCOLO 1GG
087500110103LASCIATO AVVISO 1GG
087600110103LASCIATO AVVISO 2GG
087700110103LASCIATO AVVISO 3GG
087800110103LASCIATO AVVISO 5GG
087900110103LASCIATO AVVISO 7GG
088000110103LASCIATO AVVISO 9 GIORNI
088100110103L.A. RIMANDA LO SVINCOLO
088200110103L.A. CHIUSO FINO AL
088300110103CAMBIO PORTO
088400110103COLLO/I DANNEGGIATO/I
088500110103DIROTTAMENTO
088600110103FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
088700110103GIACENZA GENERICA TEMPORANEA
088800110103GIACENZA GENERICA
088900110103ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
089000110103ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
089100110103ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
089200110103ATTENDERE 9 GIORNI
089300110103CHIUSURA SPEDIZIONE CON IDD
089400110103COLLO/I MANCANTE/I
089500110103MESSA IN CONSEGNA
089600110103NON FATTA
089700110103COLLO SCONDIZIONATO
089800110103TOLTA DALLA CONSEGNA
089900110103CONSEGNA PARZIALE
090000110103FESTIVITA PATRONALE
090100110103RISERVA
090200110103RIPRISTINO
090300110103RIFIUTATO
090400110103RIFIUTATO
090500110103RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
090600110103RIPRISTINO
090700110103RISERVA
090800110103CHIUSO PER TURNO
090900110103AMMANCO SENZA EVENTO
091000110103AMMANCO CON EVENTO
091100110103MANCANZA SENZA EVENTO
091200110103MANCANZA CON EVENTO
091300110103AVARIA SENZA EVENTO
091400110103AVARIA CON EVENTO
091500110103ALLUVIONE
091600110103BLOCCO STRADALE
091700110103FESTIVITÀ REGIONALE
091800110103MARE MOLTO MOSSO
091900110103MANIFESTAZIONE PUBBLICA
092000110103NEVICATA ECCEZIONALE
092100110103SCIOPERO
092200110103RIFIUTA SENZA SPECIFICARE IL MOTIVO
092300110103MERCE NON ORDINATA O NON CONFORME
092400110103MERCE SPEDITA IN RITARDO
092500110103MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
092600110103MERCE SPEDITA CON TROPPO ANTICIPO
092700110103MERCE RESA SENZA AUTORIZZAZIONE
092800110103DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
092900110103DESTINATARIO NON PAGA IL C/ASSEGNO
093000110103CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
093100110103DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
093200110103IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
093300110103DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
093400110103DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
093500110103DESTINATARIO RIMANDA LO SVINCOLO AL
093600110103DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
093700110103DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
093800110103DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
093900110103DESTINATARIO RISULTA TRASFERITO
094000110103DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
094100110103INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
094200110103IL DESTINATARIO E' CHIUSO FINO AL
094300110103IL DESTINATARIO E' CHIUSO PER FERIE
094400110103IL DESTINATARIO E' CHIUSO
094500110103IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
094600110103DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
094700110103ESERCIZIO NON ANCORA IN ATTIVITA
094800110103IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
094900110103IL DESTINATARIO RIFIUTA LA MERCE
095000110103IL DESTINATARIO NON INTENDE
095100110103FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
095200110103IL DESTINATARIO RICHIEDE
095300110103SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
095400110103DOCUMENTI INCOMPLETI O MANCANTI
095500110103IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
095600110103RIFIUTA LA CONSEGNA TASSATIVA
095700110103-
095800110103-
095900110103-
096000110103CONSEGNA OK
096100110103RESO
096200110927-
096300110927APERTURA GIACENZA
096400110927RICEVUTE DISPOSIZIONI GIACENZA
096500110927CHIUSURA GIACENZA
096600110927-
096700110927-
096800110927-
096900110927-
097000110927-
097100110927-
097200110927-
097300041015** WDCTBAR - TIPI ANOMALIE C/A BARTOLINI
097400110103A                                                                                            1
097500110103M                                                                                            2
097600110103V                                                                                            3
097700110103E                                                                                            4
097800080901** WDCTCLI - CAUSALI CLIENTE
097900110926TAD_A                                                                                        1
098000110926TAD_M                                                                                        2
098100110926TAD_V                                                                                        3
098200110926TAD_E                                                                                        4
098300110926** WDCTDES - DESCRIZIONE TIPI ANOMALIE C/A BARTOLINI
098400110927AMMANCO                                                                                      1
098500110927MANCA COLLO                                                                                  2
098600110927AVARIA                                                                                       3
098700110927EVENTO                                                                                       4
098800110103** WCOD9BAR - CODICI EVENTO "PERSONALIZZATI" - BARTOLINI
098900110103INCARICO1                                                                                    1
099000110103INCARICO2                                                                                    2
099100110103CONSOK1                                                                                      3
099200110103CONSOK2                                                                                      4
099300110927GIACNODIS1                                                                                   5
099400110927GIACNODIS2                                                                                   6
099500110927GIACSIDIS1                                                                                   7
099600110927GIACSIDIS2                                                                                   8
099700110927                                                                                             9
099800110927                                                                                             10
099900110103** WCOD9CLI - CODICI EVENTO "PERSONALIZZATI" - CLIENTE
100000110926ACCETTAZIONE                                                                                 1
100100110926ACCETTAZIONE                                                                                 2
100200110103NON INVIARE                                                                                  3
100300110103NON INVIARE                                                                                  4
100400110927GIAC_APERTA                                                                                  5
100500110927GIAC_APERTA                                                                                  6
100600110927GIAC_DISPOS                                                                                  7
100700110927GIAC_DISPOS                                                                                  8
100800110927NON INVIARE                                                                                  9
100900110927NON INVIARE                                                                                  10
101000110926** WCOD9DES - DESCRIZIONI CODICI EVENTO "PERSONALIZZATI"
101100110926ACCETTAZIONE SPEDIZIONE (TRANSITO)                                                           1
101200110926ACCETTAZIONE SPEDIZIONE (TRANSITO)                                                           2
101300110926NON INVIARE                                                                                  3
101400110926NON INVIARE                                                                                  4
101500110927APERTURA GIACENZA                                                                            5
101600110927APERTURA GIACENZA                                                                            6
101700110927RICEVUTE DISPOSIZIONI                                                                        7
101800110927RICEVUTE DISPOSIZIONI                                                                        8
101900110927NON INVIARE                                                                                  8
102000110927NON INVIARE                                                                                  10
