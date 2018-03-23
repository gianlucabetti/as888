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
023100130304     C*
023200130304     C* Ignoro completamente gli stati provenienti dai PDA ...
023300130304     C                   IF        stbPRS = '7'
023400130304     C                   ELSE
023500121012     C*
023600121012     C* Verifico immediatamente data/ora stato corrente => se "futura" skippo completamente
023700121012     C                   IF        stbDAS*10000+stbORS >
023800121012     C                             %int((%dec(%date():*ISO)*1000000+
023900121012     C                             %dec(%time()))/100)
024000121012     C                   ELSE
024100110929     C*
024200111103     C* Come primissima cosa verifico rottura d codice su key spedizione
024300110929     C                   IF        stbAAS = sAAS AND
024400110929     C                             stbLNP = sLNP AND
024500110929     C                             stbNRS = sNRS AND
024600110929     C                             stbNSP = sNSP
024700110929     C                   ELSE
024800110929     C                   SETOFF                                       40
024900110929     C                   EVAL      sAAS = stbAAS
025000110929     C                   EVAL      sLNP = stbLNP
025100110929     C                   EVAL      sNRS = stbNRS
025200110929     C                   EVAL      sNSP = stbNSP
025300110929     C                   ENDIF
025400031014     C*
025500030925     C* Inizializzo la DS d output con i valori di default della dichiarazione della struttura dati
025600110103     C                   RESET                   DS_Cli_Status
025700080618     C*
025800110927     C                   SETON                                        50
025900031103     C*
026000031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
026100031103     C* deve restituire al cliente nel file tradotto
026200111215     C                   IF        stbFTR = *blanks                             * ridondante
026300111215     C*
026400111215     C* Verifico (subito) se spedizione in giacenza
026500111215     C                   exsr      rtvgiac
026600110103     C*
026700111031     C* Gestisco solamente alcuni stati
026800110928     C                   IF        (stbTIS= '1' OR
026900111103     C                             (stbTIS= '9' AND stbPRS= '1'))
027000110103     C*
027100110103     C                   SETOFF                                       50
027200031103     C*
027300031103     C* Innanzitutto chaino la bolla direttamente su TITAS
027400031103     C     KEYtas30P     CHAIN     titas30c
027500031103     C                   IF        %found(titas30c)
027600110103     C*
027700110103     C                   clear                   wCAS
027800110927     C                   clear                   wDDP
027900110927     C                   clear                   wTPI
028000110929     C                   clear                   wDCR
028100110103     C*
028200110103     C* Quindi verifico subito esistenza contrassegno
028300110103     C     KEYcsb03C     CHAIN     tncsb03l
028400110103     C                   IF        %found(tncsb03l)
028500110103     C                   eval      wCAS = csbCAS
028600110927     C                   eval      wDDP = csbDDP
028700110927     C                   eval      wTPI = csbTPI
028800110103     C                   ENDIF
028900110929     C*
029000110929     C* Considerazione su eventi DIR
029100110929     C                   IF        stbCOS = 'DIR'
029200110929     C                   SETON                                        40
029300110929     C                   ENDIF
029400080618     C*
029500110103     C* Effettuo l'abbinamento tra causali standard Bartolini e causali cliente
029600110927     C                   IF         stbTIS= '1' AND
029700110927     C                             (stbPRS= '2' OR stbPRS= '3' OR stbPRS= '4')
029800110927     C*
029900110927     C                   MOVEL     *blanks       wCOS             10
030000110927     C                   MOVEL     *blanks       wCAU             10
030100110927     C                   IF        stbPRS = '3'
030200110927     C                   EVAL      wCOS = %subst(stbCOS:1:7)
030300110928     C                   EVAL      wCAU = %subst(stbCOS:8:3)
030400110927     C                   ELSE
030500110927     C                   EVAL      wCOS = stbCOS
030600110927     C                   ENDIF
030700110927     C*
030800031103     C                   Z-ADD     1             j
030900110927     C     wCOS          LOOKUP    CAUBAR(j)                              55
031000110103     C                   IF        %found AND CAUCLI(j) <> 'NON INVIARE'
031100110927     C                   EVAL      DS_CS_EveCod = CAUCLI(j)
031200110927     C                   EVAL      DS_CS_EveDes = CAUDES(j)
031300110927     C*
031400110927     C* Verifico se evento lasciato avviso
031500110927     C                   z-add     1             ii
031600110927     C     wCOS          lookup    sk2Alavv(ii)                           55
031700110927     C                   if        %found
031800110927     C                   eval      DS_CS_LavDat = stbDAS
031900110927     C                   endif
032000110927     C*
032100110927     C* Se stato d tipo giacenza
032200110927     C                   if        stbPRS = '3'
032300110927     C                   eval      DS_CS_GiacDat = stbDAS
032400110927     C*
032500110927     C* Se apertura reperisco il motivo apertura giacenza
032600110927     C                   if        stbSTS = '1'
032700110927     C                   z-add     1             i
032800110927     C     wCAU          lookup    sk2Agiac(i)                            55
032900110927     C                   if        %found
033000110927     C                   eval      DS_CS_EveDes = sk2Agiacd(i)
033100110927     C                   endif
033200111121     C                   endif
033300111121     C*
033400111121     C                   if        wGiac = 'S'
033500110927     C*
033600110927     C* Scorro tutte le note giacenza relative alla giacebza corrente
033700110927     C     KEYgnp01_P    setll     tignp01l
033800110927     C                   if        %equal(tignp01l)
033900110927     C     KEYgnp01_P    reade     tignp01l
034000110927     C                   dow       not %eof(tignp01l)
034100110927     C*** concateno note giacenza
034200110927     C                   eval      DS_CS_GiacMot =
034300110927     C                             %trim(DS_CS_GiacMot) + ' ' + gnpDMC
034400110927     C     KEYgnp01_P    reade     tignp01l
034500110927     C                   enddo
034600110927     C                   endif
034700111121     C*
034800110927     C                   endif
034900110927     C*
035000110927     C                   endif
035100110927     C*
035200031103     C                   ELSE
035300031103     C                   SETON                                        50
035400031103     C                   ENDIF
035500110927     C                   ENDIF
035600110103     C*
035700110103     C* Effettuo l'abbinamento tra causali personalizzate Bartolini e causali cliente
035800110927     C                   IF        stbTIS= '9' AND
035900110927     C                             stbPRS= '1'
036000110927     C*
036100110927     C                   SETOFF                                       50
036200110927     C*
036300110103     C                   Z-ADD     1             j
036400110927     C     stbCOS        LOOKUP    WCOD9BAR(j)                            55
036500110103     C                   IF        %found AND WCOD9CLI(j) <> 'NON INVIARE'
036600110103     C                   EVAL      DS_CS_EveCod = WCOD9CLI(j)
036700110927     C                   EVAL      DS_CS_EveDes = WCOD9DES(j)
036800110103     C                   ELSE
036900110103     C                   SETON                                        50
037000110103     C                   ENDIF
037100110103     C                   ENDIF
037200041015     C*
037300080618     C* X stati provenienti dai danni, se trattasi d tipi anomalia 'A' o 'M' o 'V'
037400110927     C                   IF        stbTIS= '1' AND
037500110927     C                             stbPRS= '5'
037600080618     C*
037700110927     C                   SETON                                        50
037800080618     C*
037900080901     C* Effettuo l'abbinamento tra causali Bartolini e causali cliente
038000080618     C                   Z-ADD     1             jTAD
038100080618     C                   MOVEL(P)  stbCOS        wTADCod
038200080618     C     wTADCod       LOOKUP    skTADCod(jTAD)                         70
038300080618     C                   IF        %found
038400110927     C                   Z-ADD     1             j
038500080618     C     skTADRag(jTAD)LOOKUP    WDCTBAR(j)                             70
038600080618     C                   IF        %found
038700110103     C                   EVAL      DS_CS_EveCod = WDCTCLI(j)
038800110927     C                   EVAL      DS_CS_EveDes = WDCTDES(j)
038900110927     C                   SETOFF                                       50
039000080618     C                   ENDIF
039100080618     C                   ENDIF
039200080618     C                   ENDIF
039300111121     C*
039400111121     C* Se evento "fittizio" e spedizione in giacenza => nn restituisco lo stato "fittizio"
039500111121     C                   z-add     1             i
039600111121     C     wCOS          lookup    sk2A_2Z                                55
039700120130     C***                if        %found and wGiac = 'S'
039800120130     C                   if        %found
039900111121     C                   SETON                                        50
040000111121     C                   endif
040100080618     C*
040200080618     C* Scarico il buffer d output
040300080618     C  N50              EXSR      WRIREC
040400110103     C*
040500110103     C                   ENDIF
040600110103     C                   ENDIF
040700110103     C                   ENDIF
040800031103     C*
040900031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
041000031103     C                   EVAL      stbFTR = 'T'
041100031103     C                   UPDATE    tistb000
041200121012     C*
041300121012     C                   ENDIF
041400130305     C                   ENDIF
041500991027     C*
041600110103     C     KEYstb03P     READE     tistb03l
041700031103     C                   SETOFF                                       50
041800030325     C                   ENDDO
041900031103     C                   ENDIF
042000991027     C*
042100030325     C                   EVAL      wrkesito = '0'
042200991027     C*
042300910830     C                   ENDSR
042400111121
042500111121
042600111121
042700111121     C     rtvgiac       BEGSR
042800111121     C*
042900111121     C                   movel     'N'           wGiac             1
043000111121     C*
043100111121     C* Costruisco la chiave sul file TIGCP51L
043200111121     C                   eval      gcpFRG = *zeros
043300111121     C*
043400111121     C* Reperisco le giacenze relative alla bolla corrente
043500111121     C     KEYgcp51_P    chain     tigcp51l
043600111121     C                   if        %found(tigcp51l)
043700111216     C                   if        gcpATB = *blanks AND gcpDCG = 0
043800111121     C                   movel     'S'           wGiac
043900111216     C                   endif
044000111121     C                   endif
044100111121     C*
044200111121     C                   ENDSR
044300031103
044400031103
044500031103
044600110103     C     wrirec        BEGSR
044700140630     C*
044800140630     C* Inizializzo il flag che governa la scrittura o meno del record da tornare al cliente
044900140630     C                   setoff                                       85
045000110928     C*
045100110928     C                   eval      wLNP = tasLNP
045200110928     C                   eval      wLNA = tasLNA
045300110928     C                   eval      wDCR = tasDCR
045400120229     C*
045500120229     C* Prendo sempre i dati più recenti da ambiente filiale
045600120229     C                   setoff                                       60
045700120229     C*
045800120229     C                   eval      pOutAnnoFI     = tasAAS
045900120229     C                   eval      pOutLineaParFI = tasLNP
046000120229     C                   eval      pOutSerieFI    = tasNRS
046100120229     C                   eval      pOutNumSpedFI  = tasNSP
046200120229     C*
046300120229     C* - 1° tentativo in filiale arrivo   (FNARB)
046400120229     C                   if        not *in60
046500120229     C     KEYarb01C     chain     fnarb01l
046600120229     C                   if        %found(fnarb01l)
046700120229     C                   seton                                        60
046800120229     C                   eval      wLNP   = arbLNP
046900120229     C                   eval      wLNA   = arbLNA
047000120229     C                   if        arbDCR > wDCR
047100120229     C                   eval      wDCR   = arbDCR
047200120229     C                   endif
047300120229     C                   endif
047400120229     C                   endif
047500120229     C*
047600120229     C* - 2° tentativo in filiale partenza (FNBLP)
047700120229     C                   if        not *in60
047800120229     C     KEYarb01C     chain     fnblp01l
047900120229     C                   if        %found(fnblp01l)
048000120229     C                   seton                                        60
048100120229     C                   eval      wLNP   = blpLNP
048200120229     C                   eval      wLNA   = blpLNA
048300120229     C                   if        blpDCR > wDCR
048400120229     C                   eval      wDCR   = blpDCR
048500120229     C                   endif
048600120229     C                   endif
048700120229     C                   endif
048800111103     C*
048900111103     C                   movel     *blanks       wEsito1           1
049000110928     C                   movel     *blanks       wEsito2           1
049100111103     C*
049200111103     C* Chiamata metodo GetLblTyp
049300111103     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
049400111103     C                                                stbAAS
049500111103     C                                               :stbLNP
049600111103     C                                               :stbNRS
049700111103     C                                               :stbNSP
049800111103     C                                               :pOutLblTyp
049900111103     C                                               :pOutAnnoBO
050000111103     C                                               :pOutLineaParBO
050100111103     C                                               :pOutSerieBO
050200111103     C                                               :pOutNumSpedBO
050300111103     C                                               :pOutDcmBO
050400111103     C                                               :pOutCcaBO
050500111103     C                                               :pOutRblBO))
050600111103     C*
050700111103     C                   if        wEsito1 = '0'
050800111103     C                   if        pOutCcaBO = '2' AND
050900111103     C                             stbDAS    > pOutDcmBO
051000111103     C                   eval      DS_CS_EveDes = %trim(DS_CS_EveDes) +
051100111103     C                             ' COMMERCITY'
051200111103     C                   endif
051300111103     C                   endif
051400111103     C*
051500111103     C* Se necessario reperisco l'ultima bolla figlia
051600111103     C                   if        *in40
051700110928     C*
051800110928     C* Chiamata metodo GetLastChild
051900110928     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
052000110928     C                                                stbAAS
052100110928     C                                               :stbLNP
052200110928     C                                               :stbNRS
052300110928     C                                               :stbNSP
052400110928     C                                               :pOutAnnoFI
052500110928     C                                               :pOutLineaParFI
052600110928     C                                               :pOutSerieFI
052700110928     C                                               :pOutNumSpedFI
052800110928     C                                               :pOutDcmFI
052900110928     C                                               :pOutCcaFI))
053000110928     C*
053100110928     C* Se esito chiamata ok => reperisco i dati della bolla figlia
053200110928     C                   if        wEsito2 = '0'
053300110929     C*
053400110929     C                   setoff                                       60
053500110928     C*
053600110928     C* - 1° tentativo in filiale arrivo   (FNARB)
053700111216     C                   if        not *in60
053800110929     C     KEYarb01C     chain     fnarb01l
053900110929     C                   if        %found(fnarb01l)
054000110929     C                   seton                                        60
054100110929     C                   eval      wLNP   = arbLNP
054200110929     C                   eval      wLNA   = arbLNA
054300110929     C                   if        arbDCR > wDCR
054400110929     C                   eval      wDCR   = arbDCR
054500110929     C                   endif
054600110929     C                   endif
054700111216     C                   endif
054800110928     C*
054900110928     C* - 2° tentativo in filiale partenza (FNBLP)
055000111216     C                   if        not *in60
055100110929     C     KEYarb01C     chain     fnblp01l
055200110929     C                   if        %found(fnblp01l)
055300110929     C                   seton                                        60
055400110929     C                   eval      wLNP   = blpLNP
055500110929     C                   eval      wLNA   = blpLNA
055600110929     C                   if        blpDCR > wDCR
055700110929     C                   eval      wDCR   = blpDCR
055800110929     C                   endif
055900110929     C                   endif
056000111216     C                   endif
056100110928     C*
056200110928     C* - 3° tentativo in sede             (TITAS)
056300110929     C                   if        not *in60
056400110929     C     KEYarb01C     chain     titas30c
056500110929     C                   if        %found(titas30c)
056600110929     C                   eval      wLNP   = tasLNP
056700110929     C                   eval      wLNA   = tasLNA
056800110929     C                   eval      wDCR   = tasDCR
056900110929     C                   endif
057000110929     C                   endif
057100110929     C*
057200110928     C                   endif
057300110929     C                   endif
057400140630     C*
057500140630     C* Se necessario reperisco l'ultima bolla figlia
057600140630     C                   if        wCOS='CONSOK' and %subst(stbFOP:1:1)<>'O'
057700140630     C*
057800140630     C* Chiamata metodo GetLastChild
057900140630     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
058000140630     C                                                stbAAS
058100140630     C                                               :stbLNP
058200140630     C                                               :stbNRS
058300140630     C                                               :stbNSP
058400140630     C                                               :pOutAnnoFI
058500140630     C                                               :pOutLineaParFI
058600140630     C                                               :pOutSerieFI
058700140630     C                                               :pOutNumSpedFI
058800140630     C                                               :pOutDcmFI
058900140630     C                                               :pOutCcaFI))
059000140630     C*
059100140630     C* Se esito chiamata ok => reperisco i dati della bolla figlia
059200140630     C                   if        wEsito2 = '0'
059300140630     C*
059400140630     C* Se l'evento di consegna è o NON sulla bolla originale o NON sul ultima bolla figlia
059500140630     C* => NON restituisco al cliente
059600140630     C                   if        stbAAS = pOutAnnoFI     AND
059700140630     C                             stbLNP = pOutLineaParFI AND
059800140630     C                             stbNRS = pOutSerieFI    AND
059900140630     C                             stbNSP = pOutNumSpedFI
060000140630     C                   else
060100140630     C                   seton                                        85
060200140630     C                   endif
060300140630     C                   endif
060400140630     C                   endif
060500110927     C*
060600110928     C     wLNP          chain     azorg01l
060700110927     C                   if        %found(azorg01l)
060800110927     C                   eval      DS_CS_LNP = orgDES
060900110927     C                   endif
061000110927     C*
061100110928     C     wLNA          chain     azorg01l
061200110927     C                   if        %found(azorg01l)
061300110927     C                   eval      DS_CS_LNA = orgDES
061400110927     C                   endif
061500110927     C*
061600110927     C                   eval      DS_CS_RMN     = stbRMN
061700111020     C                   eval      DS_CS_EveDaOr = stbDAS*10000+stbORS
061800110928     C                   eval      DS_CS_DCR     = wDCR
061900110929     C                   eval      DS_CS_CasImp  = wCAS
062000110927     C                   eval      DS_CS_CasDatP = wDDP
062100110927     C                   eval      DS_CS_CasTipI = wTPI
062200120507     C                   eval      DS_CS_KeySPED = %trim(%editc(tasLNP:'X'))+
062300120507     C                                             %trim(%editc(tasNRS:'X'))+
062400120507     C                                             %trim(%editc(tasNSP:'X'))
062500031103     C*
062600031103     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII e scrivo
062700110927     C                   EVAL      VAWDTA =
062800110927     C                                    %trim(%editc(DS_CS_RMN:'X'))
062900110927     C                               +'|'+%trim(DS_CS_LNP)
063000110927     C                               +'|'+%trim(DS_CS_LNA)
063100111020     C                               +'|'+%trim(%editc(DS_CS_EveDaOr:'X'))
063200110927     C                               +'|'+%trim(DS_CS_EveCod)
063300111103     C                               +'|'+%trim(DS_CS_EveDes)
063400110927     C                               +'|'+%trim(%editc(DS_CS_GiacDat:'X'))
063500110927     C                               +'|'+%trim(DS_CS_GiacMot)
063600110927     C                               +'|'+%trim(%editc(DS_CS_LavDat:'X'))
063700110927     C                               +'|'+%trim(%editc(DS_CS_DCR:'X'))
063800110927     C                               +'|'+%trim(%editc(DS_CS_CasImp:'3'))
063900110927     C                               +'|'+%trim(%editc(DS_CS_CasDatP:'X'))
064000110927     C                               +'|'+%trim(DS_CS_CasTipI)
064100120507     C                               +'|'+%trim(DS_CS_KeySPED)
064200110927     C                               +'|'
064300110927     C*
064400140630     C  N85              WRITE     TIVAW000
064500031103     C*
064600031103     C                   ENDSR
064700031103
064800031103
064900991027
065000991027      /TITLE Operazioni iniziali.
065100991027     C     *inzsr        BEGSR
065200991027     C*
065300991027     C     *ENTRY        PLIST
065400031103     C                   parm                    prmppi
065500991027     C     wrkesito      parm      wrkesito      prmesito
065600031103     C*
065700031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
065800031103     C                   EVAL      DSPPI = prmppi
065900030709     C*
066000030709     C* Definizione chiavi
066100110927     C*
066200110927     C* Chiave su TABEL00F - Parziale
066300110927     C     KEYtab00P     KLIST
066400110927     C                   KFLD                    tblKUT
066500110927     C                   KFLD                    tblCOD
066600030924     C*
066700110103     C* Chiave su TISTB03L - Parziale
066800110103     C     KEYstb03P     KLIST
066900031104     C                   KFLD                    wKstbKSU
067000031103     C                   KFLD                    wKstbFTR
067100031103     C*
067200031103     C* Chiave su TITAS30C - Parziale
067300031103     C     KEYtas30P     KLIST
067400031103     C                   KFLD                    stbAAS
067500031103     C                   KFLD                    stbLNP
067600031103     C                   KFLD                    stbNRS
067700031103     C                   KFLD                    stbNSP
067800110929     C*
067900110929     C* Chiave su FNARB01L / FNBLP01L - Completa
068000110929     C     KEYarb01C     KLIST
068100110929     C                   KFLD                    pOutAnnoFI
068200110929     C                   KFLD                    pOutLineaParFI
068300110929     C                   KFLD                    pOutSerieFI
068400110929     C                   KFLD                    pOutNumSpedFI
068500110103     C*
068600110103     C* Chiave su TNCSB03L - Completa
068700110103     C     KEYcsb03C     KLIST
068800110103     C                   KFLD                    stbAAS
068900110103     C                   KFLD                    stbLNP
069000110103     C                   KFLD                    stbNRS
069100110103     C                   KFLD                    stbNSP
069200110927     C*
069300110927     C* Chiave su TIGCP51L - Parziale
069400110927     C     KEYgcp51_P    KLIST
069500110927     C                   KFLD                    stbAAS
069600110927     C                   KFLD                    stbLNP
069700110927     C                   KFLD                    stbNRS
069800110927     C                   KFLD                    stbNSP
069900110927     C                   KFLD                    gcpFRG
070000110927     C*
070100110927     C* Chiave su TIGNP01L - Parziale
070200110927     C     KEYgnp01_P    KLIST
070300110927     C                   KFLD                    gcpAGC
070400110927     C                   KFLD                    gcpFGC
070500110927     C                   KFLD                    gcpNGC
070600110927     C                   KFLD                    gcpFRG
070700991027     C*
070800991027     C                   ENDSR
070900110103** CAUBAR - CAUSALI BARTOLINI
071000110103A                       L.AVVISO
071100110103ARS                     AVVISATO-RIMANDA SVINCOLO 1GG
071200110103AVV                     LASCIATO AVVISO 1GG
071300110103AV2                     LASCIATO AVVISO 2GG
071400110103AV3                     LASCIATO AVVISO 3GG
071500110103AV5                     LASCIATO AVVISO 5GG
071600110103AV7                     LASCIATO AVVISO 7GG
071700110103AV9                     LASCIATO AVVISO 9 GIORNI
071800110103A16                     L.A. RIMANDA LO SVINCOLO
071900110103A23                     L.A. CHIUSO FINO AL
072000110103CPO                     CAMBIO PORTO
072100110103DAN                     COLLO/I DANNEGGIATO/I
072200110103DIR                     DIROTTAMENTO
072300110103F                       FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
072400110103G                       GIACENZA GENERICA TEMPORANEA
072500110103GEN                     GIACENZA GENERICA
072600110103G02                     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
072700110103G03                     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
072800110103G05                     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
072900110103G09                     ATTENDERE 9 GIORNI
073000110103IDD                     CHIUSURA SPEDIZIONE CON IDD
073100110103MAN                     COLLO/I MANCANTE/I
073200110103MIC                     MESSA IN CONSEGNA
073300110103N                       NON FATTA
073400110103NAV                     COLLO SCONDIZIONATO
073500110103NIC                     TOLTA DALLA CONSEGNA
073600110103P                       CONSEGNA PARZIALE
073700110103PAT                     FESTIVITA PATRONALE
073800110103R                       RISERVA
073900110103RDC                     RIPRISTINO
074000110103RFD                     RIFIUTATO
074100110103RFM                     RIFIUTATO
074200110103RIC                     RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
074300110103RIP                     RIPRISTINO
074400110103RS                      RISERVA
074500110103T                       CHIUSO PER TURNO
074600110103WA                      AMMANCO SENZA EVENTO
074700110103WAE                     AMMANCO CON EVENTO
074800110103WM                      MANCANZA SENZA EVENTO
074900110103WME                     MANCANZA CON EVENTO
075000110103WV                      AVARIA SENZA EVENTO
075100110103WVE                     AVARIA CON EVENTO
075200110103ZAL                     ALLUVIONE
075300110103ZBS                     BLOCCO STRADALE
075400110103ZFR                     FESTIVITÀ REGIONALE
075500110103ZMM                     MARE MOLTO MOSSO
075600110103ZMP                     MANIFESTAZIONE PUBBLICA
075700110103ZNV                     NEVICATA ECCEZIONALE
075800110103ZSC                     SCIOPERO
075900110103001                     RIFIUTA SENZA SPECIFICARE IL MOTIVO
076000110103002                     MERCE NON ORDINATA O NON CONFORME
076100110103003                     MERCE SPEDITA IN RITARDO
076200110103004                     MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
076300110103005                     MERCE SPEDITA CON TROPPO ANTICIPO
076400110103006                     MERCE RESA SENZA AUTORIZZAZIONE
076500110103007                     DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
076600110103008                     DESTINATARIO NON PAGA IL C/ASSEGNO
076700110103009                     CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
076800110103010                     DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
076900110103012                     IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
077000110103013                     DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
077100110103014                     DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
077200110103016                     DESTINATARIO RIMANDA LO SVINCOLO AL
077300110103017                     DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
077400110103018                     DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
077500110103019                     DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
077600110103020                     DESTINATARIO RISULTA TRASFERITO
077700110103021                     DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
077800110103022                     INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
077900110103023                     IL DESTINATARIO E' CHIUSO FINO AL
078000110103024                     IL DESTINATARIO E' CHIUSO PER FERIE
078100110103025                     IL DESTINATARIO E' CHIUSO
078200110103026                     IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
078300110103027                     DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
078400110103028                     ESERCIZIO NON ANCORA IN ATTIVITA
078500110103029                     IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
078600110103030                     IL DESTINATARIO RIFIUTA LA MERCE
078700110103031                     IL DESTINATARIO NON INTENDE
078800110103032                     FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
078900110103033                     IL DESTINATARIO RICHIEDE
079000110103034                     SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
079100110103035                     DOCUMENTI INCOMPLETI O MANCANTI
079200110103036                     IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
079300110103037                     RIFIUTA LA CONSEGNA TASSATIVA
079400110103100                     -
079500110103101                     -
079600110103999                     -
079700110103CONSOK                  CONSEGNA OK
079800110103RESO                    RESO
079900140624MDISTRUTTA              MERCE DISTRUTTA
080000110929GIACAPE                 APERTURA GIACENZA
080100110927GIACDIS                 RICEVUTE DISPOSIZIONI GIACENZA
080200110927GIACEND                 CHIUSURA GIACENZA
080300110927                        -
080400110927                        -
080500110927                        -
080600110927                        -
080700110927                        -
080800110927                        -
080900110927                        -
081000110103** CAUCLI - CAUSALI CLIENTE
081100110926A                       L.AVVISO
081200110926ARS                     AVVISATO-RIMANDA SVINCOLO 1GG
081300110926AVV                     LASCIATO AVVISO 1GG
081400110926AV2                     LASCIATO AVVISO 2GG
081500110926AV3                     LASCIATO AVVISO 3GG
081600110926AV5                     LASCIATO AVVISO 5GG
081700110926AV7                     LASCIATO AVVISO 7GG
081800110926AV9                     LASCIATO AVVISO 9 GIORNI
081900110926A16                     L.A. RIMANDA LO SVINCOLO
082000110926A23                     L.A. CHIUSO FINO AL
082100110926CPO                     CAMBIO PORTO
082200110926DAN                     COLLO/I DANNEGGIATO/I
082300110926DIR                     DIROTTAMENTO
082400110926F                       FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
082500110926G                       GIACENZA GENERICA TEMPORANEA
082600110926GEN                     GIACENZA GENERICA
082700110926G02                     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
082800110926G03                     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
082900110926G05                     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
083000110926G09                     ATTENDERE 9 GIORNI
083100110926IDD                     CHIUSURA SPEDIZIONE CON IDD
083200110926MAN                     COLLO/I MANCANTE/I
083300110926MIC                     MESSA IN CONSEGNA
083400110926N                       NON FATTA
083500110926NAV                     COLLO SCONDIZIONATO
083600110926NIC                     TOLTA DALLA CONSEGNA
083700110926P                       CONSEGNA PARZIALE
083800110926PAT                     FESTIVITA PATRONALE
083900110926R                       RISERVA
084000110926RDC                     RIPRISTINO
084100110926RFD                     RIFIUTATO
084200110926RFM                     RIFIUTATO
084300110926RIC                     RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
084400110926RIP                     RIPRISTINO
084500110926RS                      RISERVA
084600110926T                       CHIUSO PER TURNO
084700110926WA                      AMMANCO SENZA EVENTO
084800110926WAE                     AMMANCO CON EVENTO
084900110926WM                      MANCANZA SENZA EVENTO
085000110926WME                     MANCANZA CON EVENTO
085100110926WV                      AVARIA SENZA EVENTO
085200110926WVE                     AVARIA CON EVENTO
085300110926ZAL                     ALLUVIONE
085400110926ZBS                     BLOCCO STRADALE
085500110926ZFR                     FESTIVITÀ REGIONALE
085600110926ZMM                     MARE MOLTO MOSSO
085700110926ZMP                     MANIFESTAZIONE PUBBLICA
085800110926ZNV                     NEVICATA ECCEZIONALE
085900110926ZSC                     SCIOPERO
086000110926001                     RIFIUTA SENZA SPECIFICARE IL MOTIVO
086100110926002                     MERCE NON ORDINATA O NON CONFORME
086200110926003                     MERCE SPEDITA IN RITARDO
086300110926004                     MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
086400110926005                     MERCE SPEDITA CON TROPPO ANTICIPO
086500110926006                     MERCE RESA SENZA AUTORIZZAZIONE
086600110926007                     DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
086700110926008                     DESTINATARIO NON PAGA IL C/ASSEGNO
086800110926009                     CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
086900110926010                     DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
087000110926012                     IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
087100110926013                     DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
087200110926014                     DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
087300110926016                     DESTINATARIO RIMANDA LO SVINCOLO AL
087400110926017                     DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
087500110926018                     DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
087600110926019                     DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
087700110926020                     DESTINATARIO RISULTA TRASFERITO
087800110926021                     DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
087900110926022                     INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
088000110926023                     IL DESTINATARIO E' CHIUSO FINO AL
088100110926024                     IL DESTINATARIO E' CHIUSO PER FERIE
088200110926025                     IL DESTINATARIO E' CHIUSO
088300110926026                     IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
088400110926027                     DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
088500110926028                     ESERCIZIO NON ANCORA IN ATTIVITA
088600110926029                     IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
088700110926030                     IL DESTINATARIO RIFIUTA LA MERCE
088800110926031                     IL DESTINATARIO NON INTENDE
088900110926032                     FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
089000110926033                     IL DESTINATARIO RICHIEDE
089100110926034                     SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
089200110926035                     DOCUMENTI INCOMPLETI O MANCANTI
089300110926036                     IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
089400110926037                     RIFIUTA LA CONSEGNA TASSATIVA
089500110926NON INVIARE             -
089600110926NON INVIARE             -
089700110926NON INVIARE             -
089800110926CONSOK                  CONSEGNA OK
089900110926RESO                    RESO
090000140624MDISTRUTTA              MERCE DISTRUTTA
090100110927*AG                     APERTURA GIACENZA
090200110927*DG                     RICEVUTE DISPOSIZIONI GIACENZA
090300110927*CG                     CHIUSURA GIACENZA
090400110927                        -
090500110927                        -
090600110927                        -
090700110927                        -
090800110927                        -
090900110927                        -
091000110927                        -
091100110926** CAUDES - DESCRIZIONI CAUSALI CLIENTE
091200110103L.AVVISO
091300110103AVVISATO-RIMANDA SVINCOLO 1GG
091400110103LASCIATO AVVISO 1GG
091500110103LASCIATO AVVISO 2GG
091600110103LASCIATO AVVISO 3GG
091700110103LASCIATO AVVISO 5GG
091800110103LASCIATO AVVISO 7GG
091900110103LASCIATO AVVISO 9 GIORNI
092000110103L.A. RIMANDA LO SVINCOLO
092100110103L.A. CHIUSO FINO AL
092200110103CAMBIO PORTO
092300110103COLLO/I DANNEGGIATO/I
092400110103DIROTTAMENTO
092500110103FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
092600110103GIACENZA GENERICA TEMPORANEA
092700110103GIACENZA GENERICA
092800110103ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
092900110103ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
093000110103ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
093100110103ATTENDERE 9 GIORNI
093200110103CHIUSURA SPEDIZIONE CON IDD
093300110103COLLO/I MANCANTE/I
093400110103MESSA IN CONSEGNA
093500110103NON FATTA
093600110103COLLO SCONDIZIONATO
093700110103TOLTA DALLA CONSEGNA
093800110103CONSEGNA PARZIALE
093900110103FESTIVITA PATRONALE
094000110103RISERVA
094100110103RIPRISTINO
094200110103RIFIUTATO
094300110103RIFIUTATO
094400110103RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
094500110103RIPRISTINO
094600110103RISERVA
094700110103CHIUSO PER TURNO
094800110103AMMANCO SENZA EVENTO
094900110103AMMANCO CON EVENTO
095000110103MANCANZA SENZA EVENTO
095100110103MANCANZA CON EVENTO
095200110103AVARIA SENZA EVENTO
095300110103AVARIA CON EVENTO
095400110103ALLUVIONE
095500110103BLOCCO STRADALE
095600110103FESTIVITÀ REGIONALE
095700110103MARE MOLTO MOSSO
095800110103MANIFESTAZIONE PUBBLICA
095900110103NEVICATA ECCEZIONALE
096000110103SCIOPERO
096100110103RIFIUTA SENZA SPECIFICARE IL MOTIVO
096200110103MERCE NON ORDINATA O NON CONFORME
096300110103MERCE SPEDITA IN RITARDO
096400110103MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
096500110103MERCE SPEDITA CON TROPPO ANTICIPO
096600110103MERCE RESA SENZA AUTORIZZAZIONE
096700110103DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
096800110103DESTINATARIO NON PAGA IL C/ASSEGNO
096900110103CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
097000110103DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
097100110103IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
097200110103DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
097300110103DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
097400110103DESTINATARIO RIMANDA LO SVINCOLO AL
097500110103DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
097600110103DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
097700110103DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
097800110103DESTINATARIO RISULTA TRASFERITO
097900110103DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
098000110103INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
098100110103IL DESTINATARIO E' CHIUSO FINO AL
098200110103IL DESTINATARIO E' CHIUSO PER FERIE
098300110103IL DESTINATARIO E' CHIUSO
098400110103IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
098500110103DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
098600110103ESERCIZIO NON ANCORA IN ATTIVITA
098700110103IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
098800110103IL DESTINATARIO RIFIUTA LA MERCE
098900110103IL DESTINATARIO NON INTENDE
099000110103FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
099100110103IL DESTINATARIO RICHIEDE
099200110103SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
099300110103DOCUMENTI INCOMPLETI O MANCANTI
099400110103IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
099500110103RIFIUTA LA CONSEGNA TASSATIVA
099600110103-
099700110103-
099800110103-
099900110103CONSEGNA OK
100000110103RESO
100100140624MERCE DISTRUTTA
100200110927APERTURA GIACENZA
100300110927RICEVUTE DISPOSIZIONI GIACENZA
100400110927CHIUSURA GIACENZA
100500110927-
100600110927-
100700110927-
100800110927-
100900110927-
101000110927-
101100110927-
101200041015** WDCTBAR - TIPI ANOMALIE C/A BARTOLINI
101300110103A                                                                                            1
101400110103M                                                                                            2
101500110103V                                                                                            3
101600110103E                                                                                            4
101700080901** WDCTCLI - CAUSALI CLIENTE
101800110926TAD_A                                                                                        1
101900110926TAD_M                                                                                        2
102000110926TAD_V                                                                                        3
102100110926TAD_E                                                                                        4
102200110926** WDCTDES - DESCRIZIONE TIPI ANOMALIE C/A BARTOLINI
102300110927AMMANCO                                                                                      1
102400110927MANCA COLLO                                                                                  2
102500110927AVARIA                                                                                       3
102600110927EVENTO                                                                                       4
102700110103** WCOD9BAR - CODICI EVENTO "PERSONALIZZATI" - BARTOLINI
102800110103INCARICO1                                                                                    1
102900110103INCARICO2                                                                                    2
103000110103CONSOK1                                                                                      3
103100110103CONSOK2                                                                                      4
103200110927GIACNODIS1                                                                                   5
103300110927GIACNODIS2                                                                                   6
103400110927GIACSIDIS1                                                                                   7
103500110927GIACSIDIS2                                                                                   8
103600110927                                                                                             9
103700110927                                                                                             10
103800110103** WCOD9CLI - CODICI EVENTO "PERSONALIZZATI" - CLIENTE
103900110926ACCETTAZIONE                                                                                 1
104000110926ACCETTAZIONE                                                                                 2
104100110103NON INVIARE                                                                                  3
104200110103NON INVIARE                                                                                  4
104300110927GIAC_APERTA                                                                                  5
104400110927GIAC_APERTA                                                                                  6
104500110927GIAC_DISPOS                                                                                  7
104600110927GIAC_DISPOS                                                                                  8
104700110927NON INVIARE                                                                                  9
104800110927NON INVIARE                                                                                  10
104900110926** WCOD9DES - DESCRIZIONI CODICI EVENTO "PERSONALIZZATI"
105000110926ACCETTAZIONE SPEDIZIONE (TRANSITO)                                                           1
105100110926ACCETTAZIONE SPEDIZIONE (TRANSITO)                                                           2
105200110926NON INVIARE                                                                                  3
105300110926NON INVIARE                                                                                  4
105400110927APERTURA GIACENZA                                                                            5
105500110927APERTURA GIACENZA                                                                            6
105600110927RICEVUTE DISPOSIZIONI                                                                        7
105700110927RICEVUTE DISPOSIZIONI                                                                        8
105800110927NON INVIARE                                                                                  8
105900110927NON INVIARE                                                                                  10
