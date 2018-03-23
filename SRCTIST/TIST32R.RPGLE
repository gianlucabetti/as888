000100000000     H*------------------------------------------------------------------------*
000200040827     H* SITUAZIONE CLIENTI COME VAS
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800040827     FTIVSS02L  IF   E           K DISK
000900040827     FTIVTF01L  IF   E           K DISK
001000040827     FTIVIR05L  IF   E           K DISK
001100040827     FTIVLT05L  IF   E           K DISK
001200040827     FTIVLR01L  IF   E           K DISK
001300050209     FTIVTL12L  IF   E           K DISK
001400040827     FSGIUS     IF   E           K DISK
001500090918     FTIVTA12L  IF   E           K DISK
001600000000     FTABEL00F  IF   E           K DISK
001700040827     FTNTBE01L  IF   E           K DISK
001800000000     FAZORG01L  IF   E           K DISK
001900040827     FTIST32P   O    E             PRINTER OFLIND(*IN05)
002000040906     FWFST320F  UF A E             DISK
002100000000     D*------------------------------------------------------------------------*
002200000000     D* SCHIERE
002300000000     D*------------------------------------------------------------------------*
002400040827     D*-------------------
002500040827     D* SCHIERE DIVISIONI/AREE/FILIALI
002600040827     D*-------------------
002700040827     D skFIL           S              3  0 DIM(500)
002800040831     D skARE           S              3  0 DIM(500)
002900040827     D skDIV           S              1    DIM(500)
003000040827     D*-------------------
003100040827     D* SCHIERA SERVIZI DIVISI X MACRO-TIPOLOGIA (T&T ED ALTRO)
003200040827     D*-------------------
003300050608     D skISVTT         S              2    DIM(500)
003400050608     D skISVUPL        S              2    DIM(500)
003500050608     D skISVDWL        S              2    DIM(500)
003600040827     D*-------------------
003700040831     D* SCHIERE DI WRK X CARICAMENTO DATI DEL CLIENTE DA VARI FILE DI LOG (T&T - UPL - DWL - etc...)
003800040827     D*-------------------
003900050608     D skVSS           S             19    DIM(500)
004000050608     D skVIR           S              9    DIM(500)
004100050608     D skUPL           S              4    DIM(500)
004200050608     D skDWL           S              4    DIM(500)
004300050608     D skTT            S              2    DIM(500)
004400000000     D*------------------------------------------------------------------------*
004500000000     D* INPUT
004600000000     D*------------------------------------------------------------------------*
004700000000     D*-------------------
004800000000     D* ARCHITETTURA
004900000000     D*-------------------
005000000000     D KPJBA         E DS
005100000000     D*-------------------
005200000000     D* DS PARAMETRI DI LANCIO PGM
005300000000     D*-------------------
005400040827     D PARAM           DS
005500040827     D  PARDIV                        1
005600040827     D  PARARE                        3  0
005700040827     D  PARFIL                        3  0
005800040827     D  PARKSC                        7  0
005900060109     D  PARTTNU                       1
006000060109     D  PARSDNU                       1
006100060109     D  PARDISA                       1
006200060109     D  PARWTAB                       1
006300040827     D*-------------------
006400040827     D* DS CONTROLLA DATA (8)
006500040827     D*-------------------
006600040827     D WLBDA8          DS
006700040827     D  G08DAT                 1      8  0
006800040827     D  G08INV                 9     16  0
006900040827     D  G08ERR                17     17
007000040827     D  G08TGI                18     22  0
007100090528     D*------------------
007200090528     D* DS REPERIMENTO SALDI CLIENTE
007300090528     D*-------------------
007400090528     D TISE70DS      E DS
007500040901     D*-------------------
007600040901     D* DS REPERIMENTO DATI UTENTE
007700040901     D*-------------------
007800040901     DBS69DS         E DS                  extname(TIBS69DS)
007900040901     DACODS          E DS                  extname(CNACO00F)
008000040901     DINDDS          E DS                  extname(CNIND00F)
008100040901     DCLPDS          E DS                  extname(CNCLP00F)
008200040901     DCLSDS          E DS                  extname(FNCLS00F)
008300040827     D*-------------------
008400040827     D* DS TABELLA "ISV"
008500040827     D*-------------------
008600040827     D DISV          E DS
008700040831     D*-------------------
008800040831     D* DS RIDEFINIZIONE SCHIERE D WRK X REPERIMENTO DATI CLIENTE ABILITATO CORRENTE
008900040831     D*-------------------
009000040831     D DSUPLDWL        DS
009100040831     D  UDTipISV                      2
009200040831     D  UDTipFIL                      2
009300040831     D*
009400040831     D DSVIR           DS
009500040831     D  VIRTipISV                     2
009600040831     D  VIRTipFIL                     2
009700040831     D  VIRFlgATT                     1
009800040831     D  VIRFlgTAB                     1
009900040831     D  VIRFlgTABCOD                  2
010000040831     D  VIRFlgVTF                     1
010100040831     D*
010200040831     D DSVSS           DS
010300040901     D  VSSUsrSGI                     9
010400040901     D  VSSCodCli                     7S 0
010500040831     D  VSSTipISV                     2
010600040831     D  VSSFlgATT                     1
010700040831     D*-------------------
010800040831     D* VARIABILI D WRK
010900040831     D*-------------------
011000040831     D depVSSKSU       S                   like(VSSKSU) inz(*blanks)
011100040831     D depVSSSGI       S                   like(VSSSUN) inz(*blanks)
011200040901     D depSTPSGI       S                   like(STPSGI) inz(*zeros)
011300040903     D wCODERR         S              2    inz(*blanks)
011400040903     D wTIPISV         S              2    inz(*blanks)
011500040903     D wTIPITF         S              2    inz(*blanks)
011600000000     C*------------------------------------------------------------------------*
011700000000     C* MAIN LINE
011800000000     C*------------------------------------------------------------------------*
011900000000     C*
012000040827     C* CARICAMENTO TABELLE
012100040827     C                   EXSR      CARTAB
012200000000     C*
012300040827     C* ELABORAZIONE
012400040827     C                   EXSR      CHKVSS
012500040901     C*
012600040901     C* CHIUSURA
012700040901     C                   EXSR      FINSR
012800000000     C*
012900991123     C                   SETON                                        LR
013000040827     C*------------------------------------------------------------------------*
013100040827     C* CHKVSS - ROUTINE PRINCIPALE DI CONTROLLO CLIENTI COME VAS
013200040827     C*------------------------------------------------------------------------*
013300040827     C     CHKVSS        BEGSR
013400040827     C*
013500040827     C* VALORIZZO LA TESTATA IN RELAZIONE AI PARAMETRI DI LANCIO RICHIESTI
013600040827     C                   exsr      valpartes
013700040827     C*
013800040827     C* STAMPO LA TESTATA PROSPETTO E RESETTO INDICATORE DI SALTO PAGINA
013900040827     C                   write     st32te
014000040827     C                   setoff                                       05
014100040827     C*
014200040827     C* LEGGO TUTTO IL FILE DELLE ABILITAZIONI AI SERVIZI VAS (TIVSS)
014300040827     C     *loval        setll     tivss02L
014400040827     C                   read      tivss02l
014500040827     C                   dow       not %eof(tivss02l)
014600040827     C*
014700040827     C* VERIFICO SE IL RECORD CORRENTE SODDISFA I PARAMETRI RICHIESTI NEL LANCIO
014800040831     C                   exsr      chkrec
014900040827     C                   if        WRECOK = 'S'
015000040831     C*
015100040831     C* VERIFICO ROTTURA D CODICE X CLIENTE ABILITATO IN TIVSS
015200040831     C                   if        VSSKSU <> depVSSKSU
015300040831     C*
015400040831     C* SE NN PRIMA LETTURA A ROTTURA CLIENTE ABILITATO ESEGUO ROUTINE D CONTROLLI
015500040831     C                   if        depVSSKSU <> *blanks
015600040831     C                   exsr      chkksu
015700040831     C                   endif
015800040831     C*
015900040831     C* ESEGUO ROUTINE REPERIMENTO REGOLE UPLOAD/DOWNLOAD
016000040831     C                   exsr      repvir
016100040831     C*
016200040831     C* ESEGUO ROUTINE REPERIMENTO LOG UPLOAD
016300040831     C                   exsr      repupl
016400040831     C*
016500040831     C* ESEGUO ROUTINE REPERIMENTO LOG DOWNLOAD
016600040831     C                   exsr      repdwl
016700040831     C*
016800040831     C* ESEGUO ROUTINE REPERIMENTO T&T
016900040831     C                   exsr      reptt
017000040831     C*
017100040831     C* ESEGUO ROUTINE REPERIMENTO SGI
017200040831     C                   exsr      repsgi
017300040903     C*
017400040903     C* ESEGUO ROUTINE REPERIMENTO TIVTA
017500040903     C                   exsr      repvta
017600040831     C*
017700040831     C* INIZIALIZZO SHIERA D WRK X MEMORIZZAZIONE ABILITAZIONE AI SERVIZI
017800040831     C                   z-add     *zeros        jVSS              3 0
017900040901     C                   clear                   skVSS
018000040831     C*
018100040831     C* SALVO IL CLIENTE ABILITATO CORRENTE
018200040831     C                   eval      depVSSKSU = VSSKSU
018300040831     C                   endif
018400040901     C*
018500040901     C                   exsr      memvss
018600040827     C                   endif
018700040827     C*
018800040827     C* QUINDI PROSEGUO CON LA LETTURA DEL FILE DELLE ABILITAZIONI CLIENTI AI SERVIZI VAS (TIVSS)
018900040827     C                   read      tivss02l
019000040827     C                   enddo
019100040827     C*
019200040827     C* STAMPO LA CHIUSURA PROSPETTO
019300040827     C                   write     st32ri
019400040827     C                   write     st32fi
019500040827     C*
019600040827     C                   ENDSR
019700040901     C*------------------------------------------------------------------------*
019800040901     C* MEMVSS - MEMORIZZAZOINE SCHIERA WRK ABILITAZIONI SERVIZI AI CLIENTI (TIVSS)
019900040901     C*------------------------------------------------------------------------*
020000040901     C     MEMVSS        BEGSR
020100040901     C*
020200040901     C* X MEDESIMO CLIENTE ABILITATO AI SERVIZI MEMORIZZO LA SCHIERA D WRK RELATIVA
020300040901     C                   clear                   DSVSS
020400040901     C                   eval      VSSUsrSGI = vssSUN
020500040901     C                   move(p)   vssKSU        VSSCodCli
020600040901     C                   eval      VSSTipISV = vssISV
020700040901     C*
020800040901     C* VERIFICO SE L'ABILITAZIONE AL SERVIZIO AL MOMENTO DEL LANCIO E' ATTIVA
020900040901     C* (x nn attiva intendo unicamente se la data scadenza è già trascorsa)
021000040901     C                   if        datcor > vssDSC
021100040901     C                   eval      VSSFlgATT = 'X'
021200040901     C                   endif
021300040901     C*
021400040901     C* Se nn già presente in schiera d wrk d memorizzazione => memorizzo
021500040901     C     DSVSS         lookup    skVSS                                  55
021600040901     C                   if        %found
021700040901     C                   else
021800040901     C                   add       1             jVSS
021900040901     C                   eval      skVSS(jVSS) = DSVSS
022000040901     C                   endif
022100040901     C*
022200040901     C                   ENDSR
022300040831     C*------------------------------------------------------------------------*
022400040831     C* REPVIR - REPERIMENTO DATI CLIENTE ABILITATO DA REGOLE (TIVIR)
022500040831     C*------------------------------------------------------------------------*
022600040831     C     REPVIR        BEGSR
022700040831     C*
022800040831     C* Leggo tutte le regole attualmente impostate x il cliente abilitato corrente e memorizzo
022900040831     C* se nn già presente
023000040831     C                   z-add     *zeros        jVIR              3 0
023100040831     C                   clear                   skVIR
023200040831     C     VSSKSU        setll     tivir05l
023300040831     C                   if        %equal(tivir05l)
023400040831     C     VSSKSU        reade     tivir05l
023500040831     C                   dow       not %eof(tivir05l)
023600040831     C                   clear                   DSVIR
023700040831     C                   eval      VIRTipISV = virFI1
023800040831     C                   eval      VIRTipFIL = virTIP
023900040831     C*
024000040831     C* Verifico se la regole al momento del lancio è attiva o meno
024100040831     C* (x nn attiva intendo unicamente se la data scadenza è già trascorsa)
024200040831     C                   if        datcor > virDTF
024300040831     C                   eval      VIRFlgATT = 'X'
024400040831     C                   endif
024500040831     C*
024600040831     C* Reperisco dall'anagrafica tipi-file standard l'eventuale tabella associata al tipo-file
024700040831     C     virTIP        chain     tivtf01l
024800040831     C                   if        %found(tivtf01l)
024900040831     C*
025000040831     C                   if        vtfFI1 <> *blanks
025100040831     C*
025200040831     C                   movel     1             TBLKUT
025300040831     C                   move      vtfFI1        TBLCOD
025400040831     C                   move(P)   VSSKSU        wVSSKSU
025500040831     C                   movel(P)  wVSSKSU       TBLKEY
025600040831     C     KEYTAB        chain     TABEL00F                           97
025700040831     C     *IN97         ifeq      '1'
025800040831     C     TBLFLG        oreq      '*'
025900040831     C                   eval      VIRFlgTAB = 'X'
026000040831     C                   endif
026100040901     C                   eval      VIRFlgTABCOD = vtfFI1
026200040901     C*
026300040831     C                   endif
026400040831     C*
026500040831     C                   else
026600040901     C                   eval      VIRFlgVTF = 'X'
026700040831     C                   endif
026800040831     C*
026900040831     C* Se nn già presente in schiera d wrk d memorizzazione => memorizzo
027000040831     C     DSVIR         lookup    skVIR                                  55
027100040831     C                   if        %found
027200040831     C                   else
027300040831     C                   add       1             jVIR
027400040831     C                   eval      skVIR(jVIR) = DSVIR
027500040831     C                   endif
027600040831     C*
027700040831     C     VSSKSU        reade     tivir05l
027800040831     C                   enddo
027900040831     C                   endif
028000040831     C*
028100040831     C                   ENDSR
028200040831     C*------------------------------------------------------------------------*
028300040831     C* REPUPL - REPERIMENTO DATI CLIENTE ABILITATO DA LOG UPLOAD (TIVLR)
028400040831     C*------------------------------------------------------------------------*
028500040831     C     REPUPL        BEGSR
028600040831     C*
028700040831     C* Leggo tutti i dati del log UPLOAD del cliente abilitato corrente e memorizzo
028800040831     C* se nn già presente
028900040831     C                   z-add     *zeros        jUPL              3 0
029000040831     C                   clear                   skUPL
029100040831     C     VSSKSU        setll     tivlr01l
029200040831     C                   if        %equal(tivlr01l)
029300040831     C     VSSKSU        reade     tivlr01l
029400040831     C                   dow       not %eof(tivlr01l)
029500040831     C                   clear                   DSUPLDWL
029600040831     C                   eval      UDTipISV = vlrISV
029700040831     C                   eval      UDTipFIL = vlrTIP
029800040831     C*
029900040831     C* Se nn già presente in schiera d wrk d memorizzazione => memorizzo
030000040831     C     DSUPLDWL      lookup    skUPL                                  55
030100040831     C                   if        %found
030200040831     C                   else
030300040831     C                   add       1             jUPL
030400040831     C                   eval      skUPL(jUPL) = DSUPLDWL
030500040831     C                   endif
030600040831     C*
030700040831     C     VSSKSU        reade     tivlr01l
030800040831     C                   enddo
030900040831     C                   endif
031000040831     C*
031100040831     C                   ENDSR
031200040831     C*------------------------------------------------------------------------*
031300040831     C* REPDWL - REPERIMENTO DATI CLIENTE ABILITATO DA LOG DOWNLOAD (TIVLT)
031400040831     C*------------------------------------------------------------------------*
031500040831     C     REPDWL        BEGSR
031600040831     C*
031700040831     C* Leggo tutti i dati del log DOWNLOAD del cliente abilitato corrente e memorizzo
031800040831     C* se nn già presente
031900040831     C                   z-add     *zeros        jDWL              3 0
032000040831     C                   clear                   skDWL
032100040831     C     VSSKSU        setll     tivlt05l
032200040831     C                   if        %equal(tivlt05l)
032300040831     C     VSSKSU        reade     tivlt05l
032400040831     C                   dow       not %eof(tivlt05l)
032500040831     C                   clear                   DSUPLDWL
032600040831     C                   eval      UDTipISV = vltISV
032700040831     C                   eval      UDTipFIL = vltTIP
032800040831     C*
032900040831     C* Se nn già presente in schiera d wrk d memorizzazione => memorizzo
033000040831     C     DSUPLDWL      lookup    skDWL                                  55
033100040831     C                   if        %found
033200040831     C                   else
033300040831     C                   add       1             jDWL
033400040831     C                   eval      skDWL(jDWL) = DSUPLDWL
033500040831     C                   endif
033600040831     C*
033700040831     C     VSSKSU        reade     tivlt05l
033800040831     C                   enddo
033900040831     C                   endif
034000040831     C*
034100040831     C                   ENDSR
034200040831     C*------------------------------------------------------------------------*
034300040831     C* REPTT  - REPERIMENTO DATI CLIENTE ABILITATO DA LOG T&T (TIVTL)
034400040831     C*------------------------------------------------------------------------*
034500040831     C     REPTT         BEGSR
034600040831     C*
034700040831     C* Leggo tutti i dati del log T&T del cliente abilitato corrente e memorizzo
034800040831     C* se nn già presente
034900040831     C                   z-add     *zeros        jTT               3 0
035000040831     C                   clear                   skTT
035100050209     C     VSSKSU        setll     tivtl12l
035200050209     C                   if        %equal(tivtl12l)
035300050209     C     VSSKSU        reade     tivtl12l
035400050209     C                   dow       not %eof(tivtl12l)
035500040903     C*
035600040903     C* X ogni tipo record del log del T&T considero sempre e cmq presente anche il tipo record
035700040903     C* TT in quanto il servizio TT è 1 sorta d servizio "generico" in quanto prerequisito
035800040903     C* d atri servizi
035900040903     C     'TT'          lookup    skTT                                   55
036000040903     C                   if        %found
036100040903     C                   else
036200040903     C                   add       1             jTT
036300040903     C                   eval      skTT(jTT) = vtlTIP
036400040903     C                   endif
036500040831     C*
036600040831     C* Paletto alcune considerazioni
036700040831     C                   if        vtlTIP = 'RB' OR
036800041227     C                             vtlTIP = 'DB' OR
036900041227     C                             vtlTIP = 'DM' OR
037000040831     C                             vtlTIP = 'HB' OR
037100041227     C                             vtlTIP = 'T1' OR
037200041227     C                             vtlTIP = 'T2' OR
037300041227     C                             vtlTIP = 'T3' OR
037400041227     C                             vtlTIP = 'TC' OR
037500041227     C                             vtlTIP = 'LV'
037600040831     C                   eval      vtlTIP = 'TT'
037700040831     C                   endif
037800040831     C*
037900040831     C* Se nn già presente in schiera d wrk d memorizzazione => memorizzo
038000040831     C     vtlTIP        lookup    skTT                                   55
038100040831     C                   if        %found
038200040831     C                   else
038300040831     C                   add       1             jTT
038400040831     C                   eval      skTT(jTT) = vtlTIP
038500040831     C                   endif
038600040831     C*
038700050209     C     VSSKSU        reade     tivtl12l
038800040831     C                   enddo
038900040831     C                   endif
039000040831     C*
039100040831     C                   ENDSR
039200040831     C*------------------------------------------------------------------------*
039300040831     C* REPSGI - REPERIMENTO DATI CLIENTE ABILITATO SU FILE UTENTI STRATEGI
039400040831     C*------------------------------------------------------------------------*
039500040831     C     REPSGI        BEGSR
039600040831     C*
039700040831     C                   movel     *blanks       SGIFlgATT         1
039800040831     C     VSSSUN        chain     sgius
039900040831     C                   if        %found(sgius)
040000040831     C                   if        usSTAT = '1'
040100040831     C                   movel     'S'           SGIFlgATT
040200040831     C                   else
040300040831     C                   movel     'N'           SGIFlgATT
040400040831     C                   endif
040500040831     C                   endif
040600040831     C*
040700040831     C                   ENDSR
040800040903     C*------------------------------------------------------------------------*
040900040903     C* REPVTA - REPERIMENTO BOLLE IN FILE ESTRATTO BOLLE X T&T (TIVTA)
041000040903     C*------------------------------------------------------------------------*
041100040903     C     REPVTA        BEGSR
041200040903     C*
041300040903     C                   movel     *blanks       VTAFlgSINO        1
041400090918     C     VSSKSU        chain     tivta12l
041500090918     C                   if        %found(tivta12l)
041600040903     C                   movel     'S'           VTAFlgSINO
041700040903     C                   else
041800040903     C                   movel     'N'           VTAFlgSINO
041900040903     C                   endif
042000040903     C*
042100040903     C                   ENDSR
042200040827     C*------------------------------------------------------------------------*
042300040831     C* CHKKSU - ROUTINE D CONTROLLO GENERALE PARTENDO DALLE ABILITAZIONI CLIENTI AI SERVIZI (TIVSS)
042400040831     C*------------------------------------------------------------------------*
042500040831     C     CHKKSU        BEGSR
042600040831     C*
042700040831     C* INNANZITUTTO ORDONO LA SCHIERA DELLE ABILITAZIONI CLIENTI AI SERVIZI
042800040831     C                   SORTA     skVSS
042900040831     C*
043000040831     C* INIZIALIZZO IL DEPOSITO DELL'UTENTE SGI (IN QUANTO X STESSO CLIENTE POSSONO ESSERCI + SGIUS)
043100040831     C                   MOVEL     *blanks       depVSSSGI
043200040831     C*
043300040831     C* SCORRO LA SCHIERA DELLE ABILITAZIONI DEL CLIENTE AI SERVIZI
043400040831     C                   Z-ADD     1             jWrk              3 0
043500040831     C                   DOW       jWrk <= %elem(skVSS)
043600040831     C                   IF        skVSS(jWrk) <> *blanks
043700040831     C* RIDEFINISCO L'ELEMENTO DELLA SCHIERA
043800040831     C                   EVAL      DSVSS = skVSS(jWrk)
043900040831     C**********
044000040831     C* CONTROLLO SERVIZIO UPLOAD O DOWNLOAD DISABILITATO
044100040831     C**********
044200040831     C                   IF        VSSFlgATT = 'X'
044300040901     C* ESCLUDO IL SERVIZIO TT DA UPLOAD E DOWNLOAD IN QUANTO IN QUESTO CASO TRATTASI
044400040901     C* DI DATA-ENTRY ON-LINE X IL QUALE NN E' PREVISTA ABILITAZIONE AL SERVIZIO
044500040901     C                   IF        VSSTipISV <> 'TT'
044600040831     C     VSSTipISV     LOOKUP    skISVUPL                               55
044700040831     C                   IF        %found
044800040831     C                   EVAL      STPERR = 'Cliente disabilitato al servizio '+
044900040831     C                             VSSTipISV + ' => verificare tipi-file.'
045000040903     C                   EVAL      wCODERR = 'AF'
045100040903     C                   EVAL      wTIPISV = VSSTipISV
045200040903     C                   EVAL      wTIPITF = *blanks
045300040901     C                   EXSR      WRIDET
045400040831     C                   ELSE
045500040831     C     VSSTipISV     LOOKUP    skISVDWL                               55
045600040831     C                   IF        %found
045700040831     C                   EVAL      STPERR = 'Cliente disabilitato al servizio '+
045800040831     C                             VSSTipISV + ' => verificare tipi-file.'
045900040903     C                   EVAL      wCODERR = 'AF'
046000040903     C                   EVAL      wTIPISV = VSSTipISV
046100040903     C                   EVAL      wTIPITF = *blanks
046200040901     C                   EXSR      WRIDET
046300040831     C                   ENDIF
046400040901     C                   ENDIF
046500040831     C                   ENDIF
046600040831     C                   ENDIF
046700040831     C**********
046800040831     C* CONTROLLO STRATEGI USER
046900040831     C**********
047000040827     C*
047100040831     C* X OGNI DIFFERENTE SGI USER => EFFETTUO CONSIDERAZIONI
047200040831     C                   IF        VSSUsrSGI <> depVSSSGI
047300040831     C*
047400040831     C* SE IL SERVIZIO E' D TIPO "STRATEGI" VERIFICO LO STATO DELL'UTENTE STRATEGI CORRELATO
047500040831     C                   IF        SGIFlgATT <> *blanks
047600040831     C* SE SERVIZIO ATTIVO E UTENTE STRATEGI DISATTIVO => LISTO ANOMALIA
047700040831     C                   IF        VSSFlgATT = *blanks AND
047800040831     C                             SGIFlgATT = 'N'
047900040831     C                   EVAL      STPERR = 'Cliente abilitato al servizio ' +
048000040831     C                             VSSTipISV + ' ma utente SGI ' + VSSUsrSGI +
048100040831     C                             ' disabilitato.'
048200040903     C                   EVAL      wCODERR = 'AS'
048300040903     C                   EVAL      wTIPISV = VSSTipISV
048400040903     C                   EVAL      wTIPITF = *blanks
048500040901     C                   EXSR      WRIDET
048600040831     C                   ENDIF
048700040831     C* SE SERVIZIO DISATTIVO E UTENTE STRATEGI ATTIVO => LISTO ANOMALIA
048800040831     C                   IF        VSSFlgATT = 'X' AND
048900040831     C                             SGIFlgATT = 'S'
049000040831     C                   EVAL      STPERR = 'Cliente disabilitato al servizio '+
049100040831     C                             VSSTipISV + ' ma utente SGI ' + VSSUsrSGI +
049200040831     C                             ' abilitato.'
049300040903     C                   EVAL      wCODERR = 'AS'
049400040903     C                   EVAL      wTIPISV = VSSTipISV
049500040903     C                   EVAL      wTIPITF = *blanks
049600040901     C                   EXSR      WRIDET
049700040831     C                   ENDIF
049800040831     C                   ENDIF
049900040831     C*
050000040831     C* SALVO L'ULTIMO SGI USER VERIFICATO
050100040831     C                   EVAL      depVSSSGI = VSSUsrSGI
050200040831     C                   ENDIF
050300040831     C**********
050400040831     C* CONTROLLO DISCORDANZE SERVIZI / REGOLE TIPI-FILE
050500040831     C**********
050600040901     C* ESCLUDO IL SERVIZIO TT DA UPLOAD E DOWNLOAD IN QUANTO IN QUESTO CASO TRATTASI
050700040901     C* DI DATA-ENTRY ON-LINE X IL QUALE NN E' PREVISTA ABILITAZIONE AL SERVIZIO
050800040901     C                   IF        VSSTipISV <> 'TT'
050900040831     C*
051000040831     C* SOLO SE TRATTASI D SERVIZIO D TIPO UPLOAD O DOWNLOAD
051100040831     C     VSSTipISV     LOOKUP    skISVDWL                               55
051200040831     C     VSSTipISV     LOOKUP    skISVUPL                               54
051300040831     C                   IF        *IN54 = *on OR
051400040831     C                             *IN55 = *on
051500040831     C*
051600040831     C* SE SERVIZIO ATTIVO E UTENTE NN HA TIPI FILE ATTIVI SU TALE SERVIZIO => LISTO ANOMALIA
051700040831     C                   IF        VSSFlgATT = *blanks
051800040831     C                   Z-ADD     1             jWrk2             3 0
051900040831     C                   MOVEL     'N'           wVIR_OK           1
052000040831     C                   DOW       jWrk2 <= %elem(skVIR)
052100040831     C                   IF        skVIR(jWrk2) <> *blanks
052200040831     C* RIDEFINISCO L'ELEMENTO DELLA SCHIERA
052300040831     C                   EVAL      DSVIR = skVIR(jWrk2)
052400040831     C                   IF        VIRTipISV = VSSTipISV
052500040831     C                   IF        VIRFlgATT = 'X'
052600040831     C                   EVAL      STPERR = 'Cliente abilitato al servizio ' +
052700040831     C                             VSSTipISV + ' ma tipo file ' + VIRTipFIL  +
052800040831     C                             ' disabilitato.'
052900040903     C                   EVAL      wCODERR = 'AF'
053000040903     C                   EVAL      wTIPISV = VSSTipISV
053100040903     C                   EVAL      wTIPITF = VIRTipFIL
053200040901     C                   EXSR      WRIDET
053300040831     C                   ELSE
053400040831     C                   MOVEL     'S'           wVIR_OK
053500040831     C                   ENDIF
053600040831     C                   ENDIF
053700040831     C                   ELSE
053800040831     C                   LEAVE
053900040831     C                   ENDIF
054000040831     C*
054100040831     C                   ADD       1             jWrk2
054200040831     C                   ENDDO
054300040831     C*
054400040831     C* VERIFICO SE CONTROLLO OK
054500040831     C                   IF        wVIR_OK = 'N'
054600040831     C                   EVAL      STPERR = 'Cliente abilitato al servizio ' +
054700040831     C                             VSSTipISV + ' ma servizio nn utilizzato.'
054800040903     C                   EVAL      wCODERR = 'A*'
054900040903     C                   EVAL      wTIPISV = VSSTipISV
055000040903     C                   EVAL      wTIPITF = *blanks
055100040901     C                   EXSR      WRIDET
055200040831     C                   ENDIF
055300040831     C                   ENDIF
055400040901     C                   ENDIF
055500040831     C                   ENDIF
055600040831     C**********
055700040831     C* CONTROLLO INUTILIZZO SERVIZI T&T
055800040831     C**********
055900040831     C*
056000040831     C* SOLO SE TRATTASI D SERVIZIO D TIPO T&T
056100040831     C     VSSTipISV     LOOKUP    skISVTT                                55
056200040831     C                   IF        *IN55 = *on
056300040831     C*
056400040831     C* SE SERVIZIO ATTIVO E UTENTE NN HA LOG D TIPO T&T => LISTO ANOMALIA
056500040831     C                   IF        VSSFlgATT = *blanks
056600040831     C     VSSTipISV     LOOKUP    skTT                                   55
056700040831     C                   IF        %found
056800040831     C                   ELSE
056900090528     C*
057000090528     C* X il tipo servizio TT il "nn utilizzo" è subordinato anche alla nn presenza
057100090528     C* d bolle nell'ultimo anno e dalla data attivazione servizio più vecchia sempre
057200090528     C* d un anno
057300090528     C                   MOVEL(P)  *zeros        wAAAAMMi          6 0
057400090528     C                   MOVEL(P)  datcor        wAAAAMMf          6 0
057500090528     C                   EVAL      wAAAAMMi = wAAAAMMf - 100
057600090528     C*
057700090528     C                   CLEAR                   TISE70DS
057800090528     C                   EVAL      I70TLA = 'U'
057900090528     C                   EVAL      I70TLE = 'WW'
058000090528     C                   EVAL      I70KSC = VSSCodCli
058100090528     C                   EVAL      I70DAI = wAAAAMMi
058200090528     C                   EVAL      I70DAF = wAAAAMMf
058300090528     C                   CALL      'TISE70R'
058400090528     C                   PARM                    TISE70DS
058500090528     C                   IF        O70NSP = *zeros
058600090528     C                   IF        vssDDE <= datcor - 10000
058700090528     C*
058800040831     C                   EVAL      STPERR = 'Cliente abilitato al servizio ' +
058900040831     C                             VSSTipISV + ' ma servizio nn utilizzato.'
059000040903     C                   EVAL      wCODERR = 'A*'
059100040903     C                   EVAL      wTIPISV = VSSTipISV
059200040903     C                   EVAL      wTIPITF = *blanks
059300040901     C                   EXSR      WRIDET
059400090528     C*
059500090528     C                   ENDIF
059600090528     C                   ENDIF
059700090528     C*
059800040831     C                   ENDIF
059900040831     C                   ENDIF
060000040831     C                   ENDIF
060100040831     C*
060200040831     C                   ENDIF
060300040831     C                   ADD       1             jWrk
060400040831     C                   ENDDO
060500040831     C**********
060600040831     C* CONTROLLO DISCORDANZE REGOLE TIPI-FILE
060700040831     C**********
060800040831     C*
060900040831     C* SCORRO LA SCHIERA DELLE REGOLE CLIENTI / TIPI-FILE
061000040831     C                   Z-ADD     1             jWrk              3 0
061100040831     C                   DOW       jWrk <= %elem(skVIR)
061200040831     C                   IF        skVIR(jWrk) <> *blanks
061300040831     C* RIDEFINISCO L'ELEMENTO DELLA SCHIERA
061400040831     C                   EVAL      DSVIR = skVIR(jWrk)
061500040831     C**********
061600040831     C* CONTROLLO REGOLE TIPI-FILE: DISABILITATI / TABELLE / REGOLE STANDARD
061700040831     C**********
061800040831     C*
061900040831     C* SE TIPO FILE DISABILITATO => VERIFICO ANCHE TABELLA CORRELATA => LISTO ANOMALIA
062000040831     C                   IF        VIRFlgATT = 'X'
062100040831     C                   IF        VIRFlgTAB = *blanks
062200040831     C                   EVAL      STPERR = 'Tipo file ' + VIRTipFIL +
062300040831     C                             ' disabilitato ma presente ancora'+
062400040831     C                             ' in tabella ' + VIRFlgTABCOD
062500040903     C                   EVAL      wCODERR = 'FT'
062600040903     C                   EVAL      wTIPISV = *blanks
062700040903     C                   EVAL      wTIPITF = VIRTipFIL
062800040901     C                   EXSR      WRIDET
062900040831     C                   ELSE
063000040831     C                   EVAL      STPERR = 'Tipo file ' + VIRTipFIL +
063100040831     C                             ' disabilitato.'
063200040903     C                   EVAL      wCODERR = 'F'
063300040903     C                   EVAL      wTIPISV = *blanks
063400040903     C                   EVAL      wTIPITF = VIRTipFIL
063500040901     C                   EXSR      WRIDET
063600040831     C                   ENDIF
063700040831     C                   ENDIF
063800040831     C*
063900040831     C* SE TIPO FILE ABILITATO => VERIFICO ANCHE TABELLA CORRELATA => LISTO ANOMALIA
064000040831     C                   IF        VIRFlgATT = *blanks AND
064100040831     C                             VIRFlgTAB = 'X'
064200090528     C*
064300090528     C* X il tipo file VT nn listo questa anomalia
064400090528     C                   IF        VIRTipFIL <> 'VT'
064500040831     C                   EVAL      STPERR = 'Tipo file ' + VIRTipFIL +
064600040831     C                             ' abilitato ma nn abilitato in '  +
064700040901     C                             'tabella ' + VIRFlgTABCOD
064800040903     C                   EVAL      wCODERR = 'FT'
064900040903     C                   EVAL      wTIPISV = *blanks
065000040903     C                   EVAL      wTIPITF = VIRTipFIL
065100040901     C                   EXSR      WRIDET
065200090528     C                   ENDIF
065300040831     C                   ENDIF
065400040831     C*
065500040831     C* SE TIPO FILE NN PRESENTE ANCHE IN REGOLE STANDARD => LISTO ANOMALIA
065600040831     C                   IF        VIRFlgVTF = 'X'
065700040831     C                   EVAL      STPERR = 'Tipo file ' + VIRTipFIL +
065800040831     C                             ' nn presente in anagrafica standard (TIVTF)'
065900040903     C                   EVAL      wCODERR = 'F'
066000040903     C                   EVAL      wTIPISV = *blanks
066100040903     C                   EVAL      wTIPITF = VIRTipFIL
066200040901     C                   EXSR      WRIDET
066300040831     C                   ENDIF
066400040831     C**********
066500040831     C* CONTROLLO REGOLE TIPI-FILE / SERVIZI
066600040831     C**********
066700040831     C*
066800040831     C* SE TIPO FILE ABBINATO AD 1 SERVIZIO NN ABILITAO AL CLIENTE => LISTO ANOMALIA
066900040831     C                   Z-ADD     1             jWrk2             3 0
067000040831     C                   MOVEL     'N'           wVIR_OK           1
067100040831     C                   DOW       jWrk2 <= %elem(skVSS)
067200040831     C                   IF        skVSS(jWrk2) <> *blanks
067300040831     C* RIDEFINISCO L'ELEMENTO DELLA SCHIERA
067400040831     C                   EVAL      DSVSS = skVSS(jWrk2)
067500040831     C                   IF        VIRTipISV = VSSTipISV
067600040831     C                   MOVEL     'S'           wVIR_OK
067700040831     C                   LEAVE
067800040831     C                   ENDIF
067900040831     C                   ENDIF
068000040831     C*
068100040831     C                   ADD       1             jWrk2
068200040831     C                   ENDDO
068300040831     C*
068400040831     C* VERIFICO SE CONTROLLO OK
068500040831     C                   IF        wVIR_OK = 'N'
068600040831     C                   EVAL      STPERR = 'Tipo file ' + VIRTipFIL +
068700040831     C                             ' abilitato sul servizio ' + VIRTipISV +
068800040831     C                             ' che nn è attivo sul cliente.'
068900040903     C                   EVAL      wCODERR = 'FA'
069000040903     C                   EVAL      wTIPISV = VIRTipISV
069100040903     C                   EVAL      wTIPITF = VIRTipFIL
069200040901     C                   EXSR      WRIDET
069300040831     C                   ENDIF
069400040831     C**********
069500040831     C* CONTROLLO INUTILIZZO TIPI-FILE UPLOAD / DOWNLOAD
069600040831     C**********
069700040831     C*
069800040831     C* SE SERVIZIO D TIPO UPLOAD
069900040831     C     VIRTipISV     LOOKUP    skISVUPL                               55
070000040831     C                   IF        %found
070100040831     C*
070200040831     C* SE TIPO FILE / SREVIZIO NN PRESENTE SU LOG UPLOAD (ERGO INUTILIZZATO) => LISTO ANOMALIA
070300040831     C                   Z-ADD     1             jWrk2             3 0
070400040831     C                   MOVEL     'N'           wVIR_OK           1
070500040831     C                   DOW       jWrk2 <= %elem(skUPL)
070600040831     C                   IF        skUPL(jWrk2) <> *blanks
070700040831     C* RIDEFINISCO L'ELEMENTO DELLA SCHIERA
070800040831     C                   EVAL      DSUPLDWL = skUPL(jWrk2)
070900040831     C                   IF        VIRTipISV = UDTipISV AND
071000040831     C                             VIRTipFIL = UDTipFIL
071100040831     C                   MOVEL     'S'           wVIR_OK
071200040831     C                   LEAVE
071300040831     C                   ENDIF
071400040831     C                   ELSE
071500040831     C                   LEAVE
071600040831     C                   ENDIF
071700040831     C*
071800040831     C                   ADD       1             jWrk2
071900040831     C                   ENDDO
072000040831     C*
072100040831     C* VERIFICO SE CONTROLLO OK
072200040831     C                   IF        wVIR_OK = 'N'
072300040831     C                   EVAL      STPERR = 'Tipo file ' + VIRTipFIL +
072400040831     C                             ' tipo servizio ' + VIRTipISV     +
072500040831     C                             ' abilitato ma nn utilizzato.'
072600040903     C                   EVAL      wCODERR = 'F*'
072700040903     C                   EVAL      wTIPISV = VIRTipISV
072800040903     C                   EVAL      wTIPITF = VIRTipFIL
072900040901     C                   EXSR      WRIDET
073000040831     C                   ENDIF
073100040831     C                   ENDIF
073200040831     C*
073300040831     C* SE SERVIZIO D TIPO DOWNLOAD
073400040831     C     VIRTipISV     LOOKUP    skISVDWL                               55
073500040831     C                   IF        %found
073600040831     C*
073700040831     C* SE TIPO FILE / SREVIZIO NN PRESENTE SU LOG UPLOAD (ERGO INUTILIZZATO) => LISTO ANOMALIA
073800040831     C                   Z-ADD     1             jWrk2             3 0
073900040831     C                   MOVEL     'N'           wVIR_OK           1
074000040831     C                   DOW       jWrk2 <= %elem(skDWL)
074100040831     C                   IF        skDWL(jWrk2) <> *blanks
074200040831     C* RIDEFINISCO L'ELEMENTO DELLA SCHIERA
074300040831     C                   EVAL      DSUPLDWL = skDWL(jWrk2)
074400040831     C                   IF        VIRTipISV = UDTipISV AND
074500040831     C                             VIRTipFIL = UDTipFIL
074600040831     C                   MOVEL     'S'           wVIR_OK
074700040831     C                   LEAVE
074800040831     C                   ENDIF
074900040831     C                   ELSE
075000040831     C                   LEAVE
075100040831     C                   ENDIF
075200040831     C*
075300040831     C                   ADD       1             jWrk2
075400040831     C                   ENDDO
075500040831     C*
075600040831     C* VERIFICO SE CONTROLLO OK
075700040831     C                   IF        wVIR_OK = 'N'
075800040831     C                   EVAL      STPERR = 'Tipo file ' + VIRTipFIL +
075900040831     C                             ' tipo servizio ' + VIRTipISV     +
076000040831     C                             ' abilitato ma nn utilizzato.'
076100040903     C                   EVAL      wCODERR = 'F*'
076200040903     C                   EVAL      wTIPISV = VIRTipISV
076300040903     C                   EVAL      wTIPITF = VIRTipFIL
076400040901     C                   EXSR      WRIDET
076500040831     C                   ENDIF
076600040831     C                   ENDIF
076700040831     C*
076800040831     C                   ENDIF
076900040831     C                   ADD       1             jWrk
077000040831     C                   ENDDO
077100040827     C*
077200040831     C                   ENDSR
077300040827     C*------------------------------------------------------------------------*
077400040827     C* VALPARTES - VALORIZZAZZIONE TESTATA PROSPETTO IN RELAZIONE AI PARAMETRI RICHIESTI NEL LANCIO
077500040827     C*------------------------------------------------------------------------*
077600040827     C     VALPARTES     BEGSR
077700040827     C*
077800040827     C                   eval      DSFIRS = %trim(KNSIF) + ' - ' + %trim(KNMUS)
077900040827     C*
078000040827     C* Se richiesta specifica divisione => decodifico
078100040827     C                   if        PARDIV <> *blanks
078200040827     C                   exsr      DECDIV
078300040901     C                   else
078400040901     C                   eval      STPDIV = 'Tutti'
078500040827     C                   endif
078600040827     C*
078700040827     C* Se richiesta specifica area => decodifico
078800040827     C                   if        PARARE <> *zeros
078900040827     C                   exsr      DECARE
079000040901     C                   else
079100040901     C                   eval      STPARE = 'Tutte'
079200040827     C                   endif
079300040827     C*
079400040827     C* Se richiesta specifica filiale => decodifico
079500040827     C                   if        PARFIL <> *zeros
079600040827     C                   exsr      DECFIL
079700040901     C                   else
079800040901     C                   eval      STPFIL = 'Tutti'
079900040827     C                   endif
080000040827     C*
080100040827     C                   ENDSR
080200040901     C*------------------------------------------------------------------------*
080300040901     C* WRIDET - VALORIZZAZZIONE DETTAGLIO IN STAMPA
080400040901     C*------------------------------------------------------------------------*
080500040901     C     WRIDET        BEGSR
080600040901     C*
080700040901     C* Innanzitutto testo il salto pagina
080800040901     C                   if        *in05 = *on
080900040901     C                   write     st32te
081000040901     C                   setoff                                       05
081100040901     C                   endif
081200040901     C*
081300040903     C* Valorizzo, il cliente e la sua ragione sociale e l'utente STRATEGI
081400040901     C                   eval      STPKSC = VSSCodCli
081500040901     C                   eval      STPSGI = VSSUsrSGI
081600040901     C* Reperisco la ragione sociale
081700040901     C                   CLEAR                   BS69DS
081800040901     C                   CLEAR                   ACODS
081900040901     C                   MOVEL     KNSIF         I69SIF
082000040901     C                   Z-ADD     STPKSC        I69KAC
082100040901     C                   CALL      'TIBS69R'
082200040901     C                   PARM                    BS69DS
082300040901     C                   PARM                    ACODS
082400040901     C                   PARM                    INDDS
082500040901     C                   PARM                    CLPDS
082600040901     C                   PARM                    CLSDS
082700040901     C     O69ERR        IFNE      '1'
082800040901     C                   MOVEL     ACORAG        STPRAG
082900040901     C                   ELSE
083000040901     C                   MOVEL     *ALL'*'       STPRAG
083100040901     C                   ENDIF
083200040901     C*
083300040903     C                   if        VSSUsrSGI <> depSTPSGI
083400040903     C                   seton                                        60
083500040903     C                   eval      depSTPSGI = VSSUsrSGI
083600040901     C                   else
083700040903     C                   setoff                                       60
083800040901     C                   endif
083900040901     C*
084000060109     C* Verifico quali tipi d anomalie sono stati richiesti nel lancio es escludo quelle
084100060109     C* eventualmenmte nn richieste
084200060109     C                   movel     'S'           wStampa           1
084300060109     C                   if        %scan('servizio TT':stpERR)   > *zeros AND
084400060109     C                             %scan('nn utilizzato':stpERR) > *zeros AND
084500060109     C                             PARTTNU = 'N'
084600060109     C                   movel     'N'           wStampa
084700060109     C                   else
084800060109     C                   if        %scan('nn utilizzato':stpERR) > *zeros AND
084900060109     C                             PARSDNU = 'N'
085000060109     C                   movel     'N'           wStampa
085100060109     C                   else
085200060109     C                   if        %scan('disabilitato':stpERR) > *zeros AND
085300060109     C                             PARDISA = 'N'
085400060109     C                   movel     'N'           wStampa
085500060109     C                   else
085600060109     C                   if        %scan('tabella':stpERR) > *zeros AND
085700060109     C                             PARWTAB = 'N'
085800060109     C                   movel     'N'           wStampa
085900060109     C                   endif
086000060109     C                   endif
086100060109     C                   endif
086200060109     C                   endif
086300060109     C*
086400060109     C                   if        wStampa = 'S'
086500040901     C                   write     ST32RA
086600060109     C                   endif
086700040903     C*
086800040903     C* Contestualmente scrivo anche il wrk-file
086900040903     C                   clear                   wfst3200
087000040903     C                   eval      st32dat = datcor
087100040903     C                   eval      st32sun = stpsgi
087200040903     C                   eval      st32ksu = stpksc
087300040903     C                   eval      st32rag = stprag
087400040903     C                   eval      st32isv = wTIPISV
087500040903     C                   eval      st32itf = wTIPITF
087600040903     C                   eval      st32err = wCODERR
087700040903     C                   if        wTIPISV = 'TT'
087800040903     C                   eval      st32vta = VTAFlgSINO
087900040903     C                   endif
088000040903     C                   eval      st32msg = stperr
088100040903     C                   write     wfst3200
088200040901     C*
088300040901     C                   ENDSR
088400000000     C*------------------------------------------------------------------------*
088500040827     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
088600000000     C*------------------------------------------------------------------------*
088700991025     C     CHKREC        BEGSR
088800000000     C*
088900040827     C                   MOVEL     'S'           WRECOK            1
089000040901     C*
089100040901     C* ESCLUDO I 2 CODICI "FITTIZI"
089200040901     C                   IF        WRECOK = 'S'
089300040901     C                   IF        VSSKSU = '0ORM0000' OR
089400040901     C                             VSSKSU = '0VAR0000'
089500040901     C                   MOVEL     'N'           WRECOK
089600040901     C                   ENDIF
089700040901     C                   ENDIF
089800040827     C*
089900040827     C* REPERISCO LA FILIALE/AREA/DIVISIONE DEL CLIENTE COREENTE
090000040901     C                   IF        WRECOK = 'S'
090100040901     C                   SETOFF                                       55
090200040827     C                   MOVE(P)   VSSKSU        wVSSKSU           7 0
090300040827     C                   MOVEL     wVSSKSU       wFILVSSKSU        3 0
090400040827     C                   Z-ADD     1             wIndice           3 0
090500040827     C     wFILVSSKSU    LOOKUP    skFIL(wIndice)                         55
090600040827     C                   IF        *IN55 = *on
090700000000     C*
090800000000     C* SE DIVISIONE DEL RECORD <> DIVISIONE RICHIESTA, ESCLUDO
090900040901     C                   IF        WRECOK = 'S'
091000000000     C     PARDIV        IFNE      *BLANKS
091100040827     C                   IF        skDIV(wIndice) <> PARDIV
091200000000     C                   MOVEL     'N'           WRECOK
091300030528     C                   ENDIF
091400030528     C                   ENDIF
091500040901     C                   ENDIF
091600000000     C*
091700000000     C* SE AREA DEL RECORD <> AREA RICHIESTA, ESCLUDO
091800040901     C                   IF        WRECOK = 'S'
091900000000     C     PARARE        IFNE      *ZEROS
092000040827     C                   IF        skARE(wIndice) <> PARARE
092100000000     C                   MOVEL     'N'           WRECOK
092200030528     C                   ENDIF
092300030528     C                   ENDIF
092400040901     C                   ENDIF
092500000000     C*
092600000000     C* SE FILIALE DEL RECORD <> FILIALE RICHIESTA, ESCLUDO
092700040901     C                   IF        WRECOK = 'S'
092800000000     C     PARFIL        IFNE      *ZEROS
092900040827     C                   IF        skFIL(wIndice) <> PARFIL
093000000000     C                   MOVEL     'N'           WRECOK
093100030528     C                   ENDIF
093200030528     C                   ENDIF
093300040901     C                   ENDIF
093400040827     C*
093500040827     C                   ELSE
093600040827     C                   MOVEL     'N'           WRECOK
093700040827     C                   ENDIF
093800040901     C                   ENDIF
093900991123     C*
094000991123     C* SE CLIENTE DEL RECORD <> CLIENTE RICHIESTO, ESCLUDO
094100040901     C                   IF        WRECOK = 'S'
094200991123     C     PARKSC        IFNE      *ZEROS
094300040827     C                   IF        wVSSKSU <> PARKSC
094400991123     C                   MOVEL     'N'           WRECOK
094500030528     C                   ENDIF
094600030528     C                   ENDIF
094700040901     C                   ENDIF
094800000000     C*
094900000000     C                   ENDSR
095000000000     C*------------------------------------------------------------------------*
095100040827     C* DECDIV - DECODIFICA DIVISIONE
095200000000     C*------------------------------------------------------------------------*
095300000000     C     DECDIV        BEGSR
095400000000     C*
095500040827     C                   MOVEL     1             TBLKUT
095600040827     C                   MOVE      '17'          TBLCOD
095700040827     C                   MOVE      *BLANKS       TBLKEY
095800040827     C                   MOVEL     PARDIV        TBLKEY
095900040831     C     KEYTAB        CHAIN     TABEL00F                           97
096000000000     C     *IN97         IFEQ      '1'
096100000000     C     TBLFLG        OREQ      '*'
096200040827     C                   MOVEL     *ALL'*'       STPDIV
096300000000     C                   ELSE
096400040827     C                   MOVEL     TBLUNI        STPDIV
096500001115     C                   ENDIF
096600000000     C*
096700000000     C                   ENDSR
096800000000     C*------------------------------------------------------------------------*
096900000000     C* DECARE - DECODIFICA AREA
097000000000     C*------------------------------------------------------------------------*
097100000000     C     DECARE        BEGSR
097200000000     C*
097300040827     C                   MOVEL     1             TBLKUT
097400040827     C                   MOVE      '05'          TBLCOD
097500040827     C                   MOVE      *BLANKS       TBLKEY
097600040827     C                   MOVEL     PARARE        TBLKEY
097700040831     C     KEYTAB        CHAIN     TABEL00F                           97
097800000000     C     *IN97         IFEQ      '1'
097900000000     C     TBLFLG        OREQ      '*'
098000040827     C                   MOVEL     *ALL'*'       STPARE
098100000000     C                   ELSE
098200040827     C                   MOVEL     TBLUNI        STPARE
098300001115     C                   ENDIF
098400000000     C*
098500000000     C                   ENDSR
098600000000     C*------------------------------------------------------------------------*
098700000000     C* DECFIL - DECODIFICA FILIALE/TERMINAL
098800000000     C*------------------------------------------------------------------------*
098900000000     C     DECFIL        BEGSR
099000000000     C*
099100040827     C     PARFIL        CHAIN     AZORG01L                           97
099200040827     C     *IN97         IFEQ      '1'
099300040827     C                   MOVEL     *ALL'*'       STPFIL
099400000000     C                   ELSE
099500040827     C                   MOVEL     ORGDES        STPFIL
099600001115     C                   ENDIF
099700991026     C*
099800991123     C                   ENDSR
099900040831     C*------------------------------------------------------------------------*
100000040831     C* CARTAB - CARICAMENTO TABELE OCCORRENTI
100100040831     C*------------------------------------------------------------------------*
100200040831     C     CARTAB        BEGSR
100300040831     C*
100400040831     C* CARICAMENTO SERVIZI
100500040831     C                   clear                   skISVTT
100600040831     C                   clear                   skISVUPL
100700040831     C                   clear                   skISVDWL
100800040831     C                   z-add     *zeros        jISVTT            3 0
100900040831     C                   z-add     *zeros        jISVUPL           3 0
101000040831     C                   z-add     *zeros        jISVDWL           3 0
101100040831     C     'ISV'         setll     tntbe01l
101200040831     C                   if        %equal(tntbe01l)
101300040831     C     'ISV'         reade     tntbe01l
101400040831     C                   dow       not %eof(tntbe01l)
101500040831     C                   if        tbeatb = *blanks
101600040831     C                   eval      DISV = tbeuni
101700040831     C                   if        §ISVLOG = 'T'
101800040831     C                   add       1             jISVTT
101900040831     C                   eval      skISVTT(jISVTT) = §ISVLOGTIP
102000040831     C                   else
102100040831     C                   if        §ISVLOG = 'U'
102200040831     C                   add       1             jISVUPL
102300040831     C                   eval      skISVUPL(jISVUPL) = §ISVLOGTIP
102400040831     C                   else
102500040831     C                   if        §ISVLOG = 'D'
102600040831     C                   add       1             jISVDWL
102700040831     C                   eval      skISVDWL(jISVDWL) = §ISVLOGTIP
102800040831     C                   endif
102900040831     C                   endif
103000040831     C                   endif
103100040831     C                   endif
103200040831     C     'ISV'         reade     tntbe01l
103300040831     C                   enddo
103400040831     C                   endif
103500040831     C*
103600040831     C* CARICAMENTO DIVISIONI/AREE/FILIALI
103700040831     C                   clear                   skFIL
103800040831     C                   clear                   skARE
103900040831     C                   clear                   skFIL
104000040831     C                   z-add     *zeros        jFILAREDIV        3 0
104100040831     C     *loval        setll     azorg01l
104200040831     C                   if        %found(azorg01l)
104300040831     C                   read      azorg01l
104400040831     C                   dow       not %eof(azorg01l)
104500040831     C* VALORIZZO LE SCHIERE CON TUTTE LE FILIALI/AREE/DIVISIONI
104600040831     C                   add       1             jFILAREDIV
104700040831     C                   eval      skDIV(jFILAREDIV) = orgfl3
104800040831     C                   eval      skARE(jFILAREDIV) = orgcar
104900040831     C                   eval      skFIL(jFILAREDIV) = orgfil
105000040831     C                   read      azorg01l
105100040831     C                   enddo
105200040831     C                   endif
105300040831     C*
105400040831     C                   ENDSR
105500040901     C*------------------------------------------------------------------------*
105600040901     C* FINSR - OPERAZIONI FINALI
105700040901     C*------------------------------------------------------------------------*
105800040901     C     FINSR         BEGSR
105900040901     C*
106000040901     C* CHIUDE I FILE DEI PGM CHIAMATI
106100040901     C                   CLEAR                   BS69DS
106200040901     C                   CLEAR                   ACODS
106300040901     C                   CLEAR                   INDDS
106400040901     C                   CLEAR                   CLPDS
106500040901     C                   CLEAR                   CLSDS
106600040901     C                   MOVEL     'C'           I69TLA
106700040901     C                   MOVEL     KNSIF         I69SIF
106800040901     C                   CALL      'TIBS69R'
106900040901     C                   PARM                    BS69DS
107000040901     C                   PARM                    ACODS
107100040901     C                   PARM                    INDDS
107200040901     C                   PARM                    CLPDS
107300040901     C                   PARM                    CLSDS
107400040901     C*
107500040901     C                   ENDSR
107600000000     C*------------------------------------------------------------------------*
107700000000     C* *INZSR - ROUTINE INIZIALE
107800000000     C*------------------------------------------------------------------------*
107900000000     C     *INZSR        BEGSR
108000000000     C*--------------------
108100000000     C* RICEVIMENTO PARAMENTRI
108200000000     C*--------------------
108300000000     C     *ENTRY        PLIST
108400000000     C                   PARM                    KPJBA
108500000000     C                   MOVEL     KPJBU         PARAM
108600000000     C*
108700000000     C*--------------------
108800040831     C* CHIAVI DI LETTURA
108900000000     C*--------------------
109000000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
109100000000     C     KEYTAB        KLIST
109200040827     C                   KFLD                    TBLKUT
109300040827     C                   KFLD                    TBLCOD
109400040827     C                   KFLD                    TBLKEY
109500040827     C*
109600040827     C* IMPOSTA LA DATA CORRENTE
109700040827     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
109800040827     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
109900040827     C                   Z-ADD     N8            G08DAT
110000040827     C                   Z-ADD     *ZEROS        G08INV
110100040827     C                   MOVEL     '0'           G08ERR
110200040827     C                   CALL      'XSRDA8'
110300040827     C                   PARM                    WLBDA8
110400040827     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
110500000000     C*
110600000000     C                   ENDSR
