000100000000     H*------------------------------------------------------------------------*
000200041217     H* STATISTICA UTILIZZO SERVIZI VAS INTERNET
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500041217     FTIVSS04L  IF   E           K DISK
000600050209     FTIVTL12L  IF   E           K DISK
000700041220     FTIABL03L  IF   E           K DISK
000800041227     FTIVLR08L  IF   E           K DISK
000900041227     FTIVLT09L  IF   E           K DISK
001000041220     FCNCLP00F  IF   E           K DISK
001100090918     FTIVTA12L  IF   E           K DISK
001200041220     FTFNTC01L  IF   E           K DISK
001300000000     FAZORG01L  IF   E           K DISK
001400140616     FAZCMM01L  IF   E           K DISK
001500041217     FWFSUV01L  UF A E           K DISK
001600041217     FWFSUV10F  UF A E           K DISK
001700000000     D*------------------------------------------------------------------------*
001800000000     D* SCHIERE
001900000000     D*------------------------------------------------------------------------*
002000000000     D*-------------------
002100041217     D* FILIALI CON RELATIVA DIVISIONE E AREA
002200000000     D*-------------------
002300000000     D FIL             S              3  0 DIM(500)                             *FILIALE
002400000000     D ARE             S              3  0 DIM(500)                             *AREA
002500000000     D DIV             S              1    DIM(500)                             *DIVISIONE
002600000000     D DIT             S              3    DIM(500)                             *RAMO AZIENDALE
002700000000     D*------------------------------------------------------------------------*
002800000000     D* INPUT
002900000000     D*------------------------------------------------------------------------*
003000000000     D*-------------------
003100000000     D* ARCHITETTURA
003200000000     D*-------------------
003300000000     D KPJBA         E DS
003400041209     D*------------------
003500041209     D* DS REPERIMENTO CLIENTI UNIFICANTI/STAND-ALONE
003600041209     D*-------------------
003700041209     D TIBS10DS      E DS
003800041220     D*------------------
003900041220     D* DS REPERIMENTO SALDI CLIENTE
004000041220     D*-------------------
004100041220     D TISE70DS      E DS
004200050628     D*------------------
004300050628     D* DS AGENTI COMMERCIALI
004400050628     D*-------------------
004500050628     D DS01          E DS
004600041220     D*-------------------
004700041220     D* DS REPERIMENTO DATI CLIENTE
004800041220     D*-------------------
004900041220     D BS69DS        E DS                  EXTNAME(TIBS69DS)
005000041220     D ACODS         E DS                  EXTNAME(CNACO00F) PREFIX(W_)
005100041220     D INDDS         E DS                  EXTNAME(CNIND00F) PREFIX(W_)
005200041220     D CLPDS         E DS                  EXTNAME(CNCLP00F) PREFIX(W_)
005300041220     D CLSDS         E DS                  EXTNAME(FNCLS00F) PREFIX(W_)
005400041220     D*-------------------
005500041220     D* DS CONTROLLA DATA (8)
005600041220     D*-------------------
005700041220     D WLBDA8          DS
005800041220     D  G08DAT                 1      8  0
005900041220     D  G08INV                 9     16  0
006000041220     D  G08ERR                17     17
006100041220     D  G08TGI                18     22  0
006200041217     D*------------------
006300041217     D* SCHIERA DI RIDEFINIZIONE ELENCO CLIENTI FIGLI
006400041217     D*------------------
006500041217     D skUNI           S             11    DIM(500)
006600041217     D jUNI            S              3  0
006700000000     D*-------------------
006800000000     D* DS PARAMETRI DI LANCIO PGM
006900000000     D*-------------------
007000000000     D PARAM           DS
007100050107     D  PARDAI                        6  0
007200050107     D  PARDAF                        6  0
007300041217     D*-------------------
007400041217     D* VARIABILI D WRK
007500041217     D*-------------------
007600050629     D wKSCnum         S              7  0 INZ(*zeros)
007700050629     D wKSUnum         S              7  0 INZ(*zeros)
007800050629     D wCLIENTE        S              7  0 INZ(*zeros)
007900050629     D wEMAIL          S            400    INZ(*blanks)
008000050629     D wGIRO           S              3    INZ(*blanks)
008100050629     D wTLE            S              2    INZ(*blanks)
008200050629     D wNOCLI          S              4    INZ(*blanks)
008300050629     D wFlagWRISUV     S              1    INZ(*blanks)
008400060505     d wFlg2WriSUV     s              1    inz(*blanks)
008500041217
008600041217
008700000000     C*------------------------------------------------------------------------*
008800000000     C* MAIN LINE
008900000000     C*------------------------------------------------------------------------*
009000041220     C*
009100041220     C* CARICA ORGANIGRAMMA
009200041220     C                   EXSR      CARORG
009300000000     C*
009400041220     C* ESEGUO ELABORAZIONE
009500041217     C                   EXSR      WRIVSSTT
009600041220     C                   EXSR      WRICLP
009700991122     C*
009800000000     C                   SETON                                        LR
009900991118     C*------------------------------------------------------------------------*
010000041220     C* WRIVSSTT - ELABORAZIONE DAL FILE TIVSS00F
010100991118     C*------------------------------------------------------------------------*
010200041217     C     WRIVSSTT      BEGSR
010300041220     C*
010400041220     C* IMPOSTO SUBITO IL "GIRO" CORRENTE
010500041220     C                   EVAL      wGIRO = 'VSS'
010600991118     C*
010700041217     C* LEGGO TUTTI I CLIENTI ABILITATI AL SERVIZIO T&T
010800041217     C                   EVAL      VSSISV = 'TT'
010900041217     C     VSSISV        SETLL     TIVSS04L
011000060505if  1C                   IF        %EQUAL(TIVSS04L)
011100041217     C     VSSISV        READE     TIVSS04L
011200060505do  2C                   DOW       not %eof(TIVSS04L)
011300041220     C*
011400041220     C* INIZIALIZZO IL FLAG CHE VINCOLA LA COMPLETA ELABORAZIONE DEL CLIENTE CORRENTE
011500041220     C* (IVI COMPRESI PADRI/FIGLI
011600041220     C                   EVAL      wFlagWRISUV = 'S'
011700060505     c                   eval      wFlg2WriSUV = 'S'
011800041217     C*
011900041217     C* PALETTO GLI UNICI 2 CODICI CLIENTE FITTIZI CHE NN DEVONO CMQ MAI ESSERE CONSIDERATI
012000041220     C* E VERIFICO INOLTRE CHE L'ABILITAZIONE SIA ANCORA ATTIVA NEL PERIODO
012100060505if  3C                   IF        VSSKSU <> '0ORM0000' AND
012200041220     C                             VSSKSU <> '0VAR0000' AND
012300041220     C                             VSSDDE <= DATCOR     AND
012400041220     C                             VSSDSC >= DATCOR
012500041217     C*
012600041217     C* VERIFICO SUBITO SE IL CLIENTE CORRENTE E' GIA' STATO ELABORATO
012700041217     C* O COME PADRE O COME FIGLIO D QUALCHE ALTRO CLIENTE PRECEDENTE
012800041217     C* IN QUANTO NEL FILE D WRK FINALE OGNI CLIENTE DEVE ESSERCI 1 SOLA VOLTA
012900041217     C                   MOVE(P)   VSSKSU        WFSKSC10
013000041217     C     WFSKSC10      CHAIN     WFSUV10F
013100041217     C*
013200041217     C* SE NN GIA' PRESENTE => PROSEGUO CON LA VERIFICA
013300060505if  4C                   IF        not %found(WFSUV10F)
013400041217     C*
013500041217     C* X OGNI CLIENTE GIA' ISCRITTO TENGO TRACCIA NEL WRK-FILE TEMPORANEO
013600041217     C* D TUTTI I RELATIVI PADRI E FIGLI
013700041217     C                   MOVE(P)   VSSKSU        wKSCnum
013800041220     C                   EVAL      wTLE = VSSTLE
013900041217     C                   EXSR      RTVPARENT
014000041217     C*
014100041217     C* CICLO SU TUTTI I "PARENTI" REPERITI E LI INSERISCO NEL FILE D WRK
014200060505do  5C     1             DO        500           jUNI
014300060505if  6C     skUNI(jUNI)   IFNE      *BLANKS
014400041220     C     skUNI(jUNI)   ANDNE     *ZEROS
014500041217     C                   MOVE(P)   skUNI(jUNI)   WFSKSC10
014600041217     C     WFSKSC10      CHAIN     WFSUV10F
014700041217     C*
014800041217     C* SE NN GIA' PRESENTE => INSERISCO
014900041217     C                   IF        not %found(WFSUV10F)
015000041217     C                   WRITE     WFSUV010
015100041220     C                   ELSE
015200041220     C                   EVAL      wFlagWRISUV = 'N'
015300041217     C                   ENDIF
015400041217     C*
015500060505x   6C                   ELSE
015600041217     C                   LEAVE
015700060505e   6C                   ENDIF
015800060505e   5C                   ENDDO
015900041220     C*
016000041220     C* IMPOSTO QUALE CODICE CONSIDERARE NELL'ELABORAZIONE CORRENTE
016100041220     C                   EVAL      wCLIENTE = wKSCNum
016200041220     C*
016300041220     C* SE TUTTO OK => SCARICO IL BUFFER DEL FILE S WRK D PROCEDURA
016400060505     C                   IF            wFlagWRISUV = 'S'
016500060505     c                             and wFlg2WriSUV = 'S'
016600041220     C                   EXSR      WRISUV
016700041217     C                   ENDIF
016800060505e   4C                   ENDIF
016900060505e   3C                   ENDIF
017000041217     C*
017100041217     C* PROSEGUO CON LA LETTURA DEL FILE ABILITAZIONI CLIENTI (AL T&T)
017200041217     C     VSSISV        READE     TIVSS04L
017300060505e   2C                   ENDDO
017400060505e   1C                   ENDIF
017500991118     C*
017600991118     C                   ENDSR
017700041220     C*------------------------------------------------------------------------*
017800041220     C* WRICLP - ELABORAZIONE DA FILE CNCLP00F (OVVERO P.d.C. solo clienti)
017900041220     C*------------------------------------------------------------------------*
018000041220     C     WRICLP        BEGSR
018100041220     C*
018200041220     C* IMPOSTO SUBITO IL "GIRO" CORRENTE
018300041220     C                   EVAL      wGIRO = 'CLP'
018400041220     C*
018500041220     C* LEGGO TUTTI I CLIENTI DEL PIANO DEI CONTI
018600041220     C     *LOVAL        SETLL     CNCLP00F
018700041220     C                   READ      CNCLP00F
018800060505do  1C                   DOW       not %eof(CNCLP00F)
018900041220     C*
019000041220     C* INIZIALIZZO IL FLAG CHE VINCOLA LA COMPLETA ELABORAZIONE DEL CLIENTE CORRENTE
019100041220     C* (IVI COMPRESI PADRI/FIGLI
019200041220     C                   EVAL      wFlagWRISUV = 'S'
019300060505     c                   eval      wFlg2WriSUV = 'S'
019400041220     C*
019500041220     C* ESCLUDO DALL'ELABORAZIONE TUTTI I CLIENTI xxx8888 e xxx9999
019600041220     C                   MOVE(P)   CLPKSC        wNOCLI
019700060505if  2C                   IF        wNOCLI =  '8888' OR
019800041220     C                             wNOCLI =  '9999' OR
019900041220     C                             CLPFLG <> *blanks
020000060505x   2C                   ELSE
020100041220     C*
020200041220     C* VERIFICO SUBITO SE IL CLIENTE CORRENTE E' GIA' STATO ELABORATO
020300041220     C* O COME PADRE O COME FIGLIO D QUALCHE ALTRO CLIENTE PRECEDENTE
020400041220     C* IN QUANTO NEL FILE D WRK FINALE OGNI CLIENTE DEVE ESSERCI 1 SOLA VOLTA
020500041220     C                   MOVE(P)   CLPKSC        WFSKSC10
020600041220     C     WFSKSC10      CHAIN     WFSUV10F
020700041220     C*
020800041220     C* SE NN GIA' PRESENTE => PROSEGUO CON LA VERIFICA
020900060505if  3C                   IF        not %found(WFSUV10F)
021000041220     C*
021100041220     C* X OGNI CLIENTE GIA' ISCRITTO TENGO TRACCIA NEL WRK-FILE TEMPORANEO
021200041220     C* D TUTTI I RELATIVI PADRI E FIGLI
021300041220     C                   MOVE(P)   CLPKSC        wKSCnum
021400041220     C                   EVAL      wTLE = 'ST'
021500041220     C                   EXSR      RTVPARENT
021600041220     C*
021700041220     C* CICLO SU TUTTI I "PARENTI" REPERITI E LI INSERISCO NEL FILE D WRK
021800060505do  4C     1             DO        500           jUNI
021900060505if  5C     skUNI(jUNI)   IFNE      *BLANKS
022000041220     C     skUNI(jUNI)   ANDNE     *ZEROS
022100041220     C                   MOVE(P)   skUNI(jUNI)   WFSKSC10
022200041220     C     WFSKSC10      CHAIN     WFSUV10F
022300041220     C*
022400041220     C* SE NN GIA' PRESENTE => INSERISCO
022500041220     C                   IF        not %found(WFSUV10F)
022600041220     C                   WRITE     WFSUV010
022700041220     C                   ELSE
022800041220     C                   EVAL      wFlagWRISUV = 'N'
022900041220     C                   ENDIF
023000041220     C*
023100060505x   5C                   ELSE
023200041220     C                   LEAVE
023300060505e   5C                   ENDIF
023400060505e   4C                   ENDDO
023500041220     C*
023600041220     C* IMPOSTO QUALE CODICE CONSIDERARE NELL'ELABORAZIONE CORRENTE
023700041220     C                   EVAL      wCLIENTE = wKSUNum
023800041220     C*
023900041220     C* SE TUTTO OK => SCARICO IL BUFFER DEL FILE S WRK D PROCEDURA
024000060505     C                   IF            wFlagWRISUV = 'S'
024100060505     c                             and wFlg2WriSUV = 'S'
024200041220     C                   EXSR      WRISUV
024300041220     C                   ENDIF
024400060505e   3C                   ENDIF
024500060505e   2C                   ENDIF
024600041220     C*
024700041220     C* PROSEGUO CON LA LETTURA DEL PIANO DEI CONTI
024800041220     C                   READ      CNCLP00F
024900060505e   1C                   ENDDO
025000041220     C*
025100041220     C                   ENDSR
025200991118     C*------------------------------------------------------------------------*
025300041217     C* RTVPARENT - REPERIMENTO PARENTI (PADRI/FIGLI)
025400991118     C*------------------------------------------------------------------------*
025500041217     C     RTVPARENT     BEGSR
025600991122     C*
025700041217     C* SVUOTO LA SCHIERA CONTENTE I "PARENTI"
025800041220     C                   CLEAR                   skUNI
025900041217     C*
026000041217     C* 1° STEP: considero il codice corrente come figlio e cerco il padre e i fratelli
026100041217     C                   CLEAR                   TIBS10DS
026200041220     C                   EVAL      D10TLE = wTLE
026300041217     C                   EVAL      D10PAF = 'P'
026400041217     C                   EVAL      D10COD = wKSCnum
026500041217     C                   CALL      'TIBS10R'
026600041217     C                   PARM                    TIBS10DS
026700041217     C                   PARM      'E'           TIPEXE            1
026800041217     C*
026900041217     C* 2° STEP: se non è figlio allora lo considero padre e cerco i suoi figli
027000041217     C                   IF        D10ERR = '1'
027100041217     C                   CLEAR                   TIBS10DS
027200041220     C                   EVAL      D10TLE = wTLE
027300041217     C                   EVAL      D10PAF = 'F'
027400041217     C                   EVAL      D10COD = wKSCnum
027500041217     C                   CALL      'TIBS10R'
027600041217     C                   PARM                    TIBS10DS
027700041217     C                   PARM      'E'           TIPEXE
027800041217     C*
027900041217     C* A questo punto traggo le conclusioni
028000041217     C                   IF        D10ERR = '1'
028100041217     C                   MOVE(P)   D10COD        skUNI(1)
028200041217     C                   ELSE
028300041217     C                   MOVEA     D10SKC        skUNI
028400041217     C                   ENDIF
028500041217     C*
028600041217     C                   ELSE
028700041217     C                   MOVEA     D10SKC        skUNI
028800041217     C                   ENDIF
028900041220     C*
029000041220     C* AL TERMINE "BATTEZZO" IL CODICE PADRE
029100041220     C                   MOVE(P)   skUNI(1)      wKSUnum
029200060505      *
029300060505      * Verifico se TUTTI i codici clienti presenti in SKUNI hanno
029400060505      *   ACOABL <> *blank in CNACO00F - in tal caso l'unificante NON
029500060505      *   è da elaborare.
029600060505      *   Se, invece, anche un cliente solo ha ACOABL = *blank,
029700060505      *   l'unificante è da elaborare.
029800060505     c                   eval      wFlg2WriSUV = 'N'
029900060505     c                   clear                   jUNI
030000060505do  1c                   DOW       jUNI < %elem(skUNI)
030100060505     c                   add       1             jUNI
030200060505if  2c                   if            skUNI(jUNI) <> *blanks
030300060505     c                             and skUNI(jUNI) <> *zeros
030400060505     c                   move      skUNI(jUNI)   wCliente
030500060505     c                   exsr      RtvAnaCLI_1
030600060505if  3c                   if        w_ACOabl    = *blanks
030700060505     c                   eval      wFlg2WriSUV = 'S'
030800060505     c                   leave
030900060505e   3c                   endif
031000060505e   2c                   endif
031100060505e   1c                   ENDDO
031200991118     C*
031300991118     C                   ENDSR
031400000000     C*------------------------------------------------------------------------*
031500041217     C* RTVDIVARE - RECUPERA AREA E DIVISIONE
031600000000     C*------------------------------------------------------------------------*
031700041217     C     RTVDIVARE     BEGSR
031800000000     C*
031900041220     C                   MOVEL(P)  wCLIENTE      WFIL              3 0
032000000000     C                   Z-ADD     1             I
032100000000     C     WFIL          LOOKUP    FIL(I)                                 99
032200001115     C     *IN99         IFEQ      '0'
032300000000     C                   MOVEL     '*'           WDIV              1
032400000000     C                   Z-ADD     999           WARE              3 0
032500001115     C                   ELSE
032600000000     C                   MOVEL     DIV(I)        WDIV
032700000000     C                   Z-ADD     ARE(I)        WARE
032800001115     C                   ENDIF
032900000000     C*
033000000000     C                   ENDSR
033100041220     C*------------------------------------------------------------------------*
033200041220     C* RTVANACLI - RECUPERA DATI ANAGRAFICI CLIENTI
033300041220     C*------------------------------------------------------------------------*
033400041220     C     RTVANACLI     BEGSR
033500041220     C*
033600041220     C* REPERICE I DATI DEL CLIENTE
033700041220     C                   CLEAR                   BS69DS
033800041220     C                   CLEAR                   ACODS
033900041220     C                   CLEAR                   CLPDS
034000041220     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
034100041220     C                   Z-ADD     wCLIENTE      I69KAC                         *CODICE X CNACO
034200041220     C                   Z-ADD     wCLIENTE      I69KCP                         *CODICE X CNCLP
034300041220     C                   CALL      'TIBS69R'
034400041220     C                   PARM                    BS69DS
034500041220     C                   PARM                    ACODS
034600041220     C                   PARM                    INDDS
034700041220     C                   PARM                    CLPDS
034800041220     C                   PARM                    CLSDS
034900041220     C     O69ERR        IFEQ      '1'                                          *ERRORE
035000050628     C                   EVAL      WFSCAT  = '*'
035100050628     C                   EVAL      WFSRAG  = *all'*'
035200050628     C                   EVAL      WFSAGE  = *all'0'
035300050628     C                   EVAL      WFSAGED = *all'*'
035400041220     C                   ELSE
035500041220     C                   EVAL      WFSCAT = W_CLPCLV
035600041220     C                   EVAL      WFSRAG = W_ACORAG
035700050628     C                   EVAL      WFSAGE  = W_CLPAGE
035800140616     C* Reperisco le informazioni dell'agente commerciale
035900140616     C                   EVAL      CMMCod = WFSAge
036000140616     C     CMMCod        CHAIN     AZCMM01l
036100140616     C                   IF        %found(AZCMM01L)
036200140616     C                   EVAL      WFSAGED = cmmdes
036300050628     C                   ELSE
036400050628     C                   EVAL      WFSAGED = *all'*'
036500050628     C                   ENDIF
036600050628     C*
036700041220     C                   ENDIF
036800041220     C*
036900041220     C                   ENDSR
037000060505      *
037100060505      *------------------------------------------------------------------------*
037200060505      * RtvAnaCLI_1 * Recupera dati anagrafici (solo CNACO) del cliente        *
037300060505      *------------------------------------------------------------------------*
037400060505     c     RtvAnaCLI_1   BEGSR
037500060505      *
037600060505      * REPERICE I DATI DEL CLIENTE
037700060505     c                   clear                   BS69ds
037800060505     c                   clear                   ACOds
037900060505     c                   clear                   CLPds
038000060505     c                   eval      I69sif = KNSIF                               *S.INFORMATIVO
038100060505     c                   eval      I69kac = wCliente                            *CODICE X CNACO
038200060505     c                   call      'TIBS69R'
038300060505     c                   parm                    BS69ds
038400060505     c                   parm                    ACOds
038500060505     c                   parm                    INDds
038600060505     c                   parm                    CLPds
038700060505     c                   parm                    CLSds
038800060505      *
038900060505     c                   ENDSR
039000041220     C*------------------------------------------------------------------------*
039100050628     C* RTVBOLVTAVAS - RECUPERA SE PRESENTI BOLLE IN TIVTA E/O SE CLIENTE UTILIZZA ALTRI SERVIZI SGI
039200041220     C*------------------------------------------------------------------------*
039300050628     C     RTVBOLVTAVAS  BEGSR
039400041220     C*
039500041220     C                   MOVEL(P)  *all'0'       VTAKSU
039600041220     C                   MOVE      wCLIENTE      VTAKSU
039700041227     C                   MOVEL(P)  *all'0'       VLRKSC
039800041227     C                   MOVE      wCLIENTE      VLRKSC
039900041227     C                   MOVEL(P)  *all'0'       VLTKSU
040000041227     C                   MOVE      wCLIENTE      VLTKSU
040100090918     C     KEYVTA        SETLL     TIVTA12L                               55
040200090918     C                   IF        %equal(TIVTA12L)
040300041220     C                   EVAL      WFSFVTA = 'S'
040400050628     C                   ELSE
040500041227     C                   MOVEL     'UL'          VLRISV
040600041227     C     KEYVLR        SETLL     TIVLR08L                               55
040700041227     C                   IF        %equal(TIVLR08L)
040800041227     C                   EVAL      WFSFVTA = 'A'
040900041227     C                   ELSE
041000041227     C                   MOVEL     'DL'          VLTISV
041100041227     C     KEYVLT        SETLL     TIVLT09L                               55
041200041227     C                   IF        %equal(TIVLT09L)
041300041227     C                   EVAL      WFSFVTA = 'A'
041400041227     C                   ELSE
041500041220     C                   EVAL      WFSFVTA = 'N'
041600041220     C                   ENDIF
041700041227     C                   ENDIF
041800041227     C                   ENDIF
041900050628     C*
042000050628     C     VLTKSC        SETLL     TIVLT09L                               55
042100050628     C                   IF        %equal(TIVLT09L)
042200050628     C                   EVAL      WFSFDWL = 'S'
042300050628     C                   ENDIF
042400050628     C*
042500050628     C     VLRKSC        SETLL     TIVLR08L                               55
042600050628     C                   IF        %equal(TIVLR08L)
042700050628     C                   EVAL      WFSFUPL = 'S'
042800050628     C                   ENDIF
042900041220     C*
043000041220     C                   ENDSR
043100041220     C*------------------------------------------------------------------------*
043200041220     C* RTVLOGVTL - RECUPERA SE CLIENTE UTILIZZA SERVIZI ON-LINE
043300041220     C*------------------------------------------------------------------------*
043400041220     C     RTVLOGVTL     BEGSR
043500041220     C*
043600041220     C                   MOVEL(P)  *all'0'       VTLKSU
043700041220     C                   MOVE      wCLIENTE      VTLKSU
043800050209     C     KEYVTL        SETLL     TIVTL12L                               55
043900050209     C                   IF        %equal(TIVTL12L)
044000041220     C                   EVAL      WFSTIP = 'AU'
044100041220     C                   ELSE
044200041220     C                   EVAL      WFSTIP = 'AN'
044300041220     C                   ENDIF
044400041220     C*
044500041220     C                   ENDSR
044600041220     C*------------------------------------------------------------------------*
044700041220     C* RTVEMAIL - RECUPERA GLI INDIRIZZI E-MAIL DEL CLIENTE A CUI SPEDIRE
044800041220     C*------------------------------------------------------------------------*
044900041220     C     RTVEMAIL      BEGSR
045000041220     C*
045100041220     C                   CLEAR                   wEMAIL
045200041220     C*
045300041220     C* INNANZITUTTO REPERISCO LA MAIL DEL RESPONSABILE DELL'ATTIVAZIONE AI SERVIZI ON-LINE
045400041220     C                   EVAL      ABLTIP = 'SG1'
045500041220     C                   EVAL      ABLISV = 'TT'
045600041220     C                   MOVE(P)   wCLIENTE      ABLUTE
045700041220     C     KEYABL        CHAIN     TIABL03L
045800041220     C                   IF        %found(TIABL03L)
045900041220     C                   EVAL      wEMAIL = ABLEML
046000041220     C                   ENDIF
046100041220     C*
046200041220     C* QUINDI REPERISCO TUTTE LE MAIL DALLE NOTE VOLUTE (06/03/85/88)
046300041220     C                   EVAL      NTCAPL = 'C'
046400041220     C                   EVAL      NTCNK1 = '0151' + %editc(wCLIENTE:'X')
046500041220     C                   EVAL      NTCNK2 = *blanks
046600041220     C* TIPO NOTA '06'
046700041220     C                   EVAL      NTCTNT = '06'
046800041220     C     KEYNTC        CHAIN     TFNTC01L
046900041220     C                   IF        %found(TFNTC01L)
047000041220     C                   IF        %scan(%trim(NTCRNT):wEMAIL) = *zeros
047100041220     C                   IF        wEMAIL <> *blanks
047200041220     C                   EVAL      wEMAIL = %trim(wEMAIL) + ';'
047300041220     C                   ENDIF
047400041220     C                   EVAL      wEMAIL = %trim(wEMAIL) + %trim(NTCRNT)
047500041220     C                   ENDIF
047600041220     C                   ENDIF
047700041220     C* TIPO NOTA '03'
047800041220     C                   EVAL      NTCTNT = '03'
047900041220     C     KEYNTC        CHAIN     TFNTC01L
048000041220     C                   IF        %found(TFNTC01L)
048100041220     C                   IF        %scan(%trim(NTCRNT):wEMAIL) = *zeros
048200041220     C                   IF        wEMAIL <> *blanks
048300041220     C                   EVAL      wEMAIL = %trim(wEMAIL) + ';'
048400041220     C                   ENDIF
048500041220     C                   EVAL      wEMAIL = %trim(wEMAIL) + %trim(NTCRNT)
048600041220     C                   ENDIF
048700041220     C                   ENDIF
048800041220     C* TIPO NOTA '85'
048900041220     C                   EVAL      NTCTNT = '85'
049000041220     C     KEYNTC        CHAIN     TFNTC01L
049100041220     C                   IF        %found(TFNTC01L)
049200041220     C                   IF        %scan(%trim(NTCRNT):wEMAIL) = *zeros
049300041220     C                   IF        wEMAIL <> *blanks
049400041220     C                   EVAL      wEMAIL = %trim(wEMAIL) + ';'
049500041220     C                   ENDIF
049600041220     C                   EVAL      wEMAIL = %trim(wEMAIL) + %trim(NTCRNT)
049700041220     C                   ENDIF
049800041220     C                   ENDIF
049900041220     C* TIPO NOTA '88'
050000041220     C                   EVAL      NTCTNT = '88'
050100041220     C     KEYNTC        CHAIN     TFNTC01L
050200041220     C                   IF        %found(TFNTC01L)
050300041220     C                   IF        %scan(%trim(NTCRNT):wEMAIL) = *zeros
050400041220     C                   IF        wEMAIL <> *blanks
050500041220     C                   EVAL      wEMAIL = %trim(wEMAIL) + ';'
050600041220     C                   ENDIF
050700041220     C                   EVAL      wEMAIL = %trim(wEMAIL) + %trim(NTCRNT)
050800041220     C                   ENDIF
050900041220     C                   ENDIF
051000041220     C*
051100041220     C                   EVAL      WFSEMAIL = wEMAIL
051200041220     C*
051300041220     C                   ENDSR
051400041220     C*------------------------------------------------------------------------*
051500041220     C* RTVSALDI - RECUPERA I SALDI DEL CLIENTE
051600041220     C*------------------------------------------------------------------------*
051700041220     C     RTVSALDI      BEGSR
051800041220     C*
051900041220     C                   CLEAR                   TISE70DS
052000041220     C                   EVAL      I70TLA = 'U'
052100041220     C                   EVAL      I70TLE = wTLE
052200041220     C                   EVAL      I70KSC = wCLIENTE
052300050107     C                   EVAL      I70DAI = PARDAI
052400050107     C                   EVAL      I70DAF = PARDAF
052500041220     C                   CALL      'TISE70R'
052600041220     C                   PARM                    TISE70DS
052700041220     C                   EVAL      WFSSPED = O70NSP
052800041220     C                   EVAL      WFSFATT = O70RIC
052900041220     C*
053000041220     C                   ENDSR
053100041220     C*------------------------------------------------------------------------*
053200041220     C* WRISUV - ROUTINE D VALORIZZAZIONE BUFFER FILE D WRK D PROCEDURA
053300041220     C*------------------------------------------------------------------------*
053400041220     C     WRISUV        BEGSR
053500041220     C*
053600041220     C* INIZIALIZZO IL BUFFER DEL FILE D WRK
053700041220     C                   CLEAR                   WFSUV000
053800050107     C*
053900050704     C                   EVAL      WFSDAT = DATCOR
054000050107     C                   EVAL      WFSDAI = PARDAI
054100050107     C                   EVAL      WFSDAF = PARDAF
054200041220     C*
054300041220     C* REPERISCO TUTTO CIO' CHE SERVE X LA SCRITTURA DEL FILE D WRK D PROCEDURA
054400041220     C                   EXSR      RTVDIVARE
054500041220     C                   EXSR      RTVANACLI
054600041220     C*
054700041220     C* EFFETTUO DETERMINATE VEIRFICHE SOLO NEL "GIRO" DEI CLIENTI GIA'
054800041220     C* ABILITATI AL T&T
054900041220     C                   IF        wGIRO = 'VSS'
055000050628     C                   EXSR      RTVBOLVTAVAS
055100041220     C                   EXSR      RTVLOGVTL
055200041220     C                   ELSE
055300041220     C                   EVAL      WFSTIP = 'NA'
055400041220     C                   ENDIF
055500041220     C*
055600041222     C                   EXSR      RTVSALDI
055700041220     C                   EXSR      RTVEMAIL
055800041220     C*
055900041220     C                   EVAL      WFSDIV = WDIV
056000041220     C                   EVAL      WFSARE = WARE
056100041220     C                   EVAL      WFSFIL = WFIL
056200041220     C                   EVAL      WFSKSC = wCLIENTE
056300041220     C*
056400041220     C* SE IL CLIENTE CORRENTE REPERITO NN E' GIA' PRESENTE NEL FILE WRK D PROCEDURA
056500041220     C* SCARICO IL BUFFER COSI VALORIZZATO
056600041220     C     KEYSUV        SETLL     WFSUV01L
056700041220     C                   IF        %equal(WFSUV01L)
056800041220     C                   ELSE
056900041220     C                   WRITE     WFSUV000
057000041220     C                   ENDIF
057100041220     C*
057200041220     C                   ENDSR
057300000000     C*------------------------------------------------------------------------*
057400000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
057500000000     C*------------------------------------------------------------------------*
057600000000     C     CARORG        BEGSR
057700041217     C*
057800041217     C                   Z-ADD     *ZEROS        ORGFIL
057900041217     C     KEYORG        SETLL     AZORG01L                           99
058000041217     C  N99              READ      AZORG01L                               99
058100041217     C                   Z-ADD     0             I                 3 0
058200041217     C     *IN99         DOWEQ     '0'
058300041217     C     ORGFVA        IFEQ      *BLANKS
058400041217     C                   ADD       1             I
058500041217     C                   Z-ADD     ORGFIL        FIL(I)
058600041217     C                   Z-ADD     ORGCAR        ARE(I)
058700041217     C                   MOVEL     ORGFL3        DIV(I)
058800041217     C                   MOVEL     ORGDIT        DIT(I)
058900041217     C                   ENDIF
059000041217     C                   READ      AZORG01L                               99
059100041217     C                   ENDDO
059200041217     C*
059300000000     C                   ENDSR
059400000000     C*------------------------------------------------------------------------*
059500000000     C* *INZSR - ROUTINE INIZIALE
059600000000     C*------------------------------------------------------------------------*
059700000000     C     *INZSR        BEGSR
059800000000     C*--------------------
059900000000     C* RICEVIMENTO PARAMETRI
060000000000     C*--------------------
060100000000     C     *ENTRY        PLIST
060200000000     C                   PARM                    KPJBA
060300000000     C                   MOVEL     KPJBU         PARAM
060400041220     C*
060500041220     C* IMPOSTA LA DATA CORRENTE
060600041220     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
060700041220     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
060800041220     C                   Z-ADD     N8            G08DAT
060900041220     C                   Z-ADD     *ZEROS        G08INV
061000041220     C                   MOVEL     '0'           G08ERR
061100041220     C                   CALL      'XSRDA8'
061200041220     C                   PARM                    WLBDA8
061300041220     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
061400000000     C*--------------------
061500000000     C* CHIAVI DI LETTURA
061600000000     C*--------------------
061700041220     C*
061800000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
061900000000     C     KEYORG        KLIST
062000041217     C                   KFLD                    ORGFIL
062100000000     C*
062200041220     C* CHIAVE LETTURA TIVSS04L - PARZIALE
062300041217     C     KEYVSS        KLIST
062400041217     C                   KFLD                    VSSISV
062500041220     C*
062600041220     C* CHIAVE LETTURA TIABL03L - COMPLETA
062700041220     C     KEYABL        KLIST
062800041220     C                   KFLD                    ABLTIP
062900041220     C                   KFLD                    ABLUTE
063000041220     C                   KFLD                    ABLISV
063100041217     C*
063200090918     C* CHIAVE LETTURA TIVTA12L - PARZIALE
063300041217     C     KEYVTA        KLIST
063400041217     C                   KFLD                    VTAKSU
063500041227     C*
063600041227     C* CHIAVE LETTURA TIVLR08L - PARZIALE
063700041227     C     KEYVLR        KLIST
063800041227     C                   KFLD                    VLRKSC
063900041227     C                   KFLD                    VLRISV
064000041227     C*
064100041227     C* CHIAVE LETTURA TIVLT09L - PARZIALE
064200041227     C     KEYVLT        KLIST
064300041227     C                   KFLD                    VLTKSU
064400041227     C                   KFLD                    VLTISV
064500041220     C*
064600050209     C* CHIAVE LETTURA TIVTL12L - PARZIALE
064700041220     C     KEYVTL        KLIST
064800041220     C                   KFLD                    VTLKSU
064900041220     C*
065000041220     C* CHIAVE LETTURA TFNTC01L - COMPLETA
065100041220     C     KEYNTC        KLIST
065200041220     C                   KFLD                    NTCAPL
065300041220     C                   KFLD                    NTCNK1
065400041220     C                   KFLD                    NTCNK2
065500041220     C                   KFLD                    NTCTNT
065600041220     C*
065700041220     C* CHIAVE LETTURA WFSUV01L - COMPLETA
065800041220     C     KEYSUV        KLIST
065900041220     C                   KFLD                    WFSKSC
066000041217     C*
066100041217     C* CHIAVE LETTURA WFSUV10F - COMPLETA
066200041217     C     KEYSUV10      KLIST
066300041217     C                   KFLD                    WFSKSC10
066400000000     C*
066500000000     C                   ENDSR
