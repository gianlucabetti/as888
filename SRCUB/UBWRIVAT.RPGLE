000100080616     H BNDDIR('UBRTVNETA':'QC2LE')
000200080611     H NOMAIN
000300071128     ***********************************************************************************************
000400071128     **
000500080616     ** Questo modulo gestisce la scruttura dei file FIVAT/EDIVAT
000600071128     **
000700080611     ** Può essere usato solo come sottoprocedura.
000800080612     ** Ogni metodo restituisce un valore 10I:
000900080612     **             - = 0 => tutto ok
001000080612     **             - < 0 => errore
001100080612     **
001200080612     ** --------- --- --------- ------------------------------
001300071128     **
001400080616     ** PARAMETRI DELLA SOTTOPROCEDURA: UBWRIVAT_Init
001500071128     **
001600071128     ** Posizione Uso Tipo      Descrizione
001700071128     ** --------- --- --------- ------------------------------
001800080616     ** 1         IN  BOOLEAN   Gestione file sotto controllo sincronia
001900071128     ** --------- --- --------- ------------------------------
002000080616     ** --------- --- --------- ------------------------------
002100080616     **
002200080616     ** PARAMETRI DELLA SOTTOPROCEDURA: UBWRIVAT_StrCmtCtl
002300080616     **
002400080616     ** Posizione Uso Tipo      Descrizione
002500080616     ** --------- --- --------- ------------------------------
002600080616     ** 1         IN  SIGNED    Ambito del controllo sincronia:
002700080616     **                         - 0 => *ACTGRP (default)
002800080616     **                         - 1 => *JOB
002900080616     ** --------- --- --------- ------------------------------
003000080616     ** --------- --- --------- ------------------------------
003100080616     **
003200080616     ** PARAMETRI DELLA SOTTOPROCEDURA: UBWRIVAT_Commit
003300080616     **
003400080616     ** Posizione Uso Tipo      Descrizione
003500080616     ** --------- --- --------- ------------------------------
003600080616     ** /         /   /         /
003700080616     ** --------- --- --------- ------------------------------
003800080616     ** --------- --- --------- ------------------------------
003900080616     **
004000080616     ** PARAMETRI DELLA SOTTOPROCEDURA: UBWRIVAT_RollBack
004100080616     **
004200080616     ** Posizione Uso Tipo      Descrizione
004300080616     ** --------- --- --------- ------------------------------
004400080616     ** /         /   /         /
004500080616     ** --------- --- --------- ------------------------------
004600080616     ** --------- --- --------- ------------------------------
004700080616     **
004800080616     ** PARAMETRI DELLA SOTTOPROCEDURA: UBWRIVAT_EndCmtCtl
004900080616     **
005000080616     ** Posizione Uso Tipo      Descrizione
005100080616     ** --------- --- --------- ------------------------------
005200080616     ** /         /   /         /
005300080616     ** --------- --- --------- ------------------------------
005400071129     **
005500080616     ** PARAMETRI DELLA SOTTOPROCEDURA: UBWRIVAT_Write
005600071129     **
005700071129     ** Posizione Uso Tipo      Descrizione
005800071129     ** --------- --- --------- ------------------------------
005900080616     ** 1         IN  CHARACTER DS FIVAT00F
006000080616     ** 2         IN  CHARACTER DS EDIVAT0F
006100080616     ** 3         IN  CHARACTER Scelta file:
006200080616     **                         - F => FIVAT
006300080616     **                         - E => EDIVAT
006400071129     ** --------- --- --------- ------------------------------
006500071128     **
006600071128     **
006700071128     ** ESEMPI DI CHIAMATA
006800071128     **
006900080616     ** if UBWRIVAT_StrCmtCtl() = 0
007000080616     ** if UBWRIVAT_StrCmtCtl(pInCommitScope) = 0
007100080616     ** if UBWRIVAT_Write(pInFIVATds:pInEDIVATds:pInFile) = 0
007200080616     ** if UBWRIVAT_RollBack() = 0
007300080612     **
007400080612     ** --------- --- --------- ------------------------------
007500071128     **
007600071128     ** ISTRUZIONI PER LA COMPILAZIONE
007700071128     **
007800080616     ** 1. Creare il modulo UBWRIVAT (Opz 15 PDM)
007900080616     ** 2. Creare/Aggiornare il programma di servizio UBWRIVAT (CRTSRVPGM / UPDSRVPGM)
008000080610     ** 4. Cancellare il modulo.
008100071128     **
008200071128     ***********************************************************************************************
008300080612
008400080612
008500080612      //********************************************************************************************
008600080612      //
008700080612      // Definizione file (globali)
008800080612      //
008900080612      //********************************************************************************************
009000080617     FFIVAT00F  UF A E           K DISK    extfile(LibFileFIVAT)
009100080616     F                                     commit(CmtFile)
009200080616     F                                     usropn
009300080617     FEDIVAT0F  UF A E           K DISK    extfile(LibFileEDIVAT)
009400080616     F                                     commit(CmtFile)
009500080616     F                                     usropn
009600071128
009700080612
009800071128      //********************************************************************************************
009900071128      //
010000080612      // Definizione prototipi procedure (globali)
010100071128      //
010200071128      //********************************************************************************************
010300080616     D/COPY GAITRASRC/SRCPROTOPR,UBWRIVAT
010400080616     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
010500080612
010600080612
010700071128      //********************************************************************************************
010800071128      //
010900080612      // Definizione variabili work (globali)
011000071128      //
011100071128      //********************************************************************************************
011200080612     D InitDone        S               N   INZ(*off)
011300080616     D LibFileFIVAT    S             21A   INZ
011400080616     D LibFileEDIVAT   S             21A   INZ
011401080618
011402080618     D FIVAT00F      E DS                  BASED(NULLPTR)
011403080618     D EDIVAT0F      E DS                  BASED(NULLPTR)
011404080618
011500080616
011600080616     D executeCommand...
011700080616     D                 PR            10I 0 EXTPROC('system')
011800080616     D  command                        *   VALUE OPTIONS(*STRING)
011900080616
012000080616     D exceptionMessageID...
012100080616     D                 S              7A   IMPORT('_EXCP_MSGID')
012200071129
012300080612
012400080612     P*--------------------------------------------------
012500080616     P* Procedure name: UBWRIVAT_Init
012600080612     P* Purpose:        Inizializzazioni
012700080612     P* Returns:        Integer (Status_Code)
012800080612     P*--------------------------------------------------
012900080612     P*
013000080616     P UBWRIVAT_Init...
013100080612     P                 B                   EXPORT
013200080612     D*
013300080616     D UBWRIVAT_Init...
013400080612     D                 PI            10I 0
013500080617     D pInFileCmt                     1N   CONST
013600080612     D*
013700080612      //********************************************************************************************
013800080612      //
013900080612      // Definizione variabili work (locali)
014000080612      //
014100080612      //********************************************************************************************
014200080612     D locMethodCode   S             10I 0
014300080616     D currSysNeta     S              8A
014400080612
014500080612      /FREE
014600080612        // Inizializzo opcode di metodo
014700080612        locMethodCode = -1;
014800080612        InitDone = *off;
014900080616
015000080616
015100080616        // Gestione file sotto controllo sincronia
015200080617        CmtFile = pInFileCmt;
015300080616
015400080616
015500080616        // Verifico il sistema AS/400 corrente
015600080616        if UBRTVNETA_Rtv(currSysNeta) = 0;
015700080617           if %subst(currSysNeta:1:6) = 'SETRAS';
015800080616              LibFileFIVAT  = 'FILTRAGRU/FIVAT00F';
015900080616              LibFileEDIVAT = 'FILTRAGRU/EDIVAT0F';
016000080616           else;
016100080616              LibFileFIVAT  = 'FILTRAGRPF/FIVAT00F';
016200080616              LibFileEDIVAT = 'FILTRAGRPF/EDIVAT0F';
016300080616           endif;
016400080616
016500080616           // Se necessario apro i file
016600080616           if not %open(fivat00f);
016700080616              open fivat00f;
016800080616           endif;
016900080616           if not %open(edivat0f);
017000080616              open edivat0f;
017100080616           endif;
017200080612
017300080612
017400080616           // Valorizzo opcode di metodo
017500080616           locMethodCode = 0;
017600080616           InitDone = *on;
017700080616
017800080616        endif;
017900080612
018000080612        return locMethodCode;
018100080612
018200080612      /END-FREE
018300080612
018400080616     P UBWRIVAT_Init...
018500080612     P                 E
018600080616
018700080616
018800080616
018900080616     P*--------------------------------------------------
019000080616     P* Procedure name: UBWRIVAT_StrCmtCtl
019100080616     P* Purpose:        Avvia il controllo sincronia
019200080616     P* Returns:        Integer (Status_Code)
019300080616     P*--------------------------------------------------
019400080616     P*
019500080616     P UBWRIVAT_StrCmtCtl...
019600080616     P                 B                   EXPORT
019700080616     D*
019800080616     D UBWRIVAT_StrCmtCtl...
019900080616     D                 PI            10I 0
020000080617     D pInCommitScope                 1S 0 OPTIONS(*NOPASS:*OMIT) CONST
020100080616     D*
020200080616      //********************************************************************************************
020300080616      //
020400080616      // Definizione variabili work (locali)
020500080616      //
020600080616      //********************************************************************************************
020700080616     D locMethodCode   S             10I 0
020800080616     D locCommitScope  S              1S 0
020900080616
021000080616      /FREE
021100080616        // Inizializzo opcode di metodo
021200080616        locMethodCode = -1;
021300080616
021400080616
021500080616        // Verifico se ricevuto parametro relativo allo scope
021600080616        if %parms = 1 AND %ADDR(pInCommitScope) <> *NULL;
021700080616           locCommitScope = pInCommitScope;
021800080616           // Verifico i parametri ricevuti in input
021900080616           if locCommitScope = 0 OR locCommitScope = 1;
022000080616           else;
022100080616              return locMethodCode;
022200080616           endif;
022300080616        else;
022400080616           locCommitScope = 0;
022500080616        endif;
022600080616
022700080616
022800080616        // Avvia il controllo sincronia
022900080616        if locCommitScope = 0;
023000080616           if executeCommand('STRCMTCTL LCKLVL(*ALL) CMTSCOPE(*ACTGRP)') <> 0;
023100080616              if exceptionMessageID <> 'CPF8351';
023200080616                 DUMP(A);
023300080616                 return locMethodCode;
023400080616              endif;
023500080616           endif;
023600080616        endif;
023700080616        if locCommitScope = 1;
023800080616           if executeCommand('STRCMTCTL LCKLVL(*ALL) CMTSCOPE(*JOB)') <> 0;
023900080616              if exceptionMessageID <> 'CPF8351';
024000080616                 DUMP(A);
024100080616                 return locMethodCode;
024200080616              endif;
024300080616           endif;
024400080616        endif;
024500080616
024600080616
024700080616        // Valorizzo opcode di metodo
024800080616        locMethodCode = 0;
024900080616
025000080616        return locMethodCode;
025100080616
025200080616      /END-FREE
025300080616
025400080616     P UBWRIVAT_StrCmtCtl...
025500080616     P                 E
025600080616
025700080616
025800080616
025900080616     P*--------------------------------------------------
026000080616     P* Procedure name: UBWRIVAT_Write
026100080616     P* Purpose:        Gestione scrittura
026200080616     P* Returns:        Integer (Status_Code)
026300080616     P*--------------------------------------------------
026400080616     P*
026500080616     P UBWRIVAT_Write...
026600080616     P                 B                   EXPORT
026700080616     D*
026800080616     D UBWRIVAT_Write...
026900080616     D                 PI            10I 0
027000080618     D pInFIVATds                          LIKEDS(FIVAT00F)
027100080617     D                                     OPTIONS(*NOPASS) CONST
027200080618     D pInEDIVATds                         LIKEDS(EDIVAT0F)
027300080617     D                                     OPTIONS(*NOPASS) CONST
027400080617     D pInFile                        1A   CONST
027500080616     D*
027600080616      //********************************************************************************************
027700080616      //
027800080616      // Definizione variabili work (locali)
027900080616      //
028000080616      //********************************************************************************************
028100080616     D locMethodCode   S             10I 0
028200080617     D FIVATds       E DS                  extname(FIVAT00F)
028300080617     D EDIVATds      E DS                  extname(EDIVAT0F)
028400080617     D
028500080616
028600080616      /FREE
028700080616        // Inizializzo opcode di metodo
028800080616        locMethodCode = -1;
028900080616
029000080616
029100080616        // Verifico se già evocato il metodo Init
029200080616        if not InitDone;
029300080616           // Se nn già evocato lo evoco con commit *yes
029400080616           if UBWRIVAT_Init('1') = 0;
029500080616           else;
029600080616              return locMethodCode;
029700080616           endif;
029800080616        endif;
029900080616
030000080616
030100080616        // Effettiva scrittura dei buffer d output
030200080616        if pInFile = 'F';
030300080616           if %ADDR(pInFIVATds) <> *NULL;
030400080617              fivatds = pInFIVATds;
030500080617              write fivat000;
030600080616           else;
030700080616              return locMethodCode;
030800080616           endif;
030900080616        endif;
031000080616        if pInFile = 'E';
031100080616           if %ADDR(pInEDIVATds) <> *NULL;
031200080617              edivatds = pInEDIVATds;
031300080617              write edivat00;
031400080616           else;
031500080616              return locMethodCode;
031600080616           endif;
031700080616        endif;
031800080616
031900080616
032000080616        // Valorizzo opcode di metodo
032100080616        locMethodCode = 0;
032200080616
032300080616        return locMethodCode;
032400080616
032500080616      /END-FREE
032600080616
032700080616     P UBWRIVAT_Write...
032800080616     P                 E
032900080616
033000080616
033100080616
033200080616     P*--------------------------------------------------
033300080616     P* Procedure name: UBWRIVAT_Commit
033400080616     P* Purpose:        Sancisce il commit
033500080616     P* Returns:        Integer (Status_Code)
033600080616     P*--------------------------------------------------
033700080616     P*
033800080616     P UBWRIVAT_Commit...
033900080616     P                 B                   EXPORT
034000080616     D*
034100080616     D UBWRIVAT_Commit...
034200080616     D                 PI            10I 0
034300080616     D*
034400080616      //********************************************************************************************
034500080616      //
034600080616      // Definizione variabili work (locali)
034700080616      //
034800080616      //********************************************************************************************
034900080616     D locMethodCode   S             10I 0
035000080616
035100080616      /FREE
035200080616        // Inizializzo opcode di metodo
035300080616        locMethodCode = -1;
035400080616
035500080616
035600080616        // Sancisce le transazioni in sospeso
035700080616        commit;
035800080616
035900080616
036000080616        // Valorizzo opcode di metodo
036100080616        locMethodCode = 0;
036200080616
036300080616        return locMethodCode;
036400080616
036500080616      /END-FREE
036600080616
036700080616     P UBWRIVAT_Commit...
036800080616     P                 E
036900080616
037000080616
037100080616
037200080616     P*--------------------------------------------------
037300080616     P* Procedure name: UBWRIVAT_RollBack
037400080616     P* Purpose:        RollBack delle transazioni in sospeso
037500080616     P* Returns:        Integer (Status_Code)
037600080616     P*--------------------------------------------------
037700080616     P*
037800080616     P UBWRIVAT_RollBack...
037900080616     P                 B                   EXPORT
038000080616     D*
038100080616     D UBWRIVAT_RollBack...
038200080616     D                 PI            10I 0
038300080616     D*
038400080616      //********************************************************************************************
038500080616      //
038600080616      // Definizione variabili work (locali)
038700080616      //
038800080616      //********************************************************************************************
038900080616     D locMethodCode   S             10I 0
039000080616
039100080616      /FREE
039200080616        // Inizializzo opcode di metodo
039300080616        locMethodCode = -1;
039400080616
039500080616
039600080616        // Annulla le transazioni in sospeso
039700080616        rolbk;
039800080616
039900080616
040000080616        // Valorizzo opcode di metodo
040100080616        locMethodCode = 0;
040200080616
040300080616        return locMethodCode;
040400080616
040500080616      /END-FREE
040600080616
040700080616     P UBWRIVAT_RollBack...
040800080616     P                 E
040900080616
041000080616
041100080616
041200080616     P*--------------------------------------------------
041300080616     P* Procedure name: UBWRIVAT_EndCmtCtl
041400080616     P* Purpose:        Termina il controllo sincronia
041500080616     P* Returns:        Integer (Status_Code)
041600080616     P*--------------------------------------------------
041700080616     P*
041800080616     P UBWRIVAT_EndCmtCtl...
041900080616     P                 B                   EXPORT
042000080616     D*
042100080616     D UBWRIVAT_EndCmtCtl...
042200080616     D                 PI            10I 0
042300080616     D*
042400080616      //********************************************************************************************
042500080616      //
042600080616      // Definizione variabili work (locali)
042700080616      //
042800080616      //********************************************************************************************
042900080616     D locMethodCode   S             10I 0
043000080616
043100080616      /FREE
043200080616        // Inizializzo opcode di metodo
043300080616        locMethodCode = -1;
043400080616
043500080616
043600080616        // Sancisce le transazioni in sospeso
043700080616        if executeCommand('ENDCMTCTL') <> 0;
043800080616           if exceptionMessageID <> 'CPF8356';
043900080616              DUMP(A);
044000080616              return locMethodCode;
044100080616           endif;
044200080616        endif;
044300080616
044400080616        // Valorizzo opcode di metodo
044500080616        locMethodCode = 0;
044600080616
044700080616        return locMethodCode;
044800080616
044900080616      /END-FREE
045000080616
045100080616     P UBWRIVAT_EndCmtCtl...
045200080616     P                 E
045300080612
045400080612
045500080612
045600080612     P*--------------------------------------------------
045700080616     P* Procedure name: UBWRIVAT_Finalize
045800080612     P* Purpose:        Finalizzazioni
045900080612     P* Returns:        Integer (Status_Code)
046000080612     P*--------------------------------------------------
046100080612     P*
046200080616     P UBWRIVAT_Finalize...
046300080612     P                 B                   EXPORT
046400080612     D*
046500080616     D UBWRIVAT_Finalize...
046600080612     D                 PI            10I 0
046700080612     D*
046800080612      //********************************************************************************************
046900080612      //
047000080612      // Definizione variabili work (locali)
047100080612      //
047200080612      //********************************************************************************************
047300080612     D locMethodCode   S             10I 0
047400080612
047500080612      /FREE
047600080612        // Inizializzo opcode di metodo
047700080612        locMethodCode = -1;
047800080612
047900080616
048000080612        // Se necessario chiudo i file
048100080616        if %open(fivat00f);
048200080616           close fivat00f;
048300080612        endif;
048400080616        if %open(edivat0f);
048500080616           close edivat0f;
048600080612        endif;
048700080612
048800080612
048900080612        // Valorizzo opcode di metodo
049000080612        locMethodCode = 0;
049100080612
049200080612        return locMethodCode;
049300080612
049400080612      /END-FREE
049500080612
049600080616     P UBWRIVAT_Finalize...
049700080612     P                 E
