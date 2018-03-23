000100151125     H BNDDIR('UBBNDDIR':'QC2LE')
000200080611     H NOMAIN
000300071128     ***********************************************************************************************
000400071128     **
000500161017     ** Questo modulo fornisce funzionalità varie, sia standard che utente, relativamente
000600161017     ** alla gestione dei file/cartelle nel IFS
000700071128     **
000800080611     ** Può essere usato solo come sottoprocedura.
000900080612     ** Ogni metodo restituisce un valore 10I:
001000080612     **             - = 0 => tutto ok
001100080612     **             - < 0 => errore
001200080612     **
001300151126     **
001400161017     ** --------- --- --------- ------------------------------
001500161017     **
001600161017     ** PARAMETRI DELLA SOTTOPROCEDURA: UBIFSUTIR_GetFileNameElements...
001700161017     **
001800161017     ** Posizione Uso Tipo      Descrizione
001900161017     ** --------- --- --------- ------------------------------
002000161017     ** 1         IN  CHARACTER Nome e/o percorso file
002100161017     ** 2         OUT CHARACTER Percorso file
002200161017     ** 3         OUT CHARACTER Nome file completo (nome + estensione)
002300161017     ** 4         OUT CHARACTER Nome file no estensione
002400161017     ** 5         OUT CHARACTER Estensione
002500161017     ** --------- --- --------- ------------------------------
002600071128     **
002700071128     **
002800071128     ** ESEMPI DI CHIAMATA
002900071128     **
003000161019     **  if UBIFSUTIR_GetFileSizeByte(iIFSUTIfile : oIFSUTIsizeByte) = 0;
003100161017     **
003200161017     **
003300080612     ** --------- --- --------- ------------------------------
003400071128     **
003500071128     ** ISTRUZIONI PER LA COMPILAZIONE
003600071128     **
003700161017     ** 1. Creare il modulo UBIFSUTIR (Opz 15 PDM) indicando BNDDIR('UBBNDDIR':'QC2LE')
003800161017     ** 2. Creare/Aggiornare il programma di servizio UBIFSUTIR (CRTSRVPGM / UPDSRVPGM)
003900080610     ** 4. Cancellare il modulo.
004000071128     **
004100071128     ***********************************************************************************************
004200080612
004300080612
004400080612      //********************************************************************************************
004500080612      //
004600080612      // Definizione file (globali)
004700080612      //
004800080612      //********************************************************************************************
004900071128
005000080612
005100071128      //********************************************************************************************
005200071128      //
005300080612      // Definizione prototipi procedure (globali)
005400071128      //
005500071128      //********************************************************************************************
005600161017     D/COPY GAITRASRC/SRCPROTOPR,UBIFSUTIR
005700161017     D/COPY GAITRASRC/SRCPROTOPI,UBIFSUTIR
005800161017     D/COPY GAITRASRC/SRCPROTOPR,UBIFS
005900161021     D/COPY GAITRASRC/SRCPROTOPI,UBIFS
006000161018     D/COPY GAITRASRC/SRCPROTOPR,UBBASE64R
006100161018     D/COPY GAITRASRC/SRCPROTOPI,UBBASE64R
006200080612
006300080612
006400071128      //********************************************************************************************
006500071128      //
006600080612      // Definizione variabili work (globali)
006700071128      //
006800071128      //********************************************************************************************
006900151126     D InitDone        S               N   INZ(*off)
007000161017     D
007100071129
007200151126
007300160404
007400161017     P*--------------------------------------------------
007500161017     P* Procedure name: UBIFSUTIR_GetFileNameElements
007600161017     P* Purpose:        Riconosce e separa gli elementi che compongono il percorso e/o nome file
007700160404     P* Returns:        Integer (Status_Code)
007800161017     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
007900161017     P* Parameter:      oIFSUTIpath     => Percorso file
008000161017     P* Parameter:      oIFSUTIfullName => Nome file completo (nome + estensione)
008100161017     P* Parameter:      oIFSUTIName     => Nome file no estensione
008200161017     P* Parameter:      oIFSUTIExt      => Estensione
008300160404     P*--------------------------------------------------
008400160404     P*
008500161017     P UBIFSUTIR_GetFileNameElements...
008600160404     P                 B                   EXPORT
008700160404     D*
008800161017     D UBIFSUTIR_GetFileNameElements...
008900160404     D                 PI            10I 0
009000161017     DiIFSUTIfile                  1024A   CONST
009100161017     DoIFSUTIpath                   512A
009200161017     DoIFSUTIfullName               512A
009300161017     DoIFSUTIName                   512A
009400161017     DoIFSUTIExt                     32A
009500160404     D*
009600160404      //********************************************************************************************
009700160404      //
009800160404      // Definizione variabili work (locali)
009900160404      //
010000160404      //********************************************************************************************
010100160404     D locMethodCode   S             10I 0
010200160404     D
010300161024     DwIFSUTIfile      S                   INZ LIKE(iIFSUTIfile)
010400161017     D wPosPathName    S              4S 0 inz
010500161017     D wPosNameExt     S              4S 0 inz
010600161017     D wPos            S              4S 0 inz
010700160404
010800160404      /FREE
010900161017        // Inizializzo opcode di metodo a errore
011000160404        locMethodCode = -1;
011100161017
011200161017
011300161017        // Monitorizzo l'intero processo
011400161017        Monitor;
011500161017
011600161017        // Inizializzo i parametri di output
011700161017        clear oIFSUTIpath;
011800161017        clear oIFSUTIfullName;
011900161017        clear oIFSUTIName;
012000161017        clear oIFSUTIExt;
012100160404
012200161017        // Prima di tutto "noralizzo" carattere di separazione gerarchica File System
012300161017        wIFSUTIfile = %scanrpl('\':'/':iIFSUTIfile);
012400161019
012500161019        // Verifico che il path/file inizi sempre con il carattere '/'
012600161019        if %subst(wIFSUTIfile:1:1) <> '/';
012700161019           wIFSUTIfile = '/' + %trim(wIFSUTIfile);
012800161019        endif;
012900161017
013000161017        // Reperisco posisionze del ultimo carattere separazione generachica FS
013100161017        wPosPathName = *zeros;
013200161017        wPos = %scan('/':wIFSUTIfile:1);
013300161017        dow wPos > *zeros AND wPos < %len(%trim(wIFSUTIfile));
013400161017            if wPos > *zeros;
013500161017               wPosPathName = wPos;
013600161017            endif;
013700161017            wPos = wPos + 1;
013800161017            wPos = %scan('/':wIFSUTIfile:wPos);
013900161017        enddo;
014000161017
014100161017        // Determino il path e il nome file full (nome + estensione)
014200161017        oIFSUTIpath     = %subst(wIFSUTIfile:1:wPosPathName-1);
014300161017        oIFSUTIfullName = %subst(wIFSUTIfile:wPosPathName+1);
014400161017
014500161017
014600161017        // Reperisco posisionze del ultimo '.' di separazione elementi nome file
014700161017        wPosNameExt = *zeros;
014800161017        wPos = %scan('.':oIFSUTIfullName:1);
014900161017        dow wPos > *zeros AND wPos < %len(%trim(oIFSUTIfullName));
015000161017            if wPos > *zeros;
015100161017               wPosNameExt = wPos;
015200161017            endif;
015300161017            wPos = wPos + 1;
015400161017            wPos = %scan('.':oIFSUTIfullName:wPos);
015500161017        enddo;
015600161017
015700161017        // Determino il path e il nome file no estenzione e l'estensione
015800161017        oIFSUTIName = %subst(oIFSUTIfullName:1:wPosNameExt-1);
015900161017        oIFSUTIExt  = %subst(oIFSUTIfullName:wPosNameExt+1);
016000160404
016100161017
016200161017        On-Error;
016300161017
016400161017           // Esito chiamata già inizializzato a errore e parametri output già puliti
016500161017
016600161017        // Fine monitoring
016700161017        EndMon;
016800161017
016900161017
017000161017        // Valorizzo opcode di metodo
017100161017        locMethodCode = 0;
017200160404
017300160404        return locMethodCode;
017400160404
017500160404      /END-FREE
017600160404
017700161017     P UBIFSUTIR_GetFileNameElements...
017800160404     P                 E
017900161017
018000161017
018100161017
018200161017     P*--------------------------------------------------
018300161019     P* Procedure name: UBIFSUTIR_GetFileSizeByte
018400161019     P* Purpose:        Reperisce la dimensione di un file in Byte
018500161017     P* Returns:        Integer (Status_Code)
018600161017     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
018700161019     P* Parameter:      oIFSUTIsizeByte => Dimensione file in Byte
018800161017     P*--------------------------------------------------
018900161017     P*
019000161019     P UBIFSUTIR_GetFileSizeByte...
019100161017     P                 B                   EXPORT
019200161017     D*
019300161019     D UBIFSUTIR_GetFileSizeByte...
019400161017     D                 PI            10I 0
019500161017     DiIFSUTIfile                  1024A   CONST
019600161019     DoIFSUTIsizeByte                10I 0
019700161017     D*
019800161017      //********************************************************************************************
019900161017      //
020000161017      // Definizione variabili work (locali)
020100161017      //
020200161017      //********************************************************************************************
020300161017     D locMethodCode   S             10I 0
020400161017     D
020500161024     DwIFSUTIfile      S                   INZ LIKE(iIFSUTIfile)
020600161024     DwIFSUTIstatDS    DS                  INZ LIKEDS(IFSstatDS)
020700161017
020800161017      /FREE
020900161017        // Inizializzo opcode di metodo a errore
021000161017        locMethodCode = -1;
021100161017
021200161017
021300161017        // Monitorizzo l'intero processo
021400161017        Monitor;
021500161019
021600161019        // Verifico che il path/file inizi sempre con il carattere '/'
021700161019        wIFSUTIfile = iIFSUTIfile;
021800161019        if %subst(wIFSUTIfile:1:1) <> '/';
021900161019           wIFSUTIfile = '/' + %trim(wIFSUTIfile);
022000161019        endif;
022100161017
022200161017        // Inizializzo i parametri di output
022300161019        clear oIFSUTIsizeByte;
022400161017
022500161017        // Reperisco gli attributi del file ricevuto in input
022600161017        clear wIFSUTIstatDS;
022700161019        IFSrc = IFS_GetFileInfo(%trim(wIFSUTIfile):%addr(wIFSUTIstatDS));
022800161017
022900161017        // Se reperimento attributi file riuscito
023000161017        if IFSrc >= *zeros;
023100161019           // Ritorno la dimensione del file in Byte
023200161019           oIFSUTIsizeByte = wIFSUTIstatDS.st_size;
023300161021
023400161021           // Valorizzo opcode di metodo
023500161021           locMethodCode = 0;
023600161017        endif;
023700161017
023800161017
023900161017        On-Error;
024000161017
024100161017           // Esito chiamata già inizializzato a errore e parametri output già puliti
024200161017
024300161017        // Fine monitoring
024400161017        EndMon;
024500161017
024600161017
024700161017        return locMethodCode;
024800161017
024900161017      /END-FREE
025000161017
025100161019     P UBIFSUTIR_GetFileSizeByte...
025200161017     P                 E
025300161021
025400161021
025500161021
025600161021     P*--------------------------------------------------
025700161021     P* Procedure name: UBIFSUTIR_GetFileEncoding
025800161021     P* Purpose:        Reperisce la CodePage e il CCSID
025900161021     P* Returns:        Integer (Status_Code)
026000161021     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
026100161021     P* Parameter:      oIFSUTIcodePage => CodePage del file
026200161021     P* Parameter:      oIFSUTI_CCSID   => CCSID del file
026300161021     P*--------------------------------------------------
026400161021     P*
026500161021     P UBIFSUTIR_GetFileEncoding...
026600161021     P                 B                   EXPORT
026700161021     D*
026800161021     D UBIFSUTIR_GetFileEncoding...
026900161021     D                 PI            10I 0
027000161021     DiIFSUTIfile                  1024A   CONST
027100161021     DoIFSUTIcodePage                 5U 0
027200161021     DoIFSUTI_CCSID                   5U 0
027300161021     D*
027400161021      //********************************************************************************************
027500161021      //
027600161021      // Definizione variabili work (locali)
027700161021      //
027800161021      //********************************************************************************************
027900161021     D locMethodCode   S             10I 0
028000161021     D
028100161024     DwIFSUTIfile      S                   INZ LIKE(iIFSUTIfile)
028200161024     DwIFSUTIstatDS    DS                  INZ LIKEDS(IFSstatDS)
028300161021
028400161021      /FREE
028500161021        // Inizializzo opcode di metodo a errore
028600161021        locMethodCode = -1;
028700161021
028800161021
028900161021        // Monitorizzo l'intero processo
029000161021        Monitor;
029100161021
029200161021        // Verifico che il path/file inizi sempre con il carattere '/'
029300161021        wIFSUTIfile = iIFSUTIfile;
029400161021        if %subst(wIFSUTIfile:1:1) <> '/';
029500161021           wIFSUTIfile = '/' + %trim(wIFSUTIfile);
029600161021        endif;
029700161021
029800161021        // Inizializzo i parametri di output
029900161021        clear oIFSUTIcodePage;
030000161021        clear oIFSUTI_CCSID;
030100161021
030200161021        // Reperisco gli attributi del file ricevuto in input
030300161021        clear wIFSUTIstatDS;
030400161021        IFSrc = IFS_GetFileInfo(%trim(wIFSUTIfile):%addr(wIFSUTIstatDS));
030500161021
030600161021        // Se reperimento attributi file riuscito
030700161021        if IFSrc >= *zeros;
030800161021           // Ritorno la dimensione del file in Byte
030900161021           oIFSUTIcodePage = wIFSUTIstatDS.st_codepage;
031000161021           oIFSUTI_CCSID   = wIFSUTIstatDS.st_ccsid;
031100161021
031200161021           // Valorizzo opcode di metodo
031300161021           locMethodCode = 0;
031400161021        endif;
031500161021
031600161021
031700161021        On-Error;
031800161021
031900161021           // Esito chiamata già inizializzato a errore e parametri output già puliti
032000161021
032100161021        // Fine monitoring
032200161021        EndMon;
032300161021
032400161021
032500161021        return locMethodCode;
032600161021
032700161021      /END-FREE
032800161021
032900161021     P UBIFSUTIR_GetFileEncoding...
033000161021     P                 E
033100161025
033200161025
033300161025
033400161025     P*--------------------------------------------------
033500161025     P* Procedure name: UBIFSUTIR_ReadFileUpTo16K
033600161026     P* Purpose:        Legge "one-shot" intero file (dimensioni max 16KB)
033700161025     P* Returns:        Integer (Status_Code)
033800161025     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
033900161025     P* Parameter:      iIFSUTIsizeByte => Dimensione file attesa
034000161028     P* Parameter:      oIFSUTIsizeByte => Dimensione file rilevata
034100161025     P* Parameter:      oIFSUTIdata16K  => Contenuto del file
034200161025     P*--------------------------------------------------
034300161025     P*
034400161025     P UBIFSUTIR_ReadFileUpTo16K...
034500161025     P                 B                   EXPORT
034600161025     D*
034700161025     D UBIFSUTIR_ReadFileUpTo16K...
034800161025     D                 PI            10I 0
034900161025     DiIFSUTIfile                  1024A   CONST
035000161025     DiIFSUTIsizeByte                10I 0 CONST
035100161028     DoIFSUTIsizeByte                10I 0
035200161025     DoIFSUTIdata16K              16384A   VARYING OPTIONS(*VARSIZE)
035300161025     D*
035400161025      //********************************************************************************************
035500161025      //
035600161025      // Definizione variabili work (locali)
035700161025      //
035800161025      //********************************************************************************************
035900161025     D locMethodCode   S             10I 0
036000161025     D
036100161025     DwIFSUTIfile      S                   INZ LIKE(iIFSUTIfile)
036200161102     DwIFSUTIdata16K   S          16384A   INZ
036300161025     D wLen            S             10I 0 inz
036400161025
036500161025      /FREE
036600161025        // Inizializzo opcode di metodo a errore
036700161025        locMethodCode = -1;
036800161025
036900161025
037000161025        // Monitorizzo l'intero processo
037100161025        Monitor;
037200161026
037300161026        // Inizializzo parametri di output
037400161026        clear oIFSUTIdata16K;
037500161028        clear oIFSUTIsizeByte;
037600161025
037700161025        // Verifico che il path/file inizi sempre con il carattere '/'
037800161025        wIFSUTIfile = iIFSUTIfile;
037900161025        if %subst(wIFSUTIfile:1:1) <> '/';
038000161025           wIFSUTIfile = '/' + %trim(wIFSUTIfile);
038100161025        endif;
038200161025
038300161025        // Innanzitutto apro il file in sola lettura
038400161025        IFSfileDescr = IFS_OpenFile(%trim(wIFSUTIfile):O_RDONLY);
038500161025
038600161025        // Innanzitutto apro il file in sola lettura
038700161025        if IFSfileDescr >= *zeros;
038800161102           wLen = IFS_ReadFile(IFSfileDescr:%addr(wIFSUTIdata16K):
038900161102                                            %size(wIFSUTIdata16K));
039000161025           if wLen > *zeros;
039100161025
039200161025              // Valorizzo opcode di metodo a OK
039300161025              locMethodCode = 0;
039400161028
039500161028              // Valorizzo buffer di output
039600161028              oIFSUTIsizeByte = wLen;
039700161025
039800161025              // Se passata in input anche la dimensione attesa del file
039900161025              // effettuo verifica di congruenza
040000161025              if iIFSUTIsizeByte > *zeros;
040100161030                 if iIFSUTIsizeByte = oIFSUTIsizeByte;
040200161030
040300161030                    // Valorizo parametri di output
040400161102                    oIFSUTIdata16K = %subst(wIFSUTIdata16K:1:oIFSUTIsizeByte);
040500161030
040600161025                 else;
040700161025
040800161030                    // Per sicurezza inizializzo parametri di output
040900161030                    clear oIFSUTIdata16K;
041000161030
041100161025                    // Valorizzo opcode di metodo a KO = discepanza dimensione file attesa
041200161025                    locMethodCode = -2;
041300161025
041400161025                 endif;
041500161025              endif;
041600161025
041700161025           endif;
041800161025
041900161025           // Chiudo il file nel IFS
042000161025           IFS_CloseFile(IFSfileDescr);
042100161025
042200161025        endif;
042300161025
042400161025
042500161025        On-Error;
042600161025
042700161025           // Esito chiamata già inizializzato a errore e parametri output già puliti
042800161025
042900161025           // Monitorizzo anche quanto eventualmente eseguito nella gestione errore superiore
043000161025           Monitor;
043100161025              // Chiudo il file nel IFS
043200161025              IFS_CloseFile(IFSfileDescr);
043300161025           On-Error;
043400161025           // Fine monitoring
043500161025           EndMon;
043600161025
043700161025        // Fine monitoring
043800161025        EndMon;
043900161025
044000161025
044100161025        return locMethodCode;
044200161025
044300161025      /END-FREE
044400161025
044500161026     P UBIFSUTIR_ReadFileUpTo16K...
044600161025     P                 E
044700161025
044800161025
044900161025
045000161025     P*--------------------------------------------------
045100161025     P* Procedure name: UBIFSUTIR_ReadFileUpTo32K
045200161026     P* Purpose:        Legge "one-shot" intero file (dimensioni max 32KB)
045300161025     P* Returns:        Integer (Status_Code)
045400161025     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
045500161025     P* Parameter:      iIFSUTIsizeByte => Dimensione file attesa
045600161028     P* Parameter:      oIFSUTIsizeByte => Dimensione file rilevata
045700161025     P* Parameter:      oIFSUTIdata32K  => Contenuto del file
045800161025     P*--------------------------------------------------
045900161025     P*
046000161025     P UBIFSUTIR_ReadFileUpTo32K...
046100161025     P                 B                   EXPORT
046200161025     D*
046300161025     D UBIFSUTIR_ReadFileUpTo32K...
046400161025     D                 PI            10I 0
046500161025     DiIFSUTIfile                  1024A   CONST
046600161025     DiIFSUTIsizeByte                10I 0 CONST
046700161028     DoIFSUTIsizeByte                10I 0
046800161025     DoIFSUTIdata32K              32768A   VARYING OPTIONS(*VARSIZE)
046900161025     D*
047000161025      //********************************************************************************************
047100161025      //
047200161025      // Definizione variabili work (locali)
047300161025      //
047400161025      //********************************************************************************************
047500161025     D locMethodCode   S             10I 0
047600161025     D
047700161025     DwIFSUTIfile      S                   INZ LIKE(iIFSUTIfile)
047800161102     DwIFSUTIdata32K   S          32768A   INZ
047900161025     D wLen            S             10I 0 inz
048000161025
048100161025      /FREE
048200161025        // Inizializzo opcode di metodo a errore
048300161025        locMethodCode = -1;
048400161025
048500161025
048600161025        // Monitorizzo l'intero processo
048700161025        Monitor;
048800161026
048900161026        // Inizializzo parametri di output
049000161026        clear oIFSUTIdata32K;
049100161028        clear oIFSUTIsizeByte;
049200161025
049300161025        // Verifico che il path/file inizi sempre con il carattere '/'
049400161025        wIFSUTIfile = iIFSUTIfile;
049500161025        if %subst(wIFSUTIfile:1:1) <> '/';
049600161025           wIFSUTIfile = '/' + %trim(wIFSUTIfile);
049700161025        endif;
049800161025
049900161025        // Innanzitutto apro il file in sola lettura
050000161025        IFSfileDescr = IFS_OpenFile(%trim(wIFSUTIfile):O_RDONLY);
050100161025
050200161025        // Innanzitutto apro il file in sola lettura
050300161025        if IFSfileDescr >= *zeros;
050400161102           wLen = IFS_ReadFile(IFSfileDescr:%addr(wIFSUTIdata32K):
050500161102                                            %size(wIFSUTIdata32K));
050600161025           if wLen > *zeros;
050700161025
050800161025              // Valorizzo opcode di metodo a OK
050900161025              locMethodCode = 0;
051000161028
051100161028              // Valorizzo buffer di output
051200161028              oIFSUTIsizeByte = wLen;
051300161025
051400161025              // Se passata in input anche la dimensione attesa del file
051500161025              // effettuo verifica di congruenza
051600161025              if iIFSUTIsizeByte > *zeros;
051700161030                 if iIFSUTIsizeByte = oIFSUTIsizeByte;
051800161102
051900161102                    // Valorizo parametri di output
052000161102                    oIFSUTIdata32K = %subst(wIFSUTIdata32K:1:oIFSUTIsizeByte);
052100161102
052200161025                 else;
052300161030
052400161030                    // Per sicurezza inizializzo parametri di output
052500161030                    clear oIFSUTIdata32K;
052600161025
052700161025                    // Valorizzo opcode di metodo a KO = discepanza dimensione file attesa
052800161025                    locMethodCode = -2;
052900161025
053000161025                 endif;
053100161025              endif;
053200161025
053300161025           endif;
053400161025
053500161025           // Chiudo il file nel IFS
053600161025           IFS_CloseFile(IFSfileDescr);
053700161025
053800161025        endif;
053900161025
054000161025
054100161025        On-Error;
054200161025
054300161025           // Esito chiamata già inizializzato a errore e parametri output già puliti
054400161025
054500161025           // Monitorizzo anche quanto eventualmente eseguito nella gestione errore superiore
054600161025           Monitor;
054700161025              // Chiudo il file nel IFS
054800161025              IFS_CloseFile(IFSfileDescr);
054900161025           On-Error;
055000161025           // Fine monitoring
055100161025           EndMon;
055200161025
055300161025        // Fine monitoring
055400161025        EndMon;
055500161025
055600161025
055700161025        return locMethodCode;
055800161025
055900161025      /END-FREE
056000161025
056100161025     P UBIFSUTIR_ReadFileUpTo32K...
056200161025     P                 E
056300161018
056400161018
056500161018
056600161018     P*--------------------------------------------------
056700161018     P* Procedure name: UBIFSUTIR_ReadFileUpTo64K
056800161026     P* Purpose:        Legge "one-shot" intero file (dimensioni max 64KB)
056900161018     P* Returns:        Integer (Status_Code)
057000161018     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
057100161018     P* Parameter:      iIFSUTIsizeByte => Dimensione file attesa
057200161028     P* Parameter:      oIFSUTIsizeByte => Dimensione file rilevata
057300161018     P* Parameter:      oIFSUTIdata64K  => Contenuto del file
057400161018     P*--------------------------------------------------
057500161018     P*
057600161018     P UBIFSUTIR_ReadFileUpTo64K...
057700161018     P                 B                   EXPORT
057800161018     D*
057900161018     D UBIFSUTIR_ReadFileUpTo64K...
058000161018     D                 PI            10I 0
058100161018     DiIFSUTIfile                  1024A   CONST
058200161018     DiIFSUTIsizeByte                10I 0 CONST
058300161028     DoIFSUTIsizeByte                10I 0
058400161030     DoIFSUTIdata64K                   A   LEN(65536) VARYING OPTIONS(*VARSIZE)       (4 Byte x Len)
058500161018     D*
058600161018      //********************************************************************************************
058700161018      //
058800161018      // Definizione variabili work (locali)
058900161018      //
059000161018      //********************************************************************************************
059100161018     D locMethodCode   S             10I 0
059200161018     D
059300161024     DwIFSUTIfile      S                   INZ LIKE(iIFSUTIfile)
059400161102     DwIFSUTIdata64K   S          65536A   INZ
059500161018     D wLen            S             10I 0 inz
059600161018
059700161018      /FREE
059800161018        // Inizializzo opcode di metodo a errore
059900161018        locMethodCode = -1;
060000161018
060100161018
060200161018        // Monitorizzo l'intero processo
060300161018        Monitor;
060400161026
060500161026        // Inizializzo parametri di output
060600161026        clear oIFSUTIdata64K;
060700161028        clear oIFSUTIsizeByte;
060800161019
060900161019        // Verifico che il path/file inizi sempre con il carattere '/'
061000161019        wIFSUTIfile = iIFSUTIfile;
061100161019        if %subst(wIFSUTIfile:1:1) <> '/';
061200161019           wIFSUTIfile = '/' + %trim(wIFSUTIfile);
061300161019        endif;
061400161018
061500161018        // Innanzitutto apro il file in sola lettura
061600161019        IFSfileDescr = IFS_OpenFile(%trim(wIFSUTIfile):O_RDONLY);
061700161018
061800161018        // Innanzitutto apro il file in sola lettura
061900161018        if IFSfileDescr >= *zeros;
062000161102           wLen = IFS_ReadFile(IFSfileDescr:%addr(wIFSUTIdata64K):
062100161102                                            %size(wIFSUTIdata64K));
062200161018           if wLen > *zeros;
062300161018
062400161018              // Valorizzo opcode di metodo a OK
062500161018              locMethodCode = 0;
062600161028
062700161028              // Valorizzo buffer di output
062800161028              oIFSUTIsizeByte = wLen;
062900161018
063000161018              // Se passata in input anche la dimensione attesa del file
063100161018              // effettuo verifica di congruenza
063200161018              if iIFSUTIsizeByte > *zeros;
063300161030                 if iIFSUTIsizeByte = oIFSUTIsizeByte;
063400161102
063500161102                    // Valorizo parametri di output
063600161102                    oIFSUTIdata64K = %subst(wIFSUTIdata64K:1:oIFSUTIsizeByte);
063700161102
063800161019                 else;
063900161030
064000161030                    // Per sicurezza inizializzo parametri di output
064100161030                    clear oIFSUTIdata64K;
064200161018
064300161018                    // Valorizzo opcode di metodo a KO = discepanza dimensione file attesa
064400161018                    locMethodCode = -2;
064500161018
064600161018                 endif;
064700161018              endif;
064800161018
064900161018           endif;
065000161018
065100161018           // Chiudo il file nel IFS
065200161018           IFS_CloseFile(IFSfileDescr);
065300161018
065400161018        endif;
065500161018
065600161018
065700161018        On-Error;
065800161018
065900161018           // Esito chiamata già inizializzato a errore e parametri output già puliti
066000161018
066100161018           // Monitorizzo anche quanto eventualmente eseguito nella gestione errore superiore
066200161018           Monitor;
066300161018              // Chiudo il file nel IFS
066400161018              IFS_CloseFile(IFSfileDescr);
066500161018           On-Error;
066600161018           // Fine monitoring
066700161018           EndMon;
066800161018
066900161018        // Fine monitoring
067000161018        EndMon;
067100161018
067200161018
067300161018        return locMethodCode;
067400161018
067500161018      /END-FREE
067600161018
067700161018     P UBIFSUTIR_ReadFileUpTo64K...
067800161018     P                 E
067900161030
068000161030
068100161030
068200161030     P*--------------------------------------------------
068300161030     P* Procedure name: UBIFSUTIR_ReadFileUpTo256K
068400161109     P* Purpose:        Legge intero file (dimensioni max 256K)
068500161030     P* Returns:        Integer (Status_Code)
068600161030     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
068700161030     P* Parameter:      iIFSUTIsizeByte => Dimensione file attesa
068800161030     P* Parameter:      oIFSUTIsizeByte => Dimensione file rilevata
068900161030     P* Parameter:      oIFSUTIskCnt    => Numero elementi buffer dati output valorizzati
069000161030     P* Parameter:      oIFSUTILenLast  => Lunghezze ultimo elemento parziale
069100161030     P* Parameter:      oIFSUTIskD256K  => Elementi buffer output
069200161030     P*--------------------------------------------------
069300161030     P*
069400161030     P UBIFSUTIR_ReadFileUpTo256K...
069500161030     P                 B                   EXPORT
069600161030     D*
069700161030     D UBIFSUTIR_ReadFileUpTo256K...
069800161030     D                 PI            10I 0
069900161030     DiIFSUTIfile                  1024A   CONST
070000161030     DiIFSUTIsizeByte                10I 0 CONST
070100161030     DoIFSUTIsizeByte                10I 0
070200161030     DoIFSUTIskCnt                    5U 0
070300161030     DoIFSUTILenLast                 10I 0
070400161102     DoIFSUTIskD256K               8192A   VARYING OPTIONS(*VARSIZE)
070500161030     D                                     DIM(32)
070600161030     D*
070700161030      //********************************************************************************************
070800161030      //
070900161030      // Definizione variabili work (locali)
071000161030      //
071100161030      //********************************************************************************************
071200161030     D locMethodCode   S             10I 0
071300161030     D
071400161030     DwIFSUTIfile      S                   INZ LIKE(iIFSUTIfile)
071500161102     DwIFSUTIdata8K    S           8192A   INZ
071600161030     D wLen            S             10I 0 inz
071700161030     D wCicla          S               N   INZ
071800161030
071900161030      /FREE
072000161030        // Inizializzo opcode di metodo a errore
072100161030        locMethodCode = -1;
072200161030
072300161030
072400161030        // Monitorizzo l'intero processo
072500161030        Monitor;
072600161030
072700161030        // Inizializzo parametri di output
072800161030        clear oIFSUTIsizeByte;
072900161030        clear oIFSUTIskCnt;
073000161030        clear oIFSUTILenLast;
073100161030        clear oIFSUTIskD256K;
073200161030
073300161030        // Verifico che il path/file inizi sempre con il carattere '/'
073400161030        wIFSUTIfile = iIFSUTIfile;
073500161030        if %subst(wIFSUTIfile:1:1) <> '/';
073600161030           wIFSUTIfile = '/' + %trim(wIFSUTIfile);
073700161030        endif;
073800161030
073900161030        // Innanzitutto apro il file in sola lettura
074000161030        IFSfileDescr = IFS_OpenFile(%trim(wIFSUTIfile):O_RDONLY);
074100161030
074200161030
074300161030        // Ciclo lettura file (max 256K)
074400161030        wCicla = *on;
074500161030        dow wCicla;
074600161030
074700161030            // Incremento contatore elementi dati letti
074800161030            oIFSUTIskCnt = oIFSUTIskCnt + 1;
074900161030
075000161030            // Leggo "a blocchi" per dimensione ogni singolo elementi dati previsto
075100161030            wLen = IFS_ReadFile(IFSfileDescr
075200161102                               :%addr(wIFSUTIdata8K)
075300161102                               :%size(wIFSUTIdata8K));
075400161030
075500161030            select;
075600161030               // Errore
075700161030               when wLen < *zeros;
075800161030                    // Valorizzo procedura ad errore
075900161030                    locMethodCode = -1;
076000161030
076100161030                    // Uscita ciclo lettura
076200161030                    wCicla = *off;
076300161030
076400161030               // No dati letti
076500161030               when wLen = *zeros;
076600161030                    // Svuoto (per sicurezza) elemento corrente
076700161030                    clear oIFSUTIskD256K(oIFSUTIskCnt);
076800161030
076900161030                    // Decremento contatore elementi dati letti
077000161030                    oIFSUTIskCnt = oIFSUTIskCnt - 1;
077100161030
077200161030                    // Uscita ciclo lettura
077300161030                    wCicla = *off;
077400161030
077500161030               // Lettura OK
077600161030               when wLen > *zeros;
077700161030                    // Memorizzo quantità dati effettivamente letti
077800161030                    oIFSUTILenLast = wLen;
077900161102
078000161102                    // Valorizo parametri di output
078100161102                    oIFSUTIskD256K(oIFSUTIskCnt) =
078200161102                                   %subst(wIFSUTIdata8K:1:oIFSUTILenLast);
078300161102
078400161102                    // Incremento lunghezza totale dati effettivamente letti
078500161102                    oIFSUTIsizeByte = oIFSUTIsizeByte + oIFSUTILenLast;
078600161030
078700161030                    // Valorizzo opcode di metodo
078800161030                    locMethodCode = 0;
078900161030
079000161030            endsl;
079100161030
079200161030        enddo;
079300161030
079400161030
079500161030        // Se passata in input anche la dimensione attesa del file
079600161030        // effettuo verifica di congruenza
079700161030        if iIFSUTIsizeByte > *zeros;
079800161030           if iIFSUTIsizeByte = oIFSUTIsizeByte;
079900161030           else;
080000161030
080100161030              // Per sicurezza inizializzo parametri di output
080200161030              clear oIFSUTIskCnt;
080300161030              clear oIFSUTILenLast;
080400161030              clear oIFSUTIskD256K;
080500161030
080600161030              // Valorizzo opcode di metodo a KO = discepanza dimensione file attesa
080700161030              locMethodCode = -2;
080800161030
080900161030           endif;
081000161030        endif;
081100161030
081200161030
081300161030        // Chiudo il file nel IFS
081400161030        IFS_CloseFile(IFSfileDescr);
081500161030
081600161030
081700161030        On-Error;
081800161030
081900161030           // Esito chiamata già inizializzato a errore e parametri output già puliti
082000161030
082100161030           // Monitorizzo anche quanto eventualmente eseguito nella gestione errore superiore
082200161030           Monitor;
082300161030              // Chiudo il file nel IFS
082400161030              IFS_CloseFile(IFSfileDescr);
082500161030           On-Error;
082600161030           // Fine monitoring
082700161030           EndMon;
082800161030
082900161030        // Fine monitoring
083000161030        EndMon;
083100161030
083200161030
083300161030        return locMethodCode;
083400161030
083500161030      /END-FREE
083600161030
083700161030     P UBIFSUTIR_ReadFileUpTo256K...
083800161030     P                 E
083900161109
084000161109
084100161109
084200161109     P*--------------------------------------------------
084300161109     P* Procedure name: UBIFSUTIR_ReadFileUpTo256K_Base64
084400161109     P* Purpose:        Legge intero file (dimensioni max 256K) e lo converte BASE64
084500161109     P* Returns:        Integer (Status_Code)
084600161109     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
084700161109     P* Parameter:      iIFSUTItypeB64  => Tipo di encoding BASE64: testuale o binary (*DFT)
084800161109     P* Parameter:      oIFSUTIsizeByte => Dimensione file rilevata
084900161109     P* Parameter:      oIFSUTIskCnt    => Numero elementi buffer dati output valorizzati
085000161109     P* Parameter:      oIFSUTIskD512K  => Elementi buffer output (buffer 256*2 per sicurezza)
085100161109     P*--------------------------------------------------
085200161109     P*
085300161109     P UBIFSUTIR_ReadFileUpTo256K_Base64...
085400161109     P                 B                   EXPORT
085500161109     D*
085600161109     D UBIFSUTIR_ReadFileUpTo256K_Base64...
085700161109     D                 PI            10I 0
085800161109     DiIFSUTIfile                  1024A   CONST
085900161109     DiIFSUTItypeB64                  3A   CONST
086000161109     DoIFSUTIsizeByte                10I 0
086100161109     DoIFSUTIskCnt                    5U 0
086200161109     DoIFSUTIskD512K              16384A   VARYING OPTIONS(*VARSIZE)
086300161109     D                                     DIM(32)
086400161109     D*
086500161109      //********************************************************************************************
086600161109      //
086700161109      // Definizione variabili work (locali)
086800161109      //
086900161109      //********************************************************************************************
087000161109     D locMethodCode   S             10I 0
087100161109     D
087200161109     DwIFSUTIfile      S                   INZ LIKE(iIFSUTIfile)
087300161109     DwIFSUTItypeB64   S              3A   INZ
087400161109     D wErrore         S               N   INZ
087500161109     D idx             S              5U 0 INZ
087600161109
087700161109      /FREE
087800161109        // Inizializzo opcode di metodo a errore
087900161109        locMethodCode = -1;
088000161109
088100161109
088200161109        // Monitorizzo l'intero processo
088300161109        Monitor;
088400161109
088500161109        // Inizializzo parametri di output
088600161109        clear oIFSUTIsizeByte;
088700161109        clear oIFSUTIskCnt;
088800161109        clear oIFSUTIskD512K;
088900161109
089000161109        // Verifico parametro input relativo al tipo di encoding Base64
089100161109        wIFSUTItypeB64 = iIFSUTItypeB64;
089200161109        if wIFSUTItypeB64 <> 'BIN' AND wIFSUTItypeB64 <> 'TXT';
089300161109           wIFSUTItypeB64  = 'BIN';
089400161109        endif;
089500161109
089600161109        // Verifico che il path/file inizi sempre con il carattere '/'
089700161109        wIFSUTIfile = iIFSUTIfile;
089800161109        if %subst(wIFSUTIfile:1:1) <> '/';
089900161109           wIFSUTIfile = '/' + %trim(wIFSUTIfile);
090000161109        endif;
090100161109
090200161109        // Leggo il file indicato in input (max 256K)
090300161109        if UBIFSUTIR_ReadFileUpTo256K(iIFSUTIfile
090400161109                                     :*zeros
090500161109                                     :oIFSUTIsizeByte
090600161109                                     :oIFSUTIskCnt
090700161109                                     :oIFSUTILenLast
090800161109                                     :oIFSUTIskD256K) = *zeros;
090900161109
091000161109           // Inizializzo dimensione file "as is" per memorizzare dimensioen file Base64
091100161109           clear oIFSUTIsizeByte;
091200161109
091300161109           // Procedo encodando ogni "segmento" dei dati letti dal file richiesto
091400161109           idx = 1;
091500161109           dow not wErrore AND idx <= oIFSUTIskCnt;
091600161109
091700161109               if idx < oIFSUTIskCnt;
091800161109                  iBASE64src_dta = oIFSUTIskD256K(idx);
091900161109                  iBASE64src_len = %size(oIFSUTIskD256K)-2;
092000161109               else;
092100161109                  iBASE64src_dta = %subst(oIFSUTIskD256K(idx):1:oIFSUTILenLast);
092200161109                  iBASE64src_len = oIFSUTILenLast;
092300161109               endif;
092400161109
092500161109               // Verifico encoding richiesto
092600161109               if  wIFSUTItypeB64 = 'BIN';
092700161109                   // Se richiesto => Encode Binary
092800161109                   // Encodo il segmento appena convertito in Base64
092900161109                   if UBBASE64R_Encode_Binary(oBASE64enc_dta
093000161109                                             :oBASE64enc_len
093100161109                                             :iBASE64src_dta
093200161109                                             :iBASE64src_len) < *zeros;
093300161109                      wErrore = *on;
093400161109                   else;
093500161109                      oIFSUTIskD512K(idx) =
093600161109                                %subst(oBASE64enc_dta:1:oBASE64enc_len);
093700161109                      oIFSUTIsizeByte = oIFSUTIsizeByte + oBASE64enc_len;
093800161109                   endif;
093900161109               else;
094000161109                   // Se richiesto => Encode "Text"
094100161109                   // Encodo il segmento appena convertito in Base64
094200161109                   if UBBASE64R_Encode(oBASE64enc_dta
094300161109                                      :oBASE64enc_len
094400161109                                      :iBASE64src_ptr
094500161109                                      :iBASE64src_len) < *zeros;
094600161109                      wErrore = *on;
094700161109                   else;
094800161109                      oIFSUTIskD512K(idx) =
094900161109                                %subst(oBASE64enc_dta:1:oBASE64enc_len);
095000161109                      oIFSUTIsizeByte = oIFSUTIsizeByte + oBASE64enc_len;
095100161109                   endif;
095200161109               endif;
095300161109
095400161109               idx = idx + 1;
095500161109           enddo;
095600161109
095700161109           // Se al termine tutto OK => esito OK
095800161109           if not wErrore;
095900161109
096000161109              // Valorizzo opcode di metodo
096100161109              locMethodCode = 0;
096200161109
096300161109           else;
096400161109
096500161109              // Inizializzo parametri di output
096600161109              clear oIFSUTIsizeByte;
096700161109              clear oIFSUTIskCnt;
096800161109              clear oIFSUTIskD512K;
096900161109
097000161109           endif;
097100161109
097200161109        endif;
097300161109
097400161109
097500161109        On-Error;
097600161109
097700161109           // Esito chiamata già inizializzato a errore e parametri output già puliti
097800161109
097900161109        // Fine monitoring
098000161109        EndMon;
098100161109
098200161109
098300161109        return locMethodCode;
098400161109
098500161109      /END-FREE
098600161109
098700161109     P UBIFSUTIR_ReadFileUpTo256K_Base64...
098800161109     P                 E
098900151126
