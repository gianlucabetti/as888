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
003400170516     **
003500170516     ** ISTRUZIONI PER LA COMPILAZIONE
003600170516     **
003700170516     ** 1. Creare il modulo UBIFSUTIR (Opz 15 PDM)
003800170516     **    indicando TGTRLS(*CURRENT) e CLOSQLCSR(*ENDACTGRP)
003900170516     **              BNDDIR('UBBNDDIR':'QC2LE')
004000170516     **
004100170516     ** 2. Creare/Aggiornare il programma di servizio UBIFSUTIR (CRTSRVPGM / UPDSRVPGM)
004200170516     **    con ACTGRP(*caller), e BNDDIR(UBBNDDIR)
004300170516     **      CRTSRVPGM SRVPGM(GAITRAOBJ/UBIFSUTIR) EXPORT(*ALL)
004400170516     **                TEXT('IFS - Metodi vari (standard e utente)   ')
004500170516     **                BNDDIR(UBBNDDIR QC2LE) TGTRLS(*CURRENT)
004600170516     **
004700170516     ** 3. Eventualmente si può cancellare il modulo.
004800170516     **
004900170516
005000071128     ***********************************************************************************************
005100080612
005200080612
005300080612      //********************************************************************************************
005400080612      //
005500080612      // Definizione file (globali)
005600080612      //
005700080612      //********************************************************************************************
005800071128
005900080612
006000071128      //********************************************************************************************
006100071128      //
006200080612      // Definizione prototipi procedure (globali)
006300071128      //
006400071128      //********************************************************************************************
006500161017     D/COPY GAITRASRC/SRCPROTOPR,UBIFSUTIR
006600161017     D/COPY GAITRASRC/SRCPROTOPI,UBIFSUTIR
006700161017     D/COPY GAITRASRC/SRCPROTOPR,UBIFS
006800161021     D/COPY GAITRASRC/SRCPROTOPI,UBIFS
006900161018     D/COPY GAITRASRC/SRCPROTOPR,UBBASE64R
007000161018     D/COPY GAITRASRC/SRCPROTOPI,UBBASE64R
007100080612
007200080612
007300071128      //********************************************************************************************
007400071128      //
007500080612      // Definizione variabili work (globali)
007600071128      //
007700071128      //********************************************************************************************
007800151126     D InitDone        S               N   INZ(*off)
007900161017     D
008000071129
008100151126
008200160404
008300161017     P*--------------------------------------------------
008400161017     P* Procedure name: UBIFSUTIR_GetFileNameElements
008500161017     P* Purpose:        Riconosce e separa gli elementi che compongono il percorso e/o nome file
008600160404     P* Returns:        Integer (Status_Code)
008700161017     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
008800161017     P* Parameter:      oIFSUTIpath     => Percorso file
008900161017     P* Parameter:      oIFSUTIfullName => Nome file completo (nome + estensione)
009000161017     P* Parameter:      oIFSUTIName     => Nome file no estensione
009100161017     P* Parameter:      oIFSUTIExt      => Estensione
009200160404     P*--------------------------------------------------
009300160404     P*
009400161017     P UBIFSUTIR_GetFileNameElements...
009500160404     P                 B                   EXPORT
009600160404     D*
009700161017     D UBIFSUTIR_GetFileNameElements...
009800160404     D                 PI            10I 0
009900161017     DiIFSUTIfile                  1024A   CONST
010000161017     DoIFSUTIpath                   512A
010100161017     DoIFSUTIfullName               512A
010200161017     DoIFSUTIName                   512A
010300161017     DoIFSUTIExt                     32A
010400160404     D*
010500160404      //********************************************************************************************
010600160404      //
010700160404      // Definizione variabili work (locali)
010800160404      //
010900160404      //********************************************************************************************
011000160404     D locMethodCode   S             10I 0
011100160404     D
011200161024     DwIFSUTIfile      S                   INZ LIKE(iIFSUTIfile)
011300161017     D wPosPathName    S              4S 0 inz
011400161017     D wPosNameExt     S              4S 0 inz
011500161017     D wPos            S              4S 0 inz
011600160404
011700160404      /FREE
011800161017        // Inizializzo opcode di metodo a errore
011900160404        locMethodCode = -1;
012000161017
012100161017
012200161017        // Monitorizzo l'intero processo
012300161017        Monitor;
012400161017
012500161017        // Inizializzo i parametri di output
012600161017        clear oIFSUTIpath;
012700161017        clear oIFSUTIfullName;
012800161017        clear oIFSUTIName;
012900161017        clear oIFSUTIExt;
013000160404
013100161017        // Prima di tutto "noralizzo" carattere di separazione gerarchica File System
013200161017        wIFSUTIfile = %scanrpl('\':'/':iIFSUTIfile);
013300161019
013400161019        // Verifico che il path/file inizi sempre con il carattere '/'
013500161019        if %subst(wIFSUTIfile:1:1) <> '/';
013600161019           wIFSUTIfile = '/' + %trim(wIFSUTIfile);
013700161019        endif;
013800161017
013900161017        // Reperisco posisionze del ultimo carattere separazione generachica FS
014000161017        wPosPathName = *zeros;
014100161017        wPos = %scan('/':wIFSUTIfile:1);
014200161017        dow wPos > *zeros AND wPos < %len(%trim(wIFSUTIfile));
014300161017            if wPos > *zeros;
014400161017               wPosPathName = wPos;
014500161017            endif;
014600161017            wPos = wPos + 1;
014700161017            wPos = %scan('/':wIFSUTIfile:wPos);
014800161017        enddo;
014900161017
015000161017        // Determino il path e il nome file full (nome + estensione)
015100161017        oIFSUTIpath     = %subst(wIFSUTIfile:1:wPosPathName-1);
015200161017        oIFSUTIfullName = %subst(wIFSUTIfile:wPosPathName+1);
015300161017
015400161017
015500161017        // Reperisco posisionze del ultimo '.' di separazione elementi nome file
015600161017        wPosNameExt = *zeros;
015700161017        wPos = %scan('.':oIFSUTIfullName:1);
015800161017        dow wPos > *zeros AND wPos < %len(%trim(oIFSUTIfullName));
015900161017            if wPos > *zeros;
016000161017               wPosNameExt = wPos;
016100161017            endif;
016200161017            wPos = wPos + 1;
016300161017            wPos = %scan('.':oIFSUTIfullName:wPos);
016400161017        enddo;
016500161017
016600161017        // Determino il path e il nome file no estenzione e l'estensione
016700161017        oIFSUTIName = %subst(oIFSUTIfullName:1:wPosNameExt-1);
016800161017        oIFSUTIExt  = %subst(oIFSUTIfullName:wPosNameExt+1);
016900160404
017000161017
017100161017        On-Error;
017200161017
017300161017           // Esito chiamata già inizializzato a errore e parametri output già puliti
017400161017
017500161017        // Fine monitoring
017600161017        EndMon;
017700161017
017800161017
017900161017        // Valorizzo opcode di metodo
018000161017        locMethodCode = 0;
018100160404
018200160404        return locMethodCode;
018300160404
018400160404      /END-FREE
018500160404
018600161017     P UBIFSUTIR_GetFileNameElements...
018700160404     P                 E
018800161017
018900161017
019000161017
019100161017     P*--------------------------------------------------
019200161019     P* Procedure name: UBIFSUTIR_GetFileSizeByte
019300161019     P* Purpose:        Reperisce la dimensione di un file in Byte
019400161017     P* Returns:        Integer (Status_Code)
019500161017     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
019600161019     P* Parameter:      oIFSUTIsizeByte => Dimensione file in Byte
019700161017     P*--------------------------------------------------
019800161017     P*
019900161019     P UBIFSUTIR_GetFileSizeByte...
020000161017     P                 B                   EXPORT
020100161017     D*
020200161019     D UBIFSUTIR_GetFileSizeByte...
020300161017     D                 PI            10I 0
020400161017     DiIFSUTIfile                  1024A   CONST
020500161019     DoIFSUTIsizeByte                10I 0
020600161017     D*
020700161017      //********************************************************************************************
020800161017      //
020900161017      // Definizione variabili work (locali)
021000161017      //
021100161017      //********************************************************************************************
021200161017     D locMethodCode   S             10I 0
021300161017     D
021400161024     DwIFSUTIfile      S                   INZ LIKE(iIFSUTIfile)
021500161024     DwIFSUTIstatDS    DS                  INZ LIKEDS(IFSstatDS)
021600161017
021700161017      /FREE
021800161017        // Inizializzo opcode di metodo a errore
021900161017        locMethodCode = -1;
022000161017
022100161017
022200161017        // Monitorizzo l'intero processo
022300161017        Monitor;
022400161019
022500161019        // Verifico che il path/file inizi sempre con il carattere '/'
022600161019        wIFSUTIfile = iIFSUTIfile;
022700161019        if %subst(wIFSUTIfile:1:1) <> '/';
022800161019           wIFSUTIfile = '/' + %trim(wIFSUTIfile);
022900161019        endif;
023000161017
023100161017        // Inizializzo i parametri di output
023200161019        clear oIFSUTIsizeByte;
023300161017
023400161017        // Reperisco gli attributi del file ricevuto in input
023500161017        clear wIFSUTIstatDS;
023600161019        IFSrc = IFS_GetFileInfo(%trim(wIFSUTIfile):%addr(wIFSUTIstatDS));
023700161017
023800161017        // Se reperimento attributi file riuscito
023900161017        if IFSrc >= *zeros;
024000161019           // Ritorno la dimensione del file in Byte
024100161019           oIFSUTIsizeByte = wIFSUTIstatDS.st_size;
024200161021
024300161021           // Valorizzo opcode di metodo
024400161021           locMethodCode = 0;
024500161017        endif;
024600161017
024700161017
024800161017        On-Error;
024900161017
025000161017           // Esito chiamata già inizializzato a errore e parametri output già puliti
025100161017
025200161017        // Fine monitoring
025300161017        EndMon;
025400161017
025500161017
025600161017        return locMethodCode;
025700161017
025800161017      /END-FREE
025900161017
026000161019     P UBIFSUTIR_GetFileSizeByte...
026100161017     P                 E
026200161021
026300161021
026400161021
026500161021     P*--------------------------------------------------
026600161021     P* Procedure name: UBIFSUTIR_GetFileEncoding
026700161021     P* Purpose:        Reperisce la CodePage e il CCSID
026800161021     P* Returns:        Integer (Status_Code)
026900161021     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
027000161021     P* Parameter:      oIFSUTIcodePage => CodePage del file
027100161021     P* Parameter:      oIFSUTI_CCSID   => CCSID del file
027200161021     P*--------------------------------------------------
027300161021     P*
027400161021     P UBIFSUTIR_GetFileEncoding...
027500161021     P                 B                   EXPORT
027600161021     D*
027700161021     D UBIFSUTIR_GetFileEncoding...
027800161021     D                 PI            10I 0
027900161021     DiIFSUTIfile                  1024A   CONST
028000161021     DoIFSUTIcodePage                 5U 0
028100161021     DoIFSUTI_CCSID                   5U 0
028200161021     D*
028300161021      //********************************************************************************************
028400161021      //
028500161021      // Definizione variabili work (locali)
028600161021      //
028700161021      //********************************************************************************************
028800161021     D locMethodCode   S             10I 0
028900161021     D
029000161024     DwIFSUTIfile      S                   INZ LIKE(iIFSUTIfile)
029100161024     DwIFSUTIstatDS    DS                  INZ LIKEDS(IFSstatDS)
029200161021
029300161021      /FREE
029400161021        // Inizializzo opcode di metodo a errore
029500161021        locMethodCode = -1;
029600161021
029700161021
029800161021        // Monitorizzo l'intero processo
029900161021        Monitor;
030000161021
030100161021        // Verifico che il path/file inizi sempre con il carattere '/'
030200161021        wIFSUTIfile = iIFSUTIfile;
030300161021        if %subst(wIFSUTIfile:1:1) <> '/';
030400161021           wIFSUTIfile = '/' + %trim(wIFSUTIfile);
030500161021        endif;
030600161021
030700161021        // Inizializzo i parametri di output
030800161021        clear oIFSUTIcodePage;
030900161021        clear oIFSUTI_CCSID;
031000161021
031100161021        // Reperisco gli attributi del file ricevuto in input
031200161021        clear wIFSUTIstatDS;
031300161021        IFSrc = IFS_GetFileInfo(%trim(wIFSUTIfile):%addr(wIFSUTIstatDS));
031400161021
031500161021        // Se reperimento attributi file riuscito
031600161021        if IFSrc >= *zeros;
031700161021           // Ritorno la dimensione del file in Byte
031800161021           oIFSUTIcodePage = wIFSUTIstatDS.st_codepage;
031900161021           oIFSUTI_CCSID   = wIFSUTIstatDS.st_ccsid;
032000161021
032100161021           // Valorizzo opcode di metodo
032200161021           locMethodCode = 0;
032300161021        endif;
032400161021
032500161021
032600161021        On-Error;
032700161021
032800161021           // Esito chiamata già inizializzato a errore e parametri output già puliti
032900161021
033000161021        // Fine monitoring
033100161021        EndMon;
033200161021
033300161021
033400161021        return locMethodCode;
033500161021
033600161021      /END-FREE
033700161021
033800161021     P UBIFSUTIR_GetFileEncoding...
033900161021     P                 E
034000161025
034100161025
034200161025
034300161025     P*--------------------------------------------------
034400161025     P* Procedure name: UBIFSUTIR_ReadFileUpTo16K
034500161026     P* Purpose:        Legge "one-shot" intero file (dimensioni max 16KB)
034600161025     P* Returns:        Integer (Status_Code)
034700161025     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
034800161025     P* Parameter:      iIFSUTIsizeByte => Dimensione file attesa
034900161028     P* Parameter:      oIFSUTIsizeByte => Dimensione file rilevata
035000161025     P* Parameter:      oIFSUTIdata16K  => Contenuto del file
035100161025     P*--------------------------------------------------
035200161025     P*
035300161025     P UBIFSUTIR_ReadFileUpTo16K...
035400161025     P                 B                   EXPORT
035500161025     D*
035600161025     D UBIFSUTIR_ReadFileUpTo16K...
035700161025     D                 PI            10I 0
035800161025     DiIFSUTIfile                  1024A   CONST
035900161025     DiIFSUTIsizeByte                10I 0 CONST
036000161028     DoIFSUTIsizeByte                10I 0
036100161025     DoIFSUTIdata16K              16384A   VARYING OPTIONS(*VARSIZE)
036200161025     D*
036300161025      //********************************************************************************************
036400161025      //
036500161025      // Definizione variabili work (locali)
036600161025      //
036700161025      //********************************************************************************************
036800161025     D locMethodCode   S             10I 0
036900161025     D
037000161025     DwIFSUTIfile      S                   INZ LIKE(iIFSUTIfile)
037100161102     DwIFSUTIdata16K   S          16384A   INZ
037200161025     D wLen            S             10I 0 inz
037300161025
037400161025      /FREE
037500161025        // Inizializzo opcode di metodo a errore
037600161025        locMethodCode = -1;
037700161025
037800161025
037900161025        // Monitorizzo l'intero processo
038000161025        Monitor;
038100161026
038200161026        // Inizializzo parametri di output
038300161026        clear oIFSUTIdata16K;
038400161028        clear oIFSUTIsizeByte;
038500161025
038600161025        // Verifico che il path/file inizi sempre con il carattere '/'
038700161025        wIFSUTIfile = iIFSUTIfile;
038800161025        if %subst(wIFSUTIfile:1:1) <> '/';
038900161025           wIFSUTIfile = '/' + %trim(wIFSUTIfile);
039000161025        endif;
039100161025
039200161025        // Innanzitutto apro il file in sola lettura
039300161025        IFSfileDescr = IFS_OpenFile(%trim(wIFSUTIfile):O_RDONLY);
039400161025
039500161025        // Innanzitutto apro il file in sola lettura
039600161025        if IFSfileDescr >= *zeros;
039700161102           wLen = IFS_ReadFile(IFSfileDescr:%addr(wIFSUTIdata16K):
039800161102                                            %size(wIFSUTIdata16K));
039900161025           if wLen > *zeros;
040000161025
040100161025              // Valorizzo opcode di metodo a OK
040200161025              locMethodCode = 0;
040300161028
040400161028              // Valorizzo buffer di output
040500161028              oIFSUTIsizeByte = wLen;
040600161025
040700161025              // Se passata in input anche la dimensione attesa del file
040800161025              // effettuo verifica di congruenza
040900161025              if iIFSUTIsizeByte > *zeros;
041000161030                 if iIFSUTIsizeByte = oIFSUTIsizeByte;
041100161030
041200161030                    // Valorizo parametri di output
041300161102                    oIFSUTIdata16K = %subst(wIFSUTIdata16K:1:oIFSUTIsizeByte);
041400161030
041500161025                 else;
041600161025
041700161030                    // Per sicurezza inizializzo parametri di output
041800161030                    clear oIFSUTIdata16K;
041900161030
042000161025                    // Valorizzo opcode di metodo a KO = discepanza dimensione file attesa
042100161025                    locMethodCode = -2;
042200161025
042300161025                 endif;
042400161025              endif;
042500161025
042600161025           endif;
042700161025
042800161025           // Chiudo il file nel IFS
042900161025           IFS_CloseFile(IFSfileDescr);
043000161025
043100161025        endif;
043200161025
043300161025
043400161025        On-Error;
043500161025
043600161025           // Esito chiamata già inizializzato a errore e parametri output già puliti
043700161025
043800161025           // Monitorizzo anche quanto eventualmente eseguito nella gestione errore superiore
043900161025           Monitor;
044000161025              // Chiudo il file nel IFS
044100161025              IFS_CloseFile(IFSfileDescr);
044200161025           On-Error;
044300161025           // Fine monitoring
044400161025           EndMon;
044500161025
044600161025        // Fine monitoring
044700161025        EndMon;
044800161025
044900161025
045000161025        return locMethodCode;
045100161025
045200161025      /END-FREE
045300161025
045400161026     P UBIFSUTIR_ReadFileUpTo16K...
045500161025     P                 E
045600161025
045700161025
045800161025
045900161025     P*--------------------------------------------------
046000161025     P* Procedure name: UBIFSUTIR_ReadFileUpTo32K
046100161026     P* Purpose:        Legge "one-shot" intero file (dimensioni max 32KB)
046200161025     P* Returns:        Integer (Status_Code)
046300161025     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
046400161025     P* Parameter:      iIFSUTIsizeByte => Dimensione file attesa
046500161028     P* Parameter:      oIFSUTIsizeByte => Dimensione file rilevata
046600161025     P* Parameter:      oIFSUTIdata32K  => Contenuto del file
046700161025     P*--------------------------------------------------
046800161025     P*
046900161025     P UBIFSUTIR_ReadFileUpTo32K...
047000161025     P                 B                   EXPORT
047100161025     D*
047200161025     D UBIFSUTIR_ReadFileUpTo32K...
047300161025     D                 PI            10I 0
047400161025     DiIFSUTIfile                  1024A   CONST
047500161025     DiIFSUTIsizeByte                10I 0 CONST
047600161028     DoIFSUTIsizeByte                10I 0
047700161025     DoIFSUTIdata32K              32768A   VARYING OPTIONS(*VARSIZE)
047800161025     D*
047900161025      //********************************************************************************************
048000161025      //
048100161025      // Definizione variabili work (locali)
048200161025      //
048300161025      //********************************************************************************************
048400161025     D locMethodCode   S             10I 0
048500161025     D
048600161025     DwIFSUTIfile      S                   INZ LIKE(iIFSUTIfile)
048700161102     DwIFSUTIdata32K   S          32768A   INZ
048800161025     D wLen            S             10I 0 inz
048900161025
049000161025      /FREE
049100161025        // Inizializzo opcode di metodo a errore
049200161025        locMethodCode = -1;
049300161025
049400161025
049500161025        // Monitorizzo l'intero processo
049600161025        Monitor;
049700161026
049800161026        // Inizializzo parametri di output
049900161026        clear oIFSUTIdata32K;
050000161028        clear oIFSUTIsizeByte;
050100161025
050200161025        // Verifico che il path/file inizi sempre con il carattere '/'
050300161025        wIFSUTIfile = iIFSUTIfile;
050400161025        if %subst(wIFSUTIfile:1:1) <> '/';
050500161025           wIFSUTIfile = '/' + %trim(wIFSUTIfile);
050600161025        endif;
050700161025
050800161025        // Innanzitutto apro il file in sola lettura
050900161025        IFSfileDescr = IFS_OpenFile(%trim(wIFSUTIfile):O_RDONLY);
051000161025
051100161025        // Innanzitutto apro il file in sola lettura
051200161025        if IFSfileDescr >= *zeros;
051300161102           wLen = IFS_ReadFile(IFSfileDescr:%addr(wIFSUTIdata32K):
051400161102                                            %size(wIFSUTIdata32K));
051500161025           if wLen > *zeros;
051600161025
051700161025              // Valorizzo opcode di metodo a OK
051800161025              locMethodCode = 0;
051900161028
052000161028              // Valorizzo buffer di output
052100161028              oIFSUTIsizeByte = wLen;
052200161025
052300161025              // Se passata in input anche la dimensione attesa del file
052400161025              // effettuo verifica di congruenza
052500161025              if iIFSUTIsizeByte > *zeros;
052600161030                 if iIFSUTIsizeByte = oIFSUTIsizeByte;
052700161102
052800161102                    // Valorizo parametri di output
052900161102                    oIFSUTIdata32K = %subst(wIFSUTIdata32K:1:oIFSUTIsizeByte);
053000161102
053100161025                 else;
053200161030
053300161030                    // Per sicurezza inizializzo parametri di output
053400161030                    clear oIFSUTIdata32K;
053500161025
053600161025                    // Valorizzo opcode di metodo a KO = discepanza dimensione file attesa
053700161025                    locMethodCode = -2;
053800161025
053900161025                 endif;
054000161025              endif;
054100161025
054200161025           endif;
054300161025
054400161025           // Chiudo il file nel IFS
054500161025           IFS_CloseFile(IFSfileDescr);
054600161025
054700161025        endif;
054800161025
054900161025
055000161025        On-Error;
055100161025
055200161025           // Esito chiamata già inizializzato a errore e parametri output già puliti
055300161025
055400161025           // Monitorizzo anche quanto eventualmente eseguito nella gestione errore superiore
055500161025           Monitor;
055600161025              // Chiudo il file nel IFS
055700161025              IFS_CloseFile(IFSfileDescr);
055800161025           On-Error;
055900161025           // Fine monitoring
056000161025           EndMon;
056100161025
056200161025        // Fine monitoring
056300161025        EndMon;
056400161025
056500161025
056600161025        return locMethodCode;
056700161025
056800161025      /END-FREE
056900161025
057000161025     P UBIFSUTIR_ReadFileUpTo32K...
057100161025     P                 E
057200161018
057300161018
057400161018
057500161018     P*--------------------------------------------------
057600161018     P* Procedure name: UBIFSUTIR_ReadFileUpTo64K
057700161026     P* Purpose:        Legge "one-shot" intero file (dimensioni max 64KB)
057800161018     P* Returns:        Integer (Status_Code)
057900161018     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
058000161018     P* Parameter:      iIFSUTIsizeByte => Dimensione file attesa
058100161028     P* Parameter:      oIFSUTIsizeByte => Dimensione file rilevata
058200161018     P* Parameter:      oIFSUTIdata64K  => Contenuto del file
058300161018     P*--------------------------------------------------
058400161018     P*
058500161018     P UBIFSUTIR_ReadFileUpTo64K...
058600161018     P                 B                   EXPORT
058700161018     D*
058800161018     D UBIFSUTIR_ReadFileUpTo64K...
058900161018     D                 PI            10I 0
059000161018     DiIFSUTIfile                  1024A   CONST
059100161018     DiIFSUTIsizeByte                10I 0 CONST
059200161028     DoIFSUTIsizeByte                10I 0
059300161030     DoIFSUTIdata64K                   A   LEN(65536) VARYING OPTIONS(*VARSIZE)       (4 Byte x Len)
059400161018     D*
059500161018      //********************************************************************************************
059600161018      //
059700161018      // Definizione variabili work (locali)
059800161018      //
059900161018      //********************************************************************************************
060000161018     D locMethodCode   S             10I 0
060100161018     D
060200161024     DwIFSUTIfile      S                   INZ LIKE(iIFSUTIfile)
060300161102     DwIFSUTIdata64K   S          65536A   INZ
060400161018     D wLen            S             10I 0 inz
060500161018
060600161018      /FREE
060700161018        // Inizializzo opcode di metodo a errore
060800161018        locMethodCode = -1;
060900161018
061000161018
061100161018        // Monitorizzo l'intero processo
061200161018        Monitor;
061300161026
061400161026        // Inizializzo parametri di output
061500161026        clear oIFSUTIdata64K;
061600161028        clear oIFSUTIsizeByte;
061700161019
061800161019        // Verifico che il path/file inizi sempre con il carattere '/'
061900161019        wIFSUTIfile = iIFSUTIfile;
062000161019        if %subst(wIFSUTIfile:1:1) <> '/';
062100161019           wIFSUTIfile = '/' + %trim(wIFSUTIfile);
062200161019        endif;
062300161018
062400161018        // Innanzitutto apro il file in sola lettura
062500161019        IFSfileDescr = IFS_OpenFile(%trim(wIFSUTIfile):O_RDONLY);
062600161018
062700161018        // Innanzitutto apro il file in sola lettura
062800161018        if IFSfileDescr >= *zeros;
062900161102           wLen = IFS_ReadFile(IFSfileDescr:%addr(wIFSUTIdata64K):
063000161102                                            %size(wIFSUTIdata64K));
063100161018           if wLen > *zeros;
063200161018
063300161018              // Valorizzo opcode di metodo a OK
063400161018              locMethodCode = 0;
063500161028
063600161028              // Valorizzo buffer di output
063700161028              oIFSUTIsizeByte = wLen;
063800161018
063900161018              // Se passata in input anche la dimensione attesa del file
064000161018              // effettuo verifica di congruenza
064100161018              if iIFSUTIsizeByte > *zeros;
064200161030                 if iIFSUTIsizeByte = oIFSUTIsizeByte;
064300161102
064400161102                    // Valorizo parametri di output
064500161102                    oIFSUTIdata64K = %subst(wIFSUTIdata64K:1:oIFSUTIsizeByte);
064600161102
064700161019                 else;
064800161030
064900161030                    // Per sicurezza inizializzo parametri di output
065000161030                    clear oIFSUTIdata64K;
065100161018
065200161018                    // Valorizzo opcode di metodo a KO = discepanza dimensione file attesa
065300161018                    locMethodCode = -2;
065400161018
065500161018                 endif;
065600161018              endif;
065700161018
065800161018           endif;
065900161018
066000161018           // Chiudo il file nel IFS
066100161018           IFS_CloseFile(IFSfileDescr);
066200161018
066300161018        endif;
066400161018
066500161018
066600161018        On-Error;
066700161018
066800161018           // Esito chiamata già inizializzato a errore e parametri output già puliti
066900161018
067000161018           // Monitorizzo anche quanto eventualmente eseguito nella gestione errore superiore
067100161018           Monitor;
067200161018              // Chiudo il file nel IFS
067300161018              IFS_CloseFile(IFSfileDescr);
067400161018           On-Error;
067500161018           // Fine monitoring
067600161018           EndMon;
067700161018
067800161018        // Fine monitoring
067900161018        EndMon;
068000161018
068100161018
068200161018        return locMethodCode;
068300161018
068400161018      /END-FREE
068500161018
068600161018     P UBIFSUTIR_ReadFileUpTo64K...
068700161018     P                 E
068800170516
068900170516
069000170516
069100170516     P*--------------------------------------------------
069200170516     P* Procedure name: UBIFSUTIR_ReadFileUpTo4MB
069300170516     P* Purpose:        Legge "one-shot" intero file (dimensioni max 4MB)
069400170516     P* Returns:        Integer (Status_Code)
069500170516     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
069600170516     P* Parameter:      iIFSUTIsizeByte => Dimensione file attesa
069700170516     P* Parameter:      iIFSUTIoFlg     => Open Flags
069800170516     P* Parameter:      iIFSUTImode     => Open mode
069900170516     P* Parameter:      iIFSUTIcodePage => Open CodePage
070000170516     P* Parameter:      oIFSUTIsizeByte => Dimensione file rilevata
070100170516     P* Parameter:      oIFSUTIdata4MB  => Contenuto del file
070200170516     P*--------------------------------------------------
070300170516     P*
070400170516     P UBIFSUTIR_ReadFileUpTo4MB...
070500170516     P                 B                   EXPORT
070600170516     D*
070700170516     D UBIFSUTIR_ReadFileUpTo4MB...
070800170516     D                 PI            10I 0
070900170516     DiIFSUTIfile                  1024A   CONST
071000170516     DiIFSUTIsizeByte                10I 0 CONST
071100170516     DiIFSUTIoFlg                    10I 0 CONST
071200170516     DiIFSUTImode                    10U 0 CONST
071300170516     DiIFSUTIcodePage                10U 0 CONST
071400170516     DoIFSUTIsizeByte                10I 0
071500170516     DoIFSUTIdata4MB                   A   LEN(4194304) VARYING                      (4 Byte x Len)
071600170516     D                                     OPTIONS(*VARSIZE)
071700170516     D*
071800170516      //********************************************************************************************
071900170516      //
072000170516      // Definizione variabili work (locali)
072100170516      //
072200170516      //********************************************************************************************
072300170516     D locMethodCode   S             10I 0
072400170516     D
072500170516     DwIFSUTIfile      S                   INZ LIKE(iIFSUTIfile)
072600170516     DwIFSUTIdata4MB   S               A   INZ LEN(4194304)
072700170516     D wLen            S             10I 0 inz
072800170516
072900170516      /FREE
073000170516        // Inizializzo opcode di metodo a errore
073100170516        locMethodCode = -1;
073200170516
073300170516
073400170516        // Monitorizzo l'intero processo
073500170516        Monitor;
073600170516
073700170516        // Inizializzo parametri di output
073800170516        clear oIFSUTIdata4MB;
073900170516        clear oIFSUTIsizeByte;
074000170516
074100170516        // Verifico che il path/file inizi sempre con il carattere '/'
074200170516        wIFSUTIfile = iIFSUTIfile;
074300170516        if %subst(wIFSUTIfile:1:1) <> '/';
074400170516           wIFSUTIfile = '/' + %trim(wIFSUTIfile);
074500170516        endif;
074600170516
074700170516        // Innanzitutto apro il file in sola lettura
074800170516        IFSfileDescr = IFS_OpenFile(%trim(wIFSUTIfile):iIFSUTIoFlg:
074900170516                                    iIFSUTImode:iIFSUTIcodePage);
075000170516
075100170516        // Innanzitutto apro il file in sola lettura
075200170516        if IFSfileDescr >= *zeros;
075300170516           wLen = IFS_ReadFile(IFSfileDescr:%addr(wIFSUTIdata4MB):
075400170516                                            %size(wIFSUTIdata4MB));
075500170516           if wLen > *zeros;
075600170516
075700170516              // Valorizzo opcode di metodo a OK
075800170516              locMethodCode = 0;
075900170516
076000170516              // Valorizzo buffer di output
076100170516              oIFSUTIsizeByte = wLen;
076200170516
076300170516              // Se passata in input anche la dimensione attesa del file
076400170516              // effettuo verifica di congruenza
076500170516              if iIFSUTIsizeByte > *zeros;
076600170516                 if iIFSUTIsizeByte = oIFSUTIsizeByte;
076700170516
076800170516                    // Valorizo parametri di output
076900170516                    oIFSUTIdata4MB = %subst(wIFSUTIdata4MB:1:oIFSUTIsizeByte);
077000170516
077100170516                 else;
077200170516
077300170516                    // Per sicurezza inizializzo parametri di output
077400170516                    clear oIFSUTIdata4MB;
077500170516
077600170516                    // Valorizzo opcode di metodo a KO = discepanza dimensione file attesa
077700170516                    locMethodCode = -2;
077800170516
077900170516                 endif;
078000170516              endif;
078100170516
078200170516           endif;
078300170516
078400170516           // Chiudo il file nel IFS
078500170516           IFS_CloseFile(IFSfileDescr);
078600170516
078700170516        endif;
078800170516
078900170516
079000170516        On-Error;
079100170516
079200170516           // Esito chiamata già inizializzato a errore e parametri output già puliti
079300170516
079400170516           // Monitorizzo anche quanto eventualmente eseguito nella gestione errore superiore
079500170516           Monitor;
079600170516              // Chiudo il file nel IFS
079700170516              IFS_CloseFile(IFSfileDescr);
079800170516           On-Error;
079900170516           // Fine monitoring
080000170516           EndMon;
080100170516
080200170516        // Fine monitoring
080300170516        EndMon;
080400170516
080500170516
080600170516        return locMethodCode;
080700170516
080800170516      /END-FREE
080900170516
081000170516     P UBIFSUTIR_ReadFileUpTo4MB...
081100170516     P                 E
081200170516
081300170516
081400170516
081500170516     P*--------------------------------------------------
081600170516     P* Procedure name: UBIFSUTIR_ReadFileUpTo4MBtext
081700170516     P* Purpose:        Legge "one-shot" intero file (dimensioni max 4MB) in modalità testuale
081800170516     P* Returns:        Integer (Status_Code)
081900170516     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
082000170516     P* Parameter:      iIFSUTIsizeByte => Dimensione file attesa
082100170516     P* Parameter:      oIFSUTIsizeByte => Dimensione file rilevata
082200170516     P* Parameter:      oIFSUTIdata4MB  => Contenuto del file
082300170516     P*--------------------------------------------------
082400170516     P*
082500170516     P UBIFSUTIR_ReadFileUpTo4MBtext...
082600170516     P                 B                   EXPORT
082700170516     D*
082800170516     D UBIFSUTIR_ReadFileUpTo4MBtext...
082900170516     D                 PI            10I 0
083000170516     DiIFSUTIfile                  1024A   CONST
083100170516     DiIFSUTIsizeByte                10I 0 CONST
083200170516     DoIFSUTIsizeByte                10I 0
083300170516     DoIFSUTIdata4MB                   A   LEN(4194304) VARYING                      (4 Byte x Len)
083400170516     D                                     OPTIONS(*VARSIZE)
083500170516     D*
083600170516      //********************************************************************************************
083700170516      //
083800170516      // Definizione variabili work (locali)
083900170516      //
084000170516      //********************************************************************************************
084100170516     D locMethodCode   S             10I 0
084200170516     D
084300170516     DwIFSUTIfile      S                   INZ LIKE(iIFSUTIfile)
084400170516     DwIFSUTIdata4MB   S               A   INZ LEN(4194304)
084500170516     DwIFSUTIoFlg      S             10I 0 INZ
084600170516     DwIFSUTImode      S             10U 0 INZ
084700170516     DwIFSUTIcodePage  S             10U 0 INZ
084800170516     D wLen            S             10I 0 inz
084900170516
085000170516      /FREE
085100170516        // Inizializzo opcode di metodo a errore
085200170516        locMethodCode = -1;
085300170516
085400170516
085500170516        // Monitorizzo l'intero processo
085600170516        Monitor;
085700170516
085800170516        // Inizializzo parametri di output
085900170516        clear oIFSUTIdata4MB;
086000170516        clear oIFSUTIsizeByte;
086100170516
086200170516        // Verifico che il path/file inizi sempre con il carattere '/'
086300170516        wIFSUTIfile = iIFSUTIfile;
086400170516        if %subst(wIFSUTIfile:1:1) <> '/';
086500170516           wIFSUTIfile = '/' + %trim(wIFSUTIfile);
086600170516        endif;
086700170516
086800170516        // Innanzitutto apro il file in sola lettura
086900170516        //IFSfileDescr = IFS_OpenFile(%trim(wIFSUTIfile):O_RDONLY);
087000170516        wIFSUTIoFlg     = O_RDONLY + O_TEXTDATA + O_CCSID;
087100170516        wIFSUTImode     = S_IRGRP;
087200170516        wIFSUTIcodePage = 1144;
087300170516
087400170516        IFSfileDescr = IFS_OpenFile(%trim(wIFSUTIfile):wIFSUTIoFlg:
087500170516                                    wIFSUTImode:wIFSUTIcodePage);
087600170516
087700170516        // Innanzitutto apro il file in sola lettura
087800170516        if IFSfileDescr >= *zeros;
087900170516           wLen = IFS_ReadFile(IFSfileDescr:%addr(wIFSUTIdata4MB):
088000170516                                            %size(wIFSUTIdata4MB));
088100170516           if wLen > *zeros;
088200170516
088300170516              // Valorizzo opcode di metodo a OK
088400170516              locMethodCode = 0;
088500170516
088600170516              // Valorizzo buffer di output
088700170516              oIFSUTIsizeByte = wLen;
088800170516
088900170516              // Se passata in input anche la dimensione attesa del file
089000170516              // effettuo verifica di congruenza
089100170516              if iIFSUTIsizeByte > *zeros;
089200170516                 if iIFSUTIsizeByte = oIFSUTIsizeByte;
089300170516
089400170516                    // Valorizo parametri di output
089500170516                    oIFSUTIdata4MB = %subst(wIFSUTIdata4MB:1:oIFSUTIsizeByte);
089600170516
089700170516                 else;
089800170516
089900170516                    // Per sicurezza inizializzo parametri di output
090000170516                    clear oIFSUTIdata4MB;
090100170516
090200170516                    // Valorizzo opcode di metodo a KO = discepanza dimensione file attesa
090300170516                    locMethodCode = -2;
090400170516
090500170516                 endif;
090600170516              endif;
090700170516
090800170516           endif;
090900170516
091000170516           // Chiudo il file nel IFS
091100170516           IFS_CloseFile(IFSfileDescr);
091200170516
091300170516        endif;
091400170516
091500170516
091600170516        On-Error;
091700170516
091800170516           // Esito chiamata già inizializzato a errore e parametri output già puliti
091900170516
092000170516           // Monitorizzo anche quanto eventualmente eseguito nella gestione errore superiore
092100170516           Monitor;
092200170516              // Chiudo il file nel IFS
092300170516              IFS_CloseFile(IFSfileDescr);
092400170516           On-Error;
092500170516           // Fine monitoring
092600170516           EndMon;
092700170516
092800170516        // Fine monitoring
092900170516        EndMon;
093000170516
093100170516
093200170516        return locMethodCode;
093300170516
093400170516      /END-FREE
093500170516
093600170516     P UBIFSUTIR_ReadFileUpTo4MBtext...
093700170516     P                 E
093800161030
093900161030
094000161030
094100170516     P*--------------------------------------------------
094200170516     P* Procedure name: UBIFSUTIR_ReadFileUpTo4MBbin
094300170516     P* Purpose:        Legge "one-shot" intero file (dimensioni max 4MB) in modalità binaria
094400170516     P* Returns:        Integer (Status_Code)
094500170516     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
094600170516     P* Parameter:      iIFSUTIsizeByte => Dimensione file attesa
094700170516     P* Parameter:      oIFSUTIsizeByte => Dimensione file rilevata
094800170516     P* Parameter:      oIFSUTIdata4MB  => Contenuto del file
094900170516     P*--------------------------------------------------
095000170516     P*
095100170516     P UBIFSUTIR_ReadFileUpTo4MBbin...
095200170516     P                 B                   EXPORT
095300170516     D*
095400170516     D UBIFSUTIR_ReadFileUpTo4MBbin...
095500170516     D                 PI            10I 0
095600170516     DiIFSUTIfile                  1024A   CONST
095700170516     DiIFSUTIsizeByte                10I 0 CONST
095800170516     DoIFSUTIsizeByte                10I 0
095900170516     DoIFSUTIdata4MB                   A   LEN(4194304) VARYING                      (4 Byte x Len)
096000170516     D                                     OPTIONS(*VARSIZE)
096100170516     D*
096200170516      //********************************************************************************************
096300170516      //
096400170516      // Definizione variabili work (locali)
096500170516      //
096600170516      //********************************************************************************************
096700170516     D locMethodCode   S             10I 0
096800170516     D
096900170516     DwIFSUTIfile      S                   INZ LIKE(iIFSUTIfile)
097000170516     DwIFSUTIdata4MB   S               A   INZ LEN(4194304)
097100170516     D wLen            S             10I 0 inz
097200170516
097300170516      /FREE
097400170516        // Inizializzo opcode di metodo a errore
097500170516        locMethodCode = -1;
097600170516
097700170516
097800170516        // Monitorizzo l'intero processo
097900170516        Monitor;
098000170516
098100170516        // Inizializzo parametri di output
098200170516        clear oIFSUTIdata4MB;
098300170516        clear oIFSUTIsizeByte;
098400170516
098500170516        // Verifico che il path/file inizi sempre con il carattere '/'
098600170516        wIFSUTIfile = iIFSUTIfile;
098700170516        if %subst(wIFSUTIfile:1:1) <> '/';
098800170516           wIFSUTIfile = '/' + %trim(wIFSUTIfile);
098900170516        endif;
099000170516
099100170516        // Innanzitutto apro il file in sola lettura
099200170516        IFSfileDescr = IFS_OpenFile(%trim(wIFSUTIfile):O_RDONLY);
099300170516
099400170516        // Innanzitutto apro il file in sola lettura
099500170516        if IFSfileDescr >= *zeros;
099600170516           wLen = IFS_ReadFile(IFSfileDescr:%addr(wIFSUTIdata4MB):
099700170516                                            %size(wIFSUTIdata4MB));
099800170516           if wLen > *zeros;
099900170516
100000170516              // Valorizzo opcode di metodo a OK
100100170516              locMethodCode = 0;
100200170516
100300170516              // Valorizzo buffer di output
100400170516              oIFSUTIsizeByte = wLen;
100500170516
100600170516              // Se passata in input anche la dimensione attesa del file
100700170516              // effettuo verifica di congruenza
100800170516              if iIFSUTIsizeByte > *zeros;
100900170516                 if iIFSUTIsizeByte = oIFSUTIsizeByte;
101000170516
101100170516                    // Valorizo parametri di output
101200170516                    oIFSUTIdata4MB = %subst(wIFSUTIdata4MB:1:oIFSUTIsizeByte);
101300170516
101400170516                 else;
101500170516
101600170516                    // Per sicurezza inizializzo parametri di output
101700170516                    clear oIFSUTIdata4MB;
101800170516
101900170516                    // Valorizzo opcode di metodo a KO = discepanza dimensione file attesa
102000170516                    locMethodCode = -2;
102100170516
102200170516                 endif;
102300170516              endif;
102400170516
102500170516           endif;
102600170516
102700170516           // Chiudo il file nel IFS
102800170516           IFS_CloseFile(IFSfileDescr);
102900170516
103000170516        endif;
103100170516
103200170516
103300170516        On-Error;
103400170516
103500170516           // Esito chiamata già inizializzato a errore e parametri output già puliti
103600170516
103700170516           // Monitorizzo anche quanto eventualmente eseguito nella gestione errore superiore
103800170516           Monitor;
103900170516              // Chiudo il file nel IFS
104000170516              IFS_CloseFile(IFSfileDescr);
104100170516           On-Error;
104200170516           // Fine monitoring
104300170516           EndMon;
104400170516
104500170516        // Fine monitoring
104600170516        EndMon;
104700170516
104800170516
104900170516        return locMethodCode;
105000170516
105100170516      /END-FREE
105200170516
105300170516     P UBIFSUTIR_ReadFileUpTo4MBbin...
105400170516     P                 E
105500170516
105600170516
105700170516
105800161030     P*--------------------------------------------------
105900161030     P* Procedure name: UBIFSUTIR_ReadFileUpTo256K
106000161109     P* Purpose:        Legge intero file (dimensioni max 256K)
106100161030     P* Returns:        Integer (Status_Code)
106200161030     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
106300161030     P* Parameter:      iIFSUTIsizeByte => Dimensione file attesa
106400161030     P* Parameter:      oIFSUTIsizeByte => Dimensione file rilevata
106500161030     P* Parameter:      oIFSUTIskCnt    => Numero elementi buffer dati output valorizzati
106600161030     P* Parameter:      oIFSUTILenLast  => Lunghezze ultimo elemento parziale
106700161030     P* Parameter:      oIFSUTIskD256K  => Elementi buffer output
106800161030     P*--------------------------------------------------
106900161030     P*
107000161030     P UBIFSUTIR_ReadFileUpTo256K...
107100161030     P                 B                   EXPORT
107200161030     D*
107300161030     D UBIFSUTIR_ReadFileUpTo256K...
107400161030     D                 PI            10I 0
107500161030     DiIFSUTIfile                  1024A   CONST
107600161030     DiIFSUTIsizeByte                10I 0 CONST
107700161030     DoIFSUTIsizeByte                10I 0
107800161030     DoIFSUTIskCnt                    5U 0
107900161030     DoIFSUTILenLast                 10I 0
108000161102     DoIFSUTIskD256K               8192A   VARYING OPTIONS(*VARSIZE)
108100161030     D                                     DIM(32)
108200161030     D*
108300161030      //********************************************************************************************
108400161030      //
108500161030      // Definizione variabili work (locali)
108600161030      //
108700161030      //********************************************************************************************
108800161030     D locMethodCode   S             10I 0
108900161030     D
109000161030     DwIFSUTIfile      S                   INZ LIKE(iIFSUTIfile)
109100161102     DwIFSUTIdata8K    S           8192A   INZ
109200161030     D wLen            S             10I 0 inz
109300161030     D wCicla          S               N   INZ
109400161030
109500161030      /FREE
109600161030        // Inizializzo opcode di metodo a errore
109700161030        locMethodCode = -1;
109800161030
109900161030
110000161030        // Monitorizzo l'intero processo
110100161030        Monitor;
110200161030
110300161030        // Inizializzo parametri di output
110400161030        clear oIFSUTIsizeByte;
110500161030        clear oIFSUTIskCnt;
110600161030        clear oIFSUTILenLast;
110700161030        clear oIFSUTIskD256K;
110800161030
110900161030        // Verifico che il path/file inizi sempre con il carattere '/'
111000161030        wIFSUTIfile = iIFSUTIfile;
111100161030        if %subst(wIFSUTIfile:1:1) <> '/';
111200161030           wIFSUTIfile = '/' + %trim(wIFSUTIfile);
111300161030        endif;
111400161030
111500161030        // Innanzitutto apro il file in sola lettura
111600161030        IFSfileDescr = IFS_OpenFile(%trim(wIFSUTIfile):O_RDONLY);
111700161030
111800161030
111900161030        // Ciclo lettura file (max 256K)
112000161030        wCicla = *on;
112100161030        dow wCicla;
112200161030
112300161030            // Incremento contatore elementi dati letti
112400161030            oIFSUTIskCnt = oIFSUTIskCnt + 1;
112500161030
112600161030            // Leggo "a blocchi" per dimensione ogni singolo elementi dati previsto
112700161030            wLen = IFS_ReadFile(IFSfileDescr
112800161102                               :%addr(wIFSUTIdata8K)
112900161102                               :%size(wIFSUTIdata8K));
113000161030
113100161030            select;
113200161030               // Errore
113300161030               when wLen < *zeros;
113400161030                    // Valorizzo procedura ad errore
113500161030                    locMethodCode = -1;
113600161030
113700161030                    // Uscita ciclo lettura
113800161030                    wCicla = *off;
113900161030
114000161030               // No dati letti
114100161030               when wLen = *zeros;
114200161030                    // Svuoto (per sicurezza) elemento corrente
114300161030                    clear oIFSUTIskD256K(oIFSUTIskCnt);
114400161030
114500161030                    // Decremento contatore elementi dati letti
114600161030                    oIFSUTIskCnt = oIFSUTIskCnt - 1;
114700161030
114800161030                    // Uscita ciclo lettura
114900161030                    wCicla = *off;
115000161030
115100161030               // Lettura OK
115200161030               when wLen > *zeros;
115300161030                    // Memorizzo quantità dati effettivamente letti
115400161030                    oIFSUTILenLast = wLen;
115500161102
115600161102                    // Valorizo parametri di output
115700161102                    oIFSUTIskD256K(oIFSUTIskCnt) =
115800161102                                   %subst(wIFSUTIdata8K:1:oIFSUTILenLast);
115900161102
116000161102                    // Incremento lunghezza totale dati effettivamente letti
116100161102                    oIFSUTIsizeByte = oIFSUTIsizeByte + oIFSUTILenLast;
116200161030
116300161030                    // Valorizzo opcode di metodo
116400161030                    locMethodCode = 0;
116500161030
116600161030            endsl;
116700161030
116800161030        enddo;
116900161030
117000161030
117100161030        // Se passata in input anche la dimensione attesa del file
117200161030        // effettuo verifica di congruenza
117300161030        if iIFSUTIsizeByte > *zeros;
117400161030           if iIFSUTIsizeByte = oIFSUTIsizeByte;
117500161030           else;
117600161030
117700161030              // Per sicurezza inizializzo parametri di output
117800161030              clear oIFSUTIskCnt;
117900161030              clear oIFSUTILenLast;
118000161030              clear oIFSUTIskD256K;
118100161030
118200161030              // Valorizzo opcode di metodo a KO = discepanza dimensione file attesa
118300161030              locMethodCode = -2;
118400161030
118500161030           endif;
118600161030        endif;
118700161030
118800161030
118900161030        // Chiudo il file nel IFS
119000161030        IFS_CloseFile(IFSfileDescr);
119100161030
119200161030
119300161030        On-Error;
119400161030
119500161030           // Esito chiamata già inizializzato a errore e parametri output già puliti
119600161030
119700161030           // Monitorizzo anche quanto eventualmente eseguito nella gestione errore superiore
119800161030           Monitor;
119900161030              // Chiudo il file nel IFS
120000161030              IFS_CloseFile(IFSfileDescr);
120100161030           On-Error;
120200161030           // Fine monitoring
120300161030           EndMon;
120400161030
120500161030        // Fine monitoring
120600161030        EndMon;
120700161030
120800161030
120900161030        return locMethodCode;
121000161030
121100161030      /END-FREE
121200161030
121300161030     P UBIFSUTIR_ReadFileUpTo256K...
121400161030     P                 E
121500161109
121600161109
121700161109
121800161109     P*--------------------------------------------------
121900161109     P* Procedure name: UBIFSUTIR_ReadFileUpTo256K_Base64
122000161109     P* Purpose:        Legge intero file (dimensioni max 256K) e lo converte BASE64
122100161109     P* Returns:        Integer (Status_Code)
122200161109     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
122300161109     P* Parameter:      iIFSUTItypeB64  => Tipo di encoding BASE64: testuale o binary (*DFT)
122400161109     P* Parameter:      oIFSUTIsizeByte => Dimensione file rilevata
122500161109     P* Parameter:      oIFSUTIskCnt    => Numero elementi buffer dati output valorizzati
122600161109     P* Parameter:      oIFSUTIskD512K  => Elementi buffer output (buffer 256*2 per sicurezza)
122700161109     P*--------------------------------------------------
122800161109     P*
122900161109     P UBIFSUTIR_ReadFileUpTo256K_Base64...
123000161109     P                 B                   EXPORT
123100161109     D*
123200161109     D UBIFSUTIR_ReadFileUpTo256K_Base64...
123300161109     D                 PI            10I 0
123400161109     DiIFSUTIfile                  1024A   CONST
123500161109     DiIFSUTItypeB64                  3A   CONST
123600161109     DoIFSUTIsizeByte                10I 0
123700161109     DoIFSUTIskCnt                    5U 0
123800161109     DoIFSUTIskD512K              16384A   VARYING OPTIONS(*VARSIZE)
123900161109     D                                     DIM(32)
124000161109     D*
124100161109      //********************************************************************************************
124200161109      //
124300161109      // Definizione variabili work (locali)
124400161109      //
124500161109      //********************************************************************************************
124600161109     D locMethodCode   S             10I 0
124700161109     D
124800161109     DwIFSUTIfile      S                   INZ LIKE(iIFSUTIfile)
124900161109     DwIFSUTItypeB64   S              3A   INZ
125000161109     D wErrore         S               N   INZ
125100161109     D idx             S              5U 0 INZ
125200161109
125300161109      /FREE
125400161109        // Inizializzo opcode di metodo a errore
125500161109        locMethodCode = -1;
125600161109
125700161109
125800161109        // Monitorizzo l'intero processo
125900161109        Monitor;
126000161109
126100161109        // Inizializzo parametri di output
126200161109        clear oIFSUTIsizeByte;
126300161109        clear oIFSUTIskCnt;
126400161109        clear oIFSUTIskD512K;
126500161109
126600161109        // Verifico parametro input relativo al tipo di encoding Base64
126700161109        wIFSUTItypeB64 = iIFSUTItypeB64;
126800161109        if wIFSUTItypeB64 <> 'BIN' AND wIFSUTItypeB64 <> 'TXT';
126900161109           wIFSUTItypeB64  = 'BIN';
127000161109        endif;
127100161109
127200161109        // Verifico che il path/file inizi sempre con il carattere '/'
127300161109        wIFSUTIfile = iIFSUTIfile;
127400161109        if %subst(wIFSUTIfile:1:1) <> '/';
127500161109           wIFSUTIfile = '/' + %trim(wIFSUTIfile);
127600161109        endif;
127700161109
127800161109        // Leggo il file indicato in input (max 256K)
127900161109        if UBIFSUTIR_ReadFileUpTo256K(iIFSUTIfile
128000161109                                     :*zeros
128100161109                                     :oIFSUTIsizeByte
128200161109                                     :oIFSUTIskCnt
128300161109                                     :oIFSUTILenLast
128400161109                                     :oIFSUTIskD256K) = *zeros;
128500161109
128600161109           // Inizializzo dimensione file "as is" per memorizzare dimensioen file Base64
128700161109           clear oIFSUTIsizeByte;
128800161109
128900161109           // Procedo encodando ogni "segmento" dei dati letti dal file richiesto
129000161109           idx = 1;
129100161109           dow not wErrore AND idx <= oIFSUTIskCnt;
129200161109
129300161109               if idx < oIFSUTIskCnt;
129400161109                  iBASE64src_dta = oIFSUTIskD256K(idx);
129500161109                  iBASE64src_len = %size(oIFSUTIskD256K)-2;
129600161109               else;
129700161109                  iBASE64src_dta = %subst(oIFSUTIskD256K(idx):1:oIFSUTILenLast);
129800161109                  iBASE64src_len = oIFSUTILenLast;
129900161109               endif;
130000161109
130100161109               // Verifico encoding richiesto
130200161109               if  wIFSUTItypeB64 = 'BIN';
130300161109                   // Se richiesto => Encode Binary
130400161109                   // Encodo il segmento appena convertito in Base64
130500161109                   if UBBASE64R_Encode_Binary(oBASE64enc_dta
130600161109                                             :oBASE64enc_len
130700161109                                             :iBASE64src_dta
130800161109                                             :iBASE64src_len) < *zeros;
130900161109                      wErrore = *on;
131000161109                   else;
131100161109                      oIFSUTIskD512K(idx) =
131200161109                                %subst(oBASE64enc_dta:1:oBASE64enc_len);
131300161109                      oIFSUTIsizeByte = oIFSUTIsizeByte + oBASE64enc_len;
131400161109                   endif;
131500161109               else;
131600161109                   // Se richiesto => Encode "Text"
131700161109                   // Encodo il segmento appena convertito in Base64
131800161109                   if UBBASE64R_Encode(oBASE64enc_dta
131900161109                                      :oBASE64enc_len
132000161109                                      :iBASE64src_ptr
132100161109                                      :iBASE64src_len) < *zeros;
132200161109                      wErrore = *on;
132300161109                   else;
132400161109                      oIFSUTIskD512K(idx) =
132500161109                                %subst(oBASE64enc_dta:1:oBASE64enc_len);
132600161109                      oIFSUTIsizeByte = oIFSUTIsizeByte + oBASE64enc_len;
132700161109                   endif;
132800161109               endif;
132900161109
133000161109               idx = idx + 1;
133100161109           enddo;
133200161109
133300161109           // Se al termine tutto OK => esito OK
133400161109           if not wErrore;
133500161109
133600161109              // Valorizzo opcode di metodo
133700161109              locMethodCode = 0;
133800161109
133900161109           else;
134000161109
134100161109              // Inizializzo parametri di output
134200161109              clear oIFSUTIsizeByte;
134300161109              clear oIFSUTIskCnt;
134400161109              clear oIFSUTIskD512K;
134500161109
134600161109           endif;
134700161109
134800161109        endif;
134900161109
135000161109
135100161109        On-Error;
135200161109
135300161109           // Esito chiamata già inizializzato a errore e parametri output già puliti
135400161109
135500161109        // Fine monitoring
135600161109        EndMon;
135700161109
135800161109
135900161109        return locMethodCode;
136000161109
136100161109      /END-FREE
136200161109
136300161109     P UBIFSUTIR_ReadFileUpTo256K_Base64...
136400161109     P                 E
136500151126
