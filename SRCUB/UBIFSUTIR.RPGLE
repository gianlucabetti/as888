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
004400170522     **                EXPORT(*SRCFILE) SRCFILE(GAITRASRC/SRCSRVPGM)
004500170516     **                TEXT('IFS - Metodi vari (standard e utente)   ')
004600170516     **                BNDDIR(UBBNDDIR QC2LE) TGTRLS(*CURRENT)
004700170516     **
004800170516     ** 3. Eventualmente si può cancellare il modulo.
004900170516     **
005000170516
005100071128     ***********************************************************************************************
005200080612
005300080612
005400080612      //********************************************************************************************
005500080612      //
005600080612      // Definizione file (globali)
005700080612      //
005800080612      //********************************************************************************************
005900071128
006000080612
006100071128      //********************************************************************************************
006200071128      //
006300080612      // Definizione prototipi procedure (globali)
006400071128      //
006500071128      //********************************************************************************************
006600161017     D/COPY GAITRASRC/SRCPROTOPR,UBIFSUTIR
006700161017     D/COPY GAITRASRC/SRCPROTOPI,UBIFSUTIR
006800161017     D/COPY GAITRASRC/SRCPROTOPR,UBIFS
006900161021     D/COPY GAITRASRC/SRCPROTOPI,UBIFS
007000161018     D/COPY GAITRASRC/SRCPROTOPR,UBBASE64R
007100161018     D/COPY GAITRASRC/SRCPROTOPI,UBBASE64R
007200080612
007300080612
007400071128      //********************************************************************************************
007500071128      //
007600080612      // Definizione variabili work (globali)
007700071128      //
007800071128      //********************************************************************************************
007900151126     D InitDone        S               N   INZ(*off)
008000161017     D
008100071129
008200151126
008300160404
008400161017     P*--------------------------------------------------
008500161017     P* Procedure name: UBIFSUTIR_GetFileNameElements
008600161017     P* Purpose:        Riconosce e separa gli elementi che compongono il percorso e/o nome file
008700160404     P* Returns:        Integer (Status_Code)
008800161017     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
008900161017     P* Parameter:      oIFSUTIpath     => Percorso file
009000161017     P* Parameter:      oIFSUTIfullName => Nome file completo (nome + estensione)
009100161017     P* Parameter:      oIFSUTIName     => Nome file no estensione
009200161017     P* Parameter:      oIFSUTIExt      => Estensione
009300160404     P*--------------------------------------------------
009400160404     P*
009500161017     P UBIFSUTIR_GetFileNameElements...
009600160404     P                 B                   EXPORT
009700160404     D*
009800161017     D UBIFSUTIR_GetFileNameElements...
009900160404     D                 PI            10I 0
010000161017     DiIFSUTIfile                  1024A   CONST
010100161017     DoIFSUTIpath                   512A
010200161017     DoIFSUTIfullName               512A
010300161017     DoIFSUTIName                   512A
010400161017     DoIFSUTIExt                     32A
010500160404     D*
010600160404      //********************************************************************************************
010700160404      //
010800160404      // Definizione variabili work (locali)
010900160404      //
011000160404      //********************************************************************************************
011100160404     D locMethodCode   S             10I 0
011200160404     D
011300161024     DwIFSUTIfile      S                   INZ LIKE(iIFSUTIfile)
011400161017     D wPosPathName    S              4S 0 inz
011500161017     D wPosNameExt     S              4S 0 inz
011600161017     D wPos            S              4S 0 inz
011700160404
011800160404      /FREE
011900161017        // Inizializzo opcode di metodo a errore
012000160404        locMethodCode = -1;
012100161017
012200161017
012300161017        // Monitorizzo l'intero processo
012400161017        Monitor;
012500161017
012600161017        // Inizializzo i parametri di output
012700161017        clear oIFSUTIpath;
012800161017        clear oIFSUTIfullName;
012900161017        clear oIFSUTIName;
013000161017        clear oIFSUTIExt;
013100160404
013200161017        // Prima di tutto "noralizzo" carattere di separazione gerarchica File System
013300161017        wIFSUTIfile = %scanrpl('\':'/':iIFSUTIfile);
013400161019
013500161019        // Verifico che il path/file inizi sempre con il carattere '/'
013600161019        if %subst(wIFSUTIfile:1:1) <> '/';
013700161019           wIFSUTIfile = '/' + %trim(wIFSUTIfile);
013800161019        endif;
013900161017
014000161017        // Reperisco posisionze del ultimo carattere separazione generachica FS
014100161017        wPosPathName = *zeros;
014200161017        wPos = %scan('/':wIFSUTIfile:1);
014300161017        dow wPos > *zeros AND wPos < %len(%trim(wIFSUTIfile));
014400161017            if wPos > *zeros;
014500161017               wPosPathName = wPos;
014600161017            endif;
014700161017            wPos = wPos + 1;
014800161017            wPos = %scan('/':wIFSUTIfile:wPos);
014900161017        enddo;
015000161017
015100161017        // Determino il path e il nome file full (nome + estensione)
015200161017        oIFSUTIpath     = %subst(wIFSUTIfile:1:wPosPathName-1);
015300161017        oIFSUTIfullName = %subst(wIFSUTIfile:wPosPathName+1);
015400161017
015500161017
015600161017        // Reperisco posisionze del ultimo '.' di separazione elementi nome file
015700161017        wPosNameExt = *zeros;
015800161017        wPos = %scan('.':oIFSUTIfullName:1);
015900161017        dow wPos > *zeros AND wPos < %len(%trim(oIFSUTIfullName));
016000161017            if wPos > *zeros;
016100161017               wPosNameExt = wPos;
016200161017            endif;
016300161017            wPos = wPos + 1;
016400161017            wPos = %scan('.':oIFSUTIfullName:wPos);
016500161017        enddo;
016600161017
016700161017        // Determino il path e il nome file no estenzione e l'estensione
016800161017        oIFSUTIName = %subst(oIFSUTIfullName:1:wPosNameExt-1);
016900161017        oIFSUTIExt  = %subst(oIFSUTIfullName:wPosNameExt+1);
017000160404
017100161017
017200161017        On-Error;
017300161017
017400161017           // Esito chiamata già inizializzato a errore e parametri output già puliti
017500161017
017600161017        // Fine monitoring
017700161017        EndMon;
017800161017
017900161017
018000161017        // Valorizzo opcode di metodo
018100161017        locMethodCode = 0;
018200160404
018300160404        return locMethodCode;
018400160404
018500160404      /END-FREE
018600160404
018700161017     P UBIFSUTIR_GetFileNameElements...
018800160404     P                 E
018900161017
019000161017
019100161017
019200161017     P*--------------------------------------------------
019300161019     P* Procedure name: UBIFSUTIR_GetFileSizeByte
019400161019     P* Purpose:        Reperisce la dimensione di un file in Byte
019500161017     P* Returns:        Integer (Status_Code)
019600161017     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
019700161019     P* Parameter:      oIFSUTIsizeByte => Dimensione file in Byte
019800161017     P*--------------------------------------------------
019900161017     P*
020000161019     P UBIFSUTIR_GetFileSizeByte...
020100161017     P                 B                   EXPORT
020200161017     D*
020300161019     D UBIFSUTIR_GetFileSizeByte...
020400161017     D                 PI            10I 0
020500161017     DiIFSUTIfile                  1024A   CONST
020600161019     DoIFSUTIsizeByte                10I 0
020700161017     D*
020800161017      //********************************************************************************************
020900161017      //
021000161017      // Definizione variabili work (locali)
021100161017      //
021200161017      //********************************************************************************************
021300161017     D locMethodCode   S             10I 0
021400161017     D
021500161024     DwIFSUTIfile      S                   INZ LIKE(iIFSUTIfile)
021600161024     DwIFSUTIstatDS    DS                  INZ LIKEDS(IFSstatDS)
021700161017
021800161017      /FREE
021900161017        // Inizializzo opcode di metodo a errore
022000161017        locMethodCode = -1;
022100161017
022200161017
022300161017        // Monitorizzo l'intero processo
022400161017        Monitor;
022500161019
022600161019        // Verifico che il path/file inizi sempre con il carattere '/'
022700161019        wIFSUTIfile = iIFSUTIfile;
022800161019        if %subst(wIFSUTIfile:1:1) <> '/';
022900161019           wIFSUTIfile = '/' + %trim(wIFSUTIfile);
023000161019        endif;
023100161017
023200161017        // Inizializzo i parametri di output
023300161019        clear oIFSUTIsizeByte;
023400161017
023500161017        // Reperisco gli attributi del file ricevuto in input
023600161017        clear wIFSUTIstatDS;
023700161019        IFSrc = IFS_GetFileInfo(%trim(wIFSUTIfile):%addr(wIFSUTIstatDS));
023800161017
023900161017        // Se reperimento attributi file riuscito
024000161017        if IFSrc >= *zeros;
024100161019           // Ritorno la dimensione del file in Byte
024200161019           oIFSUTIsizeByte = wIFSUTIstatDS.st_size;
024300161021
024400161021           // Valorizzo opcode di metodo
024500161021           locMethodCode = 0;
024600161017        endif;
024700161017
024800161017
024900161017        On-Error;
025000161017
025100161017           // Esito chiamata già inizializzato a errore e parametri output già puliti
025200161017
025300161017        // Fine monitoring
025400161017        EndMon;
025500161017
025600161017
025700161017        return locMethodCode;
025800161017
025900161017      /END-FREE
026000161017
026100161019     P UBIFSUTIR_GetFileSizeByte...
026200161017     P                 E
026300161021
026400161021
026500161021
026600161021     P*--------------------------------------------------
026700161021     P* Procedure name: UBIFSUTIR_GetFileEncoding
026800161021     P* Purpose:        Reperisce la CodePage e il CCSID
026900161021     P* Returns:        Integer (Status_Code)
027000161021     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
027100161021     P* Parameter:      oIFSUTIcodePage => CodePage del file
027200161021     P* Parameter:      oIFSUTI_CCSID   => CCSID del file
027300161021     P*--------------------------------------------------
027400161021     P*
027500161021     P UBIFSUTIR_GetFileEncoding...
027600161021     P                 B                   EXPORT
027700161021     D*
027800161021     D UBIFSUTIR_GetFileEncoding...
027900161021     D                 PI            10I 0
028000161021     DiIFSUTIfile                  1024A   CONST
028100161021     DoIFSUTIcodePage                 5U 0
028200161021     DoIFSUTI_CCSID                   5U 0
028300161021     D*
028400161021      //********************************************************************************************
028500161021      //
028600161021      // Definizione variabili work (locali)
028700161021      //
028800161021      //********************************************************************************************
028900161021     D locMethodCode   S             10I 0
029000161021     D
029100161024     DwIFSUTIfile      S                   INZ LIKE(iIFSUTIfile)
029200161024     DwIFSUTIstatDS    DS                  INZ LIKEDS(IFSstatDS)
029300161021
029400161021      /FREE
029500161021        // Inizializzo opcode di metodo a errore
029600161021        locMethodCode = -1;
029700161021
029800161021
029900161021        // Monitorizzo l'intero processo
030000161021        Monitor;
030100161021
030200161021        // Verifico che il path/file inizi sempre con il carattere '/'
030300161021        wIFSUTIfile = iIFSUTIfile;
030400161021        if %subst(wIFSUTIfile:1:1) <> '/';
030500161021           wIFSUTIfile = '/' + %trim(wIFSUTIfile);
030600161021        endif;
030700161021
030800161021        // Inizializzo i parametri di output
030900161021        clear oIFSUTIcodePage;
031000161021        clear oIFSUTI_CCSID;
031100161021
031200161021        // Reperisco gli attributi del file ricevuto in input
031300161021        clear wIFSUTIstatDS;
031400161021        IFSrc = IFS_GetFileInfo(%trim(wIFSUTIfile):%addr(wIFSUTIstatDS));
031500161021
031600161021        // Se reperimento attributi file riuscito
031700161021        if IFSrc >= *zeros;
031800161021           // Ritorno la dimensione del file in Byte
031900161021           oIFSUTIcodePage = wIFSUTIstatDS.st_codepage;
032000161021           oIFSUTI_CCSID   = wIFSUTIstatDS.st_ccsid;
032100161021
032200161021           // Valorizzo opcode di metodo
032300161021           locMethodCode = 0;
032400161021        endif;
032500161021
032600161021
032700161021        On-Error;
032800161021
032900161021           // Esito chiamata già inizializzato a errore e parametri output già puliti
033000161021
033100161021        // Fine monitoring
033200161021        EndMon;
033300161021
033400161021
033500161021        return locMethodCode;
033600161021
033700161021      /END-FREE
033800161021
033900161021     P UBIFSUTIR_GetFileEncoding...
034000161021     P                 E
034100161018
034200161018
034300161018
034400161018     P*--------------------------------------------------
034500161018     P* Procedure name: UBIFSUTIR_ReadFileUpTo64K
034600161026     P* Purpose:        Legge "one-shot" intero file (dimensioni max 64KB)
034700161018     P* Returns:        Integer (Status_Code)
034800161018     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
034900161018     P* Parameter:      iIFSUTIsizeByte => Dimensione file attesa
035000161028     P* Parameter:      oIFSUTIsizeByte => Dimensione file rilevata
035100161018     P* Parameter:      oIFSUTIdata64K  => Contenuto del file
035200161018     P*--------------------------------------------------
035300161018     P*
035400161018     P UBIFSUTIR_ReadFileUpTo64K...
035500161018     P                 B                   EXPORT
035600161018     D*
035700161018     D UBIFSUTIR_ReadFileUpTo64K...
035800161018     D                 PI            10I 0
035900161018     DiIFSUTIfile                  1024A   CONST
036000161018     DiIFSUTIsizeByte                10I 0 CONST
036100161028     DoIFSUTIsizeByte                10I 0
036200161030     DoIFSUTIdata64K                   A   LEN(65536) VARYING OPTIONS(*VARSIZE)       (4 Byte x Len)
036300161018     D*
036400161018      //********************************************************************************************
036500161018      //
036600161018      // Definizione variabili work (locali)
036700161018      //
036800161018      //********************************************************************************************
036900161018     D locMethodCode   S             10I 0
037000161018     D
037100161024     DwIFSUTIfile      S                   INZ LIKE(iIFSUTIfile)
037200161102     DwIFSUTIdata64K   S          65536A   INZ
037300161018     D wLen            S             10I 0 inz
037400161018
037500161018      /FREE
037600161018        // Inizializzo opcode di metodo a errore
037700161018        locMethodCode = -1;
037800161018
037900161018
038000161018        // Monitorizzo l'intero processo
038100161018        Monitor;
038200161026
038300161026        // Inizializzo parametri di output
038400161026        clear oIFSUTIdata64K;
038500161028        clear oIFSUTIsizeByte;
038600161019
038700161019        // Verifico che il path/file inizi sempre con il carattere '/'
038800161019        wIFSUTIfile = iIFSUTIfile;
038900161019        if %subst(wIFSUTIfile:1:1) <> '/';
039000161019           wIFSUTIfile = '/' + %trim(wIFSUTIfile);
039100161019        endif;
039200161018
039300161018        // Innanzitutto apro il file in sola lettura
039400161019        IFSfileDescr = IFS_OpenFile(%trim(wIFSUTIfile):O_RDONLY);
039500161018
039600161018        // Innanzitutto apro il file in sola lettura
039700161018        if IFSfileDescr >= *zeros;
039800161102           wLen = IFS_ReadFile(IFSfileDescr:%addr(wIFSUTIdata64K):
039900161102                                            %size(wIFSUTIdata64K));
040000161018           if wLen > *zeros;
040100161018
040200161018              // Valorizzo opcode di metodo a OK
040300161018              locMethodCode = 0;
040400161028
040500161028              // Valorizzo buffer di output
040600161028              oIFSUTIsizeByte = wLen;
040700161018
040800161018              // Se passata in input anche la dimensione attesa del file
040900161018              // effettuo verifica di congruenza
041000161018              if iIFSUTIsizeByte > *zeros;
041100161030                 if iIFSUTIsizeByte = oIFSUTIsizeByte;
041200161102
041300161102                    // Valorizo parametri di output
041400161102                    oIFSUTIdata64K = %subst(wIFSUTIdata64K:1:oIFSUTIsizeByte);
041500161102
041600161019                 else;
041700161030
041800161030                    // Per sicurezza inizializzo parametri di output
041900161030                    clear oIFSUTIdata64K;
042000161018
042100161018                    // Valorizzo opcode di metodo a KO = discepanza dimensione file attesa
042200161018                    locMethodCode = -2;
042300161018
042400161018                 endif;
042500161018              endif;
042600161018
042700161018           endif;
042800161018
042900161018           // Chiudo il file nel IFS
043000161018           IFS_CloseFile(IFSfileDescr);
043100161018
043200161018        endif;
043300161018
043400161018
043500161018        On-Error;
043600161018
043700161018           // Esito chiamata già inizializzato a errore e parametri output già puliti
043800161018
043900161018           // Monitorizzo anche quanto eventualmente eseguito nella gestione errore superiore
044000161018           Monitor;
044100161018              // Chiudo il file nel IFS
044200161018              IFS_CloseFile(IFSfileDescr);
044300161018           On-Error;
044400161018           // Fine monitoring
044500161018           EndMon;
044600161018
044700161018        // Fine monitoring
044800161018        EndMon;
044900161018
045000161018
045100161018        return locMethodCode;
045200161018
045300161018      /END-FREE
045400161018
045500161018     P UBIFSUTIR_ReadFileUpTo64K...
045600161018     P                 E
045700170518
045800170518
045900170518
046000170518     P*--------------------------------------------------
046100170518     P* Procedure name: UBIFSUTIR_ReadFileUpTo64Ktext
046200170518     P* Purpose:        Legge "one-shot" intero file (dimensioni max 64KB) in modalità testuale
046300170518     P* Returns:        Integer (Status_Code)
046400170518     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
046500170518     P* Parameter:      iIFSUTIsizeByte => Dimensione file attesa
046600170518     P* Parameter:      oIFSUTIsizeByte => Dimensione file rilevata
046700170518     P* Parameter:      oIFSUTIdata64K  => Contenuto del file
046800170518     P*--------------------------------------------------
046900170518     P*
047000170518     P UBIFSUTIR_ReadFileUpTo64Ktext...
047100170518     P                 B                   EXPORT
047200170518     D*
047300170518     D UBIFSUTIR_ReadFileUpTo64Ktext...
047400170518     D                 PI            10I 0
047500170518     DiIFSUTIfile                  1024A   CONST
047600170518     DiIFSUTIsizeByte                10I 0 CONST
047700170518     DoIFSUTIsizeByte                10I 0
047800170518     DoIFSUTIdata64K                   A   LEN(65536) VARYING OPTIONS(*VARSIZE)       (4 Byte x Len)
047900170518     D*
048000170518      //********************************************************************************************
048100170518      //
048200170518      // Definizione variabili work (locali)
048300170518      //
048400170518      //********************************************************************************************
048500170518     D locMethodCode   S             10I 0
048600170518     D
048700170518     DwIFSUTIfile      S                   INZ LIKE(iIFSUTIfile)
048800170518     DwIFSUTIdata64K   S          65536A   INZ
048900170518     DwIFSUTIoFlg      S             10I 0 INZ
049000170518     DwIFSUTImode      S             10U 0 INZ
049100170518     DwIFSUTIcodePage  S             10U 0 INZ
049200170518     D wLen            S             10I 0 inz
049300170518
049400170518      /FREE
049500170518        // Inizializzo opcode di metodo a errore
049600170518        locMethodCode = -1;
049700170518
049800170518
049900170518        // Monitorizzo l'intero processo
050000170518        Monitor;
050100170518
050200170518        // Inizializzo parametri di output
050300170518        clear oIFSUTIdata64K;
050400170518        clear oIFSUTIsizeByte;
050500170518
050600170518        // Verifico che il path/file inizi sempre con il carattere '/'
050700170518        wIFSUTIfile = iIFSUTIfile;
050800170518        if %subst(wIFSUTIfile:1:1) <> '/';
050900170518           wIFSUTIfile = '/' + %trim(wIFSUTIfile);
051000170518        endif;
051100170518
051200170518        // Innanzitutto apro il file in sola lettura
051300170518        wIFSUTIoFlg     = O_RDONLY + O_TEXTDATA + O_CCSID;
051400170518        wIFSUTImode     = S_IRGRP;
051500170518        wIFSUTIcodePage = 1144;
051600170518
051700170518        IFSfileDescr = IFS_OpenFile(%trim(wIFSUTIfile):wIFSUTIoFlg:
051800170518                                    wIFSUTImode:wIFSUTIcodePage);
051900170518
052000170518        // Innanzitutto apro il file in sola lettura
052100170518        if IFSfileDescr >= *zeros;
052200170518           wLen = IFS_ReadFile(IFSfileDescr:%addr(wIFSUTIdata64K):
052300170518                                            %size(wIFSUTIdata64K));
052400170518           if wLen > *zeros;
052500170518
052600170518              // Valorizzo opcode di metodo a OK
052700170518              locMethodCode = 0;
052800170518
052900170518              // Valorizzo buffer di output
053000170518              oIFSUTIsizeByte = wLen;
053100170518
053200170518              // Se passata in input anche la dimensione attesa del file
053300170518              // effettuo verifica di congruenza
053400170518              if iIFSUTIsizeByte > *zeros;
053500170518                 if iIFSUTIsizeByte = oIFSUTIsizeByte;
053600170518
053700170518                    // Valorizo parametri di output
053800170518                    oIFSUTIdata64K = %subst(wIFSUTIdata64K:1:oIFSUTIsizeByte);
053900170518
054000170518                 else;
054100170518
054200170518                    // Per sicurezza inizializzo parametri di output
054300170518                    clear oIFSUTIdata64K;
054400170518
054500170518                    // Valorizzo opcode di metodo a KO = discepanza dimensione file attesa
054600170518                    locMethodCode = -2;
054700170518
054800170518                 endif;
054900170518              endif;
055000170518
055100170518           endif;
055200170518
055300170518           // Chiudo il file nel IFS
055400170518           IFS_CloseFile(IFSfileDescr);
055500170518
055600170518        endif;
055700170518
055800170518
055900170518        On-Error;
056000170518
056100170518           // Esito chiamata già inizializzato a errore e parametri output già puliti
056200170518
056300170518           // Monitorizzo anche quanto eventualmente eseguito nella gestione errore superiore
056400170518           Monitor;
056500170518              // Chiudo il file nel IFS
056600170518              IFS_CloseFile(IFSfileDescr);
056700170518           On-Error;
056800170518           // Fine monitoring
056900170518           EndMon;
057000170518
057100170518        // Fine monitoring
057200170518        EndMon;
057300170518
057400170518
057500170518        return locMethodCode;
057600170518
057700170518      /END-FREE
057800170518
057900170518     P UBIFSUTIR_ReadFileUpTo64Ktext...
058000170518     P                 E
058100170516
058200170516
058300170516
058400170516     P*--------------------------------------------------
058500170518     P* Procedure name: UBIFSUTIR_ReadFileUpTo4MBparm
058600170516     P* Purpose:        Legge "one-shot" intero file (dimensioni max 4MB)
058700170516     P* Returns:        Integer (Status_Code)
058800170516     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
058900170516     P* Parameter:      iIFSUTIsizeByte => Dimensione file attesa
059000170516     P* Parameter:      iIFSUTIoFlg     => Open Flags
059100170516     P* Parameter:      iIFSUTImode     => Open mode
059200170516     P* Parameter:      iIFSUTIcodePage => Open CodePage
059300170516     P* Parameter:      oIFSUTIsizeByte => Dimensione file rilevata
059400170516     P* Parameter:      oIFSUTIdata4MB  => Contenuto del file
059500170516     P*--------------------------------------------------
059600170516     P*
059700170518     P UBIFSUTIR_ReadFileUpTo4MBparm...
059800170516     P                 B                   EXPORT
059900170516     D*
060000170518     D UBIFSUTIR_ReadFileUpTo4MBparm...
060100170516     D                 PI            10I 0
060200170516     DiIFSUTIfile                  1024A   CONST
060300170516     DiIFSUTIsizeByte                10I 0 CONST
060400170516     DiIFSUTIoFlg                    10I 0 CONST
060500170516     DiIFSUTImode                    10U 0 CONST
060600170516     DiIFSUTIcodePage                10U 0 CONST
060700170516     DoIFSUTIsizeByte                10I 0
060800170516     DoIFSUTIdata4MB                   A   LEN(4194304) VARYING                      (4 Byte x Len)
060900170516     D                                     OPTIONS(*VARSIZE)
061000170516     D*
061100170516      //********************************************************************************************
061200170516      //
061300170516      // Definizione variabili work (locali)
061400170516      //
061500170516      //********************************************************************************************
061600170516     D locMethodCode   S             10I 0
061700170516     D
061800170516     DwIFSUTIfile      S                   INZ LIKE(iIFSUTIfile)
061900170516     DwIFSUTIdata4MB   S               A   INZ LEN(4194304)
062000170516     D wLen            S             10I 0 inz
062100170516
062200170516      /FREE
062300170516        // Inizializzo opcode di metodo a errore
062400170516        locMethodCode = -1;
062500170516
062600170516
062700170516        // Monitorizzo l'intero processo
062800170516        Monitor;
062900170516
063000170516        // Inizializzo parametri di output
063100170516        clear oIFSUTIdata4MB;
063200170516        clear oIFSUTIsizeByte;
063300170516
063400170516        // Verifico che il path/file inizi sempre con il carattere '/'
063500170516        wIFSUTIfile = iIFSUTIfile;
063600170516        if %subst(wIFSUTIfile:1:1) <> '/';
063700170516           wIFSUTIfile = '/' + %trim(wIFSUTIfile);
063800170516        endif;
063900170516
064000170516        // Innanzitutto apro il file in sola lettura
064100170516        IFSfileDescr = IFS_OpenFile(%trim(wIFSUTIfile):iIFSUTIoFlg:
064200170516                                    iIFSUTImode:iIFSUTIcodePage);
064300170516
064400170516        // Innanzitutto apro il file in sola lettura
064500170516        if IFSfileDescr >= *zeros;
064600170516           wLen = IFS_ReadFile(IFSfileDescr:%addr(wIFSUTIdata4MB):
064700170516                                            %size(wIFSUTIdata4MB));
064800170516           if wLen > *zeros;
064900170516
065000170516              // Valorizzo opcode di metodo a OK
065100170516              locMethodCode = 0;
065200170516
065300170516              // Valorizzo buffer di output
065400170516              oIFSUTIsizeByte = wLen;
065500170516
065600170516              // Se passata in input anche la dimensione attesa del file
065700170516              // effettuo verifica di congruenza
065800170516              if iIFSUTIsizeByte > *zeros;
065900170516                 if iIFSUTIsizeByte = oIFSUTIsizeByte;
066000170516
066100170516                    // Valorizo parametri di output
066200170516                    oIFSUTIdata4MB = %subst(wIFSUTIdata4MB:1:oIFSUTIsizeByte);
066300170516
066400170516                 else;
066500170516
066600170516                    // Per sicurezza inizializzo parametri di output
066700170516                    clear oIFSUTIdata4MB;
066800170516
066900170516                    // Valorizzo opcode di metodo a KO = discepanza dimensione file attesa
067000170516                    locMethodCode = -2;
067100170516
067200170516                 endif;
067300170516              endif;
067400170516
067500170516           endif;
067600170516
067700170516           // Chiudo il file nel IFS
067800170516           IFS_CloseFile(IFSfileDescr);
067900170516
068000170516        endif;
068100170516
068200170516
068300170516        On-Error;
068400170516
068500170516           // Esito chiamata già inizializzato a errore e parametri output già puliti
068600170516
068700170516           // Monitorizzo anche quanto eventualmente eseguito nella gestione errore superiore
068800170516           Monitor;
068900170516              // Chiudo il file nel IFS
069000170516              IFS_CloseFile(IFSfileDescr);
069100170516           On-Error;
069200170516           // Fine monitoring
069300170516           EndMon;
069400170516
069500170516        // Fine monitoring
069600170516        EndMon;
069700170516
069800170516
069900170516        return locMethodCode;
070000170516
070100170516      /END-FREE
070200170516
070300170518     P UBIFSUTIR_ReadFileUpTo4MBparm...
070400170516     P                 E
070500170516
070600170516
070700170516
070800170516     P*--------------------------------------------------
070900170516     P* Procedure name: UBIFSUTIR_ReadFileUpTo4MBtext
071000170516     P* Purpose:        Legge "one-shot" intero file (dimensioni max 4MB) in modalità testuale
071100170516     P* Returns:        Integer (Status_Code)
071200170516     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
071300170516     P* Parameter:      iIFSUTIsizeByte => Dimensione file attesa
071400170516     P* Parameter:      oIFSUTIsizeByte => Dimensione file rilevata
071500170516     P* Parameter:      oIFSUTIdata4MB  => Contenuto del file
071600170516     P*--------------------------------------------------
071700170516     P*
071800170516     P UBIFSUTIR_ReadFileUpTo4MBtext...
071900170516     P                 B                   EXPORT
072000170516     D*
072100170516     D UBIFSUTIR_ReadFileUpTo4MBtext...
072200170516     D                 PI            10I 0
072300170516     DiIFSUTIfile                  1024A   CONST
072400170516     DiIFSUTIsizeByte                10I 0 CONST
072500170516     DoIFSUTIsizeByte                10I 0
072600170516     DoIFSUTIdata4MB                   A   LEN(4194304) VARYING                      (4 Byte x Len)
072700170516     D                                     OPTIONS(*VARSIZE)
072800170516     D*
072900170516      //********************************************************************************************
073000170516      //
073100170516      // Definizione variabili work (locali)
073200170516      //
073300170516      //********************************************************************************************
073400170516     D locMethodCode   S             10I 0
073500170516     D
073600170516     DwIFSUTIfile      S                   INZ LIKE(iIFSUTIfile)
073700170518     DwIFSUTIdata4MB   S               A   INZ LEN(4194304)
073800170516     DwIFSUTIoFlg      S             10I 0 INZ
073900170516     DwIFSUTImode      S             10U 0 INZ
074000170516     DwIFSUTIcodePage  S             10U 0 INZ
074100170516     D wLen            S             10I 0 inz
074200170516
074300170516      /FREE
074400170516        // Inizializzo opcode di metodo a errore
074500170516        locMethodCode = -1;
074600170516
074700170516
074800170516        // Monitorizzo l'intero processo
074900170516        Monitor;
075000170516
075100170516        // Inizializzo parametri di output
075200170516        clear oIFSUTIdata4MB;
075300170516        clear oIFSUTIsizeByte;
075400170516
075500170516        // Verifico che il path/file inizi sempre con il carattere '/'
075600170516        wIFSUTIfile = iIFSUTIfile;
075700170516        if %subst(wIFSUTIfile:1:1) <> '/';
075800170516           wIFSUTIfile = '/' + %trim(wIFSUTIfile);
075900170516        endif;
076000170516
076100170516        // Innanzitutto apro il file in sola lettura
076200170518 xxx    wIFSUTIoFlg     = O_RDONLY + O_TEXTDATA + O_CCSID;
076300170516        wIFSUTImode     = S_IRGRP;
076400170516        wIFSUTIcodePage = 1144;
076500170516
076600170516        IFSfileDescr = IFS_OpenFile(%trim(wIFSUTIfile):wIFSUTIoFlg:
076700170516                                    wIFSUTImode:wIFSUTIcodePage);
076800170516
076900170516        // Innanzitutto apro il file in sola lettura
077000170516        if IFSfileDescr >= *zeros;
077100170518           wLen = IFS_ReadFile(IFSfileDescr:%addr(wIFSUTIdata4MB):
077200170518                                            %size(wIFSUTIdata4MB));
077300170518
077400170516           if wLen > *zeros;
077500170516
077600170516              // Valorizzo opcode di metodo a OK
077700170516              locMethodCode = 0;
077800170516
077900170516              // Valorizzo buffer di output
078000170516              oIFSUTIsizeByte = wLen;
078100170516
078200170516              // Se passata in input anche la dimensione attesa del file
078300170516              // effettuo verifica di congruenza
078400170516              if iIFSUTIsizeByte > *zeros;
078500170516                 if iIFSUTIsizeByte = oIFSUTIsizeByte;
078600170516
078700170516                    // Valorizo parametri di output
078800170518                    oIFSUTIdata4MB = %subst(wIFSUTIdata4MB:1:oIFSUTIsizeByte);
078900170516
079000170516                 else;
079100170516
079200170516                    // Per sicurezza inizializzo parametri di output
079300170516                    clear oIFSUTIdata4MB;
079400170516
079500170516                    // Valorizzo opcode di metodo a KO = discepanza dimensione file attesa
079600170516                    locMethodCode = -2;
079700170516
079800170516                 endif;
079900170516              endif;
080000170516
080100170516           endif;
080200170516
080300170516           // Chiudo il file nel IFS
080400170516           IFS_CloseFile(IFSfileDescr);
080500170516
080600170516        endif;
080700170516
080800170516
080900170516        On-Error;
081000170516
081100170516           // Esito chiamata già inizializzato a errore e parametri output già puliti
081200170516
081300170516           // Monitorizzo anche quanto eventualmente eseguito nella gestione errore superiore
081400170516           Monitor;
081500170516              // Chiudo il file nel IFS
081600170516              IFS_CloseFile(IFSfileDescr);
081700170516           On-Error;
081800170516           // Fine monitoring
081900170516           EndMon;
082000170516
082100170516        // Fine monitoring
082200170516        EndMon;
082300170516
082400170516
082500170516        return locMethodCode;
082600170516
082700170516      /END-FREE
082800170516
082900170516     P UBIFSUTIR_ReadFileUpTo4MBtext...
083000170516     P                 E
083100161030
083200161030
083300161030
083400170516     P*--------------------------------------------------
083500170518     P* Procedure name: UBIFSUTIR_ReadFileUpTo4MB
083600170516     P* Purpose:        Legge "one-shot" intero file (dimensioni max 4MB) in modalità binaria
083700170516     P* Returns:        Integer (Status_Code)
083800170516     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
083900170516     P* Parameter:      iIFSUTIsizeByte => Dimensione file attesa
084000170516     P* Parameter:      oIFSUTIsizeByte => Dimensione file rilevata
084100170516     P* Parameter:      oIFSUTIdata4MB  => Contenuto del file
084200170516     P*--------------------------------------------------
084300170516     P*
084400170518     P UBIFSUTIR_ReadFileUpTo4MB...
084500170516     P                 B                   EXPORT
084600170516     D*
084700170518     D UBIFSUTIR_ReadFileUpTo4MB...
084800170516     D                 PI            10I 0
084900170516     DiIFSUTIfile                  1024A   CONST
085000170516     DiIFSUTIsizeByte                10I 0 CONST
085100170516     DoIFSUTIsizeByte                10I 0
085200170516     DoIFSUTIdata4MB                   A   LEN(4194304) VARYING                      (4 Byte x Len)
085300170516     D                                     OPTIONS(*VARSIZE)
085400170516     D*
085500170516      //********************************************************************************************
085600170516      //
085700170516      // Definizione variabili work (locali)
085800170516      //
085900170516      //********************************************************************************************
086000170516     D locMethodCode   S             10I 0
086100170516     D
086200170516     DwIFSUTIfile      S                   INZ LIKE(iIFSUTIfile)
086300170516     DwIFSUTIdata4MB   S               A   INZ LEN(4194304)
086400170516     D wLen            S             10I 0 inz
086500170516
086600170516      /FREE
086700170516        // Inizializzo opcode di metodo a errore
086800170516        locMethodCode = -1;
086900170516
087000170516
087100170516        // Monitorizzo l'intero processo
087200170516        Monitor;
087300170516
087400170516        // Inizializzo parametri di output
087500170516        clear oIFSUTIdata4MB;
087600170516        clear oIFSUTIsizeByte;
087700170516
087800170516        // Verifico che il path/file inizi sempre con il carattere '/'
087900170516        wIFSUTIfile = iIFSUTIfile;
088000170516        if %subst(wIFSUTIfile:1:1) <> '/';
088100170516           wIFSUTIfile = '/' + %trim(wIFSUTIfile);
088200170516        endif;
088300170516
088400170516        // Innanzitutto apro il file in sola lettura
088500170516        IFSfileDescr = IFS_OpenFile(%trim(wIFSUTIfile):O_RDONLY);
088600170516
088700170516        // Innanzitutto apro il file in sola lettura
088800170516        if IFSfileDescr >= *zeros;
088900170516           wLen = IFS_ReadFile(IFSfileDescr:%addr(wIFSUTIdata4MB):
089000170516                                            %size(wIFSUTIdata4MB));
089100170516           if wLen > *zeros;
089200170516
089300170516              // Valorizzo opcode di metodo a OK
089400170516              locMethodCode = 0;
089500170516
089600170516              // Valorizzo buffer di output
089700170516              oIFSUTIsizeByte = wLen;
089800170516
089900170516              // Se passata in input anche la dimensione attesa del file
090000170516              // effettuo verifica di congruenza
090100170516              if iIFSUTIsizeByte > *zeros;
090200170516                 if iIFSUTIsizeByte = oIFSUTIsizeByte;
090300170516
090400170516                    // Valorizo parametri di output
090500170516                    oIFSUTIdata4MB = %subst(wIFSUTIdata4MB:1:oIFSUTIsizeByte);
090600170516
090700170516                 else;
090800170516
090900170516                    // Per sicurezza inizializzo parametri di output
091000170516                    clear oIFSUTIdata4MB;
091100170516
091200170516                    // Valorizzo opcode di metodo a KO = discepanza dimensione file attesa
091300170516                    locMethodCode = -2;
091400170516
091500170516                 endif;
091600170516              endif;
091700170516
091800170516           endif;
091900170516
092000170516           // Chiudo il file nel IFS
092100170516           IFS_CloseFile(IFSfileDescr);
092200170516
092300170516        endif;
092400170516
092500170516
092600170516        On-Error;
092700170516
092800170516           // Esito chiamata già inizializzato a errore e parametri output già puliti
092900170516
093000170516           // Monitorizzo anche quanto eventualmente eseguito nella gestione errore superiore
093100170516           Monitor;
093200170516              // Chiudo il file nel IFS
093300170516              IFS_CloseFile(IFSfileDescr);
093400170516           On-Error;
093500170516           // Fine monitoring
093600170516           EndMon;
093700170516
093800170516        // Fine monitoring
093900170516        EndMon;
094000170516
094100170516
094200170516        return locMethodCode;
094300170516
094400170516      /END-FREE
094500170516
094600170518     P UBIFSUTIR_ReadFileUpTo4MB...
094700170516     P                 E
094800151126
