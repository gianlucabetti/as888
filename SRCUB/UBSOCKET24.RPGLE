000100151125     H BNDDIR('UBBNDDIR':'QC2LE')
000200080611     H NOMAIN
000300071128     ***********************************************************************************************
000400071128     **
000500090710     ** Questo modulo fornisce le funzionalità per eseguire un socket client program
000600071128     **
000700080611     ** Può essere usato solo come sottoprocedura.
000800080612     ** Ogni metodo restituisce un valore 10I:
000900080612     **             - = 0 => tutto ok
001000080612     **             - < 0 => errore
001100090713     **
001200080612     **
001300080612     ** --------- --- --------- ------------------------------
001400071128     **
001500151008     ** PARAMETRI DELLA SOTTOPROCEDURA: UBSOCKET2_Create
001600071128     **
001700071128     ** Posizione Uso Tipo      Descrizione
001800071128     ** --------- --- --------- ------------------------------
001900151126     ** 1         IN  CHARACTER Tipo di Socket ('TCP', 'UDP')
002000151126     ** 2         OUT UNSIGNED  Descrittore Socket
002100071128     ** --------- --- --------- ------------------------------
002200151126     **
002300151126     **
002400151126     **
002500151126     ** --------- --- --------- ------------------------------
002600151126     **
002700151126     ** PARAMETRI DELLA SOTTOPROCEDURA: UBSOCKET2_SetSockOption
002800151126     **
002900151126     ** Posizione Uso Tipo      Descrizione
003000151126     ** --------- --- --------- ------------------------------
003100151126     ** 1         IN  UNSIGNED  Descrittore Socket
003200151126     ** 2         IN  UNSIGNED  Livello Socket
003300151126     ** 3         IN  UNSIGNED  Nome   Opzione da impostare
003400151126     ** 4         IN  UNSIGNED  Valore Opzione da impostare
003500151126     ** --------- --- --------- ------------------------------
003600090713     **
003700090713     **
003800090713     **
003900090713     ** --------- --- --------- ------------------------------
004000090713     **
004100151008     ** PARAMETRI DELLA SOTTOPROCEDURA: UBSOCKET2_Garbage
004200090713     **
004300090713     ** Posizione Uso Tipo      Descrizione
004400090713     ** --------- --- --------- ------------------------------
004500151014     ** 1         IN  UNSIGNED  Descrittore Socket
004600090713     ** --------- --- --------- ------------------------------
004700090713     **
004800071129     **
004900090713     **
005000151008     ** PARAMETRI DELLA SOTTOPROCEDURA UBSOCKET2_Connect
005100071129     **
005200071129     ** Posizione Uso Tipo      Descrizione
005300071129     ** --------- --- --------- ------------------------------
005400151014     ** 1         IN  UNSIGNED  Descrittore Socket
005500151009     ** 2         IN  CHARACTER Ip Address "dotted"/name       => Indirizzo IP "puntato" o nome
005600151126     ** 3         IN  UNSIGNED  Port number                    => Numero porta del socket destinaz.
005700071129     ** --------- --- --------- ------------------------------
005800151009     **
005900151009     **
006000151009     **
006100151009     ** PARAMETRI DELLA SOTTOPROCEDURA UBSOCKET2_Bind
006200151009     **
006300151009     ** Posizione Uso Tipo      Descrizione
006400151009     ** --------- --- --------- ------------------------------
006500151014     ** 1         IN  UNSIGNED  Descrittore Socket
006600151009     ** 2         IN  CHARACTER Local network interface        => Indirizzo interfaccia rete locale
006700151009     ** 3         IN  UNSIGNED  Port number                    => Numero porta del socket "server"
006800151009     ** --------- --- --------- ------------------------------
006900151009     **
007000151009     **
007100151009     **
007200151009     ** PARAMETRI DELLA SOTTOPROCEDURA UBSOCKET2_Listen
007300151009     **
007400151009     ** Posizione Uso Tipo      Descrizione
007500151009     ** --------- --- --------- ------------------------------
007600151014     ** 1         IN  UNSIGNED  Descrittore Socket
007700151009     ** 2         IN  UNSIGNED  Bacl-log number                => Numero max client servibili
007800151009     ** --------- --- --------- ------------------------------
007900151013     **
008000151013     **
008100151013     **
008200151013     ** PARAMETRI DELLA SOTTOPROCEDURA UBSOCKET2_Accept
008300151013     **
008400151013     ** Posizione Uso Tipo      Descrizione
008500151013     ** --------- --- --------- ------------------------------
008600151014     ** 1         IN  UNSIGNED  Descrittore Socket Server
008700151014     ** 2         OUT UNSIGNED  Descrittore Socket Client
008800151013     ** 3         OUT UNSIGNED  Indirizzo IP "dottato" del Client
008900151013     ** --------- --- --------- ------------------------------
009000090713     **
009100090713     **
009200090713     **
009300151126     ** PARAMETRI DELLA SOTTOPROCEDURA UBSOCKET2_Send (TCP)
009400090713     **
009500090713     ** Posizione Uso Tipo      Descrizione
009600090713     ** --------- --- --------- ------------------------------
009700151014     ** 1         IN  UNSIGNED  Descrittore Socket
009800151126     ** 2         IN  CHARACTER Comado Socket da sottomettere via TCP
009900090713     ** 3         IN  CHARACTER Codice tabella conversione
010000090713     ** --------- --- --------- ------------------------------
010100151126     **
010200151126     **
010300151126     **
010400151126     ** PARAMETRI DELLA SOTTOPROCEDURA UBSOCKET2_SendTo (UDP)
010500151126     **
010600151126     ** Posizione Uso Tipo      Descrizione
010700151126     ** --------- --- --------- ------------------------------
010800151126     ** 1         IN  UNSIGNED  Descrittore Socket
010900151126     ** 2         IN  CHARACTER Comado Socket da sottomettere via UDP
011000151126     ** 3         IN  CHARACTER Codice tabella conversione
011100151126     ** 4         IN  CHARACTER Ip Address "dotted"/name       => Indirizzo IP "puntato" o nome
011200151126     ** 5         IN  UNSIGNED  Port number                    => Numero porta del socket destinaz.
011300151126     ** --------- --- --------- ------------------------------
011400151014     **
011500151014     **
011600151014     **
011700151126     ** PARAMETRI DELLA SOTTOPROCEDURA UBSOCKET2_Receive (TCP)
011800151014     **
011900151014     ** Posizione Uso Tipo      Descrizione
012000151014     ** --------- --- --------- ------------------------------
012100151014     ** 1         IN  UNSIGNED  Descrittore Socket Server
012200151126     ** 2         IN  INTEGER   Lunghezza del buffer di lettura messaggio socket client via TCP
012300151014     ** 3         IN  CHARACTER Tipo "End Of Line"
012400151014     ** 4         IN  CHARACTER Codice tabella conversione
012500151014     ** 5         OUT POINTER   Puntatore al contenuto del messaggio socket
012600151014     ** 6         OUT INTEGER   Lunghezza del messaggio socket
012700151014     ** --------- --- --------- ------------------------------
012800151126     **
012900151126     **
013000151126     **
013100151126     ** PARAMETRI DELLA SOTTOPROCEDURA UBSOCKET2_ReceiveFrom (UDP)
013200151126     **
013300151126     ** Posizione Uso Tipo      Descrizione
013400151126     ** --------- --- --------- ------------------------------
013500151126     ** 1         IN  UNSIGNED  Descrittore Socket Server
013600151126     ** 2         IN  INTEGER   Lunghezza del buffer di lettura messaggio socket client via UDP
013700151126     ** 3         IN  CHARACTER Tipo "End Of Line"
013800151126     ** 4         IN  CHARACTER Codice tabella conversione
013900151202     ** 5         OUT POINTER   Puntatore al contenuto del messaggio socket
014000151202     ** 6         OUT INTEGER   Lunghezza del messaggio socket
014100151126     ** --------- --- --------- ------------------------------
014200151008     **
014300151008     **
014400151008     **
014500151008     ** PARAMETRI DELLA SOTTOPROCEDURA UBSOCKET2_SendRcv
014600151008     **
014700151008     ** Posizione Uso Tipo      Descrizione
014800151008     ** --------- --- --------- ------------------------------
014900151014     ** 1         IN  UNSIGNED  Descrittore Socket
015000151008     ** 2         IN  CHARACTER Comado Socket da sottomettere
015100151008     ** 3         IN  CHARACTER Codice tabella conversione
015200151008     ** 4         OUT INTEGER   Lunghezza buffer Output
015300151008     ** 5         OUT CHARACTER Response in Output
015400151008     ** --------- --- --------- ------------------------------
015500080612     **
015600071128     **
015700071128     **
015800071128     ** ESEMPI DI CHIAMATA
015900071128     **
016000151126     **  if UBSOCKET2_Create(pInSktType : pOutSktDescr) = 0;
016100100621     **
016200151008     **  if UBSOCKET2_Garbage(pInSktDescr) = 0;
016300090713     **
016400151008     **  if UBSOCKET2_Connect(pInSktDescr : pInHostAdr : pInPort) = 0;
016500090713     **
016600151008     **  if UBSOCKET2_Send(pInSktDescr : pInSktCmd : pInCmdCvtTbl ) = 0
016700080612     **
016800080612     ** --------- --- --------- ------------------------------
016900071128     **
017000071128     ** ISTRUZIONI PER LA COMPILAZIONE
017100071128     **
017200151125     ** 1. Creare il modulo UBSOCKET2 (Opz 15 PDM) indicando BNDDIR('UBBNDDIR':'QC2LE')
017300151008     ** 2. Creare/Aggiornare il programma di servizio UBSOCKET2 (CRTSRVPGM / UPDSRVPGM)
017400080610     ** 4. Cancellare il modulo.
017500071128     **
017600071128     ***********************************************************************************************
017700080612
017800080612
017900080612      //********************************************************************************************
018000080612      //
018100080612      // Definizione file (globali)
018200080612      //
018300080612      //********************************************************************************************
018400071128
018500080612
018600071128      //********************************************************************************************
018700071128      //
018800080612      // Definizione prototipi procedure (globali)
018900071128      //
019000071128      //********************************************************************************************
019100151008     D/COPY GAITRASRC/SRCPROTOPR,UBSOCKET2
019200151127     D/COPY GAITRASRC/SRCPROTOPI,UBSOCKET2
019300151012     D/COPY GAITRASRC/SRCPROTOPR,UBCVTIDA
019400100922     D/COPY GAITRASRC/SRCPROTOPI,UBCVTIDA
019500151012     D/COPY GAITRASRC/SRCPROTOPR,UBNUF2IDA
019600151012     D/COPY GAITRASRC/SRCPROTOPI,UBNUF2IDA
019700100922     D/COPY GAITRASRC/SRCPROTOPR,UBDNSLKP
019800100922     D/COPY GAITRASRC/SRCPROTOPI,UBDNSLKP
019900100922     D/COPY GAITRASRC/SRCPROTOPR,SOCKET
020000090710     D/COPY GAITRASRC/SRCPROTOPR,INET_ADDR
020100151014     D/COPY GAITRASRC/SRCPROTOPR,UBXLATENCD
020200080612
020300080612
020400071128      //********************************************************************************************
020500071128      //
020600080612      // Definizione variabili work (globali)
020700071128      //
020800071128      //********************************************************************************************
020900151126     D InitDone        S               N   INZ(*off)
021000151126     D
021100071129
021200151126
021300080612
021400080612     P*--------------------------------------------------
021500151008     P* Procedure name: UBSOCKET2_Create
021600080612     P* Purpose:        Inizializzazioni
021700090713     P* Returns:        Integer (Status_Code)
021800151126     P* Parameter:      pInSktType     => Tipo di Socket ('TCP', 'UDP')
021900090713     P* Parameter:      pOutSktDescr   => Descrittore Socket
022000080612     P*--------------------------------------------------
022100080612     P*
022200151008     P UBSOCKET2_Create...
022300080612     P                 B                   EXPORT
022400080612     D*
022500151008     D UBSOCKET2_Create...
022600080612     D                 PI            10I 0
022700151126     D pInSktType                    10A   CONST
022800151126     D pOutSktDescr                  10I 0
022900080612     D*
023000080612      //********************************************************************************************
023100080612      //
023200080612      // Definizione variabili work (locali)
023300080612      //
023400080612      //********************************************************************************************
023500080612     D locMethodCode   S             10I 0
023600090710     D
023700151126     D wInSocketType   S             10I 0 Inz
023800151126     D wInProtocol     S             10I 0 INz
023900080612
024000090710
024100080612      /FREE
024200080612        // Inizializzo opcode di metodo
024300080612        locMethodCode = -1;
024400080612        InitDone = *off;
024500151126
024600151126        // Verifico il tipo di socket (TCP o UDP) richiesto (default è TCP)
024700151126        select;
024800151126          when pInSktType = *blanks;
024900151126               wInSocketType = SOCK_STREAM;
025000151126               wInProtocol   = IPPROTO_TCP;
025100151126          when pInSktType = 'TCP';
025200151126               wInSocketType = SOCK_STREAM;
025300151126               wInProtocol   = IPPROTO_TCP;
025400151126          when pInSktType = 'UDP';
025500151126               wInSocketType = SOCK_DGRAM;
025600151126               wInProtocol   = IPPROTO_IP;
025700151126        endsl;
025800151126
025900090710        // Definisco un socket (tipo e modalità)
026000151126        pOutSktDescr = socket(AF_INET : wInSocketType : wInProtocol);
026100090713        if pOutSktDescr < 0;
026200090710        else;
026300090710
026400080616           // Valorizzo opcode di metodo
026500080616           locMethodCode = 0;
026600080616           InitDone = *on;
026700080616
026800080616        endif;
026900080612
027000080612        return locMethodCode;
027100080612
027200080612      /END-FREE
027300080612
027400151008     P UBSOCKET2_Create...
027500080612     P                 E
027600151126
027700151126
027800151126
027900151126     P*--------------------------------------------------
028000151126     P* Procedure name: UBSOCKET2_SetSockOption...
028100151126     P* Purpose:        Socket Options Settings
028200151126     P* Returns:        Integer (Status_Code)
028300151126     P* Parameter:      pInSktDescr    => Descrittore Socket
028400151126     P* Parameter:      pInSktLevel    => Livello Socket
028500151126     P* Parameter:      pInOptName     => Nome   Opzione da impostare
028600151126     P* Parameter:      pInOptValue    => Valore Opzione da impostare
028700151126     P*--------------------------------------------------
028800151126     P*
028900151126     P UBSOCKET2_SetSockOption...
029000151126     P                 B                   EXPORT
029100151126     D*
029200151126     D UBSOCKET2_SetSockOption...
029300151126     D                 PI            10I 0
029400151126     D pInSktDescr                   10I 0 CONST
029500151126     D pInSktLevel                   10I 0 VALUE
029600151126     D pInOptName                    10I 0 VALUE
029700151126     D pInOptValue                   10I 0 VALUE
029800151126     D*
029900151126      //********************************************************************************************
030000151126      //
030100151126      // Definizione variabili work (locali)
030200151126      //
030300151126      //********************************************************************************************
030400151126     D locMethodCode   S             10I 0
030500151126     D
030600151126     D wErr            S               N   Inz(*off)
030700151126
030800151126
030900151126      /FREE
031000151126        // Inizializzo opcode di metodo
031100151126        locMethodCode = -1;
031200151126        InitDone = *off;
031300151126
031400151126        // Forzo l'opzione solo se Nome Opzione è stato passato
031500151126        if pInOptName <> *zeros;
031600151126
031700151126           select;
031800151126             when pInOptName = SO_LINGER;
031900151126
032000151126                 // Alloco la memoria necessaria a memorizzare la struttura "linger"
032100151126                 addrlen = %size(linger);
032200151126                 p_linge = %alloc(addrlen);
032300151126
032400151126                 // Faccio puntare l'indirizzo della struttura del "linger" alla nuova
032500151126                 // area di memoria appena allocata
032600151126                 p_linger = p_linge;
032700151126
032800151126                 // Quindi imposto i Valori di Opzionr richiesti
032900151126                 l_onoff = 1;
033000151126                 l_linger = pInOptValue;
033100151126
033200151126                 // Forzo le Opzioni richieste sul Socket richiesto
033300151126                 if setSockOpt(pInSktDescr : pInSktLevel : pInOptName :
033400151126                               p_linge : %size(linger)) < 0;
033500151126
033600151126                    // Errore in forzatura Opzioni Socket
033700151126                    wErr = *on;
033800151126
033900151126                 endif;
034000151126
034100151126             other;
034200151126
034300151126                 // Forzo le Opzioni richieste sul Socket richiesto
034400151126                 if setSockOpt(pInSktDescr : pInSktLevel : pInOptName :
034500151126                               %addr(pInOptValue): %size(pInOptValue)) < 0;
034600151126
034700151126                    // Errore in forzatura Opzioni Socket
034800151126                    wErr = *on;
034900151126
035000151126                 endif;
035100151126
035200151126           endsl;
035300151126
035400151126        endif;
035500151126
035600151126        if not wErr;
035700151126
035800151126           // Valorizzo opcode di metodo
035900151126           locMethodCode = 0;
036000151126           InitDone = *on;
036100151126
036200151126        endif;
036300151126
036400151126        return locMethodCode;
036500151126
036600151126      /END-FREE
036700151126
036800151126     P UBSOCKET2_SetSockOption...
036900151126     P                 E
037000080612
037100080612
037200080612
037300080612     P*--------------------------------------------------
037400151008     P* Procedure name: UBSOCKET2_Garbage
037500100612     P* Purpose:        Garbage collection
037600090713     P* Returns:        Integer (Status_Code)
037700090713     P* Parameter:      pInSktDescr    => Descrittore Socket
037800080612     P*--------------------------------------------------
037900080612     P*
038000151008     P UBSOCKET2_Garbage...
038100080612     P                 B                   EXPORT
038200080612     D*
038300151008     D UBSOCKET2_Garbage...
038400080612     D                 PI            10I 0
038500151126     D pInSktDescr                   10I 0 CONST
038600080612     D*
038700080612      //********************************************************************************************
038800080612      //
038900080612      // Definizione variabili work (locali)
039000080612      //
039100080612      //********************************************************************************************
039200080612     D locMethodCode   S             10I 0
039300080612
039400080612      /FREE
039500080612        // Inizializzo opcode di metodo
039600080612        locMethodCode = -1;
039700080612
039800090710        // Chiudo il socket
039900090713        callp close(pInSktDescr);
040000080612
040100080612        // Valorizzo opcode di metodo
040200080612        locMethodCode = 0;
040300080612
040400080612        return locMethodCode;
040500080612
040600080612      /END-FREE
040700080612
040800151008     P UBSOCKET2_Garbage...
040900080612     P                 E
041000090710
041100090710
041200090710
041300090710     P*--------------------------------------------------
041400151008     P* Procedure name: UBSOCKET2_Connect
041500090710     P* Purpose:        Connect to a socket "server"
041600090710     P* Returns:        Integer (Status_Code)
041700090713     P* Parameter:      pInSktDescr    => Descrittore Socket
041800100922     P* Parameter:      pInHostAdr     => Host remoto
041900090710     P* Parameter:      pInPort        => Numero porta
042000090710     P*--------------------------------------------------
042100090710     P*
042200151008     P UBSOCKET2_Connect...
042300090710     P                 B                   EXPORT
042400090710     D*
042500151008     D UBSOCKET2_Connect...
042600090710     D                 PI            10I 0
042700151126     D pInSktDescr                   10I 0 CONST
042800100922     D pInHost                      256A   CONST
042900090710     D pInPort                        5U 0 CONST
043000090710     D*
043100090710      //********************************************************************************************
043200090710      //
043300090710      // Definizione variabili work (locali)
043400090710      //
043500090710      //********************************************************************************************
043600090710     D locMethodCode   S             10I 0
043700090710     D
043800100922     D IpAddr          S             15A   inz
043900090710
044000090710
044100090710      /FREE
044200090710        // Inizializzo opcode di metodo
044300090710        locMethodCode = -1;
044400090710        InitDone = *off;
044500090710
044600090710        // Alloco la memoria necessaria a memorizzare la struttura del socket corrente
044700090710        addrlen = %size(sockaddr);
044800090710        p_connto = %alloc(addrlen);
044900090710
045000090710        // Faccio puntare l'inririzzo della struttura del socket corrente alla nuova
045100090710        // area di memoria appena allocata
045200090710        p_sockaddr = p_connto;
045300100922
045400151008
045500151008        // Effettuo il DNS Lookup x reperire SEMPRE l'indirizzo "puntato"
045600151008        callp UBDNSLKP_Retrieve(pInHost : pOutInetFound : pOutInetType);
045700151008
045800151008          if pOutInetType = 'NAM';
045900151008             IpAddr = %trim(pOutInetFound);
046000151008          else;
046100151008             IpAddr = %trim(pInHost);
046200151008          endif;
046300100922
046400100922          // Converto l'indirizzo internet "puntato" nel relativo valore numerico unsigned
046500100922          if UBCVTIDA_Convert(IpAddr:pOutInetAdrVal) = 0;
046600090710
046700100922             // Popolo la struttura del socket
046800100922             // Note that IP is the ip address we previously looked up
046900100922             // using the inet_addr and/or gethostbyname APIs
047000100922             // and port is the port number that we looked up using the
047100100922             // getservbyname API.
047200100922             sin_family = AF_INET;
047300100922             sin_addr = pOutInetAdrVal;
047400100922             sin_port = pInPort;
047500151127             sin_zero = *ALLx'00';
047600100922
047700100922             // Mi connetto al socket "server"
047800100922             if connect(pInSktDescr : p_connto : addrlen) < 0;
047900100922             else;
048000090710
048100100922                // Valorizzo opcode di metodo
048200100922                locMethodCode = 0;
048300100922                InitDone = *on;
048400090710
048500100922             endif;
048600110225          endif;
048700090710
048800090710        return locMethodCode;
048900090710
049000090710      /END-FREE
049100090710
049200151008     P UBSOCKET2_Connect...
049300090710     P                 E
049400151009
049500151009
049600151009
049700151009     P*--------------------------------------------------
049800151009     P* Procedure name: UBSOCKET2_Bind
049900151009     P* Purpose:        Bind on a local address/port
050000151009     P* Returns:        Integer (Status_Code)
050100151009     P* Parameter:      pInSktDescr    => Descrittore Socket
050200151009     P* Parameter:      pInLocalAdr    => Local Address
050300151009     P* Parameter:      pInLocalPort   => Numero porta
050400151009     P*--------------------------------------------------
050500151009     P*
050600151009     P UBSOCKET2_Bind...
050700151009     P                 B                   EXPORT
050800151009     D*
050900151009     D UBSOCKET2_Bind...
051000151009     D                 PI            10I 0
051100151126     D pInSktDescr                   10I 0 CONST
051200151009     D pInLocalAdr                  256A   CONST
051300151009     D pInLocalPort                   5U 0 CONST
051400151009     D*
051500151009      //********************************************************************************************
051600151009      //
051700151009      // Definizione variabili work (locali)
051800151009      //
051900151009      //********************************************************************************************
052000151009     D locMethodCode   S             10I 0
052100151009     D
052200151009     D IpAddr          S             15A   inz
052300151009
052400151009
052500151009      /FREE
052600151009        // Inizializzo opcode di metodo
052700151009        locMethodCode = -1;
052800151009        InitDone = *off;
052900151009
053000151009        // Alloco la memoria necessaria a memorizzare la struttura del socket corrente
053100151009        addrlen = %size(sockaddr);
053200151009        p_bindto = %alloc(addrlen);
053300151009
053400151009        // Faccio puntare l'indirizzo della struttura del socket corrente alla nuova
053500151009        // area di memoria appena allocata
053600151009        p_sockaddr = p_bindto;
053700151009
053800151009        // Inizializzo il valore del socket address
053900151009        sin_addr = *hival;
054000151009
054100151009        // Gestisco eventuali valori speciali per l'indirizzo di binding
054200151009        select;
054300151009          when %trim(pInLocalAdr) = 'INADDR_ANY';
054400151009             sin_addr = INADDR_ANY;
054500151009
054600151009          when %trim(pInLocalAdr) = 'INADDR_LOOPBACK';
054700151009             sin_addr = INADDR_LOOPBACK;
054800151009
054900151009          other;
055000151009            // Effettuo il DNS Lookup x reperire SEMPRE l'indirizzo "puntato"
055100151009            callp UBDNSLKP_Retrieve(pInLocalAdr : pOutInetFound : pOutInetType);
055200151009
055300151009            if pOutInetType = 'NAM';
055400151009               IpAddr = %trim(pOutInetFound);
055500151009            else;
055600151009               IpAddr = %trim(pInLocalAdr);
055700151009            endif;
055800151009
055900151009            // Converto l'indirizzo internet "puntato" nel relativo valore numerico unsigned
056000151009            if UBCVTIDA_Convert(IpAddr:pOutInetAdrVal) = 0;
056100151009
056200151009               sin_addr = pOutInetAdrVal;
056300151009
056400151009            endif;
056500151009        endsl;
056600151009
056700151009
056800151009        // Se a questo punto il socket address è valorizzato => proseguo
056900151009        if sin_addr <> *hival;
057000151009
057100151009           // Popolo la struttura del socket
057200151009           sin_family = AF_INET;
057300151009           sin_port = pInLocalPort;
057400151127           sin_zero = *ALLx'00';
057500151009
057600151009           // Mi bindo (ovvero "preparo indirizzo/porta" per il server)
057700151009           if bind(pInSktDescr : p_bindto : addrlen) < 0;
057800151009           else;
057900151009
058000151009              // Valorizzo opcode di metodo
058100151009              locMethodCode = 0;
058200151009              InitDone = *on;
058300151009
058400151009           endif;
058500151009
058600151009        endif;
058700151009
058800151009        return locMethodCode;
058900151009
059000151009      /END-FREE
059100151009
059200151009     P UBSOCKET2_Bind...
059300151009     P                 E
059400151008
059500151008
059600151008
059700151008     P*--------------------------------------------------
059800151126     P* Procedure name: UBSOCKET2_Send (TCP)
059900151008     P* Purpose:        Send socket command
060000151008     P* Returns:        Integer (Status_Code)
060100151008     P* Parameter:      pInSktDescr    => Descrittore Socket
060200151126     P* Parameter:      pInSktCmd      => Comando da sottomettere al socket destinazione via TCP
060300151008     P* Parameter:      pInCmdCvtTbl   => Tabella conversione comando socket
060400151008     P*--------------------------------------------------
060500151008     P*
060600151008     P UBSOCKET2_Send...
060700151008     P                 B                   EXPORT
060800151008     D*
060900151008     D UBSOCKET2_Send...
061000151008     D                 PI            10I 0
061100151126     D pInSktDescr                   10I 0 CONST
061200151008     D pInSktCmd                   1024A   CONST
061300151125     D pInCmdCvtTbl                  10A                                        * QTCPASC QTCPEBC
061400151008     D*
061500151008      //********************************************************************************************
061600151008      //
061700151008      // Definizione variabili work (locali)
061800151008      //
061900151008      //********************************************************************************************
062000151008     D locMethodCode   S             10I 0
062100151008     D
062200151008     D Rcv             S             10I 0
062300151008     D SktCmd          S           1024A
062400151008     D SktCmdLenght    S             10I 0
062500151008
062600151008
062700151008      /FREE
062800151008        // Inizializzo opcode di metodo
062900151008        locMethodCode = -1;
063000151008        InitDone = *off;
063100151008
063200151008        // Verifica parametri "tabellati"
063300151008        if pInCmdCvtTbl = *blanks;
063400151008           pInCmdCvtTbl = 'QTCPASC';
063500151008        endif;
063600151008
063700151126        // Sottometto il comando al socket destinazione
063800151008        SktCmd = %trim(pInSktCmd);
063900151008        SktCmdLenght = %len(%trim(SktCmd));
064000151014        callp UBXLATENCD_Translate(SktCmdLenght : SktCmd : pInCmdCvtTbl);
064100151008        Rcv = send(pInSktDescr : %addr(SktCmd) : SktCmdLenght  : 0);
064200151126        if  Rcv < SktCmdLenght;
064300151126        else;
064400151008
064500151008           // Valorizzo opcode di metodo
064600151008           locMethodCode = 0;
064700151008           InitDone = *on;
064800151008
064900151008        endif;
065000151008
065100151008        return locMethodCode;
065200151008
065300151008      /END-FREE
065400151008
065500151008     P UBSOCKET2_Send...
065600151008     P                 E
065700151126
065800151126
065900151126
066000151126     P*--------------------------------------------------
066100151126     P* Procedure name: UBSOCKET2_SendTo (UDP)
066200151126     P* Purpose:        Send socket command
066300151126     P* Returns:        Integer (Status_Code)
066400151126     P* Parameter:      pInSktDescr    => Descrittore Socket
066500151126     P* Parameter:      pInSktCmd      => Comando da sottomettere al socket destinazione via UDP
066600151126     P* Parameter:      pInCmdCvtTbl   => Tabella conversione comando socket
066700151126     P* Parameter:      pInHostAdr     => Host remoto
066800151126     P* Parameter:      pInPort        => Numero porta
066900151126     P*--------------------------------------------------
067000151126     P*
067100151126     P UBSOCKET2_SendTo...
067200151126     P                 B                   EXPORT
067300151126     D*
067400151126     D UBSOCKET2_SendTo...
067500151126     D                 PI            10I 0
067600151126     D pInSktDescr                   10I 0 CONST
067700151126     D pInSktCmd                   1024A   CONST
067800151126     D pInCmdCvtTbl                  10A                                        * QTCPASC QTCPEBC
067900151126     D pInHost                      256A   CONST
068000151126     D pInPort                        5U 0 CONST
068100151126     D*
068200151126      //********************************************************************************************
068300151126      //
068400151126      // Definizione variabili work (locali)
068500151126      //
068600151126      //********************************************************************************************
068700151126     D locMethodCode   S             10I 0
068800151126     D
068900151126     D Rcv             S             10I 0
069000151126     D SktCmd          S           1024A
069100151126     D SktCmdLenght    S             10I 0
069200151126     D
069300151126     D IpAddr          S             15A   inz
069400151126
069500151126
069600151126      /FREE
069700151126        // Inizializzo opcode di metodo
069800151126        locMethodCode = -1;
069900151126        InitDone = *off;
070000151126
070100151126        // Alloco la memoria necessaria a memorizzare la struttura del socket corrente
070200151126        addrlen = %size(sockaddr);
070300151126        p_connto = %alloc(addrlen);
070400151126
070500151126        // Faccio puntare l'inririzzo della struttura del socket corrente alla nuova
070600151126        // area di memoria appena allocata
070700151126        p_sockaddr = p_connto;
070800151126
070900151126
071000151126        // Effettuo il DNS Lookup x reperire SEMPRE l'indirizzo "puntato"
071100151126        callp UBDNSLKP_Retrieve(pInHost : pOutInetFound : pOutInetType);
071200151126
071300151126        if pOutInetType = 'NAM';
071400151126           IpAddr = %trim(pOutInetFound);
071500151126        else;
071600151126           IpAddr = %trim(pInHost);
071700151126        endif;
071800151126
071900151126        // Converto l'indirizzo internet "puntato" nel relativo valore numerico unsigned
072000151126        if UBCVTIDA_Convert(IpAddr:pOutInetAdrVal) = 0;
072100151126
072200151126           // Popolo la struttura del socket
072300151126           // Note that IP is the ip address we previously looked up
072400151126           // using the inet_addr and/or gethostbyname APIs
072500151126           // and port is the port number that we looked up using the
072600151126           // getservbyname API.
072700151126           sin_family = AF_INET;
072800151126           sin_addr = pOutInetAdrVal;
072900151126           sin_port = pInPort;
073000151127           sin_zero = *ALLx'00';
073100151126
073200151126           // Verifica parametri "tabellati"
073300151126           if pInCmdCvtTbl = *blanks;
073400151126              pInCmdCvtTbl = 'QTCPASC';
073500151126           endif;
073600151126
073700151126           // Sottometto il comando al socket "server"
073800151126           SktCmd = %trim(pInSktCmd);
073900151126           SktCmdLenght = %len(%trim(SktCmd));
074000151126           callp UBXLATENCD_Translate(SktCmdLenght : SktCmd : pInCmdCvtTbl);
074100151126           Rcv = sendto(pInSktDescr : %addr(SktCmd) : SktCmdLenght  : 0
074200151126                                    : p_connto : addrlen);
074300151126           if  Rcv < SktCmdLenght;
074400151126           else;
074500151126
074600151126              // Valorizzo opcode di metodo
074700151126              locMethodCode = 0;
074800151126              InitDone = *on;
074900151126
075000151126           endif;
075100151126
075200151126        endif;
075300151126
075400151126        return locMethodCode;
075500151126
075600151126      /END-FREE
075700151126
075800151126     P UBSOCKET2_SendTo...
075900151126     P                 E
076000151013
076100151013
076200151013
076300151014     P*--------------------------------------------------
076400151126     P* Procedure name: UBSOCKET2_Receive (TCP)
076500151013     P* Purpose:        Read socket response
076600151013     P* Returns:        Integer (Status_Code)
076700151013     P* Parameter:      pInSktDescr    => Descrittore Socket
076800151126     P* Parameter:      pInBufferLen   => Lunghezza del buffer di lettura messaggio socket clientTCP
076900151013     P* Parameter:      pInCmdCvtTbl   => Codice tabella conversione
077000151013     P* Parameter:      pOutResponse   => Contenuto del messaggio socket
077100151013     P* Parameter:      pOutRespLen    => Lunghezza del messaggio socket
077200151013     P*--------------------------------------------------
077300151013     D*
077400151014     P UBSOCKET2_Receive...
077500151013     P                 B                   EXPORT
077600151013     D*
077700151014     D UBSOCKET2_Receive...
077800151013     D                 PI            10I 0
077900151126     D pInSktDescr                   10I 0 CONST
078000151126     D pInBufferLen                  10I 0 CONST
078100151013     D pInTypEOL                      1A   CONST
078200151125     D pInCmdCvtTbl                  10A                                        * QTCPASC QTCPEBC
078300151014     D pOutRespPtr                     *
078400151126     D pOutRespLen                   10I 0
078500151013     D*
078600151013      //********************************************************************************************
078700151013      //
078800151013      // Definizione variabili work (locali)
078900151013      //
079000151013      //********************************************************************************************
079100151013     D locMethodCode   S             10I 0
079200151013     D
079300151126     D Rcv             S             10I 0 Inz
079400151014     D wResponse       S          65536A   VARYING Based(pOutRespPtr)
079500151014     D wResponseLen    S             10I 0 Inz
079600151014     D wXlate          S          65536A   Inz
079700151013     D wChar           S              1A   Inz
079800151013     D wCR             S              1A   Inz(x'0D')
079900151013     D wLF             S              1A   Inz(x'0A')
080000151014     D wEOL            S              2A   Inz Varying
080100151013
080200151013
080300151013      /FREE
080400151013        // Inizializzo opcode di metodo
080500151013        locMethodCode = -1;
080600151013        InitDone = *off;
080700151014
080800151014
080900151013        // Verifica parametri "tabellati" relativi alla conversione del encoding dati
081000151013        if pInCmdCvtTbl = *blanks;
081100151013           pInCmdCvtTbl = 'QTCPEBC';
081200151013        endif;
081300151014
081400151013
081500151013        // Verifica parametri "tabellati" relativi alla conversione del encoding dati
081600151013        select;
081700151013          when pInTypEOL = '1';
081800151013               wEOL = wCR;
081900151013          when pInTypEOL = '2';
082000151014               wEOL = wLF;
082100151013          when pInTypEOL = '3';
082200151014               wEOL = wCR+wLF;
082300151013          other;
082400151014               wEOL = wCR+wLF;
082500151013        endsl;
082600151013
082700151013
082800151013        // Ricevo il messaggio byte-a-byte
082900151013        dou Rcv < 1;
083000151013
083100151013            // Inizializzo variabili di wrk
083200151014            wResponse    = %trim(' ');
083300151013            wResponseLen = *zeros;
083400151014            wXlate       = *blanks;
083500151014            wChar        = *blanks;
083600151013
083700151014            dou wResponseLen = %size(pInBufferLen) OR
083800151014                %scan(wChar : wEOL) > *zeros;
083900151014
084000151013                Rcv = recv(pInSktDescr : %addr(wChar) : 1 : 0);
084100151013                if Rcv < 1;
084200151013                   leave;
084300151013                endif;
084400151013
084500151014                if %scan(wChar : wEOL) = *zeros;
084600151013                   wResponseLen = wResponseLen + 1;
084700151014                   %subst(wResponse : wResponseLen : 1) = wChar;
084800151013                endif;
084900151013
085000151013            enddo;
085100151013
085200151013        enddo;
085300151013
085400151014
085500151013        if wResponseLen > *zeros;
085600151014
085700151013           if pInCmdCvtTbl <> '*NONE';
085800151014              wXlate = wResponse;
085900151014              callp UBXLATENCD_Translate(wResponseLen : wXlate : pInCmdCvtTbl);
086000151014              wResponse = wXlate;
086100151014           endif;
086200151014
086300151014           // Valorizzo opcode di metodo
086400151014           pOutRespPtr = %addr(wResponse);
086500151014           pOutRespLen = wResponseLen;
086600151014
086700151014           // Valorizzo opcode di metodo
086800151014           locMethodCode = 0;
086900151014           InitDone = *on;
087000151014
087100151013        endif;
087200151013
087300151013
087400151013        return locMethodCode;
087500151013
087600151013      /END-FREE
087700151013
087800151014     P UBSOCKET2_Receive...
087900151013     P                 E
088000151126
088100151126
088200151126
088300151126     P*--------------------------------------------------
088400151126     P* Procedure name: UBSOCKET2_ReceiveFrom (UDP)
088500151126     P* Purpose:        Read socket response
088600151126     P* Returns:        Integer (Status_Code)
088700151126     P* Parameter:      pInSktDescr    => Descrittore Socket
088800151126     P* Parameter:      pInBufferLen   => Lunghezza del buffer di lettura messaggio socket clientUDP
088900151126     P* Parameter:      pInCmdCvtTbl   => Codice tabella conversione
089000151126     P* Parameter:      pOutRespLen    => Lunghezza del messaggio socket
089100151212     P* Parameter:      pOutRespTxt    => Contenuto del messaggio socket (lunghezza variabile)
089200151126     P*--------------------------------------------------
089300151126     D*
089400151126     P UBSOCKET2_ReceiveFrom...
089500151126     P                 B                   EXPORT
089600151126     D*
089700151126     D UBSOCKET2_ReceiveFrom...
089800151126     D                 PI            10I 0
089900151126     D pInSktDescr                   10I 0 CONST
090000151212 xxx D pInBufferLen                  10I 0 CONST
090100151126     D pInTypEOL                      1A   CONST
090200151126     D pInCmdCvtTbl                  10A                                        * QTCPASC QTCPEBC
090300151126     D pOutRespLen                   10I 0
090400151212     D pOutRespTxt                32768A   OPTIONS(*VARSIZE)
090500151126     D*
090600151126      //********************************************************************************************
090700151126      //
090800151126      // Definizione variabili work (locali)
090900151126      //
091000151126      //********************************************************************************************
091100151126     D locMethodCode   S             10I 0
091200151126     D
091300151212     D wResponse       S           1024A   Inz
091400151126     D wXlate          S          65536A   Inz
091500151126     D wChar           S              1A   Inz
091600151203     D wCharPrev       S              1A   Inz
091700151212
091800151212     D RcvDataLen      S             10I 0 Inz
091900151126     D wCR             S              1A   Inz(x'0D')
092000151126     D wLF             S              1A   Inz(x'0A')
092100151126     D wEOL            S              2A   Inz Varying
092200151203     D wTermLen        S              1S 0 Inz
092300151126
092400151203
092500151212 ccc  /FREE
092600151126        // Inizializzo opcode di metodo
092700151126        locMethodCode = -1;
092800151126        InitDone = *off;
092900151203
093000151212
093100151212        // Inizializzo variabili di work (fondamentali)
093200151212        clear wResponse;
093300151212
093400151203
093500151203        // Verifica parametro di input relativo a lunghezza max buffer da ricevere
093600151212 xxx    if pInBufferLen > 32768;
093700151203           return locMethodCode;
093800151203        endif;
093900151126
094000151126
094100151126        // Verifica parametri "tabellati" relativi alla conversione del encoding dati
094200151126        if pInCmdCvtTbl = *blanks;
094300151126           pInCmdCvtTbl = 'QTCPEBC';
094400151126        endif;
094500151126
094600151126
094700151126        // Verifica parametri "tabellati" relativi alla conversione del encoding dati
094800151126        select;
094900151126          when pInTypEOL = '1';
095000151126               wEOL = wCR;
095100151203               wTermLen = 1;
095200151126          when pInTypEOL = '2';
095300151126               wEOL = wLF;
095400151203               wTermLen = 1;
095500151126          when pInTypEOL = '3';
095600151126               wEOL = wCR+wLF;
095700151203               wTermLen = 2;
095800151126          other;
095900151126               wEOL = wCR+wLF;
096000151203               wTermLen = 2;
096100151126        endsl;
096200151212
096300151212
096400151212        // Alloco la memoria necessaria a memorizzare la struttura del socket corrente
096500151212        addrlen = %size(sockaddr);
096600151212        p_connfrom = %alloc(addrlen);
096700151212
096800151212
096900151212        // Ricevo il messaggio per intiero
097000151212        RcvDataLen = recvfrom(pInSktDescr : %addr(wResponse) :
097100151212                     %size(wResponse) : 0 : p_connfrom : addrlen);
097200151212
097300151212
097400151212        // Se nessun dato ricevuto => esco
097500151212        if RcvDataLen < 1;
097600151212 xxx    //   leave;
097700151212
097800151212        else;
097900151212
098000151212           // Se ricevuti dati ... se richiesto converto encoding
098100151212           if pInCmdCvtTbl <> '*NONE';
098200151212              wXlate = wResponse;
098300151212              callp UBXLATENCD_Translate(RcvDataLen : wXlate : pInCmdCvtTbl);
098400151212              wResponse = wXlate;
098500151212           endif;
098600151212
098700151212           // Verifico se messaggio ricevuto è completo (ovvero include terminatori
098800151212           // (ovvero include terminatori di acknowledgement)
098900151212           //if %trim(wCharPrev + wChar) = %trim(wEOL);
099000151212           //else;
099100151212           //   wResponseLen = wResponseLen + 1;
099200151212           //   %subst(wResponse : wResponseLen : 1) = wChar;
099300151212           //endif;
099400151212
099500151212        endif;
099600151212
099700151212
099800151212
099900151126        // Ricevo il messaggio byte-a-byte
100000151212        //dou  Rcv < 1;
100100151126
100200151126            // Inizializzo variabili di wrk
100300151212        //    wResponse    = %trim(' ');
100400151212        //    wResponseLen = *zeros;
100500151212        //    wXlate       = *blanks;
100600151212        //    wChar        = *blanks;
100700151212        //    wCharPrev    = *blanks;
100800151126
100900151212        //    dou wResponseLen = %size(pInBufferLen) OR
101000151212        //        %trim(wCharPrev + wChar) = %trim(wEOL);
101100151126
101200151212        //        wCharPrev = wChar;
101300151212        //        Rcv = recvfrom(pInSktDescr : %addr(wChar) : 1 : 0
101400151212        //                                 : p_connto : addrlen);
101500151212        //        if Rcv < 1;
101600151212        //           leave;
101700151212        //        endif;
101800151126
101900151212        //        if %trim(wCharPrev + wChar) = %trim(wEOL);
102000151212        //        else;
102100151212        //           wResponseLen = wResponseLen + 1;
102200151212        //           %subst(wResponse : wResponseLen : 1) = wChar;
102300151212        //        endif;
102400151126
102500151212        //    enddo;
102600151126
102700151212        //enddo;
102800151126
102900151126
103000151212        if RcvDataLen > *zeros;
103100151126
103200151126           // Valorizzo opcode di metodo
103300151203           pOutRespTxt = wResponse;
103400151212           pOutRespLen = RcvDataLen;
103500151126
103600151126           // Valorizzo opcode di metodo
103700151126           locMethodCode = 0;
103800151126           InitDone = *on;
103900151126
104000151126        endif;
104100151126
104200151126
104300151126        return locMethodCode;
104400151126
104500151126      /END-FREE
104600151126
104700151126     P UBSOCKET2_ReceiveFrom...
104800151126     P                 E
104900151009
105000151009
105100151009
105200151009     P*--------------------------------------------------
105300151009     P* Procedure name: UBSOCKET2_Listen
105400151009     P* Purpose:        Create socket server
105500151009     P* Returns:        Integer (Status_Code)
105600151009     P* Parameter:      pInSktDescr    => Descrittore Socket
105700151009     P* Parameter:      pInBackLog     => Max client servibili (contemporaneamente)
105800151009     P*--------------------------------------------------
105900151009     P*
106000151009     P UBSOCKET2_Listen...
106100151009     P                 B                   EXPORT
106200151009     D*
106300151009     D UBSOCKET2_Listen...
106400151009     D                 PI            10I 0
106500151126     D pInSktDescr                   10I 0 CONST
106600151009     D pInBackLog                     5U 0 CONST
106700151009     D*
106800151009      //********************************************************************************************
106900151009      //
107000151009      // Definizione variabili work (locali)
107100151009      //
107200151009      //********************************************************************************************
107300151009     D locMethodCode   S             10I 0
107400151009     D
107500151012     D BackLog         S              5U 0 INZ(1024)
107600151009
107700151009
107800151009      /FREE
107900151009        // Inizializzo opcode di metodo
108000151009        locMethodCode = -1;
108100151009        InitDone = *off;
108200151009
108300151012        // Se non indicato un valore specifico per il back-log => assumo 1024
108400151009        if pInBackLog > *zeros;
108500151009           BackLog = pInBackLog;
108600151009        endif;
108700151009
108800151009        // Mi metto in ascolto (listening) sul socket server
108900151009        if listen(pInSktDescr : BackLog) < 0;
109000151009        else;
109100151009
109200151009           // Valorizzo opcode di metodo
109300151009           locMethodCode = 0;
109400151009           InitDone = *on;
109500151009
109600151009        endif;
109700151009
109800151009        return locMethodCode;
109900151009
110000151009      /END-FREE
110100151009
110200151009     P UBSOCKET2_Listen...
110300151009     P                 E
110400151009
110500151009
110600151009
110700151009     P*--------------------------------------------------
110800151126     P* Procedure name: UBSOCKET2_Accept
110900151009     P* Purpose:        Accept a socket client request
111000151009     P* Returns:        Integer (Status_Code)
111100151014     P* Parameter:      pInSktDescr    => Descrittore Socket Server
111200151014     P* Parameter:      pOutSktDescr   => Descrittore Socket Client
111300151014     P* Parameter:      pOutSktAddr    => Indirizzo IP "dottato" del client
111400151009     P*--------------------------------------------------
111500151009     P*
111600151009     P UBSOCKET2_Accept...
111700151009     P                 B                   EXPORT
111800151009     D*
111900151009     D UBSOCKET2_Accept...
112000151009     D                 PI            10I 0
112100151126     D pInSktDescr                   10I 0 CONST
112200151126     D pOutSktDescr                  10I 0
112300151125     D pOutSktAddr                   15A
112400151009     D*
112500151009      //********************************************************************************************
112600151009      //
112700151009      // Definizione variabili work (locali)
112800151009      //
112900151009      //********************************************************************************************
113000151009     D locMethodCode   S             10I 0
113100151009     D
113200151009
113300151009
113400151009      /FREE
113500151009        // Inizializzo opcode di metodo
113600151009        locMethodCode = -1;
113700151009        InitDone = *off;
113800151009
113900151009        // Alloco la memoria necessaria a memorizzare la struttura del socket corrente
114000151009        addrlen = %size(sockaddr);
114100151009        p_connfrom = %alloc(addrlen);
114200151009
114300151009        // Servo la richiesta cliente "estraendola" dallo stack del server
114400151009        pOutSktDescr = accept(pInSktDescr : p_connfrom : addrlen);
114500151009
114600151009        // Se nuovo socket creato correttamente (Accept riuscita)
114700151009        if pOutSktDescr < *zeros;
114800151009        else;
114900151009           // Lunghezza "illegale" connessione TCP
115000151009           if addrlen <> 16;
115100151009           else;
115200151125
115300151125              p_sockaddr = p_connfrom;
115400151009
115500151012              if UBNUF2IDA_Convert(sin_addr:pOutInetDotAdr) = 0;
115600151012
115700151012                 // Valorizzo i parametri di output
115800151125                 pOutSktAddr = pOutInetDotAdr;
115900151012
116000151012                 // Valorizzo opcode di metodo
116100151012                 locMethodCode = 0;
116200151012                 InitDone = *on;
116300151126
116400151012              endif;
116500151009           endif;
116600151009        endif;
116700151009
116800151009
116900151009        return locMethodCode;
117000151009
117100151009      /END-FREE
117200151009
117300151009     P UBSOCKET2_Accept...
117400151009     P                 E
117500151127
117600151127
117700151127
117800151127     P*--------------------------------------------------
117900151127     P* Procedure name: UBSOCKET2_MngDescrAttrbs
118000151127     P* Purpose:        Manage Descriptors Attributes
118100151127     P* Returns:        Integer (Status_Code)
118200151127     P* Parameter:      pInSktDescr    => Descrittore Socket di cui gestire gli attributi
118300151127     P* Parameter:      pInCommand     => Comando da eseguire (metodo)
118400151127     P* Parameter:      pInCmdArg      => Argomento del comando (valore)
118500151127     P*--------------------------------------------------
118600151127     P*
118700151127     P UBSOCKET2_MngDescrAttrbs...
118800151127     P                 B                   EXPORT
118900151127     D*
119000151127     D UBSOCKET2_MngDescrAttrbs...
119100151127     D                 PI            10I 0
119200151127     D pInSktDescr                   10I 0 CONST
119300151127     D pInCommand                    10I 0 CONST
119400151127     D pInCmdArg                     10I 0 CONST
119500151127     D*
119600151127      //********************************************************************************************
119700151127      //
119800151127      // Definizione variabili work (locali)
119900151127      //
120000151127      //********************************************************************************************
120100151127     D locMethodCode   S             10I 0
120200151127     D
120300151127     D wErr            S               N   Inz(*off)
120400151127
120500151127
120600151127      /FREE
120700151127        // Inizializzo opcode di metodo
120800151127        locMethodCode = -1;
120900151127        InitDone = *off;
121000151127
121100151127        // A seconda del metodo richiesto
121200151127        // gestisco l'attributo richiesto per il socket richiesto
121300151127        select;
121400151127           when pInCommand = F_GETFL;
121500151127              if fcntl(pInSktDescr : F_GETFL) < 0;
121600151127
121700151127                 // Errore in gestione attributi descrittore socket
121800151127                 wErr = *on;
121900151127
122000151127               endif;
122100151127
122200151127           when pInCommand = F_SETFL;
122300151127              if fcntl(pInSktDescr : F_SETFL : pInCmdArg) < 0;
122400151127
122500151127                 // Errore in gestione attributi descrittore socket
122600151127                 wErr = *on;
122700151127
122800151127               endif;
122900151127
123000151127           other;
123100151127
123200151127              // Errore in passaggio parametri
123300151127              wErr = *on;
123400151127
123500151127        endsl;
123600151127
123700151127
123800151127        if not wErr;
123900151127
124000151127           // Valorizzo opcode di metodo
124100151127           locMethodCode = 0;
124200151127           InitDone = *on;
124300151127
124400151127        endif;
124500151127
124600151127
124700151127        return locMethodCode;
124800151127
124900151127      /END-FREE
125000151127
125100151127     P UBSOCKET2_MngDescrAttrbs...
125200151127     P                 E
125300151127
125400151127
125500151127
125600151127     P*--------------------------------------------------
125700151127     P* Procedure name: UBSOCKET2_Select
125800151127     P* Purpose:        Select socket descriptor (to manage)
125900151127     P* Returns:        Integer (Status_Code)
126000151127     P* Parameter:      pInMaxDescr    => Numero massimo Descrittori Socket "gestibili"
126100151127     P* Parameter:      pInReadSet     => Set di Descrittori Socket da "leggere"
126200151127     P* Parameter:      pInWriteSet    => Set di Descrittori Socket da "scrivere"
126300151127     P* Parameter:      pInExcpSet     => Set di Descrittori Socket con "eccezioni"
126400151127     P* Parameter:      pInWaitTime    => Intervallo di "polling" tra Descrittori Socket
126500151127     P*--------------------------------------------------
126600151127     P*
126700151127     P UBSOCKET2_Select...
126800151127     P                 B                   EXPORT
126900151127     D*
127000151127     D UBSOCKET2_Select...
127100151127     D                 PI            10I 0
127200151127     D pInMaxDescr                   10I 0 CONST
127300151202     D pInReadSet                    28A   VALUE
127400151202     D pInWriteSet                   28A   VALUE
127500151202     D pInExcpSet                    28A   VALUE
127600151202     D pInWaitTime                   10I 0 VALUE
127700151127     D*
127800151127      //********************************************************************************************
127900151127      //
128000151127      // Definizione variabili work (locali)
128100151127      //
128200151127      //********************************************************************************************
128300151127     D locMethodCode   S             10I 0
128400151127     D
128500151127
128600151127
128700151127      /FREE
128800151127        // Inizializzo opcode di metodo
128900151127        locMethodCode = -1;
129000151127        InitDone = *off;
129100151127
129200151127        // Alloco la memoria necessaria a memorizzare la struttura del timeval
129300151127        addrlen = %size(timeval);
129400151127        p_tv = %alloc(addrlen);
129500151127        p_timeval = p_tv;
129600151127
129700151127        tv_sec = %div(pInWaitTime:1000000);
129800151127        tv_usec = %rem(pInWaitTime:1000000);
129900151127
130000151127
130100151127        if select(pInMaxDescr+1 : %addr(pInReadSet) :
130200151127                  %addr(pInWriteSet) : %addr(pInExcpSet): p_timeval) < 0;
130300151127        else;
130400151127
130500151127           // Valorizzo opcode di metodo
130600151127           locMethodCode = 0;
130700151127           InitDone = *on;
130800151127
130900151127        endif;
131000151127
131100151127        return locMethodCode;
131200151127
131300151127      /END-FREE
131400151127
131500151127     P UBSOCKET2_Select...
131600151127     P                 E
131700151127
131800151127
131900151127
132000151127     P*--------------------------------------------------
132100151127     P* Procedure name: UBSOCKET2_FD_zero
132200151127     P* Purpose:        Clear all Descriptors in a set
132300151127     P* Returns:        ---
132400151127     P* Parameter:      pInFDset       => Descriptors' set
132500151127     P*--------------------------------------------------
132600151127     P*
132700151127     P UBSOCKET2_FD_zero...
132800151127     P                 B                   EXPORT
132900151127     D*
133000151127     D UBSOCKET2_FD_zero...
133100151127     D                 PI
133200151127     D pInFDset                      28A
133300151127     D*
133400151127      //********************************************************************************************
133500151127      //
133600151127      // Definizione variabili work (locali)
133700151127      //
133800151127      //********************************************************************************************
133900151127     D
134000151127
134100151127      /FREE
134200151127
134300151127        // Inizializzo il Set dei Descripts ricevuto in input
134400151127        pInFDset = *ALLx'00';
134500151127
134600151127      /END-FREE
134700151127
134800151127     P UBSOCKET2_FD_zero...
134900151127     P                 E
135000151127
135100151127
135200151127
135300151127     P*--------------------------------------------------
135400151127     P* Procedure name: UBSOCKET2_FD_set
135500151127     P* Purpose:        Set Descriptors in a set
135600151127     P* Returns:        ---
135700151127     P* Parameter:      pInFD          => Descriptors
135800151127     P* Parameter:      pInFDset       => Descriptors' set
135900151127     P*--------------------------------------------------
136000151127     P*
136100151127     P UBSOCKET2_FD_set...
136200151127     P                 B                   EXPORT
136300151127     D*
136400151127     D UBSOCKET2_FD_set...
136500151127     D                 PI
136600151127     D pInFD                         10I 0
136700151127     D pInFDset                      28A
136800151127     D*
136900151127      //********************************************************************************************
137000151127      //
137100151127      // Definizione variabili work (locali)
137200151127      //
137300151127      //********************************************************************************************
137400151127     D
137500151127     D wkByteNo        S              5I 0
137600151127     D wkMask          S              1A
137700151127     D wkByte          S              1A
137800151127     C*
137900151127     C                   callp     UBSOCKET2_CalcBitPos(pInFD:wkByteNo:wkMask)
138000151127     C                   eval      wkByte = %subst(pInFDset:wkByteNo:1)
138100151127     C                   biton     wkMask        wkByte
138200151127     C                   eval      %subst(pInFDset:wkByteNo:1) = wkByte
138300151127     C*
138400151127     P UBSOCKET2_FD_set...
138500151127     P                 E
138600151127
138700151127
138800151127
138900151127     P*--------------------------------------------------
139000151127     P* Procedure name: UBSOCKET2_FD_clr
139100151127     P* Purpose:        Clear Descriptors in a set
139200151127     P* Returns:        ---
139300151127     P* Parameter:      pInFD          => Descriptors
139400151127     P* Parameter:      pInFDset       => Descriptors' set
139500151127     P*--------------------------------------------------
139600151127     P*
139700151127     P UBSOCKET2_FD_clr...
139800151127     P                 B                   EXPORT
139900151127     D*
140000151127     D UBSOCKET2_FD_clr...
140100151127     D                 PI
140200151127     D pInFD                         10I 0
140300151127     D pInFDset                      28A
140400151127     D*
140500151127      //********************************************************************************************
140600151127      //
140700151127      // Definizione variabili work (locali)
140800151127      //
140900151127      //********************************************************************************************
141000151127     D
141100151127     D wkByteNo        S              5I 0
141200151127     D wkMask          S              1A
141300151127     D wkByte          S              1A
141400151127     C*
141500151127     C                   callp     UBSOCKET2_CalcBitPos(pInFD:wkByteNo:wkMask)
141600151127     C                   eval      wkByte = %subst(pInFDset:wkByteNo:1)
141700151127     C                   bitoff    wkMask        wkByte
141800151127     C                   eval      %subst(pInFDset:wkByteNo:1) = wkByte
141900151127     C*
142000151127     P UBSOCKET2_FD_clr...
142100151127     P                 E
142200151127
142300151127
142400151127
142500151127     P*--------------------------------------------------
142600151127     P* Procedure name: UBSOCKET2_FD_isSet
142700151127     P* Purpose:        Check if a Descriptor Is Set (or not)
142800151127     P* Returns:        Boolean (indicator *on/*off)
142900151127     P* Parameter:      pInFD          => Descriptors
143000151127     P* Parameter:      pInFDset       => Descriptors' set
143100151127     P*--------------------------------------------------
143200151127     P*
143300151127     P UBSOCKET2_FD_isSet...
143400151127     P                 B                   EXPORT
143500151127     D*
143600151127     D UBSOCKET2_FD_isSet...
143700151127     D                 PI             1N
143800151127     D pInFD                         10I 0
143900151127     D pInFDset                      28A
144000151127     D*
144100151127      //********************************************************************************************
144200151127      //
144300151127      // Definizione variabili work (locali)
144400151127      //
144500151127      //********************************************************************************************
144600151127     D
144700151127     D wkByteNo        S              5I 0
144800151127     D wkMask          S              1A
144900151127     D wkByte          S              1A
145000151127     C*
145100151127     C                   callp     UBSOCKET2_CalcBitPos(pInFD:wkByteNo:wkMask)
145200151127     C                   eval      wkByte = %subst(pInFDset:wkByteNo:1)
145300151127     C                   testb     wkMask        wkByte                   88
145400151127     C                   return    *IN88
145500151127     C*
145600151127     P UBSOCKET2_FD_isSet...
145700151127     P                 E
145800151127
145900151127
146000151127
146100151127     P*--------------------------------------------------
146200151127     P* Procedure name: UBSOCKET2_CalcBitPos...
146300151127     P* Purpose:        Calculate Descriptor BitMask
146400151127     P* Returns:        ---
146500151127     P* Parameter:      pInSktDescr    => Descriptor
146600151127     P* Parameter:      pInByteNo      => Byte's number
146700151127     P* Parameter:      pInBitMask     => Byte's BitMask
146800151127     P*--------------------------------------------------
146900151127     P*
147000151127     P UBSOCKET2_CalcBitPos...
147100151127     P                 B                   EXPORT
147200151127     D*
147300151127     D UBSOCKET2_CalcBitPos...
147400151127     D                 PI
147500151127     D pInSktDescr                   10I 0
147600151127     D pInByteNo                      5I 0
147700151127     D pInBitMask                     1A
147800151127     D*
147900151127      //********************************************************************************************
148000151127      //
148100151127      // Definizione variabili work (locali)
148200151127      //
148300151127      //********************************************************************************************
148400151127     D dsMakeMask      DS
148500151127     D   dsZeroByte            1      1A
148600151127     D   dsMask                2      2A
148700151127     D   dsBitMult             1      2U 0 Inz(0)
148800151127     D
148900151127     D wkGroup         S              2  0 Inz
149000151127     D wkByteNo        S              2  0 Inz
149100151127     D wkBitNo         S              2  0 Inz
149200151127     C*
149300151127     C     pInSktDescr   DIV       32            wkGroup
149400151127     C                   MVR                     wkByteNo
149500151127     C                   DIV       8             wkByteNo
149600151127     C                   MVR                     wkBitNo
149700151127     C*
149800151127     C                   eval      wkByteNo = 4 - wkByteNo
149900151127     C                   eval      pInByteNo = (wkGroup * 4) + wkByteNo
150000151127     C                   eval      dsBitMult = 2 ** wkBitNo
150100151127     C                   eval      dsZeroByte = x'00'
150200151127     C                   eval      pInBitMask = dsMask
150300151127     C*
150400151127     P UBSOCKET2_CalcBitPos...
150500151127     P                 E
