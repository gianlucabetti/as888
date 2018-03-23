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
010400160404     ** PARAMETRI DELLA SOTTOPROCEDURA UBSOCKET2_SendTo (UDP)
010500151126     **
010600151126     ** Posizione Uso Tipo      Descrizione
010700151126     ** --------- --- --------- ------------------------------
010800160404     ** 1         IN  UNSIGNED  Descrittore Socket
010900160404     ** 2         IN  INTEGER   Lunghezza buffer risposta messaggio al socket client UDP
011000160404     ** 3         IN  CHARACTER Messaggio da sottomettere al socket destinazione via UDP
011100160404     ** 4         IN  CHARACTER Tabella conversione messaggio socket
011200160404     ** 5         IN  CHARACTER Indirizzo IP "puntato" o nome
011300160404     ** 6         IN  UNSIGNED  Numero porta del socket destinazione
011400151126     ** --------- --- --------- ------------------------------
011500151014     **
011600151014     **
011700151014     **
011800151126     ** PARAMETRI DELLA SOTTOPROCEDURA UBSOCKET2_Receive (TCP)
011900151014     **
012000151014     ** Posizione Uso Tipo      Descrizione
012100151014     ** --------- --- --------- ------------------------------
012200151014     ** 1         IN  UNSIGNED  Descrittore Socket Server
012300151126     ** 2         IN  INTEGER   Lunghezza del buffer di lettura messaggio socket client via TCP
012400151014     ** 3         IN  CHARACTER Tipo "End Of Line"
012500151014     ** 4         IN  CHARACTER Codice tabella conversione
012600151014     ** 5         OUT POINTER   Puntatore al contenuto del messaggio socket
012700151014     ** 6         OUT INTEGER   Lunghezza del messaggio socket
012800151014     ** --------- --- --------- ------------------------------
012900151126     **
013000151126     **
013100151126     **
013200151126     ** PARAMETRI DELLA SOTTOPROCEDURA UBSOCKET2_ReceiveFrom (UDP)
013300151126     **
013400151126     ** Posizione Uso Tipo      Descrizione
013500151126     ** --------- --- --------- ------------------------------
013600151126     ** 1         IN  UNSIGNED  Descrittore Socket Server
013700151126     ** 2         IN  INTEGER   Lunghezza del buffer di lettura messaggio socket client via UDP
013800151126     ** 3         IN  CHARACTER Tipo "End Of Line"
013900151126     ** 4         IN  CHARACTER Codice tabella conversione
014000151202     ** 5         OUT POINTER   Puntatore al contenuto del messaggio socket
014100151202     ** 6         OUT INTEGER   Lunghezza del messaggio socket
014200151126     ** --------- --- --------- ------------------------------
014300151008     **
014400151008     **
014500151008     **
014600151008     ** PARAMETRI DELLA SOTTOPROCEDURA UBSOCKET2_SendRcv
014700151008     **
014800151008     ** Posizione Uso Tipo      Descrizione
014900151008     ** --------- --- --------- ------------------------------
015000151014     ** 1         IN  UNSIGNED  Descrittore Socket
015100151008     ** 2         IN  CHARACTER Comado Socket da sottomettere
015200151008     ** 3         IN  CHARACTER Codice tabella conversione
015300151008     ** 4         OUT INTEGER   Lunghezza buffer Output
015400151008     ** 5         OUT CHARACTER Response in Output
015500151008     ** --------- --- --------- ------------------------------
015600080612     **
015700071128     **
015800071128     **
015900071128     ** ESEMPI DI CHIAMATA
016000071128     **
016100151126     **  if UBSOCKET2_Create(pInSktType : pOutSktDescr) = 0;
016200100621     **
016300151008     **  if UBSOCKET2_Garbage(pInSktDescr) = 0;
016400090713     **
016500151008     **  if UBSOCKET2_Connect(pInSktDescr : pInHostAdr : pInPort) = 0;
016600090713     **
016700151216     **  if UBSOCKET2_Send(pInSktDescr : pInSktMsg : pInMsgCvtTbl ) = 0
016800080612     **
016900080612     ** --------- --- --------- ------------------------------
017000071128     **
017100071128     ** ISTRUZIONI PER LA COMPILAZIONE
017200071128     **
017300151125     ** 1. Creare il modulo UBSOCKET2 (Opz 15 PDM) indicando BNDDIR('UBBNDDIR':'QC2LE')
017400151008     ** 2. Creare/Aggiornare il programma di servizio UBSOCKET2 (CRTSRVPGM / UPDSRVPGM)
017500080610     ** 4. Cancellare il modulo.
017600071128     **
017700071128     ***********************************************************************************************
017800080612
017900080612
018000080612      //********************************************************************************************
018100080612      //
018200080612      // Definizione file (globali)
018300080612      //
018400080612      //********************************************************************************************
018500071128
018600080612
018700071128      //********************************************************************************************
018800071128      //
018900080612      // Definizione prototipi procedure (globali)
019000071128      //
019100071128      //********************************************************************************************
019200151008     D/COPY GAITRASRC/SRCPROTOPR,UBSOCKET2
019300151127     D/COPY GAITRASRC/SRCPROTOPI,UBSOCKET2
019400151012     D/COPY GAITRASRC/SRCPROTOPR,UBCVTIDA
019500100922     D/COPY GAITRASRC/SRCPROTOPI,UBCVTIDA
019600151012     D/COPY GAITRASRC/SRCPROTOPR,UBNUF2IDA
019700151012     D/COPY GAITRASRC/SRCPROTOPI,UBNUF2IDA
019800100922     D/COPY GAITRASRC/SRCPROTOPR,UBDNSLKP
019900100922     D/COPY GAITRASRC/SRCPROTOPI,UBDNSLKP
020000100922     D/COPY GAITRASRC/SRCPROTOPR,SOCKET
020100090710     D/COPY GAITRASRC/SRCPROTOPR,INET_ADDR
020200151014     D/COPY GAITRASRC/SRCPROTOPR,UBXLATENCD
020300080612
020400080612
020500071128      //********************************************************************************************
020600071128      //
020700080612      // Definizione variabili work (globali)
020800071128      //
020900071128      //********************************************************************************************
021000151126     D InitDone        S               N   INZ(*off)
021100151126     D
021200071129
021300151126
021400080612
021500080612     P*--------------------------------------------------
021600151008     P* Procedure name: UBSOCKET2_Create
021700080612     P* Purpose:        Inizializzazioni
021800090713     P* Returns:        Integer (Status_Code)
021900151126     P* Parameter:      pInSktType     => Tipo di Socket ('TCP', 'UDP')
022000090713     P* Parameter:      pOutSktDescr   => Descrittore Socket
022100080612     P*--------------------------------------------------
022200080612     P*
022300151008     P UBSOCKET2_Create...
022400080612     P                 B                   EXPORT
022500080612     D*
022600151008     D UBSOCKET2_Create...
022700080612     D                 PI            10I 0
022800151126     D pInSktType                    10A   CONST
022900151126     D pOutSktDescr                  10I 0
023000080612     D*
023100080612      //********************************************************************************************
023200080612      //
023300080612      // Definizione variabili work (locali)
023400080612      //
023500080612      //********************************************************************************************
023600080612     D locMethodCode   S             10I 0
023700090710     D
023800151126     D wInSocketType   S             10I 0 Inz
023900151126     D wInProtocol     S             10I 0 INz
024000080612
024100090710
024200080612      /FREE
024300080612        // Inizializzo opcode di metodo
024400080612        locMethodCode = -1;
024500080612        InitDone = *off;
024600151126
024700151126        // Verifico il tipo di socket (TCP o UDP) richiesto (default è TCP)
024800151126        select;
024900151126          when pInSktType = *blanks;
025000151126               wInSocketType = SOCK_STREAM;
025100151126               wInProtocol   = IPPROTO_TCP;
025200151126          when pInSktType = 'TCP';
025300151126               wInSocketType = SOCK_STREAM;
025400151126               wInProtocol   = IPPROTO_TCP;
025500151126          when pInSktType = 'UDP';
025600151126               wInSocketType = SOCK_DGRAM;
025700151126               wInProtocol   = IPPROTO_IP;
025800151126        endsl;
025900151126
026000090710        // Definisco un socket (tipo e modalità)
026100151126        pOutSktDescr = socket(AF_INET : wInSocketType : wInProtocol);
026200090713        if pOutSktDescr < 0;
026300090710        else;
026400090710
026500080616           // Valorizzo opcode di metodo
026600080616           locMethodCode = 0;
026700080616           InitDone = *on;
026800080616
026900080616        endif;
027000080612
027100080612        return locMethodCode;
027200080612
027300080612      /END-FREE
027400080612
027500151008     P UBSOCKET2_Create...
027600080612     P                 E
027700151126
027800151126
027900151126
028000151126     P*--------------------------------------------------
028100151126     P* Procedure name: UBSOCKET2_SetSockOption...
028200151126     P* Purpose:        Socket Options Settings
028300151126     P* Returns:        Integer (Status_Code)
028400151126     P* Parameter:      pInSktDescr    => Descrittore Socket
028500151126     P* Parameter:      pInSktLevel    => Livello Socket
028600151126     P* Parameter:      pInOptName     => Nome   Opzione da impostare
028700151126     P* Parameter:      pInOptValue    => Valore Opzione da impostare
028800151126     P*--------------------------------------------------
028900151126     P*
029000151126     P UBSOCKET2_SetSockOption...
029100151126     P                 B                   EXPORT
029200151126     D*
029300151126     D UBSOCKET2_SetSockOption...
029400151126     D                 PI            10I 0
029500151126     D pInSktDescr                   10I 0 CONST
029600151126     D pInSktLevel                   10I 0 VALUE
029700151126     D pInOptName                    10I 0 VALUE
029800151126     D pInOptValue                   10I 0 VALUE
029900151126     D*
030000151126      //********************************************************************************************
030100151126      //
030200151126      // Definizione variabili work (locali)
030300151126      //
030400151126      //********************************************************************************************
030500151126     D locMethodCode   S             10I 0
030600151126     D
030700151126     D wErr            S               N   Inz(*off)
030800151126
030900151126
031000151126      /FREE
031100151126        // Inizializzo opcode di metodo
031200151126        locMethodCode = -1;
031300151126        InitDone = *off;
031400151126
031500151126        // Forzo l'opzione solo se Nome Opzione è stato passato
031600151126        if pInOptName <> *zeros;
031700151126
031800151126           select;
031900151126             when pInOptName = SO_LINGER;
032000151126
032100151126                 // Alloco la memoria necessaria a memorizzare la struttura "linger"
032200151126                 addrlen = %size(linger);
032300151126                 p_linge = %alloc(addrlen);
032400151126
032500151126                 // Faccio puntare l'indirizzo della struttura del "linger" alla nuova
032600151126                 // area di memoria appena allocata
032700151126                 p_linger = p_linge;
032800151126
032900151126                 // Quindi imposto i Valori di Opzionr richiesti
033000151126                 l_onoff = 1;
033100151126                 l_linger = pInOptValue;
033200151126
033300151126                 // Forzo le Opzioni richieste sul Socket richiesto
033400151126                 if setSockOpt(pInSktDescr : pInSktLevel : pInOptName :
033500151126                               p_linge : %size(linger)) < 0;
033600151126
033700151126                    // Errore in forzatura Opzioni Socket
033800151126                    wErr = *on;
033900151126
034000151126                 endif;
034100151126
034200151126             other;
034300151126
034400151126                 // Forzo le Opzioni richieste sul Socket richiesto
034500151126                 if setSockOpt(pInSktDescr : pInSktLevel : pInOptName :
034600151126                               %addr(pInOptValue): %size(pInOptValue)) < 0;
034700151126
034800151126                    // Errore in forzatura Opzioni Socket
034900151126                    wErr = *on;
035000151126
035100151126                 endif;
035200151126
035300151126           endsl;
035400151126
035500151126        endif;
035600151126
035700151126        if not wErr;
035800151126
035900151126           // Valorizzo opcode di metodo
036000151126           locMethodCode = 0;
036100151126           InitDone = *on;
036200151126
036300151126        endif;
036400151126
036500151126        return locMethodCode;
036600151126
036700151126      /END-FREE
036800151126
036900151126     P UBSOCKET2_SetSockOption...
037000151126     P                 E
037100080612
037200080612
037300080612
037400080612     P*--------------------------------------------------
037500151008     P* Procedure name: UBSOCKET2_Garbage
037600100612     P* Purpose:        Garbage collection
037700090713     P* Returns:        Integer (Status_Code)
037800090713     P* Parameter:      pInSktDescr    => Descrittore Socket
037900080612     P*--------------------------------------------------
038000080612     P*
038100151008     P UBSOCKET2_Garbage...
038200080612     P                 B                   EXPORT
038300080612     D*
038400151008     D UBSOCKET2_Garbage...
038500080612     D                 PI            10I 0
038600151126     D pInSktDescr                   10I 0 CONST
038700080612     D*
038800080612      //********************************************************************************************
038900080612      //
039000080612      // Definizione variabili work (locali)
039100080612      //
039200080612      //********************************************************************************************
039300080612     D locMethodCode   S             10I 0
039400080612
039500080612      /FREE
039600080612        // Inizializzo opcode di metodo
039700080612        locMethodCode = -1;
039800080612
039900090710        // Chiudo il socket
040000090713        callp close(pInSktDescr);
040100080612
040200080612        // Valorizzo opcode di metodo
040300080612        locMethodCode = 0;
040400080612
040500080612        return locMethodCode;
040600080612
040700080612      /END-FREE
040800080612
040900151008     P UBSOCKET2_Garbage...
041000080612     P                 E
041100090710
041200090710
041300090710
041400090710     P*--------------------------------------------------
041500151008     P* Procedure name: UBSOCKET2_Connect
041600090710     P* Purpose:        Connect to a socket "server"
041700090710     P* Returns:        Integer (Status_Code)
041800090713     P* Parameter:      pInSktDescr    => Descrittore Socket
041900100922     P* Parameter:      pInHostAdr     => Host remoto
042000090710     P* Parameter:      pInPort        => Numero porta
042100090710     P*--------------------------------------------------
042200090710     P*
042300151008     P UBSOCKET2_Connect...
042400090710     P                 B                   EXPORT
042500090710     D*
042600151008     D UBSOCKET2_Connect...
042700090710     D                 PI            10I 0
042800151126     D pInSktDescr                   10I 0 CONST
042900100922     D pInHost                      256A   CONST
043000090710     D pInPort                        5U 0 CONST
043100090710     D*
043200090710      //********************************************************************************************
043300090710      //
043400090710      // Definizione variabili work (locali)
043500090710      //
043600090710      //********************************************************************************************
043700090710     D locMethodCode   S             10I 0
043800090710     D
043900100922     D IpAddr          S             15A   inz
044000090710
044100090710
044200090710      /FREE
044300090710        // Inizializzo opcode di metodo
044400090710        locMethodCode = -1;
044500090710        InitDone = *off;
044600090710
044700090710        // Alloco la memoria necessaria a memorizzare la struttura del socket corrente
044800090710        addrlen = %size(sockaddr);
044900090710        p_connto = %alloc(addrlen);
045000090710
045100090710        // Faccio puntare l'inririzzo della struttura del socket corrente alla nuova
045200090710        // area di memoria appena allocata
045300090710        p_sockaddr = p_connto;
045400100922
045500151008
045600151008        // Effettuo il DNS Lookup x reperire SEMPRE l'indirizzo "puntato"
045700151008        callp UBDNSLKP_Retrieve(pInHost : pOutInetFound : pOutInetType);
045800151008
045900151008          if pOutInetType = 'NAM';
046000151008             IpAddr = %trim(pOutInetFound);
046100151008          else;
046200151008             IpAddr = %trim(pInHost);
046300151008          endif;
046400100922
046500100922          // Converto l'indirizzo internet "puntato" nel relativo valore numerico unsigned
046600100922          if UBCVTIDA_Convert(IpAddr:pOutInetAdrVal) = 0;
046700090710
046800100922             // Popolo la struttura del socket
046900100922             // Note that IP is the ip address we previously looked up
047000100922             // using the inet_addr and/or gethostbyname APIs
047100100922             // and port is the port number that we looked up using the
047200100922             // getservbyname API.
047300100922             sin_family = AF_INET;
047400100922             sin_addr = pOutInetAdrVal;
047500100922             sin_port = pInPort;
047600151127             sin_zero = *ALLx'00';
047700100922
047800100922             // Mi connetto al socket "server"
047900100922             if connect(pInSktDescr : p_connto : addrlen) < 0;
048000100922             else;
048100090710
048200100922                // Valorizzo opcode di metodo
048300100922                locMethodCode = 0;
048400100922                InitDone = *on;
048500090710
048600100922             endif;
048700110225          endif;
048800090710
048900090710        return locMethodCode;
049000090710
049100090710      /END-FREE
049200090710
049300151008     P UBSOCKET2_Connect...
049400090710     P                 E
049500151009
049600151009
049700151009
049800151009     P*--------------------------------------------------
049900151009     P* Procedure name: UBSOCKET2_Bind
050000151009     P* Purpose:        Bind on a local address/port
050100151009     P* Returns:        Integer (Status_Code)
050200151009     P* Parameter:      pInSktDescr    => Descrittore Socket
050300151009     P* Parameter:      pInLocalAdr    => Local Address
050400151009     P* Parameter:      pInLocalPort   => Numero porta
050500151009     P*--------------------------------------------------
050600151009     P*
050700151009     P UBSOCKET2_Bind...
050800151009     P                 B                   EXPORT
050900151009     D*
051000151009     D UBSOCKET2_Bind...
051100151009     D                 PI            10I 0
051200151126     D pInSktDescr                   10I 0 CONST
051300151009     D pInLocalAdr                  256A   CONST
051400151009     D pInLocalPort                   5U 0 CONST
051500151009     D*
051600151009      //********************************************************************************************
051700151009      //
051800151009      // Definizione variabili work (locali)
051900151009      //
052000151009      //********************************************************************************************
052100151009     D locMethodCode   S             10I 0
052200151009     D
052300151009     D IpAddr          S             15A   inz
052400151009
052500151009
052600151009      /FREE
052700151009        // Inizializzo opcode di metodo
052800151009        locMethodCode = -1;
052900151009        InitDone = *off;
053000151009
053100151009        // Alloco la memoria necessaria a memorizzare la struttura del socket corrente
053200151009        addrlen = %size(sockaddr);
053300151009        p_bindto = %alloc(addrlen);
053400151009
053500151009        // Faccio puntare l'indirizzo della struttura del socket corrente alla nuova
053600151009        // area di memoria appena allocata
053700151009        p_sockaddr = p_bindto;
053800151009
053900151009        // Inizializzo il valore del socket address
054000151009        sin_addr = *hival;
054100151009
054200151009        // Gestisco eventuali valori speciali per l'indirizzo di binding
054300151009        select;
054400151009          when %trim(pInLocalAdr) = 'INADDR_ANY';
054500151009             sin_addr = INADDR_ANY;
054600151009
054700151009          when %trim(pInLocalAdr) = 'INADDR_LOOPBACK';
054800151009             sin_addr = INADDR_LOOPBACK;
054900151009
055000151009          other;
055100151009            // Effettuo il DNS Lookup x reperire SEMPRE l'indirizzo "puntato"
055200151009            callp UBDNSLKP_Retrieve(pInLocalAdr : pOutInetFound : pOutInetType);
055300151009
055400151009            if pOutInetType = 'NAM';
055500151009               IpAddr = %trim(pOutInetFound);
055600151009            else;
055700151009               IpAddr = %trim(pInLocalAdr);
055800151009            endif;
055900151009
056000151009            // Converto l'indirizzo internet "puntato" nel relativo valore numerico unsigned
056100151009            if UBCVTIDA_Convert(IpAddr:pOutInetAdrVal) = 0;
056200151009
056300151009               sin_addr = pOutInetAdrVal;
056400151009
056500151009            endif;
056600151009        endsl;
056700151009
056800151009
056900151009        // Se a questo punto il socket address è valorizzato => proseguo
057000151009        if sin_addr <> *hival;
057100151009
057200151009           // Popolo la struttura del socket
057300151009           sin_family = AF_INET;
057400151009           sin_port = pInLocalPort;
057500151127           sin_zero = *ALLx'00';
057600151009
057700151009           // Mi bindo (ovvero "preparo indirizzo/porta" per il server)
057800151009           if bind(pInSktDescr : p_bindto : addrlen) < 0;
057900151009           else;
058000151009
058100151009              // Valorizzo opcode di metodo
058200151009              locMethodCode = 0;
058300151009              InitDone = *on;
058400151009
058500151009           endif;
058600151009
058700151009        endif;
058800151009
058900151009        return locMethodCode;
059000151009
059100151009      /END-FREE
059200151009
059300151009     P UBSOCKET2_Bind...
059400151009     P                 E
059500151008
059600151008
059700151008
059800151008     P*--------------------------------------------------
059900151126     P* Procedure name: UBSOCKET2_Send (TCP)
060000151008     P* Purpose:        Send socket command
060100151008     P* Returns:        Integer (Status_Code)
060200151008     P* Parameter:      pInSktDescr    => Descrittore Socket
060300151216     P* Parameter:      pInSktMsg      => Messaggio da sottomettere al socket destinazione via TCP
060400151216     P* Parameter:      pInMsgCvtTbl   => Tabella conversione messaggio socket
060500151008     P*--------------------------------------------------
060600151008     P*
060700151008     P UBSOCKET2_Send...
060800151008     P                 B                   EXPORT
060900151008     D*
061000151008     D UBSOCKET2_Send...
061100151008     D                 PI            10I 0
061200151126     D pInSktDescr                   10I 0 CONST
061300160331     D pInSktMsg                   4096A   CONST OPTIONS(*VARSIZE)
061400151216     D pInMsgCvtTbl                  10A                                        * QTCPASC QTCPEBC
061500151008     D*
061600151008      //********************************************************************************************
061700151008      //
061800151008      // Definizione variabili work (locali)
061900151008      //
062000151008      //********************************************************************************************
062100151008     D locMethodCode   S             10I 0
062200151008     D
062300151008     D Rcv             S             10I 0
062400160331     D SktMsg          S           4096A
062500151216     D SktMsgLenght    S             10I 0
062600151008
062700151008
062800151008      /FREE
062900151008        // Inizializzo opcode di metodo
063000151008        locMethodCode = -1;
063100151008        InitDone = *off;
063200151008
063300151008        // Verifica parametri "tabellati"
063400151216        if pInMsgCvtTbl = *blanks;
063500151216           pInMsgCvtTbl = 'QTCPASC';
063600151008        endif;
063700151008
063800151216        // Sottometto il messaggio al socket destinazione
063900151216        SktMsg = %trim(pInSktMsg);
064000151216        SktMsgLenght = %len(%trim(SktMsg));
064100151216        callp UBXLATENCD_Translate(SktMsgLenght : SktMsg : pInMsgCvtTbl);
064200151216        Rcv = send(pInSktDescr : %addr(SktMsg) : SktMsgLenght  : 0);
064300151216        if  Rcv < SktMsgLenght;
064400151126        else;
064500151008
064600151008           // Valorizzo opcode di metodo
064700151008           locMethodCode = 0;
064800151008           InitDone = *on;
064900151008
065000151008        endif;
065100151008
065200151008        return locMethodCode;
065300151008
065400151008      /END-FREE
065500151008
065600151008     P UBSOCKET2_Send...
065700151008     P                 E
065800160404
065900160404
066000160404
066100160404     P*--------------------------------------------------
066200160404     P* Procedure name: UBSOCKET2_SendTo (UDP)
066300160404     P* Purpose:        SendTo socket (UDP)
066400160404     P* Returns:        Integer (Status_Code)
066500160404     P* Parameter:      pInSktDescr    => Descrittore Socket
066600160404     P* Parameter:      pInBufferLen   => Lunghezza buffer risposta messaggio al socket client UDP
066700160404     P* Parameter:      pInSktMsg      => Messaggio da sottomettere al socket destinazione via UDP
066800160404     P* Parameter:      pInMsgCvtTbl   => Tabella conversione messaggio socket
066900160404     P* Parameter:      pInHost        => Indirizzo IP "puntato" o nome
067000160404     P* Parameter:      pInPort        => Numero porta del socket destinazione
067100160404     P*--------------------------------------------------
067200160404     P*
067300160404     P UBSOCKET2_SendTo...
067400160404     P                 B                   EXPORT
067500160404     D*
067600160404     D UBSOCKET2_SendTo...
067700160404     D                 PI            10I 0
067800160404     D pInSktDescr                   10I 0 CONST
067900160404     D pInBufferLen                  10I 0 CONST
068000160404     D pInSktMsg                   4096A   CONST OPTIONS(*VARSIZE)
068100160404     D pInMsgCvtTbl                  10A                                        * QTCPASC QTCPEBC
068200160404     D pInHost                      256A   CONST
068300160404     D pInPort                        5U 0 CONST
068400160404     D*
068500160404      //********************************************************************************************
068600160404      //
068700160404      // Definizione variabili work (locali)
068800160404      //
068900160404      //********************************************************************************************
069000160404     D locMethodCode   S             10I 0
069100160404     D
069200160404     D SktMsg          S           4096A
069300160404     D wDSsktAdr       DS                  likeds(sockaddr_in)
069400160404     D                                     based(p_connto)
069500160404     D IpAddr          S             15A   inz
069600160404
069700160404
069800160404      /FREE
069900160404        // Inizializzo opcode di metodo
070000160404        locMethodCode = -1;
070100160404        InitDone = *off;
070200160404
070300160404
070400160404        // Alloco la memoria necessaria a memorizzare la struttura del socket corrente
070500160404        addrlen = %size(sockaddr);
070600160404        p_connto = %alloc(addrlen);
070700160404
070800160404        // Verifica parametri "tabellati"
070900160404        if pInMsgCvtTbl = *blanks;
071000160404           pInMsgCvtTbl = 'QTCPASC';
071100160404        endif;
071200160404
071300160404        // Verifica eventuale porta "forzata" richiesta in input
071400160404        // Se non specificata di default rimane la porta origine ricezione messaggio dal client
071500160404        if pInPort <> *zeros;
071600160404           wDSsktAdr.sin_port = pInPort;
071700160404        endif;
071800160404
071900160404        // Effettuo il DNS Lookup x reperire SEMPRE l'indirizzo "puntato"
072000160404        callp UBDNSLKP_Retrieve(pInHost : pOutInetFound : pOutInetType);
072100160404
072200160404        if pOutInetType = 'NAM';
072300160404           IpAddr = %trim(pOutInetFound);
072400160404        else;
072500160404           IpAddr = %trim(pInHost);
072600160404        endif;
072700160404
072800160404        // Converto l'indirizzo internet "puntato" nel relativo valore numerico unsigned
072900160404        if UBCVTIDA_Convert(IpAddr:pOutInetAdrVal) = 0;
073000160404
073100160404           wDSsktAdr.sin_family = AF_INET;
073200160404           wDSsktAdr.sin_addr = pOutInetAdrVal;
073300160404           wDSsktAdr.sin_zero = *ALLx'00';
073400160404
073500160404
073600160404           // Sottometto il messaggio al socket "client"
073700160404           SktMsg = pInSktMsg;
073800160404           callp UBXLATENCD_Translate(pInBufferLen : SktMsg : pInMsgCvtTbl);
073900160404           if sendto(pInSktDescr : %addr(SktMsg) : pInBufferLen  : 0
074000160404                                      : p_connto : addrlen) < 0;
074100160404           else;
074200160404
074300160404               // Valorizzo opcode di metodo
074400160404               locMethodCode = 0;
074500160404               InitDone = *on;
074600160404
074700160404           endif;
074800160404
074900160404        endif;
075000160404
075100160404
075200160404        return locMethodCode;
075300160404
075400160404      /END-FREE
075500160404
075600160404     P UBSOCKET2_SendTo...
075700160404     P                 E
075800151126
075900151126
076000151126
076100151126     P*--------------------------------------------------
076200151216     P* Procedure name: UBSOCKET2_ReplyTo (UDP)
076300151216     P* Purpose:        ReplyTo socket (UDP)
076400151126     P* Returns:        Integer (Status_Code)
076500151126     P* Parameter:      pInSktDescr    => Descrittore Socket
076600151216     P* Parameter:      pInBufferLen   => Lunghezza buffer risposta messaggio al socket client UDP
076700151216     P* Parameter:      pInSktMsg      => Messaggio da sottomettere al socket destinazione via UDP
076800151216     P* Parameter:      pInMsgCvtTbl   => Tabella conversione messaggio socket
076900151216     P* Parameter:      pInSktAddrPtr  => Puntatore alla struttura socket_address di input
077000151216     P* Parameter:      pInPort        => Porta per risposta sul socket client
077100151126     P*--------------------------------------------------
077200151126     P*
077300151216     P UBSOCKET2_ReplyTo...
077400151126     P                 B                   EXPORT
077500151126     D*
077600151216     D UBSOCKET2_ReplyTo...
077700151126     D                 PI            10I 0
077800151126     D pInSktDescr                   10I 0 CONST
077900151216     D pInBufferLen                  10I 0 CONST
078000160331     D pInSktMsg                   4096A   CONST OPTIONS(*VARSIZE)
078100151216     D pInMsgCvtTbl                  10A                                        * QTCPASC QTCPEBC
078200151216     D pInSktAddrPtr                   *   CONST
078300151216     D pInPort                        5U 0 CONST
078400151126     D*
078500151126      //********************************************************************************************
078600151126      //
078700151126      // Definizione variabili work (locali)
078800151126      //
078900151126      //********************************************************************************************
079000151126     D locMethodCode   S             10I 0
079100151126     D
079200160331     D SktMsg          S           4096A
079300151216     D wDSsktAdr       DS                  likeds(sockaddr_in)
079400151216     D                                     based(p_connto)
079500151126
079600151126
079700151126      /FREE
079800151126        // Inizializzo opcode di metodo
079900151126        locMethodCode = -1;
080000151126        InitDone = *off;
080100151126
080200151216
080300151216        // Alloco la memoria necessaria a memorizzare la struttura del socket corrente
080400151216        addrlen = %size(sockaddr);
080500151216
080600151216        // Faccio puntare l'inririzzo della struttura del socket corrente al indirizzo
080700151216        // di memoria della struttura socket ricevuta in inoput
080800151216        p_connto = pInSktAddrPtr;
080900151126
081000151216        // Verifica parametri "tabellati"
081100151216        if pInMsgCvtTbl = *blanks;
081200151216           pInMsgCvtTbl = 'QTCPASC';
081300151216        endif;
081400151216
081500151216        // Verifica eventuale porta "forzata" richiesta in input
081600151216        // Se non specificata di default rimane la porta origine ricezione messaggio dal client
081700151216        if pInPort <> *zeros;
081800151216           wDSsktAdr.sin_port = pInPort;
081900151216        endif;
082000151126
082100151216
082200151216        // Sottometto il messaggio al socket "client"
082300151216        SktMsg = pInSktMsg;
082400151216        callp UBXLATENCD_Translate(pInBufferLen : SktMsg : pInMsgCvtTbl);
082500151216        if sendto(pInSktDescr : %addr(SktMsg) : pInBufferLen  : 0
082600151216                                   : p_connto : addrlen) < 0;
082700151216        else;
082800151126
082900151216            // Valorizzo opcode di metodo
083000151216            locMethodCode = 0;
083100151216            InitDone = *on;
083200151126
083300151216        endif;
083400151126
083500151126
083600151126        return locMethodCode;
083700151126
083800151126      /END-FREE
083900151126
084000151216     P UBSOCKET2_ReplyTo...
084100151126     P                 E
084200151013
084300151013
084400151013
084500161130     P*--------------------------------------------------
084600161130     P* Procedure name: UBSOCKET2_Receive
084700161130     P* Purpose:        Read socket response (TCP)
084800161130     P* Returns:        Integer (Status_Code)
084900161130     P* Parameter:      pInSktDescr    => Descrittore Socket Server
085000161130     P* Parameter:      pInBufferLen   => Lunghezza del buffer di lettura messaggio socket client CP
085100161130     P* Parameter:      pInTypEOL      => Tipo "End Of Line"
085200161130     P* Parameter:      pInMsgCvtTbl   => Codice tabella conversione
085300161130     P* Parameter:      pOutRespLen    => Lunghezza del messaggio socket
085400161130     P* Parameter:      pOutRespTxt    => Contenuto del messaggio socket (lunghezza variabile)
085500161130     P* Parameter:      pOutRespFull   => Flag messaggio socket completo Si/No
085600151013     P*--------------------------------------------------
085700151013     D*
085800151014     P UBSOCKET2_Receive...
085900151013     P                 B                   EXPORT
086000151013     D*
086100151014     D UBSOCKET2_Receive...
086200151013     D                 PI            10I 0
086300151126     D pInSktDescr                   10I 0 CONST
086400151126     D pInBufferLen                  10I 0 CONST
086500151013     D pInTypEOL                      1A   CONST
086600151216     D pInMsgCvtTbl                  10A                                        * QTCPASC QTCPEBC
086700161130     D pOutRespLen                   10I 0
086800161130     D pOutRespTxt                 4096A   OPTIONS(*VARSIZE)
086900161130     D pOutRespFull                  10I 0                                      * 0=No 1=Yes 2=Undef
087000161130     D*
087100151013      //********************************************************************************************
087200151013      //
087300151013      // Definizione variabili work (locali)
087400151013      //
087500151013      //********************************************************************************************
087600151013     D locMethodCode   S             10I 0
087700151013     D
087800161130     D RcvDataLen      S             10I 0 Inz
087900161130     D wResponse       S           4096A   Inz
088000160331     D wXlate          S          32766A   Inz
088100151013     D wCR             S              1A   Inz(x'0D')
088200151013     D wLF             S              1A   Inz(x'0A')
088300151014     D wEOL            S              2A   Inz Varying
088400161130     D wEOLlen         S              1S 0 Inz
088500151013
088600151013
088700151013      /FREE
088800151013        // Inizializzo opcode di metodo
088900151013        locMethodCode = -1;
089000151013        InitDone = *off;
089100161130
089200161130
089300161130        // Inizializzo parametri di output
089400161130        clear pOutRespLen;
089500161130        clear pOutRespTxt;
089600161130        clear pOutRespFull;
089700161130
089800161130
089900161130        // Inizializzo variabili di work (fondamentali)
090000161130        clear wResponse;
090100161130
090200161130
090300161130        // Verifica parametro di input relativo a lunghezza max buffer da ricevere
090400161130        if pInBufferLen > %size(wResponse);
090500161130           return locMethodCode;
090600161130        endif;
090700151014
090800151014
090900151013        // Verifica parametri "tabellati" relativi alla conversione del encoding dati
091000151216        if pInMsgCvtTbl = *blanks;
091100151216           pInMsgCvtTbl = 'QTCPEBC';
091200151013        endif;
091300151014
091400151013
091500151013        // Verifica parametri "tabellati" relativi alla conversione del encoding dati
091600161130        select;
091700161130          when pInTypEOL = '0';
091800161130               wEOL = '';
091900161130               wEOLlen = 0;
092000161130          when pInTypEOL = '1';
092100161130               wEOL = wCR;
092200161130               wEOLlen = 1;
092300161130          when pInTypEOL = '2';
092400161130               wEOL = wLF;
092500161130               wEOLlen = 1;
092600161130          when pInTypEOL = '3';
092700161130               wEOL = wCR+wLF;
092800161130               wEOLlen = 2;
092900161130          other;
093000161130               wEOL = wCR+wLF;
093100161130               wEOLlen = 2;
093200161130        endsl;
093300151013
093400161130
093500161130        // Ricevo il messaggio per intiero
093600161130        RcvDataLen = recv(pInSktDescr : %addr(wResponse) : pInBufferLen : 0);
093700161130
093800151013
093900161130        // Se nessun dato ricevuto => esito negativo
094000161130        if RcvDataLen < 1;
094100161130        else;
094200161130
094300161130           // Verifico se messaggio ricevuto è completo (ovvero include terminatori
094400161130           // (ovvero include terminatori di acknowledgement)
094500161130           if wEOLlen > *zeros;
094600161130              if %scan(%trim(wEOL) : wResponse) > *zeros;
094700161130                 pOutRespFull = 1;
094800161130              endif;
094900161130           else;
095000161130              pOutRespFull = 2;
095100161130           endif;
095200161130
095300161130           // Se ricevuti dati ... se richiesto converto encoding
095400161130           if pInMsgCvtTbl <> '*NONE';
095500161130              wXlate = wResponse;
095600161130              callp UBXLATENCD_Translate(RcvDataLen : wXlate : pInMsgCvtTbl);
095700161130              wResponse = wXlate;
095800161130           endif;
095900161130
096000161130           // Se intercettato acknowledgment richiesto => lo elimino dal messaggio vero e proprio
096100161130           if pOutRespFull = 1;
096200161130              RcvDataLen = RcvDataLen-wEOLlen;
096300161130              wResponse = %subst(wResponse:1:RcvDataLen);
096400161130           endif;
096500161130
096600161130           // Valorizzo i parametri di output
096700161130           pOutRespTxt = wResponse;
096800161130           pOutRespLen = RcvDataLen;
096900161130
097000161130           // Valorizzo opcode di metodo
097100161130           locMethodCode = 0;
097200161130           InitDone = *on;
097300161130
097400161130        endif;
097500151013
097600151013        return locMethodCode;
097700151013
097800151013      /END-FREE
097900151013
098000151014     P UBSOCKET2_Receive...
098100151013     P                 E
098200151126
098300151126
098400151126
098500151126     P*--------------------------------------------------
098600151126     P* Procedure name: UBSOCKET2_ReceiveFrom (UDP)
098700151126     P* Purpose:        Read socket response
098800151126     P* Returns:        Integer (Status_Code)
098900151212     P* Parameter:      pInSktDescr    => Descrittore Socket Server UDP
099000151212     P* Parameter:      pInBufferLen   => Lunghezza buffer lettura messaggio socket client UDP
099100151212     P* Parameter:      pInTypEOL      => Tipo "End Of Line"
099200151216     P* Parameter:      pInMsgCvtTbl   => Codice tabella conversione
099300151212     P* Parameter:      pOutRespLen    => Lunghezza del messaggio socket
099400151212     P* Parameter:      pOutRespTxt    => Contenuto del messaggio socket (lunghezza variabile)
099500151212     P* Parameter:      pOutRespFull   => Flag messaggio socket completo Si/No
099600151216     P* Parameter:      pOutSktAddrPtr => Puntatore alla struttura socket_address di output
099700151217     P* Parameter:      pOutSktAddr    => Indirizzo IP "dottato" del client sorgente
099800151126     P*--------------------------------------------------
099900151126     D*
100000151126     P UBSOCKET2_ReceiveFrom...
100100151126     P                 B                   EXPORT
100200151212     D*
100300151126     D UBSOCKET2_ReceiveFrom...
100400151126     D                 PI            10I 0
100500151126     D pInSktDescr                   10I 0 CONST
100600151212     D pInBufferLen                  10I 0 CONST
100700151212     D pInTypEOL                      1A   CONST                                * CR / LF / CRLF
100800151216     D pInMsgCvtTbl                  10A                                        * QTCPASC QTCPEBC
100900151212     D pOutRespLen                   10I 0
101000160331     D pOutRespTxt                 4096A   OPTIONS(*VARSIZE)
101100151212     D pOutRespFull                  10I 0                                      * 0=No 1=Yes 2=Undef
101200151216     D pOutSktAddrPtr                  *
101300151217     D pOutSktAddr                   15A
101400151126     D*
101500151126      //********************************************************************************************
101600151126      //
101700151126      // Definizione variabili work (locali)
101800151126      //
101900151126      //********************************************************************************************
102000151126     D locMethodCode   S             10I 0
102100151126     D
102200151212     D RcvDataLen      S             10I 0 Inz
102300160331     D wResponse       S           4096A   Inz
102400160331     D wXlate          S          32766A   Inz
102500151126     D wCR             S              1A   Inz(x'0D')
102600151126     D wLF             S              1A   Inz(x'0A')
102700151126     D wEOL            S              2A   Inz Varying
102800151212     D wEOLlen         S              1S 0 Inz
102900151126
103000151203
103100151212      /FREE
103200151126        // Inizializzo opcode di metodo
103300151126        locMethodCode = -1;
103400151126        InitDone = *off;
103500151212
103600151212
103700151212        // Inizializzo parametri di output
103800151212        clear pOutRespLen;
103900151212        clear pOutRespTxt;
104000151212        clear pOutRespFull;
104100151216        clear pOutSktAddrPtr;
104200151217        clear pOutSktAddr;
104300151212
104400151212
104500151212        // Inizializzo variabili di work (fondamentali)
104600151212        clear wResponse;
104700151212
104800151203
104900151203        // Verifica parametro di input relativo a lunghezza max buffer da ricevere
105000161130        if pInBufferLen > %size(wResponse);
105100151203           return locMethodCode;
105200151203        endif;
105300151126
105400151126
105500151126        // Verifica parametri "tabellati" relativi alla conversione del encoding dati
105600151216        if pInMsgCvtTbl = *blanks;
105700151216           pInMsgCvtTbl = 'QTCPEBC';
105800151126        endif;
105900151126
106000151126
106100151126        // Verifica parametri "tabellati" relativi alla conversione del encoding dati
106200151126        select;
106300151212          when pInTypEOL = '0';
106400151212               wEOL = '';
106500151212               wEOLlen = 0;
106600151126          when pInTypEOL = '1';
106700151126               wEOL = wCR;
106800151212               wEOLlen = 1;
106900151126          when pInTypEOL = '2';
107000151126               wEOL = wLF;
107100151212               wEOLlen = 1;
107200151126          when pInTypEOL = '3';
107300151126               wEOL = wCR+wLF;
107400151212               wEOLlen = 2;
107500151126          other;
107600151126               wEOL = wCR+wLF;
107700151212               wEOLlen = 2;
107800151126        endsl;
107900151212
108000151212
108100151212        // Alloco la memoria necessaria a memorizzare la struttura del socket corrente
108200151212        addrlen = %size(sockaddr);
108300151212        p_connfrom = %alloc(addrlen);
108400151212
108500151212
108600151212        // Ricevo il messaggio per intiero
108700151212        RcvDataLen = recvfrom(pInSktDescr : %addr(wResponse) :
108800151212                     pInBufferLen : 0 : p_connfrom : addrlen);
108900151212
109000151212
109100151212        // Se nessun dato ricevuto => esito negativo
109200151212        if RcvDataLen < 1;
109300151212        else;
109400151212
109500151212           // Verifico se messaggio ricevuto è completo (ovvero include terminatori
109600151212           // (ovvero include terminatori di acknowledgement)
109700151212           if wEOLlen > *zeros;
109800151212              if %scan(%trim(wEOL) : wResponse) > *zeros;
109900151212                 pOutRespFull = 1;
110000151212              endif;
110100151212           else;
110200151212              pOutRespFull = 2;
110300151212           endif;
110400151212
110500151212           // Se ricevuti dati ... se richiesto converto encoding
110600151216           if pInMsgCvtTbl <> '*NONE';
110700151212              wXlate = wResponse;
110800151216              callp UBXLATENCD_Translate(RcvDataLen : wXlate : pInMsgCvtTbl);
110900151212              wResponse = wXlate;
111000151212           endif;
111100151212
111200151212           // Se intercettato acknowledgment richiesto => lo elimino dal messaggio vero e proprio
111300151212           if pOutRespFull = 1;
111400151212              RcvDataLen = RcvDataLen-wEOLlen;
111500151212              wResponse = %subst(wResponse:1:RcvDataLen);
111600151212           endif;
111700151217
111800151217              // Converto l'indirizzo IP del cleint sorgente del messagio in indirizzip IP "dottato"
111900151217              p_sockaddr = p_connfrom;
112000151217              if UBNUF2IDA_Convert(sin_addr:pOutInetDotAdr) = 0;
112100151217
112200151217                 // Valorizzo i parametri di output
112300151217                 pOutRespTxt = wResponse;
112400151217                 pOutRespLen = RcvDataLen;
112500151217                 pOutSktAddrPtr = p_connfrom;
112600151217                 pOutSktAddr = pOutInetDotAdr;
112700151126
112800151217                 // Valorizzo opcode di metodo
112900151217                 locMethodCode = 0;
113000151217                 InitDone = *on;
113100151217
113200151217              endif;
113300151126
113400151126        endif;
113500151126
113600151126
113700151126        return locMethodCode;
113800151126
113900151126      /END-FREE
114000151126
114100151126     P UBSOCKET2_ReceiveFrom...
114200151126     P                 E
114300151009
114400151009
114500151009
114600151009     P*--------------------------------------------------
114700151009     P* Procedure name: UBSOCKET2_Listen
114800151009     P* Purpose:        Create socket server
114900151009     P* Returns:        Integer (Status_Code)
115000151009     P* Parameter:      pInSktDescr    => Descrittore Socket
115100151009     P* Parameter:      pInBackLog     => Max client servibili (contemporaneamente)
115200151009     P*--------------------------------------------------
115300151009     P*
115400151009     P UBSOCKET2_Listen...
115500151009     P                 B                   EXPORT
115600151009     D*
115700151009     D UBSOCKET2_Listen...
115800151009     D                 PI            10I 0
115900151126     D pInSktDescr                   10I 0 CONST
116000151009     D pInBackLog                     5U 0 CONST
116100151009     D*
116200151009      //********************************************************************************************
116300151009      //
116400151009      // Definizione variabili work (locali)
116500151009      //
116600151009      //********************************************************************************************
116700151009     D locMethodCode   S             10I 0
116800151009     D
116900151012     D BackLog         S              5U 0 INZ(1024)
117000151009
117100151009
117200151009      /FREE
117300151009        // Inizializzo opcode di metodo
117400151009        locMethodCode = -1;
117500151009        InitDone = *off;
117600151009
117700151012        // Se non indicato un valore specifico per il back-log => assumo 1024
117800151009        if pInBackLog > *zeros;
117900151009           BackLog = pInBackLog;
118000151009        endif;
118100151009
118200151009        // Mi metto in ascolto (listening) sul socket server
118300151009        if listen(pInSktDescr : BackLog) < 0;
118400151009        else;
118500151009
118600151009           // Valorizzo opcode di metodo
118700151009           locMethodCode = 0;
118800151009           InitDone = *on;
118900151009
119000151009        endif;
119100151009
119200151009        return locMethodCode;
119300151009
119400151009      /END-FREE
119500151009
119600151009     P UBSOCKET2_Listen...
119700151009     P                 E
119800151009
119900151009
120000151009
120100151009     P*--------------------------------------------------
120200151126     P* Procedure name: UBSOCKET2_Accept
120300151009     P* Purpose:        Accept a socket client request
120400151009     P* Returns:        Integer (Status_Code)
120500151014     P* Parameter:      pInSktDescr    => Descrittore Socket Server
120600151014     P* Parameter:      pOutSktDescr   => Descrittore Socket Client
120700151014     P* Parameter:      pOutSktAddr    => Indirizzo IP "dottato" del client
120800151009     P*--------------------------------------------------
120900151009     P*
121000151009     P UBSOCKET2_Accept...
121100151009     P                 B                   EXPORT
121200151009     D*
121300151009     D UBSOCKET2_Accept...
121400151009     D                 PI            10I 0
121500151126     D pInSktDescr                   10I 0 CONST
121600151126     D pOutSktDescr                  10I 0
121700151125     D pOutSktAddr                   15A
121800151009     D*
121900151009      //********************************************************************************************
122000151009      //
122100151009      // Definizione variabili work (locali)
122200151009      //
122300151009      //********************************************************************************************
122400151009     D locMethodCode   S             10I 0
122500151009     D
122600151009
122700151009
122800151009      /FREE
122900151009        // Inizializzo opcode di metodo
123000151009        locMethodCode = -1;
123100151009        InitDone = *off;
123200151009
123300151009        // Alloco la memoria necessaria a memorizzare la struttura del socket corrente
123400151009        addrlen = %size(sockaddr);
123500151009        p_connfrom = %alloc(addrlen);
123600151009
123700151009        // Servo la richiesta cliente "estraendola" dallo stack del server
123800151009        pOutSktDescr = accept(pInSktDescr : p_connfrom : addrlen);
123900151009
124000151009        // Se nuovo socket creato correttamente (Accept riuscita)
124100151009        if pOutSktDescr < *zeros;
124200151009        else;
124300151009           // Lunghezza "illegale" connessione TCP
124400151009           if addrlen <> 16;
124500151009           else;
124600151125
124700151125              p_sockaddr = p_connfrom;
124800151009
124900151012              if UBNUF2IDA_Convert(sin_addr:pOutInetDotAdr) = 0;
125000151012
125100151012                 // Valorizzo i parametri di output
125200151125                 pOutSktAddr = pOutInetDotAdr;
125300151012
125400151012                 // Valorizzo opcode di metodo
125500151012                 locMethodCode = 0;
125600151012                 InitDone = *on;
125700151126
125800151012              endif;
125900151009           endif;
126000151009        endif;
126100151009
126200151009
126300151009        return locMethodCode;
126400151009
126500151009      /END-FREE
126600151009
126700151009     P UBSOCKET2_Accept...
126800151009     P                 E
126900151127
127000151127
127100151127
127200151127     P*--------------------------------------------------
127300151127     P* Procedure name: UBSOCKET2_MngDescrAttrbs
127400151127     P* Purpose:        Manage Descriptors Attributes
127500151127     P* Returns:        Integer (Status_Code)
127600151127     P* Parameter:      pInSktDescr    => Descrittore Socket di cui gestire gli attributi
127700151127     P* Parameter:      pInCommand     => Comando da eseguire (metodo)
127800151127     P* Parameter:      pInCmdArg      => Argomento del comando (valore)
127900151127     P*--------------------------------------------------
128000151127     P*
128100151127     P UBSOCKET2_MngDescrAttrbs...
128200151127     P                 B                   EXPORT
128300151127     D*
128400151127     D UBSOCKET2_MngDescrAttrbs...
128500151127     D                 PI            10I 0
128600151127     D pInSktDescr                   10I 0 CONST
128700151127     D pInCommand                    10I 0 CONST
128800151127     D pInCmdArg                     10I 0 CONST
128900151127     D*
129000151127      //********************************************************************************************
129100151127      //
129200151127      // Definizione variabili work (locali)
129300151127      //
129400151127      //********************************************************************************************
129500151127     D locMethodCode   S             10I 0
129600151127     D
129700151127     D wErr            S               N   Inz(*off)
129800151127
129900151127
130000151127      /FREE
130100151127        // Inizializzo opcode di metodo
130200151127        locMethodCode = -1;
130300151127        InitDone = *off;
130400151127
130500151127        // A seconda del metodo richiesto
130600151127        // gestisco l'attributo richiesto per il socket richiesto
130700151127        select;
130800151127           when pInCommand = F_GETFL;
130900151127              if fcntl(pInSktDescr : F_GETFL) < 0;
131000151127
131100151127                 // Errore in gestione attributi descrittore socket
131200151127                 wErr = *on;
131300151127
131400151127               endif;
131500151127
131600151127           when pInCommand = F_SETFL;
131700151127              if fcntl(pInSktDescr : F_SETFL : pInCmdArg) < 0;
131800151127
131900151127                 // Errore in gestione attributi descrittore socket
132000151127                 wErr = *on;
132100151127
132200151127               endif;
132300151127
132400151127           other;
132500151127
132600151127              // Errore in passaggio parametri
132700151127              wErr = *on;
132800151127
132900151127        endsl;
133000151127
133100151127
133200151127        if not wErr;
133300151127
133400151127           // Valorizzo opcode di metodo
133500151127           locMethodCode = 0;
133600151127           InitDone = *on;
133700151127
133800151127        endif;
133900151127
134000151127
134100151127        return locMethodCode;
134200151127
134300151127      /END-FREE
134400151127
134500151127     P UBSOCKET2_MngDescrAttrbs...
134600151127     P                 E
134700151127
134800151127
134900151127
135000151127     P*--------------------------------------------------
135100151127     P* Procedure name: UBSOCKET2_Select
135200151127     P* Purpose:        Select socket descriptor (to manage)
135300151127     P* Returns:        Integer (Status_Code)
135400151127     P* Parameter:      pInMaxDescr    => Numero massimo Descrittori Socket "gestibili"
135500151127     P* Parameter:      pInReadSet     => Set di Descrittori Socket da "leggere"
135600151127     P* Parameter:      pInWriteSet    => Set di Descrittori Socket da "scrivere"
135700151127     P* Parameter:      pInExcpSet     => Set di Descrittori Socket con "eccezioni"
135800151127     P* Parameter:      pInWaitTime    => Intervallo di "polling" tra Descrittori Socket
135900151127     P*--------------------------------------------------
136000151127     P*
136100151127     P UBSOCKET2_Select...
136200151127     P                 B                   EXPORT
136300151127     D*
136400151127     D UBSOCKET2_Select...
136500151127     D                 PI            10I 0
136600151127     D pInMaxDescr                   10I 0 CONST
136700151202     D pInReadSet                    28A   VALUE
136800151202     D pInWriteSet                   28A   VALUE
136900151202     D pInExcpSet                    28A   VALUE
137000151202     D pInWaitTime                   10I 0 VALUE
137100151127     D*
137200151127      //********************************************************************************************
137300151127      //
137400151127      // Definizione variabili work (locali)
137500151127      //
137600151127      //********************************************************************************************
137700151127     D locMethodCode   S             10I 0
137800151127     D
137900151127
138000151127
138100151127      /FREE
138200151127        // Inizializzo opcode di metodo
138300151127        locMethodCode = -1;
138400151127        InitDone = *off;
138500151127
138600151127        // Alloco la memoria necessaria a memorizzare la struttura del timeval
138700151127        addrlen = %size(timeval);
138800151127        p_tv = %alloc(addrlen);
138900151127        p_timeval = p_tv;
139000151127
139100151127        tv_sec = %div(pInWaitTime:1000000);
139200151127        tv_usec = %rem(pInWaitTime:1000000);
139300151127
139400151127
139500151127        if select(pInMaxDescr+1 : %addr(pInReadSet) :
139600151127                  %addr(pInWriteSet) : %addr(pInExcpSet): p_timeval) < 0;
139700151127        else;
139800151127
139900151127           // Valorizzo opcode di metodo
140000151127           locMethodCode = 0;
140100151127           InitDone = *on;
140200151127
140300151127        endif;
140400151127
140500151127        return locMethodCode;
140600151127
140700151127      /END-FREE
140800151127
140900151127     P UBSOCKET2_Select...
141000151127     P                 E
141100151127
141200151127
141300151127
141400151127     P*--------------------------------------------------
141500151127     P* Procedure name: UBSOCKET2_FD_zero
141600151127     P* Purpose:        Clear all Descriptors in a set
141700151127     P* Returns:        ---
141800151127     P* Parameter:      pInFDset       => Descriptors' set
141900151127     P*--------------------------------------------------
142000151127     P*
142100151127     P UBSOCKET2_FD_zero...
142200151127     P                 B                   EXPORT
142300151127     D*
142400151127     D UBSOCKET2_FD_zero...
142500151127     D                 PI
142600151127     D pInFDset                      28A
142700151127     D*
142800151127      //********************************************************************************************
142900151127      //
143000151127      // Definizione variabili work (locali)
143100151127      //
143200151127      //********************************************************************************************
143300151127     D
143400151127
143500151127      /FREE
143600151127
143700151127        // Inizializzo il Set dei Descripts ricevuto in input
143800151127        pInFDset = *ALLx'00';
143900151127
144000151127      /END-FREE
144100151127
144200151127     P UBSOCKET2_FD_zero...
144300151127     P                 E
144400151127
144500151127
144600151127
144700151127     P*--------------------------------------------------
144800151127     P* Procedure name: UBSOCKET2_FD_set
144900151127     P* Purpose:        Set Descriptors in a set
145000151127     P* Returns:        ---
145100151127     P* Parameter:      pInFD          => Descriptors
145200151127     P* Parameter:      pInFDset       => Descriptors' set
145300151127     P*--------------------------------------------------
145400151127     P*
145500151127     P UBSOCKET2_FD_set...
145600151127     P                 B                   EXPORT
145700151127     D*
145800151127     D UBSOCKET2_FD_set...
145900151127     D                 PI
146000151127     D pInFD                         10I 0
146100151127     D pInFDset                      28A
146200151127     D*
146300151127      //********************************************************************************************
146400151127      //
146500151127      // Definizione variabili work (locali)
146600151127      //
146700151127      //********************************************************************************************
146800151127     D
146900151127     D wkByteNo        S              5I 0
147000151127     D wkMask          S              1A
147100151127     D wkByte          S              1A
147200151127     C*
147300151127     C                   callp     UBSOCKET2_CalcBitPos(pInFD:wkByteNo:wkMask)
147400151127     C                   eval      wkByte = %subst(pInFDset:wkByteNo:1)
147500151127     C                   biton     wkMask        wkByte
147600151127     C                   eval      %subst(pInFDset:wkByteNo:1) = wkByte
147700151127     C*
147800151127     P UBSOCKET2_FD_set...
147900151127     P                 E
148000151127
148100151127
148200151127
148300151127     P*--------------------------------------------------
148400151127     P* Procedure name: UBSOCKET2_FD_clr
148500151127     P* Purpose:        Clear Descriptors in a set
148600151127     P* Returns:        ---
148700151127     P* Parameter:      pInFD          => Descriptors
148800151127     P* Parameter:      pInFDset       => Descriptors' set
148900151127     P*--------------------------------------------------
149000151127     P*
149100151127     P UBSOCKET2_FD_clr...
149200151127     P                 B                   EXPORT
149300151127     D*
149400151127     D UBSOCKET2_FD_clr...
149500151127     D                 PI
149600151127     D pInFD                         10I 0
149700151127     D pInFDset                      28A
149800151127     D*
149900151127      //********************************************************************************************
150000151127      //
150100151127      // Definizione variabili work (locali)
150200151127      //
150300151127      //********************************************************************************************
150400151127     D
150500151127     D wkByteNo        S              5I 0
150600151127     D wkMask          S              1A
150700151127     D wkByte          S              1A
150800151127     C*
150900151127     C                   callp     UBSOCKET2_CalcBitPos(pInFD:wkByteNo:wkMask)
151000151127     C                   eval      wkByte = %subst(pInFDset:wkByteNo:1)
151100151127     C                   bitoff    wkMask        wkByte
151200151127     C                   eval      %subst(pInFDset:wkByteNo:1) = wkByte
151300151127     C*
151400151127     P UBSOCKET2_FD_clr...
151500151127     P                 E
151600151127
151700151127
151800151127
151900151127     P*--------------------------------------------------
152000151127     P* Procedure name: UBSOCKET2_FD_isSet
152100151127     P* Purpose:        Check if a Descriptor Is Set (or not)
152200151127     P* Returns:        Boolean (indicator *on/*off)
152300151127     P* Parameter:      pInFD          => Descriptors
152400151127     P* Parameter:      pInFDset       => Descriptors' set
152500151127     P*--------------------------------------------------
152600151127     P*
152700151127     P UBSOCKET2_FD_isSet...
152800151127     P                 B                   EXPORT
152900151127     D*
153000151127     D UBSOCKET2_FD_isSet...
153100151127     D                 PI             1N
153200151127     D pInFD                         10I 0
153300151127     D pInFDset                      28A
153400151127     D*
153500151127      //********************************************************************************************
153600151127      //
153700151127      // Definizione variabili work (locali)
153800151127      //
153900151127      //********************************************************************************************
154000151127     D
154100151127     D wkByteNo        S              5I 0
154200151127     D wkMask          S              1A
154300151127     D wkByte          S              1A
154400151127     C*
154500151127     C                   callp     UBSOCKET2_CalcBitPos(pInFD:wkByteNo:wkMask)
154600151127     C                   eval      wkByte = %subst(pInFDset:wkByteNo:1)
154700151127     C                   testb     wkMask        wkByte                   88
154800151127     C                   return    *IN88
154900151127     C*
155000151127     P UBSOCKET2_FD_isSet...
155100151127     P                 E
155200151127
155300151127
155400151127
155500151127     P*--------------------------------------------------
155600151127     P* Procedure name: UBSOCKET2_CalcBitPos...
155700151127     P* Purpose:        Calculate Descriptor BitMask
155800151127     P* Returns:        ---
155900151127     P* Parameter:      pInSktDescr    => Descriptor
156000151127     P* Parameter:      pInByteNo      => Byte's number
156100151127     P* Parameter:      pInBitMask     => Byte's BitMask
156200151127     P*--------------------------------------------------
156300151127     P*
156400151127     P UBSOCKET2_CalcBitPos...
156500151127     P                 B                   EXPORT
156600151127     D*
156700151127     D UBSOCKET2_CalcBitPos...
156800151127     D                 PI
156900151127     D pInSktDescr                   10I 0
157000151127     D pInByteNo                      5I 0
157100151127     D pInBitMask                     1A
157200151127     D*
157300151127      //********************************************************************************************
157400151127      //
157500151127      // Definizione variabili work (locali)
157600151127      //
157700151127      //********************************************************************************************
157800151127     D dsMakeMask      DS
157900151127     D   dsZeroByte            1      1A
158000151127     D   dsMask                2      2A
158100151127     D   dsBitMult             1      2U 0 Inz(0)
158200151127     D
158300151127     D wkGroup         S              2  0 Inz
158400151127     D wkByteNo        S              2  0 Inz
158500151127     D wkBitNo         S              2  0 Inz
158600151127     C*
158700151127     C     pInSktDescr   DIV       32            wkGroup
158800151127     C                   MVR                     wkByteNo
158900151127     C                   DIV       8             wkByteNo
159000151127     C                   MVR                     wkBitNo
159100151127     C*
159200151127     C                   eval      wkByteNo = 4 - wkByteNo
159300151127     C                   eval      pInByteNo = (wkGroup * 4) + wkByteNo
159400151127     C                   eval      dsBitMult = 2 ** wkBitNo
159500151127     C                   eval      dsZeroByte = x'00'
159600151127     C                   eval      pInBitMask = dsMask
159700151127     C*
159800151127     P UBSOCKET2_CalcBitPos...
159900151127     P                 E
