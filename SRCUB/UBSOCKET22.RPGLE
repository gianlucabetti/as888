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
013900151126     ** 5         IN  CHARACTER Ip Address "dotted"/name       => Indirizzo IP "puntato" o nome
014000151126     ** 6         IN  UNSIGNED  Port number                    => Numero porta del socket destinaz.
014100151126     ** 7         OUT POINTER   Puntatore al contenuto del messaggio socket
014200151126     ** 8         OUT INTEGER   Lunghezza del messaggio socket
014300151126     ** --------- --- --------- ------------------------------
014400151008     **
014500151008     **
014600151008     **
014700151008     ** PARAMETRI DELLA SOTTOPROCEDURA UBSOCKET2_SendRcv
014800151008     **
014900151008     ** Posizione Uso Tipo      Descrizione
015000151008     ** --------- --- --------- ------------------------------
015100151014     ** 1         IN  UNSIGNED  Descrittore Socket
015200151008     ** 2         IN  CHARACTER Comado Socket da sottomettere
015300151008     ** 3         IN  CHARACTER Codice tabella conversione
015400151008     ** 4         OUT INTEGER   Lunghezza buffer Output
015500151008     ** 5         OUT CHARACTER Response in Output
015600151008     ** --------- --- --------- ------------------------------
015700080612     **
015800071128     **
015900071128     **
016000071128     ** ESEMPI DI CHIAMATA
016100071128     **
016200151126     **  if UBSOCKET2_Create(pInSktType : pOutSktDescr) = 0;
016300100621     **
016400151008     **  if UBSOCKET2_Garbage(pInSktDescr) = 0;
016500090713     **
016600151008     **  if UBSOCKET2_Connect(pInSktDescr : pInHostAdr : pInPort) = 0;
016700090713     **
016800151008     **  if UBSOCKET2_Send(pInSktDescr : pInSktCmd : pInCmdCvtTbl ) = 0
016900080612     **
017000080612     ** --------- --- --------- ------------------------------
017100071128     **
017200071128     ** ISTRUZIONI PER LA COMPILAZIONE
017300071128     **
017400151125     ** 1. Creare il modulo UBSOCKET2 (Opz 15 PDM) indicando BNDDIR('UBBNDDIR':'QC2LE')
017500151008     ** 2. Creare/Aggiornare il programma di servizio UBSOCKET2 (CRTSRVPGM / UPDSRVPGM)
017600080610     ** 4. Cancellare il modulo.
017700071128     **
017800071128     ***********************************************************************************************
017900080612
018000080612
018100080612      //********************************************************************************************
018200080612      //
018300080612      // Definizione file (globali)
018400080612      //
018500080612      //********************************************************************************************
018600071128
018700080612
018800071128      //********************************************************************************************
018900071128      //
019000080612      // Definizione prototipi procedure (globali)
019100071128      //
019200071128      //********************************************************************************************
019300151008     D/COPY GAITRASRC/SRCPROTOPR,UBSOCKET2
019400151127     D/COPY GAITRASRC/SRCPROTOPI,UBSOCKET2
019500151012     D/COPY GAITRASRC/SRCPROTOPR,UBCVTIDA
019600100922     D/COPY GAITRASRC/SRCPROTOPI,UBCVTIDA
019700151012     D/COPY GAITRASRC/SRCPROTOPR,UBNUF2IDA
019800151012     D/COPY GAITRASRC/SRCPROTOPI,UBNUF2IDA
019900100922     D/COPY GAITRASRC/SRCPROTOPR,UBDNSLKP
020000100922     D/COPY GAITRASRC/SRCPROTOPI,UBDNSLKP
020100100922     D/COPY GAITRASRC/SRCPROTOPR,SOCKET
020200090710     D/COPY GAITRASRC/SRCPROTOPR,INET_ADDR
020300151014     D/COPY GAITRASRC/SRCPROTOPR,UBXLATENCD
020400080612
020500080612
020600071128      //********************************************************************************************
020700071128      //
020800080612      // Definizione variabili work (globali)
020900071128      //
021000071128      //********************************************************************************************
021100151126     D InitDone        S               N   INZ(*off)
021200151126     D
021300071129
021400151126
021500080612
021600080612     P*--------------------------------------------------
021700151008     P* Procedure name: UBSOCKET2_Create
021800080612     P* Purpose:        Inizializzazioni
021900090713     P* Returns:        Integer (Status_Code)
022000151126     P* Parameter:      pInSktType     => Tipo di Socket ('TCP', 'UDP')
022100090713     P* Parameter:      pOutSktDescr   => Descrittore Socket
022200080612     P*--------------------------------------------------
022300080612     P*
022400151008     P UBSOCKET2_Create...
022500080612     P                 B                   EXPORT
022600080612     D*
022700151008     D UBSOCKET2_Create...
022800080612     D                 PI            10I 0
022900151126     D pInSktType                    10A   CONST
023000151126     D pOutSktDescr                  10I 0
023100080612     D*
023200080612      //********************************************************************************************
023300080612      //
023400080612      // Definizione variabili work (locali)
023500080612      //
023600080612      //********************************************************************************************
023700080612     D locMethodCode   S             10I 0
023800090710     D
023900151126     D wInSocketType   S             10I 0 Inz
024000151126     D wInProtocol     S             10I 0 INz
024100080612
024200090710
024300080612      /FREE
024400080612        // Inizializzo opcode di metodo
024500080612        locMethodCode = -1;
024600080612        InitDone = *off;
024700151126
024800151126        // Verifico il tipo di socket (TCP o UDP) richiesto (default è TCP)
024900151126        select;
025000151126          when pInSktType = *blanks;
025100151126               wInSocketType = SOCK_STREAM;
025200151126               wInProtocol   = IPPROTO_TCP;
025300151126          when pInSktType = 'TCP';
025400151126               wInSocketType = SOCK_STREAM;
025500151126               wInProtocol   = IPPROTO_TCP;
025600151126          when pInSktType = 'UDP';
025700151126               wInSocketType = SOCK_DGRAM;
025800151126               wInProtocol   = IPPROTO_IP;
025900151126        endsl;
026000151126
026100090710        // Definisco un socket (tipo e modalità)
026200151126        pOutSktDescr = socket(AF_INET : wInSocketType : wInProtocol);
026300090713        if pOutSktDescr < 0;
026400090710        else;
026500090710
026600080616           // Valorizzo opcode di metodo
026700080616           locMethodCode = 0;
026800080616           InitDone = *on;
026900080616
027000080616        endif;
027100080612
027200080612        return locMethodCode;
027300080612
027400080612      /END-FREE
027500080612
027600151008     P UBSOCKET2_Create...
027700080612     P                 E
027800151126
027900151126
028000151126
028100151126     P*--------------------------------------------------
028200151126     P* Procedure name: UBSOCKET2_SetSockOption...
028300151126     P* Purpose:        Socket Options Settings
028400151126     P* Returns:        Integer (Status_Code)
028500151126     P* Parameter:      pInSktDescr    => Descrittore Socket
028600151126     P* Parameter:      pInSktLevel    => Livello Socket
028700151126     P* Parameter:      pInOptName     => Nome   Opzione da impostare
028800151126     P* Parameter:      pInOptValue    => Valore Opzione da impostare
028900151126     P*--------------------------------------------------
029000151126     P*
029100151126     P UBSOCKET2_SetSockOption...
029200151126     P                 B                   EXPORT
029300151126     D*
029400151126     D UBSOCKET2_SetSockOption...
029500151126     D                 PI            10I 0
029600151126     D pInSktDescr                   10I 0 CONST
029700151126     D pInSktLevel                   10I 0 VALUE
029800151126     D pInOptName                    10I 0 VALUE
029900151126     D pInOptValue                   10I 0 VALUE
030000151126     D*
030100151126      //********************************************************************************************
030200151126      //
030300151126      // Definizione variabili work (locali)
030400151126      //
030500151126      //********************************************************************************************
030600151126     D locMethodCode   S             10I 0
030700151126     D
030800151126     D p_linger        S               *
030900151126     D linger          DS                  BASED(p_linger)
031000151126     D   l_onoff                     10I 0
031100151126     D   l_linger                    10I 0
031200151126     D
031300151126     D p_linge         S               *
031400151126     D addrlen         S             10I 0
031500151126     D
031600151126     D wErr            S               N   Inz(*off)
031700151126
031800151126
031900151126      /FREE
032000151126        // Inizializzo opcode di metodo
032100151126        locMethodCode = -1;
032200151126        InitDone = *off;
032300151126
032400151126        // Forzo l'opzione solo se Nome Opzione è stato passato
032500151126        if pInOptName <> *zeros;
032600151126
032700151126           select;
032800151126             when pInOptName = SO_LINGER;
032900151126
033000151126                 // Alloco la memoria necessaria a memorizzare la struttura "linger"
033100151126                 addrlen = %size(linger);
033200151126                 p_linge = %alloc(addrlen);
033300151126
033400151126                 // Faccio puntare l'indirizzo della struttura del "linger" alla nuova
033500151126                 // area di memoria appena allocata
033600151126                 p_linger = p_linge;
033700151126
033800151126                 // Quindi imposto i Valori di Opzionr richiesti
033900151126                 l_onoff = 1;
034000151126                 l_linger = pInOptValue;
034100151126
034200151126                 // Forzo le Opzioni richieste sul Socket richiesto
034300151126                 if setSockOpt(pInSktDescr : pInSktLevel : pInOptName :
034400151126                               p_linge : %size(linger)) < 0;
034500151126
034600151126                    // Errore in forzatura Opzioni Socket
034700151126                    wErr = *on;
034800151126
034900151126                 endif;
035000151126
035100151126             other;
035200151126
035300151126                 // Forzo le Opzioni richieste sul Socket richiesto
035400151126                 if setSockOpt(pInSktDescr : pInSktLevel : pInOptName :
035500151126                               %addr(pInOptValue): %size(pInOptValue)) < 0;
035600151126
035700151126                    // Errore in forzatura Opzioni Socket
035800151126                    wErr = *on;
035900151126
036000151126                 endif;
036100151126
036200151126           endsl;
036300151126
036400151126        endif;
036500151126
036600151126        if not wErr;
036700151126
036800151126           // Valorizzo opcode di metodo
036900151126           locMethodCode = 0;
037000151126           InitDone = *on;
037100151126
037200151126        endif;
037300151126
037400151126        return locMethodCode;
037500151126
037600151126      /END-FREE
037700151126
037800151126     P UBSOCKET2_SetSockOption...
037900151126     P                 E
038000080612
038100080612
038200080612
038300080612     P*--------------------------------------------------
038400151008     P* Procedure name: UBSOCKET2_Garbage
038500100612     P* Purpose:        Garbage collection
038600090713     P* Returns:        Integer (Status_Code)
038700090713     P* Parameter:      pInSktDescr    => Descrittore Socket
038800080612     P*--------------------------------------------------
038900080612     P*
039000151008     P UBSOCKET2_Garbage...
039100080612     P                 B                   EXPORT
039200080612     D*
039300151008     D UBSOCKET2_Garbage...
039400080612     D                 PI            10I 0
039500151126     D pInSktDescr                   10I 0 CONST
039600080612     D*
039700080612      //********************************************************************************************
039800080612      //
039900080612      // Definizione variabili work (locali)
040000080612      //
040100080612      //********************************************************************************************
040200080612     D locMethodCode   S             10I 0
040300080612
040400080612      /FREE
040500080612        // Inizializzo opcode di metodo
040600080612        locMethodCode = -1;
040700080612
040800090710        // Chiudo il socket
040900090713        callp close(pInSktDescr);
041000080612
041100080612        // Valorizzo opcode di metodo
041200080612        locMethodCode = 0;
041300080612
041400080612        return locMethodCode;
041500080612
041600080612      /END-FREE
041700080612
041800151008     P UBSOCKET2_Garbage...
041900080612     P                 E
042000090710
042100090710
042200090710
042300090710     P*--------------------------------------------------
042400151008     P* Procedure name: UBSOCKET2_Connect
042500090710     P* Purpose:        Connect to a socket "server"
042600090710     P* Returns:        Integer (Status_Code)
042700090713     P* Parameter:      pInSktDescr    => Descrittore Socket
042800100922     P* Parameter:      pInHostAdr     => Host remoto
042900090710     P* Parameter:      pInPort        => Numero porta
043000090710     P*--------------------------------------------------
043100090710     P*
043200151008     P UBSOCKET2_Connect...
043300090710     P                 B                   EXPORT
043400090710     D*
043500151008     D UBSOCKET2_Connect...
043600090710     D                 PI            10I 0
043700151126     D pInSktDescr                   10I 0 CONST
043800100922     D pInHost                      256A   CONST
043900090710     D pInPort                        5U 0 CONST
044000090710     D*
044100090710      //********************************************************************************************
044200090710      //
044300090710      // Definizione variabili work (locali)
044400090710      //
044500090710      //********************************************************************************************
044600090710     D locMethodCode   S             10I 0
044700090710     D
044800090710     D p_sockaddr      S               *
044900090710     D sockaddr        DS                  based(p_sockaddr)
045000090710     D   sa_family                    5I 0
045100090710     D   sa_data                     14A
045200090710     D sockaddr_in     DS                  based(p_sockaddr)
045300090710     D   sin_family                   5I 0
045400090710     D   sin_port                     5U 0
045500090710     D   sin_addr                    10U 0
045600090710     D   sin_zero                     8A
045700090710     D
045800090710     D p_connto        S               *
045900090710     D addrlen         S             10I 0
046000100922     D IpAddr          S             15A   inz
046100090710
046200090710
046300090710      /FREE
046400090710        // Inizializzo opcode di metodo
046500090710        locMethodCode = -1;
046600090710        InitDone = *off;
046700090710
046800090710        // Alloco la memoria necessaria a memorizzare la struttura del socket corrente
046900090710        addrlen = %size(sockaddr);
047000090710        p_connto = %alloc(addrlen);
047100090710
047200090710        // Faccio puntare l'inririzzo della struttura del socket corrente alla nuova
047300090710        // area di memoria appena allocata
047400090710        p_sockaddr = p_connto;
047500100922
047600151008
047700151008        // Effettuo il DNS Lookup x reperire SEMPRE l'indirizzo "puntato"
047800151008        callp UBDNSLKP_Retrieve(pInHost : pOutInetFound : pOutInetType);
047900151008
048000151008          if pOutInetType = 'NAM';
048100151008             IpAddr = %trim(pOutInetFound);
048200151008          else;
048300151008             IpAddr = %trim(pInHost);
048400151008          endif;
048500100922
048600100922          // Converto l'indirizzo internet "puntato" nel relativo valore numerico unsigned
048700100922          if UBCVTIDA_Convert(IpAddr:pOutInetAdrVal) = 0;
048800090710
048900100922             // Popolo la struttura del socket
049000100922             // Note that IP is the ip address we previously looked up
049100100922             // using the inet_addr and/or gethostbyname APIs
049200100922             // and port is the port number that we looked up using the
049300100922             // getservbyname API.
049400100922             sin_family = AF_INET;
049500100922             sin_addr = pOutInetAdrVal;
049600100922             sin_port = pInPort;
049700151127             sin_zero = *ALLx'00';
049800100922
049900100922             // Mi connetto al socket "server"
050000100922             if connect(pInSktDescr : p_connto : addrlen) < 0;
050100100922             else;
050200090710
050300100922                // Valorizzo opcode di metodo
050400100922                locMethodCode = 0;
050500100922                InitDone = *on;
050600090710
050700100922             endif;
050800110225          endif;
050900090710
051000090710        return locMethodCode;
051100090710
051200090710      /END-FREE
051300090710
051400151008     P UBSOCKET2_Connect...
051500090710     P                 E
051600151009
051700151009
051800151009
051900151009     P*--------------------------------------------------
052000151009     P* Procedure name: UBSOCKET2_Bind
052100151009     P* Purpose:        Bind on a local address/port
052200151009     P* Returns:        Integer (Status_Code)
052300151009     P* Parameter:      pInSktDescr    => Descrittore Socket
052400151009     P* Parameter:      pInLocalAdr    => Local Address
052500151009     P* Parameter:      pInLocalPort   => Numero porta
052600151009     P*--------------------------------------------------
052700151009     P*
052800151009     P UBSOCKET2_Bind...
052900151009     P                 B                   EXPORT
053000151009     D*
053100151009     D UBSOCKET2_Bind...
053200151009     D                 PI            10I 0
053300151126     D pInSktDescr                   10I 0 CONST
053400151009     D pInLocalAdr                  256A   CONST
053500151009     D pInLocalPort                   5U 0 CONST
053600151009     D*
053700151009      //********************************************************************************************
053800151009      //
053900151009      // Definizione variabili work (locali)
054000151009      //
054100151009      //********************************************************************************************
054200151009     D locMethodCode   S             10I 0
054300151009     D
054400151009     D p_sockaddr      S               *
054500151009     D sockaddr        DS                  based(p_sockaddr)
054600151009     D   sa_family                    5I 0
054700151009     D   sa_data                     14A
054800151009     D sockaddr_in     DS                  based(p_sockaddr)
054900151009     D   sin_family                   5I 0
055000151009     D   sin_port                     5U 0
055100151009     D   sin_addr                    10U 0
055200151009     D   sin_zero                     8A
055300151009     D
055400151009     D p_bindto        S               *
055500151009     D addrlen         S             10I 0
055600151009     D IpAddr          S             15A   inz
055700151009
055800151009
055900151009      /FREE
056000151009        // Inizializzo opcode di metodo
056100151009        locMethodCode = -1;
056200151009        InitDone = *off;
056300151009
056400151009        // Alloco la memoria necessaria a memorizzare la struttura del socket corrente
056500151009        addrlen = %size(sockaddr);
056600151009        p_bindto = %alloc(addrlen);
056700151009
056800151009        // Faccio puntare l'indirizzo della struttura del socket corrente alla nuova
056900151009        // area di memoria appena allocata
057000151009        p_sockaddr = p_bindto;
057100151009
057200151009        // Inizializzo il valore del socket address
057300151009        sin_addr = *hival;
057400151009
057500151009        // Gestisco eventuali valori speciali per l'indirizzo di binding
057600151009        select;
057700151009          when %trim(pInLocalAdr) = 'INADDR_ANY';
057800151009             sin_addr = INADDR_ANY;
057900151009
058000151009          when %trim(pInLocalAdr) = 'INADDR_LOOPBACK';
058100151009             sin_addr = INADDR_LOOPBACK;
058200151009
058300151009          other;
058400151009            // Effettuo il DNS Lookup x reperire SEMPRE l'indirizzo "puntato"
058500151009            callp UBDNSLKP_Retrieve(pInLocalAdr : pOutInetFound : pOutInetType);
058600151009
058700151009            if pOutInetType = 'NAM';
058800151009               IpAddr = %trim(pOutInetFound);
058900151009            else;
059000151009               IpAddr = %trim(pInLocalAdr);
059100151009            endif;
059200151009
059300151009            // Converto l'indirizzo internet "puntato" nel relativo valore numerico unsigned
059400151009            if UBCVTIDA_Convert(IpAddr:pOutInetAdrVal) = 0;
059500151009
059600151009               sin_addr = pOutInetAdrVal;
059700151009
059800151009            endif;
059900151009        endsl;
060000151009
060100151009
060200151009        // Se a questo punto il socket address è valorizzato => proseguo
060300151009        if sin_addr <> *hival;
060400151009
060500151009           // Popolo la struttura del socket
060600151009           sin_family = AF_INET;
060700151009           sin_port = pInLocalPort;
060800151127           sin_zero = *ALLx'00';
060900151009
061000151009           // Mi bindo (ovvero "preparo indirizzo/porta" per il server)
061100151009           if bind(pInSktDescr : p_bindto : addrlen) < 0;
061200151009           else;
061300151009
061400151009              // Valorizzo opcode di metodo
061500151009              locMethodCode = 0;
061600151009              InitDone = *on;
061700151009
061800151009           endif;
061900151009
062000151009        endif;
062100151009
062200151009        return locMethodCode;
062300151009
062400151009      /END-FREE
062500151009
062600151009     P UBSOCKET2_Bind...
062700151009     P                 E
062800151008
062900151008
063000151008
063100151008     P*--------------------------------------------------
063200151126     P* Procedure name: UBSOCKET2_Send (TCP)
063300151008     P* Purpose:        Send socket command
063400151008     P* Returns:        Integer (Status_Code)
063500151008     P* Parameter:      pInSktDescr    => Descrittore Socket
063600151126     P* Parameter:      pInSktCmd      => Comando da sottomettere al socket destinazione via TCP
063700151008     P* Parameter:      pInCmdCvtTbl   => Tabella conversione comando socket
063800151008     P*--------------------------------------------------
063900151008     P*
064000151008     P UBSOCKET2_Send...
064100151008     P                 B                   EXPORT
064200151008     D*
064300151008     D UBSOCKET2_Send...
064400151008     D                 PI            10I 0
064500151126     D pInSktDescr                   10I 0 CONST
064600151008     D pInSktCmd                   1024A   CONST
064700151125     D pInCmdCvtTbl                  10A                                        * QTCPASC QTCPEBC
064800151008     D*
064900151008      //********************************************************************************************
065000151008      //
065100151008      // Definizione variabili work (locali)
065200151008      //
065300151008      //********************************************************************************************
065400151008     D locMethodCode   S             10I 0
065500151008     D
065600151008     D Rcv             S             10I 0
065700151008     D SktCmd          S           1024A
065800151008     D SktCmdLenght    S             10I 0
065900151008
066000151008
066100151008      /FREE
066200151008        // Inizializzo opcode di metodo
066300151008        locMethodCode = -1;
066400151008        InitDone = *off;
066500151008
066600151008        // Verifica parametri "tabellati"
066700151008        if pInCmdCvtTbl = *blanks;
066800151008           pInCmdCvtTbl = 'QTCPASC';
066900151008        endif;
067000151008
067100151126        // Sottometto il comando al socket destinazione
067200151008        SktCmd = %trim(pInSktCmd);
067300151008        SktCmdLenght = %len(%trim(SktCmd));
067400151014        callp UBXLATENCD_Translate(SktCmdLenght : SktCmd : pInCmdCvtTbl);
067500151008        Rcv = send(pInSktDescr : %addr(SktCmd) : SktCmdLenght  : 0);
067600151126        if  Rcv < SktCmdLenght;
067700151126        else;
067800151008
067900151008           // Valorizzo opcode di metodo
068000151008           locMethodCode = 0;
068100151008           InitDone = *on;
068200151008
068300151008        endif;
068400151008
068500151008        return locMethodCode;
068600151008
068700151008      /END-FREE
068800151008
068900151008     P UBSOCKET2_Send...
069000151008     P                 E
069100151126
069200151126
069300151126
069400151126     P*--------------------------------------------------
069500151126     P* Procedure name: UBSOCKET2_SendTo (UDP)
069600151126     P* Purpose:        Send socket command
069700151126     P* Returns:        Integer (Status_Code)
069800151126     P* Parameter:      pInSktDescr    => Descrittore Socket
069900151126     P* Parameter:      pInSktCmd      => Comando da sottomettere al socket destinazione via UDP
070000151126     P* Parameter:      pInCmdCvtTbl   => Tabella conversione comando socket
070100151126     P* Parameter:      pInHostAdr     => Host remoto
070200151126     P* Parameter:      pInPort        => Numero porta
070300151126     P*--------------------------------------------------
070400151126     P*
070500151126     P UBSOCKET2_SendTo...
070600151126     P                 B                   EXPORT
070700151126     D*
070800151126     D UBSOCKET2_SendTo...
070900151126     D                 PI            10I 0
071000151126     D pInSktDescr                   10I 0 CONST
071100151126     D pInSktCmd                   1024A   CONST
071200151126     D pInCmdCvtTbl                  10A                                        * QTCPASC QTCPEBC
071300151126     D pInHost                      256A   CONST
071400151126     D pInPort                        5U 0 CONST
071500151126     D*
071600151126      //********************************************************************************************
071700151126      //
071800151126      // Definizione variabili work (locali)
071900151126      //
072000151126      //********************************************************************************************
072100151126     D locMethodCode   S             10I 0
072200151126     D
072300151126     D Rcv             S             10I 0
072400151126     D SktCmd          S           1024A
072500151126     D SktCmdLenght    S             10I 0
072600151126     D
072700151126     D p_sockaddr      S               *
072800151126     D sockaddr        DS                  based(p_sockaddr)
072900151126     D   sa_family                    5I 0
073000151126     D   sa_data                     14A
073100151126     D sockaddr_in     DS                  based(p_sockaddr)
073200151126     D   sin_family                   5I 0
073300151126     D   sin_port                     5U 0
073400151126     D   sin_addr                    10U 0
073500151126     D   sin_zero                     8A
073600151126     D
073700151126     D p_connto        S               *
073800151126     D addrlen         S             10I 0
073900151126     D IpAddr          S             15A   inz
074000151126
074100151126
074200151126      /FREE
074300151126        // Inizializzo opcode di metodo
074400151126        locMethodCode = -1;
074500151126        InitDone = *off;
074600151126
074700151126        // Alloco la memoria necessaria a memorizzare la struttura del socket corrente
074800151126        addrlen = %size(sockaddr);
074900151126        p_connto = %alloc(addrlen);
075000151126
075100151126        // Faccio puntare l'inririzzo della struttura del socket corrente alla nuova
075200151126        // area di memoria appena allocata
075300151126        p_sockaddr = p_connto;
075400151126
075500151126
075600151126        // Effettuo il DNS Lookup x reperire SEMPRE l'indirizzo "puntato"
075700151126        callp UBDNSLKP_Retrieve(pInHost : pOutInetFound : pOutInetType);
075800151126
075900151126        if pOutInetType = 'NAM';
076000151126           IpAddr = %trim(pOutInetFound);
076100151126        else;
076200151126           IpAddr = %trim(pInHost);
076300151126        endif;
076400151126
076500151126        // Converto l'indirizzo internet "puntato" nel relativo valore numerico unsigned
076600151126        if UBCVTIDA_Convert(IpAddr:pOutInetAdrVal) = 0;
076700151126
076800151126           // Popolo la struttura del socket
076900151126           // Note that IP is the ip address we previously looked up
077000151126           // using the inet_addr and/or gethostbyname APIs
077100151126           // and port is the port number that we looked up using the
077200151126           // getservbyname API.
077300151126           sin_family = AF_INET;
077400151126           sin_addr = pOutInetAdrVal;
077500151126           sin_port = pInPort;
077600151127           sin_zero = *ALLx'00';
077700151126
077800151126           // Verifica parametri "tabellati"
077900151126           if pInCmdCvtTbl = *blanks;
078000151126              pInCmdCvtTbl = 'QTCPASC';
078100151126           endif;
078200151126
078300151126           // Sottometto il comando al socket "server"
078400151126           SktCmd = %trim(pInSktCmd);
078500151126           SktCmdLenght = %len(%trim(SktCmd));
078600151126           callp UBXLATENCD_Translate(SktCmdLenght : SktCmd : pInCmdCvtTbl);
078700151126           Rcv = sendto(pInSktDescr : %addr(SktCmd) : SktCmdLenght  : 0
078800151126                                    : p_connto : addrlen);
078900151126           if  Rcv < SktCmdLenght;
079000151126           else;
079100151126
079200151126              // Valorizzo opcode di metodo
079300151126              locMethodCode = 0;
079400151126              InitDone = *on;
079500151126
079600151126           endif;
079700151126
079800151126        endif;
079900151126
080000151126        return locMethodCode;
080100151126
080200151126      /END-FREE
080300151126
080400151126     P UBSOCKET2_SendTo...
080500151126     P                 E
080600151013
080700151013
080800151013
080900151014     P*--------------------------------------------------
081000151126     P* Procedure name: UBSOCKET2_Receive (TCP)
081100151013     P* Purpose:        Read socket response
081200151013     P* Returns:        Integer (Status_Code)
081300151013     P* Parameter:      pInSktDescr    => Descrittore Socket
081400151126     P* Parameter:      pInBufferLen   => Lunghezza del buffer di lettura messaggio socket clientTCP
081500151013     P* Parameter:      pInCmdCvtTbl   => Codice tabella conversione
081600151013     P* Parameter:      pOutResponse   => Contenuto del messaggio socket
081700151013     P* Parameter:      pOutRespLen    => Lunghezza del messaggio socket
081800151013     P*--------------------------------------------------
081900151013     D*
082000151014     P UBSOCKET2_Receive...
082100151013     P                 B                   EXPORT
082200151013     D*
082300151014     D UBSOCKET2_Receive...
082400151013     D                 PI            10I 0
082500151126     D pInSktDescr                   10I 0 CONST
082600151126     D pInBufferLen                  10I 0 CONST
082700151013     D pInTypEOL                      1A   CONST
082800151125     D pInCmdCvtTbl                  10A                                        * QTCPASC QTCPEBC
082900151014     D pOutRespPtr                     *
083000151126     D pOutRespLen                   10I 0
083100151013     D*
083200151013      //********************************************************************************************
083300151013      //
083400151013      // Definizione variabili work (locali)
083500151013      //
083600151013      //********************************************************************************************
083700151013     D locMethodCode   S             10I 0
083800151013     D
083900151126     D Rcv             S             10I 0 Inz
084000151014     D wResponse       S          65536A   VARYING Based(pOutRespPtr)
084100151014     D wResponseLen    S             10I 0 Inz
084200151014     D wXlate          S          65536A   Inz
084300151013     D wChar           S              1A   Inz
084400151013     D wCR             S              1A   Inz(x'0D')
084500151013     D wLF             S              1A   Inz(x'0A')
084600151014     D wEOL            S              2A   Inz Varying
084700151013
084800151013
084900151013      /FREE
085000151013        // Inizializzo opcode di metodo
085100151013        locMethodCode = -1;
085200151013        InitDone = *off;
085300151014
085400151014
085500151013        // Verifica parametri "tabellati" relativi alla conversione del encoding dati
085600151013        if pInCmdCvtTbl = *blanks;
085700151013           pInCmdCvtTbl = 'QTCPEBC';
085800151013        endif;
085900151014
086000151013
086100151013        // Verifica parametri "tabellati" relativi alla conversione del encoding dati
086200151013        select;
086300151013          when pInTypEOL = '1';
086400151013               wEOL = wCR;
086500151013          when pInTypEOL = '2';
086600151014               wEOL = wLF;
086700151013          when pInTypEOL = '3';
086800151014               wEOL = wCR+wLF;
086900151013          other;
087000151014               wEOL = wCR+wLF;
087100151013        endsl;
087200151013
087300151013
087400151013        // Ricevo il messaggio byte-a-byte
087500151013        dou Rcv < 1;
087600151013
087700151013            // Inizializzo variabili di wrk
087800151014            wResponse    = %trim(' ');
087900151013            wResponseLen = *zeros;
088000151014            wXlate       = *blanks;
088100151014            wChar        = *blanks;
088200151013
088300151014            dou wResponseLen = %size(pInBufferLen) OR
088400151014                %scan(wChar : wEOL) > *zeros;
088500151014
088600151013                Rcv = recv(pInSktDescr : %addr(wChar) : 1 : 0);
088700151013                if Rcv < 1;
088800151013                   leave;
088900151013                endif;
089000151013
089100151014                if %scan(wChar : wEOL) = *zeros;
089200151013                   wResponseLen = wResponseLen + 1;
089300151014                   %subst(wResponse : wResponseLen : 1) = wChar;
089400151013                endif;
089500151013
089600151013            enddo;
089700151013
089800151013        enddo;
089900151013
090000151014
090100151013        if wResponseLen > *zeros;
090200151014
090300151013           if pInCmdCvtTbl <> '*NONE';
090400151014              wXlate = wResponse;
090500151014              callp UBXLATENCD_Translate(wResponseLen : wXlate : pInCmdCvtTbl);
090600151014              wResponse = wXlate;
090700151014           endif;
090800151014
090900151014           // Valorizzo opcode di metodo
091000151014           pOutRespPtr = %addr(wResponse);
091100151014           pOutRespLen = wResponseLen;
091200151014
091300151014           // Valorizzo opcode di metodo
091400151014           locMethodCode = 0;
091500151014           InitDone = *on;
091600151014
091700151013        endif;
091800151013
091900151013
092000151013        return locMethodCode;
092100151013
092200151013      /END-FREE
092300151013
092400151014     P UBSOCKET2_Receive...
092500151013     P                 E
092600151126
092700151126
092800151126
092900151126     P*--------------------------------------------------
093000151126     P* Procedure name: UBSOCKET2_ReceiveFrom (UDP)
093100151126     P* Purpose:        Read socket response
093200151126     P* Returns:        Integer (Status_Code)
093300151126     P* Parameter:      pInSktDescr    => Descrittore Socket
093400151126     P* Parameter:      pInBufferLen   => Lunghezza del buffer di lettura messaggio socket clientUDP
093500151126     P* Parameter:      pInCmdCvtTbl   => Codice tabella conversione
093600151126     P* Parameter:      pInHostAdr     => Host remoto
093700151126     P* Parameter:      pInPort        => Numero porta
093800151126     P* Parameter:      pOutResponse   => Contenuto del messaggio socket
093900151126     P* Parameter:      pOutRespLen    => Lunghezza del messaggio socket
094000151126     P*--------------------------------------------------
094100151126     D*
094200151126     P UBSOCKET2_ReceiveFrom...
094300151126     P                 B                   EXPORT
094400151126     D*
094500151126     D UBSOCKET2_ReceiveFrom...
094600151126     D                 PI            10I 0
094700151126     D pInSktDescr                   10I 0 CONST
094800151126     D pInBufferLen                  10I 0 CONST
094900151126     D pInTypEOL                      1A   CONST
095000151126     D pInCmdCvtTbl                  10A                                        * QTCPASC QTCPEBC
095100151126     D pInHost                      256A   CONST
095200151126     D pInPort                        5U 0 CONST
095300151126     D pOutRespPtr                     *
095400151126     D pOutRespLen                   10I 0
095500151126     D*
095600151126      //********************************************************************************************
095700151126      //
095800151126      // Definizione variabili work (locali)
095900151126      //
096000151126      //********************************************************************************************
096100151126     D locMethodCode   S             10I 0
096200151126     D
096300151126     D p_sockaddr      S               *
096400151126     D sockaddr        DS                  based(p_sockaddr)
096500151126     D   sa_family                    5I 0
096600151126     D   sa_data                     14A
096700151126     D sockaddr_in     DS                  based(p_sockaddr)
096800151126     D   sin_family                   5I 0
096900151126     D   sin_port                     5U 0
097000151126     D   sin_addr                    10U 0
097100151126     D   sin_zero                     8A
097200151126     D
097300151126     D p_connto        S               *
097400151126     D addrlen         S             10I 0
097500151126     D
097600151126     D Rcv             S             10I 0 Inz
097700151126     D wResponse       S          65536A   VARYING Based(pOutRespPtr)
097800151126     D wResponseLen    S             10I 0 Inz
097900151126     D wXlate          S          65536A   Inz
098000151126     D wChar           S              1A   Inz
098100151126     D wCR             S              1A   Inz(x'0D')
098200151126     D wLF             S              1A   Inz(x'0A')
098300151126     D wEOL            S              2A   Inz Varying
098400151126
098500151126
098600151126      /FREE
098700151126        // Inizializzo opcode di metodo
098800151126        locMethodCode = -1;
098900151126        InitDone = *off;
099000151126
099100151126
099200151126        // Verifica parametri "tabellati" relativi alla conversione del encoding dati
099300151126        if pInCmdCvtTbl = *blanks;
099400151126           pInCmdCvtTbl = 'QTCPEBC';
099500151126        endif;
099600151126
099700151126
099800151126        // Verifica parametri "tabellati" relativi alla conversione del encoding dati
099900151126        select;
100000151126          when pInTypEOL = '1';
100100151126               wEOL = wCR;
100200151126          when pInTypEOL = '2';
100300151126               wEOL = wLF;
100400151126          when pInTypEOL = '3';
100500151126               wEOL = wCR+wLF;
100600151126          other;
100700151126               wEOL = wCR+wLF;
100800151126        endsl;
100900151126
101000151126
101100151126        // Ricevo il messaggio byte-a-byte
101200151126        dou Rcv < 1;
101300151126
101400151126            // Inizializzo variabili di wrk
101500151126            wResponse    = %trim(' ');
101600151126            wResponseLen = *zeros;
101700151126            wXlate       = *blanks;
101800151126            wChar        = *blanks;
101900151126
102000151126            dou wResponseLen = %size(pInBufferLen) OR
102100151126                %scan(wChar : wEOL) > *zeros;
102200151126
102300151126                Rcv = recvfrom(pInSktDescr : %addr(wChar) : 1 : 0
102400151126                                           : p_connto : addrlen);
102500151126                if Rcv < 1;
102600151126                   leave;
102700151126                endif;
102800151126
102900151126                if %scan(wChar : wEOL) = *zeros;
103000151126                   wResponseLen = wResponseLen + 1;
103100151126                   %subst(wResponse : wResponseLen : 1) = wChar;
103200151126                endif;
103300151126
103400151126            enddo;
103500151126
103600151126        enddo;
103700151126
103800151126
103900151126        if wResponseLen > *zeros;
104000151126
104100151126           if pInCmdCvtTbl <> '*NONE';
104200151126              wXlate = wResponse;
104300151126              callp UBXLATENCD_Translate(wResponseLen : wXlate : pInCmdCvtTbl);
104400151126              wResponse = wXlate;
104500151126           endif;
104600151126
104700151126           // Valorizzo opcode di metodo
104800151126           pOutRespPtr = %addr(wResponse);
104900151126           pOutRespLen = wResponseLen;
105000151126
105100151126           // Valorizzo opcode di metodo
105200151126           locMethodCode = 0;
105300151126           InitDone = *on;
105400151126
105500151126        endif;
105600151126
105700151126
105800151126        return locMethodCode;
105900151126
106000151126      /END-FREE
106100151126
106200151126     P UBSOCKET2_ReceiveFrom...
106300151126     P                 E
106400151009
106500151009
106600151009
106700151009     P*--------------------------------------------------
106800151009     P* Procedure name: UBSOCKET2_Listen
106900151009     P* Purpose:        Create socket server
107000151009     P* Returns:        Integer (Status_Code)
107100151009     P* Parameter:      pInSktDescr    => Descrittore Socket
107200151009     P* Parameter:      pInBackLog     => Max client servibili (contemporaneamente)
107300151009     P*--------------------------------------------------
107400151009     P*
107500151009     P UBSOCKET2_Listen...
107600151009     P                 B                   EXPORT
107700151009     D*
107800151009     D UBSOCKET2_Listen...
107900151009     D                 PI            10I 0
108000151126     D pInSktDescr                   10I 0 CONST
108100151009     D pInBackLog                     5U 0 CONST
108200151009     D*
108300151009      //********************************************************************************************
108400151009      //
108500151009      // Definizione variabili work (locali)
108600151009      //
108700151009      //********************************************************************************************
108800151009     D locMethodCode   S             10I 0
108900151009     D
109000151012     D BackLog         S              5U 0 INZ(1024)
109100151009
109200151009
109300151009      /FREE
109400151009        // Inizializzo opcode di metodo
109500151009        locMethodCode = -1;
109600151009        InitDone = *off;
109700151009
109800151012        // Se non indicato un valore specifico per il back-log => assumo 1024
109900151009        if pInBackLog > *zeros;
110000151009           BackLog = pInBackLog;
110100151009        endif;
110200151009
110300151009        // Mi metto in ascolto (listening) sul socket server
110400151009        if listen(pInSktDescr : BackLog) < 0;
110500151009        else;
110600151009
110700151009           // Valorizzo opcode di metodo
110800151009           locMethodCode = 0;
110900151009           InitDone = *on;
111000151009
111100151009        endif;
111200151009
111300151009        return locMethodCode;
111400151009
111500151009      /END-FREE
111600151009
111700151009     P UBSOCKET2_Listen...
111800151009     P                 E
111900151009
112000151009
112100151009
112200151009     P*--------------------------------------------------
112300151126     P* Procedure name: UBSOCKET2_Accept
112400151009     P* Purpose:        Accept a socket client request
112500151009     P* Returns:        Integer (Status_Code)
112600151014     P* Parameter:      pInSktDescr    => Descrittore Socket Server
112700151014     P* Parameter:      pOutSktDescr   => Descrittore Socket Client
112800151014     P* Parameter:      pOutSktAddr    => Indirizzo IP "dottato" del client
112900151009     P*--------------------------------------------------
113000151009     P*
113100151009     P UBSOCKET2_Accept...
113200151009     P                 B                   EXPORT
113300151009     D*
113400151009     D UBSOCKET2_Accept...
113500151009     D                 PI            10I 0
113600151126     D pInSktDescr                   10I 0 CONST
113700151126     D pOutSktDescr                  10I 0
113800151125     D pOutSktAddr                   15A
113900151009     D*
114000151009      //********************************************************************************************
114100151009      //
114200151009      // Definizione variabili work (locali)
114300151009      //
114400151009      //********************************************************************************************
114500151009     D locMethodCode   S             10I 0
114600151009     D
114700151009     D p_sockaddr      S               *
114800151009     D sockaddr        DS                  based(p_sockaddr)
114900151009     D   sa_family                    5I 0
115000151009     D   sa_data                     14A
115100151009     D sockaddr_in     DS                  based(p_sockaddr)
115200151009     D   sin_family                   5I 0
115300151009     D   sin_port                     5U 0
115400151009     D   sin_addr                    10U 0
115500151009     D   sin_zero                     8A
115600151009     D
115700151009     D p_connfrom      S               *
115800151009     D addrlen         S             10I 0
115900151009
116000151009
116100151009      /FREE
116200151009        // Inizializzo opcode di metodo
116300151009        locMethodCode = -1;
116400151009        InitDone = *off;
116500151009
116600151009        // Alloco la memoria necessaria a memorizzare la struttura del socket corrente
116700151009        addrlen = %size(sockaddr);
116800151009        p_connfrom = %alloc(addrlen);
116900151009
117000151009        // Servo la richiesta cliente "estraendola" dallo stack del server
117100151009        pOutSktDescr = accept(pInSktDescr : p_connfrom : addrlen);
117200151009
117300151009        // Se nuovo socket creato correttamente (Accept riuscita)
117400151009        if pOutSktDescr < *zeros;
117500151009        else;
117600151009           // Lunghezza "illegale" connessione TCP
117700151009           if addrlen <> 16;
117800151009           else;
117900151125
118000151125              p_sockaddr = p_connfrom;
118100151009
118200151012              if UBNUF2IDA_Convert(sin_addr:pOutInetDotAdr) = 0;
118300151012
118400151012                 // Valorizzo i parametri di output
118500151125                 pOutSktAddr = pOutInetDotAdr;
118600151012
118700151012                 // Valorizzo opcode di metodo
118800151012                 locMethodCode = 0;
118900151012                 InitDone = *on;
119000151126
119100151012              endif;
119200151009           endif;
119300151009        endif;
119400151009
119500151009
119600151009        return locMethodCode;
119700151009
119800151009      /END-FREE
119900151009
120000151009     P UBSOCKET2_Accept...
120100151009     P                 E
120200151127
120300151127
120400151127
120500151127     P*--------------------------------------------------
120600151127     P* Procedure name: UBSOCKET2_MngDescrAttrbs
120700151127     P* Purpose:        Manage Descriptors Attributes
120800151127     P* Returns:        Integer (Status_Code)
120900151127     P* Parameter:      pInSktDescr    => Descrittore Socket di cui gestire gli attributi
121000151127     P* Parameter:      pInCommand     => Comando da eseguire (metodo)
121100151127     P* Parameter:      pInCmdArg      => Argomento del comando (valore)
121200151127     P*--------------------------------------------------
121300151127     P*
121400151127     P UBSOCKET2_MngDescrAttrbs...
121500151127     P                 B                   EXPORT
121600151127     D*
121700151127     D UBSOCKET2_MngDescrAttrbs...
121800151127     D                 PI            10I 0
121900151127     D pInSktDescr                   10I 0 CONST
122000151127     D pInCommand                    10I 0 CONST
122100151127     D pInCmdArg                     10I 0 CONST
122200151127     D*
122300151127      //********************************************************************************************
122400151127      //
122500151127      // Definizione variabili work (locali)
122600151127      //
122700151127      //********************************************************************************************
122800151127     D locMethodCode   S             10I 0
122900151127     D
123000151127     D wErr            S               N   Inz(*off)
123100151127
123200151127
123300151127      /FREE
123400151127        // Inizializzo opcode di metodo
123500151127        locMethodCode = -1;
123600151127        InitDone = *off;
123700151127
123800151127        // A seconda del metodo richiesto
123900151127        // gestisco l'attributo richiesto per il socket richiesto
124000151127        select;
124100151127           when pInCommand = F_GETFL;
124200151127              if fcntl(pInSktDescr : F_GETFL) < 0;
124300151127
124400151127                 // Errore in gestione attributi descrittore socket
124500151127                 wErr = *on;
124600151127
124700151127               endif;
124800151127
124900151127           when pInCommand = F_SETFL;
125000151127              if fcntl(pInSktDescr : F_SETFL : pInCmdArg) < 0;
125100151127
125200151127                 // Errore in gestione attributi descrittore socket
125300151127                 wErr = *on;
125400151127
125500151127               endif;
125600151127
125700151127           other;
125800151127
125900151127              // Errore in passaggio parametri
126000151127              wErr = *on;
126100151127
126200151127        endsl;
126300151127
126400151127
126500151127        if not wErr;
126600151127
126700151127           // Valorizzo opcode di metodo
126800151127           locMethodCode = 0;
126900151127           InitDone = *on;
127000151127
127100151127        endif;
127200151127
127300151127
127400151127        return locMethodCode;
127500151127
127600151127      /END-FREE
127700151127
127800151127     P UBSOCKET2_MngDescrAttrbs...
127900151127     P                 E
128000151127
128100151127
128200151127
128300151127     P*--------------------------------------------------
128400151127     P* Procedure name: UBSOCKET2_Select
128500151127     P* Purpose:        Select socket descriptor (to manage)
128600151127     P* Returns:        Integer (Status_Code)
128700151127     P* Parameter:      pInMaxDescr    => Numero massimo Descrittori Socket "gestibili"
128800151127     P* Parameter:      pInReadSet     => Set di Descrittori Socket da "leggere"
128900151127     P* Parameter:      pInWriteSet    => Set di Descrittori Socket da "scrivere"
129000151127     P* Parameter:      pInExcpSet     => Set di Descrittori Socket con "eccezioni"
129100151127     P* Parameter:      pInWaitTime    => Intervallo di "polling" tra Descrittori Socket
129200151127     P*--------------------------------------------------
129300151127     P*
129400151127     P UBSOCKET2_Select...
129500151127     P                 B                   EXPORT
129600151127     D*
129700151127     D UBSOCKET2_Select...
129800151127     D                 PI            10I 0
129900151127     D pInMaxDescr                   10I 0 CONST
130000151127     D pInReadSet                    28A
130100151127     D pInWriteSet                   28A
130200151127     D pInExcpSet                    28A
130300151127     D pInWaitTime                   10I 0 CONST
130400151127     D*
130500151127      //********************************************************************************************
130600151127      //
130700151127      // Definizione variabili work (locali)
130800151127      //
130900151127      //********************************************************************************************
131000151127     D locMethodCode   S             10I 0
131100151127     D
131200151127
131300151127
131400151127      /FREE
131500151127        // Inizializzo opcode di metodo
131600151127        locMethodCode = -1;
131700151127        InitDone = *off;
131800151127
131900151127        // Alloco la memoria necessaria a memorizzare la struttura del timeval
132000151127        addrlen = %size(timeval);
132100151127        p_tv = %alloc(addrlen);
132200151127        p_timeval = p_tv;
132300151127
132400151127        tv_sec = %div(pInWaitTime:1000000);
132500151127        tv_usec = %rem(pInWaitTime:1000000);
132600151127
132700151127
132800151127        if select(pInMaxDescr+1 : %addr(pInReadSet) :
132900151127                  %addr(pInWriteSet) : %addr(pInExcpSet): p_timeval) < 0;
133000151127        else;
133100151127
133200151127           // Valorizzo opcode di metodo
133300151127           locMethodCode = 0;
133400151127           InitDone = *on;
133500151127
133600151127        endif;
133700151127
133800151127        return locMethodCode;
133900151127
134000151127      /END-FREE
134100151127
134200151127     P UBSOCKET2_Select...
134300151127     P                 E
134400151127
134500151127
134600151127
134700151127     P*--------------------------------------------------
134800151127     P* Procedure name: UBSOCKET2_FD_zero
134900151127     P* Purpose:        Clear all Descriptors in a set
135000151127     P* Returns:        ---
135100151127     P* Parameter:      pInFDset       => Descriptors' set
135200151127     P*--------------------------------------------------
135300151127     P*
135400151127     P UBSOCKET2_FD_zero...
135500151127     P                 B                   EXPORT
135600151127     D*
135700151127     D UBSOCKET2_FD_zero...
135800151127     D                 PI
135900151127     D pInFDset                      28A
136000151127     D*
136100151127      //********************************************************************************************
136200151127      //
136300151127      // Definizione variabili work (locali)
136400151127      //
136500151127      //********************************************************************************************
136600151127     D
136700151127
136800151127      /FREE
136900151127
137000151127        // Inizializzo il Set dei Descripts ricevuto in input
137100151127        pInFDset = *ALLx'00';
137200151127
137300151127      /END-FREE
137400151127
137500151127     P UBSOCKET2_FD_zero...
137600151127     P                 E
137700151127
137800151127
137900151127
138000151127     P*--------------------------------------------------
138100151127     P* Procedure name: UBSOCKET2_FD_set
138200151127     P* Purpose:        Set Descriptors in a set
138300151127     P* Returns:        ---
138400151127     P* Parameter:      pInFD          => Descriptors
138500151127     P* Parameter:      pInFDset       => Descriptors' set
138600151127     P*--------------------------------------------------
138700151127     P*
138800151127     P UBSOCKET2_FD_set...
138900151127     P                 B                   EXPORT
139000151127     D*
139100151127     D UBSOCKET2_FD_set...
139200151127     D                 PI
139300151127     D pInFD                         10I 0
139400151127     D pInFDset                      28A
139500151127     D*
139600151127      //********************************************************************************************
139700151127      //
139800151127      // Definizione variabili work (locali)
139900151127      //
140000151127      //********************************************************************************************
140100151127     D
140200151127     D wkByteNo        S              5I 0
140300151127     D wkMask          S              1A
140400151127     D wkByte          S              1A
140500151127     C*
140600151127     C                   callp     UBSOCKET2_CalcBitPos(pInFD:wkByteNo:wkMask)
140700151127     C                   eval      wkByte = %subst(pInFDset:wkByteNo:1)
140800151127     C                   biton     wkMask        wkByte
140900151127     C                   eval      %subst(pInFDset:wkByteNo:1) = wkByte
141000151127     C*
141100151127     P UBSOCKET2_FD_set...
141200151127     P                 E
141300151127
141400151127
141500151127
141600151127     P*--------------------------------------------------
141700151127     P* Procedure name: UBSOCKET2_FD_clr
141800151127     P* Purpose:        Clear Descriptors in a set
141900151127     P* Returns:        ---
142000151127     P* Parameter:      pInFD          => Descriptors
142100151127     P* Parameter:      pInFDset       => Descriptors' set
142200151127     P*--------------------------------------------------
142300151127     P*
142400151127     P UBSOCKET2_FD_clr...
142500151127     P                 B                   EXPORT
142600151127     D*
142700151127     D UBSOCKET2_FD_clr...
142800151127     D                 PI
142900151127     D pInFD                         10I 0
143000151127     D pInFDset                      28A
143100151127     D*
143200151127      //********************************************************************************************
143300151127      //
143400151127      // Definizione variabili work (locali)
143500151127      //
143600151127      //********************************************************************************************
143700151127     D
143800151127     D wkByteNo        S              5I 0
143900151127     D wkMask          S              1A
144000151127     D wkByte          S              1A
144100151127     C*
144200151127     C                   callp     UBSOCKET2_CalcBitPos(pInFD:wkByteNo:wkMask)
144300151127     C                   eval      wkByte = %subst(pInFDset:wkByteNo:1)
144400151127     C                   bitoff    wkMask        wkByte
144500151127     C                   eval      %subst(pInFDset:wkByteNo:1) = wkByte
144600151127     C*
144700151127     P UBSOCKET2_FD_clr...
144800151127     P                 E
144900151127
145000151127
145100151127
145200151127     P*--------------------------------------------------
145300151127     P* Procedure name: UBSOCKET2_FD_isSet
145400151127     P* Purpose:        Check if a Descriptor Is Set (or not)
145500151127     P* Returns:        Boolean (indicator *on/*off)
145600151127     P* Parameter:      pInFD          => Descriptors
145700151127     P* Parameter:      pInFDset       => Descriptors' set
145800151127     P*--------------------------------------------------
145900151127     P*
146000151127     P UBSOCKET2_FD_isSet...
146100151127     P                 B                   EXPORT
146200151127     D*
146300151127     D UBSOCKET2_FD_isSet...
146400151127     D                 PI             1N
146500151127     D pInFD                         10I 0
146600151127     D pInFDset                      28A
146700151127     D*
146800151127      //********************************************************************************************
146900151127      //
147000151127      // Definizione variabili work (locali)
147100151127      //
147200151127      //********************************************************************************************
147300151127     D
147400151127     D wkByteNo        S              5I 0
147500151127     D wkMask          S              1A
147600151127     D wkByte          S              1A
147700151127     C*
147800151127     C                   callp     UBSOCKET2_CalcBitPos(pInFD:wkByteNo:wkMask)
147900151127     C                   eval      wkByte = %subst(pInFDset:wkByteNo:1)
148000151127     C                   testb     wkMask        wkByte                   88
148100151127     C                   return    *IN88
148200151127     C*
148300151127     P UBSOCKET2_FD_isSet...
148400151127     P                 E
148500151127
148600151127
148700151127
148800151127     P*--------------------------------------------------
148900151127     P* Procedure name: UBSOCKET2_CalcBitPos...
149000151127     P* Purpose:        Calculate Descriptor BitMask
149100151127     P* Returns:        ---
149200151127     P* Parameter:      pInSktDescr    => Descriptor
149300151127     P* Parameter:      pInByteNo      => Byte's number
149400151127     P* Parameter:      pInBitMask     => Byte's BitMask
149500151127     P*--------------------------------------------------
149600151127     P*
149700151127     P UBSOCKET2_CalcBitPos...
149800151127     P                 B                   EXPORT
149900151127     D*
150000151127     D UBSOCKET2_CalcBitPos...
150100151127     D                 PI
150200151127     D pInSktDescr                   10I 0
150300151127     D pInByteNo                      5I 0
150400151127     D pInBitMask                     1A
150500151127     D*
150600151127      //********************************************************************************************
150700151127      //
150800151127      // Definizione variabili work (locali)
150900151127      //
151000151127      //********************************************************************************************
151100151127     D dsMakeMask      DS
151200151127     D   dsZeroByte            1      1A
151300151127     D   dsMask                2      2A
151400151127     D   dsBitMult             1      2U 0 Inz(0)
151500151127     D
151600151127     D wkGroup         S              2  0 Inz
151700151127     D wkByteNo        S              2  0 Inz
151800151127     D wkBitNo         S              2  0 Inz
151900151127     C*
152000151127     C     pInSktDescr   DIV       32            wkGroup
152100151127     C                   MVR                     wkByteNo
152200151127     C                   DIV       8             wkByteNo
152300151127     C                   MVR                     wkBitNo
152400151127     C*
152500151127     C                   eval      wkByteNo = 4 - wkByteNo
152600151127     C                   eval      pInByteNo = (wkGroup * 4) + wkByteNo
152700151127     C                   eval      dsBitMult = 2 ** wkBitNo
152800151127     C                   eval      dsZeroByte = x'00'
152900151127     C                   eval      pInBitMask = dsMask
153000151127     C*
153100151127     P UBSOCKET2_CalcBitPos...
153200151127     P                 E
