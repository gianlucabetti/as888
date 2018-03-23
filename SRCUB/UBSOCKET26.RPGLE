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
016600151216     **  if UBSOCKET2_Send(pInSktDescr : pInSktMsg : pInMsgCvtTbl ) = 0
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
060200151216     P* Parameter:      pInSktMsg      => Messaggio da sottomettere al socket destinazione via TCP
060300151216     P* Parameter:      pInMsgCvtTbl   => Tabella conversione messaggio socket
060400151008     P*--------------------------------------------------
060500151008     P*
060600151008     P UBSOCKET2_Send...
060700151008     P                 B                   EXPORT
060800151008     D*
060900151008     D UBSOCKET2_Send...
061000151008     D                 PI            10I 0
061100151126     D pInSktDescr                   10I 0 CONST
061200151216     D pInSktMsg                  16384A   CONST OPTIONS(*VARSIZE)
061300151216     D pInMsgCvtTbl                  10A                                        * QTCPASC QTCPEBC
061400151008     D*
061500151008      //********************************************************************************************
061600151008      //
061700151008      // Definizione variabili work (locali)
061800151008      //
061900151008      //********************************************************************************************
062000151008     D locMethodCode   S             10I 0
062100151008     D
062200151008     D Rcv             S             10I 0
062300151216     D SktMsg          S          16384A
062400151216     D SktMsgLenght    S             10I 0
062500151008
062600151008
062700151008      /FREE
062800151008        // Inizializzo opcode di metodo
062900151008        locMethodCode = -1;
063000151008        InitDone = *off;
063100151008
063200151008        // Verifica parametri "tabellati"
063300151216        if pInMsgCvtTbl = *blanks;
063400151216           pInMsgCvtTbl = 'QTCPASC';
063500151008        endif;
063600151008
063700151216        // Sottometto il messaggio al socket destinazione
063800151216        SktMsg = %trim(pInSktMsg);
063900151216        SktMsgLenght = %len(%trim(SktMsg));
064000151216        callp UBXLATENCD_Translate(SktMsgLenght : SktMsg : pInMsgCvtTbl);
064100151216        Rcv = send(pInSktDescr : %addr(SktMsg) : SktMsgLenght  : 0);
064200151216        if  Rcv < SktMsgLenght;
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
066100151216     P* Procedure name: UBSOCKET2_ReplyTo (UDP)
066200151216     P* Purpose:        ReplyTo socket (UDP)
066300151126     P* Returns:        Integer (Status_Code)
066400151126     P* Parameter:      pInSktDescr    => Descrittore Socket
066500151216     P* Parameter:      pInBufferLen   => Lunghezza buffer risposta messaggio al socket client UDP
066600151216     P* Parameter:      pInSktMsg      => Messaggio da sottomettere al socket destinazione via UDP
066700151216     P* Parameter:      pInMsgCvtTbl   => Tabella conversione messaggio socket
066800151216     P* Parameter:      pInSktAddrPtr  => Puntatore alla struttura socket_address di input
066900151216     P* Parameter:      pInPort        => Porta per risposta sul socket client
067000151126     P*--------------------------------------------------
067100151126     P*
067200151216     P UBSOCKET2_ReplyTo...
067300151126     P                 B                   EXPORT
067400151126     D*
067500151216     D UBSOCKET2_ReplyTo...
067600151126     D                 PI            10I 0
067700151126     D pInSktDescr                   10I 0 CONST
067800151216     D pInBufferLen                  10I 0 CONST
067900151216     D pInSktMsg                  16384A   CONST OPTIONS(*VARSIZE)
068000151216     D pInMsgCvtTbl                  10A                                        * QTCPASC QTCPEBC
068100151216     D pInSktAddrPtr                   *   CONST
068200151216     D pInPort                        5U 0 CONST
068300151126     D*
068400151126      //********************************************************************************************
068500151126      //
068600151126      // Definizione variabili work (locali)
068700151126      //
068800151126      //********************************************************************************************
068900151126     D locMethodCode   S             10I 0
069000151126     D
069100151216     D SktMsg          S          16384A
069200151216     D wDSsktAdr       DS                  likeds(sockaddr_in)
069300151216     D                                     based(p_connto)
069400151126
069500151126
069600151126      /FREE
069700151126        // Inizializzo opcode di metodo
069800151126        locMethodCode = -1;
069900151126        InitDone = *off;
070000151126
070100151216
070200151216        // Alloco la memoria necessaria a memorizzare la struttura del socket corrente
070300151216        addrlen = %size(sockaddr);
070400151216
070500151216        // Faccio puntare l'inririzzo della struttura del socket corrente al indirizzo
070600151216        // di memoria della struttura socket ricevuta in inoput
070700151216        p_connto = pInSktAddrPtr;
070800151126
070900151216        // Verifica parametri "tabellati"
071000151216        if pInMsgCvtTbl = *blanks;
071100151216           pInMsgCvtTbl = 'QTCPASC';
071200151216        endif;
071300151216
071400151216        // Verifica eventuale porta "forzata" richiesta in input
071500151216        // Se non specificata di default rimane la porta origine ricezione messaggio dal client
071600151216        if pInPort <> *zeros;
071700151216           wDSsktAdr.sin_port = pInPort;
071800151216        endif;
071900151126
072000151216
072100151216        // Sottometto il messaggio al socket "client"
072200151216        SktMsg = pInSktMsg;
072300151216        callp UBXLATENCD_Translate(pInBufferLen : SktMsg : pInMsgCvtTbl);
072400151216        if sendto(pInSktDescr : %addr(SktMsg) : pInBufferLen  : 0
072500151216                                   : p_connto : addrlen) < 0;
072600151216        else;
072700151126
072800151216            // Valorizzo opcode di metodo
072900151216            locMethodCode = 0;
073000151216            InitDone = *on;
073100151126
073200151216        endif;
073300151126
073400151126
073500151126        return locMethodCode;
073600151126
073700151126      /END-FREE
073800151126
073900151216     P UBSOCKET2_ReplyTo...
074000151126     P                 E
074100151013
074200151013
074300151013
074400151014     P*--------------------------------------------------
074500151126     P* Procedure name: UBSOCKET2_Receive (TCP)
074600151013     P* Purpose:        Read socket response
074700151013     P* Returns:        Integer (Status_Code)
074800151013     P* Parameter:      pInSktDescr    => Descrittore Socket
074900151126     P* Parameter:      pInBufferLen   => Lunghezza del buffer di lettura messaggio socket clientTCP
075000151216     P* Parameter:      pInMsgCvtTbl   => Codice tabella conversione
075100151013     P* Parameter:      pOutResponse   => Contenuto del messaggio socket
075200151013     P* Parameter:      pOutRespLen    => Lunghezza del messaggio socket
075300151013     P*--------------------------------------------------
075400151013     D*
075500151014     P UBSOCKET2_Receive...
075600151013     P                 B                   EXPORT
075700151013     D*
075800151014     D UBSOCKET2_Receive...
075900151013     D                 PI            10I 0
076000151126     D pInSktDescr                   10I 0 CONST
076100151126     D pInBufferLen                  10I 0 CONST
076200151013     D pInTypEOL                      1A   CONST
076300151216     D pInMsgCvtTbl                  10A                                        * QTCPASC QTCPEBC
076400151014     D pOutRespPtr                     *
076500151126     D pOutRespLen                   10I 0
076600151013     D*
076700151013      //********************************************************************************************
076800151013      //
076900151013      // Definizione variabili work (locali)
077000151013      //
077100151013      //********************************************************************************************
077200151013     D locMethodCode   S             10I 0
077300151013     D
077400151126     D Rcv             S             10I 0 Inz
077500151014     D wResponse       S          65536A   VARYING Based(pOutRespPtr)
077600151014     D wResponseLen    S             10I 0 Inz
077700151014     D wXlate          S          65536A   Inz
077800151013     D wChar           S              1A   Inz
077900151013     D wCR             S              1A   Inz(x'0D')
078000151013     D wLF             S              1A   Inz(x'0A')
078100151014     D wEOL            S              2A   Inz Varying
078200151013
078300151013
078400151013      /FREE
078500151013        // Inizializzo opcode di metodo
078600151013        locMethodCode = -1;
078700151013        InitDone = *off;
078800151014
078900151014
079000151013        // Verifica parametri "tabellati" relativi alla conversione del encoding dati
079100151216        if pInMsgCvtTbl = *blanks;
079200151216           pInMsgCvtTbl = 'QTCPEBC';
079300151013        endif;
079400151014
079500151013
079600151013        // Verifica parametri "tabellati" relativi alla conversione del encoding dati
079700151013        select;
079800151013          when pInTypEOL = '1';
079900151013               wEOL = wCR;
080000151013          when pInTypEOL = '2';
080100151014               wEOL = wLF;
080200151013          when pInTypEOL = '3';
080300151014               wEOL = wCR+wLF;
080400151013          other;
080500151014               wEOL = wCR+wLF;
080600151013        endsl;
080700151013
080800151013
080900151013        // Ricevo il messaggio byte-a-byte
081000151013        dou Rcv < 1;
081100151013
081200151013            // Inizializzo variabili di wrk
081300151014            wResponse    = %trim(' ');
081400151013            wResponseLen = *zeros;
081500151014            wXlate       = *blanks;
081600151014            wChar        = *blanks;
081700151013
081800151014            dou wResponseLen = %size(pInBufferLen) OR
081900151014                %scan(wChar : wEOL) > *zeros;
082000151014
082100151013                Rcv = recv(pInSktDescr : %addr(wChar) : 1 : 0);
082200151013                if Rcv < 1;
082300151013                   leave;
082400151013                endif;
082500151013
082600151014                if %scan(wChar : wEOL) = *zeros;
082700151013                   wResponseLen = wResponseLen + 1;
082800151014                   %subst(wResponse : wResponseLen : 1) = wChar;
082900151013                endif;
083000151013
083100151013            enddo;
083200151013
083300151013        enddo;
083400151013
083500151014
083600151013        if wResponseLen > *zeros;
083700151014
083800151216           if pInMsgCvtTbl <> '*NONE';
083900151014              wXlate = wResponse;
084000151216              callp UBXLATENCD_Translate(wResponseLen : wXlate : pInMsgCvtTbl);
084100151014              wResponse = wXlate;
084200151014           endif;
084300151014
084400151014           // Valorizzo opcode di metodo
084500151014           pOutRespPtr = %addr(wResponse);
084600151014           pOutRespLen = wResponseLen;
084700151014
084800151014           // Valorizzo opcode di metodo
084900151014           locMethodCode = 0;
085000151014           InitDone = *on;
085100151014
085200151013        endif;
085300151013
085400151013
085500151013        return locMethodCode;
085600151013
085700151013      /END-FREE
085800151013
085900151014     P UBSOCKET2_Receive...
086000151013     P                 E
086100151126
086200151126
086300151126
086400151126     P*--------------------------------------------------
086500151126     P* Procedure name: UBSOCKET2_ReceiveFrom (UDP)
086600151126     P* Purpose:        Read socket response
086700151126     P* Returns:        Integer (Status_Code)
086800151212     P* Parameter:      pInSktDescr    => Descrittore Socket Server UDP
086900151212     P* Parameter:      pInBufferLen   => Lunghezza buffer lettura messaggio socket client UDP
087000151212     P* Parameter:      pInTypEOL      => Tipo "End Of Line"
087100151216     P* Parameter:      pInMsgCvtTbl   => Codice tabella conversione
087200151212     P* Parameter:      pOutRespLen    => Lunghezza del messaggio socket
087300151212     P* Parameter:      pOutRespTxt    => Contenuto del messaggio socket (lunghezza variabile)
087400151212     P* Parameter:      pOutRespFull   => Flag messaggio socket completo Si/No
087500151216     P* Parameter:      pOutSktAddrPtr => Puntatore alla struttura socket_address di output
087600151217     P* Parameter:      pOutSktAddr    => Indirizzo IP "dottato" del client sorgente
087700151126     P*--------------------------------------------------
087800151126     D*
087900151126     P UBSOCKET2_ReceiveFrom...
088000151126     P                 B                   EXPORT
088100151212     D*
088200151126     D UBSOCKET2_ReceiveFrom...
088300151126     D                 PI            10I 0
088400151126     D pInSktDescr                   10I 0 CONST
088500151212     D pInBufferLen                  10I 0 CONST
088600151212     D pInTypEOL                      1A   CONST                                * CR / LF / CRLF
088700151216     D pInMsgCvtTbl                  10A                                        * QTCPASC QTCPEBC
088800151212     D pOutRespLen                   10I 0
088900151212     D pOutRespTxt                16384A   OPTIONS(*VARSIZE)
089000151212     D pOutRespFull                  10I 0                                      * 0=No 1=Yes 2=Undef
089100151216     D pOutSktAddrPtr                  *
089200151217     D pOutSktAddr                   15A
089300151126     D*
089400151126      //********************************************************************************************
089500151126      //
089600151126      // Definizione variabili work (locali)
089700151126      //
089800151126      //********************************************************************************************
089900151126     D locMethodCode   S             10I 0
090000151126     D
090100151212     D RcvDataLen      S             10I 0 Inz
090200151212     D wResponse       S          16384A   Inz
090300151126     D wXlate          S          65536A   Inz
090400151126     D wCR             S              1A   Inz(x'0D')
090500151126     D wLF             S              1A   Inz(x'0A')
090600151126     D wEOL            S              2A   Inz Varying
090700151212     D wEOLlen         S              1S 0 Inz
090800151126
090900151203
091000151212      /FREE
091100151126        // Inizializzo opcode di metodo
091200151126        locMethodCode = -1;
091300151126        InitDone = *off;
091400151212
091500151212
091600151212        // Inizializzo parametri di output
091700151212        clear pOutRespLen;
091800151212        clear pOutRespTxt;
091900151212        clear pOutRespFull;
092000151216        clear pOutSktAddrPtr;
092100151217        clear pOutSktAddr;
092200151212
092300151212
092400151212        // Inizializzo variabili di work (fondamentali)
092500151212        clear wResponse;
092600151212
092700151203
092800151203        // Verifica parametro di input relativo a lunghezza max buffer da ricevere
092900151212        if pInBufferLen > 16384;
093000151203           return locMethodCode;
093100151203        endif;
093200151126
093300151126
093400151126        // Verifica parametri "tabellati" relativi alla conversione del encoding dati
093500151216        if pInMsgCvtTbl = *blanks;
093600151216           pInMsgCvtTbl = 'QTCPEBC';
093700151126        endif;
093800151126
093900151126
094000151126        // Verifica parametri "tabellati" relativi alla conversione del encoding dati
094100151126        select;
094200151212          when pInTypEOL = '0';
094300151212               wEOL = '';
094400151212               wEOLlen = 0;
094500151126          when pInTypEOL = '1';
094600151126               wEOL = wCR;
094700151212               wEOLlen = 1;
094800151126          when pInTypEOL = '2';
094900151126               wEOL = wLF;
095000151212               wEOLlen = 1;
095100151126          when pInTypEOL = '3';
095200151126               wEOL = wCR+wLF;
095300151212               wEOLlen = 2;
095400151126          other;
095500151126               wEOL = wCR+wLF;
095600151212               wEOLlen = 2;
095700151126        endsl;
095800151212
095900151212
096000151212        // Alloco la memoria necessaria a memorizzare la struttura del socket corrente
096100151212        addrlen = %size(sockaddr);
096200151212        p_connfrom = %alloc(addrlen);
096300151212
096400151212
096500151212        // Ricevo il messaggio per intiero
096600151212        RcvDataLen = recvfrom(pInSktDescr : %addr(wResponse) :
096700151212                     pInBufferLen : 0 : p_connfrom : addrlen);
096800151212
096900151212
097000151212        // Se nessun dato ricevuto => esito negativo
097100151212        if RcvDataLen < 1;
097200151212        else;
097300151212
097400151212           // Verifico se messaggio ricevuto è completo (ovvero include terminatori
097500151212           // (ovvero include terminatori di acknowledgement)
097600151212           if wEOLlen > *zeros;
097700151212              if %scan(%trim(wEOL) : wResponse) > *zeros;
097800151212                 pOutRespFull = 1;
097900151212              endif;
098000151212           else;
098100151212              pOutRespFull = 2;
098200151212           endif;
098300151212
098400151212           // Se ricevuti dati ... se richiesto converto encoding
098500151216           if pInMsgCvtTbl <> '*NONE';
098600151212              wXlate = wResponse;
098700151216              callp UBXLATENCD_Translate(RcvDataLen : wXlate : pInMsgCvtTbl);
098800151212              wResponse = wXlate;
098900151212           endif;
099000151212
099100151212           // Se intercettato acknowledgment richiesto => lo elimino dal messaggio vero e proprio
099200151212           if pOutRespFull = 1;
099300151212              RcvDataLen = RcvDataLen-wEOLlen;
099400151212              wResponse = %subst(wResponse:1:RcvDataLen);
099500151212           endif;
099600151217
099700151217              // Converto l'indirizzo IP del cleint sorgente del messagio in indirizzip IP "dottato"
099800151217              if UBNUF2IDA_Convert(sin_addr:pOutInetDotAdr) = 0;
099900151217
100000151217                 // Valorizzo i parametri di output
100100151217                 pOutRespTxt = wResponse;
100200151217                 pOutRespLen = RcvDataLen;
100300151217                 pOutSktAddrPtr = p_connfrom;
100400151217                 pOutSktAddr = pOutInetDotAdr;
100500151126
100600151217                 // Valorizzo opcode di metodo
100700151217                 locMethodCode = 0;
100800151217                 InitDone = *on;
100900151217
101000151217              endif;
101100151126
101200151126        endif;
101300151126
101400151126
101500151126        return locMethodCode;
101600151126
101700151126      /END-FREE
101800151126
101900151126     P UBSOCKET2_ReceiveFrom...
102000151126     P                 E
102100151009
102200151009
102300151009
102400151009     P*--------------------------------------------------
102500151009     P* Procedure name: UBSOCKET2_Listen
102600151009     P* Purpose:        Create socket server
102700151009     P* Returns:        Integer (Status_Code)
102800151009     P* Parameter:      pInSktDescr    => Descrittore Socket
102900151009     P* Parameter:      pInBackLog     => Max client servibili (contemporaneamente)
103000151009     P*--------------------------------------------------
103100151009     P*
103200151009     P UBSOCKET2_Listen...
103300151009     P                 B                   EXPORT
103400151009     D*
103500151009     D UBSOCKET2_Listen...
103600151009     D                 PI            10I 0
103700151126     D pInSktDescr                   10I 0 CONST
103800151009     D pInBackLog                     5U 0 CONST
103900151009     D*
104000151009      //********************************************************************************************
104100151009      //
104200151009      // Definizione variabili work (locali)
104300151009      //
104400151009      //********************************************************************************************
104500151009     D locMethodCode   S             10I 0
104600151009     D
104700151012     D BackLog         S              5U 0 INZ(1024)
104800151009
104900151009
105000151009      /FREE
105100151009        // Inizializzo opcode di metodo
105200151009        locMethodCode = -1;
105300151009        InitDone = *off;
105400151009
105500151012        // Se non indicato un valore specifico per il back-log => assumo 1024
105600151009        if pInBackLog > *zeros;
105700151009           BackLog = pInBackLog;
105800151009        endif;
105900151009
106000151009        // Mi metto in ascolto (listening) sul socket server
106100151009        if listen(pInSktDescr : BackLog) < 0;
106200151009        else;
106300151009
106400151009           // Valorizzo opcode di metodo
106500151009           locMethodCode = 0;
106600151009           InitDone = *on;
106700151009
106800151009        endif;
106900151009
107000151009        return locMethodCode;
107100151009
107200151009      /END-FREE
107300151009
107400151009     P UBSOCKET2_Listen...
107500151009     P                 E
107600151009
107700151009
107800151009
107900151009     P*--------------------------------------------------
108000151126     P* Procedure name: UBSOCKET2_Accept
108100151009     P* Purpose:        Accept a socket client request
108200151009     P* Returns:        Integer (Status_Code)
108300151014     P* Parameter:      pInSktDescr    => Descrittore Socket Server
108400151014     P* Parameter:      pOutSktDescr   => Descrittore Socket Client
108500151014     P* Parameter:      pOutSktAddr    => Indirizzo IP "dottato" del client
108600151009     P*--------------------------------------------------
108700151009     P*
108800151009     P UBSOCKET2_Accept...
108900151009     P                 B                   EXPORT
109000151009     D*
109100151009     D UBSOCKET2_Accept...
109200151009     D                 PI            10I 0
109300151126     D pInSktDescr                   10I 0 CONST
109400151126     D pOutSktDescr                  10I 0
109500151125     D pOutSktAddr                   15A
109600151009     D*
109700151009      //********************************************************************************************
109800151009      //
109900151009      // Definizione variabili work (locali)
110000151009      //
110100151009      //********************************************************************************************
110200151009     D locMethodCode   S             10I 0
110300151009     D
110400151009
110500151009
110600151009      /FREE
110700151009        // Inizializzo opcode di metodo
110800151009        locMethodCode = -1;
110900151009        InitDone = *off;
111000151009
111100151009        // Alloco la memoria necessaria a memorizzare la struttura del socket corrente
111200151009        addrlen = %size(sockaddr);
111300151009        p_connfrom = %alloc(addrlen);
111400151009
111500151009        // Servo la richiesta cliente "estraendola" dallo stack del server
111600151009        pOutSktDescr = accept(pInSktDescr : p_connfrom : addrlen);
111700151009
111800151009        // Se nuovo socket creato correttamente (Accept riuscita)
111900151009        if pOutSktDescr < *zeros;
112000151009        else;
112100151009           // Lunghezza "illegale" connessione TCP
112200151009           if addrlen <> 16;
112300151009           else;
112400151125
112500151125              p_sockaddr = p_connfrom;
112600151009
112700151012              if UBNUF2IDA_Convert(sin_addr:pOutInetDotAdr) = 0;
112800151012
112900151012                 // Valorizzo i parametri di output
113000151125                 pOutSktAddr = pOutInetDotAdr;
113100151012
113200151012                 // Valorizzo opcode di metodo
113300151012                 locMethodCode = 0;
113400151012                 InitDone = *on;
113500151126
113600151012              endif;
113700151009           endif;
113800151009        endif;
113900151009
114000151009
114100151009        return locMethodCode;
114200151009
114300151009      /END-FREE
114400151009
114500151009     P UBSOCKET2_Accept...
114600151009     P                 E
114700151127
114800151127
114900151127
115000151127     P*--------------------------------------------------
115100151127     P* Procedure name: UBSOCKET2_MngDescrAttrbs
115200151127     P* Purpose:        Manage Descriptors Attributes
115300151127     P* Returns:        Integer (Status_Code)
115400151127     P* Parameter:      pInSktDescr    => Descrittore Socket di cui gestire gli attributi
115500151127     P* Parameter:      pInCommand     => Comando da eseguire (metodo)
115600151127     P* Parameter:      pInCmdArg      => Argomento del comando (valore)
115700151127     P*--------------------------------------------------
115800151127     P*
115900151127     P UBSOCKET2_MngDescrAttrbs...
116000151127     P                 B                   EXPORT
116100151127     D*
116200151127     D UBSOCKET2_MngDescrAttrbs...
116300151127     D                 PI            10I 0
116400151127     D pInSktDescr                   10I 0 CONST
116500151127     D pInCommand                    10I 0 CONST
116600151127     D pInCmdArg                     10I 0 CONST
116700151127     D*
116800151127      //********************************************************************************************
116900151127      //
117000151127      // Definizione variabili work (locali)
117100151127      //
117200151127      //********************************************************************************************
117300151127     D locMethodCode   S             10I 0
117400151127     D
117500151127     D wErr            S               N   Inz(*off)
117600151127
117700151127
117800151127      /FREE
117900151127        // Inizializzo opcode di metodo
118000151127        locMethodCode = -1;
118100151127        InitDone = *off;
118200151127
118300151127        // A seconda del metodo richiesto
118400151127        // gestisco l'attributo richiesto per il socket richiesto
118500151127        select;
118600151127           when pInCommand = F_GETFL;
118700151127              if fcntl(pInSktDescr : F_GETFL) < 0;
118800151127
118900151127                 // Errore in gestione attributi descrittore socket
119000151127                 wErr = *on;
119100151127
119200151127               endif;
119300151127
119400151127           when pInCommand = F_SETFL;
119500151127              if fcntl(pInSktDescr : F_SETFL : pInCmdArg) < 0;
119600151127
119700151127                 // Errore in gestione attributi descrittore socket
119800151127                 wErr = *on;
119900151127
120000151127               endif;
120100151127
120200151127           other;
120300151127
120400151127              // Errore in passaggio parametri
120500151127              wErr = *on;
120600151127
120700151127        endsl;
120800151127
120900151127
121000151127        if not wErr;
121100151127
121200151127           // Valorizzo opcode di metodo
121300151127           locMethodCode = 0;
121400151127           InitDone = *on;
121500151127
121600151127        endif;
121700151127
121800151127
121900151127        return locMethodCode;
122000151127
122100151127      /END-FREE
122200151127
122300151127     P UBSOCKET2_MngDescrAttrbs...
122400151127     P                 E
122500151127
122600151127
122700151127
122800151127     P*--------------------------------------------------
122900151127     P* Procedure name: UBSOCKET2_Select
123000151127     P* Purpose:        Select socket descriptor (to manage)
123100151127     P* Returns:        Integer (Status_Code)
123200151127     P* Parameter:      pInMaxDescr    => Numero massimo Descrittori Socket "gestibili"
123300151127     P* Parameter:      pInReadSet     => Set di Descrittori Socket da "leggere"
123400151127     P* Parameter:      pInWriteSet    => Set di Descrittori Socket da "scrivere"
123500151127     P* Parameter:      pInExcpSet     => Set di Descrittori Socket con "eccezioni"
123600151127     P* Parameter:      pInWaitTime    => Intervallo di "polling" tra Descrittori Socket
123700151127     P*--------------------------------------------------
123800151127     P*
123900151127     P UBSOCKET2_Select...
124000151127     P                 B                   EXPORT
124100151127     D*
124200151127     D UBSOCKET2_Select...
124300151127     D                 PI            10I 0
124400151127     D pInMaxDescr                   10I 0 CONST
124500151202     D pInReadSet                    28A   VALUE
124600151202     D pInWriteSet                   28A   VALUE
124700151202     D pInExcpSet                    28A   VALUE
124800151202     D pInWaitTime                   10I 0 VALUE
124900151127     D*
125000151127      //********************************************************************************************
125100151127      //
125200151127      // Definizione variabili work (locali)
125300151127      //
125400151127      //********************************************************************************************
125500151127     D locMethodCode   S             10I 0
125600151127     D
125700151127
125800151127
125900151127      /FREE
126000151127        // Inizializzo opcode di metodo
126100151127        locMethodCode = -1;
126200151127        InitDone = *off;
126300151127
126400151127        // Alloco la memoria necessaria a memorizzare la struttura del timeval
126500151127        addrlen = %size(timeval);
126600151127        p_tv = %alloc(addrlen);
126700151127        p_timeval = p_tv;
126800151127
126900151127        tv_sec = %div(pInWaitTime:1000000);
127000151127        tv_usec = %rem(pInWaitTime:1000000);
127100151127
127200151127
127300151127        if select(pInMaxDescr+1 : %addr(pInReadSet) :
127400151127                  %addr(pInWriteSet) : %addr(pInExcpSet): p_timeval) < 0;
127500151127        else;
127600151127
127700151127           // Valorizzo opcode di metodo
127800151127           locMethodCode = 0;
127900151127           InitDone = *on;
128000151127
128100151127        endif;
128200151127
128300151127        return locMethodCode;
128400151127
128500151127      /END-FREE
128600151127
128700151127     P UBSOCKET2_Select...
128800151127     P                 E
128900151127
129000151127
129100151127
129200151127     P*--------------------------------------------------
129300151127     P* Procedure name: UBSOCKET2_FD_zero
129400151127     P* Purpose:        Clear all Descriptors in a set
129500151127     P* Returns:        ---
129600151127     P* Parameter:      pInFDset       => Descriptors' set
129700151127     P*--------------------------------------------------
129800151127     P*
129900151127     P UBSOCKET2_FD_zero...
130000151127     P                 B                   EXPORT
130100151127     D*
130200151127     D UBSOCKET2_FD_zero...
130300151127     D                 PI
130400151127     D pInFDset                      28A
130500151127     D*
130600151127      //********************************************************************************************
130700151127      //
130800151127      // Definizione variabili work (locali)
130900151127      //
131000151127      //********************************************************************************************
131100151127     D
131200151127
131300151127      /FREE
131400151127
131500151127        // Inizializzo il Set dei Descripts ricevuto in input
131600151127        pInFDset = *ALLx'00';
131700151127
131800151127      /END-FREE
131900151127
132000151127     P UBSOCKET2_FD_zero...
132100151127     P                 E
132200151127
132300151127
132400151127
132500151127     P*--------------------------------------------------
132600151127     P* Procedure name: UBSOCKET2_FD_set
132700151127     P* Purpose:        Set Descriptors in a set
132800151127     P* Returns:        ---
132900151127     P* Parameter:      pInFD          => Descriptors
133000151127     P* Parameter:      pInFDset       => Descriptors' set
133100151127     P*--------------------------------------------------
133200151127     P*
133300151127     P UBSOCKET2_FD_set...
133400151127     P                 B                   EXPORT
133500151127     D*
133600151127     D UBSOCKET2_FD_set...
133700151127     D                 PI
133800151127     D pInFD                         10I 0
133900151127     D pInFDset                      28A
134000151127     D*
134100151127      //********************************************************************************************
134200151127      //
134300151127      // Definizione variabili work (locali)
134400151127      //
134500151127      //********************************************************************************************
134600151127     D
134700151127     D wkByteNo        S              5I 0
134800151127     D wkMask          S              1A
134900151127     D wkByte          S              1A
135000151127     C*
135100151127     C                   callp     UBSOCKET2_CalcBitPos(pInFD:wkByteNo:wkMask)
135200151127     C                   eval      wkByte = %subst(pInFDset:wkByteNo:1)
135300151127     C                   biton     wkMask        wkByte
135400151127     C                   eval      %subst(pInFDset:wkByteNo:1) = wkByte
135500151127     C*
135600151127     P UBSOCKET2_FD_set...
135700151127     P                 E
135800151127
135900151127
136000151127
136100151127     P*--------------------------------------------------
136200151127     P* Procedure name: UBSOCKET2_FD_clr
136300151127     P* Purpose:        Clear Descriptors in a set
136400151127     P* Returns:        ---
136500151127     P* Parameter:      pInFD          => Descriptors
136600151127     P* Parameter:      pInFDset       => Descriptors' set
136700151127     P*--------------------------------------------------
136800151127     P*
136900151127     P UBSOCKET2_FD_clr...
137000151127     P                 B                   EXPORT
137100151127     D*
137200151127     D UBSOCKET2_FD_clr...
137300151127     D                 PI
137400151127     D pInFD                         10I 0
137500151127     D pInFDset                      28A
137600151127     D*
137700151127      //********************************************************************************************
137800151127      //
137900151127      // Definizione variabili work (locali)
138000151127      //
138100151127      //********************************************************************************************
138200151127     D
138300151127     D wkByteNo        S              5I 0
138400151127     D wkMask          S              1A
138500151127     D wkByte          S              1A
138600151127     C*
138700151127     C                   callp     UBSOCKET2_CalcBitPos(pInFD:wkByteNo:wkMask)
138800151127     C                   eval      wkByte = %subst(pInFDset:wkByteNo:1)
138900151127     C                   bitoff    wkMask        wkByte
139000151127     C                   eval      %subst(pInFDset:wkByteNo:1) = wkByte
139100151127     C*
139200151127     P UBSOCKET2_FD_clr...
139300151127     P                 E
139400151127
139500151127
139600151127
139700151127     P*--------------------------------------------------
139800151127     P* Procedure name: UBSOCKET2_FD_isSet
139900151127     P* Purpose:        Check if a Descriptor Is Set (or not)
140000151127     P* Returns:        Boolean (indicator *on/*off)
140100151127     P* Parameter:      pInFD          => Descriptors
140200151127     P* Parameter:      pInFDset       => Descriptors' set
140300151127     P*--------------------------------------------------
140400151127     P*
140500151127     P UBSOCKET2_FD_isSet...
140600151127     P                 B                   EXPORT
140700151127     D*
140800151127     D UBSOCKET2_FD_isSet...
140900151127     D                 PI             1N
141000151127     D pInFD                         10I 0
141100151127     D pInFDset                      28A
141200151127     D*
141300151127      //********************************************************************************************
141400151127      //
141500151127      // Definizione variabili work (locali)
141600151127      //
141700151127      //********************************************************************************************
141800151127     D
141900151127     D wkByteNo        S              5I 0
142000151127     D wkMask          S              1A
142100151127     D wkByte          S              1A
142200151127     C*
142300151127     C                   callp     UBSOCKET2_CalcBitPos(pInFD:wkByteNo:wkMask)
142400151127     C                   eval      wkByte = %subst(pInFDset:wkByteNo:1)
142500151127     C                   testb     wkMask        wkByte                   88
142600151127     C                   return    *IN88
142700151127     C*
142800151127     P UBSOCKET2_FD_isSet...
142900151127     P                 E
143000151127
143100151127
143200151127
143300151127     P*--------------------------------------------------
143400151127     P* Procedure name: UBSOCKET2_CalcBitPos...
143500151127     P* Purpose:        Calculate Descriptor BitMask
143600151127     P* Returns:        ---
143700151127     P* Parameter:      pInSktDescr    => Descriptor
143800151127     P* Parameter:      pInByteNo      => Byte's number
143900151127     P* Parameter:      pInBitMask     => Byte's BitMask
144000151127     P*--------------------------------------------------
144100151127     P*
144200151127     P UBSOCKET2_CalcBitPos...
144300151127     P                 B                   EXPORT
144400151127     D*
144500151127     D UBSOCKET2_CalcBitPos...
144600151127     D                 PI
144700151127     D pInSktDescr                   10I 0
144800151127     D pInByteNo                      5I 0
144900151127     D pInBitMask                     1A
145000151127     D*
145100151127      //********************************************************************************************
145200151127      //
145300151127      // Definizione variabili work (locali)
145400151127      //
145500151127      //********************************************************************************************
145600151127     D dsMakeMask      DS
145700151127     D   dsZeroByte            1      1A
145800151127     D   dsMask                2      2A
145900151127     D   dsBitMult             1      2U 0 Inz(0)
146000151127     D
146100151127     D wkGroup         S              2  0 Inz
146200151127     D wkByteNo        S              2  0 Inz
146300151127     D wkBitNo         S              2  0 Inz
146400151127     C*
146500151127     C     pInSktDescr   DIV       32            wkGroup
146600151127     C                   MVR                     wkByteNo
146700151127     C                   DIV       8             wkByteNo
146800151127     C                   MVR                     wkBitNo
146900151127     C*
147000151127     C                   eval      wkByteNo = 4 - wkByteNo
147100151127     C                   eval      pInByteNo = (wkGroup * 4) + wkByteNo
147200151127     C                   eval      dsBitMult = 2 ** wkBitNo
147300151127     C                   eval      dsZeroByte = x'00'
147400151127     C                   eval      pInBitMask = dsMask
147500151127     C*
147600151127     P UBSOCKET2_CalcBitPos...
147700151127     P                 E
