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
001900151014     ** 1         OUT UNSIGNED  Descrittore Socket
002000071128     ** --------- --- --------- ------------------------------
002100090713     **
002200090713     **
002300090713     **
002400090713     ** --------- --- --------- ------------------------------
002500090713     **
002600151008     ** PARAMETRI DELLA SOTTOPROCEDURA: UBSOCKET2_Garbage
002700090713     **
002800090713     ** Posizione Uso Tipo      Descrizione
002900090713     ** --------- --- --------- ------------------------------
003000151014     ** 1         IN  UNSIGNED  Descrittore Socket
003100090713     ** --------- --- --------- ------------------------------
003200090713     **
003300071129     **
003400090713     **
003500151008     ** PARAMETRI DELLA SOTTOPROCEDURA UBSOCKET2_Connect
003600071129     **
003700071129     ** Posizione Uso Tipo      Descrizione
003800071129     ** --------- --- --------- ------------------------------
003900151014     ** 1         IN  UNSIGNED  Descrittore Socket
004000151009     ** 2         IN  CHARACTER Ip Address "dotted"/name       => Indirizzo IP "puntato" o nome
004100090713     ** 3         IN  UNSIGNED  Port number                    => Numero porta del socket "server"
004200071129     ** --------- --- --------- ------------------------------
004300151009     **
004400151009     **
004500151009     **
004600151009     ** PARAMETRI DELLA SOTTOPROCEDURA UBSOCKET2_Bind
004700151009     **
004800151009     ** Posizione Uso Tipo      Descrizione
004900151009     ** --------- --- --------- ------------------------------
005000151014     ** 1         IN  UNSIGNED  Descrittore Socket
005100151009     ** 2         IN  CHARACTER Local network interface        => Indirizzo interfaccia rete locale
005200151009     ** 3         IN  UNSIGNED  Port number                    => Numero porta del socket "server"
005300151009     ** --------- --- --------- ------------------------------
005400151009     **
005500151009     **
005600151009     **
005700151009     ** PARAMETRI DELLA SOTTOPROCEDURA UBSOCKET2_Listen
005800151009     **
005900151009     ** Posizione Uso Tipo      Descrizione
006000151009     ** --------- --- --------- ------------------------------
006100151014     ** 1         IN  UNSIGNED  Descrittore Socket
006200151009     ** 2         IN  UNSIGNED  Bacl-log number                => Numero max client servibili
006300151009     ** --------- --- --------- ------------------------------
006400151013     **
006500151013     **
006600151013     **
006700151013     ** PARAMETRI DELLA SOTTOPROCEDURA UBSOCKET2_Accept
006800151013     **
006900151013     ** Posizione Uso Tipo      Descrizione
007000151013     ** --------- --- --------- ------------------------------
007100151014     ** 1         IN  UNSIGNED  Descrittore Socket Server
007200151014     ** 2         OUT UNSIGNED  Descrittore Socket Client
007300151013     ** 3         OUT UNSIGNED  Indirizzo IP "dottato" del Client
007400151013     ** --------- --- --------- ------------------------------
007500090713     **
007600090713     **
007700090713     **
007800151008     ** PARAMETRI DELLA SOTTOPROCEDURA UBSOCKET2_Send
007900090713     **
008000090713     ** Posizione Uso Tipo      Descrizione
008100090713     ** --------- --- --------- ------------------------------
008200151014     ** 1         IN  UNSIGNED  Descrittore Socket
008300090713     ** 2         IN  CHARACTER Comado Socket da sottomettere
008400090713     ** 3         IN  CHARACTER Codice tabella conversione
008500090713     ** --------- --- --------- ------------------------------
008600151014     **
008700151014     **
008800151014     **
008900151014     ** PARAMETRI DELLA SOTTOPROCEDURA UBSOCKET2_Receive
009000151014     **
009100151014     ** Posizione Uso Tipo      Descrizione
009200151014     ** --------- --- --------- ------------------------------
009300151014     ** 1         IN  UNSIGNED  Descrittore Socket Server
009400151014     ** 2         IN  INTEGER   Lunghezza del buffer di lettura messaggio socket client
009500151014     ** 3         IN  CHARACTER Tipo "End Of Line"
009600151014     ** 4         IN  CHARACTER Codice tabella conversione
009700151014     ** 5         OUT POINTER   Puntatore al contenuto del messaggio socket
009800151014     ** 6         OUT INTEGER   Lunghezza del messaggio socket
009900151014     ** --------- --- --------- ------------------------------
010000151008     **
010100151008     **
010200151008     **
010300151008     ** PARAMETRI DELLA SOTTOPROCEDURA UBSOCKET2_SendRcv
010400151008     **
010500151008     ** Posizione Uso Tipo      Descrizione
010600151008     ** --------- --- --------- ------------------------------
010700151014     ** 1         IN  UNSIGNED  Descrittore Socket
010800151008     ** 2         IN  CHARACTER Comado Socket da sottomettere
010900151008     ** 3         IN  CHARACTER Codice tabella conversione
011000151008     ** 4         OUT INTEGER   Lunghezza buffer Output
011100151008     ** 5         OUT CHARACTER Response in Output
011200151008     ** --------- --- --------- ------------------------------
011300080612     **
011400071128     **
011500071128     **
011600071128     ** ESEMPI DI CHIAMATA
011700071128     **
011800151008     **  if UBSOCKET2_Create(pOutSktDescr) = 0;
011900100621     **
012000151008     **  if UBSOCKET2_Garbage(pInSktDescr) = 0;
012100090713     **
012200151008     **  if UBSOCKET2_Connect(pInSktDescr : pInHostAdr : pInPort) = 0;
012300090713     **
012400151008     **  if UBSOCKET2_Send(pInSktDescr : pInSktCmd : pInCmdCvtTbl ) = 0
012500080612     **
012600080612     ** --------- --- --------- ------------------------------
012700071128     **
012800071128     ** ISTRUZIONI PER LA COMPILAZIONE
012900071128     **
013000151125     ** 1. Creare il modulo UBSOCKET2 (Opz 15 PDM) indicando BNDDIR('UBBNDDIR':'QC2LE')
013100151008     ** 2. Creare/Aggiornare il programma di servizio UBSOCKET2 (CRTSRVPGM / UPDSRVPGM)
013200080610     ** 4. Cancellare il modulo.
013300071128     **
013400071128     ***********************************************************************************************
013500080612
013600080612
013700080612      //********************************************************************************************
013800080612      //
013900080612      // Definizione file (globali)
014000080612      //
014100080612      //********************************************************************************************
014200071128
014300080612
014400071128      //********************************************************************************************
014500071128      //
014600080612      // Definizione prototipi procedure (globali)
014700071128      //
014800071128      //********************************************************************************************
014900151008     D/COPY GAITRASRC/SRCPROTOPR,UBSOCKET2
015000151012     D/COPY GAITRASRC/SRCPROTOPR,UBCVTIDA
015100100922     D/COPY GAITRASRC/SRCPROTOPI,UBCVTIDA
015200151012     D/COPY GAITRASRC/SRCPROTOPR,UBNUF2IDA
015300151012     D/COPY GAITRASRC/SRCPROTOPI,UBNUF2IDA
015400100922     D/COPY GAITRASRC/SRCPROTOPR,UBDNSLKP
015500100922     D/COPY GAITRASRC/SRCPROTOPI,UBDNSLKP
015600100922     D/COPY GAITRASRC/SRCPROTOPR,SOCKET
015700090710     D/COPY GAITRASRC/SRCPROTOPR,INET_ADDR
015800151014     D/COPY GAITRASRC/SRCPROTOPR,UBXLATENCD
015900080612
016000080612
016100071128      //********************************************************************************************
016200071128      //
016300080612      // Definizione variabili work (globali)
016400071128      //
016500071128      //********************************************************************************************
016600080612     D InitDone        S               N   INZ(*off)
016700090710     D
016800100612     D* Socket - Address Family
016900100612     D AF_INET         C                   CONST(2)                             * TCP
017000100612     D AF_IPX          C                   CONST(6)                             * IPX/SPX
017100100612     D AF_NETBIOS      C                   CONST(17)                            * NetBIOS
017200100612     D AF_INET6        C                   CONST(23)                            * TCP
017300100612     D AF_IRDA         C                   CONST(26)                            * Infrared
017400100612     D AF_BTH          C                   CONST(32)                            * BlueTooth
017500151009     D
017600151009     D* Socket - Internet Address special value
017700151009     D INADDR_ANY      C                   CONST(0)
017800151009     D INADDR_BROADCAST...
017900151009     D                 C                   CONST(4294967295)
018000151009     D INADDR_LOOPBACK...
018100151009     D                 C                   CONST(2130706433)
018200100612     D* Socket - Type
018300100612     D SOCK_STREAM     C                   CONST(1)                             * TCP
018400100612     D SOCK_DGRAM      C                   CONST(2)                             * UDP
018500100612     D SOCK_RAW        C                   CONST(3)                             * IP Datagram
018600100612     D SOCK_RDM        C                   CONST(4)                             * PGM
018700100612     D SOCK_SEQPACKET  C                   CONST(5)                             * ???
018800100612     D
018900100612     D* Socket - Protocol
019000100612     D IPPROTO_IP      C                   CONST(0)                             * Generic IP
019100100612     D IPPROTO_ICMP    C                   CONST(1)                             * ICMP
019200100612     D BTHPROTO_RFCOM  C                   CONST(3)                             * BlueTooth
019300100612     D IPPROTO_TCP     C                   CONST(6)                             * TCP
019400100612     D IPPROTO_UDP     C                   CONST(17)                            * UDP
019500100612     D IPPROTO_ICMPV6  C                   CONST(58)                            * ICMP V6
019600100612     D IPPROTO_RM      C                   CONST(113)                           * PGM
019700071129
019800080612
019900080612     P*--------------------------------------------------
020000151008     P* Procedure name: UBSOCKET2_Create
020100080612     P* Purpose:        Inizializzazioni
020200090713     P* Returns:        Integer (Status_Code)
020300090713     P* Parameter:      pOutSktDescr   => Descrittore Socket
020400080612     P*--------------------------------------------------
020500080612     P*
020600151008     P UBSOCKET2_Create...
020700080612     P                 B                   EXPORT
020800080612     D*
020900151008     D UBSOCKET2_Create...
021000080612     D                 PI            10I 0
021100151125     D pOutSktDescr                  10U 0
021200080612     D*
021300080612      //********************************************************************************************
021400080612      //
021500080612      // Definizione variabili work (locali)
021600080612      //
021700080612      //********************************************************************************************
021800080612     D locMethodCode   S             10I 0
021900090710     D
022000080612
022100090710
022200080612      /FREE
022300080612        // Inizializzo opcode di metodo
022400080612        locMethodCode = -1;
022500080612        InitDone = *off;
022600080616
022700090710        // Definisco un socket (tipo e modalità)
022800090713        pOutSktDescr = socket(AF_INET:SOCK_STREAM:IPPROTO_TCP);
022900090713        if pOutSktDescr < 0;
023000090710        else;
023100090710
023200080616           // Valorizzo opcode di metodo
023300080616           locMethodCode = 0;
023400080616           InitDone = *on;
023500080616
023600080616        endif;
023700080612
023800080612        return locMethodCode;
023900080612
024000080612      /END-FREE
024100080612
024200151008     P UBSOCKET2_Create...
024300080612     P                 E
024400080612
024500080612
024600080612
024700080612     P*--------------------------------------------------
024800151008     P* Procedure name: UBSOCKET2_Garbage
024900100612     P* Purpose:        Garbage collection
025000090713     P* Returns:        Integer (Status_Code)
025100090713     P* Parameter:      pInSktDescr    => Descrittore Socket
025200080612     P*--------------------------------------------------
025300080612     P*
025400151008     P UBSOCKET2_Garbage...
025500080612     P                 B                   EXPORT
025600080612     D*
025700151008     D UBSOCKET2_Garbage...
025800080612     D                 PI            10I 0
025900090713     D pInSktDescr                   10U 0 CONST
026000080612     D*
026100080612      //********************************************************************************************
026200080612      //
026300080612      // Definizione variabili work (locali)
026400080612      //
026500080612      //********************************************************************************************
026600080612     D locMethodCode   S             10I 0
026700080612
026800080612      /FREE
026900080612        // Inizializzo opcode di metodo
027000080612        locMethodCode = -1;
027100080612
027200090710        // Chiudo il socket
027300090713        callp close(pInSktDescr);
027400080612
027500080612        // Valorizzo opcode di metodo
027600080612        locMethodCode = 0;
027700080612
027800080612        return locMethodCode;
027900080612
028000080612      /END-FREE
028100080612
028200151008     P UBSOCKET2_Garbage...
028300080612     P                 E
028400090710
028500090710
028600090710
028700090710     P*--------------------------------------------------
028800151008     P* Procedure name: UBSOCKET2_Connect
028900090710     P* Purpose:        Connect to a socket "server"
029000090710     P* Returns:        Integer (Status_Code)
029100090713     P* Parameter:      pInSktDescr    => Descrittore Socket
029200100922     P* Parameter:      pInHostAdr     => Host remoto
029300090710     P* Parameter:      pInPort        => Numero porta
029400090710     P*--------------------------------------------------
029500090710     P*
029600151008     P UBSOCKET2_Connect...
029700090710     P                 B                   EXPORT
029800090710     D*
029900151008     D UBSOCKET2_Connect...
030000090710     D                 PI            10I 0
030100090713     D pInSktDescr                   10U 0 CONST
030200100922     D pInHost                      256A   CONST
030300090710     D pInPort                        5U 0 CONST
030400090710     D*
030500090710      //********************************************************************************************
030600090710      //
030700090710      // Definizione variabili work (locali)
030800090710      //
030900090710      //********************************************************************************************
031000090710     D locMethodCode   S             10I 0
031100090710     D
031200090710     D p_sockaddr      S               *
031300090710     D sockaddr        DS                  based(p_sockaddr)
031400090710     D   sa_family                    5I 0
031500090710     D   sa_data                     14A
031600090710     D sockaddr_in     DS                  based(p_sockaddr)
031700090710     D   sin_family                   5I 0
031800090710     D   sin_port                     5U 0
031900090710     D   sin_addr                    10U 0
032000090710     D   sin_zero                     8A
032100090710     D
032200090710     D p_connto        S               *
032300090710     D addrlen         S             10I 0
032400100922     D IpAddr          S             15A   inz
032500090710
032600090710
032700090710      /FREE
032800090710        // Inizializzo opcode di metodo
032900090710        locMethodCode = -1;
033000090710        InitDone = *off;
033100090710
033200090710        // Alloco la memoria necessaria a memorizzare la struttura del socket corrente
033300090710        addrlen = %size(sockaddr);
033400090710        p_connto = %alloc(addrlen);
033500090710
033600090710        // Faccio puntare l'inririzzo della struttura del socket corrente alla nuova
033700090710        // area di memoria appena allocata
033800090710        p_sockaddr = p_connto;
033900090710
034000100922
034100151008
034200151008        // Effettuo il DNS Lookup x reperire SEMPRE l'indirizzo "puntato"
034300151008        callp UBDNSLKP_Retrieve(pInHost : pOutInetFound : pOutInetType);
034400151008
034500151008          if pOutInetType = 'NAM';
034600151008             IpAddr = %trim(pOutInetFound);
034700151008          else;
034800151008             IpAddr = %trim(pInHost);
034900151008          endif;
035000100922
035100100922          // Converto l'indirizzo internet "puntato" nel relativo valore numerico unsigned
035200100922          if UBCVTIDA_Convert(IpAddr:pOutInetAdrVal) = 0;
035300090710
035400100922             // Popolo la struttura del socket
035500100922             // Note that IP is the ip address we previously looked up
035600100922             // using the inet_addr and/or gethostbyname APIs
035700100922             // and port is the port number that we looked up using the
035800100922             // getservbyname API.
035900100922             sin_family = AF_INET;
036000100922             sin_addr = pOutInetAdrVal;
036100100922             sin_port = pInPort;
036200100922             sin_zero = *ALLX'00';
036300100922
036400100922             // Mi connetto al socket "server"
036500100922             if connect(pInSktDescr : p_connto : addrlen) < 0;
036600100922             else;
036700090710
036800100922                // Valorizzo opcode di metodo
036900100922                locMethodCode = 0;
037000100922                InitDone = *on;
037100090710
037200100922             endif;
037300110225          endif;
037400090710
037500090710        return locMethodCode;
037600090710
037700090710      /END-FREE
037800090710
037900151008     P UBSOCKET2_Connect...
038000090710     P                 E
038100151009
038200151009
038300151009
038400151009     P*--------------------------------------------------
038500151009     P* Procedure name: UBSOCKET2_Bind
038600151009     P* Purpose:        Bind on a local address/port
038700151009     P* Returns:        Integer (Status_Code)
038800151009     P* Parameter:      pInSktDescr    => Descrittore Socket
038900151009     P* Parameter:      pInLocalAdr    => Local Address
039000151009     P* Parameter:      pInLocalPort   => Numero porta
039100151009     P*--------------------------------------------------
039200151009     P*
039300151009     P UBSOCKET2_Bind...
039400151009     P                 B                   EXPORT
039500151009     D*
039600151009     D UBSOCKET2_Bind...
039700151009     D                 PI            10I 0
039800151009     D pInSktDescr                   10U 0 CONST
039900151009     D pInLocalAdr                  256A   CONST
040000151009     D pInLocalPort                   5U 0 CONST
040100151009     D*
040200151009      //********************************************************************************************
040300151009      //
040400151009      // Definizione variabili work (locali)
040500151009      //
040600151009      //********************************************************************************************
040700151009     D locMethodCode   S             10I 0
040800151009     D
040900151009     D p_sockaddr      S               *
041000151009     D sockaddr        DS                  based(p_sockaddr)
041100151009     D   sa_family                    5I 0
041200151009     D   sa_data                     14A
041300151009     D sockaddr_in     DS                  based(p_sockaddr)
041400151009     D   sin_family                   5I 0
041500151009     D   sin_port                     5U 0
041600151009     D   sin_addr                    10U 0
041700151009     D   sin_zero                     8A
041800151009     D
041900151009     D p_bindto        S               *
042000151009     D addrlen         S             10I 0
042100151009     D IpAddr          S             15A   inz
042200151009
042300151009
042400151009      /FREE
042500151009        // Inizializzo opcode di metodo
042600151009        locMethodCode = -1;
042700151009        InitDone = *off;
042800151009
042900151009        // Alloco la memoria necessaria a memorizzare la struttura del socket corrente
043000151009        addrlen = %size(sockaddr);
043100151009        p_bindto = %alloc(addrlen);
043200151009
043300151009        // Faccio puntare l'indirizzo della struttura del socket corrente alla nuova
043400151009        // area di memoria appena allocata
043500151009        p_sockaddr = p_bindto;
043600151009
043700151009        // Inizializzo il valore del socket address
043800151009        sin_addr = *hival;
043900151009
044000151009        // Gestisco eventuali valori speciali per l'indirizzo di binding
044100151009        select;
044200151009          when %trim(pInLocalAdr) = 'INADDR_ANY';
044300151009             sin_addr = INADDR_ANY;
044400151009
044500151009          when %trim(pInLocalAdr) = 'INADDR_LOOPBACK';
044600151009             sin_addr = INADDR_LOOPBACK;
044700151009
044800151009          other;
044900151009            // Effettuo il DNS Lookup x reperire SEMPRE l'indirizzo "puntato"
045000151009            callp UBDNSLKP_Retrieve(pInLocalAdr : pOutInetFound : pOutInetType);
045100151009
045200151009            if pOutInetType = 'NAM';
045300151009               IpAddr = %trim(pOutInetFound);
045400151009            else;
045500151009               IpAddr = %trim(pInLocalAdr);
045600151009            endif;
045700151009
045800151009            // Converto l'indirizzo internet "puntato" nel relativo valore numerico unsigned
045900151009            if UBCVTIDA_Convert(IpAddr:pOutInetAdrVal) = 0;
046000151009
046100151009               sin_addr = pOutInetAdrVal;
046200151009
046300151009            endif;
046400151009        endsl;
046500151009
046600151009
046700151009        // Se a questo punto il socket address è valorizzato => proseguo
046800151009        if sin_addr <> *hival;
046900151009
047000151009           // Popolo la struttura del socket
047100151009           sin_family = AF_INET;
047200151009           sin_port = pInLocalPort;
047300151009           sin_zero = *ALLX'00';
047400151009
047500151009           // Mi bindo (ovvero "preparo indirizzo/porta" per il server)
047600151009           if bind(pInSktDescr : p_bindto : addrlen) < 0;
047700151009           else;
047800151009
047900151009              // Valorizzo opcode di metodo
048000151009              locMethodCode = 0;
048100151009              InitDone = *on;
048200151009
048300151009           endif;
048400151009
048500151009        endif;
048600151009
048700151009        return locMethodCode;
048800151009
048900151009      /END-FREE
049000151009
049100151009     P UBSOCKET2_Bind...
049200151009     P                 E
049300151008
049400151008
049500151008
049600151008     P*--------------------------------------------------
049700151008     P* Procedure name: UBSOCKET2_Send
049800151008     P* Purpose:        Send socket command
049900151008     P* Returns:        Integer (Status_Code)
050000151008     P* Parameter:      pInSktDescr    => Descrittore Socket
050100151008     P* Parameter:      pInSktCmd      => Comando da sottomettere al socket "server"
050200151008     P* Parameter:      pInCmdCvtTbl   => Tabella conversione comando socket
050300151008     P*--------------------------------------------------
050400151008     P*
050500151008     P UBSOCKET2_Send...
050600151008     P                 B                   EXPORT
050700151008     D*
050800151008     D UBSOCKET2_Send...
050900151008     D                 PI            10I 0
051000151008     D pInSktDescr                   10U 0 CONST
051100151008     D pInSktCmd                   1024A   CONST
051200151125     D pInCmdCvtTbl                  10A                                        * QTCPASC QTCPEBC
051300151008     D*
051400151008      //********************************************************************************************
051500151008      //
051600151008      // Definizione variabili work (locali)
051700151008      //
051800151008      //********************************************************************************************
051900151008     D locMethodCode   S             10I 0
052000151008     D
052100151008     D Rcv             S             10I 0
052200151008     D SktCmd          S           1024A
052300151008     D SktCmdLenght    S             10I 0
052400151008
052500151008
052600151008      /FREE
052700151008        // Inizializzo opcode di metodo
052800151008        locMethodCode = -1;
052900151008        InitDone = *off;
053000151008
053100151008        // Verifica parametri "tabellati"
053200151008        if pInCmdCvtTbl = *blanks;
053300151008           pInCmdCvtTbl = 'QTCPASC';
053400151008        endif;
053500151008
053600151008        // Sottometto il comando al socket "server"
053700151008        SktCmd = %trim(pInSktCmd);
053800151008        SktCmdLenght = %len(%trim(SktCmd));
053900151014        callp UBXLATENCD_Translate(SktCmdLenght : SktCmd : pInCmdCvtTbl);
054000151008        Rcv = send(pInSktDescr : %addr(SktCmd) : SktCmdLenght  : 0);
054100151008        If  Rcv < SktCmdLenght;
054200151008        Else;
054300151008
054400151008           // Valorizzo opcode di metodo
054500151008           locMethodCode = 0;
054600151008           InitDone = *on;
054700151008
054800151008        endif;
054900151008
055000151008        return locMethodCode;
055100151008
055200151008      /END-FREE
055300151008
055400151008     P UBSOCKET2_Send...
055500151008     P                 E
055600151013
055700151013
055800151013
055900151014     P*--------------------------------------------------
056000151014     P* Procedure name: UBSOCKET2_Receive
056100151013     P* Purpose:        Read socket response
056200151013     P* Returns:        Integer (Status_Code)
056300151013     P* Parameter:      pInSktDescr    => Descrittore Socket
056400151014     P* Parameter:      pInBufferLen   => Lunghezza del buffer di lettura messaggio socket client
056500151013     P* Parameter:      pInCmdCvtTbl   => Codice tabella conversione
056600151013     P* Parameter:      pOutResponse   => Contenuto del messaggio socket
056700151013     P* Parameter:      pOutRespLen    => Lunghezza del messaggio socket
056800151013     P*--------------------------------------------------
056900151013     D*
057000151014     P UBSOCKET2_Receive...
057100151013     P                 B                   EXPORT
057200151013     D*
057300151014     D UBSOCKET2_Receive...
057400151013     D                 PI            10I 0
057500151013     D pInSktDescr                   10U 0 CONST
057600151014     D pInBufferLen                  10U 0 CONST
057700151013     D pInTypEOL                      1A   CONST
057800151125     D pInCmdCvtTbl                  10A                                        * QTCPASC QTCPEBC
057900151014     D pOutRespPtr                     *
058000151125     D pOutRespLen                   10U 0
058100151013     D*
058200151013      //********************************************************************************************
058300151013      //
058400151013      // Definizione variabili work (locali)
058500151013      //
058600151013      //********************************************************************************************
058700151013     D locMethodCode   S             10I 0
058800151013     D
058900151013     D Rcv             S             10I 0
059000151014     D wResponse       S          65536A   VARYING Based(pOutRespPtr)
059100151014     D wResponseLen    S             10I 0 Inz
059200151014     D wXlate          S          65536A   Inz
059300151013     D wChar           S              1A   Inz
059400151013     D wCR             S              1A   Inz(x'0D')
059500151013     D wLF             S              1A   Inz(x'0A')
059600151014     D wEOL            S              2A   Inz Varying
059700151013
059800151013
059900151013      /FREE
060000151013        // Inizializzo opcode di metodo
060100151013        locMethodCode = -1;
060200151013        InitDone = *off;
060300151014
060400151014
060500151013        // Verifica parametri "tabellati" relativi alla conversione del encoding dati
060600151013        if pInCmdCvtTbl = *blanks;
060700151013           pInCmdCvtTbl = 'QTCPEBC';
060800151013        endif;
060900151014
061000151013
061100151013        // Verifica parametri "tabellati" relativi alla conversione del encoding dati
061200151013        select;
061300151013          when pInTypEOL = '1';
061400151013               wEOL = wCR;
061500151013          when pInTypEOL = '2';
061600151014               wEOL = wLF;
061700151013          when pInTypEOL = '3';
061800151014               wEOL = wCR+wLF;
061900151013          other;
062000151014               wEOL = wCR+wLF;
062100151013        endsl;
062200151013
062300151013
062400151013        // Ricevo il messaggio byte-a-byte
062500151013        dou Rcv < 1;
062600151013
062700151013            // Inizializzo variabili di wrk
062800151014            wResponse    = %trim(' ');
062900151013            wResponseLen = *zeros;
063000151014            wXlate       = *blanks;
063100151014            wChar        = *blanks;
063200151013
063300151014            dou wResponseLen = %size(pInBufferLen) OR
063400151014                %scan(wChar : wEOL) > *zeros;
063500151014
063600151013                Rcv = recv(pInSktDescr : %addr(wChar) : 1 : 0);
063700151013                if Rcv < 1;
063800151013                   leave;
063900151013                endif;
064000151013
064100151014                if %scan(wChar : wEOL) = *zeros;
064200151013                   wResponseLen = wResponseLen + 1;
064300151014                   %subst(wResponse : wResponseLen : 1) = wChar;
064400151013                endif;
064500151013
064600151013            enddo;
064700151013
064800151013        enddo;
064900151013
065000151014
065100151013        if wResponseLen > *zeros;
065200151014
065300151013           if pInCmdCvtTbl <> '*NONE';
065400151014              wXlate = wResponse;
065500151014              callp UBXLATENCD_Translate(wResponseLen : wXlate : pInCmdCvtTbl);
065600151014              wResponse = wXlate;
065700151014           endif;
065800151014
065900151014           // Valorizzo opcode di metodo
066000151014           pOutRespPtr = %addr(wResponse);
066100151014           pOutRespLen = wResponseLen;
066200151014
066300151014           // Valorizzo opcode di metodo
066400151014           locMethodCode = 0;
066500151014           InitDone = *on;
066600151014
066700151013        endif;
066800151013
066900151013
067000151013        return locMethodCode;
067100151013
067200151013      /END-FREE
067300151013
067400151014     P UBSOCKET2_Receive...
067500151013     P                 E
067600151009
067700151009
067800151009
067900151009     P*--------------------------------------------------
068000151009     P* Procedure name: UBSOCKET2_Listen
068100151009     P* Purpose:        Create socket server
068200151009     P* Returns:        Integer (Status_Code)
068300151009     P* Parameter:      pInSktDescr    => Descrittore Socket
068400151009     P* Parameter:      pInBackLog     => Max client servibili (contemporaneamente)
068500151009     P*--------------------------------------------------
068600151009     P*
068700151009     P UBSOCKET2_Listen...
068800151009     P                 B                   EXPORT
068900151009     D*
069000151009     D UBSOCKET2_Listen...
069100151009     D                 PI            10I 0
069200151009     D pInSktDescr                   10U 0 CONST
069300151009     D pInBackLog                     5U 0 CONST
069400151009     D*
069500151009      //********************************************************************************************
069600151009      //
069700151009      // Definizione variabili work (locali)
069800151009      //
069900151009      //********************************************************************************************
070000151009     D locMethodCode   S             10I 0
070100151009     D
070200151012     D BackLog         S              5U 0 INZ(1024)
070300151009
070400151009
070500151009      /FREE
070600151009        // Inizializzo opcode di metodo
070700151009        locMethodCode = -1;
070800151009        InitDone = *off;
070900151009
071000151012        // Se non indicato un valore specifico per il back-log => assumo 1024
071100151009        if pInBackLog > *zeros;
071200151009           BackLog = pInBackLog;
071300151009        endif;
071400151009
071500151009        // Mi metto in ascolto (listening) sul socket server
071600151009        if listen(pInSktDescr : BackLog) < 0;
071700151009        else;
071800151009
071900151009           // Valorizzo opcode di metodo
072000151009           locMethodCode = 0;
072100151009           InitDone = *on;
072200151009
072300151009        endif;
072400151009
072500151009        return locMethodCode;
072600151009
072700151009      /END-FREE
072800151009
072900151009     P UBSOCKET2_Listen...
073000151009     P                 E
073100151009
073200151009
073300151009
073400151009     P*--------------------------------------------------
073500151009     P* Procedure name: UBSOCKET2_Acccept
073600151009     P* Purpose:        Accept a socket client request
073700151009     P* Returns:        Integer (Status_Code)
073800151014     P* Parameter:      pInSktDescr    => Descrittore Socket Server
073900151014     P* Parameter:      pOutSktDescr   => Descrittore Socket Client
074000151014     P* Parameter:      pOutSktAddr    => Indirizzo IP "dottato" del client
074100151009     P*--------------------------------------------------
074200151009     P*
074300151009     P UBSOCKET2_Accept...
074400151009     P                 B                   EXPORT
074500151009     D*
074600151009     D UBSOCKET2_Accept...
074700151009     D                 PI            10I 0
074800151009     D pInSktDescr                   10U 0 CONST
074900151125     D pOutSktDescr                  10U 0
075000151125     D pOutSktAddr                   15A
075100151009     D*
075200151009      //********************************************************************************************
075300151009      //
075400151009      // Definizione variabili work (locali)
075500151009      //
075600151009      //********************************************************************************************
075700151009     D locMethodCode   S             10I 0
075800151009     D
075900151009     D p_sockaddr      S               *
076000151009     D sockaddr        DS                  based(p_sockaddr)
076100151009     D   sa_family                    5I 0
076200151009     D   sa_data                     14A
076300151009     D sockaddr_in     DS                  based(p_sockaddr)
076400151009     D   sin_family                   5I 0
076500151009     D   sin_port                     5U 0
076600151009     D   sin_addr                    10U 0
076700151009     D   sin_zero                     8A
076800151009     D
076900151009     D p_connfrom      S               *
077000151009     D addrlen         S             10I 0
077100151009
077200151009
077300151009      /FREE
077400151009        // Inizializzo opcode di metodo
077500151009        locMethodCode = -1;
077600151009        InitDone = *off;
077700151009
077800151009        // Alloco la memoria necessaria a memorizzare la struttura del socket corrente
077900151009        addrlen = %size(sockaddr);
078000151009        p_connfrom = %alloc(addrlen);
078100151009
078200151009        // Servo la richiesta cliente "estraendola" dallo stack del server
078300151009        pOutSktDescr = accept(pInSktDescr : p_connfrom : addrlen);
078400151009
078500151009        // Se nuovo socket creato correttamente (Accept riuscita)
078600151009        if pOutSktDescr < *zeros;
078700151009        else;
078800151009           // Lunghezza "illegale" connessione TCP
078900151009           if addrlen <> 16;
079000151009           else;
079100151125
079200151125              p_sockaddr = p_connfrom;
079300151009
079400151012              if UBNUF2IDA_Convert(sin_addr:pOutInetDotAdr) = 0;
079500151012
079600151012                 // Valorizzo i parametri di output
079700151125                 pOutSktAddr = pOutInetDotAdr;
079800151012
079900151012                 // Valorizzo opcode di metodo
080000151012                 locMethodCode = 0;
080100151012                 InitDone = *on;
080200151012              endif;
080300151009           endif;
080400151009        endif;
080500151009
080600151009
080700151009        return locMethodCode;
080800151009
080900151009      /END-FREE
081000151009
081100151009     P UBSOCKET2_Accept...
081200151009     P                 E
