000100090710     H BNDDIR('UBBNDDIR')
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
001500100612     ** PARAMETRI DELLA SOTTOPROCEDURA: UBSOCKET_Create
001600071128     **
001700071128     ** Posizione Uso Tipo      Descrizione
001800071128     ** --------- --- --------- ------------------------------
001900090713     ** 1         OUT UNSIGNED  Descrittoere Socket
002000071128     ** --------- --- --------- ------------------------------
002100090713     **
002200090713     **
002300090713     **
002400090713     ** --------- --- --------- ------------------------------
002500090713     **
002600100612     ** PARAMETRI DELLA SOTTOPROCEDURA: UBSOCKET_Garbage
002700090713     **
002800090713     ** Posizione Uso Tipo      Descrizione
002900090713     ** --------- --- --------- ------------------------------
003000090713     ** 1         IN  UNSIGNED  Descrittoere Socket
003100090713     ** --------- --- --------- ------------------------------
003200090713     **
003300071129     **
003400090713     **
003500090713     ** PARAMETRI DELLA SOTTOPROCEDURA UBSOCKET_Connect
003600071129     **
003700071129     ** Posizione Uso Tipo      Descrizione
003800071129     ** --------- --- --------- ------------------------------
003900090713     ** 1         IN  UNSIGNED  Descrittoere Socket
004000090713     ** 2         IN  CHARACTER Ip Address "dotted"            => Indirizzo IP "puntato"
004100090713     ** 3         IN  UNSIGNED  Port number                    => Numero porta del socket "server"
004200071129     ** --------- --- --------- ------------------------------
004300090713     **
004400090713     **
004500090713     **
004600090713     ** PARAMETRI DELLA SOTTOPROCEDURA UBSOCKET_Send
004700090713     **
004800090713     ** Posizione Uso Tipo      Descrizione
004900090713     ** --------- --- --------- ------------------------------
005000090713     ** 1         IN  UNSIGNED  Descrittoere Socket
005100090713     ** 2         IN  CHARACTER Comado Socket da sottomettere
005200090713     ** 3         IN  CHARACTER Codice tabella conversione
005300090713     ** --------- --- --------- ------------------------------
005400080612     **
005500071128     **
005600071128     **
005700071128     ** ESEMPI DI CHIAMATA
005800071128     **
005900100612     **  if UBSOCKET_Create(pOutSktDescr) = 0;
006000100621     **
006100100621     **  if UBSOCKET_Garbage(pInSktDescr) = 0;
006200090713     **
006300100922     **  if UBSOCKET_Connect(pInSktDescr : pInHostAdr : pInPort) = 0;
006400090713     **
006500100612     **  if UBSOCKET_Send(pInSktDescr : pInSktCmd : pInCmdCvtTbl ) = 0
006600080612     **
006700080612     ** --------- --- --------- ------------------------------
006800071128     **
006900071128     ** ISTRUZIONI PER LA COMPILAZIONE
007000071128     **
007100090710     ** 1. Creare il modulo UBSOCKET (Opz 15 PDM)
007200090710     ** 2. Creare/Aggiornare il programma di servizio UBSOCKET (CRTSRVPGM / UPDSRVPGM)
007300080610     ** 4. Cancellare il modulo.
007400071128     **
007500071128     ***********************************************************************************************
007600080612
007700080612
007800080612      //********************************************************************************************
007900080612      //
008000080612      // Definizione file (globali)
008100080612      //
008200080612      //********************************************************************************************
008300071128
008400080612
008500071128      //********************************************************************************************
008600071128      //
008700080612      // Definizione prototipi procedure (globali)
008800071128      //
008900071128      //********************************************************************************************
009000090710     D/COPY GAITRASRC/SRCPROTOPR,UBSOCKET
009100090710     D/COPY GAITRASRC/SRCPROTOPR,UBCVTIDA
009200100922     D/COPY GAITRASRC/SRCPROTOPI,UBCVTIDA
009300100922     D/COPY GAITRASRC/SRCPROTOPR,UBDNSLKP
009400100922     D/COPY GAITRASRC/SRCPROTOPI,UBDNSLKP
009500100922     D/COPY GAITRASRC/SRCPROTOPR,SOCKET
009600090710     D/COPY GAITRASRC/SRCPROTOPR,INET_ADDR
009700090710     D/COPY GAITRASRC/SRCPROTOPR,QDCXLATE
009800080612
009900080612
010000071128      //********************************************************************************************
010100071128      //
010200080612      // Definizione variabili work (globali)
010300071128      //
010400071128      //********************************************************************************************
010500080612     D InitDone        S               N   INZ(*off)
010600090710     D
010700100612     D* Socket - Address Family
010800100612     D AF_INET         C                   CONST(2)                             * TCP
010900100612     D AF_IPX          C                   CONST(6)                             * IPX/SPX
011000100612     D AF_NETBIOS      C                   CONST(17)                            * NetBIOS
011100100612     D AF_INET6        C                   CONST(23)                            * TCP
011200100612     D AF_IRDA         C                   CONST(26)                            * Infrared
011300100612     D AF_BTH          C                   CONST(32)                            * BlueTooth
011400100612     D
011500100612     D* Socket - Type
011600100612     D SOCK_STREAM     C                   CONST(1)                             * TCP
011700100612     D SOCK_DGRAM      C                   CONST(2)                             * UDP
011800100612     D SOCK_RAW        C                   CONST(3)                             * IP Datagram
011900100612     D SOCK_RDM        C                   CONST(4)                             * PGM
012000100612     D SOCK_SEQPACKET  C                   CONST(5)                             * ???
012100100612     D
012200100612     D* Socket - Protocol
012300100612     D IPPROTO_IP      C                   CONST(0)                             * Generic IP
012400100612     D IPPROTO_ICMP    C                   CONST(1)                             * ICMP
012500100612     D BTHPROTO_RFCOM  C                   CONST(3)                             * BlueTooth
012600100612     D IPPROTO_TCP     C                   CONST(6)                             * TCP
012700100612     D IPPROTO_UDP     C                   CONST(17)                            * UDP
012800100612     D IPPROTO_ICMPV6  C                   CONST(58)                            * ICMP V6
012900100612     D IPPROTO_RM      C                   CONST(113)                           * PGM
013000071129
013100080612
013200080612     P*--------------------------------------------------
013300100612     P* Procedure name: UBSOCKET_Create
013400080612     P* Purpose:        Inizializzazioni
013500090713     P* Returns:        Integer (Status_Code)
013600090713     P* Parameter:      pOutSktDescr   => Descrittore Socket
013700080612     P*--------------------------------------------------
013800080612     P*
013900100612     P UBSOCKET_Create...
014000080612     P                 B                   EXPORT
014100080612     D*
014200100612     D UBSOCKET_Create...
014300080612     D                 PI            10I 0
014400090713     D pOutSktDescr                  10U 0
014500080612     D*
014600080612      //********************************************************************************************
014700080612      //
014800080612      // Definizione variabili work (locali)
014900080612      //
015000080612      //********************************************************************************************
015100080612     D locMethodCode   S             10I 0
015200090710     D
015300080612
015400090710
015500080612      /FREE
015600080612        // Inizializzo opcode di metodo
015700080612        locMethodCode = -1;
015800080612        InitDone = *off;
015900080616
016000090710        // Definisco un socket (tipo e modalità)
016100090713        pOutSktDescr = socket(AF_INET:SOCK_STREAM:IPPROTO_TCP);
016200090713        if pOutSktDescr < 0;
016300090710        else;
016400090710
016500080616           // Valorizzo opcode di metodo
016600080616           locMethodCode = 0;
016700080616           InitDone = *on;
016800080616
016900080616        endif;
017000080612
017100080612        return locMethodCode;
017200080612
017300080612      /END-FREE
017400080612
017500100612     P UBSOCKET_Create...
017600080612     P                 E
017700080612
017800080612
017900080612
018000080612     P*--------------------------------------------------
018100100612     P* Procedure name: UBSOCKET_Garbage
018200100612     P* Purpose:        Garbage collection
018300090713     P* Returns:        Integer (Status_Code)
018400090713     P* Parameter:      pInSktDescr    => Descrittore Socket
018500080612     P*--------------------------------------------------
018600080612     P*
018700100612     P UBSOCKET_Garbage...
018800080612     P                 B                   EXPORT
018900080612     D*
019000100612     D UBSOCKET_Garbage...
019100080612     D                 PI            10I 0
019200090713     D pInSktDescr                   10U 0 CONST
019300080612     D*
019400080612      //********************************************************************************************
019500080612      //
019600080612      // Definizione variabili work (locali)
019700080612      //
019800080612      //********************************************************************************************
019900080612     D locMethodCode   S             10I 0
020000080612
020100080612      /FREE
020200080612        // Inizializzo opcode di metodo
020300080612        locMethodCode = -1;
020400080612
020500090710        // Chiudo il socket
020600090713        callp close(pInSktDescr);
020700080612
020800080612        // Valorizzo opcode di metodo
020900080612        locMethodCode = 0;
021000080612
021100080612        return locMethodCode;
021200080612
021300080612      /END-FREE
021400080612
021500100612     P UBSOCKET_Garbage...
021600080612     P                 E
021700090710
021800090710
021900090710
022000090710     P*--------------------------------------------------
022100090710     P* Procedure name: UBSOCKET_Connect
022200090710     P* Purpose:        Connect to a socket "server"
022300090710     P* Returns:        Integer (Status_Code)
022400090713     P* Parameter:      pInSktDescr    => Descrittore Socket
022500100922     P* Parameter:      pInHostAdr     => Host remoto
022600090710     P* Parameter:      pInPort        => Numero porta
022700090710     P*--------------------------------------------------
022800090710     P*
022900090710     P UBSOCKET_Connect...
023000090710     P                 B                   EXPORT
023100090710     D*
023200090710     D UBSOCKET_Connect...
023300090710     D                 PI            10I 0
023400090713     D pInSktDescr                   10U 0 CONST
023500100922     D pInHost                      256A   CONST
023600090710     D pInPort                        5U 0 CONST
023700090710     D*
023800090710      //********************************************************************************************
023900090710      //
024000090710      // Definizione variabili work (locali)
024100090710      //
024200090710      //********************************************************************************************
024300090710     D locMethodCode   S             10I 0
024400090710     D
024500090710     D p_sockaddr      S               *
024600090710     D sockaddr        DS                  based(p_sockaddr)
024700090710     D   sa_family                    5I 0
024800090710     D   sa_data                     14A
024900090710     D sockaddr_in     DS                  based(p_sockaddr)
025000090710     D   sin_family                   5I 0
025100090710     D   sin_port                     5U 0
025200090710     D   sin_addr                    10U 0
025300090710     D   sin_zero                     8A
025400090710     D
025500090710     D p_connto        S               *
025600090710     D addrlen         S             10I 0
025700100922     D IpAddr          S             15A   inz
025800090710
025900090710
026000090710      /FREE
026100090710        // Inizializzo opcode di metodo
026200090710        locMethodCode = -1;
026300090710        InitDone = *off;
026400090710
026500090710        // Alloco la memoria necessaria a memorizzare la struttura del socket corrente
026600090710        addrlen = %size(sockaddr);
026700090710        p_connto = %alloc(addrlen);
026800090710
026900090710        // Faccio puntare l'inririzzo della struttura del socket corrente alla nuova
027000090710        // area di memoria appena allocata
027100090710        p_sockaddr = p_connto;
027200090710
027300100922
027400100922
027500100922        // Effettuo il DNS Lookup x reperire SEMPRE l'indirizzo "puntato"
027600100922        if UBDNSLKP_Retrieve(pInHost : pOutInetFound : pOutInetType) = 0;
027700100922
027800100922          if pOutInetType = 'ADR';
027900100922             IpAddr = %trim(pInHost);
028000100922          else;
028100100922             IpAddr = %trim(pOutInetFound);
028200100922          endif;
028300100922
028400100922          // Converto l'indirizzo internet "puntato" nel relativo valore numerico unsigned
028500100922          if UBCVTIDA_Convert(IpAddr:pOutInetAdrVal) = 0;
028600090710
028700100922             // Popolo la struttura del socket
028800100922             // Note that IP is the ip address we previously looked up
028900100922             // using the inet_addr and/or gethostbyname APIs
029000100922             // and port is the port number that we looked up using the
029100100922             // getservbyname API.
029200100922             sin_family = AF_INET;
029300100922             sin_addr = pOutInetAdrVal;
029400100922             sin_port = pInPort;
029500100922             sin_zero = *ALLX'00';
029600100922
029700100922             // Mi connetto al socket "server"
029800100922             if connect(pInSktDescr : p_connto : addrlen) < 0;
029900100922             else;
030000090710
030100100922                // Valorizzo opcode di metodo
030200100922                locMethodCode = 0;
030300100922                InitDone = *on;
030400090710
030500100922             endif;
030600100922          endif;
030700100922        endif;
030800090710
030900090710        return locMethodCode;
031000090710
031100090710      /END-FREE
031200090710
031300090710     P UBSOCKET_Connect...
031400090710     P                 E
031500090710
031600090710
031700090710
031800090710     P*--------------------------------------------------
031900090710     P* Procedure name: UBSOCKET_Send
032000090710     P* Purpose:        Send socket command
032100090710     P* Returns:        Integer (Status_Code)
032200100612     P* Parameter:      pInSktDescr    => Descrittore Socket
032300090710     P* Parameter:      pInSktCmd      => Comando da sottomettere al socket "server"
032400090713     P* Parameter:      pInCmdCvtTbl   => Tabella conversione comando socket
032500090710     P*--------------------------------------------------
032600090710     P*
032700090710     P UBSOCKET_Send...
032800090710     P                 B                   EXPORT
032900090710     D*
033000090710     D UBSOCKET_Send...
033100090710     D                 PI            10I 0
033200090713     D pInSktDescr                   10U 0 CONST
033300090710     D pInSktCmd                   1024A   CONST
033400090713     D pInCmdCvtTbl                  10A   VALUE                                * QTCPASC QTCPEBC
033500090710     D*
033600090710      //********************************************************************************************
033700090710      //
033800090710      // Definizione variabili work (locali)
033900090710      //
034000090710      //********************************************************************************************
034100090710     D locMethodCode   S             10I 0
034200090710     D
034300090710     D Rcv             S             10I 0
034400090710     D SktCmd          S           1024A
034500090710     D SktCmdLenght    S             10I 0
034600090710
034700090710
034800090710      /FREE
034900090710        // Inizializzo opcode di metodo
035000090710        locMethodCode = -1;
035100090710        InitDone = *off;
035200090713
035300090713        // Verifica parametri "tabellati"
035400090713        if pInCmdCvtTbl = *blanks;
035500090713           pInCmdCvtTbl = 'QTCPASC';
035600090713        endif;
035700090710
035800090710        // Sottometto il comando al socket "server"
035900100612        SktCmd = %trim(pInSktCmd);
036000090713        SktCmdLenght = %len(%trim(SktCmd));
036100090713        callp ConvertData(SktCmdLenght : SktCmd : pInCmdCvtTbl);
036200090713        Rcv = send(pInSktDescr : %addr(SktCmd) : SktCmdLenght  : 0);
036300090710        If  Rcv < SktCmdLenght;
036400090710        Else;
036500090710
036600090710           // Valorizzo opcode di metodo
036700090710           locMethodCode = 0;
036800090710           InitDone = *on;
036900090710
037000090710        endif;
037100090710
037200090710        return locMethodCode;
037300090710
037400090710      /END-FREE
037500090710
037600090710     P UBSOCKET_Send...
037700090710     P                 E
