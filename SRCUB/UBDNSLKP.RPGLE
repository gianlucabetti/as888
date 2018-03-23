000100090710     H NOMAIN
000200071128     ***********************************************************************************************
000300071128     **
000400100612     ** Questo modulo fornisce le funzionalità per reperire l'indirizzo IP
000500100612     ** dato un nome internet
000600071128     **
000700080611     ** Può essere usato solo come sottoprocedura.
000800080612     ** Ogni metodo restituisce un valore 10I:
000900080612     **             - = 0 => tutto ok
001000080612     **             - < 0 => errore
001100080612     **
001200071129     **
001300100612     ** PARAMETRI DELLA SOTTOPROCEDURA UBDNSLKP_Retrieve
001400071129     **
001500071129     ** Posizione Uso Tipo      Descrizione
001600071129     ** --------- --- --------- ------------------------------
001700100612     ** 1         IN  CHARACTER "nome" internet
001800100612     ** 2         OUT CHARACTER Indirizzo internet "dottato" (puntato)
001900100922     ** 2         OUT CHARACTER Tipo indirizzo ricevuto ('ADR'=Ind.IP 'NAM'=Nome DNS)
002000071129     ** --------- --- --------- ------------------------------
002100080612     **
002200071128     **
002300071128     **
002400071128     ** ESEMPI DI CHIAMATA
002500090710     **
002600100922     ** if UBDNSLKP_Retrieve(pInInetHost : pOutInetFound : pOutInetType ) = 0
002700080612     **
002800080612     ** --------- --- --------- ------------------------------
002900071128     **
003000071128     ** ISTRUZIONI PER LA COMPILAZIONE
003100071128     **
003200151008     ** 1. Creare il modulo UBDNSLKP (Opz 15 PDM) indicando BNDDIR('QC2LE')
003300100612     ** 2. Creare/Aggiornare il programma di servizio UBDNSLKP (CRTSRVPGM / UPDSRVPGM)
003400080610     ** 4. Cancellare il modulo.
003500071128     **
003600071128     ***********************************************************************************************
003700080612
003800080612
003900080612      //********************************************************************************************
004000080612      //
004100080612      // Definizione file (globali)
004200080612      //
004300080612      //********************************************************************************************
004400071128
004500080612
004600071128      //********************************************************************************************
004700071128      //
004800080612      // Definizione prototipi procedure (globali)
004900071128      //
005000090710      //********************************************************************************************
005100100612     D/COPY GAITRASRC/SRCPROTOPR,UBDNSLKP
005200100612     D/COPY GAITRASRC/SRCPROTOPR,INET_NTOA
005300100922     D/COPY GAITRASRC/SRCPROTOPR,INET_ADDR
005400100612     D/COPY GAITRASRC/SRCPROTOPR,GETHSTBYNM
005500100922     D/COPY GAITRASRC/SRCPROTOPR,GETHSTBYAD
005600080612
005700080612
005800071128      //********************************************************************************************
005900071128      //
006000080612      // Definizione variabili work (globali)
006100071128      //
006200071128      //********************************************************************************************
006300090710
006400090710
006500090710
006600090710     P*--------------------------------------------------
006700100612     P* Procedure name: UBDNSLKP_Retrieve
006800100612     P* Purpose:        Retrieve Inet Name from an Inet Dotted Address
006900090710     P* Returns:        Integer (Status_Code)
007000100922     P* Parameter:      pInInetHost     => Host internet (Nome DNS / Indirizzo IP)
007100100922     P* Parameter:      pOutInetFound   => Indirizzo / Nome Internet "dottato" (puntato)
007200100922     P* Parameter:      pOutInetType    => Tipo indirizzo ricevuto ('ADR'=Ind.IP 'NAM'=Nome DNS)
007300090710     P*--------------------------------------------------
007400090710     P*
007500100612     P UBDNSLKP_Retrieve...
007600090710     P                 B                   EXPORT
007700090710     D*
007800100612     D UBDNSLKP_Retrieve...
007900090710     D                 PI            10I 0
008000100922     D pInInetHost                  256A   CONST
008100100922     D pOutInetFound                256A
008200100922     D pOutInetType                   3A
008300090710     D*
008400090710      //********************************************************************************************
008500090710      //
008600090710      // Definizione variabili work (locali)
008700090710      //
008800090710      //********************************************************************************************
008900090710     D locMethodCode   S             10I 0
009000100612
009100100612
009200100612     D p_hostent       S               *
009300151008
009400100612     D hostent         DS                  Based(p_hostent)
009500151008     D  h_name                         *
009600151008     D  h_aliases                      *
009700151008     D  h_addrtype                   10I 0
009800151008     D  h_length                     10I 0
009900151008     D  h_addr_list                    *
010000151008
010100151008     D p_h_addr        S               *   Based(h_addr_list)
010200100612     D h_addr          S             10U 0 Based(p_h_addr)
010300151008
010400100922     D h_ip32          S             10U 0 inz
010500151008     D AF_INET         C                   Const(2)
010600151008     D INADDR_NONE     C                   Const(4294967295)
010700100922
010800090710
010900090710
011000090710      /FREE
011100090710        // Inizializzo opcode di metodo
011200090710        locMethodCode = -1;
011300100922
011400100922
011500100922        // Inizializzo l'output
011600100922        pOutInetFound = *blanks;
011700100922        pOutInetType  = *blanks;
011800100922
011900090710
012000100922        // Reperisco dal nome DNS l'indirizzo internet "dottato"
012100100922        p_hostent = gethostbyname(%trim(pInInetHost));
012200100612        if p_hostent <> *NULL;
012300100612
012400100612           // Converto in formato stringa
012500100922           pOutInetFound = %str(inet_ntoa(h_addr));
012600100922           pOutInetType  = 'NAM';
012700090710
012800090710           // Valorizzo opcode di metodo
012900090710           locMethodCode = 0;
013000100922
013100100922        else;
013200100922
013300100922           // Reperisco dal'indirizzo internet "dottato" il nome DNS
013400151008           h_ip32 = inet_addr(%trim(pInInetHost));
013500100922           p_hostent = gethostbyaddr(h_ip32:%size(h_ip32):AF_INET);
013600100922           if p_hostent <> *NULL;
013700100922
013800100922              // Converto in formato stringa
013900100922              pOutInetFound = %str(h_name);
014000100922              pOutInetType  = 'ADR';
014100100922
014200100922              // Valorizzo opcode di metodo
014300100922              locMethodCode = 0;
014400100922
014500100922           endif;
014600090710
014700100922        endif;
014800090710
014900090710        return locMethodCode;
015000090710
015100090710      /END-FREE
015200090710
015300100612     P UBDNSLKP_Retrieve...
015400090710     P                 E
