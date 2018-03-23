000100151125     H Option(*SrcStmt)
000200151126     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('UBBNDDIR')
000300151126
000400151125     **-----------------------------------------------------------------------**
000500151126     ** EROGAZIONE SOCKET SERVER - UDP
000600151126     **-----------------------------------------------------------------------**
000700151126
000800151008     D/COPY GAITRASRC/SRCPROTOPR,UBSOCKET2
000900151012     D/COPY GAITRASRC/SRCPROTOPI,UBSOCKET2
001000151012     D/COPY GAITRASRC/SRCPROTOPR,UBDNSLKP
001100151012     D/COPY GAITRASRC/SRCPROTOPI,UBDNSLKP
001200151218
001300151218
001400151218     D*------------------
001500151218     D* PASSAGGIO PARAMETRI A PROCEDURE SYSTEM
001600151218     D*------------------
001700151218     D executeCommand...
001800151218     D                 PR            10I 0 EXTPROC('system')
001900151218     D  command                        *   VALUE OPTIONS(*STRING)
002000151218     D exceptionMessageID...
002100151218     D                 S              7A   IMPORT('_EXCP_MSGID')
002200090521
002300090520
002400151218     D pInSrvNIC_IP    S             15A
002500151218     D pInSrvSktTyp    S              3A
002600151218     D pInSrvPort      S              5U 0
002700151218     D pInSrvPortRpy   S              5U 0
002800151218     D pInSrvKillCnn   S              1A
002900151218
003000151202     D wServerSocket   S                   like(pInSktDescr) inz
003100151202     D wClientSocket   S                   like(pInSktDescr) inz
003200151202     D wReadSet        S                   like(pInFDset) inz
003300151202     D wWriteSet       S                   like(pInFDset) inz
003400151202     D wExcpSet        S                   like(pInFDset) inz
003500151202     D wRC             S             10I 0 inz
003600151212     D wMessage        S            512A   inz
003700151216     D wReply          S            512A   inz
003800151218     D wCommand        S            512A   inz
003900151217     D wMainCycle      S               N   inz
004000151218     D wContaMsgs      S             10I 0 inz
004100151218
004200151202
004300151012
004400090520     C*
004500090610
004600090610      /FREE
004700151125
004800151125         *inlr = *on;
004900151218
005000151218
005100151218         // Verifica/impostazione default parametri di input servente
005200151218         exsr  sr_ChkParam;
005300151125
005400151218
005500151125         // Creazione server socket
005600151125         exsr  sr_MakeServer;
005700151125
005800151125
005900151125         // Ascolto never-ending
006000151125         dow 1 = 1;
006100151125            exsr  sr_Listening;
006200151125            exsr  sr_ChiudiClient;
006300151125         enddo;
006400151012
006500151012
006600151125         // Chiusura e garbage-collection lato Server
006700151125         exsr  sr_ChiudiServer;
006800151012
006900151125
007000151125         return;
007100151125
007200151218
007300151218
007400151218         //--------------------------------------------------------------
007500151218 ccc     // Verifica/impostazione default parametri di input servente
007600151218         //--------------------------------------------------------------
007700151218         BEGSR  sr_ChkParam;
007800151218
007900151218           // Verifico il tipo di socket srever richiesto TCP/UDP
008000151218           // (default = 'TCP')
008100151218           if pInSrvSktTyp = *blanks;
008200151218              pInSrvSktTyp = 'TCP';
008300151218           endif;
008400151218
008500151218           // Verifico se richiesto valore speciale '*' come interfaccia di rete
008600151218           // sulla quale stare in ascolto come servente (default = '*')
008700151218           if pInSrvNIC_IP = *blanks;
008800151218              pInSrvNIC_IP = '*';
008900151218           endif;
009000151218
009100151218           // Verifico la porta di comunicazione lato serer
009200151218           // (default = 3001)
009300151218           if pInSrvPort = *zeros;
009400151218              pInSrvPort = 3001;
009500151218           endif;
009600151218
009700151218           // Verifico la porta di comunicazione per risposte (lato client)
009800151218           // (0 = risposta sulla stessa porta del messaggio sorgente)
009900151218           // (default = 0)
010000151218           if pInSrvPortRpy = *zeros;
010100151218              pInSrvPortRpy = *zeros;
010200151218           endif;
010300151218
010400151218           // Verifico se richiesto di killare ventuali connessioni già in essere
010500151218           // su stessa NIC/porta
010600151218           // (S=Sì)
010700151218           // (default = 'N')
010800151218           if pInSrvKillCnn <> 'S';
010900151218              pInSrvKillCnn = 'N';
011000151218           endif;
011100151218
011200151218
011300151218         ENDSR;
011400151125
011500151218
011600151125
011700151125         //--------------------------------------------------------------
011800151125         // Operazioni di "creazione server socket"
011900151125         //--------------------------------------------------------------
012000151125         BEGSR  sr_MakeServer;
012100151125
012200151218           // Imposto i valori di procedura per il server corrente richiesto
012300151218           if pInSrvNIC_IP = '*';
012400151218              pInLocalAdr = 'INADDR_ANY';
012500151218           endif;
012600151218 xxx       pInLocalPort = pInSrvPort;
012700151218
012800151218
012900151125 xxx       pInLocalAdr  = 'INADDR_ANY';
013000151125 xxx       pInLocalPort = 4000;
013100151125 xxx       pInBackLog   = 5;
013200151218 xxx       pInSrvSktTyp = 'UDP';
013300151218 xxx       pInSrvPort   = pInLocalPort;
013400151125
013500151125
013600151218           // Se richiesto killo un eventuale connessione già in essere sulla stessa
013700151218           // NIC/porta indicate per il servente corrente
013800151218           if pInSrvKillCnn = 'S';
013900151218              exceptionMessageID = *blanks;
014000151218              dow exceptionMessageID = *blanks OR
014100151218                  exceptionMessageID = 'TCP8A3D';
014200151218                  wCommand = 'ENDTCPCNN PROTOCOL(*'+%trim(pInSrvSktTyp)+')' +
014300151218                             ' LCLINTNETA('''+%trim(pInSrvNIC_IP)+''')' +
014400151218                             ' LCLPORT('+%trim(%editc(pInSrvPort:'4'))+')';
014500151218                  if executeCommand(%trim(wCommand)) <> *zeros;
014600151218                  endif;
014700151218              enddo;
014800151218           endif;
014900151218
015000151218
015100151202           // Creo il socket di tipo UDP
015200151126           if UBSOCKET2_Create('UDP' : wServerSocket) = 0;
015300151126
015400151126              // Forzo il socket a ri-utilizzare la porta
015500151126              callp UBSOCKET2_SetSockOption(wServerSocket : SOL_SOCKET :
015600151126                                            SO_REUSEADDR : 1);
015700151126
015800151126              // Forzo il socket ad un "linger" (permanenza) di 120 secondi
015900151126              callp UBSOCKET2_SetSockOption(wServerSocket : SOL_SOCKET :
016000151126                                            SO_LINGER : 120);
016100151126
016200151125           else;
016300151125              // Chiusura e garbage-collection lato Server
016400151125              exsr  sr_ChiudiServer;
016500151125           endif;
016600151125
016700151125
016800151202           // Bindo il socket appena creato creando quindi il servente UDP
016900151125           if UBSOCKET2_Bind(wServerSocket : pInLocalAdr : pInLocalPort) = 0;
017000151202
017100151202              // Utilizzo il metodo "select" per determinare e gestire i dati scambiati
017200151217              wMainCycle = *on;
017300151217              dow wMainCycle;
017400151212                  //callp UBSOCKET2_FD_zero(wReadSet);
017500151212                  //callp UBSOCKET2_FD_zero(wExcpSet);
017600151212                  //callp UBSOCKET2_FD_set(wServerSocket : wReadSet);
017700151212                  //callp UBSOCKET2_FD_set(wServerSocket : wExcpSet);
017800151202
017900151203                  // Effettuo check di "select" ogni 5,8 secondi
018000151212                  //pInWaitTime = 5800000;
018100151212                  //wRC = UBSOCKET2_Select(wServerSocket : wReadSet : wWriteSet :
018200151212                  //                wExcpSet : pInWaitTime);
018300151202
018400151202
018500151202                  // Dentro ciclo di ascolto lato server testo costantemente lo shut-down
018600151202                  if %shtdn;
018700151202                     // Chiusura e garbage-collection lato Server
018800151202                     exsr  sr_ChiudiServer;
018900151202                     return;
019000151202                  endif;
019100151202
019200151202
019300151202                  // Se il metodo "select" arriva a timeout (ergo nessun dato scambiato via socket)
019400151202                  // proseguo il ciclo
019500151203                  //if wRC = *zeros;
019600151203                  //   iter;
019700151203                  //endif;
019800151202
019900151216
020000151216                  // Se sin qui tutto OK => ricevo il buffer del messaggio socket
020100151216                  pInMsgCvtTbl = *blanks;
020200151212                  if UBSOCKET2_ReceiveFrom(wServerSocket : 1024 : '3' :
020300151216                     pInMsgCvtTbl : pOutRespLen : wMessage : pOutRespFull :
020400151217                     pOutSktAddrPtr : pOutSktAddr) = 0;
020500151216
020600151217
020700151218  xxx                // Incremento il contatore dimessaggi TOTALI (da tutti i client) ricevuti
020800151218  xxx                wContaMsgs = wContaMsgs + 1;
020900151216
021000151216  ccc                // Se messaggio ricevuto è completo invio esito OK al client
021100151216  xxx                pInMsgCvtTbl = *blanks;
021200151216                     if pOutRespFull = 1;
021300151217                        wReply = 'OK <' + %subst(wMessage:1:pOutRespLen) + '>' +
021400151217                                 %editc(wContaMsgs:'4');
021500151216                     else;
021600151217                        wReply = 'KO <' + %subst(wMessage:1:pOutRespLen) + '>' +
021700151217                                 %editc(wContaMsgs:'4');
021800151216                     endif;
021900151216                     if UBSOCKET2_ReplyTo(wServerSocket : %len(%trim(wReply)) :
022000151217                            wReply : pInMsgCvtTbl : pOutSktAddrPtr : 3001) < 0;
022100151202
022200151216                     endif;
022300151202                  endif;
022400151202
022500151202              enddo;
022600151202
022700151125           else;
022800151125              // Chiusura e garbage-collection lato Server
022900151125              exsr  sr_ChiudiServer;
023000151125           endif;
023100151125
023200151125
023300151126 xxx       if UBSOCKET2_Listen(wServerSocket :  pInBackLog) = 0;
023400151126 ccc       else;
023500151125              // Chiusura e garbage-collection lato Server
023600151125              exsr  sr_ChiudiServer;
023700151125           endif;
023800151125
023900151125         ENDSR;
024000151125
024100151125
024200151125
024300151125         //--------------------------------------------------------------
024400151125         // Operazioni di attesa infinita di connessioni da client
024500151125         //--------------------------------------------------------------
024600151125         BEGSR  sr_Listening;
024700151125
024800151125           // Accepting
024900151125           if UBSOCKET2_Accept(wServerSocket : wClientSocket : pOutSktAddr) = 0;
025000151125
025100151125             if wClientSocket < 0;
025200151125
025300151125                // Chiusura e garbage-collection lato Server
025400151125                exsr  sr_ChiudiServer;
025500151126
025600151126             else;
025700151126
025800151126                // Forzo il socket ad un "linger" (permanenza) di 120 secondi
025900151126                callp UBSOCKET2_SetSockOption(wClientSocket : SOL_SOCKET :
026000151126                                              SO_LINGER : 120);
026100151125
026200151125             endif;
026300151125
026400151125          endif;
026500151125
026600151125         ENDSR;
026700151125
026800090610
026900151125
027000151125         //--------------------------------------------------------------
027100151125         // Operazioni di chiusura e garbage-collection lato Client
027200151125         //--------------------------------------------------------------
027300151125         BEGSR  sr_ChiudiClient;
027400151125
027500151125           if UBSOCKET2_Garbage(wClientSocket) = 0;
027600151125           endif;
027700151125
027800151125         ENDSR;
027900151125
028000151125
028100151125
028200151125         //--------------------------------------------------------------
028300151125         // Operazioni di chiusura e garbage-collection lato Server
028400151125         //--------------------------------------------------------------
028500151125         BEGSR  sr_ChiudiServer;
028600151125
028700151125           if UBSOCKET2_Garbage(wServerSocket) = 0;
028800151125           endif;
028900151125
029000151126           // Attesa <n> secondi per scaricamento socket
029100151126           // callp delay(<n>);
029200151126
029300151125           return;
029400151125
029500151125         ENDSR;
029600151125
029700151125
029800151125      /END-FREE
029900151125
030000151125
