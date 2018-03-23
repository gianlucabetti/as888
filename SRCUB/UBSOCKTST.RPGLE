000100151125     H Option(*SrcStmt)
000200151126     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('UBBNDDIR')
000300151126
000400151125     **-----------------------------------------------------------------------**
000500151126     ** EROGAZIONE SOCKET SERVER - TCP
000600151126     **-----------------------------------------------------------------------**
000700151126
000800151008     D/COPY GAITRASRC/SRCPROTOPR,UBSOCKET2
000900151012     D/COPY GAITRASRC/SRCPROTOPI,UBSOCKET2
001000151012     D/COPY GAITRASRC/SRCPROTOPR,UBDNSLKP
001100151012     D/COPY GAITRASRC/SRCPROTOPI,UBDNSLKP
001200090521
001300090520
001400151126     D wServerSocket   S             10I 0
001500151126     D wClientSocket   S             10I 0
001600151012
001700151012
001800090520     C*
001900090610
002000090610      /FREE
002100151125
002200151125         *inlr = *on;
002300151125
002400151125         // Creazione server socket
002500151125         exsr  sr_MakeServer;
002600151125
002700151125
002800151125         // Ascolto never-ending
002900151125         dow 1 = 1;
003000151125            exsr  sr_Listening;
003100151125 xxx        //exsr  sr_AnswerClient;
003200151125            exsr  sr_ChiudiClient;
003300151125         enddo;
003400151012
003500151012
003600151125         // Chiusura e garbage-collection lato Server
003700151125         exsr  sr_ChiudiServer;
003800151012
003900151125
004000151125         return;
004100151125
004200151125
004300151125
004400151125         //--------------------------------------------------------------
004500151125         // Operazioni di "creazione server socket"
004600151125         //--------------------------------------------------------------
004700151125         BEGSR  sr_MakeServer;
004800151125
004900151125 xxx       pInLocalAdr  = 'INADDR_ANY';
005000151125 xxx       pInLocalPort = 4000;
005100151125 xxx       pInBackLog   = 5;
005200151125
005300151125
005400151126           if UBSOCKET2_Create('TCP' : wServerSocket) = 0;
005500151125           else;
005600151125              // Chiusura e garbage-collection lato Server
005700151125              exsr  sr_ChiudiServer;
005800151125           endif;
005900151125
006000151125
006100151125           if UBSOCKET2_Bind(wServerSocket : pInLocalAdr : pInLocalPort) = 0;
006200151125           else;
006300151125              // Chiusura e garbage-collection lato Server
006400151125              exsr  sr_ChiudiServer;
006500151125           endif;
006600151125
006700151125
006800151125           if UBSOCKET2_Listen(wServerSocket :  pInBackLog) = 0;
006900151125           else;
007000151125              // Chiusura e garbage-collection lato Server
007100151125              exsr  sr_ChiudiServer;
007200151125           endif;
007300151125
007400151125         ENDSR;
007500151125
007600151125
007700151125
007800151125         //--------------------------------------------------------------
007900151125         // Operazioni di attesa infinita di connessioni da client
008000151125         //--------------------------------------------------------------
008100151125         BEGSR  sr_Listening;
008200151125
008300151125           // Accepting
008400151125           if UBSOCKET2_Accept(wServerSocket : wClientSocket : pOutSktAddr) = 0;
008500151125
008600151125             if wClientSocket < 0;
008700151125
008800151125                // Chiusura e garbage-collection lato Server
008900151125                exsr  sr_ChiudiServer;
009000151125
009100151125             endif;
009200151125
009300151125          endif;
009400151125
009500151125         ENDSR;
009600151125
009700090610
009800151125
009900151125         //--------------------------------------------------------------
010000151125         // Operazioni di chiusura e garbage-collection lato Client
010100151125         //--------------------------------------------------------------
010200151125         BEGSR  sr_ChiudiClient;
010300151125
010400151125           if UBSOCKET2_Garbage(wClientSocket) = 0;
010500151125           endif;
010600151125
010700151125         ENDSR;
010800151125
010900151125
011000151125
011100151125         //--------------------------------------------------------------
011200151125         // Operazioni di chiusura e garbage-collection lato Server
011300151125         //--------------------------------------------------------------
011400151125         BEGSR  sr_ChiudiServer;
011500151125
011600151125           if UBSOCKET2_Garbage(wServerSocket) = 0;
011700151125           endif;
011800151125
011900151126           // Attesa <n> secondi per scaricamento socket
012000151126           // callp delay(<n>);
012100151126
012200151125           return;
012300151125
012400151125         ENDSR;
012500151125
012600151125
012700151125      /END-FREE
012800151125
012900151125
