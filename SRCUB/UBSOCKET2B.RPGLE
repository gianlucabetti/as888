000100151125     H Option(*SrcStmt)
000200090610     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('UBTEST')
000300151125     **-----------------------------------------------------------------------**
000400090610
000500151008     D/COPY GAITRASRC/SRCPROTOPR,UBSOCKET2
000600151012     D/COPY GAITRASRC/SRCPROTOPI,UBSOCKET2
000700151012     D/COPY GAITRASRC/SRCPROTOPR,UBDNSLKP
000800151012     D/COPY GAITRASRC/SRCPROTOPI,UBDNSLKP
000900090521
001000090520
001100151125     D wLen            S             10I 0 inz(*hival)
001200151012     D wServerSocket   S             10U 0
001300151012     D wClientSocket   S             10U 0
001400151012
001500151012
001600090520     C*
001700090610
001800090610      /FREE
001900151125
002000151125         *inlr = *on;
002100151125
002200151125         // Creazione server socket
002300151125         exsr  sr_MakeServer;
002400151125
002500151125
002600151125         // Ascolto never-ending
002700151125         dow 1 = 1;
002800151125            exsr  sr_Listening;
002900151125 xxx        //exsr  sr_AnswerClient;
003000151125            exsr  sr_ChiudiClient;
003100151125         enddo;
003200151012
003300151012
003400151125         // Chiusura e garbage-collection lato Server
003500151125         exsr  sr_ChiudiServer;
003600151012
003700151125
003800151125         return;
003900151125
004000151125
004100151125
004200151125         //--------------------------------------------------------------
004300151125         // Operazioni di "creazione server socket"
004400151125         //--------------------------------------------------------------
004500151125         BEGSR  sr_MakeServer;
004600151125
004700151125 xxx       pInLocalAdr  = 'INADDR_ANY';
004800151125 xxx       pInLocalPort = 4000;
004900151125 xxx       pInBackLog   = 5;
005000151125
005100151125
005200151125           if UBSOCKET2_Create(wServerSocket) = 0;
005300151125           else;
005400151125              // Chiusura e garbage-collection lato Server
005500151125              exsr  sr_ChiudiServer;
005600151125           endif;
005700151125
005800151125
005900151125           if UBSOCKET2_Bind(wServerSocket : pInLocalAdr : pInLocalPort) = 0;
006000151125           else;
006100151125              // Chiusura e garbage-collection lato Server
006200151125              exsr  sr_ChiudiServer;
006300151125           endif;
006400151125
006500151125
006600151125           if UBSOCKET2_Listen(wServerSocket :  pInBackLog) = 0;
006700151125 xxx          // wServerSocket = pOutSktDescr;
006800151125           else;
006900151125              // Chiusura e garbage-collection lato Server
007000151125              exsr  sr_ChiudiServer;
007100151125           endif;
007200151125
007300151125         ENDSR;
007400151125
007500151125
007600151125
007700151125         //--------------------------------------------------------------
007800151125         // Operazioni di attesa infinita di connessioni da client
007900151125         //--------------------------------------------------------------
008000151125         BEGSR  sr_Listening;
008100151125
008200151125           // Accepting
008300151125 xxx       // if UBSOCKET2_Accept(wServerSocket : wClientSocket : pOutSktAddr :
008400151125           //                    pOutSktAdrPtr : pOutSktAdrPLen) = 0;
008500151125           if UBSOCKET2_Accept(wServerSocket : wClientSocket : pOutSktAddr) = 0;
008600151125
008700151125             if wClientSocket < 0;
008800151125
008900151125                // Chiusura e garbage-collection lato Server
009000151125                exsr  sr_ChiudiServer;
009100151125
009200151125             endif;
009300151125
009400151125          endif;
009500151125
009600151125         ENDSR;
009700151125
009800090610
009900151125
010000151125         //--------------------------------------------------------------
010100151125         // Operazioni di chiusura e garbage-collection lato Client
010200151125         //--------------------------------------------------------------
010300151125         BEGSR  sr_ChiudiClient;
010400151125
010500151125           if UBSOCKET2_Garbage(wClientSocket) = 0;
010600151125           endif;
010700151125
010800151125         ENDSR;
010900151125
011000151125
011100151125
011200151125         //--------------------------------------------------------------
011300151125         // Operazioni di chiusura e garbage-collection lato Server
011400151125         //--------------------------------------------------------------
011500151125         BEGSR  sr_ChiudiServer;
011600151125
011700151125           if UBSOCKET2_Garbage(wServerSocket) = 0;
011800151125           endif;
011900151125
012000151125           return;
012100151125
012200151125         ENDSR;
012300151125
012400151125
012500151125      /END-FREE
012600151125
012700151125
