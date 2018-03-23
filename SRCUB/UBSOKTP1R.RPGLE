000100151125     H Option(*SrcStmt)
000200160331     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('UBBNDDIR')
000300151126
000400151125     **-----------------------------------------------------------------------**
000500160323     ** EROGAZIONE SERVER SOCKET - UDP
000600151126     **-----------------------------------------------------------------------**
000700160330
000800160330     FTNTBE01L  IF   E           K DISK    usropn
000900151126
001000151008     D/COPY GAITRASRC/SRCPROTOPR,UBSOCKET2
001100151012     D/COPY GAITRASRC/SRCPROTOPI,UBSOCKET2
001200160324
001300160324
001400160324     D UBSOKTP2R       PR                  extpgm('UBSOKTP2R')
001500160324     D   pInMessage                1024A   const
001600160324     D   pInTypCtrl                   3A   const
001700160324     D   pOutDati                  1024A
001800160324     D   pOutResponse              1024A
001900160324     D   pOutEsito                    1N
002000160324
002100160330
002200160330     D UBSOKTP3C       PR                  extpgm('UBSOKTP3C')
002300160330     D   pInDati                   1024A   const
002400160330     D   pInSktAddr                  15A   const
002500160330     D   pInTypCtrl                   3A   const
002600160330     D   pOutEsito                    1N
002700160330
002800160324
002900160324     D UBSOKTPLR       PR                  extpgm('UBSOKTPLR')
003000160324     D   pApplType                   20A   const
003100160324     D   pServerAddr                 15A   const
003200160324     D   pClientAddr                 15A   const
003300160324     D   pSktType                     3A   const
003400160324     D   pSktPort                     5U 0 const
003500160324     D   pFlgCtrl                     1N   const
003600160324     D   pMessage                  1024A   const
003700160324     D   pResponse                 1024A   const
003800160324
003900160324
004000160324     D executeCommand...
004100160324     D                 PR            10I 0 EXTPROC('system')
004200160324     D  command                        *   VALUE OPTIONS(*STRING)
004300151218
004400160323
004500160331     D pInSrvAddress   S             15A   inz
004600160323     D pInSrvNIC_IP    S             15A   inz
004700160323     D pInSrvSktTyp    S              3A   inz
004800160323     D pInSrvPort      S              5U 0 inz
004900160323     D pInSrvPortRpy   S              5U 0 inz
005000160330     D pInSrvKillCnn   S              3S 0 inz
005100160324     D pInSrvLogging   S              1A   inz
005200160330     D pInTypCtrl      S              3A   inz
005300160331     D pInSrvCycWait   S              4P 0
005400160323
005500160323     D wServerSocket   S                   like(pInSktDescr) inz
005600160404     D wServerSocket2  S                   like(pInSktDescr) inz
005700160324     D wMessage        S           1024A   inz
005800160324     D wOutMsg         S           1024A   inz
005900160324     D wReply          S           1024A   inz
006000160324     D wCommand        S            512A   inz
006100160323     D wMainCycle      S               N   inz
006200160323     D wCorretto       S               N   inz
006300160323     D wCRLF           S              2A   inz(x'0D25')
006400160330     D wSrvKillCnn     S              3S 0 inz
006500160330     D i               S              3S 0 inz
006600160404
006700160404     D wRelMsg         S            128A   inz
006800160404     D wRelIP          S             15A   inz
006900160404     D wRelPort        S              5U 0 inz
007000160404     D posMsg          S              3S 0 inz
007100160404     D posIP           S              3S 0 inz
007200160404     D posPort         S              3S 0 inz
007300160404
007400160404
007500160323
007600151012
007700090520     C*
007800160331     C     *ENTRY        PLIST
007900160331     C                   PARM                    pInSrvCycWait
008000090610
008100090610      /FREE
008200160323
008300160323         // Verifica/impostazione default parametri di input servente
008400160330         exsr  sr_SetDefault;
008500160330
008600160330         // Carico le tabelle occorrenti
008700160330         exsr  sr_CarTab;
008800151125
008900160323         // Creazione server socket
009000160323         exsr  sr_MakeServer;
009100160330
009200160323         // Chiusura e garbage-collection lato Server
009300160323         exsr  sr_ChiudiServer;
009400160323
009500160331         // Fine *pgm ...
009600151125
009700151218
009800151218
009900151218         //--------------------------------------------------------------
010000160330         // Impostazione default parametri
010100151218         //--------------------------------------------------------------
010200160330         BEGSR  sr_SetDefault;
010300151218
010400160323           pInSrvNIC_IP  = '*';             // Tutte le interfacce
010500160323           pInSrvSktTyp  = 'UDP';           // Server Socket di tipo UDP
010600160323           pInSrvPort    = 13001;           // In ascolto su porta 13001 (regola firewall)
010700160323           pInSrvPortRpy = *zeros;          // Porta sulla quale rispondere: 0 = stessa porta source
010800160330           pInSrvKillCnn = 10;              // Forzo ciclo di "pulizia" socket su porta preposta
010900160324           pInSrvLogging = 'S';             // Imposto attivazione logging messaggi ricevuti
011000160330           pInTypCtrl    = 'TPP';           // Tipo controllo/azione da intraprendere
011100160331           pInSrvCycWait = 10;              // Attesa di default sono 10 secondi
011200160331
011300160330         ENDSR;
011400160330
011500160330
011600160330
011700160330         //-------------------------------------------------------------
011800160330         // Caricamento tabelle di procedura
011900160330         //-------------------------------------------------------------
012000160330         BEGSR sr_CarTab;
012100160330
012200160330           if not %open(tntbe01l);
012300160330              open tntbe01l;
012400160330           endif;
012500160330
012600160330           setll ('TPP':'UBSOKTP1R') tntbe01l;
012700160330           reade ('TPP':'UBSOKTP1R') tntbe01l;
012800160330
012900160330           dow not %eof(tntbe01l);
013000160330             select;
013100160330
013200160330               when %trim(tbeKE2) = 'SRV-KILL-CNN';
013300160330                    if tbeUNI <> *blanks;
013400160330                       pInSrvKillCnn = %dec(%trim(tbeUNI):3:0);
013500160330                    endif;
013600160330
013700160330               when %trim(tbeKE2) = 'SRV-LOG';
013800160330                    if tbeUNI <> *blanks;
013900160330                       pInSrvLogging = %trim(tbeUNI);
014000160330                    endif;
014100160330
014200160330               when %trim(tbeKE2) = 'SRV-NIC-IP';
014300160330                    if tbeUNI <> *blanks;
014400160330                       pInSrvNIC_IP = %trim(tbeUNI);
014500160330                    endif;
014600160330
014700160330               when %trim(tbeKE2) = 'SRV-POR-REPLY';
014800160330                    if tbeUNI <> *blanks;
014900160330                       pInSrvPortRpy = %dec(%trim(tbeUNI):5:0);
015000160330                    endif;
015100160330
015200160330               when %trim(tbeKE2) = 'SRV-PORT-ACCEPT';
015300160330                    if tbeUNI <> *blanks;
015400160330                       pInSrvPort = %dec(%trim(tbeUNI):5:0);
015500160330                    endif;
015600160330
015700160330               when %trim(tbeKE2) = 'SRV-SKT-TYPE';
015800160330                    if tbeUNI <> *blanks;
015900160330                       pInSrvSktTyp = %trim(tbeUNI);
016000160330                    endif;
016100160330
016200160330               when %trim(tbeKE2) = 'SRV-TYPE-CTRL';
016300160330                    if tbeUNI <> *blanks;
016400160330                       pInTypCtrl = %trim(tbeUNI);
016500160330                    endif;
016600160331
016700160331               when %trim(tbeKE2) = 'SRV-CICLO-WAIT';
016800160331                    if tbeUNI <> *blanks;
016900160331                       pInSrvCycWait = %dec(%trim(tbeUNI):4:0);
017000160331                    endif;
017100160330
017200160330             endsl;
017300160330
017400160330             reade ('TPP':'UBSOKTP1R') tntbe01l;
017500160330           enddo;
017600160330
017700160330           if %open(tntbe01l);
017800160330              close tntbe01l;
017900160330           endif;
018000160330
018100160330         ENDSR;
018200151125
018300151218
018400151125
018500151125         //--------------------------------------------------------------
018600151125         // Operazioni di "creazione server socket"
018700151125         //--------------------------------------------------------------
018800151125         BEGSR  sr_MakeServer;
018900151125
019000151218           // Imposto i valori di procedura per il server corrente richiesto
019100151218           if pInSrvNIC_IP = '*';
019200160323              pInSrvAddress = 'INADDR_ANY';
019300151218           endif;
019400160323           if pInSrvPortRpy = *zeros;
019500160323              pInSrvPortRpy = pInSrvPort;
019600160323           endif;
019700160324
019800160324
019900160324           // Monitoro l'intero processo
020000160330           monitor;
020100160324
020200160330           // Ciclo "pulizia" socket su porta preposta
020300160330           wSrvKillCnn = pInSrvKillCnn;
020400160330           exsr  sr_ClrBind;
020500160323
020600151202           // Creo il socket di tipo UDP
020700160323           if UBSOCKET2_Create(pInSrvSktTyp : wServerSocket) = 0;
020800160323
020900160323              // Forzo il socket a ri-utilizzare la porta
021000160323              callp UBSOCKET2_SetSockOption(wServerSocket : SOL_SOCKET :
021100160323                                            SO_REUSEADDR : 1);
021200151126
021300151126              // Forzo il socket ad un "linger" (permanenza) di 120 secondi
021400160324              callp UBSOCKET2_SetSockOption(wServerSocket : SOL_SOCKET :
021500160324                                            SO_LINGER : 120);
021600151126
021700151125           else;
021800160330
021900151125              // Chiusura e garbage-collection lato Server
022000151125              exsr  sr_ChiudiServer;
022100151125           endif;
022200151125
022300151125
022400151202           // Bindo il socket appena creato creando quindi il servente UDP
022500160323           if UBSOCKET2_Bind(wServerSocket : pInSrvAddress : pInSrvPort) = 0;
022600160323
022700160323
022800160330              // Ciclo al infinito ... in attesa di messaggi socket
022900151217              wMainCycle = *on;
023000151217              dow wMainCycle;
023100151202
023200160323
023300151202                  // Dentro ciclo di ascolto lato server testo costantemente lo shut-down
023400151202                  if %shtdn;
023500151202                     // Chiusura e garbage-collection lato Server
023600151202                     exsr  sr_ChiudiServer;
023700151202                  endif;
023800151202
023900151216
024000151216                  // Se sin qui tutto OK => ricevo il buffer del messaggio socket
024100160323                  pInMsgCvtTbl  = *blanks;
024200160323                  if UBSOCKET2_ReceiveFrom(wServerSocket : 1024 : '3' :
024300151216                     pInMsgCvtTbl : pOutRespLen : wMessage : pOutRespFull :
024400151217                     pOutSktAddrPtr : pOutSktAddr) = 0;
024500151216
024600160324
024700160330                     // Verifico se trattasi di messaggio di "comando" oppure di "messaggio client"
024800160330                     select;
024900160330
025000160330                       when %scan('<UBSOKTP1R_END>' : wMessage) > *zeros;
025100160330                            wReply = '<END OK>';
025200160330                            wMainCycle = *off;
025300160330
025400160330                       when %scan('<UBSOKTP1R_RELOAD>' : wMessage) > *zeros;
025500160330                            wReply = '<RELOAD OK>';
025600160331                            wMainCycle = *off;
025700160331                            pInSrvCycWait = 1;
025800160404
025900160404                       when %scan('<UBSOKTP1R_RELMSG:' : wMessage) > *zeros;
026000160404                            wReply = '<RELMSG OK>';
026100160404                            exsr  sr_RelayMessage;
026200160330
026300160330                       other;
026400160324
026500160330                         // Se messaggio ricevuto è completo verifico se i dati sono corretti
026600160330                         if pOutRespFull = 1;
026700160330                            // Effettuo chiamata a driver di controllo dati con tipo da parametro
026800160330                            callp UBSOKTP2R(wMessage : pInTypCtrl : wOutMsg :
026900160330                                            wReply : wCorretto);
027000160404                            // Se tutto OK effettuo chiamata di "esecuzione" coi dati del messaggio
027100160330                            if wCorretto;
027200160330                               callp UBSOKTP3C(wOutMsg : pOutSktAddr :
027300160330                                            pInTypCtrl : wCorretto);
027400160330                            endif;
027500160330                         else;
027600160330                            wCorretto = *off;
027700160330                         endif;
027800160323
027900160330                     endsl;
028000160323
028100160330
028200160330                     pInMsgCvtTbl  = *blanks;
028300160404                     if UBSOCKET2_ReplyTo(wServerSocket :
028400160404                               %len(%trim(wReply))+%len(%trim(wCRLF)) :
028500160404                               %trim(wReply)+%trim(wCRLF):
028600160404                               pInMsgCvtTbl :
028700160404                               pOutSktAddrPtr :
028800160404                               pInSrvPortRpy) < 0;
028900160330                     endif;
029000160324
029100160324
029200160324                     // Se richiesto loggo tutti i messaggi ricevuti
029300160324                     if pInSrvLogging = 'S';
029400160324                        // Effettuo chiamata a driver di logging
029500160330                        callp UBSOKTPLR(pInTypCtrl : pInSrvAddress :
029600160330                                 pOutSktAddr : pInSrvSktTyp : pInSrvPort :
029700160330                                 wCorretto : wMessage : wReply);
029800160324                     endif;
029900160324
030000160324                  endif;
030100160324
030200151202              enddo;
030300151202
030400151125           else;
030500151125              // Chiusura e garbage-collection lato Server
030600151125              exsr  sr_ChiudiServer;
030700151125           endif;
030800160323
030900160323
031000160330           on-error;
031100160323
031200160323              // Chiusura e garbage-collection lato Server
031300160330              exsr  sr_ChiudiServer;
031400160323
031500160330           endmon;
031600151125
031700151125         ENDSR;
031800160330
031900160330
032000160330
032100160330         //--------------------------------------------------------------
032200160330         // Ciclo "pulizia" socket su porta preposta
032300160330         //--------------------------------------------------------------
032400160330         BEGSR  sr_ClrBind;
032500160330
032600160330           // Killo eventuali connessioni già in essere sulla stessa
032700160330           // NIC/porta indicate per il servente corrente
032800160330           for i=1 to wSrvKillCnn;
032900160330               wCommand = 'ENDTCPCNN PROTOCOL(*'+%trim(pInSrvSktTyp)+')' +
033000160330                          ' LCLINTNETA('''+%trim(pInSrvNIC_IP)+''')' +
033100160330                          ' LCLPORT('+%trim(%editc(pInSrvPort:'4'))+')';
033200160330               if executeCommand(%trim(wCommand)) <> *zeros;
033300160330               endif;
033400160330           endfor;
033500160330
033600160330         ENDSR;
033700151125
033800160404
033900160404
034000160404         //--------------------------------------------------------------
034100160404         // Operazioni per Relay Message: da un socket client ad altro socket client tramite server
034200160404         //--------------------------------------------------------------
034300160404         BEGSR  sr_RelayMessage;
034400160404
034500160404           // Creo un nuovo socket client per il "relay" sel messaggio
034600160404           if UBSOCKET2_Create(pInSrvSktTyp : wServerSocket2) = 0;
034700160404
034800160404              // Forzo il socket a ri-utilizzare la porta
034900160404              callp UBSOCKET2_SetSockOption(wServerSocket2 : SOL_SOCKET :
035000160404                                            SO_REUSEADDR : 1);
035100160404
035200160404              // Forzo il socket ad un "linger" (permanenza) di 120 secondi
035300160404              callp UBSOCKET2_SetSockOption(wServerSocket2 : SOL_SOCKET :
035400160404                                            SO_LINGER : 120);
035500160404
035600160404              wMessage = %trim(wMessage);
035700160404              posMsg   = %scan(':':wMessage:1)+1;
035800160404              posIP    = %scan(':':wMessage:posMsg)+1;
035900160404              posPort  = %scan(':':wMessage:posIP)+1;
036000160404              wRelMsg  = %subst(wMessage:posMsg:posIP-posMsg-1);
036100160404              wRelIP   = %subst(wMessage:posIP:posPort-posIP-1);
036200160404              wRelPort = %dec(%subst(wMessage:posPort:
036300160404                           %len(%trim(wMessage))-posPort):5:0);
036400160404
036500160404              // Relayo  sul socket client e porta richiesti il messaggio ricevuto
036600160404              pInMsgCvtTbl  = *blanks;
036700160404              if UBSOCKET2_SendTo(wServerSocket2 :
036800160404                        %len(%trim(wRelMsg))+%len(%trim(wCRLF)) :
036900160404                        %trim(wRelMsg)+%trim(wCRLF):
037000160404                        pInMsgCvtTbl :
037100160404                        wRelIP :
037200160404                        wRelPort) < 0;
037300160404              endif;
037400160404
037500160404              // Chiusura e garbage-collection del nuovo socket client "relay" apppena creato
037600160404              if UBSOCKET2_Garbage(wServerSocket2) = 0;
037700160404              endif;
037800160404
037900160404           else;
038000160404
038100160404              // Chiusura e garbage-collection del nuovo socket client "relay" apppena creato
038200160404              if UBSOCKET2_Garbage(wServerSocket2) = 0;
038300160404              endif;
038400160404
038500160404           endif;
038600160404
038700160404         ENDSR;
038800151125
038900160404
039000151125
039100151125         //--------------------------------------------------------------
039200151125         // Operazioni di chiusura e garbage-collection lato Server
039300151125         //--------------------------------------------------------------
039400151125         BEGSR  sr_ChiudiServer;
039500160330
039600160330           // Ciclo "pulizia" socket su porta preposta
039700160330           wSrvKillCnn = 1;
039800160330           exsr  sr_ClrBind;
039900151125
040000151125           if UBSOCKET2_Garbage(wServerSocket) = 0;
040100151125           endif;
040200151126
040300160331           *inlr = *on;
040400151125           return;
040500151125
040600151125         ENDSR;
040700151125
040800151125
040900151125      /END-FREE
041000151125
