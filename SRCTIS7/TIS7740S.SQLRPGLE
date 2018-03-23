000100160317      // ***********************************************************************
000101160317      //
000102160317      // Questo programma è il corrispondente del HSM server TIS1740S.
000103160317      //
000104160317      // ***********************************************************************
000105160317
000106160317       CTL-OPT DFTACTGRP(*NO) OPTION(*NOUNREF:*NODEBUGIO:*SRCSTMT);
000107160317
000108160317      // ***********************************************************************
000109160317      //
000110160317      // Costanti.
000111160317      //
000112160317      // ***********************************************************************
000113160317
000114160317      /COPY GAITRASRC/SRCCONST,TIS7740S
000115160317      /COPY GAITRASRC/SRCCONST,TRTC0TTR
000116160317
000117160317      // ***********************************************************************
000118160317      //
000119160317      // Strutture.
000120160317      //
000121160317      // ***********************************************************************
000122160317
000126160317       DCL-DS this QUALIFIED INZ;
000127160317         esito INT(10);
000128160317       END-DS;
000130160317
000131160317      // ***********************************************************************
000132160317      //
000133160317      // Prototipi.
000134160317      //
000135160317      // ***********************************************************************
000136160317
000137160317      /COPY GAITRASRC/SRCPROTOPR,TIS7740S
000138160317      /COPY GAITRASRC/SRCPROTOPR,TRTC0TTR
000139160317
000140160317      // ***********************************************************************
000141160317      //
000142160317      // Parametri.
000143160317      //
000144160317      // ***********************************************************************
000145160317
000146160317       DCL-PI Tis7740s;
000147160317         rqsOpc CHAR(10) CONST;
000148160317         rqsDta CHAR(9999) CONST;
000149160317         rqsLen PACKED(4) CONST;
000150160317         rqsCty CHAR(10) CONST;
000151160317         rqsCid CHAR(10) CONST;
000152160317         rpyOpc CHAR(10);
000153160317         rpyDta CHAR(9999);
000154160317         rpyLen PACKED(4);
000155160317       END-PI ;
000156160317
000157160317      // ***********************************************************************
000158160317      //
000159160317      // Main.
000160160317      //
000161160317      // ***********************************************************************
000164160317
000165160317       RESET this;
000166160317       rpyOpc = TIS7740_RPYOPC_DONE;
000167160317       CLEAR rpyDta;
000168160317       CLEAR rpyLen;
000169160317
000170160317       SELECT;
000171160317         WHEN rqsOpc = TIS7740_RQSOPC_GETLINKPTN;
000172160317           this.esito = GetLinkPartner();
000173160317         WHEN rqsOpc = TIS7740_RQSOPC_STOP;
000174160317           *INLR = *ON;
000175160317         OTHER;
000176160317           rpyOpc = TIS7740_RPYOPC_OTHERERROR;
000177160317           rpyDta = 'Codice operativo "' + rqsOpc + '" sconosciuto.';
000178160317           rpyLen = %LEN(%TRIMR(rpyDta));
000179160317       ENDSL;
000180160317
000181160317       RETURN;
000182160317
000183160317       // --------------------------------------------------
000184160317       // Procedure name: GetLinkPartner
000185160317       // Purpose:
000186160317       // Returns:        Esito
000187160317       // --------------------------------------------------
000188160317
000189160317       DCL-PROC GetLinkPartner ;
000190160317
000191160317         DCL-PI *N INT(10);
000192160317         END-PI ;
000193160317
000194160317         DCL-S retField INT(10);
000195160317         DCL-DS trtc0tti0 EXT QUALIFIED INZ(*EXTDFT) END-DS;
000196160317         DCL-DS trtc0tto0 EXT QUALIFIED INZ(*EXTDFT) END-DS;
000202160317
000203160317         DCL-DS local QUALIFIED INZ;
000204160317           rpyOpCode INT(10);
000205160317           rpyIdMsg INT(10);
000206160317         END-DS;
000207160317
000208160317         trtc0tti0 = %SUBST(rqsDta : 1 : rqsLen);
000210160317
000211160317         MONITOR;
000212160317           Trtc0tt_http( TRTC0TT_RQSOPCODE_GET_LINKPTN
000213160317                       : local.rpyOpCode : local.rpyIdMsg
000214160317                       : trtc0tti0.formato : trtc0tti0 : %SIZE(trtc0tti0)
000215160317                       : trtc0tto0.formato : trtc0tto0 : %SIZE(trtc0tto0) );
000216160317         ON-ERROR;
000217160317           rpyOpc = TIS7740_RPYOPC_OTHERERROR;
000218160317           rpyDta = 'Chiamata a Trtc0tt_http() terminata in modo anomalo.';
000219160317           rpyLen = %LEN(%TRIMR(rpyDta));
000220160317           RETURN -1;
000221160317         ENDMON;
000222160317
000223160317         IF local.rpyOpCode < TRTC0TT_ESITO_OK;
000224160317           rpyOpc = TIS7740_RPYOPC_ERROR;
000226160317           RETURN -1;
000228160317         ENDIF;
000229160317
000230160317         rpyDta = trtc0tto0;
000231160317         rpyLen = %SIZE(trtc0tto0);
000232160317
000233160317         RETURN retField ;
000234160317
000235160317       END-PROC ;
000236160317
