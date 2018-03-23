000002160217      //************************************************************************
000003160217      //
000004160217      // Questo programma è chiamato da AS777 via DRDA.
000005160217      // Questo programma è l'ingresso alla preparazione del dettaglio ORM.
000006160217      //
000007160217      //************************************************************************
000008160217
000100160217       CTL-OPT DFTACTGRP(*NO) OPTION(*NOUNREF:*NODEBUGIO:*SRCSTMT);
000101160218
000102160218      //************************************************************************
000103160218      //
000104160218      // Costanti.
000105160218      //
000106160218      //************************************************************************
000107160218
000108160218       /COPY GAITRASRC/SRCCONST,FIORC00R
000109160301       /COPY GAITRASRC/SRCCONST,TIO7G31R
000110160218
000111160218       DCL-C DETTAGLIO 'DETTAGLIO';
000112160218       DCL-C DONE 'DONE';
000113160218       DCL-C ERRORE 'ERRORE';
000114160218       DCL-C OTHERERROR 'OTHERERROR';
000200060626
000201160217      //************************************************************************
000202160217      //
000203160217      // Strutture.
000204160217      //
000205160217      //************************************************************************
000206160217
000400160218       DCL-DS tio1g3ids EXT QUALIFIED INZ(*EXTDFT) END-DS;
000401160218       DCL-DS tio1g3ods EXT QUALIFIED INZ(*EXTDFT) END-DS;
000402160218       DCL-DS fiorc0i0 EXT QUALIFIED INZ(*EXTDFT) END-DS;
000403160218       DCL-DS fiorc0o0 EXT QUALIFIED INZ(*EXTDFT) END-DS;
000404160218       DCL-DS fiorc0m0 EXT QUALIFIED INZ(*EXTDFT) END-DS;
000614160218
000615160217      //************************************************************************
000616160217      //
000617160217      // Campi.
000618160217      //
000619160217      //************************************************************************
000620160302
000621160302       DCL-DS this QUALIFIED INZ;
000622160302         rpyOpCode INT(10);
000623160302         rpyIdMsg INT(10);
000624160302         i INT(10);
000625160302         esito CHAR(1);
000626160302       END-DS;
000812160217
000813160217      //************************************************************************
000814160217      //
000815160217      // Prototipi.
000816160217      //
000817160217      //************************************************************************
000818160218
000819160218       /COPY GAITRASRC/SRCPROTOPR,FIORC00R
000820160301       /COPY GAITRASRC/SRCPROTOPR,TIO7G31R
000827160218
000900160217      //************************************************************************
001000160217      //
001100160217      // Parametri.
001200160217      //
001300160217      //************************************************************************
001301160302
001309160302       DCL-PI Tio7g3r;
001310160302         rqsOpc CHAR(10) CONST;
001311160302         rqsDta CHAR(9999) CONST;
001312160302         rqsLen PACKED(4) CONST;
001313160302         rqsCty CHAR(10) CONST;
001314160302         rqsCid CHAR(10) CONST;
001315160302         rpyOpc CHAR(10);
001316160302         rpyDta CHAR(9999);
001317160302         rpyLen PACKED(4);
001318160302       END-PI ;
001808160217
001809160217      //************************************************************************
001810160217      //
001811160217      // Main
001812160217      //
001813160217      //************************************************************************
002300041223
002301160217      /FREE
002302160217
002303160218       rpyOpc = DONE;
002304160218       CLEAR rpyDta;
002305160218       CLEAR rpyLen;
002306160218       RESET this;
002307160218
002308160218       SELECT;
002309160218
002310160218         WHEN rqsOpc = DETTAGLIO;
002311160218
002313160218           RESET tio1g3ids;
002314160218           RESET tio1g3ods;
002315160218           tio1g3ids = %SUBST(rqsDta : 1 : rqsLen);
002316160218
002317160218           // Con il numero prenotazione ritiro reperisco l'ID ORM.
002318160218
002319160218           IF tio1g3ids.npr <> *BLANK AND tio1g3ids.orm = *ZERO;
002320160219
002321160218             RESET fiorc0i0;
002322160218             RESET fiorc0o0;
002323160218             RESET fiorc0m0;
002324160218             fiorc0i0.npr = tio1g3ids.npr;
002325160218             fiorc0i0.idLingua = tio1g3ids.lang;
002326160219             fiorc0i0.unificante = tio1g3ids.ksc;
002327160219
002328160218             MONITOR;
002329160218               FIORC00_GetOrm( FIORC00_RQSOPCODE_GET
002330160218                             : this.rpyOpCode : this.rpyIdMsg
002331160218                             : fiorc0i0.formato : fiorc0i0 : %SIZE(fiorc0i0)
002332160218                             : fiorc0o0.formato : fiorc0o0 : %SIZE(fiorc0o0)
002333160218                             : fiorc0m0.formato : fiorc0m0 : %SIZE(fiorc0m0)
002334160218                             );
002335160218             ON-ERROR;
002336160218               rpyOpc = OTHERERROR;
002337160218               rpyDta = 'Chiamata a FIORC00_GetOrm() terminata in modo +
002338160218                         anomalo (' + %CHAR(%STATUS()) + ').';
002339160218               rpyLen = 255;
002340160218               RETURN;
002341160218             ENDMON;
002342160219
002343160218             IF this.rpyOpCode < *ZERO;
002344160218               rpyOpc = OTHERERROR;
002345160218               rpyDta = 'Chiamata a FIORC00_GetOrm() terminata in modo +
002346160218                         anomalo (' + %CHAR(this.rpyOpCode) + ').';
002347160218               rpyLen = 255;
002348160218               RETURN;
002349160218             ENDIF;
002351160218
002352160219             IF fiorc0m0.nrMsg > *ZERO;
002353160219               FOR this.i = 1 TO fiorc0m0.nrMsg;
002354160219                 IF %SUBST(fiorc0m0.errWarn : this.i : 1) = FIORC00_MSG_ERRORE;
002355160314                   rpyOpc = ERRORE;
002356160314                   rpyDta = %SUBST( fiorc0m0.textMsg
002357160314                                  : (this.i * 255) - 254 : 255);
002359160314                   rpyLen = 255;
002360160219                   RETURN;
002361160219                 ENDIF;
002362160219               ENDFOR;
002363160219             ENDIF;
002364160218
002365160219             tio1g3ids.orm = %DEC( %EDITC(fiorc0o0.poe : 'X')
002366160219                                 + %EDITC(fiorc0o0.nsr : 'X')
002367160219                                 + %EDITC(fiorc0o0.nor : 'X')
002368160219                                 + %EDITC(fiorc0o0.nrv : 'X') : 14 : 0);
002369160219
002370160219           ENDIF;
002371160218
002372160218           MONITOR;
002373160302             Tio7G31_GetDettaglioORM( TIO7G31_RQSOPCODE_GETDETTAGLIOORM
002375160302                              : this.rpyOpCode : this.rpyIdMsg
002376160302                              : tio1g3ids.formato : tio1g3ids : %SIZE(tio1g3ids)
002377160302                              : tio1g3ods.formato : tio1g3ods : %SIZE(tio1g3ods)
002379160302                              );
002380160218           ON-ERROR;
002381160218             rpyOpc = OTHERERROR;
002382160302             rpyDta = 'Chiamata a Tio7G31_GetDettaglioORM() terminata in modo +
002383160218                       anomalo (' + %CHAR(%STATUS()) + ').';
002384160218             rpyLen = 255;
002385160218             RETURN;
002386160218           ENDMON;
002700030620
002701160301           IF this.rpyOpCode = TIO7G31_RPYOPCODE_DONE;
002702160218             rpyDta = tio1g3ods;
002703160218             rpyLen = %SIZE(tio1g3ods);
002704160218           ELSE;
002707160218             rpyOpc = ERRORE;
002709160218           ENDIF;
002710160218
002711160218       ENDSL;
002712160218
002800160217       RETURN;
002801160217
002802160217      /END-FREE
002803160302
