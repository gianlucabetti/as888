000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200110421     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR':'UBJDBCR4':'QC2LE') ACTGRP('BARTVAS')
000300991027
000400110420     FTIVGD00F  UF A E             DISK    USROPN
000500991027
000600110421
000700110421     D Sleep           PR            10I 0 ExtProc('sleep')
000800110421     D Seconds                       10U 0 Value
000900110420
001000110419     D KPJBA         E DS
001100110419
001200110419     D DS_INPUT        DS
001300110419     D  I_KSULST                      8A
001400110419     D  I_KSUVAS                      8A
001500110419     D  I_TIPLST                      2A
001600110419     D  I_TIPVAS                      2A
001700110419     D  I_PROC                        5S 0
001800110504     D  I_FLGDWL                      1A
001900110504     D  I_DATADA                     23A
002000110420
002100110420     D TRUL47DS      E DS
002200110420     D TIS781DS      E DS
002300110420     D TIS781DFLO    E DS
002400110420     D DVGDFLO       E DS
002500110420
002600110420     D wDataOraStart   S               Z   INZ
002700110420     D wDataOraStartA  S             23A   INZ
002800110420     D wDataOraEnd     S               Z   INZ
002900110420     D wDataOraEndA    S             23A   INZ
003000110420     D wDataOraCheck   S               Z   INZ
003100110420     D wTimeTaken      S             15P 0
003200110420     D wPrepStm        S                   LIKE(PreparedStatement)
003300110420     D wRs             S                   LIKE(ResultSet)
003400110420     D wNomeFile       S            256A   INZ
003500110420
003600110419
003700110419
003800110419     D*-------------------
003900110419     D* DEFINIZIONI PROCEDURE ESTERNE
004000110419     D*-------------------
004100110419     D/COPY GAITRASRC/SRCPROTOPI,UBBVSKT01
004200110419     D/COPY GAITRASRC/SRCPROTOPR,UBBVSKT01
004300110420     D/COPY GAITRASRC/SRCPROTOPI,UBBVSQL01
004400110420     D/COPY GAITRASRC/SRCPROTOPR,UBBVSQL01
004500110420     D/COPY GAITRASRC/SRCPROTOPR,UBBVSQL02
004600110420     D/COPY GAITRASRC/SRCPROTOPR,UBJDBC_H
004700110419
004800110419
004900991027
005000110419     C*
005100110419     C* Inizializzo indicatore d errore generale
005200110419     C                   SETOFF                                       40
005300110504     C*
005400110504     C* Verifica i parametri in ingresso
005500110504     C                   EVAL      DS_INPUT = KPJBU
005600110420     C*
005700110504     C* Se nn già passata come parametro, salvo la data/ora iniziale
005800110504     C                   IF        I_DATADA = *blanks
005900110504     C                   EVAL      wDataOraStart  = %timestamp
006000110504     C                   EVAL      wDataOraStartA = %char(wDataOraStart)
006100110504     C                   ELSE
006200110504     C                   EVAL      wDataOraStartA = I_DATADA
006300110504     C                   ENDIF
006400110504     C*
006500110419     C                   IF        I_KSULST <> *blanks
006600110419     C                   IF        I_KSUVAS  = *blanks
006700110419     C                   EVAL      I_KSUVAS  = I_KSULST
006800110419     C                   ENDIF
006900110419     C                   IF        I_TIPLST  = *blanks
007000110419     C                   EVAL      I_TIPLST  = 'PI'
007100110419     C                   ENDIF
007200110419     C                   IF        I_TIPVAS  = *blanks
007300110419     C                   EVAL      I_TIPVAS  = 'EP'
007400110419     C                   ENDIF
007500110419     C                   IF        I_PROC    = *zeros
007600110419     C                   EVAL      I_PROC    = 23
007700110419     C                   ENDIF
007800110419     C*
007900110420     C*  1) Apro la connessione alla "BARTVAS"
008000110420     C                   EXSR      opnCONN
008100110504     C*
008200110504     C* Se nei parametri nn richiesto "solo download"
008300110504     C                   IF        I_FLGDWL = *blanks
008400110419     C*
008500110502     C*  2) Update anagrafico XX da processo 0 a processo ZZ
008600110419     C  N40              EXSR      udpFrom0ToZZ
008700110419     C*
008800110420     C*  3) Forzatura ricaricamento cliente/tipo file XX/processo ZZ
008900110419     C  N40              EXSR      exeReload_XX
009000110419     C*
009100110420     C*  4) Attesa job d controllo cliente/tipo file 'XX'
009200110419     C  N40              EXSR      waitCtrlXX
009300110419     C*
009400110420     C*  5) Forzatura esecuzione cliente/tipo file 'XX'
009500110419     C  N40              EXSR      exeXX
009600110419     C*
009700110420     C*  6) Attesa presenza job d trasferimento cliente/tipo file 'XX'
009800110419     C  N40              EXSR      waitFindXX
009900110419     C*
010000110420     C*  7) Attesa assenza job d trasferimento cliente/tipo file 'XX'
010100110419     C  N40              EXSR      waitExeXX
010200110504     C*
010300110504     C                   ENDIF
010400110419     C*
010500110420     C*  8) Lettura file inviati da JobLog BartVAS e scrive TIVGD
010600110503     C                   EXSR      listSentXX
010700110419     C*
010800110502     C*  9) Update anagrafico XX da processo ZZ a processo 0
010900110502     C                   EXSR      udpFromZZTo0
011000110420     C*
011100110420     C* 10) Forzatura ricaricamento cliente/tipo file XX/processo ZZ
011200110502     C                   EXSR      exeReload_XX
011300110419     C*
011400110420     C* 11) Forzatura esecuzione cliente/tipo file YY
011500110503     C                   EXSR      exeYY
011600110419     C*
011700110419     C                   ENDIF
011800110420     C*
011900110420     C* 12) Chiudo la connessione alla "BARTVAS"
012000110420     C                   EXSR      clsCONN
012100110419     C*
012200921023     C                   SETON                                        LR
012300110420
012400110420
012500110420
012600110420     C*  1)
012700110420     C     OPNCONN       BEGSR
012800110420     C*
012900110420      /FREE
013000110420
013100110420           if UBBVSQL01_Connect('BARTVAS' : pOutConn) = 0;
013200110420              pInConn = pOutConn;
013300110420           else;
013400110420              *in40 = *on;
013500110420           endif;
013600110420
013700110420      /END-FREE
013800110420      *
013900110420     C                   ENDSR
014000110419
014100110419
014200110419
014300110420     C*  2)
014400110419     C     udpFrom0ToZZ  BEGSR
014500110420     C*
014600110419      /FREE
014700110419
014800110419         if UBBVSQL02_Upd02UnqCliTipFile(
014900110419                         pInConn
015000110419                       : I_KSULST
015100110419                       : I_TIPLST
015200110419                       : I_PROC) = 0;
015300110419         else;
015400110419            *in40 = *on;
015500110419         endif;
015600110419
015700110419      /END-FREE
015800110419     C*
015900110419     C                   ENDSR
016000110419
016100110419
016200110419
016300110420     C*  3)
016400110419     C     exeReload_XX  BEGSR
016500110420     C*
016600110420      /FREE
016700110420
016800110420         if UBBVSKT01_ReloadCliTipF(
016900110420                   I_KSULST:I_TIPLST:I_PROC) = 0;
017000110420         else;
017100110420            *in40 = *on;
017200110420         endif;
017300110420
017400110420      /END-FREE
017500110419     C*
017600110419     C                   ENDSR
017700110420
017800140613
017900110420
018000110420     C*  4)
018100110420     C     waitCtrlXX    BEGSR
018200110420     C*
018300110420     C                   eval      wDataOraCheck = %timestamp
018400110420     C*
018500110420     C* Ciclo sino a che:
018600110420     C*  - o ho trovato job d tipo controllo x il cliente/tipo file richiesti
018700110420     C*  - o al massimo sono passati 60 minuti
018800110420     C                   setoff                                       55
018900110420     C                   dow       not *in55
019000110420     C*
019100110420     C* Verifica esistenza job d controllo
019200110420     C                   if        not *in55
019300110420     C*
019400110420      /FREE
019500110420
019600110420         if UBBVSQL01_RtvInfoJob(pInConn : I_KSULST : I_TIPLST :
019700110420                      1 : pOutProcesso : pOutNumJob :
019800110420                      pOutTipTrasf :  pOutStsJob) = 0;
019900110420            if pOutStsJob = 'A';
020000110420               *in55 = *on;
020100110420            endif;
020200110420         endif;
020300110420
020400110420      /END-FREE
020500110420     C*
020600110420     C                   endif
020700110503     C*
020800110503     C* Attendo 5 secondi x ottimizzare
020900110503     C  N55              CallP     Sleep(5)
021000110420     C*
021100110420     C* Verifica condizione sul tempo passato
021200110420     C                   if        not *in55 AND
021300110420     C                             %diff(%timestamp :
021400110420     C                                   wDataOraCheck : *MINUTES) > 60
021500110420     C                   seton                                        55
021600110420     C                   seton                                        40
021700110420     C                   endif
021800110420     C*
021900110420     C                   enddo
022000110420     C*
022100110420     C                   ENDSR
022200110419
022300110419
022400110419
022500110420     C*  5)
022600110419     C     exeXX         BEGSR
022700110420     C*
022800110420      /FREE
022900110420
023000110420         if UBBVSKT01_ExeImmedCliTipF(
023100110420                      I_KSULST:I_TIPLST) = 0;
023200110420         else;
023300110420            *in40 = *on;
023400110420         endif;
023500110420
023600110420      /END-FREE
023700030908     C*
023800110422     C* Attendo 60 secondi
023900110422     C  N40              CallP     Sleep(60)
024000110422     C*
024100030908     C                   ENDSR
024200110420
024300110420
024400110420
024500110420     C*  6)
024600110420     C     waitFindXX    BEGSR
024700110420     C*
024800110420     C                   eval      wDataOraCheck = %timestamp
024900110420     C*
025000110420     C* Ciclo sino a che:
025100110420     C*  - o ho trovati job d tipo trasferimento x il cliente/tipo file richiesti
025200110503     C*  - o al massimo sono passati 10 minuti
025300110420     C                   setoff                                       55
025400110420     C                   dow       not *in55
025500110420     C*
025600110421     C* Verifica esistenza job d trasferimento
025700110420     C                   if        not *in55
025800110420     C*
025900110420      /FREE
026000110420
026100110420         if UBBVSQL01_RtvInfoJob(pInConn : I_KSULST : I_TIPLST :
026200110422                      2 : pOutProcesso : pOutNumJob :
026300110420                      pOutTipTrasf :  pOutStsJob) = 0;
026400110420            if pOutStsJob = 'A';
026500110420               *in55 = *on;
026600110420            endif;
026700110420         endif;
026800110503
026900110420      /END-FREE
027000110420     C*
027100110420     C                   endif
027200110421     C*
027300110503     C* Attendo 5 secondi x ottimizzare
027400110503     C  N55              CallP     Sleep(5)
027500110420     C*
027600110420     C* Verifica condizione sul tempo passato
027700110420     C                   if        not *in55 AND
027800110420     C                             %diff(%timestamp :
027900110429     C                                   wDataOraCheck : *MINUTES) > 10
028000110420     C                   seton                                        55
028100110420     C                   seton                                        40
028200110420     C                   endif
028300110420     C*
028400110420     C                   enddo
028500110420     C*
028600110420     C                   ENDSR
028700110420
028800110420
028900110420
029000110420     C*  7)
029100110420     C     waitExeXX     BEGSR
029200110420     C*
029300110420     C                   eval      wDataOraCheck = %timestamp
029400110420     C*
029500110420     C* Ciclo sino a che:
029600110421     C*  - o nn ho trovato job d tipo trasferimento x il cliente/tipo file richiesti
029700140613     C*  - o al massimo sono passati 120 minuti
029800110420     C                   setoff                                       55
029900110420     C                   dow       not *in55
030000110420     C*
030100110421     C* Verifica nn esistenza job d trasferimento
030200110420     C                   if        not *in55
030300110420     C*
030400110420      /FREE
030500110420
030600110420         if UBBVSQL01_RtvInfoJob(pInConn : I_KSULST : I_TIPLST :
030700110422                      2 : pOutProcesso : pOutNumJob :
030800110420                      pOutTipTrasf :  pOutStsJob) = 0;
030900110421            if pOutNumJob = 0;
031000110421               *in55 = *on;
031100110421            endif;
031200110420         endif;
031300110420
031400110420      /END-FREE
031500110420     C*
031600110420     C                   endif
031700110421     C*
031800110422     C* Attendo 30 secondi x "ottimizzare"
031900110422     C  N55              CallP     Sleep(30)
032000110420     C*
032100110420     C* Verifica condizione sul tempo passato
032200110420     C                   if        not *in55 AND
032300110420     C                             %diff(%timestamp :
032400140613     C                                   wDataOraCheck : *MINUTES) > 120
032500110420     C                   seton                                        55
032600110420     C                   seton                                        40
032700110420     C                   endif
032800110420     C*
032900110420     C                   enddo
033000110504     C*
033100110504     C* Attendo 30 secondi
033200110504     C                   CallP     Sleep(30)
033300110420     C*
033400110420     C                   ENDSR
033500110420
033600110420
033700110420
033800110420     C*  8)
033900110420     C     listSentXX    BEGSR
034000110504     C*
034100110504     C* Salvo la data/ora finale
034200110504     C                   EVAL      wDataOraEnd = %timestamp
034300110420     C*
034400110420     C                   open      tivgd00f
034500110420     C*
034600110420     C* Come prima cosa avvio il blocco elaborazione TIVGD x tipo file corrente: 'VC'
034700110420     C                   clear                   TRUL47DS
034800110420     C                   eval      d47opz  = 'I'
034900120927     C                   eval      d47tip  = I_TIPVAS
035000110420     C                   eval      d47lck  = 'N'
035100110420     C                   eval      d47chkj = 'N'
035200110420     C                   eval      d47pgm  = 'TIS7TEPR'
035300110420     C                   call      'TRUL47R'
035400110420     C                   parm                    TRUL47DS
035500110420     C*
035600110420     C* Se elaborazione consentita => proseguo
035700120928     C***                if        d47sts <> 'A'
035800110420     C*
035900110420     C* Formatto le date/ore nel modo corretto
036000110420     C                   EVAL      %subst(wDataOraStartA : 11 : 1) = *blanks
036100110420     C                   EVAL      %subst(wDataOraStartA : 14 : 1) = ':'
036200110420     C                   EVAL      %subst(wDataOraStartA : 17 : 1) = ':'
036300110420     C                   EVAL      %subst(wDataOraStartA : 20 : 1) = ':'
036400110420     C*
036500110420     C                   EVAL      wDataOraEndA = %char(wDataOraEnd)
036600110420     C                   EVAL      %subst(wDataOraEndA : 11 : 1) = *blanks
036700110420     C                   EVAL      %subst(wDataOraEndA : 14 : 1) = ':'
036800110420     C                   EVAL      %subst(wDataOraEndA : 17 : 1) = ':'
036900110420     C                   EVAL      %subst(wDataOraEndA : 20 : 1) = ':'
037000110420     C*
037100110420      /FREE
037200110420
037300110420         wPrepStm = JDBC_prepStmt(pInConn :
037400110425           'SELECT DISTINCT JobLog.NomeFile ' +
037500110420           'FROM   JobLog ' +
037600110420           'WHERE  JobLog.CodiceCliente = ?   AND ' +
037700110421                 ' JobLog.TipoFile = ?        AND ' +
037800110421                 ' JobLog.DataEOraReale between ? AND ? AND ' +
037900110425                 ' JobLog.TipoJob  = 2        AND ' +
038000110420                 ' JobLog.Esito    = 0        AND ' +
038100110425                 ' JobLog.NomeFile <> '' ''   AND ' +
038200110420                 ' JobLog.DimensioneFile > 0  ' );
038300110420
038400110420         if (wPrepStm = *NULL);
038500110420            *in40 = *on;
038600110420         else;
038700110420
038800110420            // Set SQL parameter values
038900110420            JDBC_SetString(wPrepStm:1:I_KSULST);
039000110420            JDBC_SetString(wPrepStm:2:I_TIPLST);
039100110421            JDBC_SetString(wPrepStm:3:wDataOraStartA);
039200110421            JDBC_SetString(wPrepStm:4:wDataOraEndA);
039300110420
039400110420            // Query the database
039500110420            wRs = jdbc_ExecPrepQry(wPrepStm);
039600110420
039700110420            dow (jdbc_nextRow(wRs));
039800110420               wNomeFile = jdbc_getCol(wRs : 1);
039900110420               exsr WRITIVGD;
040000110420            enddo;
040100110420
040200110420            JDBC_FreeResult(wRs);
040300110420            JDBC_FreePrepStmt(wPrepStm);
040400110420
040500110420         endif;
040600110420
040700110420      /END-FREE
040800110420     C*
040900110420     C* Infine elimino il blocco elaborazione TIVGD x tipo file corrente: 'VC'
041000110420     C                   clear                   TRUL47DS
041100110420     C                   eval      d47opz  = 'F'
041200120928     C                   eval      d47tip  = I_TIPVAS
041300110420     C                   call      'TRUL47R'
041400110420     C                   parm                    TRUL47DS
041500110420     C*
041600120928     C***                endif
041700110420     C                   close     tivgd00f
041800110420     C  N40              exsr      forzaDWN
041900110420     C*
042000110420     C                   ENDSR
042100110419
042200110419
042300110419
042400110420     C*  9)
042500110419     C     udpFromZZTo0  BEGSR
042600110419     C*
042700110419      /FREE
042800110419
042900110419         if UBBVSQL02_Upd02UnqCliTipFile(
043000110419                         pInConn
043100110419                       : I_KSULST
043200110419                       : I_TIPLST
043300110419                       : 0) = 0;
043400110419         else;
043500110419            *in40 = *on;
043600110419         endif;
043700110419
043800110419      /END-FREE
043900110419     C*
044000110419     C                   ENDSR
044100110419
044200110419
044300110419
044400110420     C* 11)
044500110419     C     exeYY         BEGSR
044600110420     C*
044700110420      /FREE
044800110420
044900110420         if UBBVSKT01_ExeImmedCliTipF(
045000110420                      I_KSUVAS:I_TIPVAS) = 0;
045100110420         else;
045200110420            *in40 = *on;
045300110420         endif;
045400110420
045500110420      /END-FREE
045600110419     C*
045700110419     C                   ENDSR
045800110419
045900110419
046000110419
046100110420     C* 12)
046200110419     C     CLSCONN       BEGSR
046300110420     C*
046400110419      /FREE
046500110419
046600110419           if UBBVSQL01_Disconnect(pInConn) = 0;
046700110419           endif;
046800110419
046900110419      /END-FREE
047000110420     C*
047100110419     C                   ENDSR
047200110420
047300110420
047400110420
047500110420     C*------------------------------------------------------------------------*
047600110420     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
047700110420     C*------------------------------------------------------------------------*
047800110420     C     WRITIVGD      BEGSR
047900110420     C*
048000110420     C                   clear                   tivgd000
048100110420     C                   eval      vgdDTA = wNomeFile
048200110420     C                   eval      vgdTIP = I_TIPVAS
048300110420     C                   eval      vgdKSU = I_KSUVAS
048400110420     C                   eval      vgdTSC = 'WW'
048500110420     C                   eval      vgdDAT = %dec(%date() : *ISO)
048600110420     C                   eval      vgdPGM = 'TIS7TEPR'
048700110420     C*
048800110420     C                   clear                   DVGDFLO
048900110420     C                   movel     'P'           §VGDFELA
049000110420     C                   movel     DVGDFLO       vgdflo
049100110420     C*
049200110420     C                   write     tivgd000
049300110420     C*
049400110420     C                   ENDSR
049500110420     C*------------------------------------------------------------------------*
049600110420
049700110420
049800110420
049900110420     C*------------------------------------------------------------------------*
050000110420     C* FORZADWN - Routine di forzatura elaborazione immediata download
050100110420     C*------------------------------------------------------------------------*
050200110420     C     FORZADWN      BEGSR
050300110420     C*
050400110420     C                   clear                   TIS781DS
050500110420     C                   eval      §781tip = vgdTIP
050600110420     C                   eval      §781ksu = vgdKSU
050700110420     C                   eval      §781tsc = vgdTSC
050800110420     C                   eval      §781dat = vgdDAT
050900110420     C                   eval      §781pgm = vgdPGM
051000110420     C*
051100110420     C                   clear                   TIS781DFLO
051200110420     C                   movel     'P'           §781floela
051300110420     C                   movel     TIS781DFLO    §781flo
051400110420     C*
051500110420     C* Finita l'elaborazione chiamo pgm
051600110420     C                   call      'TIS781C1'
051700110420     C                   parm      *blanks       esito             1
051800110420     C                   parm                    TIS781DS
051900110420     C                   parm      *blanks       vgdPRG           10
052000110420     C*
052100110420     C                   ENDSR
052200110420     C*------------------------------------------------------------------------*
052300110420
052400110420
052500110419
052600110419
052700110419
052800991027      /TITLE Operazioni iniziali.
052900991027     C     *inzsr        BEGSR
053000991027     C*
053100991027     C     *ENTRY        PLIST
053200110419     C                   PARM                    KPJBA
053300110419     C*
053400991027     C                   ENDSR
