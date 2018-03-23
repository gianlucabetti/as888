000100061128     H NOMAIN BNDDIR('QC2LE')
000200061128
000300061130      //********************************************************************************************
000400061130      //
000500061130      // Definizione costanti.
000600061130      //
000700061130      //********************************************************************************************
000800061201     D Dominio...
000900120314     D                 C                   '@brt.it'
001000061201     D Add...
001100061201     D                 C                   'A'
001200061130     D Replace...
001300061130     D                 C                   'R'
001400061130     D ErroreParametroUtente...
001500061128     D                 C                   1
001600061128     D ErroreParametroOggetto...
001700061128     D                 C                   2
001800061128     D ErroreParametroMessaggio...
001900061128     D                 C                   3
002000061201     D ErroreInvioEmail...
002100061201     D                 C                   4
002200061201     D ErroreSendBreakMessage...
002300061201     D                 C                   5
002400061201     D ErroreLunghezzaIndirizzo...
002500061201     D                 C                   6
002600061128
002700061130      //********************************************************************************************
002800061130      //
002900061130      // Definizione procedure esterne.
003000061130      //
003100061130      //********************************************************************************************
003200080320     D/COPY GAITRASRC/SRCPROTOPR,SYSTEM
003300080320     D/COPY GAITRASRC/SRCPROTOPR,QSYRUSRI
003400061130     D sendMessage...
003500061130     D                 PR                  EXTPGM('QEZSNDMG')
003600061130     D  messageType                  10A   CONST
003700061130     D  deliveryMode                 10A   CONST
003800061201     D  messageText                 494A   CONST
003900061130     D  lengthOfMessageText...
004000061130     D                               10I 0 CONST
004100061130     D  listOfUserProfile...
004200061130     D                               10A   DIM(299)
004300061130     D  numberOfUserProfile...
004400061130     D                               10I 0 CONST
004500061130     D  messageSentIndicator...
004600061130     D                               10I 0
004700061130     D  functionRequested...
004800061130     D                               10I 0
004900061130     D  errorCode...
005000061130     D                                     LIKE(qusec)
005100061130     D  showSendAMessageDisplay...
005200061130     D                                1A   CONST
005300061130     D  qualifiedMessageQueueName...
005400061130     D                               20A   CONST
005500061130     D  nameTypeIndicator...
005600061130     D                                4A   CONST
005700061201
005800061130      //********************************************************************************************
005900061130      //
006000061130      // Definizione variabili globali.
006100061130      //
006200061130      //********************************************************************************************
006300061201     D exceptionMessageID...
006400061201     D                 S              7A   IMPORT('_EXCP_MSGID')
006500061201     D adr             S            253A   DIM(300) VARYING BASED(adrPtr)
006600061130     D adrPtr          S               *
006700061130     D adrIdx          S             10I 0
006800061130     D adrElem         S             10I 0
006900061201     D listOfUserProfile...
007000061201     D                 S             10A   DIM(299)
007100061201     D numberOfUserProfile...
007200061201     D                 S             10I 0
007300061201     D messageSentIndicator...
007400061201     D                 S             10I 0
007500061201     D functionRequested...
007600061201     D                 S             10I 0
007700061130
007800061130      //********************************************************************************************
007900061130      //
008000061130      // Definizione strutture dati.
008100061130      //
008200061130      //********************************************************************************************
008300061130     D/COPY QSYSINC/QRPGLESRC,QUSEC
008400080320     D/COPY QSYSINC/QRPGLESRC,QSYRUSRI
008500061130     D psds           SDS
008600061130     D  jobName              244    253A
008700061130     D  jobUser              254    263A
008800061130     D  jobNumber            264    269A
008900061130
009000061130     D*--------------------------------------------------
009100061201     D* Procedure name: email_setAddressFromUser
009200061201     D* Purpose:        Imposta indirizzo associato all'utente
009300061130     D* Returns:        Esito.
009400061130     D* Parameter:      utente => Utente
009500061130     D* Parameter:      rpyOpCode => Codice operativo.
009600061130     D* Parameter:      rpyStatus => Codice stato.
009700061130     D* Parameter:      rpyEsito => Esito.
009800061130     D*--------------------------------------------------
009900061201     Demail_setAddressFromUser...
010000061201     D                 PR            10I 0
010100061130     D  rqsUtente                    10A   CONST
010200061130     D  rqsAddReplace                 1A   CONST
010300061130     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
010400061130     D  rpyStatus                    10I 0 OPTIONS(*NOPASS:*OMIT)
010500061130     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
010600061201
010700061201     D*--------------------------------------------------
010800061201     D* Procedure name: email_addAddress
010900061201     D* Purpose:        Aggiunge un indirizzo e-mail.
011000061201     D* Returns:        Esito.
011100061201     D* Parameter:      rqsIndirizzo
011200061201     D*--------------------------------------------------
011300061201     D email_addAddress...
011400061201     D                 PR            10I 0
011500061201     D  rqsIndirizzo                   *   VALUE
011600061201     D                                     OPTIONS(*STRING:*TRIM)
011700061201     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
011800061201     D  rpyStatus                    10I 0 OPTIONS(*NOPASS:*OMIT)
011900061201     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
012000061128
012100061128     D*--------------------------------------------------
012200061201     D* Procedure name: email_send
012300061128     D* Purpose:        Invia e-mail ad un utente.
012400061128     D* Returns:        Esito.
012500061128     D* Parameter:      rqsOggetto => Oggetto
012600061128     D* Parameter:      rqsMessaggio => Messaggio.
012700061128     D* Parameter:      rpyOpCode => Codice operativo.
012800061128     D* Parameter:      rpyStatus => Codice stato.
012900061128     D* Parameter:      rpyEsito => Esito.
013000061128     D*--------------------------------------------------
013100061201     D email_send      PR            10I 0
013200061130     D  rqsOggetto                     *   VALUE OPTIONS(*STRING:*TRIM)
013300061130     D  rqsMessaggio                   *   VALUE OPTIONS(*STRING:*TRIM)
013400061128     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
013500061128     D  rpyStatus                    10I 0 OPTIONS(*NOPASS:*OMIT)
013600061128     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
013700061201
013800061201     D*--------------------------------------------------
013900061201     D* Procedure name: email_sndBrkMsg
014000061201     D* Purpose:        Invia un messaggio interruzione agli utenti.
014100061201     D* Returns:        Esito.
014200061201     D* Parameter:      messaggio => Testo del messaggio
014300061201     D*--------------------------------------------------
014400061201     Demail_sndBrkMsg  PR            10I 0
014500061201     D  rqsMessaggio                   *   VALUE OPTIONS(*STRING:*TRIM)
014600061201     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
014700061201     D  rpyStatus                    10I 0 OPTIONS(*NOPASS:*OMIT)
014800061201     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
014900061130
015000061130
015100061130     P*--------------------------------------------------
015200061201     P* Procedure name: email_setAddressFromUser
015300061201     P* Purpose:        Imposta indirizzo associato all'utente
015400061130     P* Returns:        Esito.
015500061130     P* Parameter:      utente => Utente
015600061130     P*--------------------------------------------------
015700061201     Pemail_setAddressFromUser...
015800061201     P                 B                   EXPORT
015900061201     Demail_setAddressFromUser...
016000061201     D                 PI            10I 0
016100061130     D  rqsUtente                    10A   CONST
016200061130     D  rqsAddReplace                 1A   CONST
016300061130     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
016400061130     D  rpyStatus                    10I 0 OPTIONS(*NOPASS:*OMIT)
016500061130     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
016600061130
016700061130     D retField        S             10I 0
016800061130     D uteUte          S             10A
016900061130     D uteEma          S            256A   VARYING
017000061130     D stmSql          S            256A   VARYING
017100061201     D p1              S              3U 0
017200061201     D p2              S              3U 0
017300061130
017400061130      /FREE
017500061130
017600110525       IF %PARMS > 2 AND %ADDR(rpyOpCode) <> *NULL;
017700061130         rpyOpCode = 'DONE';
017800061130       ENDIF;
017900110525       IF %PARMS > 3 AND %ADDR(rpyStatus) <> *NULL;
018000061130         CLEAR rpyStatus;
018100061130       ENDIF;
018200110525       IF %PARMS > 4 AND %ADDR(rpyEsito) <> *NULL;
018300061130         CLEAR rpyEsito;
018400061130       ENDIF;
018500061130
018600061130       IF rqsUtente = *BLANK;
018700110525         IF %PARMS > 2 AND %ADDR(rpyOpCode) <> *NULL;
018800061130           rpyOpCode = 'ERRORE';
018900061130         ENDIF;
019000110525         IF %PARMS > 3 AND %ADDR(rpyStatus) <> *NULL;
019100061130           rpyStatus = ErroreParametroUtente;
019200061130         ENDIF;
019300110525         IF %PARMS > 4 AND %ADDR(rpyEsito) <> *NULL;
019400061130           rpyEsito = -1;
019500061130         ENDIF;
019600061130         RETURN -1;
019700061130       ENDIF;
019800061130
019900061130       stmSQL = 'SELECT UTEUTE, RTRIM(UTEEMA) FROM AZUTE00F +
020000061130       WHERE UTEDSC >= ' + %CHAR(%DEC(%DATE():*ISO));
020100061130
020200061130       // Posso ricevere un nome utente (p.e. 'EDPDC')
020300061130       // oppure un nome utente generico (p.e. 'EDP*')
020400061130       // oppure tutti gli utenti ('*ALL').
020500061130       SELECT;
020600061130         WHEN rqsUtente = '*ALL';
020700061130         WHEN %SCAN('*':rqsUtente) = 0;
020800061201           stmSQL += ' AND UTEUTE = ''' + %TRIMR(rqsUtente) + '''';
020900061130         OTHER;
021000061201           stmSQL += ' AND UTEUTE LIKE ''' + %TRIMR(%XLATE('*':'%':rqsUtente))
021100061201           + '''';
021200061130       ENDSL;
021300061130
021400061130       stmSQL += ' FOR READ ONLY';
021500061130
021600061130      /END-FREE
021700061130     C/EXEC SQL
021800061130     C+ PREPARE UTENTI FROM :stmSQL
021900061130     C/END-EXEC
022000061130      /FREE
022100061130
022200061130       IF sqlCode < 0;
022300110525         IF %PARMS > 2 AND %ADDR(rpyOpCode) <> *NULL;
022400061130           rpyOpCode = 'SQLCODE';
022500061130         ENDIF;
022600110525         IF %PARMS > 3 AND %ADDR(rpyStatus) <> *NULL;
022700061130           rpyStatus = sqlCode;
022800061130         ENDIF;
022900110525         IF %PARMS > 4 AND %ADDR(rpyEsito) <> *NULL;
023000061130           rpyEsito = -1;
023100061130         ENDIF;
023200061130         RETURN -1;
023300061130       ENDIF;
023400061130
023500061130      /END-FREE
023600061130     C/EXEC SQL
023700061130     C+ DECLARE UTENTI CURSOR FOR UTENTI
023800061130     C/END-EXEC
023900061130      /FREE
024000061130
024100061130       IF sqlCode < 0;
024200110525         IF %PARMS > 2 AND %ADDR(rpyOpCode) <> *NULL;
024300061130           rpyOpCode = 'SQLCODE';
024400061130         ENDIF;
024500110525         IF %PARMS > 3 AND %ADDR(rpyStatus) <> *NULL;
024600061130           rpyStatus = sqlCode;
024700061130         ENDIF;
024800110525         IF %PARMS > 4 AND %ADDR(rpyEsito) <> *NULL;
024900061130           rpyEsito = -1;
025000061130         ENDIF;
025100061130         RETURN -1;
025200061130       ENDIF;
025300061130
025400061130      /END-FREE
025500061130     C/EXEC SQL
025600061130     C+ OPEN UTENTI
025700061130     C/END-EXEC
025800061130      /FREE
025900061130
026000061130       IF sqlCode < 0;
026100110525         IF %PARMS > 2 AND %ADDR(rpyOpCode) <> *NULL;
026200061130           rpyOpCode = 'SQLCODE';
026300061130         ENDIF;
026400110525         IF %PARMS > 3 AND %ADDR(rpyStatus) <> *NULL;
026500061130           rpyStatus = sqlCode;
026600061130         ENDIF;
026700110525         IF %PARMS > 4 AND %ADDR(rpyEsito) <> *NULL;
026800061130           rpyEsito = -1;
026900061130         ENDIF;
027000061130         RETURN -1;
027100061130       ENDIF;
027200061130
027300061201       IF rqsAddReplace = Replace;
027400061130         CLEAR adrElem;
027500061130         CLEAR numberOfUserProfile;
027600061130         DEALLOC(EN) adrPtr;
027700061130         adrPtr = %ALLOC(1);
027800061130       ENDIF;
027900061130
028000061130
028100061130       DOU sqlCode = 100;
028200061130      /END-FREE
028300061130     C/EXEC SQL
028400061130     C+ FETCH NEXT FROM UTENTI INTO :uteUte, :uteEma
028500061130     C/END-EXEC
028600061130      /FREE
028700061130         SELECT;
028800061130           WHEN sqlCode = 100;
028900061130             LEAVE;
029000061130           WHEN sqlCode < 0;
029100110525             IF %PARMS > 2 AND %ADDR(rpyOpCode) <> *NULL;
029200061130               rpyOpCode = 'SQLCODE';
029300061130             ENDIF;
029400110525             IF %PARMS > 3 AND %ADDR(rpyStatus) <> *NULL;
029500061130               rpyStatus = sqlCode;
029600061130             ENDIF;
029700110525             IF %PARMS > 4 AND %ADDR(rpyEsito) <> *NULL;
029800061130               rpyEsito = -1;
029900061130             ENDIF;
030000061130             retField = -1;
030100061130             LEAVE;
030200061130           OTHER;
030300080320             CLEAR qusec;
030400080320             qusbprv = %SIZE(qusec);
030500080320             RetrieveUserInformation( QSYI0100
030600080320                                    : %SIZE(QSYI0100)
030700080320                                    : 'USRI0100'
030800080320                                    : uteUte
030900080320                                    : QUSEC
031000080320                                    );
031100080320             IF QUSEI = *BLANK;
031200080320               // Aggiungo l'indirizzo alla lista degli indirizzi.
031300080320               IF %LEN(uteEma) > 0 AND adrElem < %ELEM(adr);
031400080320                 p1 = 0;
031500080320                 p2 = 0;
031600080320                 DOU p2 = 0 OR p2 >= %LEN(uteEma);
031700080320                   p1 = p2 + 1;
031800080320                   p2 = %SCAN(';' : uteEma : p1);
031900080320                   IF p2 = 0;
032000080320                     p2 = %LEN(uteEma) + 1;
032100080320                   ENDIF;
032101170612                   IF %SCAN('@' : uteEma) = *ZERO;
032200170612                     email_addAddress(%SUBST(uteEma : p1 : p2 - p1) + DOMINIO);
032201170612                   ELSE;
032202170612                     email_addAddress(%SUBST(uteEma : p1 : p2 - p1));
032203170612                   ENDIF;
032300080320                 ENDDO;
032400080320               ENDIF;
032500080320               // Aggiungo il profilo alla lista del send break message.
032600080320               IF numberOfUserProfile < %ELEM(listOfUserProfile) AND
032700080320               %LOOKUP(uteUte : listOfUserProfile : 1 : numberOfUserProfile)
032800080320               = 0;
032900080320                 numberOfUserProfile += 1;
033000080320                 listOfUserProfile(numberOfUserProfile) = uteUte;
033100080320               ENDIF;
033200080320             ENDIF;
033300061130         ENDSL;
033400061130       ENDDO;
033500061130
033600061130      /END-FREE
033700061130     C/EXEC SQL
033800061130     C+ CLOSE UTENTI
033900061130     C/END-EXEC
034000061130      /FREE
034100061130
034200061130       RETURN retField;
034300061130
034400061130      /END-FREE
034500061201     Pemail_setAddressFromUser...
034600061201     P                 E
034700061201
034800061201
034900061201     P*--------------------------------------------------
035000061201     P* Procedure name: email_send
035100061201     P* Purpose:        Invia e-mail ad un utente.
035200061201     P* Returns:        Esito.
035300061201     P* Parameter:      rqsOggetto => Oggetto
035400061201     P* Parameter:      rqsMessaggio => Messaggio.
035500061201     P* Parameter:      rpyOpCode => Codice operativo.
035600061201     P* Parameter:      rpyStatus => Codice stato.
035700061201     P* Parameter:      rpyEsito => Esito.
035800061201     P*--------------------------------------------------
035900061201     P email_send      B                   EXPORT
036000061201     D email_send      PI            10I 0
036100061201     D  rqsOggetto                     *   VALUE OPTIONS(*STRING:*TRIM)
036200061201     D  rqsMessaggio                   *   VALUE OPTIONS(*STRING:*TRIM)
036300061201     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
036400061201     D  rpyStatus                    10I 0 OPTIONS(*NOPASS:*OMIT)
036500061201     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
036600061201
036700061201     D retField        S             10I 0
036800061201     D command         S          32767A   VARYING
036900061201     D oggetto         S             44A   VARYING
037000061201     D messaggio       S           5000A   VARYING
037100061201
037200061201      /FREE
037300061201
037400061201       IF %PARMS > 2 AND %ADDR(rpyOpCode) <> *NULL;
037500061201         CLEAR rpyOpCode;
037600061201       ENDIF;
037700061201       IF %PARMS > 3 AND %ADDR(rpyStatus) <> *NULL;
037800061201         CLEAR rpyStatus;
037900061201       ENDIF;
038000061201       IF %PARMS > 4 AND %ADDR(rpyEsito) <> *NULL;
038100061201         CLEAR rpyEsito;
038200061201       ENDIF;
038300061201
038400061201       // Controlli sull'oggetto.
038500061201       IF %LEN(%STR(rqsOggetto)) = 0 OR %STR(rqsOggetto) = *BLANK;
038600061201         IF %PARMS > 2 AND %ADDR(rpyOpCode) <> *NULL;
038700061201           rpyOpCode = 'ERRORE';
038800061201         ENDIF;
038900061201         IF %PARMS > 3 AND %ADDR(rpyStatus) <> *NULL;
039000061201           rpyStatus = ErroreParametroOggetto;
039100061201         ENDIF;
039200061201         IF %PARMS > 4 AND %ADDR(rpyEsito) <> *NULL;
039300061201           rpyEsito = -1;
039400061201         ENDIF;
039500061201         RETURN -1;
039600061201       ENDIF;
039700061201
039800061201       oggetto = %STR(rqsOggetto);
039900061201
040000061201       // Controlli sul messaggio.
040100061201       IF %LEN(%STR(rqsMessaggio)) = 0 OR %STR(rqsMessaggio) = *BLANK;
040200061201         IF %PARMS > 2 AND %ADDR(rpyOpCode) <> *NULL;
040300061201           rpyOpCode = 'ERRORE';
040400061201         ENDIF;
040500061201         IF %PARMS > 3 AND %ADDR(rpyStatus) <> *NULL;
040600061201           rpyStatus = ErroreParametroMessaggio;
040700061201         ENDIF;
040800061201         IF %PARMS > 4 AND %ADDR(rpyEsito) <> *NULL;
040900061201           rpyEsito = -1;
041000061201         ENDIF;
041100061201         RETURN -1;
041200061201       ENDIF;
041300061201
041400061201       messaggio = %STR(rqsMessaggio) +
041500061201       ':/PMessaggio automatico inviato dal lavoro ' +
041600061201       jobNumber + '/' + %TRIMR(jobUser) + '/' + %TRIMR(jobName) + '.';
041700061201
041800061201       // Invio e-mail.
041900061201       IF adrElem > 0;
042000061201         command = 'SNDDST TYPE(*LMSG) TOINTNET(';
042100061201         FOR adrIdx = 1 TO adrElem;
042200061201           command += ' (''' + adr(adrIdx) + ''')';
042300061201         ENDFOR;
042400061201         command += ') DSTD(''' + oggetto + ''')';
042500061201         command += ' LONGMSG(''' + messaggio + ''')';
042600061201         IF executeCommand(command) <> 0;
042700061201           IF %PARMS > 2 AND %ADDR(rpyOpCode) <> *NULL;
042800061201             rpyOpCode = exceptionMessageID;
042900061201           ENDIF;
043000061201           IF %PARMS > 3 AND %ADDR(rpyStatus) <> *NULL;
043100061201             rpyStatus = ErroreInvioEmail;
043200061201           ENDIF;
043300061201           IF %PARMS > 4 AND %ADDR(rpyEsito) <> *NULL;
043400061201             rpyEsito = -1;
043500061201           ENDIF;
043600061201           RETURN -1;
043700061201         ENDIF;
043800080320       ELSE;
043900080320         RETURN -1;
044000061201       ENDIF;
044100061201
044200061201       RETURN retField;
044300061201
044400061201      /END-FREE
044500061201     P email_send      E
044600061201
044700061201
044800061201     P*--------------------------------------------------
044900061201     P* Procedure name: email_sndBrkMsg
045000061201     P* Purpose:        Invia un messaggio interruzione agli utenti.
045100061201     P* Returns:        Esito.
045200061201     P* Parameter:      messaggio => Testo del messaggio
045300061201     P*--------------------------------------------------
045400061201     Pemail_sndBrkMsg  B                   EXPORT
045500061201     Demail_sndBrkMsg  PI            10I 0
045600061201     D  rqsMessaggio                   *   VALUE OPTIONS(*STRING:*TRIM)
045700061201     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
045800061201     D  rpyStatus                    10I 0 OPTIONS(*NOPASS:*OMIT)
045900061201     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
046000061201
046100061201     D retField        S             10I 0
046200061201     D messaggio       S            494A   VARYING
046300061201
046400061201      /FREE
046500061201
046600061201       IF %PARMS > 1 AND %ADDR(rpyOpCode) <> *NULL;
046700061201         CLEAR rpyOpCode;
046800061201       ENDIF;
046900061201       IF %PARMS > 2 AND %ADDR(rpyStatus) <> *NULL;
047000061201         CLEAR rpyStatus;
047100061201       ENDIF;
047200061201       IF %PARMS > 3 AND %ADDR(rpyEsito) <> *NULL;
047300061201         CLEAR rpyEsito;
047400061201       ENDIF;
047500061201
047600061201       // Controllo messaggio.
047700061201       IF %LEN(%STR(rqsMessaggio)) = 0 OR %STR(rqsMessaggio) = *BLANK;
047800061201         IF %PARMS > 1 AND %ADDR(rpyOpCode) <> *NULL;
047900061201           rpyOpCode = 'ERRORE';
048000061201         ENDIF;
048100061201         IF %PARMS > 2 AND %ADDR(rpyStatus) <> *NULL;
048200061201           rpyStatus = ErroreParametroMessaggio;
048300061201         ENDIF;
048400061201         IF %PARMS > 3 AND %ADDR(rpyEsito) <> *NULL;
048500061201           rpyEsito = -1;
048600061201         ENDIF;
048700061201         RETURN -1;
048800061201       ENDIF;
048900061201
049000061201       messaggio = %STR(rqsMessaggio);
049100061201
049200061201       IF numberOfUserProfile > 0;
049300061201           CLEAR qusec;
049400061201           qusbprv = %SIZE(qusec);
049500061201           sendMessage('*INFO' : '*BREAK' : messaggio : %LEN(messaggio)
049600061201           : ListOfUserProfile : NumberOfUserProfile : MessageSentIndicator
049700061201           : FunctionRequested : qusec : 'N' : *BLANK : '*USR');
049800061201         IF qusei <> ' ';
049900061201           IF %PARMS > 1 AND %ADDR(rpyOpCode) <> *NULL;
050000061201             rpyOpCode = qusei;
050100061201           ENDIF;
050200061201           IF %PARMS > 2 AND %ADDR(rpyStatus) <> *NULL;
050300061201             rpyStatus = ErroreSendBreakMessage;
050400061201           ENDIF;
050500061201           IF %PARMS > 3 AND %ADDR(rpyEsito) <> *NULL;
050600061201             rpyEsito = -1;
050700061201           ENDIF;
050800061201           RETURN -1;
050900061201         ENDIF;
051000080320       ELSE;
051100080320         RETURN -1;
051200061201       ENDIF;
051300061201
051400061201       RETURN retField;
051500061201
051600061201      /END-FREE
051700061201     Pemail_sndBrkMsg  E
051800061201
051900061201
052000061201     P*--------------------------------------------------
052100061201     P* Procedure name: email_addAddress
052200061201     P* Purpose:        Aggiunge un indirizzo e-mail.
052300061201     P* Returns:        Esito.
052400061201     P* Parameter:      rqsIndirizzo
052500061201     P*--------------------------------------------------
052600061201     P email_addAddress...
052700061201     P                 B                   EXPORT
052800061201     D email_addAddress...
052900061201     D                 PI            10I 0
053000061201     D  rqsIndirizzo                   *   VALUE
053100061201     D                                     OPTIONS(*STRING:*TRIM)
053200061201     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
053300061201     D  rpyStatus                    10I 0 OPTIONS(*NOPASS:*OMIT)
053400061201     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
053500061201
053600061201     D retField        S             10I 0
053700061201     D li              S             10I 0
053800061201     D adrArg          S                   LIKE(adr)
053900061201
054000061201      /FREE
054100061201
054200061201       IF %PARMS > 1 AND %ADDR(rpyOpCode) <> *NULL;
054300061201         rpyOpCode = 'DONE';
054400061201       ENDIF;
054500061201       IF %PARMS > 2 AND %ADDR(rpyStatus) <> *NULL;
054600061201         CLEAR rpyStatus;
054700061201       ENDIF;
054800061201       IF %PARMS > 3 AND %ADDR(rpyEsito) <> *NULL;
054900061201         CLEAR rpyEsito;
055000061201       ENDIF;
055100061201
055200061201       IF adrElem = %ELEM(adr);
055300061201         RETURN 0;
055400061201       ENDIF;
055500061201
055600061201       li = %LEN(%STR(rqsIndirizzo)); // Lunghezza indirizzo.
055700061201
055800061201       IF li = 0 OR li > %SIZE(adr);
055900061201         IF %PARMS > 1 AND %ADDR(rpyOpCode) <> *NULL;
056000061201           rpyOpCode = 'ERRORE';
056100061201         ENDIF;
056200061201         IF %PARMS > 2 AND %ADDR(rpyStatus) <> *NULL;
056300061201           rpyStatus = ErroreLunghezzaIndirizzo;
056400061201         ENDIF;
056500061201         IF %PARMS > 3 AND %ADDR(rpyEsito) <> *NULL;
056600061201           rpyEsito = -1;
056700061201         ENDIF;
056800061201         RETURN -1;
056900061201       ENDIF;
057000061201
057100061201       adrArg = %STR(rqsIndirizzo);
057200061201       IF %LOOKUP(adrArg : adr : 1 : adrElem) = 0;
057300061201         adrElem += 1;
057400061201         adrPtr = %REALLOC(adrPtr : %SIZE(adr) * adrElem);
057500061201         adr(adrElem) = adrArg;
057600061201       ENDIF;
057700061201
057800061201       RETURN retField;
057900061201
058000061201      /END-FREE
058100061201     P email_addAddress...
058200061201     P                 E
058300061201
