000100060515      //********************************************************************************************
000200060515      //
000300060608      // Questa procedura restituisce il testo di 1o e 2o livello di un messaggio in lingua.
000400060609      //
000500060609      // Comandi di compilazione:
000600060609      //
000700060609      // CRTRPGMOD  MODULE([libreria]/TISRTVMSG) SRCFILE(GAITRASRC/SRCTIS7) SRCMBR(TISRTVMSG)
000800060609      // CRTSRVPGM  SRVPGM([libreria]/TISRTVMSG) EXPORT(*ALL) {OPTION(*DUPPROC)}
000900060609      //            TEXT('Reperimento messaggio in lingua.')
001000060609      // CRTPGM     PGM([libreria]/TISRTVMSG) ACTGRP(*CALLER)
001100060609      //            La creazione del programma serve per le chiamate da AS777 via SQL.
001200060609      // DLTMOD     MODULE([libreria]/TISRTVMSG)
001300060515      //
001400060515      //********************************************************************************************
001500060515
001600060515      //********************************************************************************************
001700060515      //
001800060515      // Definizione costanti.
001900060515      //
002000060515      //********************************************************************************************
002100060515     D Italiano        C                   'it'
002200060515     D Inglese         C                   'en'
002300060515     D Francese        C                   'fr'
002400060515     D Tedesco         C                   'de'
002500060516     D MsgFileItaliano...
002600060516     D                 C                   'TIS_IT'
002700060516     D MsgFileInglese...
002800060516     D                 C                   'TIS_EN'
002900060516     D MsgFileFrancese...
003000060516     D                 C                   'TIS_FR'
003100060516     D MsgFileTedesco...
003200060516     D                 C                   'TIS_DE'
003300060516     D MsgFileLibItaliano...
003400060516     D                 C                   '*LIBL'
003500060516     D MsgFileLibInglese...
003600060516     D                 C                   '*LIBL'
003700060516     D MsgFileLibFrancese...
003800060516     D                 C                   '*LIBL'
003900060516     D MsgFileLibTedesco...
004000060516     D                 C                   '*LIBL'
004100060516     D FileMessaggiNonTrovato...
004200060516     D                 C                   'CPF2407'
004300060516     D IdMessaggioNonTrovato...
004400060516     D                 C                   'CPF2419'
004500060515
004600060515      //********************************************************************************************
004700060515      //
004800060515      // Definizione procedure usate.
004900060515      //
005000060515      //********************************************************************************************
005100060612     D retrieveMessage...
005200060612     D                 PR                                                       Reperisce la
005300060612     D                                     EXTPGM('QMHRTVM')                    descrizione di un
005400060612     D                                                                          messaggio in formato
005500060612     D                                                                          RTVM0100
005600060612     D  messageInformation...
005700060612     D                            65535A
005800060612     D                                     OPTIONS(*VARSIZE)
005900060612     D  lengthOfMessageInformation...
006000060612     D                               10I 0
006100060612     D  formatName...
006200060612     D                                8A
006300060612     D                                     CONST
006400060612     D  messageIdentifier...
006500060612     D                                7A
006600060612     D  qualifiedMessageFileName...
006700060612     D                               20A
006800060612     D                                     CONST
006900060612     D  replacementData...
007000060612     D                              512A
007100060612     D                                     OPTIONS(*VARSIZE)
007200060612     D  lengthOfReplacementData...
007300060612     D                               10I 0
007400060612     D  replaceSubstitutionValues...
007500060612     D                               10A
007600060612     D                                     CONST
007700060612     D  returnFormatControlCharacters...
007800060612     D                               10A
007900060612     D                                     CONST
008000060612     D  errorCode...
008100060612     D                                     LIKE(qusec)
008200060612     D                                     OPTIONS(*VARSIZE)
008300060612     D  retrieveOption...
008400060612     D                               10A
008500060612     D                                     OPTIONS(*NOPASS)
008600060612     D  ccsidToConvertTo...
008700060612     D                               10I 0
008800060612     D                                     OPTIONS(*NOPASS)
008900060612     D  ccsidOfRplacementData...
009000060612     D                               10I 0
009100060612     D                                     OPTIONS(*NOPASS)
009200060612     D*--------------------------------------------------
009300060608     D* Procedure name: rtvMsgLang
009400060608     D* Purpose:        Reperisce la descrizione in lingus di un messaggio.
009500060608     D* Returns:        Testo messaggio secondo livello
009600060608     D* Parameter:      linguaISO2 => Lingua ISO2
009700060608     D* Parameter:      msgId => Identificativo messaggio
009800060608     D* Parameter:      msgDta => Valori campi dati messaggi
009900060608     D* Parameter:      msg => Testo messaggio primo livello
010000060608     D* Parameter:      secLvl => Testo messaggio secondo livello
010100060608     D* Parameter:      rtnCode => Codice di ritorno
010200060608     D* Parameter:      esito => Esito
010300060608     D*--------------------------------------------------
010400060608     D rtvMsgLang      PR          3512A
010500060614     D  msgId                         7A
010600060614     D  piLinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
010700060614     D  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
010800060614     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
010900060614     D                                     VARYING
011000060614     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
011100060614     D                                     VARYING
011200060614     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
011300060614     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
011400060614
011500060608      //********************************************************************************************
011600060608      //
011700060608      // Definizione variabili globali.
011800060608      //
011900060608      //********************************************************************************************
012000060612     D/COPY QSYSINC/QRPGLESRC,QMHRTVM
012100060612     D/COPY QSYSINC/QRPGLESRC,QUSEC
012200060612     D msgId...
012300060608     D                 S              7A
012400060609     D linguaISO2...
012500060609     D                 S              2A
012600060609     D msgDta...
012700060614     D                 S            512A
012800060608     D msg...
012900060614     D                 S            644A
013000060608     D secLvl...
013100060614     D                 S           3512A
013200060608     D rtnCode...
013300060608     D                 S             10A
013400060608     D esito...
013500060608     D                 S             15P 0
013600060614     D piMsg...
013700060614     D                 S            644A   VARYING
013800060614     D piSecLvl...
013900060614     D                 S           3512A   VARYING
014000060608
014100060608      //********************************************************************************************
014200060608      //
014300060608      // Definizione parametri.
014400060608      //
014500060608      //********************************************************************************************
014600060608     C     *ENTRY        PLIST
014700060608     C                   PARM                    msgId                          Id messaggio
014800060609     C                   PARM                    linguaISO2                     Id lingua ISO 2
014900060614     C                   PARM                    msgDta                         Valori campi dati me
015000060614     C                   PARM      piMsg         msg                            Testo 1o livello
015100060614     C                   PARM      piSecLvl      secLvl                         Testo 2o livello
015200060608     C                   PARM                    rtnCode                        Codice di ritorno
015300060608     C                   PARM                    esito                          Esito
015400060609
015500060608      /FREE
015600060614       rtvMsgLang(msgId:linguaISO2:msgDta:piMsg:piSecLvl:rtnCode:esito);
015700060608       RETURN;
015800060608      /END-FREE
015900060608
016000060608     P*--------------------------------------------------
016100060608     P* Procedure name: rtvMsgLang
016200060608     P* Purpose:        Reperisce la descrizione in lingus di un messaggio.
016300060608     P* Returns:        Testo messaggio secondo livello
016400060608     P* Parameter:      linguaISO2 => Lingua ISO2
016500060608     P* Parameter:      msgId => Identificativo messaggio
016600060608     P* Parameter:      msgDta => Valori campi dati messaggi
016700060608     P* Parameter:      msg => Testo messaggio primo livello
016800060608     P* Parameter:      secLvl => Testo messaggio secondo livello
016900060608     P* Parameter:      rtnCode => Codice di ritorno
017000060608     P* Parameter:      esito => Esito
017100060608     P*--------------------------------------------------
017200060608     P rtvMsgLang      B                   EXPORT
017300060608     D rtvMsgLang      PI          3512A
017400060608     D  msgId                         7A
017500060609     D  piLinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
017600060613     D  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
017700060612     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
017800060609     D                                     VARYING
017900060612     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
018000060609     D                                     VARYING
018100060609     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
018200060609     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
018300060608
018400060614     D linguaISO2...
018500060614     D                 S              2A
018600060614     D msgDta...
018700060614     D                 S            512A
018800060614     D msg...
018900060614     D                 S            644A
019000060614     D secLvl...
019100060614     D                 S           3512A
019200060614     D rtnCode...
019300060614     D                 S             10A
019400060614     D esito...
019500060614     D                 S             15P 0
019600060608     D msgFile...
019700060608     D                 S             10A
019800060608     D                                     INZ(MsgFileItaliano)
019900060608     D msgFileLib...
020000060608     D                 S             10A
020100060608     D                                     INZ(MsgFileLibItaliano)
020200060612     D messageInformation...
020300060612     D                 S           4180A                                        24+644+3512
020400060612     D lengthOfMessageInformation...
020500060612     D                 S             10I 0
020600060612     D                                     INZ(%SIZE(messageInformation))
020700060612     D lengthOfReplacementData...
020800060612     D                 S             10I 0
020900060612     D                                     INZ(%SIZE(msgDta))
021000060612     D returnFormatControlCharacters...
021100060612     D                 S             10A
021200060612     D                                     INZ('*NO')
021300060612     D retrieveOption...
021400060612     D                 S             10A
021500060612     D                                     INZ('*MSGID')
021600060612     D ccsidToConvertTo...
021700060612     D                 S             10I 0
021800060612     D ccsidOfRplacementData...
021900060612     D                 S             10I 0
022000060612
022100060608      /FREE
022200060609
022300060609       // Pulisco i parametri da restituire.
022400060614       CLEAR msg;
022500060614       CLEAR secLvl;
022600060612
022700060612       IF %PARMS > 3 AND %ADDR(piMsg) <> *NULL;
022800060608         CLEAR piMsg;
022900060608       ENDIF;
023000060612       IF %PARMS > 4 AND %ADDR(piSecLvl) <> *NULL;
023100060608         CLEAR piSecLvl;
023200060608       ENDIF;
023300060612       IF %PARMS > 5 AND %ADDR(piRtnCode) <> *NULL;
023400060608         CLEAR piRtnCode;
023500060608       ENDIF;
023600060612       IF %PARMS > 6 AND %ADDR(piEsito) <> *NULL;
023700060608         CLEAR piEsito;
023800060608       ENDIF;
023900060608
024000060609       // L'identificativo del messaggio è obbligatorio.
024100060608       IF msgId = *BLANK;
024200060608         IF %ADDR(piRtnCode) <> *NULL;
024300060608           piRtnCode = 'ERRORE';
024400060608         ENDIF;
024500060608         IF %ADDR(piEsito) <> *NULL;
024600060608           piEsito = -1;
024700060608         ENDIF;
024800060608         RETURN secLvl;
024900060608       ENDIF;
025000060608
025100060609       IF %ADDR(piMsgdTA) <> *NULL;
025200060609         msgDta = piMsgDta;
025300060609       ELSE;
025400060609         CLEAR msgDta;
025500060609       ENDIF;
025600060609
025700060609       // Se non ho ricevuto la lingua, imposto l'italiano.
025800060612       IF %PARMS < 2 OR %ADDR(piLinguaISO2) = *NULL OR piLinguaISO2 = *BLANK;
025900060609         linguaISO2 = Italiano;
026000060609       ELSE;
026100060609         linguaISO2 = piLinguaISO2;
026200060609       ENDIF;
026300060609
026400060609       // Reperisco la descrizione del messaggio.
026500060609       // Faccio massimo 2 tentativi, il 1o con la lingua richiesta,
026600060609       // il 2o con l'italiano.
026700060608       DOU rtnCode = 'DONE';
026800060608         SELECT;
026900060608           WHEN linguaISO2 = Italiano;
027000060608             msgFile = MsgFileItaliano;
027100060608             msgFileLib = MsgFileLibItaliano;
027200060608           WHEN linguaISO2 = Inglese;
027300060608             msgFile = MsgFileInglese;
027400060608             msgFileLib = MsgFileLibInglese;
027500060608           WHEN linguaISO2 = Francese;
027600060608             msgFile = MsgFileFrancese;
027700060608             msgFileLib = MsgFileLibFrancese;
027800060608           WHEN linguaISO2 = Tedesco;
027900060608             msgFile = MsgFileTedesco;
028000060608             msgFileLib = MsgFileLibTedesco;
028100060608           OTHER;
028200060608             linguaISO2 = Italiano;
028300060608             msgFile = MsgFileItaliano;
028400060608             msgFileLib = MsgFileLibItaliano;
028500060608         ENDSL;
028600060612         CLEAR qusec;
028700060612         qusbprv = %SIZE(qusec);
028800060612         retrieveMessage(messageInformation:lengthOfMessageInformation
028900060612         :'RTVM0100':msgId:msgFile+msgFileLib:msgDta:lengthOfReplacementData
029000060612         :'*YES':'*NO':qusec);
029100060614         IF qusei = *BLANK;
029200060614           qmhm010004 = messageInformation;
029300060614           msg = %SUBST(messageInformation:25:qmhlmavl02);
029400060614           secLvl = %SUBST(messageInformation:25+qmhlmavl02:qmhlhavl02);
029500060614           rtnCode = 'DONE';
029600060614           esito = 0;
029700060614           LEAVE;
029800060614         ELSE;
029900060614           rtnCode = qusei;
030000060614           esito = 1;
030100060614         ENDIF;
030200060614         IF linguaISO2 = Italiano;
030300060612           LEAVE;
030400060608         ENDIF;
030500060608         linguaISO2 = Italiano;
030600060608       ENDDO;
030700060608
030800060612       IF %PARMS > 1 AND %ADDR(piLinguaISO2) <> *NULL;
030900060609         piLinguaISO2 = linguaISO2;
031000060609       ENDIF;
031100060612       IF %PARMS > 3 AND %ADDR(piMsg) <> *NULL;
031200060608         piMsg = msg;
031300060608       ENDIF;
031400060612       IF %PARMS > 4 AND %ADDR(piSecLvl) <> *NULL;
031500060608         piSecLvl = secLvl;
031600060608       ENDIF;
031700060612       IF %PARMS > 5 AND %ADDR(piRtnCode) <> *NULL;
031800060608         piRtnCode = rtnCode;
031900060608       ENDIF;
032000060612       IF %PARMS > 6 AND %ADDR(piEsito) <> *NULL;
032100060608         piEsito = esito;
032200060608       ENDIF;
032300060608
032400060608       RETURN secLvl;
032500060608
032600060608      /END-FREE
032700060608     P rtvMsgLang      E
