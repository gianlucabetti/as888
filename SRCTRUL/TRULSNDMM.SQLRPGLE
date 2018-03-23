000100110225     ***************************************************************************
000200110225     **
000300110225     ** Questo programma di servizio compone ed invia una e-mail MIME ad uno o
000400110225     ** più profili utente.
000401140110     **
000402140110     ** CRTSRVPGM SRVPGM(GAITRAFIL/TRULSNDMM) MODULE(GAITRAFIL/TRULSNDMM)
000403140110     ** SRCFILE(GAITRASRC/SRCSRVPGM) TEXT('Invia e-mail MIME a profilo utente.')
000404140110     ** BNDSRVPGM((QTCP/QTMMSNDM))
000500110225     **
000600110225     ***************************************************************************
000700160526     H NOMAIN BNDDIR('QC2LE')
000800110228
000900110228     ***************************************************************************
001000110228     **
001100110228     ** Costanti.
001200110228     **
001300110228     ***************************************************************************
001400110302      /COPY GAITRASRC/SRCCONST,IFS
001500110304      /COPY GAITRASRC/SRCCONST,TRULSNDMM
001600110303     D CRLF...
001700110303     D                 C                   X'0D25'
001800110302     D SQLCODE_ROW_NOT_FOUND...
001900110302     D                 C                   100
002000110228
002100110225     ***************************************************************************
002200110225     **
002300110225     ** Strutture dati.
002400110225     **
002500110225     ***************************************************************************
002600110303      /COPY GAITRASRC/SRCPROTOPI,ERRNO
002700110302      /COPY QSYSINC/QRPGLESRC,QSYRUSRI
002800110228      /COPY QSYSINC/QRPGLESRC,QTMMSNDM
002900110228      /COPY QSYSINC/QRPGLESRC,QUSEC
003000110303     D psds           SDS                  QUALIFIED
003100110304     D  procName               1     10A                                        Program name
003200110304     D  procLib               81     90A                                        Library program
003300110303     D  jobName              244    253A                                        Job name
003400110228     D  jobUser              254    263A                                        Job user profile
003500110228     D  jobNumber            264    269S 0                                      Job number
003600110228     D  curUser              358    367A                                        Current user profile
003700110225     D done            DS                  QUALIFIED INZ
003800110225     D  init                           N
003900110225     D  newMail                        N
003901160527     D  addBody                        N
003902160527     D  addLargeBody                   N
004000110228     D mail            DS                  QUALIFIED
004100110302     D  originatorUserProfile...
004200110228     D                               10A
004300110302     D  originatorAddress...
004400110303     D                              256A   VARYING
004500110302     D  originatorName...
004600110303     D                               50A   VARYING
004700110225     D  date                         31A   VARYING
004800110225     D  subject                     255A   VARYING
004900110228     D  contentType                 255A   VARYING
005000110303     D  streamFile                  255A   VARYING
005100110304     D  tmpFile                        N   INZ(*OFF)
005200110303     D  addt0100                  65535A   VARYING
005300110303     D  header                    65535A   VARYING
005400110309     D  head                      65535A   VARYING                              <head>...</head>
005500110309     D  cssStyle                     15A
005600110309     D  style                     65535A   VARYING                              <style>...</style>
005700110228     D recipients      DS                  QUALIFIED
005800110302     D  n                            10I 0 INZ
005900110228     D  data                               DIM(255)
006000110228     D   address                    255A   OVERLAY(data) VARYING
006100110228     D   name                        50A   OVERLAY(data:*NEXT) VARYING
006200110228     D   distributionType...
006300110301     D                               10I 0 OVERLAY(data:*NEXT) INZ
006400110228     D   QTMADDR                    256A   OVERLAY(data:*NEXT) VARYING
006500110225
006600110225     ***************************************************************************
006700110225     **
006800110225     ** Campi.
006900110225     **
007000110225     ***************************************************************************
007100110303     D b               S             10I 0
007200110310     D body            S          65535A   DIM(255) VARYING BASED(bodyPtr)
007300110309     D cssStyleDft     S             15A
007400110302     D fd              S             10I 0                                      File descriptor
007500110309     D i               S             10I 0
007600110303     D nomeSistema     S              8A
007700110302     D p1              S             10I 0
007800110302     D p2              S             10I 0
007900110303     D stat            S              1A
008000110303     D statPtr         S               *   INZ(%ADDR(stat))
008100110309     D shortString     S            255A   VARYING
008200110309     D styleDft        S          65535A   VARYING
008300110302
008301160318     D mail_header_old...
008302160318     D                 s          65535A   VARYING
008400110302     D*--------------------------------------------------
008500110302     D* Procedure name: SNDMM_GetAddressByUserProfile
008600110302     D* Purpose:        Restituisce l'indirizzo di un profilo utente.
008700110302     D* Returns:        Indirizzo.
008800110302     D* Parameter:      priUserProfile => Profilo utente.
008900110302     D*--------------------------------------------------
009000110302     D SNDMM_GetAddressByUserProfile...
009100110302     D                 PR           256A
009200110302     D priUserProfile                10A   CONST
009300110302
009400110302     D*--------------------------------------------------
009500110302     D* Procedure name: SNDMM_GetNameByUserProfile
009600110302     D* Purpose:        Restituisce il nome del profilo utente.
009700110302     D* Returns:        Nome profilo utente.
009800110302     D* Parameter:      priUserProfile => Profilo utente.
009900110302     D*--------------------------------------------------
010000110302     D SNDMM_GetNameByUserProfile...
010100110302     D                 PR            50A
010200110302     D priUserProfile                10A   CONST
010300110309
010400110309     D*--------------------------------------------------
010500110309     D* Procedure name: SNDMM_GetStyle
010600110309     D* Purpose:        Restituisce una stringa contenente lo stile.
010700110309     D* Returns:        <style>...</style>
010800110309     D* Parameter:      priCssStyle => ID CSS style.
010900110309     D*--------------------------------------------------
011000110309     D SNDMM_GetStyle  PR         65535A   VARYING
011100110309     D  priCssStyle                  15A   VALUE
011200110309
011300110309
011400110225     ***************************************************************************
011500110225     **
011600110225     ** Prototipi.
011700110225     **
011800110225     ***************************************************************************
011900110303      /COPY GAITRASRC/SRCPROTOPR,ERRNO
012000110303      /COPY GAITRASRC/SRCPROTOPR,IFS
012100110303      /COPY GAITRASRC/SRCPROTOPR,IFS_TMPNAM
012200110302      /COPY GAITRASRC/SRCPROTOPR,QTMMSNDM
012300110304      /COPY GAITRASRC/SRCPROTOPR,TRULSNDMM
012400110228      /COPY GAITRASRC/SRCPROTOPR,QSYRUSRI
012500110303      /COPY GAITRASRC/SRCPROTOPR,XNETA
012600110303
012700110303
012800110225     P*--------------------------------------------------
012900110228     P* Procedure name: SNDMM_Init
013000110225     P* Purpose:        Inizializza il modulo.
013100110225     P* Returns:        Esito.
013200110225     P*--------------------------------------------------
013300110228     P SNDMM_Init      B                   EXPORT
013400110228     D SNDMM_Init      PI            10I 0
013500110225
013600110225     D retField        S             10I 0
013700110225
013800110225      /FREE
013900110225
014000110303       GetNomeSistema(nomeSistema);
014100110309       styleDft = SNDMM_GetStyle(SNDMM_CSS_STYLE_DEFAULT);
014200110309       done.init = *ON;
014300110225       RETURN retField;
014400110225
014500110225      /END-FREE
014600110228     P SNDMM_Init      E
014700110225
014800110225
014900110225     P*--------------------------------------------------
015000110228     P* Procedure name: SNDMM_Finalize
015100110225     P* Purpose:        Chiude il modulo.
015200110225     P* Returns:
015300110225     P*--------------------------------------------------
015400110228     P SNDMM_Finalize  B                   EXPORT
015500110228     D SNDMM_Finalize  PI
015600110225
015700110225
015800110225      /FREE
015900110225
016000110225       RESET done;
016100110228       RESET mail;
016200110225
016300110225      /END-FREE
016400110228     P SNDMM_Finalize  E
016500110225
016600110225
016700110225     P*--------------------------------------------------
016800110228     P* Procedure name: SNDMM_NewMail
016900110225     P* Purpose:        Inizia una nuova e-mail.
017000110225     P* Returns:        Esito.
017100110228     P* Parameter:      priUserProfileOriginator => Profilo utente mittente.
017200110225     P* Parameter:      priSubject => Oggetto.
017300110225     P* Parameter:      priContentType => Content-Type.
017400110302     P* Parameter:      priStreamFile => Stream file contenente il messaggio
017500110302     P*                 MIME preparato dal chiamante.
017600110309     P* Parameter:      priCssStyle => CSS style (tabella 'CSS').
017700110309     P*                 Se non passato si assume *DFT.
017800110309     P*--------------------------------------------------
017900110228     P SNDMM_NewMail   B                   EXPORT
018000110228     D SNDMM_NewMail   PI            10I 0
018100110228     D  priUserProfileOriginator...
018200110225     D                               10A   CONST
018300110225     D                                     OPTIONS(*OMIT)
018400110225     D  priSubject                  255A   VARYING
018500110225     D                                     CONST
018600110225     D priContentType               255A   VARYING
018700110225     D                                     CONST
018800110302     D priStreamFile...
018900110228     D                              255A   VARYING
019000110309     D                                     CONST OPTIONS(*NOPASS:*OMIT)
019100110309     D priCssStyle...
019200110309     D                               15A   CONST OPTIONS(*NOPASS)
019300110309
019400110225     D retField        S             10I 0
019500110225
019600110225      /FREE
019700110225
019800110228       IF NOT done.init;
019900110228         RETURN SNDMM_ESITO_MODULO_NON_INIZIALIZZATO;
020000110228       ENDIF;
020100110225
020200110228       RESET mail;
020300110228       RESET recipients;
020301140110       RESET b;
020400110228
020500110228       IF %ADDR(priUserProfileOriginator) <> *NULL
020600110228       AND priUserProfileOriginator <> *BLANK;
020700110302         mail.originatorUserProfile = priUserProfileOriginator;
020800110228       ELSE;
020900110302         mail.originatorUserProfile = psds.curUser;
021000110228       ENDIF;
021100110228
021200110302       mail.originatorAddress =
021300110303              %TRIMR(SNDMM_GetAddressByUserProfile(mail.originatorUserProfile));
021400110302       mail.originatorName =
021500110303              %TRIMR(SNDMM_GetNameByUserProfile(mail.originatorUserProfile));
021600110228       mail.subject = priSubject;
021700110228       mail.contentType = priContentType;
021800110228
021900110309       IF %PARMS() > 3 AND %ADDR(priStreamFile) <> *NULL
022000110309       AND priStreamFile <> *BLANK;
022100110302
022200110302         mail.streamFile = priStreamFile;
022300110304         mail.tmpFile = *OFF;
022400110302
022500110228       ELSE;
022600110302
022700110303         mail.streamFile = %STR(IFS_ProduceTemporaryFileName(*OMIT));
022800110303         IFS_RemoveLinkToFile(mail.streamFile);
022900110302
023000110302         fd = IFS_OpenFile( mail.streamFile
023100110302                          : O_CREAT + O_EXCL + O_WRONLY + O_CCSID
023200110302                          : M_RDWR
023300110302                          : CP_ISO8859_1
023400110302                          );
023500110302
023600110302         IF fd = -1;
023700110303           errnoPtr = GetErrno();
023800110303           DUMP(A);
023900110302           RETURN SNDMM_ESITO_ERRORE_IFS;
024000110302         ENDIF;
024100110302
024200110302         IFS_CloseFile(fd);
024300110303
024400110303         fd = IFS_OpenFile( mail.streamFile
024500110303                          : O_WRONLY + O_TEXTDATA + O_CCSID
024600110303                          : M_RDWR
024700110303                          : CP_ITALIAN
024800110303                          );
024900110302
025000110302         IF fd = -1;
025100110303           errnoPtr = GetErrno();
025200110302           DUMP(A);
025300110302           RETURN SNDMM_ESITO_ERRORE_IFS;
025400110302         ENDIF;
025500110303
025600110304         mail.tmpFile = *ON;
025700110304
025800160318         mail_header_old = 'Date: Ddd, 0 Mmm 0000 00:00:00 +0000' + CRLF
025900110303                     + 'Subject: ' + mail.subject + CRLF
026000150401                     + 'X-BRT-IBM i-SysName: ' + %TRIMR(nomeSistema)
026100110304                     + CRLF
026200150401                     + 'X-BRT-IBM i-Job: ' + %EDITC(psds.jobNumber : 'X')
026300110311                                                + '/'
026400110311                                                + %TRIMR(psds.jobUser)
026500110311                                                + '/'
026600110311                                                + %TRIMR(psds.jobName)
026700110304                     + CRLF
026800150401                     + 'X-BRT-IBM i-ProcName: ' + %TRIMR(psds.procLib)
026900110304                     + '/' + %TRIMR(psds.procName) + CRLF
027000150401                     + 'X-BRT-IBM i-Timestamp: ' + %CHAR(%TIMESTAMP())
027100110304                     + CRLF
027200110303                     + 'MIME-Version: 1.0' + CRLF
027300110303                     + 'Content-type: ' + mail.contentType + CRLF
027400110303                     + CRLF;
027500110309
027600110309         // Reperisco lo stile. Chiaramente lo faccio solo se il chiamante non
027700110309         // ha già preparato il file MIME.
027800110309
027900110309         IF %PARMS() > 4;
028000110309           mail.cssStyle = priCssStyle;
028100110309           mail.style = SNDMM_GetStyle(priCssStyle);
028200110309         ELSE;
028300110309           mail.cssStyle = SNDMM_CSS_STYLE_DEFAULT;
028400110309           mail.style = SNDMM_GetStyle(SNDMM_CSS_STYLE_DEFAULT);
028500110309         ENDIF;
028600110309
028700110309       ENDIF;
028800110228
028900110228       done.newMail = *ON;
029000110228
029100110303       RETURN retField;
029200110225
029300110225      /END-FREE
029400110228     P SNDMM_NewMail   E
029500110228
029600110228
029700110228     P*--------------------------------------------------
029800110228     P* Procedure name: SNDMM_AddRecipient
029900110228     P* Purpose:        Aggiunge un destinatario.
030000110228     P* Returns:        Esito.
030100110228     P* Parameter:      priRecipientAddress => Indirizzo destinatario.
030200110302     P*                 (p.e. 'd.cussini§tiscali.it')
030300110228     P* Parameter:      priDistributionType => 0 = normale; 1 = Cc; 2 = Ccn.
030400110228     P* Parameter:      priRecipientName => Nome destinatario.
030500110228     P*                 (p.e. 'Danilo Cussini')
030600110228     P*--------------------------------------------------
030700110228     P SNDMM_AddRecipient...
030800110228     P                 B                   EXPORT
030900110228     D SNDMM_AddRecipient...
031000110228     D                 PI            10I 0
031100110228     D  priRecipientAddress...
031200110228     D                              255A   CONST VARYING
031300110228     D  priDistributionType...
031400110228     D                               10I 0 CONST
031500110228     D  priRecipientName...
031600110228     D                               50A   CONST VARYING OPTIONS(*NOPASS)
031700110228
031800110228     D retField        S             10I 0
031900110228
032000110228      /FREE
032100110228
032200110228       IF NOT done.newMail;
032300110228         RETURN SNDMM_ESITO_MAIL_NON_INIZIALIZZATA;
032400110228       ENDIF;
032500110228
032600110228       IF recipients.n = %ELEM(recipients.address);
032700110228         RETURN SNDMM_ESITO_RECIPIENTS_OVERFLOW;
032800110228       ENDIF;
032900110228
033000110228       IF priDistributionType < SNDMM_DISTRIBUTION_TYPE_NORMAL
033100110228       OR priDistributionType > SNDMM_DISTRIBUTION_TYPE_BLIND_CARBON_COPY;
033200110228         RETURN SNDMM_ESITO_DISTRIBUTION_TYPE_NON_VALIDO;
033300110228       ENDIF;
033400110228
033500110228       IF recipients.n > 0 AND %LOOKUP( priRecipientAddress
033600110228                                      : recipients.address
033700110228                                      : 1
033800110228                                      : recipients.n
033900110228                                      ) > 0;
034000110228         RETURN retField;
034100110228       ENDIF;
034200110228
034300110228       recipients.n += 1;
034400110228       recipients.address(recipients.n) = priRecipientAddress;
034500110228       recipients.distributionType(recipients.n) = priDistributionType;
034600110228
034700110228       IF %PARMS() > 2;
034800110228         recipients.name(recipients.n) = priRecipientName;
034900110228       ENDIF;
035000110228
035100110228       RETURN retField;
035200110228
035300110228      /END-FREE
035400110228     P SNDMM_AddRecipient...
035500110228     P                 E
035600110228
035700110228
035800110228     P*--------------------------------------------------
035900110228     P* Procedure name: SNDMM_AddRecipientByUserProfile
036000110228     P* Purpose:        Aggiunge un destinatario dal profilo utente passato.
036100110228     P* Returns:        Esito.
036200110228     P* Parameter:      priUserProfileRecipient => Profilo utente destinatario.
036300110228     P*                 (p.e. 'EDPDC' 'CLI009' 'BAN*')
036400110228     P*                 Valori speciali: '*JOBUSER'  utente del lavoro
036500110228     P*                                  '*CURUSER'  utente corrente del lavoro
036600110302     P*                                  '*CED'      ced§bartolini.it
036700110302     P*                                  '*CEDALERT' cedalert§bartolini.it
036800110228     P* Parameter:      priDistributionType => 0 = normale; 1 = Cc; 2 = Ccn.
036900110228     P*--------------------------------------------------
037000110228     P SNDMM_AddRecipientByUserProfile...
037100110228     P                 B                   EXPORT
037200110228     D SNDMM_AddRecipientByUserProfile...
037300110228     D                 PI            10I 0
037400110228     D  priUserProfileRecipient...
037500110228     D                               10A   CONST
037600110228     D  priDistributionType...
037700110228     D                               10I 0 CONST
037800110228
037900110228     D retField        S             10I 0
038000110228     D userProfileRecipient...
038100110228     D                 S             10A   VARYING
038200110228     D stm             S            255A   VARYING
038300110228     D uteUte          S             10A
038400110228     D uteEma          S            100A   VARYING
038500110228
038600110228      /FREE
038700110228
038800110228       IF NOT done.newMail;
038900110228         RETURN SNDMM_ESITO_MAIL_NON_INIZIALIZZATA;
039000110228       ENDIF;
039100110228
039200110228       IF recipients.n = %ELEM(recipients.address);
039300110228         RETURN SNDMM_ESITO_RECIPIENTS_OVERFLOW;
039400110228       ENDIF;
039500110228
039600110228       SELECT;
039700110228         WHEN priUserProfileRecipient = SNDMM_RECIPIENT_CURRENT_USER;
039800110228           userProfileRecipient = %TRIMR(psds.curUser);
039900110228         WHEN priUserProfileRecipient = SNDMM_RECIPIENT_JOB_USER;
040000110228           userProfileRecipient = %TRIMR(psds.jobUser);
040100110228         WHEN priUserProfileRecipient = SNDMM_RECIPIENT_CED;
040200110303           RETURN SNDMM_AddRecipient( 'ced' + SNDMM_SM050000_CP1144
040300110303                                    + SNDMM_DOMINIO
040400110228                                    : priDistributionType
040500110228                                    );
040600110228         WHEN priUserProfileRecipient = SNDMM_RECIPIENT_CED_ALERT;
040700110303           RETURN SNDMM_AddRecipient( 'cedalert' + SNDMM_SM050000_CP1144
040800110303                                    + SNDMM_DOMINIO
040900110228                                    : priDistributionType
041000110228                                    );
041100110228         OTHER;
041200110228           userProfileRecipient = %TRIMR(priUserProfileRecipient);
041300110228       ENDSL;
041400110228
041500110311       stm = 'SELECT uteUte, +
041600110311             RTRIM(CASE uteEma WHEN '''' THEN uteUte ELSE uteEma END) +
041700110311             FROM azute00f +
041800110311             WHERE uteDsc >= ' + %CHAR(%DEC(%DATE():*ISO))
041900110228           ;
042000110228
042100110228       // Posso ricevere un nome utente (p.e. 'EDPDC')
042200110228       // oppure un nome utente generico (p.e. 'EDP*')
042300110228
042400110301       stm += ' AND uteUte ';
042500110228
042600110228       SELECT;
042700110228         WHEN %SCAN('*' : userProfileRecipient) = 0;
042800110228           stm += '= ''' + userProfileRecipient;
042900110228         OTHER;
043000110228           stm += 'LIKE ''' + %XLATE('*' : '%' : userProfileRecipient);
043100110228       ENDSL;
043200110228
043300110228       stm += '''';
043400110228       stm += ' FOR READ ONLY';
043500110228
043600110228       EXEC SQL
043700110228         PREPARE utenti FROM :stm
043800110228       ;
043900110228
044000110228       EXEC SQL
044100110228         DECLARE utenti NO SCROLL CURSOR FOR utenti
044200110228       ;
044300110228
044400110228       EXEC SQL
044500110228         OPEN utenti
044600110228       ;
044700110228
044800110228       IF sqlCode < 0;
044900110228         DUMP(A);
045000110228         RETURN sqlCode;
045100110228       ENDIF;
045200110228
045300110228       DOU sqlCode < 0;
045400110228
045500110228         EXEC SQL
045600110228           FETCH NEXT FROM utenti
045700110228             INTO :uteUte, :uteEma
045800110228         ;
045900110228
046000110228         SELECT;
046100110228           WHEN sqlCode = 100;
046200110228             LEAVE;
046300110228           WHEN sqlCode < 0;
046400110228             DUMP(A);
046500110228             retField = sqlCode;
046600110228             LEAVE;
046700110228         ENDSL;
046800110228
046900110228         p1 = 0;
047000110228         p2 = 0;
047100110228
047200110228         DOU p2 = 0 OR p2 >= %LEN(uteEma);
047300110228           p1 = p2 + 1;
047400110228           p2 = %SCAN(';' : uteEma : p1);
047500110228           IF p2 = 0;
047600110228             p2 = %LEN(uteEma) + 1;
047700110228           ENDIF;
047800170612           IF %SCAN(SNDMM_SM050000_CP1144 : uteEma) = *ZERO;
047801170612             SNDMM_AddRecipient( %SUBST(uteEma : p1 : p2 - p1)
047900170612                               + SNDMM_SM050000_CP1144 + SNDMM_DOMINIO
048000170612                               : priDistributionType
048100170612                               : %TRIMR(SNDMM_GetNameByUserProfile(uteUte))
048200170612                               );
048201170612           ELSE;
048202170612             SNDMM_AddRecipient( %SUBST(uteEma : p1 : p2 - p1)
048204170612                               : priDistributionType
048205170612                               : %TRIMR(SNDMM_GetNameByUserProfile(uteUte))
048206170612                               );
048207170612           ENDIF;
048300110228         ENDDO;
048400110228
048500110228       ENDDO;
048600110228
048700110228       EXEC SQL
048800110228         CLOSE utenti
048900110228       ;
049000110228
049100110228       RETURN retField;
049200110228
049300110228      /END-FREE
049400110228     P SNDMM_AddRecipientByUserProfile...
049500110228     P                 E
049600110228
049700110228
049800110228     P*--------------------------------------------------
049900110228     P* Procedure name: SNDMM_EndMail
050000110228     P* Purpose:        Chiude la mail e la invia.
050100110228     P* Returns:        Esito.
050200110228     P* Parameter:      priInviare => *ON = inviare e-mail.
050300110228     P*--------------------------------------------------
050400110228     P SNDMM_EndMail   B                   EXPORT
050500110228     D SNDMM_EndMail   PI            10I 0
050600110228     D  priInviare                     N   CONST
050700110228
050800110228     D retField        S             10I 0
050900110228     D p               S             10I 0 INZ(1)
051000110303     D to              S          32767A   VARYING
051100110228
051200110228      /FREE
051300110228
051400110228       IF NOT done.newMail;
051500110228         RETURN SNDMM_ESITO_MAIL_NON_INIZIALIZZATA;
051600110228       ENDIF;
051700110302
051701160318         // header (Felletti: prima era nella new_mail)
051702160318         mail.header = 'Date: Ddd, 0 Mmm 0000 00:00:00 +0000' + CRLF
051703160318                     + 'Subject: ' + mail.subject + CRLF
051704160318                     + 'X-BRT-IBM i-SysName: ' + %TRIMR(nomeSistema)
051705160318                     + CRLF
051706160318                     + 'X-BRT-IBM i-Job: ' + %EDITC(psds.jobNumber : 'X')
051707160318                                                + '/'
051708160318                                                + %TRIMR(psds.jobUser)
051709160318                                                + '/'
051710160318                                                + %TRIMR(psds.jobName)
051711160318                     + CRLF
051712160318                     + 'X-BRT-IBM i-ProcName: ' + %TRIMR(psds.procLib)
051713160318                     + '/' + %TRIMR(psds.procName) + CRLF
051714160318                     + 'X-BRT-IBM i-Timestamp: ' + %CHAR(%TIMESTAMP())
051715160318                     + CRLF
051716160318                     + 'MIME-Version: 1.0' + CRLF
051717160318                     + 'Content-type: ' + mail.contentType + CRLF
051718160318                     + CRLF;
051719160318
051800110304       IF mail.tmpFile;
051900110303
052000110303         // Destinatari CC.
052100110303
052200110303         to = 'CC: ';
052300110303
052400110303         FOR i = 1 TO recipients.n;
052500110303           IF recipients.distributionType(i) =
052600110303                                            SNDMM_DISTRIBUTION_TYPE_CARBON_COPY;
052700110303             IF to <> 'CC: ';
052800110303               to += ' ; ';
052900110303             ENDIF;
053000110303             to += recipients.name(i) + ' <' + recipients.address(i) + '>';
053100110303           ENDIF;
053200110303         ENDFOR;
053300110303
053400110303         IF to <> 'CC: ';
053500110303           mail.header = to + CRLF + mail.header;
053600110303         ENDIF;
053700110303
053800110303         // Destinatari TO.
053900110303
054000110303         to = 'To: ';
054100110303
054200110303         FOR i = 1 TO recipients.n;
054300110303           IF recipients.distributionType(i) = SNDMM_DISTRIBUTION_TYPE_NORMAL;
054400110303             IF to <> 'To: ';
054500110303               to += ' ; ';
054600110303             ENDIF;
054700110303             to += recipients.name(i) + ' <' + recipients.address(i) + '>';
054800110303           ENDIF;
054900110303         ENDFOR;
055000110303
055100110303         IF to <> 'To: ';
055200110303           mail.header = to + CRLF + mail.header;
055300110303         ENDIF;
055400110303
055500110303         mail.header = 'From: ' + mail.originatorName
055600110303                     + ' <' + mail.originatorAddress + '>' + CRLF
055700110303                     + mail.header
055800110303                     ;
055900110303
056000110303         IFS_WriteToDescriptor( fd
056100110303                              : %ADDR(mail.header) + 2
056200110303                              : %LEN(mail.header)
056300110303                              );
056301160908
056302160908         shortString = '<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" +
056303160908                        "http://www.w3.org/TR/html4/strict.dtd">' + CRLF;
056304160908
056305160908         IFS_WriteToDescriptor( fd
056306160908                              : %ADDR(shortString) + 2
056307160908                              : %LEN(shortString)
056308160908                              );
056400110309
056500110309         shortString = '<html>' + CRLF;
056600110309
056700110309         IFS_WriteToDescriptor( fd
056800110309                              : %ADDR(shortString) + 2
056900110309                              : %LEN(shortString)
057000110309                              );
057001160908
057002160908         shortString = '<head>' + CRLF;
057003160908
057004160908         IFS_WriteToDescriptor( fd
057005160908                              : %ADDR(shortString) + 2
057006160908                              : %LEN(shortString)
057007160908                              );
057100110309
057200160908         shortString = '<title></title>' + CRLF;
057300110309
057400110309         IFS_WriteToDescriptor( fd
057500110309                              : %ADDR(shortString) + 2
057600110309                              : %LEN(shortString)
057700110309                              );
057701160908
057702160908         shortString = '<meta http-equiv="content-type" +
057703160908                        content="text/html; charset=iso-8859-1">' + CRLF;
057704160908
057705160908         IFS_WriteToDescriptor( fd
057706160908                              : %ADDR(shortString) + 2
057707160908                              : %LEN(shortString)
057708160908                              );
057800110309
057900110309         shortString = '<style type="text/css">' + CRLF;
058000110309
058100110309         IFS_WriteToDescriptor( fd
058200110309                              : %ADDR(shortString) + 2
058300110309                              : %LEN(shortString)
058400110309                              );
058500110309
058600110309         IFS_WriteToDescriptor( fd
058700110309                              : %ADDR(mail.style) + 2
058800110309                              : %LEN(mail.style)
058900110309                              );
059000110309
059100110309         shortString = '</style>' + CRLF;
059200110309
059300110309         IFS_WriteToDescriptor( fd
059400110309                              : %ADDR(shortString) + 2
059500110309                              : %LEN(shortString)
059600110309                              );
059700110309
059800110309         shortString = '</head>' + CRLF;
059900110309
060000110309         IFS_WriteToDescriptor( fd
060100110309                              : %ADDR(shortString) + 2
060200110309                              : %LEN(shortString)
060300110309                              );
060400110310
060500110310         shortString = '<body>' + CRLF;
060600110310
060700110310         IFS_WriteToDescriptor( fd
060800110310                              : %ADDR(shortString) + 2
060900110310                              : %LEN(shortString)
061000110310                              );
061100110303
061200110303         FOR i = 1 TO b;
061300110303           IFS_WriteToDescriptor( fd
061400110310                                : %ADDR(body(i)) + 2
061500110310                                : %LEN(body(i))
061600110303                                );
061700110303         ENDFOR;
061800110310
061900110310         shortString = '</body>' + CRLF;
062000110310
062100110310         IFS_WriteToDescriptor( fd
062200110310                              : %ADDR(shortString) + 2
062300110310                              : %LEN(shortString)
062400110310                              );
062500110309
062600110309         shortString = '</html>';
062700110309
062800110309         IFS_WriteToDescriptor( fd
062900110309                              : %ADDR(shortString) + 2
063000110309                              : %LEN(shortString)
063100110309                              );
063200110303
063300110302         IFS_CloseFile(fd);
063400110303
063500110302       ENDIF;
063600110228
063700110228       IF recipients.n = 0;
063800110228         RETURN SNDMM_ESITO_NO_RECIPIENTS;
063900110228       ENDIF;
064000110302
064100110228       CLEAR QTMT0100;
064200110301       QTMFN = 'ADDT0100';
064300110228
064400110228       FOR i = 1 TO recipients.n;
064500110228         QTMDT = recipients.distributionType(i);
064600110304         recipients.QTMADDR(i) = %XLATE( SNDMM_SM050000_CP1144
064700110304                                       : SNDMM_SM050000_CP1140
064800110304                                       : recipients.address(i) );
064900110228         QTMAL = %LEN(recipients.QTMADDR(i));
065000110302         QTMNASO = %SIZE(QTMT0100) + QTMAL;
065100110304         %SUBST(mail.addt0100 : p) = QTMT0100 + recipients.QTMADDR(i);
065200110228         p += QTMNASO;
065300110302         IF p > %SIZE(mail.addt0100);
065400110228           recipients.n = i;
065500110228           LEAVE;
065600110228         ENDIF;
065700110228       ENDFOR;
065800110228
065900110228       IF priInviare;
066000110303
066100110302         CLEAR QUSEC;
066200110302         QUSBPRV = %SIZE(QUSEC);
066300110303
066400110303         mail.originatorAddress = %XLATE( SNDMM_SM050000_CP1144
066500110303                                        : SNDMM_SM050000_CP1140
066600110303                                        : mail.originatorAddress
066700110303                                        );
066800110303
066900110303         SendMIMEMail( mail.streamFile
067000110303                     : %LEN(mail.streamFile)
067100110302                     : mail.originatorAddress
067200110303                     : %LEN(mail.originatorAddress)
067300110302                     : mail.addt0100
067400110301                     : recipients.n
067500110302                     : QUSEC
067600110228                     );
067700110303
067800110302         IF QUSEI <> *BLANK;
067900110301           DUMP(A);
068000110301           retField = SNDMM_ESITO_INVIO_NON_RIUSCITO;
068100110303         ELSE;
068200110303           // Se l'invio riesce lo stream file viene cancellato, quindi se a
068300110303           // questo punto lo stream file esiste ancora qualcosa è andato male.
068400110303           IF IFS_GetFileInformation(mail.streamFile : statPtr) = 0;
068500110303             retField = SNDMM_ESITO_INVIO_NON_RIUSCITO;
068600110303           ENDIF;
068700110303         ENDIF;
068800110303
068900110228       ENDIF;
069000110228
069100110228       RESET done.newMail;
069200110228       RESET mail;
069300140110       DEALLOC(N) bodyPtr;
069400110228
069500110303       RETURN retField;
069600110228
069700110228      /END-FREE
069800110228     P SNDMM_EndMail   E
069900110228
070000110302
070100110302     P*--------------------------------------------------
070200110302     P* Procedure name: SNDMM_GetAddressByUserProfile
070300110302     P* Purpose:        Restituisce l'indirizzo di un profilo utente.
070400110302     P* Returns:        Indirizzo.
070500110302     P* Parameter:      priUserProfile => Profilo utente.
070600110302     P*--------------------------------------------------
070700110302     P SNDMM_GetAddressByUserProfile...
070800110302     P                 B
070900110302     D SNDMM_GetAddressByUserProfile...
071000110302     D                 PI           256A
071100110302     D priUserProfile                10A   CONST
071200110302
071300110302     D retField        S            256A
071400110302
071500110302      /FREE
071600110302
071700110302       EXEC SQL
071800110311         SELECT CASE uteEma WHEN '' THEN uteUte ELSE uteEma END
071900110302           INTO :retField
072000110302           FROM azute00f
072100110302           WHERE uteUte = :priUserProfile
072200110302       ;
072300110302
072400110302       SELECT;
072500110302         WHEN sqlCode < 0;
072600110302           DUMP(A);
072700110302           RETURN *BLANK;
072800110302         WHEN sqlCode = SQLCODE_ROW_NOT_FOUND;
072900110304           retField = priUserProfile;
073000110302       ENDSL;
073100110302
073200110302       // Prendo solo il primo indirizzo:
073300110302       // p.e. 'danilo.cussini;ced' restituisco 'danilo.cussini@bartolini.it'.
073400110302
073500110302       p1 = %SCAN(';' : retField);
073600110302
073700110302       IF p1 > 0;
073800170612         retField = %SUBST(retField : 1 : p1 - 1);
074000110302       ENDIF;
074100110302
074200170612       IF %SCAN(SNDMM_SM050000_CP1144 : retField) = *ZERO;
074201170612         retField = %TRIMR(retField) + SNDMM_SM050000_CP1144 + SNDMM_DOMINIO;
074202170612       ENDIF;
074203170612
074204170612       RETURN retField;
074300110302
074400110302      /END-FREE
074500110302     P SNDMM_GetAddressByUserProfile...
074600110302     P                 E
074700110302
074800110302
074900110302     P*--------------------------------------------------
075000110302     P* Procedure name: SNDMM_GetNameByUserProfile
075100110302     P* Purpose:        Restituisce il nome del profilo utente.
075200110302     P* Returns:        Nome profilo utente.
075300110302     P* Parameter:      priUserProfile => Profilo utente.
075400110302     P*--------------------------------------------------
075500110302     P SNDMM_GetNameByUserProfile...
075600110302     P                 B
075700110302     D SNDMM_GetNameByUserProfile...
075800110302     D                 PI            50A
075900110302     D priUserProfile                10A   CONST
076000110302
076100110302      /FREE
076200110302
076300110302       CLEAR QUSEC;
076400110302       QUSBPRV = %SIZE(QUSEC);
076500110302
076600110302       RetrieveUserInformation( QSYI0300
076700110302                              : %SIZE(QSYI0300)
076800110302                              : 'USRI0300'
076900110302                              : priUserProfile
077000110302                              : QUSEC
077100110302                              );
077200110302
077300110302       IF QUSEI <> *BLANK;
077400110302         RETURN priUserProfile;
077500110302       ENDIF;
077600110302
077700110302       RETURN QSYTD;
077800110302
077900110302      /END-FREE
078000110302     P SNDMM_GetNameByUserProfile...
078100110302     P                 E
078200110302
078300110302
078400110302     P*--------------------------------------------------
078500110310     P* Procedure name: SNDMM_AddBody
078600110302     P* Purpose:        Aggiunge il corpo del messaggio.
078700110302     P* Returns:        Esito.
078800110310     P* Parameter:      priBody => <body>...</body>
078900110302     P*--------------------------------------------------
079000110310     P SNDMM_AddBody...
079100110302     P                 B                   EXPORT
079200110310     D SNDMM_AddBody...
079300110302     D                 PI            10I 0
079400110310     D priBody                    65535A   VARYING
079500110302     D                                     OPTIONS(*VARSIZE)
079600110302
079700110302     D retField        S             10I 0
079800110302
079900110302      /FREE
080000110303
080100110303       IF NOT done.newMail;
080200110303         RETURN SNDMM_ESITO_MAIL_NON_INIZIALIZZATA;
080300110303       ENDIF;
080400110302
080500110304       IF NOT mail.tmpFile;
080600110302         RETURN retField;
080700110302       ENDIF;
080701160527
080702160527       IF done.addLargeBody;
080703160527         RETURN SNDMM_ESITO_ADDBODY_NON_PERMESSO_DOPO_ADDLARGEBODY;
080704160527       ENDIF;
080705160527
080707160527       done.addBody = *ON;
080800110302
080900110303       b += 1;
081000110303
081100110303       IF b = 1;
081200110310         bodyPtr = %ALLOC(%SIZE(body));
081300110303       ELSE;
081400110310         bodyPtr = %REALLOC(bodyPtr : %SIZE(body) * b);
081500110303       ENDIF;
081600110309
081700110310       body(b) = priBody;
081800110303
081900110303       RETURN retField;
082000110302
082100110302      /END-FREE
082200110310     P SNDMM_AddBody...
082300110302     P                 E
082400110302
082500110309
082600110309     P*--------------------------------------------------
082700110309     P* Procedure name: SNDMM_GetStyle
082800110309     P* Purpose:        Restituisce una stringa contenente lo stile.
082900110309     P* Returns:        <style>...</style>
083000110309     P* Parameter:      priCssStyle => ID CSS style.
083100110309     P*--------------------------------------------------
083200110309     P SNDMM_GetStyle  B
083300110309     D SNDMM_GetStyle  PI         65535A   VARYING
083400110309     D  priCssStyle                  15A   VALUE
083500110309
083600110309     D retField        S          65535A   VARYING
083700110309     D tbeke1          S             15A   VARYING
083800110309     D tbeke2          S             15A   VARYING
083900110309     D tbeUni          S            256A   VARYING
084000110309
084100110309      /FREE
084200110309
084300110309       SELECT;
084400110309         WHEN priCssStyle = SNDMM_CSS_STYLE_NONE;
084500110309           RETURN retField;
084600110309         WHEN priCssStyle = SNDMM_CSS_STYLE_DEFAULT AND styleDft <> *BLANK;
084700110309           RETURN styleDft;
084800110309       ENDSL;
084900110309
085000110309       EXEC SQL
085100110309         DECLARE style NO SCROLL CURSOR FOR
085200110309           -- Prendo tutto lo stile richiesto.
085300110309           SELECT RTRIM(rqs.tbeKe1), RTRIM(rqs.tbeKe2), RTRIM(rqs.tbeUni)
085400110309           FROM tntbe00f AS rqs
085500110309           WHERE rqs.tbeCod = 'CSS'
085600110309             AND rqs.tbeKe1 = :priCssStyle
085700110309             AND rqs.tbeLin = ''
085800110309             AND rqs.tbeSif = ''
085900110309           --
086000110309           UNION
086100110309           -- Prendo lo stile di default per differenza con lo stile richiesto.
086200110309           SELECT RTRIM(dft.tbeKe1), RTRIM(dft.tbeKe2), RTRIM(dft.tbeUni)
086300110309           FROM tntbe00f AS dft
086400110309           EXCEPTION JOIN tntbe00f AS rqs
086500110309                       ON dft.tbeCod = rqs.tbeCod
086600110309                      AND :priCssStyle = rqs.tbeke1
086700110309                      AND dft.tbeKe2 = rqs.tbeKe2
086800110309                      AND dft.tbeLin = rqs.tbeLin
086900110309                      AND dft.tbeSif = rqs.tbeSif
087000110309           WHERE dft.tbeCod = 'CSS'
087100110309             AND dft.tbeKe1 = :SNDMM_CSS_STYLE_DEFAULT
087200110309             AND dft.tbeLin = ''
087300110309             AND dft.tbeSif = ''
087400110309       ;
087500110309
087600110309       EXEC SQL
087700110309         OPEN style
087800110309       ;
087900110309
088000110309       SELECT;
088100110309         WHEN sqlCode < *ZERO;
088200110309           DUMP(A);
088300110309           RETURN retField;
088400110309       ENDSL;
088500110309
088600110309       DOU sqlCode = SQLCODE_ROW_NOT_FOUND OR sqlCode < *ZERO;
088700110309
088800110309         EXEC SQL
088900110309           FETCH NEXT FROM style
089000110309             INTO :tbeKe1, :tbeKe2, :tbeUni
089100110309         ;
089200110309
089300110309         SELECT;
089400110309           WHEN sqlCode = SQLCODE_ROW_NOT_FOUND;
089500110309             LEAVE;
089600110309           WHEN sqlCode < *ZERO;
089700110309             DUMP(A);
089800110309             CLEAR retField;
089900110309             LEAVE;
090000110309         ENDSL;
090100110309
090200110311         retField += '/* ' + tbeKe1 + ' ' + tbeKe2 + ' */ '
090300110309                   + tbeUni + CRLF;
090400110309
090500110309       ENDDO;
090600110309
090700110309       EXEC SQL
090800110309         CLOSE style
090900110309       ;
091000110309
091100110309       RETURN retField;
091200110309
091300110309      /END-FREE
091400110309     P SNDMM_GetStyle  E
091401160318
091402160318     P*--------------------------------------------------
091403160318     P* Procedure name: SNDMM_SetSubject...
091404160318     P* Purpose:        reimposta l'oggetto della mail
091405160318     P* Returns:        Esito.
091406160318     P* Parameter:      priSubject => Oggetto.
091408160318     P*--------------------------------------------------
091414160318     P SNDMM_SetSubject...
091415160318     P                 B                   EXPORT
091416160318     D SNDMM_SetSubject...
091417160318     D                 PI            10I 0
091418160318     D  priSubject                  255A   VARYING
091422160318     D                                     CONST
091423160318
091431160318     D retField        S             10I 0
091432160318
091433160318                     mail.subject = priSubject;
091434160318                     RETURN retField;
091435160318
091500160318     P SNDMM_SetSubject...
091501160318     P                 E
091502160526
091505160526     P*--------------------------------------------------
091506160526     P* Procedure name: SNDMM_AddLargeBody
091507160526     P* Purpose:        Aggiunge il testo passato al corpo del messaggio.
091508160526     P* Returns:        Esito.
091509160526     P* Parameter:      priBody => <body>...</body>
091510160526     P*--------------------------------------------------
091511160526     P SNDMM_AddLargeBody...
091512160526     P                 B                   EXPORT
091513160526     D SNDMM_AddLargeBody...
091514160526     D                 PI            10I 0
091515160526     D priBody                    65535A   VARYING
091516160526     D                                     OPTIONS(*VARSIZE)
091517160526
091518160526     D retField        S             10I 0
091519160526
091520160526       IF NOT done.newMail;
091523160526         RETURN SNDMM_ESITO_MAIL_NON_INIZIALIZZATA;
091524160526       ENDIF;
091525160526
091526160526       IF NOT mail.tmpFile;
091527160526         RETURN retField;
091528160526       ENDIF;
091529160527
091530160527       IF done.addBody;
091531160527         RETURN SNDMM_ESITO_ADDLARGEBODY_NON_PERMESSO_DOPO_ADDBODY;
091532160527       ENDIF;
091533160527
091534160527       done.addLargeBody = *ON;
091535160527
091536160526        IF b = *zero;
091537160526         b = 1;
091538160526         bodyPtr = %ALLOC(%SIZE(body));
091539160527         clear body(b);
091540160527        ELSE;
091541160527          if %len(body(b)) + %len(priBody) > 65535;
091542160527          b += 1;
091543160527          bodyPtr = %REALLOC(bodyPtr : %SIZE(body) * b);
091544160527          clear body(b);
091545160527          endif;
091546160526        ENDIF;
091547160526
091548160526       body(b) += priBody;
091549160526
091550160526       RETURN retField;
091551160526
091552160526     P SNDMM_AddLargeBody...
091553160526     P                 E
091554160526
091555160526
