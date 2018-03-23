000100110225     D*--------------------------------------------------
000200110228     D* Procedure name: SNDMM_Init
000300110225     D* Purpose:        Inizializza il modulo.
000400110225     D* Returns:        Esito.
000500110225     D*--------------------------------------------------
000600110228     D SNDMM_Init      PR            10I 0
000700110225
000800110225     D*--------------------------------------------------
000900110228     D* Procedure name: SNDMM_Finalize
001000110225     D* Purpose:        Chiude il modulo.
001100110225     D* Returns:
001200110225     D*--------------------------------------------------
001300110228     D SNDMM_Finalize  PR
001400110225
001500110228     D*--------------------------------------------------
001600110228     D* Procedure name: SNDMM_NewMail
001700110228     D* Purpose:        Inizia una nuova e-mail.
001800110228     D* Returns:        Esito.
001900110228     D* Parameter:      priUserProfileOriginator => Profilo utente mittente.
002000110228     D* Parameter:      priSubject => Oggetto.
002100110228     D* Parameter:      priContentType => Content-Type.
002200110302     D* Parameter:      priStreamFile => Stream file contenente il messaggio
002300110302     D*                 MIME preparato dal chiamante.
002400110309     D* Parameter:      priCssStyle => CSS style (tabella 'CSS').
002500110309     D*                 Se non passato si assume *DFT.
002600110228     D*--------------------------------------------------
002700110228     D SNDMM_NewMail   PR            10I 0
002800110228     D  priUserProfileOriginator...
002900110228     D                               10A   CONST
003000110228     D                                     OPTIONS(*OMIT)
003100110228     D  priSubject                  255A   VARYING
003200110228     D                                     CONST
003300110228     D priContentType               255A   VARYING
003400110228     D                                     CONST
003500110302     D priStreamFile...
003600110228     D                              255A   VARYING
003700110309     D                                     CONST OPTIONS(*NOPASS:*OMIT)
003800110309     D priCssStyle...
003900110309     D                               15A   CONST OPTIONS(*NOPASS)
004000110228
004100110228     D*--------------------------------------------------
004200110228     D* Procedure name: SNDMM_EndMail
004300110228     D* Purpose:        Chiude la mail e la invia.
004400110228     D* Returns:        Esito.
004500110228     D* Parameter:      priInviare => *ON = inviare e-mail.
004600110228     D*--------------------------------------------------
004700110228     D SNDMM_EndMail   PR            10I 0
004800110228     D  priInviare                     N   CONST
004900110228
005000110228     D*--------------------------------------------------
005100110228     D* Procedure name: SNDMM_AddRecipientByUserProfile
005200110228     D* Purpose:        Aggiunge un destinatario dal profilo utente passato.
005300110228     D* Returns:        Esito.
005400110228     D* Parameter:      priUserProfileRecipient => Profilo utente destinatario.
005500110228     D*                 (p.e. 'EDPDC' 'CLI009' 'BAN*')
005600110228     D*                 Valori speciali: '*JOBUSER'  utente del lavoro
005700110228     D*                                  '*CURUSER'  utente corrente del lavoro
005800110228     D*                                  '*CED'      ced@bartolini.it
005900110228     D*                                  '*CEDALERT' cedalert@bartolini.it
006000110228     D* Parameter:      priDistributionType => 0 = normale; 1 = Cc; 2 = Ccn.
006100110228     D*--------------------------------------------------
006200110228     D SNDMM_AddRecipientByUserProfile...
006300110228     D                 PR            10I 0
006400110228     D  priUserProfileRecipient...
006500110228     D                               10A   CONST
006600110228     D  priDistributionType...
006700110228     D                               10I 0 CONST
006800110228
006900110228     D*--------------------------------------------------
007000110228     D* Procedure name: SNDMM_AddRecipient
007100110228     D* Purpose:        Aggiunge un destinatario.
007200110228     D* Returns:        Esito.
007300110228     D* Parameter:      priRecipientAddress => Indirizzo destinatario.
007400110228     D*                 (p.e. 'd.cussini@tiscali.it')
007500110228     D* Parameter:      priDistributionType => 0 = normale; 1 = Cc; 2 = Ccn.
007600110228     D* Parameter:      priRecipientName => Nome destinatario.
007700110228     D*                 (p.e. 'Danilo Cussini')
007800110228     D*--------------------------------------------------
007900110228     D SNDMM_AddRecipient...
008000110228     D                 PR            10I 0
008100110228     D  priRecipientAddress...
008200110228     D                              255A   CONST VARYING
008300110228     D  priDistributionType...
008400110228     D                               10I 0 CONST
008500110228     D  priRecipientName...
008600110228     D                               50A   CONST VARYING OPTIONS(*NOPASS)
008700110228
008800110302     D*--------------------------------------------------
008900110310     D* Procedure name: SNDMM_AddBody
009000110302     D* Purpose:        Aggiunge il corpo del messaggio.
009100110302     D* Returns:        Esito.
009200110310     D* Parameter:      priBody => <body>...</body>
009300110302     D*--------------------------------------------------
009400110310     D SNDMM_AddBody...
009500110302     D                 PR            10I 0
009600110310     D priBody                    65535A   VARYING
009700110302     D                                     OPTIONS(*VARSIZE)
009800110302
009801160318     P*--------------------------------------------------
009802160318     P* Procedure name: SNDMM_SetSubject...
009803160318     P* Purpose:        reimposta l'oggetto della mail
009804160318     P* Returns:        Esito.
009805160318     P* Parameter:      priSubject => Oggetto.
009806160318     P*--------------------------------------------------
009807160318     D SNDMM_SetSubject...
009810160318     D                 PR            10I 0
009811160318     D  priSubject                  255A   VARYING
009812160318     D                                     CONST
009813160318
009814160526
009815160526     D*--------------------------------------------------
009816160526     D* Procedure name: SNDMM_AddLargeBody
009817160526     P* Purpose:        Aggiunge il testo passato al corpo del messaggio.
009818160526     D* Returns:        Esito.
009819160526     D* Parameter:      priBody => <body>...</body>
009820160526     D*--------------------------------------------------
009821160526     D SNDMM_AddLargeBody...
009822160526     D                 PR            10I 0
009823160526     D priBody                    65535A   VARYING
009824160526     D                                     OPTIONS(*VARSIZE)
009825160526
