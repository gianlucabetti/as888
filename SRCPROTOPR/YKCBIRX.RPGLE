000100101119     D*--------------------------------------------------
000200101119     D* Procedure name: CBIRX_Init
000300101119     D* Purpose:        Inizializzazione modulo.
000400101119     D* Returns:        Esito.
000500101119     D* Parameter:      piKpjba
000600101119     D*--------------------------------------------------
000700101119     D CBIRX_Init      PR            10I 0
000800101119     D  piSocieta                          LIKE(andiz00f.societa)
000900101119     D  piKpjba                            LIKEDS(kpjba)
001000101125     D  piPrtLog                       N   CONST
001100101119
001200101119     D*--------------------------------------------------
001300101119     D* Procedure name: CBIRX_Finalize
001400101119     D* Purpose:        Chiusura modulo.
001500101119     D* Returns:
001600101119     D*--------------------------------------------------
001700101119     D CBIRX_Finalize  PR
001800101119
001900101119     D*--------------------------------------------------
002000101119     D* Procedure name: CBIRX_NewProvider
002100101119     D* Purpose:        Inizia una sessione per un provider.
002200101119     D* Returns:        Esito.
002300101119     D* Parameter:      piProvider => Provider.
002400101119     D*--------------------------------------------------
002500101119     D CBIRX_NewProvider...
002600101119     D                 PR            10I 0
002700101119     D  piProvider                   15A   CONST
002800101119
002900101122     D*--------------------------------------------------
003000101122     D* Procedure name: CBIRX_EndProvider
003100101122     D* Purpose:        Fine provider.
003200101122     D* Returns:        Esito.
003300101122     D*--------------------------------------------------
003400101122     D CBIRX_EndProvider...
003500101122     D                 PR            10I 0
003600101124
003700101122     D*--------------------------------------------------
003800101122     D* Procedure name: CBIRX_RunProvider
003900101122     D* Purpose:        Elaborazione del provider.
004000101122     D* Returns:        Esito.
004100101122     D* Parameter:      piNumeroFilesElaborati => Numero dei files elaborati.
004200101122     D*--------------------------------------------------
004300101122     D CBIRX_RunProvider...
004400101122     D                 PR            10I 0
004500101122     D  piNumeroFilesElaborati...
004600101123     D                               10I 0 OPTIONS(*NOPASS)
004700101123     D  piNumeroRigheCopiate...
004800101123     D                               10I 0 OPTIONS(*NOPASS)
004900101123
005000101122     D*--------------------------------------------------
005100101122     D* Procedure name: CBIRX_GetNextFileName
005200101122     D* Purpose:        Restituisce il nome del prossimo file da elaborare.
005300101122     D* Returns:        File name.
005400101122     D*--------------------------------------------------
005500101122     D CBIRX_GetNextFileName...
005600101122     D                 PR            10I 0
005700101122     D piFileName                   255A   VARYING
005800101122
005900101122     D*--------------------------------------------------
006000101122     D* Procedure name: CBIRX_RunFile
006100101122     D* Purpose:        Elabora un file.
006200101122     D* Returns:        Esito.
006300101122     D* Parameter:      piIfsDir => Indirizzario IFS.
006400101122     D* Parameter:      piFileName => Nome file.
006500101122     D* Parameter:      piDb2Lib => Libreria DB2.
006600101122     D* Parameter:      piDb2File => File DB2.
006700101122     D* Parameter:      piDb2Mbr => Membro DB2.
006800101122     D*--------------------------------------------------
006900101122     D CBIRX_RunFile   PR            10I 0
007000101122     D  piIfsDir                    255A   VARYING
007100101122     D                                     CONST
007200101122     D  piFileName                  255A   VARYING
007300101122     D                                     CONST
007400101122     D  piDb2Lib                     10A   VARYING
007500101122     D                                     CONST
007600101122     D  piDb2File                    10A   VARYING
007700101122     D                                     CONST
007800101122     D  piDb2Mbr                     10A   VARYING
007900101122     D                                     CONST
008000101123     D  piNumeroRigheCopiate...
008100110415     D                               10I 0 OPTIONS(*NOPASS:*OMIT)
008200110415     D  piIfsDirSav                 255A   VARYING
008300110415     D                                     CONST
008400110415     D                                     OPTIONS(*NOPASS)
008500110418
008600101123     D*--------------------------------------------------
008700101123     D* Procedure name: CBIRX_GetDescrizioneProvider
008800101123     D* Purpose:        Restituisce la descrizione del provider.
008900101123     D* Returns:        Descrizione provider.
009000101123     D*--------------------------------------------------
009100101123     D CBIRX_GetDescrizioneProvider...
009200101123     D                 PR            20A   VARYING
009300101123
009400101123     D*--------------------------------------------------
009500101123     D* Procedure name: CBIRX_Mailto
009600101123     D* Purpose:        Invia una e-mail agli utenti interessati.
009700101123     D* Returns:        Esito.
009800101123     D* Parameter:      piSubject => Oggetto.
009900101123     D* Parameter:      piBody => Corpo del messaggio.
010000110310     D* Parameter:      piHtml => Inviare in formato HTML.
010100101123     D*--------------------------------------------------
010200101123     D CBIRX_Mailto    PR            10I 0
010300101123     D  piSubject                      *   VALUE OPTIONS(*STRING:*TRIM)
010400101123     D  piBody                         *   VALUE OPTIONS(*STRING:*TRIM)
010500110310     D  piHtml                         N   VALUE OPTIONS(*NOPASS)
010600101123
010700101124     D*--------------------------------------------------
010800101124     D* Procedure name: CBIRX_GetMessaggioErrore
010900101124     D* Purpose:        Restituisce il messaggio dell'ultimo errore.
011000101124     D* Returns:        Messaggio di errore.
011100101124     D*--------------------------------------------------
011200101124     D CBIRX_GetMessaggioErrore...
011300101124     D                 PR           255A   VARYING
011400101124
