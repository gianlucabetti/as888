000100101119     D*--------------------------------------------------
000200101202     D* Procedure name: CBITX_Init
000300101119     D* Purpose:        Inizializzazione modulo.
000400101119     D* Returns:        Esito.
000500101119     D* Parameter:      piKpjba
000600101119     D*--------------------------------------------------
000700101202     D CBITX_Init      PR            10I 0
000800101119     D  piSocieta                          LIKE(andiz00f.societa)
000900101119     D  piKpjba                            LIKEDS(kpjba)
001000101125     D  piPrtLog                       N   CONST
001100101119
001200101119     D*--------------------------------------------------
001300101202     D* Procedure name: CBITX_Finalize
001400101119     D* Purpose:        Chiusura modulo.
001500101119     D* Returns:
001600101119     D*--------------------------------------------------
001700101202     D CBITX_Finalize  PR
001800101119
001900101203     D*--------------------------------------------------
002000101203     D* Procedure name: CBITX_GetBancaByDistinta
002100101203     D* Purpose:        Restituisce la banca di una distinta.
002200101203     D* Returns:        Banca.
002300101203     D* Parameter:      piSocieta => Società.
002400101203     D* Parameter:      piDataDistinta => Data distinta.
002500101203     D* Parameter:      piNumeroDistinta => Numero distinta.
002600101203     D*--------------------------------------------------
002700101203     D CBITX_GetBancaByDistinta...
002800101203     D                 PR                  LIKE(andiz00f.banca)
002900101203     D  piSocieta                          LIKE(andiz00f.societa)
003000101203     D piDataDistinta                  D   DATFMT(*ISO)
003100101203     D  piNumeroDistinta...
003200101203     D                                     LIKE(andiz00f.nr)
003300101203
003400101203     D*--------------------------------------------------
003500101203     D* Procedure name: CBITX_GetAbiByBanca
003600101203     D* Purpose:        Restituisce l' ABI di una banca.
003700101203     D* Returns:        ABI
003800101203     D* Parameter:      piBanca => Banca
003900101203     D*--------------------------------------------------
004000101203     D CBITX_GetAbiByBanca...
004100101203     D                 PR                  LIKE(andiz00f.abi)
004200101203     D  piBanca                            LIKE(andiz00f.banca)
004300101203
004400101202     D*--------------------------------------------------
004500101202     D* Procedure name: CBITX_GetProviderByAbi
004600101202     D* Purpose:        Restituisce il provider di una banca.
004700101202     D* Returns:        Provider.
004800101202     D* Parameter:      piAbi => ABI
004900101202     D*--------------------------------------------------
005000101202     D CBITX_GetProviderByAbi...
005100101202     D                 PR            15A
005200101202     D  piAbi                              LIKE(andiz00f.abi) CONST
005300101202
005400101119     D*--------------------------------------------------
005500101202     D* Procedure name: CBITX_NewProvider
005600101119     D* Purpose:        Inizia una sessione per un provider.
005700101119     D* Returns:        Esito.
005800101119     D* Parameter:      piProvider => Provider.
005900101119     D*--------------------------------------------------
006000101202     D CBITX_NewProvider...
006100101119     D                 PR            10I 0
006200101119     D  piProvider                   15A   CONST
006300101119
006400101122     D*--------------------------------------------------
006500101202     D* Procedure name: CBITX_EndProvider
006600101122     D* Purpose:        Fine provider.
006700101122     D* Returns:        Esito.
006800101122     D*--------------------------------------------------
006900101202     D CBITX_EndProvider...
007000101122     D                 PR            10I 0
007100101209
007200101209     D*--------------------------------------------------
007300101220     D* Procedure name: CBITX_GetDb2MbrByProvider
007400101209     D* Purpose:        Restituisce il membro DB2 del provider
007500101209     D* Returns:        Esito.
007600101209     D* Parameter:      piDb2Lib => Libreria.
007700101209     D* Parameter:      piDb2File => File.
007800101209     D* Parameter:      piDb2Mbr => Membro.
007900101209     D*--------------------------------------------------
008000101220     D CBITX_GetDb2MbrByProvider...
008100101209     D                 PR            10I 0
008200101220     D piTipoDistinta                 1A   CONST
008300101209     D  piDb2Lib                     10A   VARYING
008400101209     D  piDb2File                    10A   VARYING
008500101220     D  piDb2Mbr                     10A   VARYING
008600101123
008700101123     D*--------------------------------------------------
008800101202     D* Procedure name: CBITX_GetDescrizioneProvider
008900101123     D* Purpose:        Restituisce la descrizione del provider.
009000101123     D* Returns:        Descrizione provider.
009100101123     D*--------------------------------------------------
009200101202     D CBITX_GetDescrizioneProvider...
009300101123     D                 PR            20A   VARYING
009400101123
009500101123     D*--------------------------------------------------
009600101202     D* Procedure name: CBITX_Mailto
009700101123     D* Purpose:        Invia una e-mail agli utenti interessati.
009800101123     D* Returns:        Esito.
009900101123     D* Parameter:      piSubject => Oggetto.
010000101123     D* Parameter:      piBody => Corpo del messaggio.
010100101123     D* Parameter:      piBodySize => Dimensione corpo del messaggio.
010200101123     D*--------------------------------------------------
010300101202     D CBITX_Mailto    PR            10I 0
010400101123     D  piSubject                      *   VALUE OPTIONS(*STRING:*TRIM)
010500101123     D  piBody                         *   VALUE OPTIONS(*STRING:*TRIM)
010600101123
010700101124     D*--------------------------------------------------
010800101202     D* Procedure name: CBITX_GetMessaggioErrore
010900101124     D* Purpose:        Restituisce il messaggio dell'ultimo errore.
011000101124     D* Returns:        Messaggio di errore.
011100101124     D*--------------------------------------------------
011200101202     D CBITX_GetMessaggioErrore...
011300101124     D                 PR           255A   VARYING
011400101124
011500101203     D*--------------------------------------------------
011600101209     D* Procedure name: CBITX_GetProviderByDistinta
011700101209     D* Purpose:        Restituisce il provider di una distinta.
011800101209     D* Returns:        Provider.
011900101203     D* Parameter:      piSocieta => Societa
012000101203     D* Parameter:      piDataDistinta => Data distinta.
012100101203     D* Parameter:      piNumeroDistinta => Numero distinta.
012200101203     D*--------------------------------------------------
012300101209     D CBITX_GetProviderByDistinta...
012400101209     D                 PR            15A
012500101203     D  piSocieta                          LIKE(andiz00f.societa)
012600101203     D piDataDistinta                  D   DATFMT(*ISO)
012700101203     D  piNumeroDistinta...
012800101203     D                                     LIKE(andiz00f.nr)
012900101203
013000101220     D*--------------------------------------------------
013100101220     D* Procedure name: CBITX_GetSiaBySocieta
013200101220     D* Purpose:        Restituisce il codice SIA di una società.
013300101220     D* Returns:        Codice SIA società.
013400101220     D* Parameter:      piSocieta => Società.
013500101220     D*--------------------------------------------------
013600101220     D CBITX_GetSiaBySocieta...
013700101220     D                 PR                  LIKE(andiz00f.cdSiaSi)
013800101220     D  piSocieta                          LIKE(andiz00f.societa)
013900101220
014000101220     D*--------------------------------------------------
014100101220     D* Procedure name: CBITX_GetProviderBySia
014200101220     D* Purpose:        Restituisce il provider di un codice SIA.
014300101220     D* Returns:        Provider.
014400101220     D* Parameter:      piSia => Codice SIA.
014500101220     D*--------------------------------------------------
014600101220     D CBITX_GetProviderBySia...
014700101220     D                 PR            15A
014800101220     D  piSia                              LIKE(andiz00f.cdSiaSi)
014900101220
015000101220     D*--------------------------------------------------
015100101220     D* Procedure name: CBITX_GetProviderBySocieta
015200101220     D* Purpose:        Restituisce il provider di una società.
015300101220     D* Returns:        Provider.
015400101220     D* Parameter:      piSocieta => Società.
015500101220     D*--------------------------------------------------
015600101220     D CBITX_GetProviderBySocieta...
015700101220     D                 PR            15A
015800101220     D  piSocieta                          LIKE(andiz00f.societa)
015900101220
016000101220     D*--------------------------------------------------
016100101220     D* Procedure name: CBITX_GetTipoDistinta
016200101220     D* Purpose:        Restituisce il tipo distinta.
016300101220     D* Returns:        Tipo distinta.
016400101220     D*--------------------------------------------------
016500101220     D CBITX_GetTipoDistinta...
016600101220     D                 PR             2A
016700101220
016800101222     D*--------------------------------------------------
016900101222     D* Procedure name: CBITX_NewCopiaFlusso
017000101222     D* Purpose:        Inizio copia flusso.
017100101222     D* Returns:        Esito.
017200101222     D*--------------------------------------------------
017300101222     D CBITX_NewCopiaFlusso...
017400101222     D                 PR            10I 0
017500101222     D piTipoDistinta                 1A   CONST
017600101222
017700101222     D*--------------------------------------------------
017800101222     D* Procedure name: CBITX_EndCopiaFlusso
017900101222     D* Purpose:        Fine copia flusso.
018000101222     D* Returns:        Esito.
018100101222     D*--------------------------------------------------
018200101222     D CBITX_EndCopiaFlusso...
018300101222     D                 PR            10I 0
018400101222
018500101222     D*--------------------------------------------------
018600101222     D* Procedure name: CBITX_NewSocieta
018700101222     D* Purpose:        Nuova società.
018800101222     D* Returns:        Esito.
018900101222     D* Parameter:      piSocieta => Società.
019000101222     D*--------------------------------------------------
019100101222     D CBITX_NewSocieta...
019200101222     D                 PR            10I 0
019300101222     D  piSocieta                          LIKE(andiz00f.societa)
019400101222     D piTipoDistinta                 1A   CONST
019700101222
019800101222     D*--------------------------------------------------
019900101222     D* Procedure name: CBITX_EndSocieta
020000101222     D* Purpose:        Fine società.
020100101222     D* Returns:        Esito.
020200101222     D*--------------------------------------------------
020300101222     D CBITX_EndSocieta...
020400101222     D                 PR            10I 0
020601101222
020602101222     D*--------------------------------------------------
020603101222     D* Procedure name: CBITX_GetCountRigheDb2Mbr
020604101222     D* Purpose:        Restituisce il conteggio righe del membro DB2 del p...
020605101222     D*                          rovider.
020606101222     D* Returns:        Conteggio righe membro DB2.
020607101222     D*--------------------------------------------------
020608101222     D CBITX_GetCountRigheDb2Mbr...
020609101222     D                 PR            10I 0
020700101222
020701101223     D*--------------------------------------------------
020702101223     D* Procedure name: CBITX_CpyDb2MbrToStreamFile
020703101223     D* Purpose:        Copia i dati dal memnro DB2 al file di flusso.
020704101223     D* Returns:        Esito.
020705101223     D* Parameter:      piTipoDistinta => Tipo distinta.
020706101223     D*--------------------------------------------------
020707101223     D CBITX_CpyDb2MbrToStreamFile...
020708101223     D                 PR            10I 0
020709101223     D piTipoDistinta                 1A   CONST
020710101223
