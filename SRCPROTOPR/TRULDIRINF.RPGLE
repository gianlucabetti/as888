000100110623      /IF NOT DEFINED(DIRINF_idxDirObj)
000200110623     D DIRINF_idxDirObj...
000300110623     D                 DS                  QUALIFIED INZ
000400110623     D  dirIdx                       10I 0                                      Parent dir index
000500110623     D  dirNam                      512A   VARYING                              Parent dir path
000600110623     D  objNam                      256A   VARYING                              Object name
000700110623      /ENDIF
000800110623
000900110623     D*--------------------------------------------------
001000110623     D* Procedure name: DIRINF_NewRichiamo
001100110624     D* Purpose:        Prepara un nuovo richiamo informazioni indirizzario.
001200110623     D* Returns:        Esito.
001300110623     D* Parameter:      priIndirizzario
001400110623     D*                 Specifica il nome percorso dell'oggetto IFS (integrated
001500110623     D*                 file system) per cui saranno raccolte le informazioni.
001600110624     D* Parameter:      priNomeOggettoLike
001700110624     D*                 Nome oggetto da filtrare (p.e. '%.txt' per avere solo
001800110624     D*                 i files .txt).
001900110623     D* Parameter:      priAlberoSecondarioIndirizzari
002000110623     D*                 Specifica se raccogliere o meno informazioni
002100110623     D*                 sull'indirizzario per sottoindirizzari dell'indirizzario
002200110623     D*                 specificato dal parametro DIR.
002300110623     D*                 *ALL
002400110623     D*                 Verranno raccolte informazioni sull'indirizzario per
002500110623     D*                 sottoindirizzari dell'indirizzario specificato.
002600110623     D*                 *NONE
002700110623     D*                 Informazioni sull'indirizzario verranno raccolte solo
002800110623     D*                 per file nell'indirizzario specificato. Non verrà
002900110623     D*                 raccolta alcuna informazione per sottoindirizzari.
003000110623     D* Parameter:      priPrefissoFileInformazioni
003100110623     D*                 Specifica il prefisso nome file dei file di database dove
003200110623     D*                 si devono memorizzare le informazioni sull'indirizzario
003300110623     D*                 recuperate.
003400110623     D*                 *GEN
003500110623     D*                 I file di database verranno creati con un prefisso
003600110623     D*                 univoco generato da questo comando.  Il prefisso
003700110623     D*                 inizierà con QAEZD seguito da quattro cifre. I file
003800110623     D*                 creati per memorizzare le informazioni raccolte
003900110623     D*                 verranno denominati utilizzando questo prefisso
004000110623     D*                 seguito dalla lettera 'D' (per il file che contiene le
004100110623     D*                 informazioni sull'indirizzario) o dalla lettera 'O'
004200110623     D*                 (per il file che contiene le informazioni sugli
004300110623     D*                 oggetti negli indirizzari).
004400110623     D* Parameter:      priLibreriaInformazioni
004500110623     D*                 Specifica la libreria in cui verranno creati i file di
004600110623     D*                 database utilizzati per memorizzare le informazioni
004700110623     D*                 sull'indirizzario.
004800110623     D*--------------------------------------------------
004900110623     D DIRINF_NewRichiamo...
005000110623     D                 PR            10I 0
005100110623     D  priIndirizzario...
005200110623     D                              512A   VARYING
005300110623     D                                     CONST
005400170210     D  priAlberoSecondarioIndirizzari...
005500170210     D                                5A   CONST
005600170210     D                                     OPTIONS(*NOPASS:*OMIT)
005700170210     D  priPrefissoFileInformazioni...
005800170210     D                                9A   CONST
005900170210     D                                     OPTIONS(*NOPASS:*OMIT)
006000170210     D  priLibreriaInformazioni...
006100170210     D                               10A   CONST
006200170210     D                                     OPTIONS(*NOPASS)
006300110624
006400110624     D*--------------------------------------------------
006500110624     D* Procedure name: DIRINF_SetWhereObjectName
006600110624     D* Purpose:        Imposta la WHERE sul nome oggetto.
006700110624     D* Returns:        Esito.
006800110624     D* Parameter:      priWhereObjectNameSearchCondition => WHERE object name.
006900110624     D*                 search condition.
007000110624     D*--------------------------------------------------
007100110624     D DIRINF_SetWhereObjectName...
007200110624     D                 PR            10I 0
007300110624     D  priWhereObjectNameSearchCondition...
007400110624     D                              255A   VARYING
007500110624     D                                     CONST
007600110624
007700110624     D*--------------------------------------------------
007800110624     D* Procedure name: DIRINF_SetWhereObjectType
007900110624     D* Purpose:        Imposta la WHERE sul tipo oggetto (p.e. *STMF, *DIR)
008000110624     D* Returns:        Esito.
008100110624     D* Parameter:      priWherObjectTypeSearchCondition
008200110624     D*--------------------------------------------------
008300110624     D DIRINF_SetWhereObjectType...
008400110624     D                 PR            10I 0
008500110624     D  priWhereObjectTypeSearchCondition...
008600110624     D                              255A   VARYING
008700110624     D                                     CONST
008800110624
008900110624     D*--------------------------------------------------
009000110624     D* Procedure name: DIRINF_RunRichiamo
009100110624     D* Purpose:        Esegue un nuovo richiamo informazioni indirizzario.
009200110624     D* Returns:        Esito.
009300110624     D*--------------------------------------------------
009400110624     D DIRINF_RunRichiamo...
009500110624     D                 PR            10I 0
009600110623
009700110623     D*--------------------------------------------------
009800110623     D* Procedure name: DIRINF_GetNextObjectName
009900110623     D* Purpose:        Restituisce il prossimo nome oggetto.
010000110623     D* Returns:        Parent directory index, parent directory path e obj...
010100110623     D*                          ect name.
010200110623     D*--------------------------------------------------
010300110623     D DIRINF_GetNextObjectName...
010400110623     D                 PR            10I 0
010500110623     D priIdxDirObj                        LIKEDS(DIRINF_idxDirObj)
010600110623
010700110623     D*--------------------------------------------------
010800110623     D* Procedure name: DIRINF_EndRichiamo
010900110623     D* Purpose:        Fine richiamo.
011000110623     D* Returns:        Esito.
011100110623     D*--------------------------------------------------
011200110623     D DIRINF_EndRichiamo...
011300110623     D                 PR            10I 0
011400110623
