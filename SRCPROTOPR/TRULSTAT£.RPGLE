000100161011      /IF NOT DEFINED(DIR_idxDirObj)
000200161011     D DIR_idxDirObj...
000300110623     D                 DS                  QUALIFIED INZ
000400110623     D  dirIdx                       10I 0                                      Parent dir index
000500110623     D  dirNam                      512A   VARYING                              Parent dir path
000600110623     D  objNam                      256A   VARYING                              Object name
000700110623      /ENDIF
000800110623
000900110623     D*--------------------------------------------------
001000161011     D* Procedure name: DIR_NewRichiamo
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
004900161011     D DIR_NewRichiamo...
005000110623     D                 PR            10I 0
005100110623     D  priIndirizzario...
005200110623     D                              512A   VARYING
005300110623     D                                     CONST
005301161012     D  piPrefisso...
005302161012     D                               50    VARYING
005303161012     D                                     CONST
005304161012     D  piEstensione...
005305161012     D                               50    VARYING
005306161012     D                                     CONST
005307161012     D*--------------------------------------------------
009800161011     D* Procedure name: DIR_GetNextObjectName
009900110623     D* Purpose:        Restituisce il prossimo nome oggetto.
010000110623     D* Returns:        Parent directory index, parent directory path e obj...
010100110623     D*                          ect name.
010200110623     D*--------------------------------------------------
010300161011     D DIR_GetNextObjectName...
010400110623     D                 PR            10I 0
010500161012     D FileName                     512    varying
010600110623
010700110623     D*--------------------------------------------------
010800161011     D* Procedure name: DIR_EndRichiamo
010900110623     D* Purpose:        Fine richiamo.
011000110623     D* Returns:        Esito.
011100110623     D*--------------------------------------------------
011200161011     D DIR_EndRichiamo...
011300110623     D                 PR            10I 0
011400110623
