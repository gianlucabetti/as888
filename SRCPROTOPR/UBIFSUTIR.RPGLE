000100161017     P*--------------------------------------------------
000200161017     P* Procedure name: UBIFSUTIR_GetFileNameElements
000300161017     P* Purpose:        Riconosce e separa gli elementi che compongono il percorso e/o nome file
000400161017     P* Returns:        Integer (Status_Code)
000500161017     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
000600161017     P* Parameter:      oIFSUTIpath     => Percorso file
000700161017     P* Parameter:      oIFSUTIfullName => Nome file completo (nome + estensione)
000800161017     P* Parameter:      oIFSUTIName     => Nome file no estensione
000900161017     P* Parameter:      oIFSUTIExt      => Estensione
001000161017     P*--------------------------------------------------
001100161017     D UBIFSUTIR_GetFileNameElements...
001200161021     D                 PR            10I 0
001300161017     DiIFSUTIfile                  1024A   CONST
001400161017     DoIFSUTIpath                   512A
001500161017     DoIFSUTIfullName               512A
001600161017     DoIFSUTIName                   512A
001700161017     DoIFSUTIExt                     32A
001800151126     D*
001900161017     P*--------------------------------------------------
002000161019     P* Procedure name: UBIFSUTIR_GetFileSizeByte
002100161019     P* Purpose:        Reperisce la dimensione di un file in Byte
002200161017     P* Returns:        Integer (Status_Code)
002300161017     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
002400161019     P* Parameter:      oIFSUTIsizeByte => Dimensione file in Byte
002500161017     P*--------------------------------------------------
002600161017     P*
002700161019     D UBIFSUTIR_GetFileSizeByte...
002800161021     D                 PR            10I 0
002900161017     DiIFSUTIfile                  1024A   CONST
003000161019     DoIFSUTIsizeByte                10I 0
003100161017     D*
003200161021     P*--------------------------------------------------
003300161021     P* Procedure name: UBIFSUTIR_GetFileEncoding
003400161021     P* Purpose:        Reperisce la CodePage e il CCSID
003500161021     P* Returns:        Integer (Status_Code)
003600161021     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
003700161021     P* Parameter:      oIFSUTIcodePage => CodePage del file
003800161021     P* Parameter:      oIFSUTI_CCSID   => CCSID del file
003900161021     P*--------------------------------------------------
004000161021     P*
004100161021     D UBIFSUTIR_GetFileEncoding...
004200161021     D                 PR            10I 0
004300161021     DiIFSUTIfile                  1024A   CONST
004400161021     DoIFSUTIcodePage                 5U 0
004500161021     DoIFSUTI_CCSID                   5U 0
004600161021     D*
004700161018     P*--------------------------------------------------
004800161018     P* Procedure name: UBIFSUTIR_ReadFileUpTo64K
004900161026     P* Purpose:        Legge "one-shot" intero file (dimensioni max 64KB)
005000161018     P* Returns:        Integer (Status_Code)
005100161018     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
005200161018     P* Parameter:      iIFSUTIsizeByte => Dimensione file attesa
005300161028     P* Parameter:      oIFSUTIsizeByte => Dimensione file rilevata
005400161018     P* Parameter:      oIFSUTIdata64K  => Contenuto del file
005500161018     P*--------------------------------------------------
005600161018     D*
005700161018     D UBIFSUTIR_ReadFileUpTo64K...
005800161021     D                 PR            10I 0
005900161018     DiIFSUTIfile                  1024A   CONST
006000161018     DiIFSUTIsizeByte                10I 0 CONST
006100161028     DoIFSUTIsizeByte                10I 0
006200161030     DoIFSUTIdata64K                   A   LEN(65536) VARYING OPTIONS(*VARSIZE)       (4 Byte x Len)
006300161018     D*
006400170518     P*--------------------------------------------------
006500170518     P* Procedure name: UBIFSUTIR_ReadFileUpTo64Ktext
006600170518     P* Purpose:        Legge "one-shot" intero file (dimensioni max 64KB) in modalità testuale
006700170518     P* Returns:        Integer (Status_Code)
006800170518     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
006900170518     P* Parameter:      iIFSUTIsizeByte => Dimensione file attesa
007000170518     P* Parameter:      oIFSUTIsizeByte => Dimensione file rilevata
007100170518     P* Parameter:      oIFSUTIdata64K  => Contenuto del file
007200170518     P*--------------------------------------------------
007300170518     D*
007400170518     D UBIFSUTIR_ReadFileUpTo64Ktext...
007500170518     D                 PR            10I 0
007600170518     DiIFSUTIfile                  1024A   CONST
007700170518     DiIFSUTIsizeByte                10I 0 CONST
007800170518     DoIFSUTIsizeByte                10I 0
007900170518     DoIFSUTIdata64K                   A   LEN(65536) VARYING OPTIONS(*VARSIZE)       (4 Byte x Len)
008000170518     D*
008100170516     P*--------------------------------------------------
008200170518     P* Procedure name: UBIFSUTIR_ReadFileUpTo4MBparm
008300170516     P* Purpose:        Legge "one-shot" intero file (dimensioni max 4MB)
008400170516     P* Returns:        Integer (Status_Code)
008500170516     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
008600170516     P* Parameter:      iIFSUTIsizeByte => Dimensione file attesa
008700170516     P* Parameter:      iIFSUTIoFlg     => Open Flags
008800170516     P* Parameter:      iIFSUTImode     => Open mode
008900170516     P* Parameter:      iIFSUTIcodePage => Open CodePage
009000170516     P* Parameter:      oIFSUTIsizeByte => Dimensione file rilevata
009100170516     P* Parameter:      oIFSUTIdata4MB  => Contenuto del file
009200170516     P*--------------------------------------------------
009300170516     D*
009400170518     D UBIFSUTIR_ReadFileUpTo4MBparm...
009500170516     D                 PR            10I 0
009600170516     DiIFSUTIfile                  1024A   CONST
009700170516     DiIFSUTIsizeByte                10I 0 CONST
009800170516     DiIFSUTIoFlg                    10I 0 CONST
009900170516     DiIFSUTImode                    10U 0 CONST
010000170516     DiIFSUTIcodePage                10U 0 CONST
010100170516     DoIFSUTIsizeByte                10I 0
010200170516     DoIFSUTIdata4MB                   A   LEN(4194304) VARYING                       (4 Byte x Len)
010300170516     D                                     OPTIONS(*VARSIZE)
010400170516     D*
010500170516     P*--------------------------------------------------
010600170516     P* Procedure name: UBIFSUTIR_ReadFileUpTo4MBtext
010700170516     P* Purpose:        Legge "one-shot" intero file (dimensioni max 4MB) in modalità testuale
010800170516     P* Returns:        Integer (Status_Code)
010900170516     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
011000170516     P* Parameter:      iIFSUTIsizeByte => Dimensione file attesa
011100170516     P* Parameter:      oIFSUTIsizeByte => Dimensione file rilevata
011200170516     P* Parameter:      oIFSUTIdata4MB  => Contenuto del file
011300170516     P*--------------------------------------------------
011400170516     D*
011500170516     D UBIFSUTIR_ReadFileUpTo4MBtext...
011600170516     D                 PR            10I 0
011700170516     DiIFSUTIfile                  1024A   CONST
011800170516     DiIFSUTIsizeByte                10I 0 CONST
011900170516     DoIFSUTIsizeByte                10I 0
012000170516     DoIFSUTIdata4MB                   A   LEN(4194304) VARYING                       (4 Byte x Len)
012100170516     D                                     OPTIONS(*VARSIZE)
012200170516     D*
012300170516     P*--------------------------------------------------
012400170518     P* Procedure name: UBIFSUTIR_ReadFileUpTo4MB
012500170516     P* Purpose:        Legge "one-shot" intero file (dimensioni max 4MB) in modalità binaria
012600170516     P* Returns:        Integer (Status_Code)
012700170516     P* Parameter:      iIFSUTIfile     => Nome e/o percorso file
012800170516     P* Parameter:      iIFSUTIsizeByte => Dimensione file attesa
012900170516     P* Parameter:      oIFSUTIsizeByte => Dimensione file rilevata
013000170516     P* Parameter:      oIFSUTIdata4MB  => Contenuto del file
013100170516     P*--------------------------------------------------
013200170516     D*
013300170518     D UBIFSUTIR_ReadFileUpTo4MB...
013400170516     D                 PR            10I 0
013500170516     DiIFSUTIfile                  1024A   CONST
013600170516     DiIFSUTIsizeByte                10I 0 CONST
013700170516     DoIFSUTIsizeByte                10I 0
013800170516     DoIFSUTIdata4MB                   A   LEN(4194304) VARYING                       (4 Byte x Len)
013900170516     D                                     OPTIONS(*VARSIZE)
014000170516     D*
