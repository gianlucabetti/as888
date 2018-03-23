000100090914     D*--------------------------------------------------
000200090914     D* Procedure name: DBCTBPJ_Init
000300090914     D* Purpose:        Inizializzazione modulo.
000400090914     D* Returns:        Esito.
000500090914     D* Parameter:      piKpjba
000600090914     D*--------------------------------------------------
000700090914     D DBCTBPJ_Init    PR            10I 0
000800090914     D  piKpjba                            LIKEDS(kpjba)
000900090914     D                                     CONST
001000090914
001100090914     D*--------------------------------------------------
001200090914     D* Procedure name: DBCTBPJ_ChkParametro_libreria
001300090911     D* Purpose:        Controlla il nome e la descrizione della libreria.
001400090911     D* Returns:        Esito.
001500090911     D* Parameter:      piNomeLibreria => Nome libreria.
001600090911     D* Parameter:      piDescrizioneLibreria => Descrizione libreria.
001700090911     D*--------------------------------------------------
001800090914     D DBCTBPJ_ChkParametro_libreria...
001900090911     D                 PR            10I 0
002000090911     D piNomeLibreria                10A
002100090911     D  piDescrizioneLibreria...
002200090911     D                               50A
002300090911
002400090911     D*--------------------------------------------------
002500090914     D* Procedure name: DBCTBPJ_ChkParametro_dataLimite
002600090911     D* Purpose:        Controlla la data limite.
002700090911     D* Returns:        Esito.
002800090911     D* Parameter:      piDataLimite => Data limite.
002900090911     D*--------------------------------------------------
003000090914     D DBCTBPJ_ChkParametro_dataLimite...
003100090911     D                 PR            10I 0
003200090911     D  piSocieta                          LIKE(andiz00f.societa)
003300090911     D  piDataLimite                   D   DATFMT(*ISO)
003400090911
003500090911     D*--------------------------------------------------
003600090914     D* Procedure name: DBCTBPJ_ChkParametro_societaCopia
003700090911     D* Purpose:        Controlla le società da copiare.
003800090911     D* Returns:        Esito.
003900090911     D* Parameter:      piSocieta => Societa.
004000090911     D* Parameter:      piListaSocieta => Lista di societa.
004100090911     D*--------------------------------------------------
004200090914     D DBCTBPJ_ChkParametro_societaCopia...
004300090911     D                 PR            10I 0
004400090914     D  piSocieta                          LIKE(andiz00f.societa)
004500090911     D                                     CONST
004600090911     D piListaSocieta                 8A   CONST
004700090915     D  piDescrizione...
004800090915     D                               50A   OPTIONS(*NOPASS)
004900090911
