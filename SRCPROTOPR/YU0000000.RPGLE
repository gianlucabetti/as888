000100071001
000200071001     D*--------------------------------------------------
000300071107     D* Procedure name: Fusione_newRicercaSocietaIncorporata
000400071001     D* Purpose:        Inizializza il modulo per una nuova ricerca delle s...
000500071001     D*                          ocietà incorporate.
000600071001     D* Returns:        Esito.
000700071001     D* Parameter:      piSocietaIncorporante => Società incorporante.
000800071001     D*--------------------------------------------------
000900071107     D Fusione_newRicercaSocietaIncorporata...
001000071001     D                 PR            10I 0
001100071001     D  piSocietaIncorporante...
001200071002     D                                3A   OPTIONS(*NOPASS)
001300071001
001400071001     D*--------------------------------------------------
001500071107     D* Procedure name: Fusione_getSocietaIncorporata
001600071001     D* Purpose:        Restituisce una società incorporata.
001700071001     D* Returns:        Esito.
001800071001     D* Parameter:      piSocietaIncorporata => Società incorporata.
001900071001     D*--------------------------------------------------
002000071107     D Fusione_getSocietaIncorporata...
002100071001     D                 PR            10I 0
002200071001     D  piSocietaIncorporata...
002300071001     D                                3A
002400071001
002500071001     D*--------------------------------------------------
002600071107     D* Procedure name: Fusione_getSocietaIncorporante
002700071001     D* Purpose:        Restituisce la società incorporante.
002800071001     D* Returns:        Esito.
002900071001     D* Parameter:      piSocietaIncorporata => Società incorporata.
003000071001     D* Parameter:      piSocietaIncorporante => Società incorporante.
003100071001     D*--------------------------------------------------
003200071107     D Fusione_getSocietaIncorporante...
003300071001     D                 PR            10I 0
003400071001     D  piSocietaIncorporata...
003500071001     D                                3A
003600071001     D  piSocietaIncorporante...
003700071001     D                                3A
003800071001
