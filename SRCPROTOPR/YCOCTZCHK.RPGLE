000100130328     D*--------------------------------------------------
000200130328     D* Procedure name: YCOCTZCHK_Init
000300130328     D* Purpose:        Inizializza il modulo.
000400130328     D* Returns:        Esito.
000500130328     D* Parameter:      priKpjba
000600130328     D*--------------------------------------------------
000700130328     D YCOCTZCHK_Init  PR            10I 0
000800130328     D  priKpjba                           LIKEDS(kpjba)
000900130328
001000130328     D*--------------------------------------------------
001100130328     D* Procedure name: YCOCTZCHK_Finalize
001200130328     D* Purpose:        Chiude il modulo.
001300130328     D* Returns:
001400130328     D*--------------------------------------------------
001500130328     D YCOCTZCHK_Finalize...
001600130328     D                 PR
001700130328
001800130328     D*--------------------------------------------------
001900130328     D* Procedure name: YCOCTZCHK_IsClienteInContenzioso
002000130328     D* Purpose:        Restituisce *ON se il cliente ha un contenzioso aperto.
002100130328     D* Returns:        *ON = cliente con contenzioso aperto.
002200130328     D* Parameter:      priIdCliente
002300130328     D* Parameter:      priPartitaIva
002400130328     D* Parameter:      priCodiceFiscale
002500130328     D*--------------------------------------------------
002600130328     D YCOCTZCHK_IsClienteInContenzioso...
002700130328     D                 PR              N
002800130328     D  priIdSocieta                  3A   CONST
002900130328     D  priIdCliente                  8A   CONST
003000130328     D  priPartitaIva                20A   CONST
003100130328     D  priCodiceFiscale...
003200130328     D                               20A   CONST
003300130328
