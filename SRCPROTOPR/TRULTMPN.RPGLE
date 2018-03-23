000100130122     D*--------------------------------------------------
000200130122     D* Procedure name: trulTmpN
000300130123     D* Purpose:        Genera un nome oggetto temporaneo inesistente in
000400130123     D*                 GAIITRATMP.
000500130122     D* Returns:
000600130122     D* Parameter:      priTipoOggetto => Tipo oggetto.
000700130123     D* Parameter:      priPrefissoNomeOggetto => Una lettera A-Z da mettere
000800130123     D*                 nella prima posizione del nome.
000900130123     D*                 Se passato ' ' viene assunto 'F' per *FILE,
001000130123     D*                 'A' per *DTAARA, 'S' per *USRSPC, 'L' per *LIB
001100130123     D*                 e 'Z' per altri tipi.
001200130123     D* Parameter:      priLibreria  => *BLANK = 'GAITRATMP'.
001300130123     D* Parameter:      priNomeOggetto  => output.
001400130123     D*                 Se l'elaborazione termina in modo anomalo ritorna
001500130123     D*                 un '*' seguito dal ID errore (p.e. '*CPF9810').
001600130122     D*--------------------------------------------------
001700130123     D TrulTmpN        PR                  EXTPGM('TRULTMPN')
001800130123     D  priTipoOggetto...
001900130123     D                               10A   CONST
002000130123     D  priPrefissoNomeOggetto...
002100130123     D                                1A   CONST
002200130123     D  priLibreria...
002300130123     D                               10A
002400130123     D  priNomeOggetto...
002500130123     D                               10A
