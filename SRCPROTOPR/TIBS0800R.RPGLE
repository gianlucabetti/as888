000100090526     D*--------------------------------------------------
000200090526     D* Procedure name: inzLingue
000300090526     D* Purpose:        Inizializza schiere lingue.
000400090526     D* Returns:        Numero elementi reperiti.
000500090526     D* Parameter:      rpyOpCode => Codice risposta.
000600090526     D* Parameter:      rpyEsito => Esito.
000700090526     D* Parameter:      rpyElementi => Numero elementi reperiti.
000800090526     D*--------------------------------------------------
000900090526     D inzLingue       PR            10I 0
001000090526     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
001100090526     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
001200090526     D  rpyElementi                  10I 0 OPTIONS(*NOPASS:*OMIT)
001300090526
001400090526     D*--------------------------------------------------
001500090526     D* Procedure name: cvtLinguaISO2ToTabel
001600090526     D* Purpose:        Conversione codice lingua da ISO2 a TABEL00F
001700090526     D* Returns:        Codice lingua TABEL00F
001800090526     D* Parameter:      rqsISO2 => Codice lingua ISO2
001900090526     D* Parameter:      rpyOpCode => Codice risposta.
002000090526     D* Parameter:      rpyEsito => Esito
002100090526     D* Parameter:      rpyTabel => Codice lingua TABEL00F
002200090526     D*--------------------------------------------------
002300090526     D cvtLinguaISO2ToTabel...
002400091028     D                 PR             1P 0
002500091028     D  rqsISO2                       2A
002600090526     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
002700090526     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
002800091028     D  rpyTabel                      1P 0 OPTIONS(*NOPASS:*OMIT)
002900090526
003000090526     D*--------------------------------------------------
003100090526     D* Procedure name: cvtLinguaISO2ToTntbe
003200090526     D* Purpose:        Conversione codice lingua da ISO2 a TNTBE00F.
003300090526     D* Returns:        Codice lingua TNTBE00F.
003400090526     D* Parameter:      rqsISO2 => Codice lingua ISO2
003500090526     D* Parameter:      rpyOpCode => Codice risposta.
003600090526     D* Parameter:      rpyEsito => Esito.
003700090526     D* Parameter:      rpyTntbe => Codice lingua TNTBE00F
003800090526     D*--------------------------------------------------
003900090526     D cvtLinguaISO2ToTntbe...
004000091028     D                 PR             1A
004100091028     D  rqsISO2                       2A
004200090526     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
004300090526     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
004400091028     D  rpyTntbe                      1A   OPTIONS(*NOPASS:*OMIT)
004500091028
004600091028     D*--------------------------------------------------
004700091028     D* Procedure name: cvtLinguaISO3ToISO2
004800091028     D* Purpose:        Conversione codice lingua da ISO3 a ISO2.
004900091028     D* Returns:        Codice lingua ISO2.
005000091028     D* Parameter:      rqsISO3 => Codice lingua ISO3
005100091028     D* Parameter:      rpyOpCode => Codice risposta.
005200091028     D* Parameter:      rpyEsito => Esito.
005300091028     D* Parameter:      rpyISO2 => Codice lingua ISO2
005400091028     D*--------------------------------------------------
005500091028     D cvtLinguaISO3ToISO2...
005600091028     D                 PR             2A
005700091028     D  rqsISO3                       3A
005800091028     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
005900091028     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
006000091028     D  rpyISO2                       2A   OPTIONS(*NOPASS:*OMIT)
006100090526
