000100080722     D*--------------------------------------------------
000200080722     D* Procedure name: FatturaManuale_ctb_init
000300080722     D* Purpose:        Inizializza il modulo.
000400080722     D* Returns:        Esito.
000500080722     D* Parameter:      piKpjba
000600080722     D*--------------------------------------------------
000700080722     D FatturaManuale_ctb_init...
000800080722     D                 PR            10I 0
000900080722     D  piKpjba                            LIKEDS(kpjba)
001000080722
001100080722     D*--------------------------------------------------
001200080722     D* Procedure name: FatturaManuale_ctb_finalize
001300080722     D* Purpose:        Chiude il modulo.
001400080722     D* Returns:
001500080722     D*--------------------------------------------------
001600080722     D FatturaManuale_ctb_finalize...
001700080722     D                 PR
001800080722
001900080722     D*--------------------------------------------------
002000080722     D* Procedure name: FatturaManuale_ctb_newFattura
002100080722     D* Purpose:        Inizia la registrazione di una fattura.
002200080722     D* Returns:        Esito.
002300080722     D* Parameter:      piNumeroRichiesta => Numero richiesta.
002400080722     D*--------------------------------------------------
002500080722     D FatturaManuale_ctb_newFattura...
002600080722     D                 PR            10I 0
002700080722     D  piNumeroRichiesta...
002800080722     D                               10I 0 CONST
002900080722
003000080722     D*--------------------------------------------------
003100080722     D* Procedure name: FatturaManuale_ctb_endFattura
003200080722     D* Purpose:        Chiude la registrazione della fattura.
003300080722     D* Returns:        Esito.
003600080730     D* Parameter:      piPrimaNotaProj => *ON = chiamare la prima nota di ...
003700080730     D*                          Proj.
003800080722     D*--------------------------------------------------
003900080722     D FatturaManuale_ctb_endFattura...
004000080722     D                 PR            10I 0
004400080730     D  piPrimaNotaProj...
004500080730     D                                 N   CONST
004600080722
004700080722     D*--------------------------------------------------
004800080722     D* Procedure name: FatturaManuale_ctb_newRegistrazione
004900080722     D* Purpose:        Prepara la registrazione della fattura.
005000080722     D* Returns:        Esito.
005100080722     D*--------------------------------------------------
005200080722     D FatturaManuale_ctb_newRegistrazione...
005300080722     D                 PR            10I 0
005400080722
005500080723     D*--------------------------------------------------
005600080723     D* Procedure name: FatturaManuale_ctb_endRegistrazione
005700080723     D* Purpose:        Chiude la registrazione.
005800080723     D* Returns:        Esito.
005900080723     D* Parameter:      piCommitRollback => *ON = commit; *OFF = rollback; ...
006000080730     D*                          *NOPASS = nulla.
006100080723     D*--------------------------------------------------
006200080723     D FatturaManuale_ctb_endRegistrazione...
006300080723     D                 PR            10I 0
006400080723     D  piCommitRollback...
006500080723     D                                 N   CONST
006600080730     D                                     OPTIONS(*NOPASS)
006700080723
