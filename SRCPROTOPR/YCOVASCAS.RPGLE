000100110307     D*--------------------------------------------------
000200110307     D* Procedure name: VASCAS_Init
000300110307     D* Purpose:        Inizializza il modulo.
000400110307     D* Returns:        Esito.
000500110307     D* Parameter:      priAvviareControlloCommit => *ON = Avviare il contr...
000600110307     D*                          ollo commit.
000700110307     D* Parameter:      priKpjba
000800110307     D*--------------------------------------------------
000900110307     D VASCAS_Init     PR            10I 0
001000110307     D  priAvviareControlloCommit...
001100110307     D                                 N   CONST
001200110307     D  priProgrammaChiamante...
001300110307     D                               10A   CONST
001400110307     D  priKpjba                           LIKEDS(kpjba)
001500110307     D                                     OPTIONS(*NOPASS)
001600110307
001700110307     D*--------------------------------------------------
001800110307     D* Procedure name: VASCAS_Finalize
001900110307     D* Purpose:        Chiude il modulo.
002000110307     D* Returns:
002100110307     D*--------------------------------------------------
002200110307     DVASCAS_Finalize  PR
002300110307
002400110307     D*--------------------------------------------------
002500110307     D* Procedure name: VASCAS_NewFlusso
002600110307     D* Purpose:        Inizio nuovo flusso.
002700110307     D* Returns:        Esito.
002800110307     D* Parameter:      priAggiornareConCommit => *ON = Aggiornare il datab...
002900110307     D*                          ase col commit.
003000110307     D* Parameter:      priRigheFlusso => Numero delle righe che comporrann...
003100110307     D*                          o il flusso (se conosciute).
003200110307     D*--------------------------------------------------
003300110307     D VASCAS_NewFlusso...
003400110307     D                 PR            10I 0
003500110307     D  priAggiornareConCommit...
003600110307     D                                 N   CONST
003700110307     D priRigheFlusso                10U 0 OPTIONS(*NOPASS)
003800110307
003900110307     D*--------------------------------------------------
004000110307     D* Procedure name: VASCAS_EndFlusso
004100110307     D* Purpose:        Fine del flusso.
004200110307     D* Returns:        Esito.
004300110307     D* Parameter:      priCommit => *ON = Commit; *OFF = Rollback.
004400110307     D*--------------------------------------------------
004500110307     D VASCAS_EndFlusso...
004600110307     D                 PR            10I 0
004700110307     D  priCommit...
004800110307     D                                 N   CONST
004900110307     D                                     OPTIONS(*NOPASS)
005000110307
005100110307     D*--------------------------------------------------
005200110307     D* Procedure name: VASCAS_NewContrassegno
005300110307     D* Purpose:        Nuovo contrassegno.
005400110307     D*                 Prima di chiamare questa sottoprocedura il contrassegno
005500110307     D*                 deve essere già aggiornato.
005600110307     D* Returns:        Esito.
005700110307     D* Parameter:      priAnnoBolla => Anno bolla.
005800110307     D* Parameter:      priFilialeBolla => Filiale bolla.
005900110307     D* Parameter:      priSerieBolla => Serie bolla.
006000110307     D* Parameter:      priNumeroBolla => Numero bolla.
006100110307     D*--------------------------------------------------
006200110307     D VASCAS_NewContrassegno...
006300110307     D                 PR            10I 0
006400110307     D  priAnnoBolla                       LIKE(cndizion.$aas)
006500110307     D  priFilialeBolla...
006600110307     D                                     LIKE(cndizion.$lnp)
006700110307     D  priSerieBolla                      LIKE(cndizion.$nrs)
006800110307     D priNumeroBolla                      LIKE(cndizion.$nsp)
006900110307
007000110307     D*--------------------------------------------------
007100110307     D* Procedure name: VASCAS_EndContrassegno
007200110307     D* Purpose:        Fine elaborazione contrassegno.
007300110307     D* Returns:        Esito.
007400110307     D* Parameter:      priCommit => *ON = Commit; *OFF = Rollback.
007500110307     D*--------------------------------------------------
007600110307     D VASCAS_EndContrassegno...
007700110307     D                 PR            10I 0
007800110307     D  priCommit                      N   CONST
007900110307     D                                     OPTIONS(*NOPASS)
008000110307
