000100110120     D*--------------------------------------------------
000200110120     D* Procedure name: YHCBKAMM_Init
000300110120     D* Purpose:        Inizializza il modulo.
000400110120     D* Returns:        Esito.
000500110120     D* Parameter:      priKpjba
000600110120     D*--------------------------------------------------
000700110120     D YHCBKAMM_Init   PR            10I 0
000800110120     D  priKpjba                           LIKE(kpjba)
000900110120     D  priTipoElaborazione...
001000110120     D                                1A   CONST
001100110120
001200110120     D*--------------------------------------------------
001300110120     D* Procedure name: YHCBKAMM_Finalize
001400110120     D* Purpose:        Chiusura modulo.
001500110120     D* Returns:
001600110120     D*--------------------------------------------------
001700110120     D YHCBKAMM_Finalize...
001800110120     D                 PR
001900110120
002000110120     D*--------------------------------------------------
002100110120     D* Procedure name: YHCBKAMM_NewContrassegni
002200110120     D* Purpose:        Prepara una nuova lista di contrassegni incassati e...
002300110120     D*                           non pagati di clienti bloccati.
002400110120     D* Returns:        Esito.
002500110120     D*--------------------------------------------------
002600110120     D YHCBKAMM_NewContrassegni...
002700110120     D                 PR            10I 0
002800110120
002900110120     D*--------------------------------------------------
003000110120     D* Procedure name: YHCBKAMM_EndContrassegni
003100110120     D* Purpose:        Chiudo la lista dei contrassegni.
003200110120     D* Returns:        Esito.
003300110120     D*--------------------------------------------------
003400110120     D YHCBKAMM_EndContrassegni...
003500110120     D                 PR            10I 0
003600110120
003700110120     D*--------------------------------------------------
003800110120     D* Procedure name: YHCBKAMM_GetNextIdContrassegnoFromContrassegni
003900110120     D* Purpose:        Restituisce l'ID contrassegno dalla lista dei contr...
004000110120     D*                          assegni.
004100110120     D* Returns:        Esito.
004200110120     D*--------------------------------------------------
004300110120     D YHCBKAMM_GetNextIdContrassegnoFromContrassegni...
004400110120     D                 PR            10I 0
004500110120     D  priAnno                            LIKE(cndizion.$aas)
004600110120     D  priFiliale                         LIKE(cndizion.$lnp)
004700110120     D  priSerie                           LIKE(cndizion.$nrs)
004800110120     D  priNumero                          LIKE(cndizion.$nsp)
004900110120
005000110120     D*--------------------------------------------------
005100110120     D* Procedure name: YHCBKAMM_NewContrassegno
005200110120     D* Purpose:        Nuovo contrassegno.
005300110120     D* Returns:        Esito.
005400110120     D*--------------------------------------------------
005500110120     D YHCBKAMM_NewContrassegno...
005600110120     D                 PR            10I 0
005700110120     D  priAnno                            LIKE(cndizion.$aas) CONST
005800110120     D  priFiliale                         LIKE(cndizion.$lnp) CONST
005900110120     D  priSerie                           LIKE(cndizion.$nrs) CONST
006000110120     D  priNumero                          LIKE(cndizion.$nsp) CONST
006100110120
006200110120     D*--------------------------------------------------
006300110120     D* Procedure name: YHCBKAMM_EndContrassegno
006400110120     D* Purpose:        Fine contrassegno.
006500110120     D* Returns:        Esito.
006600110120     D* Parameter:      priCommit: *ON = commit; *OFF = rollback.
006700110120     D*--------------------------------------------------
006800110120     D YHCBKAMM_EndContrassegno...
006900110120     D                 PR            10I 0
007000110120     D priCommit                       N   CONST
007100110120
007200110120     D*--------------------------------------------------
007300110120     D* Procedure name: YHCBKAMM_Compensazione
007400110120     D* Purpose:        Esegue la compensazione di un contrassegno.
007500110120     D* Returns:        Esito.
007600110120     D*--------------------------------------------------
007700110120     D YHCBKAMM_Compensazione...
007800110120     D                 PR            10I 0
007900110124     D priRegistrazionePagamentoData...
008000110124     D                                 D
008100110124     D priRegistrazionePagamentoNumero...
008200110124     D                                     LIKE(andiz00f.nr)
008300110124     D priRegistrazionePagamentoSerie...
008400110124     D                                     LIKE(andiz00f.serieNm)
008500110124     D priRegistrazionePagamentoErrata...
008600110124     D                                 N
008700110124
008800110121     D*--------------------------------------------------
008900110121     D* Procedure name: YHCBKAMM_GetInfoContrassegno
009000110121     D* Purpose:        Restituisce varie informazioni del contrassegno.
009100110121     D* Returns:        Esito.
009200110121     D* Parameter:      priImporto => Importo contrassegno.
009300110121     D* Parameter:      priDivisa => Divisa contrassegno.
009400110121     D* Parameter:      priClienteAssegno => Cliente contrassegno.
009500110121     D* Parameter:      priClienteFattura => Cliente fattura.
009600110121     D* Parameter:      priRagioneSocialeCliente => Ragione sociale del cli...
009700110121     D*                          ente.
009800110124     D* Parameter:      prtStatoDelCredito => Blocco amministra...
009900110121     D*                          tivo del cliente.
010000110121     D*--------------------------------------------------
010100110121     D YHCBKAMM_GetInfoContrassegno...
010200110121     D                 PR            10I 0
010300110121     D  priImporto                         LIKE(cndizion.$cas)
010400110121     D  priDivisa                          LIKE(cndizion.$vca)
010500110121     D  priClienteAssegno...
010600110121     D                                     LIKE(cndizion.$ksc)
010700110121     D  priClienteFattura...
010800110121     D                                     LIKE(andiz00f.ksc)
010900110121     D  priRagioneSocialeCliente...
011000110121     D                                     LIKE(andiz00f.descr)
011100110124     D  prtStatoDelCredito...
011200110124     D                                     LIKE(andiz00f.ctgAna)
011300110121
