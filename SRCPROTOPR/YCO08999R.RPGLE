000100130212     D*--------------------------------------------------
000200130212     D* Procedure name: YCO089_Init
000300130212     D* Purpose:        Inizializzazione.
000400130212     D* Returns:        Esito.
000500130212     D* Parameter:      priKpjba
000600130218     D* Parameter:      priTipoElaborazione => Tipo elaborazione: '2' = pro...
000700130218     D*                          va; '4' = definitiva.
000800130212     D*--------------------------------------------------
000900130212     D YCO089_Init     PR            10I 0
001000130212     D  priKpjba                           LIKEDS(kpjba)
001100130218     D  priTipoElaborazione...
001200130218     D                                1A   CONST
001300130212
001400130212     D*--------------------------------------------------
001500130212     D* Procedure name: YCO089_Finalize
001600130212     D* Purpose:        Chiusura programma servizio.
001700130212     D* Returns:
001800130212     D*--------------------------------------------------
001900130212     DYCO089_Finalize  PR
002000130212
002100130212     D*--------------------------------------------------
002200130212     D* Procedure name: YCO089_RunPartitaIVA
002300130212     D* Purpose:        Elabora tutti i codici di una partita IVA.
002400130212     D* Returns:        Esito.
002500130212     D* Parameter:      priPartitaIVA
002600130212     D* Parameter:      priCodiceFiscale
002700130212     D*--------------------------------------------------
002800130212     D YCO089_RunPartitaIVA...
002900130212     D                 PR            10I 0
003000130213     D  priPartitaIVA                      LIKE(andiz00f.partIva)
003100130212     D                                     CONST
003200130212     D  priCodiceFiscale...
003300130213     D                                     LIKE(andiz00f.cdFisc)
003400130212     D                                     CONST
003500130218     D  priStatoCreditoCambiato...
003600130218     D                                 N
003700130218     D  priNuovoStatoCredito...
003800130218     D                                     LIKE(andiz00f.ctgAna)
003900130218
004000130214     D*--------------------------------------------------
004100130214     D* Procedure name: YCO089_GetStatoCreditoRicalcolato
004200130214     D* Purpose:        Ritorna lo stato del credito ricalcolato.
004300130214     D* Returns:        Stato del credito ricalcolato (' 33',' 36').
004400130214     D* Parameter:      priKsc => Sotto conto cliente.
004500130214     D*--------------------------------------------------
004600130214     D YCO089_GetStatoCreditoRicalcolato...
004700130214     D                 PR             4A
004800130214     D  priKcc                             LIKE(andiz00f.kcc) CONST
004900130214     D  priKsc                             LIKE(andiz00f.ksc) CONST
005000130214     D  priStatoCredito...
005100130214     D                                     LIKE(andiz00f.ctgAna) CONST
005200130214
005300130215     D*--------------------------------------------------
005400130215     D* Procedure name: YCO089_RunClienti
005500130215     D* Purpose:        Elabora tutti i clienti.
005600130215     D* Returns:        Esito.
005700130215     D*--------------------------------------------------
005800130215     D YCO089_RunClienti...
005900130215     D                 PR            10I 0
006000140109     D  priStatoCredito...
006001140109     D                                     LIKE(andiz00f.ctgAna) CONST
006002140109
