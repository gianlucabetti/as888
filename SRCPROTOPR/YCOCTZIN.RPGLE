000100120521     D*--------------------------------------------------
000200120521     D* Procedure name: YCOCTZIN_Init
000300120521     D* Purpose:        Inizializzazione modulo.
000400120521     D* Returns:        Esito.
000500120521     D* Parameter:      priKpjba
000600120521     D*--------------------------------------------------
000700120521     D YCOCTZIN_Init   PR            10I 0
000800120521     D  priKpjba                           LIKEDS(kpjba)
000900120521
001000120521     D*--------------------------------------------------
001100120521     D* Procedure name: YCOCTZIN_Finalize
001200120521     D* Purpose:        Chiusura modulo.
001300120521     D* Returns:
001400120521     D*--------------------------------------------------
001500120521     D YCOCTZIN_Finalize...
001600120521     D                 PR
001700120521
001800120521     D*--------------------------------------------------
001900120521     D* Procedure name: YCOCTZIN_NewPratica
002000120521     D* Purpose:        Inizia a leggere una pratica.
002100120521     D* Returns:        Esito.
002200120521     D* Parameter:      priIdContenzioso => ID contenzioso.
002300120521     D*--------------------------------------------------
002400120521     D YCOCTZIN_NewPratica...
002500120521     D                 PR            10I 0
002600120521     D  priIdContenzioso...
002700120521     D                               10I 0 CONST
002800120521
002900120521     D*--------------------------------------------------
003000120521     D* Procedure name: YCOCTZIN_EndPratica
003100120521     D* Purpose:        Finisce di leggere una pratica.
003200120521     D* Returns:        Esito.
003300120521     P* Parameter:      priCommit => *ON = commit; *OFF = rollback
003400120521     P*                              *NOPASS = niente.
003500120521     D*--------------------------------------------------
003600120521     D YCOCTZIN_EndPratica...
003700120521     D                 PR            10I 0
003800120521     D priCommit                       N   OPTIONS(*NOPASS) CONST
003900120521
004000120521     D*--------------------------------------------------
004100120521     D* Procedure name: YCOCTZIN_NewClienti
004200120521     D* Purpose:        Inizia a leggere i clienti di una pratica di conten...
004300120521     D*                          zioso.
004400120521     D* Returns:        Esito.
004500120521     D*--------------------------------------------------
004600120521     D YCOCTZIN_NewClienti...
004700120521     D                 PR            10I 0
004800120521
004900120521     D*--------------------------------------------------
005000120521     D* Procedure name: YCOCTZIN_EndClienti
005100120521     D* Purpose:        Finisce di leggere i clienti di una pratica.
005200120521     D* Returns:        Esito.
005300120521     D*--------------------------------------------------
005400120521     D YCOCTZIN_EndClienti...
005500120521     D                 PR            10I 0
005600120521
005700120521     D*--------------------------------------------------
005800120521     D* Procedure name: YCOCTZIN_GetNextIdCliente
005900120521     D* Purpose:        Restituisce il prossimo ID cliente.
006000120521     D* Returns:        >0 = ID cliente; <0 = errore; 0 = fine.
006100120521     D*--------------------------------------------------
006200120521     D YCOCTZIN_GetNextIdCliente...
006300120521     D                 PR            10I 0
006400120521
006500120521     D*--------------------------------------------------
006600120521     D* Procedure name: YCOCTZIN_NewCliente
006700120521     D* Purpose:        Inizia la lettura di un cliente.
006800120521     D* Returns:        Esito.
006900120521     D* Parameter:      priIdRigaCliente => ID riga cliente.
007000120521     D*--------------------------------------------------
007100120521     D YCOCTZIN_NewCliente...
007200120521     D                 PR            10I 0
007300120521     D  priIdRigaCliente...
007400120521     D                               10I 0 CONST
007500120521
007600120521     D*--------------------------------------------------
007700120521     D* Procedure name: YCOCTZIN_EndCliente
007800120521     D* Purpose:        Finisce la lettura di un cliente.
007900120521     D* Returns:        Esito.
008000120521     D*--------------------------------------------------
008100120521     D YCOCTZIN_EndCliente...
008200120521     D                 PR            10I 0
008300120521
008400120521     D*--------------------------------------------------
008500120521     D* Procedure name: YCOCTZIN_GetCliente
008600120521     D* Purpose:        Reperisce le informazioni del cliente in contenzioso.
008700120521     D* Returns:        Esito.
008800120521     D* Parameter:      priIdSocieta => ID società.
008900120521     D* Parameter:      priIdCliente => ID cliente.
009000120521     D* Parameter:      priAccentramentoSys => Registrazione Proj accentram...
009100120521     D*                          ento: sistema.
009200120523     D* Parameter:      priAccentramentoNas => Registrazione Proj accent...
009300120521     D*                          ramento: n. assoluto.
009400120521     D* Parameter:      priImportoContenzioso => Importo contenzioso in euro.
009500120521     D*--------------------------------------------------
009600120521     D YCOCTZIN_GetCliente...
009700120521     D                 PR            10I 0
009800120521     D  priIdSocieta                  3A
009900120521     D  priIdCliente                  8A
010000120521     D  priAccentramentoSys...
010100120521     D                                5I 0
010200120523     D  priAccentramentoNas...
010300120521     D                               10I 0
010400120521     D  priImportoContenzioso...
010500120521     D                               15P 2
010600120521
010700120529     D*--------------------------------------------------
010800120529     D* Procedure name: YCOCTZIN_NewStati
010900120529     D* Purpose:        Inizia la lettura di tutti gli stati di una pratica.
011000120529     D* Returns:        Esito.
011100120529     D*--------------------------------------------------
011200120529     D YCOCTZIN_NewStati...
011300120529     D                 PR            10I 0
011400120529
011500120529     D*--------------------------------------------------
011600120529     D* Procedure name: YCOCTZIN_GetNextIdStato
011700120529     D* Purpose:        Restituisce il prossimo ID stato.
011800120529     D* Returns:        Esito.
011900120529     D*--------------------------------------------------
012000120529     D YCOCTZIN_GetNextIdStato...
012100120529     D                 PR            10I 0
012200120529
012300120529     D*--------------------------------------------------
012400120529     D* Procedure name: YCOCTZIN_EndStati
012500120529     D* Purpose:        Finisce di leggere gli stati di una pratica.
012600120529     D* Returns:        Esito.
012700120529     D*--------------------------------------------------
012800120529     D YCOCTZIN_EndStati...
012900120529     D                 PR            10I 0
013000120529
013100120529     D*--------------------------------------------------
013200120529     D* Procedure name: YCOCTZIN_NewStato
013300120529     D* Purpose:        Inizia la lettura di uno stato.
013400120529     D* Returns:        Esito.
013500120529     D* Parameter:      priIdRigaStato => ID riga stato.
013600120529     D*--------------------------------------------------
013700120529     D YCOCTZIN_NewStato...
013800120529     D                 PR            10I 0
013900120529     D priIdRigaStato                10I 0 CONST
014000120529
014100120529     D*--------------------------------------------------
014200120529     D* Procedure name: YCOCTZIN_GetStato
014300120529     D* Purpose:        Reperisce le informazioni dello stato del credito.
014400120529     D* Returns:        Esito.
014500120529     D* Parameter:      priIdStatoCredito => ID stato del credito.
014600120529     D* Parameter:      priDataStatoCredito => Data dello stato del credito.
014700120529     D*--------------------------------------------------
014800120529     D YCOCTZIN_GetStato...
014900120529     D                 PR            10I 0
015000120529     D  priIdStatoCredito...
015100120529     D                                4A
015200120529     D  priDataStatoCredito...
015300120529     D                                 D   DATFMT(*ISO)
015400120705     D  priTimestampStatoCredito...
015500120705     D                                 Z   OPTIONS(*NOPASS)
015600120705
015700120529     D*--------------------------------------------------
015800120529     D* Procedure name: YCOCTZIN_EndStato
015900120529     D* Purpose:        Finisce la lettura di uno stato del credito.
016000120529     D* Returns:        Esito.
016100120529     D*--------------------------------------------------
016200120529     D YCOCTZIN_EndStato...
016300120529     D                 PR            10I 0
016400120529
016500120605     D*--------------------------------------------------
016600120605     D* Procedure name: YCOCTZIN_GetPratica
016700120605     D* Purpose:        Restituisce i dati di una pratica di contenzioso.
016800120605     D* Returns:        Esito.
016900120605     D* Parameter:      priPartitaIva => Partita IVA.
017000120605     D* Parameter:      priCodiceFiscale => Codice fiscale.
017100120605     D* Parameter:      priIdClienteSocieta => ID società cliente.
017200120605     D* Parameter:      priIdCliente => ID cliente accentratore.
017300120605     D* Parameter:      priDataInizioContenzioso => Data inizio contenzioso.
017400120605     D* Parameter:      priDataContenzioso => Data fine contenzioso.
017500120605     D* Parameter:      priIdStatoCredito => ID stato del credito.
017600120605     D* Parameter:      priImportoContenzioso => Importo contenzioso in euro.
017700120605     D*--------------------------------------------------
017800120605     D YCOCTZIN_GetPratica...
017900120605     D                 PR            10I 0
018000120605     D  priPartitaIva                20A   VARYING OPTIONS(*NULLIND)
018100120605     D  priCodiceFiscale...
018200120605     D                               16A   VARYING OPTIONS(*NULLIND)
018300120605     D  priIdClienteSocieta...
018400120605     D                                3A
018500120605     D  priIdCliente                  8A
018600120605     D  priDataInizioContenzioso...
018700120605     D                                 D   DATFMT(*ISO)
018800120605     D  priDataFineContenzioso...
018900120605     D                                 D   DATFMT(*ISO)
019000120605     D                                     OPTIONS(*NULLIND)
019100120605     D  priIdStatoCredito...
019200120605     D                                4A
019300120606     D  priDataStatoCredito...
019400120706     D                                 Z
019500120606     D  priImportoContenzioso...
019600120605     D                               15P 2
019700120605
019800120606     D*--------------------------------------------------
019900120606     D* Procedure name: YCOCTZIN_GetStatoContenziosoByData
020000120606     D* Purpose:        Restituisce lo stato del contenzioso in base alla data.
020100120606     D* Returns:        Negativo = errore; Positivo = ID riga stato.
020200120606     D* Parameter:      priDataStatoCredito => Data di riferimento (*NOPASS ...
020300120606     D*                          = data corrente).
020400120606     D*--------------------------------------------------
020500120606     D YCOCTZIN_GetStatoContenziosoByData...
020600120606     D                 PR            10I 0
020700120606     D  priDataStatoCredito...
020800120606     D                                 D   DATFMT(*ISO)
020900120606     D                                     VALUE
021000120606     D                                     OPTIONS(*NOPASS)
021100120606
