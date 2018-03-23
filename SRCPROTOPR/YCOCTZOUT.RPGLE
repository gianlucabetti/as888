000100120509     D*--------------------------------------------------
000200120509     D* Procedure name: YCOCTZOUT_Init
000300120509     D* Purpose:        Inizializza il modulo.
000400120509     D* Returns:        Esito.
000500120509     D* Parameter:      priKpjba
000600120509     D*--------------------------------------------------
000700120509     D YCOCTZOUT_Init  PR            10I 0
000800120509     D  priKpjba                           LIKEDS(kpjba)
000900120509
001000120509     D*--------------------------------------------------
001100120509     D* Procedure name: YCOCTZOUT_Finalize
001200120509     D* Purpose:        Chiude il modulo.
001300120509     D* Returns:
001400120509     D*--------------------------------------------------
001500120509     D YCOCTZOUT_Finalize...
001600120509     D                 PR
001700120509
001800120509     D*--------------------------------------------------
001900120509     D* Procedure name: YCOCTZOUT_NewPratica
002000120509     D* Purpose:        Inizia a lavorare con una pratica di contenzioso.
002100120509     D* Returns:        Esito.
002200120509     D* Parameter:      priOperazione => Operazione (inserire, modificare, ...
002300120509     D*                          annullare, ripristinare, cancellare).
002400120509     D* Parameter:      priIdContenzioso => ID contenzioso.
002500120509     D*--------------------------------------------------
002600120509     D YCOCTZOUT_NewPratica...
002700120509     D                 PR            10I 0
002800120509     D  priOperazione                10I 0 CONST
002900120510     D  priIdSocieta                  3A   CONST
003000120510     D  priIdContenzioso...
003100120509     D                               10I 0 OPTIONS(*NOPASS)
003200120509
003300120509     D*--------------------------------------------------
003400120509     D* Procedure name: YCOCTZOUT_SetSoggetto
003500120509     D* Purpose:        Imposta il soggetto del contenzioso.
003600120509     D* Returns:        Esito.
003700120509     D* Parameter:      priPartitaIVA => Partita IVA.
003800120509     D* Parameter:      priCodiceFiscale => Codice fiscale.
003900120509     D* Parameter:      priIdClienteSocieta => ID società cliente.
004000120509     D* Parameter:      priIdClienteKsc => ID cliente accentratore.
004100120509     D*--------------------------------------------------
004200120509     D YCOCTZOUT_SetSoggetto...
004300120509     D                 PR            10I 0
004400120509     D  priPartitaIVA                20A
004500120509     D  priCodiceFiscale...
004600120509     D                               16A
004700120509     D  priIdClienteSocieta...
004800120509     D                                3A
004900120509     D  priIdClienteKsc...
005000120509     D                                8A
005100120509
005200120509     D*--------------------------------------------------
005300120509     D* Procedure name: YCOCTZOUT_EndPratica
005400120509     D* Purpose:        Finisce di lavorare con una pratica.
005500120509     D* Returns:        >0 = ID contenzioso; <0 = esito.
005600120509     D* Parameter:      priCommit => *ON = Commit; *OFF = RollBack.
005700120509     D*--------------------------------------------------
005800120509     D YCOCTZOUT_EndPratica...
005900120509     D                 PR            10I 0
006000120509     D  priCommit                      N   CONST
006100120509
006200120509     D*--------------------------------------------------
006300120509     D* Procedure name: YCOCTZOUT_AddPartitaAlContenzioso
006400120509     D* Purpose:        Aggiunge una partita contabile al contenzioso.
006500120509     D* Returns:        Esito.
006600120509     D* Parameter:      priIdSocieta => ID società.
006700120509     D* Parameter:      priCapoConto => Capo conto.
006800120509     D* Parameter:      priSottoConto => Sotto conto.
006900120509     D* Parameter:      priDataPartita => Data partita.
007000120509     D* Parameter:      priNumeroPartita => Numero partita.
007100120509     D* Parameter:      priSeriePartita => Serie partita.
007200120509     D* Parameter:      priSaldoPartitaEuro => Saldo partita in euro (>0 = ...
007300120509     D*                          dare; <0 = avere).
007400120509     D*--------------------------------------------------
007500120509     D YCOCTZOUT_AddPartitaAlContenzioso...
007600120509     D                 PR            10I 0
007700120509     D  priIdSocieta                  3A
007800120509     D  priContabilita...
007900120509     D                                2A
008000120509     D  priCapoConto                  6A
008100120509     D  priSottoConto                 8A
008200120509     D priDataPartita                  D   DATFMT(*ISO)
008300120509     D  priNumeroPartita...
008400120509     D                                9P 0
008500120509     D  priSeriePartita...
008600120509     D                                4A
008700120509     D  priSaldoPartitaEuro...
008800120509     D                               15P 2
008900120713     D  priEsposizionePartitaEuro...
009000120713     D                               15P 2
009100121026     D  priDataScadenza...
009200121026     D                                 D   DATFMT(*ISO)
009300121026
009400120515     D*--------------------------------------------------
009500120515     D* Procedure name: YCOCTZOUT_SetStatoCreditoCliente
009600120515     D* Purpose:        Imposta lo stato del credito del cliente.
009700120515     D* Returns:        Esito.
009800120515     D* Parameter:      priIdSocieta => ID società.
009900120515     D* Parameter:      priIdCliente
010000120515     D* Parameter:      priStatoDelCredito
010100120515     D*--------------------------------------------------
010200120515     D YCOCTZOUT_SetStatoCreditoCliente...
010300120515     D                 PR            10I 0
010400120712     D  priIdSocieta                  3A                                        1
010500120712     D  priIdCliente                  8A                                        2
010600120712     D  priStatoDelCredito...                                                   3
010700120515     D                                4A   CONST
010800120712     D  priSoloAnagraficaCliente...                                             4
010900120712     D                                 N   CONST OPTIONS(*NOPASS)
011000120712
011100120618     D*--------------------------------------------------
011200120618     D* Procedure name: YCOCTZOUT_AddClienteAlContenzioso
011300120618     D* Purpose:        Aggiunge un codice cliente al contenzioso.
011400120618     D* Returns:        Esito.
011500120618     D* Parameter:      priIdSocieta => ID società.
011600120618     D* Parameter:      priIdCliente => ID cliente
011700120618     D* Parameter:      priSaldoContabile => Saldo contabile portato al con...
011800120618     D*                          tenzioso.
011900120618     D* Parameter:      priNumeroPartite => Numero di partite portate al co...
012000120618     D*                          ntenzioso.
012100120618     D*--------------------------------------------------
012200120618     D YCOCTZOUT_AddClienteAlContenzioso...
012300120618     D                 PR            10I 0
012400120618     D  priIdSocieta                  3A   CONST
012500120618     D  priIdCliente                  8A   CONST
012600120618     D  priSaldoContabile...
012700120618     D                               15P 2
012800120618     D  priNumeroPartite...
012900120618     D                               10I 0
013000120618
013100120620     D*--------------------------------------------------
013200120620     D* Procedure name: YCOCTZOUT_IsFatturazioneInCorso
013300120620     D* Purpose:        Restituisce *ON se è in corso la fatturazione.
013400120620     D* Returns:        *ON = fatturazione in corso.
013500120620     D*--------------------------------------------------
013600120620     D YCOCTZOUT_IsFatturazioneInCorso...
013700120620     D                 PR              N
013800120620
013900120711     D*--------------------------------------------------
014000120711     D* Procedure name: YCOCTZOUT_SetStatoCreditoPratica
014100120711     D* Purpose:        Imposta lo stato del credito della pratica.
014200120711     D* Returns:        Esito.
014300120711     D* Parameter:      priStatoDelCredito => ID stato del credito.
014400120711     D*--------------------------------------------------
014500120711     D YCOCTZOUT_SetStatoCreditoPratica...
014600120711     D                 PR            10I 0
014700120711     D  priStatoDelCredito...
014800120711     D                                4A   CONST
014900120712     D  priRqsOpCode...
015000120712     D                               10I 0 CONST OPTIONS(*NOPASS)
015100120712     D  priIdRiga...
015200120712     D                               10I 0 CONST OPTIONS(*NOPASS)
015300120713
015400120716     D*--------------------------------------------------
015500120716     D* Procedure name: YCOCTZOUT_SetFineContenzioso
015600120716     D* Purpose:        Imposta la fine del contenzioso.
015700120716     D* Returns:        Esito.
015800120716     D* Parameter:      priFineContenzioso => *ON = fine contenzioso.
015900120716     D*--------------------------------------------------
016000120716     D YCOCTZOUT_SetFineContenzioso...
016100120716     D                 PR            10I 0
016200120716     D  priFineContenzioso...
016300120716     D                                 N   CONST
016400120716
