000100120531     D YCOCTZSEL_resultClienti...
000200120531     D                 DS                  QUALIFIED TEMPLATE
000300120531     D  idCliente                     8A
000400120705     D YCOCTZSEL_resultClientiInfo...
000500120705     D                 DS                  QUALIFIED TEMPLATE
000600120709     D  daContendere...
000700120709     D                               15P 2
000800120709     D  saldoCoGe...
000900120706     D                               15P 2
001000120706     D  promesseScadere...
001100120705     D                               15P 2
001200120706     D  cessioniScadere...
001300120705     D                               15P 2
001400120706     D  ribaPresentateScadere...
001500120705     D                               15P 2
001600120710     D  effettiNonPresentati...
001700120710     D                               15P 2
001800120531     D YCOCTZSEL_resultFiltro...
001900120531     D                 DS                  QUALIFIED TEMPLATE
002000120531     D  idContenzioso                10I 0
002100120531
002200120531     D*--------------------------------------------------
002300120530     D* Procedure name: YCOCTZSEL_Init
002400120530     D* Purpose:        Inizializza il modulo.
002500120530     D* Returns:        Esito.
002600120530     D* Parameter:      priKpjba
002700120530     D*--------------------------------------------------
002800120530     D YCOCTZSEL_Init  PR            10I 0
002900120530     D  priKpjba                           LIKE(kpjba)
003000120530
003100120530     D*--------------------------------------------------
003200120530     D* Procedure name: YCOCTZSEL_Finalize
003300120530     D* Purpose:        Chiusura modulo.
003400120530     D* Returns:
003500120530     D*--------------------------------------------------
003600120530     D YCOCTZSEL_Finalize...
003700120530     D                 PR
003800120530
003900120530     D*--------------------------------------------------
004000120530     D* Procedure name: YCOCTZSEL_NewClientiByIdCliente
004100120530     D* Purpose:        Seleziona gli ID cliente legati all'ID cliente passato.
004200120530     D* Returns:        Esito.
004300120530     D* Parameter:      priIdCliente => ID cliente
004400120530     D*--------------------------------------------------
004500120530     D YCOCTZSEL_NewClientiByIdCliente...
004600120530     D                 PR            10I 0
004700120530     D  priIdSocieta                  3A   CONST
004800120530     D  priIdCliente                  8A   CONST
004900120718     D  priSituazioneCompleta...
005000120718     D                                 N   CONST
005100130606     D  priEsclusioneCategoria5...
005101130606     D                                 N   CONST OPTIONS(*NOPASS)
005200120530     D*--------------------------------------------------
005300120530     D* Procedure name: YCOCTZSEL_EndClientiByIdCliente
005400120530     D* Purpose:        Fine selezione ID clienti.
005500120530     D* Returns:        Esito.
005600120530     D*--------------------------------------------------
005700120530     D YCOCTZSEL_EndClientiByIdCliente...
005800120530     D                 PR            10I 0
005900120530
006000120530     D*--------------------------------------------------
006100120530     D* Procedure name: YCOCTZSEL_FetchNextClientiByIdCliente
006200120530     D* Purpose:        Restituisce n clienti.
006300120530     D* Returns:        <0 = errore; 0 = no data; >0 = numero elementi rest...
006400120530     D*                          ituiti.
006500120530     D* Parameter:      priNumberOfRowsToFetch => Numero di righe da restit...
006600120530     D*                          uire.
006700120530     D* Parameter:      priIdClienti => Schiera ID clienti
006800120530     D*--------------------------------------------------
006900120530     D YCOCTZSEL_FetchNextClientiByIdCliente...
007000120530     D                 PR            10I 0
007100120530     D  priNumberOfRowsToFetch...
007200120531     D                               10I 0 VALUE
007300120531     D  priResult                          LIKEDS(YCOCTZSEL_resultClienti)
007400120601     D                                     DIM(255) OPTIONS(*VARSIZE)
007500120706     D  priResultInfo                      LIKEDS(YCOCTZSEL_resultClientiInfo)
007600120706     D                                     DIM(255) OPTIONS(*VARSIZE:*NOPASS)
007700120706
007800120531     D*--------------------------------------------------
007900120531     D* Procedure name: YCOCTZSEL_NewFiltroContenziosi
008000120531     D* Purpose:        Inizia un nuovo filtro di pratiche contenzioso.
008100120531     D* Returns:        Esito.
008200120531     D*--------------------------------------------------
008300120531     D YCOCTZSEL_NewFiltroContenziosi...
008400120531     D                 PR            10I 0
008500120608     D  priIdContenzioso...
008600120608     D                               10I 0 CONST OPTIONS(*NOPASS)
008700120608
008800120531     D*--------------------------------------------------
008900120531     D* Procedure name: YCOCTZSEL_SetFiltroContenziosi_WherePartitaIva
009000120531     D* Purpose:        Imposta il filtro sulla partita IVA del cliente.
009100120531     D* Returns:        Esito.
009200120531     D* Parameter:      priPartitaIva => Partita IVA.
009300120531     D*--------------------------------------------------
009400120531     D YCOCTZSEL_SetFiltroContenziosi_WherePartitaIva...
009500120531     D                 PR            10I 0
009600120531     D  priPartitaIva                20A   VARYING
009700120531     D                                     CONST
009800120531
009900120531     D*--------------------------------------------------
010000120531     D* Procedure name: YCOCTZSEL_SetFiltroContenziosi_WhereCodiceFiscale
010100120531     D* Purpose:        Imposta il filtro sul codice fiscale.
010200120531     D* Returns:        Esito.
010300120531     D* Parameter:      priCodiceFiscale => Codice fiscale.
010400120531     D*--------------------------------------------------
010500120531     D YCOCTZSEL_SetFiltroContenziosi_WhereCodiceFiscale...
010600120531     D                 PR            10I 0
010700120531     D  priCodiceFiscale...
010800120531     D                               20A   VARYING
010900120531     D                                     CONST
011000120531
011100120531     D*--------------------------------------------------
011200120531     D* Procedure name: YCOCTZSEL_SetFiltroContenziosi_WhereClienteAccentratore
011300120531     D* Purpose:        Imposta il filtro sul cliente accentratore.
011400120531     D* Returns:        Esito.
011500120531     D* Parameter:      priClienteAccentratoreSocieta => Società cliente ac...
011600120531     D*                          centratore
011700120531     D* Parameter:      priClienteAccentratoreKsc => ID cliente accentratore
011800120531     D*--------------------------------------------------
011900120531     D YCOCTZSEL_SetFiltroContenziosi_WhereClienteAccentratore...
012000120531     D                 PR            10I 0
012100120531     D  priClienteAccentratoreSocieta...
012200120531     D                                3A   VALUE
012300120531     D  priClienteAccentratoreKsc...
012400120531     D                                8A   CONST
012500120531
012600120531     D*--------------------------------------------------
012700120531     D* Procedure name: YCOCTZSEL_SetFiltroContenziosi_WhereDataInizio
012800120531     D* Purpose:        Imposta il filtro sulla data inizio contenzioso
012900120531     D* Returns:        Esito.
013000120531     D* Parameter:      priDallaData
013100120531     D* Parameter:      priAllaData
013200120531     D*--------------------------------------------------
013300120531     D YCOCTZSEL_SetFiltroContenziosi_WhereDataInizio...
013400120531     D                 PR            10I 0
013500120531     D  priDallaData                   D   DATFMT(*ISO)
013600120531     D                                     VALUE
013700120531     D  priAllaData                    D   DATFMT(*ISO)
013800120531     D                                     VALUE
013900120531
014000120531     D*--------------------------------------------------
014100120531     D* Procedure name: YCOCTZSEL_SetFiltroContenziosi_WhereDataFine
014200120531     D* Purpose:        Imposta il filtro sulla data fine contenzioso.
014300120531     D* Returns:        Esito.
014400120531     D* Parameter:      priDallaData
014500120531     D* Parameter:      priAllaData
014600120531     D*--------------------------------------------------
014700120531     D YCOCTZSEL_SetFiltroContenziosi_WhereDataFine...
014800120531     D                 PR            10I 0
014900120531     D  priDallaData                   D   DATFMT(*ISO)
015000120531     D                                     VALUE
015100120531     D  priAllaData                    D   DATFMT(*ISO)
015200120531     D                                     VALUE
015300120531
015400120531     D*--------------------------------------------------
015500120531     D* Procedure name: YCOCTZSEL_SetFiltroContenziosi_WhereStatiCredito
015600120531     D* Purpose:        Imposta il filtro sullo stato del credito.
015700120531     D* Returns:        Esito.
015800120531     D* Parameter:      priIdStatoCredito => ID stato del credito.
015900120531     D* Parameter:      priPredicato => Predicato ( = , <> , < , > , <= , >= )
016000120531     D*--------------------------------------------------
016100120531     D YCOCTZSEL_SetFiltroContenziosi_WhereStatiCredito...
016200120531     D                 PR            10I 0
016300120531     D  priIdStatoCredito...
016400120531     D                                4A   VALUE
016500120531     D  priPredicato                  2A   VALUE
016600120531
016700120531     D*--------------------------------------------------
016800120531     D* Procedure name: YCOCTZSEL_RunFiltroContenziosi
016900120531     D* Purpose:        Esegue il filtro.
017000120531     P* Returns:        Negativo = Esito; Positivo = Numero righe.
017100120531     D*--------------------------------------------------
017200120531     D YCOCTZSEL_RunFiltroContenziosi...
017300120531     D                 PR            10I 0
017400120531
017500120531     D*--------------------------------------------------
017600120531     D* Procedure name: YCOCTZSEL_FetchNextFiltroContenziosi
017700120531     D* Purpose:        Restituisce n contenziosi.
017800120531     D* Returns:        Esito.
017900120531     D* Parameter:      priNumberOfRowsToFetch => Numero di righe da leggere.
018000120531     D*--------------------------------------------------
018100120531     D YCOCTZSEL_FetchNextFiltroContenziosi...
018200120531     D                 PR            10I 0
018300120531     D  priNumberOfRowsToFetch...
018400120531     D                               10I 0 VALUE
018500120531     D  priResult                          LIKEDS(YCOCTZSEL_resultFiltro)
018600120601     D                                     DIM(255) OPTIONS(*VARSIZE)
018700120705     D  priResultInfo                      LIKEDS(YCOCTZSEL_resultClientiInfo)
018800120705     D                                     DIM(255) OPTIONS(*VARSIZE:*NOPASS)
018900120705
019000120531     D*--------------------------------------------------
019100120531     D* Procedure name: YCOCTZSEL_EndFiltroContenziosi
019200120531     D* Purpose:        Finisce l'elaborazione di un filtro.
019300120531     D* Returns:        Esito.
019400120531     D*--------------------------------------------------
019500120531     D YCOCTZSEL_EndFiltroContenziosi...
019600120531     D                 PR            10I 0
019700120531
019800120608     D*--------------------------------------------------
019900120608     D* Procedure name: YCOCTZSEL_SetFiltroContenziosi_WhereProfiloUtente
020000120608     D* Purpose:        Imposta la WHERE sul profilo utente inserimento.
020100120608     D* Returns:        Esito.
020200120608     D* Parameter:      priProfiloUtente
020300120608     D*--------------------------------------------------
020400120608     D YCOCTZSEL_SetFiltroContenziosi_WhereProfiloUtente...
020500120608     D                 PR            10I 0
020600120608     D  priProfiloUtente...
020700120608     D                               10A
020800120608
020900120608     D*--------------------------------------------------
021000120608     D* Procedure name: YCOCTZSEL_SetFiltroContenziosi_WhereAperti
021100120608     D* Purpose:        Prepara la WHERE sui contenziosi aperti.
021200120608     D* Returns:        Esito.
021300120608     D* Parameter:      priAperti => ' '=tutto; 'A'=solo aperti; 'C'=solo c...
021400120608     D*                          hiusi.
021500120608     D*--------------------------------------------------
021600120608     D YCOCTZSEL_SetFiltroContenziosi_WhereAperti...
021700120608     D                 PR            10I 0
021800120608     D  priAperti                     1A   CONST
021900120608
