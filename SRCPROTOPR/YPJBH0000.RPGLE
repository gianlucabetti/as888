000100110617      /IF NOT DEFINED(andiz00f)
000200110720     D*andiz00f      E DS                  QUALIFIED BASED(nullPtr)
000300110617      /ENDIF
000400110617      /IF NOT DEFINED(kpjba)
000500110720     D*kpjba         E DS                  QUALIFIED BASED(nullPtr)
000600110617      /ENDIF
000700110617
000800110617     D*--------------------------------------------------
000900110617     D* Procedure name: YPJBH_Init
001000110617     D* Purpose:        Inizializzazione modulo.
001100110617     D* Returns:        Esito.
001200110617     D* Parameter:      priKpjba
001300110617     D* Parameter:      priSocietaLavoro => Società lavoro.
001400110617     D*--------------------------------------------------
001500110617     D YPJBH_Init      PR            10I 0
001600110617     D  priKpjba                           LIKEDS(kpjba)
001700110617     D  priSocietaLavoro...
001800110617     D                                     LIKE(andiz00f.societa)
001900110617     D                                     CONST
002000110617     D                                     OPTIONS(*NOPASS)
002100110617
002200110617     D*--------------------------------------------------
002300110617     D* Procedure name: YPJBH_Finalize
002400110617     D* Purpose:        Chiude il modulo.
002500110617     D* Returns:        Esito.
002600110617     D*--------------------------------------------------
002700110617     D YPJBH_Finalize  PR            10I 0
002800110617
002900110617     D*--------------------------------------------------
003000110620     D* Procedure name: YPJBH_NewRegistrazione
003100110620     D* Purpose:        Nuova registrazione.
003200110617     D* Returns:        Esito.
003300110617     D*--------------------------------------------------
003400110620     D YPJBH_NewRegistrazione...
003500110617     D                 PR            10I 0
003600110621     D  priAccorpareMovimenti...                                                1
003700110621     D                                 N   CONST
003800110621     D  priCausaleTestata...                                                    2
003900110620     D                                     LIKE(andiz00f.causale)
004000110620     D                                     CONST
004100110621     D  priDataRegistrazione...                                                 3
004200110620     D                                 D   CONST
004300110620     D                                     OPTIONS(*NOPASS:*OMIT)
004400110621     D  priSocietaRegistrazione...                                              4
004500110620     D                                     LIKE(andiz00f.societa)
004600120514     D                                     OPTIONS(*NOPASS:*OMIT) CONST
004700120514     D  priGetNumeroAssoluto...                                                 5
004800120514     D                                 N   CONST OPTIONS(*NOPASS)
004801151218     D  priLibroIVA                   3A   CONST OPTIONS(*NOPASS:*OMIT)         6
004900151218     D  priNumeroAssoluto...
004901151218     D                                9P 0 OPTIONS(*NOPASS)                     7
004902170215     D  priDivisa                     4A   OPTIONS(*NOPASS)                     8
004903151218
005000110620     D*--------------------------------------------------
005100110620     D* Procedure name: YPJBH_SetLottoRegistrazione
005200110620     D* Purpose:        Imposta il lotto della registrazione.
005300110620     D* Returns:        Esito.
005400110620     D* Parameter:      priLottoRegistrazione => Lotto registrazione
005500110620     D*                 *NOPASS = numero azione.
005600110620     D*--------------------------------------------------
005700110620     D YPJBH_SetLottoRegistrazione...
005800110620     D                 PR            10I 0
005900110620     D  priLotto...
006000110620     D                                9A   CONST
006100110620
006200110620     D*--------------------------------------------------
006300110620     D* Procedure name: YPJBH_SetSubNumeroRegistrazione
006400110620     D* Purpose:        Imposta il subnumero di registrazione.
006500110620     D* Returns:        Esito.
006600110620     D* Parameter:      priSubNumeroRegistrazione => Subnumero registrazione
006700110620     D*                 *NOPASS = +1
006800110620     D*--------------------------------------------------
006900110620     D YPJBH_SetSubNumeroRegistrazione...
007000110620     D                 PR            10I 0
007100110620     D  priSubNumero...
007200110620     D                                9P 0 CONST
007300110620
007400110620     D*--------------------------------------------------
007500110620     D* Procedure name: YPJBH_SetUnitaRegistrazione
007600110620     D* Purpose:        Imposta l'unità di registrazione.
007700110620     D* Returns:        Esito.
007800110620     D*--------------------------------------------------
007900110620     D YPJBH_SetUnitaRegistrazione...
008000110620     D                 PR            10I 0
008100110620     D  priUnita...
008200110621     D                                     LIKE(andiz00f.unita) CONST
008201180129     D  priUnitaErrataForzareUnitaSede...
008202180129     D                                 N   CONST OPTIONS(*NOPASS)
008300110620
008400110620     D*--------------------------------------------------
008500110620     D* Procedure name: YPJBH_SetContabilitaRegistrazione
008600110620     D* Purpose:        Imposta la contabilità di registrazione.
008700110620     D* Returns:        Esito.
008800110620     D*--------------------------------------------------
008900110620     D YPJBH_SetContabilitaRegistrazione...
009000110620     D                 PR            10I 0
009100110620     D  priContabilita...
009200110620     D                                     LIKE(andiz00f.ctb)
009300110620     D                                     CONST
009400110620
009500110620     D*--------------------------------------------------
009600110620     D* Procedure name: YPJBH_SetTipoRegistrazione
009700110620     D* Purpose:        Imposta il tipo registrazione.
009800110620     D* Returns:        Esito.
009900110620     D* Parameter:      priTipoRegistrazione => Tipo registrazione
010000110620     D*--------------------------------------------------
010100110620     D YPJBH_SetTipoRegistrazione...
010200110620     D                 PR            10I 0
010300110620     D  priTipoRegistrazione...
010400110620     D                                     LIKE(andiz00f.tpRegZ)
010500110620     D                                     CONST
010600110620
010700110621     D*--------------------------------------------------
010800110621     D* Procedure name: YPJBH_SetNumeroRegistrazione
010900110621     D* Purpose:        Imposta numero registrazione
011000110621     D* Returns:        Esito.
011100110621     D* Parameter:      priNumeroRegistrazione => Numero registrazione.
011200110621     D* Parameter:      priSerieRegistrazione => Serie registrazione.
011300110621     D*--------------------------------------------------
011400110621     D YPJBH_SetNumeroRegistrazione...
011500110621     D                 PR            10I 0
011600110621     D  priNumeroRegistrazione...
011700110621     D                                     LIKE(andiz00f.nr)
011800110621     D                                     CONST
011900110621     D  priSerieRegistrazione...
012000110621     D                                     LIKE(andiz00f.serieNm)
012100110621     D                                     CONST
012200110621     D                                     OPTIONS(*NOPASS)
012300110621
012400110623     D*--------------------------------------------------
012500110623     D* Procedure name: YPJBH_SetDataLibroGiornale
012600110623     D* Purpose:        Imposta la data libro giornale della registrazione.
012700110623     D* Returns:        Esito.
012800110623     D* Parameter:      priDataLibroGiornale
012900110623     D*--------------------------------------------------
013000110623     D YPJBH_SetDataLibroGiornale...
013100110623     D                 PR            10I 0
013200110623     D  priDataLibroGiornale...
013300110623     D                                 D   DATFMT(*ISO)
013400110623     D                                     CONST
013500110623
013600110620     D*--------------------------------------------------
013700110620     D* Procedure name: YPJBH_EndRegistrazione
013800110620     D* Purpose:        Fine registrazione.
013900110620     D* Returns:        Esito.
014000110620     D* Parameter:      priCommit => *ON = commit; *OFF = rollback; *NOPASS...
014100110620     D*                           = niente.
014200110620     D*--------------------------------------------------
014300110620     D YPJBH_EndRegistrazione...
014400110620     D                 PR            10I 0
014500110620     D  priCommit                      N   CONST
014600110620     D                                     OPTIONS(*NOPASS)
014700110620
014800110620     D*--------------------------------------------------
014900110620     D* Procedure name: YPJBH_NewMovimento
015000110620     D* Purpose:        Nuovo movimento contabilità generale.
015100110620     D* Returns:        Esito.
015200110620     D* Parameter:      priCapoConto => Capo conto.
015300110620     D* Parameter:      priSottoConto => Sotto conto.
015400110620     D* Parameter:      priImportoMonetaCorrente => Importo in moneta corre...
015500110620     D*                          nte (euro).
015600110620     D* Parameter:      priDareAvere => Dare/Avere.
015700110620     D*--------------------------------------------------
015800110620     D YPJBH_NewMovimento...
015900110620     D                 PR            10I 0
016000110620     D  priCapoConto                       LIKE(andiz00f.kcc)
016100110620     D                                     CONST
016200110620     D  priSottoConto                      LIKE(andiz00f.ksc)
016300110620     D                                     CONST
016400110620     D  priImportoMonetaCorrente...
016500110620     D                               15P 2 CONST
016600110620     D  priDareAvere                  1A   CONST
016601170215     D  priImportoDivisa...
016602170215     D                               15P 2 CONST OPTIONS(*NOPASS)
016700110621
016800110623     D*--------------------------------------------------
016900110623     D* Procedure name: YPJBH_SetSocietaValere
017000110623     D* Purpose:        Imposta la società a valere del movimento.
017100110623     D* Returns:        Esito.
017200110623     D* Parameter:      priSocietaValere
017300110623     D*--------------------------------------------------
017400110623     D YPJBH_SetSocietaValere...
017500110623     D                 PR            10I 0
017600110623     D  priSocietaValere...
017700110623     D                                     LIKE(andiz00f.societa)
017800110623     D                                     CONST
017900110623
018000110621     D*--------------------------------------------------
018100110621     D* Procedure name: YPJBH_InsMovimento
018200110621     D* Purpose:        Inserisce movimento.
018300110621     D* Returns:        Esito.
018400110621     D*--------------------------------------------------
018500110621     D YPJBH_InsMovimento...
018600110621     D                 PR            10I 0
018700110620
018800110620     D*--------------------------------------------------
018900110620     D* Procedure name: YPJBH_EndMovimento
019000110620     D* Purpose:        Fine movimento.
019100110620     D* Returns:        Esito.
019200110621     D*--------------------------------------------------
019300110620     D YPJBH_EndMovimento...
019400110620     D                 PR            10I 0
019500110621
019600110621     D*--------------------------------------------------
019700110621     D* Procedure name: YPJBH_IsContoGestitoPartite
019800110621     D* Purpose:        Restituisce *ON se il conto è gestito a partite.
019900110621     D* Returns:        *ON = gestito a partite.
020000110621     D*--------------------------------------------------
020100110621     D YPJBH_IsContoGestitoPartite...
020200110621     D                 PR              N
020300110621
020400110621     D*--------------------------------------------------
020500110621     D* Procedure name: YPJBH_IsContoGestitoAnalitica
020600110621     D* Purpose:        Restituisce *ON se il conto è gestito in analitica.
020700110621     D* Returns:        *ON = conto gestito in contabilità analitica.
020800110621     D*--------------------------------------------------
020900110621     D YPJBH_IsContoGestitoAnalitica...
021000110621     D                 PR              N
021100110621
021200110621     D*--------------------------------------------------
021300110621     D* Procedure name: YPJBH_NewMovimentoAnalitica
021400110621     D* Purpose:        Nuovo movimento contabilità analitica.
021500110621     D* Returns:        Esito.
021600110622     P* Parameter:      priCompetenzaMensile => *ON = forzare primo giorno del mese.
021700110621     D* Parameter:      priDimensione => Dimensione di analitica (*NOPASS =...
021800110621     D*                           'D1'.
021900110621     D*--------------------------------------------------
022000110621     D YPJBH_NewMovimentoAnalitica...
022100110621     D                 PR            10I 0
022200110621     D  priImportoMonetaCorrente...
022300110621     D                               15P 2 CONST
022400110621     D  priDareAvere                  1A   CONST
022500110622     D  priCompetenzaMensile...
022600170215     D                                 N   CONST OPTIONS(*NOPASS:*OMIT)
022700110622     D  priDimensione                      LIKE(andiz00f.dimen) CONST
022800170215     D                                     OPTIONS(*NOPASS:*OMIT)
022801170215     D  priImportoDivisa...
022802170215     D                               15P 2 CONST OPTIONS(*NOPASS)
022900110621
023000110621     D*--------------------------------------------------
023100110621     D* Procedure name: YPJBH_SetVoceAnalitica
023200110621     D* Purpose:        Imposta la voce analitica.
023300110621     D* Returns:        Esito.
023400110621     D* Parameter:      priVoce => Voce di analitica.
023500110621     D*--------------------------------------------------
023600110621     D YPJBH_SetVoceAnalitica...
023700110621     D                 PR            10I 0
023800110621     D  priVoce                            LIKE(andiz00f.voce)
023900110621     D                                     CONST
024000110621
024100110621     D*--------------------------------------------------
024200110621     D* Procedure name: YPJBH_SetDataCompetenzaAnalitica
024300110621     D* Purpose:        Imposta la data competenza analitica.
024400110621     D* Returns:        Esito.
024500110621     D* Parameter:      priDataCompetenza => Data competenza analitica.
024600110622     D*--------------------------------------------------
024700110621     D YPJBH_SetDataCompetenzaAnalitica...
024800110621     D                 PR            10I 0
024900110621     D  priDataCompetenza...
025000110621     D                                 D   DATFMT(*ISO)
025100110621     D                                     CONST
025200110622
025300110621     D*--------------------------------------------------
025400110621     D* Procedure name: YPJBH_SetEntitaAnalitica
025500110621     D* Purpose:        Imposta le entità di analitica.
025600110621     D* Returns:        Esito.
025700110621     D* Parameter:      priEntita1
025800110621     D* Parameter:      priEntita2
025900110621     D* Parameter:      priEntita3
026000110621     D* Parameter:      priEntita4
026100110621     D* Parameter:      priEntita5
026200110621     D* Parameter:      priEntita6
026300110621     D* Parameter:      priEntita7
026400110621     D* Parameter:      priEntita8
026500110621     D*--------------------------------------------------
026600110621     D YPJBH_SetEntitaAnalitica...
026700110621     D                 PR            10I 0
026800110621     D  priEntita1                         LIKE(andiz00f.entita)
026900110621     D                                     CONST
027000110621     D  priEntita2                         LIKE(andiz00f.entita)
027100110621     D                                     CONST
027200110621     D                                     OPTIONS(*NOPASS)
027300110621     D  priEntita3                         LIKE(andiz00f.entita)
027400110621     D                                     CONST
027500110621     D                                     OPTIONS(*NOPASS)
027600110621     D  priEntita4                         LIKE(andiz00f.entita)
027700110621     D                                     CONST
027800110621     D                                     OPTIONS(*NOPASS)
027900110621     D  priEntita5                         LIKE(andiz00f.entita)
028000110621     D                                     CONST
028100110621     D                                     OPTIONS(*NOPASS)
028200110621     D  priEntita6                         LIKE(andiz00f.entita)
028300110621     D                                     CONST
028400110621     D                                     OPTIONS(*NOPASS)
028500110621     D  priEntita7                         LIKE(andiz00f.entita)
028600110621     D                                     CONST
028700110621     D                                     OPTIONS(*NOPASS)
028800110621     D  priEntita8                         LIKE(andiz00f.entita)
028900110621     D                                     OPTIONS(*NOPASS)
029000110621
029100110621     D*--------------------------------------------------
029200110621     D* Procedure name: YPJBH_InsMovimentoAnalitica
029300110621     D* Purpose:        Inserisce il moviemnto in NDBHA00F.
029400110621     D* Returns:        Esito.
029500110621     D*--------------------------------------------------
029600110621     D YPJBH_InsMovimentoAnalitica...
029700110621     D                 PR            10I 0
029800110621
029900110622     D*--------------------------------------------------
030000110622     D* Procedure name: YPJBH_EndMovimentoAnalitica
030100110622     D* Purpose:        Fine movimento analitica.
030200110622     D* Returns:        Esito.
030300110622     D*--------------------------------------------------
030400110622     D YPJBH_EndMovimentoAnalitica...
030500110622     D                 PR            10I 0
030600110622
030700110622     D*--------------------------------------------------
030800110622     D* Procedure name: YPJBH_NewMovimentoPartita
030900110622     D* Purpose:        Nuovo movimento partita.
031000110622     D* Returns:        Esito.
031100110622     D* Parameter:      priDataPartita
031200110622     D* Parameter:      priNumeroPartita
031300110622     D* Parameter:      priSeriePartita
031400110622     D* Parameter:      priImportoMonetaCorrente => Importo in euro.
031500110622     D* Parameter:      priDareAvere
031600110622     D*--------------------------------------------------
031700110622     D YPJBH_NewMovimentoPartita...
031800110622     D                 PR            10I 0
031900110622     D priDataPartita                  D   DATFMT(*ISO)
032000110622     D                                     CONST
032100110622     D  priNumeroPartita...
032200110622     D                                     LIKE(andiz00f.nr)
032300110622     D                                     CONST
032400110622     D  priSeriePartita...
032500110622     D                                     LIKE(andiz00f.serieNm)
032600110622     D                                     CONST
032700110622     D  priImportoMonetaCorrente...
032800110622     D                               15P 2 CONST
032900110622     D  priDareAvere                  1A   CONST
033000121022     D  priDataScadenza...
033100121022     D                                 D   DATFMT(*ISO)
033200170215     D                                     CONST OPTIONS(*NOPASS:*OMIT)
033201170215     D  priImportoDivisa...
033202170215     D                               15P 2 CONST OPTIONS(*NOPASS)
033300121022
033400110622     D*--------------------------------------------------
033500110622     D* Procedure name: YPJBH_InsMovimentoPartita
033600110622     D* Purpose:        Inserisce movimento partita in NDBHP00F.
033700110622     D* Returns:        Esito.
033800110622     D*--------------------------------------------------
033900110622     D YPJBH_InsMovimentoPartita...
034000110622     D                 PR            10I 0
034100110622
034200110622     D*--------------------------------------------------
034300110622     D* Procedure name: YPJBH_EndMovimentoPartita
034400110622     D* Purpose:        Fine movimento partita.
034500110622     D* Returns:        Esito
034600110622     D*--------------------------------------------------
034700110622     D YPJBH_EndMovimentoPartita...
034800110622     D                 PR            10I 0
034900110622
035000110719     D*--------------------------------------------------
035100110719     D* Procedure name: YPJBH_RegistrazioneLotto
035200110719     D* Purpose:        Sottomette la registrazione di tutte le registrazio...
035300110719     D*                          ni di un lotto.
035400110719     D* Returns:        Esito: negativo = fine anomala.
035500110719     D*                        positivo = numero di lavori sottomessi
035600110719     D* Parameter:      priLotto => ID lotto
035700110719     D*--------------------------------------------------
035800110719     D YPJBH_RegistrazioneLotto...
035900110719     D                 PR            10I 0
036000110719     D  priSoloControllo...
036100110719     D                                 N   CONST
036200110719     D  priUnSoloCommit...
036300110719     D                                 N   CONST
036400110719     D  priLotto                      9A   CONST
036500110719     D                                     OPTIONS(*NOPASS)
036600110719
036700110720     D*--------------------------------------------------
036800110720     D* Procedure name: YPJBH_SetDescrizioneBreveMovimento
036900110720     D* Purpose:        Imposta la descrizione breve del movimento.
037000110720     D* Returns:        Esito.
037100110720     D* Parameter:      priDescrizioneBreve
037200110720     D*--------------------------------------------------
037300110720     D YPJBH_SetDescrizioneBreveMovimento...
037400110720     D                 PR            10I 0
037500110720     D  priDescrizioneBreve...
037600110720     D                                     LIKE(andiz00f.desBrev)
037700110720     D                                     CONST
037800110720
037900120516     D*--------------------------------------------------
038000120516     D* Procedure name: YPJBH_SetDocumento
038100120516     D* Purpose:        Imposta data, numero e serie documento.
038200120516     D* Returns:        Esito.
038300120516     D* Parameter:      priDataDocumento => Data documento.
038400120516     D* Parameter:      priNumeroDocumento => Numero documento.
038500120516     D* Parameter:      priSerieDocumento => Serie documento.
038600120516     D*--------------------------------------------------
038700120516     D YPJBH_SetDocumento...
038800120516     D                 PR            10I 0
038900120516     D  priDataDocumento...
039000120516     D                                 D   DATFMT(*ISO)
039100120516     D                                     CONST
039200120516     D  priNumeroDocumento...
039300120516     D                                9P 0 CONST
039400120516     D  priSerieDocumento...
039500120516     D                                4A   CONST
039501151202
039502151202     D*--------------------------------------------------
039503151202     D* Procedure name: YPJBH_SetPartita
039504151202     D* Purpose:        Imposta data, numero e serie partita.
039505151202     D* Returns:        Esito.
039506151202     D* Parameter:      priDataPartita => Data partita.
039507151202     D* Parameter:      priNumeroPartita => Numero partita.
039508151202     D* Parameter:      priSeriePartita => Serie partita.
039509151202     D*--------------------------------------------------
039510151202     D YPJBH_SetPartita...
039511151202     D                 PR            10I 0
039512151202     D  priDataPartita...
039513151202     D                                 D   DATFMT(*ISO)
039514151202     D                                     CONST
039515151202     D  priNumeroPartita...
039516151202     D                                9P 0 CONST
039517151202     D  priSeriePartita...
039518151202     D                                4A   CONST
039600120516
039700120516     D*--------------------------------------------------
039800120516     D* Procedure name: YPJBH_SetCategoriaPartita
039900120516     D* Purpose:        Imposta le categorie della partita.
040000120516     D* Returns:        Esito.
040100120516     D* Parameter:      priCtgAna1 => Categoria 1.
040200120516     D* Parameter:      priCtgAna2 => Categoria 2.
040300120516     D* Parameter:      priCtgAna3 => Categoria 3.
040400120516     D*--------------------------------------------------
040500120516     D YPJBH_SetCategoriaPartita...
040600120516     D                 PR            10I 0
040700120516     D  priCtgAna1                    4A   CONST
040800120516     D                                     OPTIONS(*OMIT)
040900120516     D  priCtgAna2                    4A   CONST
041000120516     D                                     OPTIONS(*NOPASS:*OMIT)
041100120516     D  priCtgAna3                    4A   CONST
041200120516     D                                     OPTIONS(*NOPASS:*OMIT)
041300120516
041400120517     D*--------------------------------------------------
041500120517     D* Procedure name: YPJBH_SetContropartitaUtente
041600120517     D* Purpose:        Imposta la contropartita utente sul movimento.
041700120517     D* Returns:        Esito.
041800120517     D* Parameter:      priCapoContoContropartita => Capo conto controparti...
041900120517     D*                          ta utente
042000120517     D* Parameter:      priSottoContoContropartita => Sotto conto contropar...
042100120517     D*                          tita utente
042200120517     D*--------------------------------------------------
042300120517     D YPJBH_SetContropartitaUtente...
042400120517     D                 PR            10I 0
042500120517     D  priCapoContoContropartita...
042600120517     D                                6A   CONST
042700120517     D  priSottoContoContropartita...
042800120517     D                                8A   CONST
042900120517
043000121105     D*--------------------------------------------------
043100121105     D* Procedure name: YPJBH_SetBloccoPartita
043200121105     D* Purpose:        Imposta il blocco pagamento partita.
043300121105     D* Returns:        Esito.
043400121105     D* Parameter:      priBkAmm => Blocco amministrativo partita.
043500121105     D*--------------------------------------------------
043600121105     D YPJBH_SetBloccoPartita...
043700121105     D                 PR            10I 0
043800121105     D  priBkAmm                      2A   CONST
043801151202
043900151202     D* --------------------------------------------------
043901151202     D* Prototype for procedure: YPJBH_NewMovimentoIVA
043902151202     D* --------------------------------------------------
043903151202     D YPJBH_NewMovimentoIVA...
043904151202     D                 PR            10I 0
043906151202     D  priDareAvere                  1A   VALUE
043907151202     D  priImponibileEUR...
043908151202     D                               15P 2 VALUE
043909151202     D  priImponibileDivisa...
043910151202     D                               15P 2 VALUE
043911151202     D  priImpostaEUR                15P 2 VALUE
043912151202     D  priImpostaDivisa...
043913151202     D                               15P 2 VALUE
043915151202     D priAliquotaIVA                 5P 2 CONST
043916151202     D  priRiferimentoIVA...
043917151202     D                                3A   CONST
043921161202     D  priCausRigIVA...                                                        9
043922161202     D                                4A   CONST
043923161202
043924151202     D* --------------------------------------------------
043925151202     D* Prototype for procedure: YPJBH_InsMovimentoIVA
043926151202     D* --------------------------------------------------
043927151202     D YPJBH_InsMovimentoIVA...
043928151202     D                 PR            10I 0
043929151202
043930151202     D* --------------------------------------------------
043931151202     D* Prototype for procedure: YPJBH_EndMovimentoIVA
043932151202     D* --------------------------------------------------
043933151202     D YPJBH_EndMovimentoIVA...
043934151202     D                 PR            10I 0
043935151202
043936170213     D*--------------------------------------------------
043937170213     D* Procedure name: YPJBH_SetCausaleRigaMovimento...
043938170213     D* Purpose:        Imposta la descrizione breve del movimento.
043939170213     D* Returns:        Esito.
043940170213     D* Parameter:      priDescrizioneBreve
043941170213     D*--------------------------------------------------
043942170213     D YPJBH_SetCausaleRigaMovimento...
043943170213     D                 PR            10I 0
043944170213     D YPJBH_SetCausaleRigaMovimento...
043945170213     D                                4    CONST
043947170213
043948170509     D*--------------------------------------------------
043949170509     D* Procedure name: YPJBH_SetFunzioneGenerante...
043950170509     D* Purpose:        Imposta la funzione generante
043951170509     D* Returns:        Esito.
043952170509     D* Parameter:      priFunzioneGenerante
043953170509     D*--------------------------------------------------
043954170509     D YPJBH_SetFunzioneGenerante...
043955170509     D                 PR            10I 0
043956170509     D priFunzioneGenerante...
043957170509     D                                1    CONST
043958170509
043959171026     P*--------------------------------------------------
043970171026     P* Procedure name: YPJBH_GetMessaggio...
043971171026     D* Purpose:        decodifico il messaggio
043972171026     P* Returns:
043973171026     P* Parameter:      priFunzioneGenerante
043974171026     P*--------------------------------------------------
043977171026     D YPJBH_GetMessaggio...
043978171026     D                 Pr           100    varying
043979171026     D  pi_MessageNumber...
043980171026     D                               10i 0 CONST
043981171026
