000100120508     ***************************************************************************
000200120508     **
000300120508     ** Questo modulo gestisce l'inserimento, l'aggiornamento e la cancellazione
000400120508     ** di una pratica di contenzioso cliente.
000500120508     **
000600120508     ***************************************************************************
000700120716     H NOMAIN BNDDIR('PJXBND':'TIBS':'QC2LE':'YCO':'YCOCTZSEL') EXTBININT(*YES)
000800120509
000900120508     ***************************************************************************
001000120508     **
001100120508     ** Strutture dati.
001200120508     **
001300120508     ***************************************************************************
001400120706     D ana723ds      E DS                  QUALIFIED INZ
001500120706     D  cmt723       E                     INZ(*OFF)
001600120706     D andiz00f      E DS                  QUALIFIED TEMPLATE
001700120514     D done            DS                  QUALIFIED INZ
001800120509     D  init                           N
001900120509     D  newPratica                     N
002000120711     D ds4w          E DS                  QUALIFIED
002100120711     D  tblUni                 1     89A
002200120711     D kpjba         E DS                  QUALIFIED INZ
002300120515     D ndbhm00f      E DS                  INZ QUALIFIED
002400120515     D  bhmCausTes   E                     INZ('CONT')
002500120515     D  bhmUnita     E                     INZ('505')
002600120515     D  bhmDtReg     E                     INZ(*SYS)
002700120515     D  bhmDtDoc     E                     INZ(*SYS)
002800120515     D ndc071ds      E DS                  QUALIFIED INZ
002900120515     D  solCont071   E                     INZ(*OFF)
003000120515     D  interat071   E                     INZ(*OFF)
003100120515     D  unoCmt071    E                     INZ(*OFF)
003200120509     D partite         DS                  QUALIFIED DIM(32767)
003300120509     D                                     BASED(partite_ptr)
003400120509     D  idSocieta                     3A
003500120509     D  contabilita                   2A
003600120509     D  capoConto                     6A
003700120509     D  sottoConto                    8A
003800120509     D  data                           D
003900120509     D  numero                        9P 0
004000120509     D  serie                         4A
004100120509     D  saldoEuro                    15P 2
004200120515     D  segno                         1A
004300120713     D  esposizioneEuro...
004400120713     D                               15P 2
004500121026     D  scadenza                       D
004600120509     D pratica         DS                  QUALIFIED INZ
004700120509     D  operazione                   10I 0
004800120509     D  p                            10I 0                                      Contatore partite
004900120510     D  sysProj                       9P 0
005000120510     D  nAssolutoProj                 9P 0
005100120511     D  capoContoCliente...
005200120511     D                                6A   INZ(PROJ_SOTTONATURA_CLIENTE)
005300130313     D psds           SDS                  QUALIFIED
005400130313     D  pgmName          *PROC
005500130313     D xChkAutDs     E DS                  QUALIFIED INZ
005600120713     D  xcaFnc       E                     INZ('YCTZ00')
005700120713     D  xcaVfu       E                     INZ(*ZERO)
005800120713     D  xcaTck       E                     INZ('CK')
005900120713     D xSoc001Ds     E DS                  QUALIFIED INZ
006000120622     D yCtzPra0f     E DS                  QUALIFIED INZ(*EXTDFT)
006100120619     D yCtzPra0f_ind   DS                  QUALIFIED INZ
006200120619     D  usrPrfUpd                     5I 0
006300120619     D  usrPrfDlt                     5I 0
006400120619     D  usrPrfRst                     5I 0
006500120619     D  dtUpd                         5I 0
006600120619     D  dtDlt                         5I 0
006700120619     D  dtRst                         5I 0
006800120619     D  partitaIva                    5I 0
006900120619     D  codFiscale                    5I 0
007000120619     D  dtContenzF                    5I 0
007100120619     D yCtzSta0f     E DS                  QUALIFIED INZ(*EXTDFT)
007200120508
007300120508     ***************************************************************************
007400120508     **
007500120508     ** Campi.
007600120508     **
007700120508     ***************************************************************************
007800120509     D esito           S             10I 0
007900120510     D esitoProj       S              1A
008000120510     D xnmAut          S              1A
008100120509
008200120509     ***************************************************************************
008300120509     **
008400120509     ** Costanti.
008500120509     **
008600120509     ***************************************************************************
008700120622      /COPY GAITRASRC/SRCCONST,YCOCTZ
008800120622      /COPY GAITRASRC/SRCCONST,YCOCTZOUT
008900120622      /COPY GAITRASRC/SRCCONST,YPJBH0000
009000130313     D*BLOCCO_PAGAMENTO_CONTENZIOSO...
009100130313     D*                C                   'CZ'
009101140923     D CLIENTE_ESCLUSO_DA_CONTENZIOSO...
009102140923     D                 C                   'E'
009200120515     D COMMIT...
009300120515     D                 C                   *ON
009400120514     D PROJ_AVERE...
009500120510     D                 C                   'A'
009600120706     D PROJ_COMMENTO_CLIENTE...
009700120706     D                 C                   '700'
009800120706     D PROJ_COMMENTO_INSERT...
009900120706     D                 C                   '01'
010000120510     D PROJ_DARE...
010100120510     D                 C                   'D'
010200120510     D PROJ_ESITO_OK...
010300120510     D                 C                   '0'
010400120515     D PROJ_RICHIESTA_CHAIN...
010500120515     D                 C                   '1'
010600120511     D PROJ_SOTTONATURA_CLIENTE...
010700120511     D                 C                   'C'
010800120515     D ROLLBACK...
010900120515     D                 C                   *OFF
011000120516     D SERIE...
011100120516     D                 C                   'CTZ'
011200120509     D SQLCODE_ROW_IN_USE...
011300120509     D                 C                   -913
011400120509     D SQLCODE_ROW_NOT_FOUND...
011500120509     D                 C                   100
011600120515     D STATO_CREDITO_CONTENZIOSO...
011700120515     D                 C                   ' 41'
011800120509
011900120508     ***************************************************************************
012000120508     **
012100120508     ** Prototipi.
012200120508     **
012300120508     ***************************************************************************
012400120706      /COPY GAITRASRC/SRCPROTOPR,ANA723R
012500120706      /COPY GAITRASRC/SRCPROTOPR,PROJ_ILE
012600120622      /COPY GAITRASRC/SRCPROTOPR,QSORT
012700120622      /COPY GAITRASRC/SRCPROTOPR,TIBSAZNMR
012800120713      /COPY GAITRASRC/SRCPROTOPR,XCHKAUT
012900120713      /COPY GAITRASRC/SRCPROTOPR,XSOC
013000120622      /COPY GAITRASRC/SRCPROTOPR,YCOCTZOUT
013100120622      /COPY GAITRASRC/SRCPROTOPR,YCOCTZSEL
013200120622      /COPY GAITRASRC/SRCPROTOPR,YPJBH0000
013300130313      /COPY GAITRASRC/SRCPROTOPR,YCO111R
013400130313
013500120509     D*--------------------------------------------------
013600120510     D* Procedure name: YCOCTZOUT_RunClienti
013700120509     D* Purpose:        Inserisce le righe dei clienti passati in contenzioso.
013800120509     D* Returns:        Esito.
013900120509     D*--------------------------------------------------
014000120510     D YCOCTZOUT_RunClienti...
014100120509     D                 PR            10I 0
014200120711     D priRqsOpCode                  10I 0 CONST OPTIONS(*NOPASS)
014300120711
014400120509     D*--------------------------------------------------
014500120509     D* Procedure name: SortArrayPartite
014600120509     D* Purpose:        Ordina la schiera delle partite.
014700120509     D* Returns:        Esito.
014800120509     D* Parameter:      parm1
014900120509     D* Parameter:      parm2
015000120509     D*--------------------------------------------------
015100120509     D SortArrayPartite...
015200120509     D                 PR            10I 0
015300120509     D  parm1                              LIKE(partite)
015400120509     D  parm2                              LIKE(partite)
015500120510
015600120510     D*--------------------------------------------------
015700120510     D* Procedure name: YCOCTZOUT_RunRegistrazione
015800120510     D* Purpose:        Inserisce in Proj la registrazione di accentramento.
015900120510     D* Returns:        Esito.
016000120510     D*--------------------------------------------------
016100120510     D YCOCTZOUT_RunRegistrazione...
016200120510     D                 PR            10I 0
016300120511
016400120511     D*--------------------------------------------------
016500120511     D* Procedure name: YCOCTZOUT_SetPartitaInContenzioso
016600120511     D* Purpose:        Imposta NDPPS00F.PPSLEGALE = *ON
016700120511     D* Returns:        Esito.
016800120511     D* Parameter:      priSocieta => ID società.
016900120511     D* Parameter:      priCtb => Contabilità
017000120511     D* Parameter:      priKcc => Capo conto.
017100120511     D* Parameter:      priKsc => Sotto conto.
017200120511     D* Parameter:      priDtPar => Data partita.
017300120511     D* Parameter:      priNrPar => Numero partita.
017400120511     D* Parameter:      priSerPar => Serie partita.
017500120511     D* Parameter:      priLegale
017600120511     D*--------------------------------------------------
017700120511     D YCOCTZOUT_SetPartitaInContenzioso...
017800120511     D                 PR            10I 0
017900120511     D  priSocieta                    3A
018000120511     D  priCtb                        2A
018100120511     D  priKcc                        6A
018200120511     D  priKsc                        8A
018300120511     D  priDtPar                       D   DATFMT(*ISO)
018400120511     D  priNrPar                      9P 0
018500120511     D  priSerPar                     4A
018600120511     D  priLegale                      N   CONST
018700120509
018800120509
018900120509     P*--------------------------------------------------
019000120509     P* Procedure name: YCOCTZOUT_Init
019100120509     P* Purpose:        Inizializza il modulo.
019200120509     P* Returns:        Esito.
019300120509     P* Parameter:      priKpjba
019400120509     P*--------------------------------------------------
019500120509     P YCOCTZOUT_Init  B                   EXPORT
019600120509     D YCOCTZOUT_Init  PI            10I 0
019700120509     D  priKpjba                           LIKEDS(kpjba)
019800120509
019900120713     D retField        S             10I 0 STATIC
020000120713
020100120509      /FREE
020200120509
020300120622       IF done.init;
020400120622         RETURN YCOCTZ_ESITO_OK;
020500120622       ENDIF;
020600120622
020700120622       kpjba = priKpjba;
020800120713
020900120713       Proj_Societa( 'SOC001'
021000120713                   : xSoc001ds.xscSoc
021100120713                   : *ZERO
021200120713                   : *BLANK
021300120713                   : esitoProj
021400120713                   : xSoc001ds
021500120713                   : kpjba
021600120713                   );
021700120713
021800120622       Aznmr_init();
021900120622       YPJBH_Init( kpjba );
022000120622
022100120622       ndbhm00f.bhmRifInt = SERIE + %SUBST(%EDITC(kpjba.knraz : 'X') : 4);
022200120713
022300120713       done.init = *ON;
022400120509
022500120509       RETURN YCOCTZ_ESITO_OK;
022600120509
022700120509      /END-FREE
022800120509     P YCOCTZOUT_Init  E
022900120509
023000120509
023100120509     P*--------------------------------------------------
023200120509     P* Procedure name: YCOCTZOUT_Finalize
023300120509     P* Purpose:        Chiude il modulo.
023400120509     P* Returns:
023500120509     P*--------------------------------------------------
023600120509     P YCOCTZOUT_Finalize...
023700120509     P                 B                   EXPORT
023800120509     D YCOCTZOUT_Finalize...
023900120509     D                 PI
024000120509
024100120509      /FREE
024200120509
024300120509       RESET done;
024400120509
024500120509      /END-FREE
024600120509     P YCOCTZOUT_Finalize...
024700120509     P                 E
024800120509
024900120509
025000120509     P*--------------------------------------------------
025100120509     P* Procedure name: YCOCTZOUT_NewPratica
025200120509     P* Purpose:        Inizia a lavorare con una pratica di contenzioso.
025300120509     P* Returns:        Esito.
025400120509     P* Parameter:      priOperazione => Operazione (inserire, modificare, ...
025500120509     P*                          annullare, ripristinare, cancellare).
025600120509     P* Parameter:      priIdContenzioso => ID contenzioso.
025700120509     P*--------------------------------------------------
025800120509     P YCOCTZOUT_NewPratica...
025900120509     P                 B                   EXPORT
026000120509     D YCOCTZOUT_NewPratica...
026100120509     D                 PI            10I 0
026200120509     D  priOperazione                10I 0 CONST
026300120510     D  priIdSocieta                  3A   CONST
026400120509     D  priIdContenzioso...
026500120509     D                               10I 0 OPTIONS(*NOPASS)
026600120509
026700120509      /FREE
026800120509
026900120509       IF done.newPratica;
027000120509         RETURN YCOCTZ_ERRORE_PRATICA_GIA_INIZIATA;
027100120509       ENDIF;
027200120509
027300120509       IF priOperazione <> YCOCTZOUT_OPERAZIONE_INSERIRE AND
027400120509          priOperazione <> YCOCTZOUT_OPERAZIONE_AGGIORNARE AND
027500120509          priOperazione <> YCOCTZOUT_OPERAZIONE_ANNULLARE AND
027600120509          priOperazione <> YCOCTZOUT_OPERAZIONE_RIPRISTINARE AND
027700120711          priOperazione <> YCOCTZOUT_OPERAZIONE_CANCELLARE AND
027800120711          priOperazione <> YCOCTZOUT_OPERAZIONE_NUOVO_STATO;
027900120711         RETURN YCOCTZ_ERRORE_OPERAZIONE_SCONOSCIUTA;
028000120509       ENDIF;
028100120509
028200120509       IF priOperazione <> YCOCTZOUT_OPERAZIONE_INSERIRE AND
028300120509          (%PARMS() < 2 OR priIdContenzioso = *ZERO);
028400120509         RETURN YCOCTZ_ERRORE_PARAMETRI;
028500120509       ENDIF;
028600120713
028700120713       // Controllo che l'utente sia autorizzato al contenzioso.
028800120713
028900120713       RESET xChkAutDs;
029000120713       xChkAutDs.xcaSoc = xSoc001Ds.xscSoc;
029100120713       xChkAutDs.xcaGrp = xSoc001Ds.xscGrp;
029200120713       xChkAutDs.xcaPrf = kpjba.knmus;
029300120713       Proj_Autorizzazione( xChkAutDs );
029400120713
029500120713       IF xChkAutDs.xcaErr <> *OFF;
029600120713         RETURN YCOCTZ_ERRORE_AUTORIZZAZIONE;
029700120713       ENDIF;
029800120509
029900120509       RESET yCtzPra0f;
030000120509
030100120509       IF priOperazione <> YCOCTZOUT_OPERAZIONE_INSERIRE;
030200120509
030300120619         EXEC SQL
030400120619           SELECT id_riga
030500120619                , profilo_utente_insert
030600120619                , profilo_utente_update
030700120619                , profilo_utente_delete
030800120619                , profilo_utente_restore
030900120619                , data_insert
031000120619                , data_update
031100120619                , data_delete
031200120619                , data_restore
031300120619                , partita_iva
031400120619                , codice_fiscale
031500120619                , id_cliente_societa
031600120619                , id_cliente_ksc
031700120619                , id_contenzioso
031800120619                , data_inizio_contenzioso
031900120619                , data_fine_contenzioso
032000120619             INTO :yCtzPra0f.idRiga
032100120619                , :yCtzPra0f.usrPrfIns
032200120619                , :yCtzPra0f.usrPrfUpd :yCtzPra0f_ind.usrPrfUpd
032300120619                , :yCtzPra0f.usrPrfDlt :yCtzPra0f_ind.usrPrfDlt
032400120619                , :yCtzPra0f.usrPrfRst :yCtzPra0f_ind.usrPrfRst
032500120619                , :yCtzPra0f.dtIns
032600120619                , :yCtzPra0f.dtUpd :yCtzPra0f_ind.dtUpd
032700120619                , :yCtzPra0f.dtDlt :yCtzPra0f_ind.dtDlt
032800120619                , :yCtzPra0f.dtRst :yCtzPra0f_ind.dtRst
032900120619                , :yCtzPra0f.partitaIva :yCtzPra0f_ind.partitaIva
033000120619                , :yCtzPra0f.codFiscale :yCtzPra0f_ind.codFiscale
033100120619                , :yCtzPra0f.idCliSoc
033200120619                , :yCtzPra0f.idCliKsc
033300120619                , :yCtzPra0f.idContenz
033400120619                , :yCtzPra0f.dtContenzI
033500120619                , :yCtzPra0f.dtContenzF :yCtzPra0f_ind.dtContenzF
033600120619             FROM yCtzPra0f
033700120619             WHERE id_contenzioso = :priIdContenzioso
033800120619             WITH RS
033900120619         ;
034000120619
034100120509         SELECT;
034200120509           WHEN sqlCode = SQLCODE_ROW_NOT_FOUND;
034300120509             RETURN YCOCTZ_ERRORE_PRATICA_NON_TROVATA;
034400120509           WHEN sqlCode = SQLCODE_ROW_IN_USE;
034500120509             RETURN YCOCTZ_ERRORE_PRATICA_ALLOCATA;
034600120509           WHEN sqlCode < *ZERO;
034700120509             DUMP(A);
034800120509             RETURN sqlCode;
034900120509         ENDSL;
035000120509
035100120509       ENDIF;
035200120510
035300120522       RESET yCtzSta0f;
035400120509       RESET pratica;
035500120509       pratica.operazione = priOperazione;
035600120509       done.newPratica = *ON;
035700120509       RETURN YCOCTZ_ESITO_OK;
035800120509
035900120509      /END-FREE
036000120509     P YCOCTZOUT_NewPratica...
036100120509     P                 E
036200120509
036300120509
036400120509     P*--------------------------------------------------
036500120509     P*
036600120509     P* Procedure name: YCOCTZOUT_SetSoggetto
036700120509     P* Purpose:        Imposta il soggetto del contenzioso.
036800120509     P* Returns:        Esito.
036900120509     P* Parameter:      priPartitaIVA => Partita IVA.
037000120509     P* Parameter:      priCodiceFiscale => Codice fiscale.
037100120509     P* Parameter:      priIdClienteSocieta => ID società cliente.
037200120509     P* Parameter:      priIdClienteKsc => ID cliente accentratore.
037300120509     P*--------------------------------------------------
037400120509     P YCOCTZOUT_SetSoggetto...
037500120509     P                 B                   EXPORT
037600120509     D YCOCTZOUT_SetSoggetto...
037700120509     D                 PI            10I 0
037800120509     D  priPartitaIVA                20A
037900120509     D  priCodiceFiscale...
038000120509     D                               16A
038100120509     D  priIdClienteSocieta...
038200120509     D                                3A
038300120509     D  priIdClienteKsc...
038400120509     D                                8A
038500120509
038600120509     D retField        S             10I 0 STATIC
038700120509
038800120509      /FREE
038900120509
039000120509       RESET retField;
039100120509
039200120509       IF NOT done.newPratica;
039300120509         RETURN YCOCTZ_ERRORE_PRATICA_NON_INIZIATA;
039400120509       ENDIF;
039500120509
039600120509       IF priIdClienteSocieta = *BLANK
039700120509       OR priIdClienteKsc = *BLANK;
039800120509         RETURN YCOCTZ_ERRORE_PARAMETRI;
039900120509       ENDIF;
040000120509
040100120509       IF priPartitaIVA = *BLANK AND priCodiceFiscale = *BLANK;
040200120509         RETURN YCOCTZ_ERRORE_PARAMETRI;
040300120509       ENDIF;
040400120509
040500120509       // Inserire qui il richiamo ai metodi di controllo dei dati ricevuti.
040600120509       // YCOCTZCHK_ChkClienteAccentratore()
040700120509       // YCOCTZCHK_ChkPartitaIVA()
040800120509       // YCOCTZCHK_ChkCodiceFiscale()
040900120509
041000120509       yCtzPra0f.partitaIVA = %TRIMR(priPartitaIVA);
041100120509       yCtzPra0f.codFiscale = %TRIMR(priCodiceFiscale);
041200120509       yCtzPra0f.idCliSoc = priIdClienteSocieta;
041300120509       yCtzPra0f.idCliKsc = priIdClienteKsc;
041400120509
041500120509       IF pratica.operazione = YCOCTZOUT_OPERAZIONE_AGGIORNARE;
041600120509
041700120509         EXEC SQL
041800120509           UPDATE yCtzPra0f
041900120509             SET profilo_utente_update = RTRIM(:kpjba.knmus)
042000120509               , data_update = CURRENT DATE
042100120509               , partita_iva = :yCtzPra0f.partitaIVA
042200120509               , codice_fiscale = :yCtzPra0f.codFiscale
042300120509               , id_cliente_societa = :yCtzPra0f.idCliSoc
042400120509               , id_cliente_ksc = :yCtzPra0f.idCliKsc
042500120509             WHERE id_contenzioso = :yCtzPra0f.idContenz
042600120509             WITH RS
042700120509         ;
042800120509
042900120509         SELECT;
043000120509           WHEN sqlCode = SQLCODE_ROW_NOT_FOUND;
043100120509             RETURN YCOCTZ_ERRORE_PRATICA_NON_TROVATA;
043200120509           WHEN sqlCode = SQLCODE_ROW_IN_USE;
043300120509             RETURN YCOCTZ_ERRORE_PRATICA_ALLOCATA;
043400120509           WHEN sqlCode < *ZERO;
043500120509             DUMP(A);
043600120509             RETURN sqlCode;
043700120509         ENDSL;
043800120509
043900120509       ENDIF;
044000120511
044100120509       RETURN retField;
044200120509
044300120509      /END-FREE
044400120509     P YCOCTZOUT_SetSoggetto...
044500120509     P                 E
044600120509
044700120509
044800120509     P*--------------------------------------------------
044900120509     P* Procedure name: YCOCTZOUT_EndPratica
045000120509     P* Purpose:        Finisce di lavorare con una pratica.
045100120509     P* Returns:        >0 = ID contenzioso; <0 = esito.
045200120509     P* Parameter:      priCommit => *ON = Commit; *OFF = RollBack.
045300120509     P*--------------------------------------------------
045400120509     P YCOCTZOUT_EndPratica...
045500120509     P                 B                   EXPORT
045600120509     D YCOCTZOUT_EndPratica...
045700120509     D                 PI            10I 0
045800120509     D  priCommit                      N   CONST
045900120509
046000120509     D retField        S             10I 0 STATIC
046100120724     D local           DS                  STATIC QUALIFIED INZ
046200120724     D  idContenziosoGiaUsato...
046300120724     D                                 N
046400120509
046500120724      /FREE
046600120509
046700120509       RESET retField;
046800120509
046900120509       IF NOT done.newPratica;
047000120509         RETURN YCOCTZ_ERRORE_PRATICA_NON_INIZIATA;
047100120509       ENDIF;
047200120509
047300120509       IF NOT priCommit;
047400120509         EXEC SQL ROLLBACK HOLD;
047500120509         RETURN YCOCTZ_ESITO_OK;
047600120509       ENDIF;
047700120511
047800120724       RESET local;
047900120724
048000120511       // Reperisco il capo conto del cliente accentratore.
048100120511
048200120511       RESET pratica.capoContoCliente;
048300120511
048400120511       Proj_GetCapocontoClienteFornitore( yCtzPra0f.idCliSoc
048500120511                                        : pratica.capoContoCliente
048600120511                                        : yCtzPra0f.idCliKsc
048700120511                                        );
048800120509
048900120509       SELECT;
049000120509
049100120509         WHEN pratica.operazione = YCOCTZOUT_OPERAZIONE_INSERIRE;
049200120509
049300120724           DOU NOT local.idContenziosoGiaUsato;
049400120724
049500120724             yCtzPra0f.idContenz = Aznmr_getNumeroN( SERIE
049600120724                                                   : *OMIT
049700120724                                                   : *OMIT
049800120724                                                   : *OMIT
049900120724                                                   : *OMIT
050000120724                                                   : esito
050100120724                                                   );
050200120724
050300120724             RESET local.idContenziosoGiaUsato;
050400120724
050500120724             EXEC SQL
050600120724               SELECT '1'
050700120724                 INTO :local.idContenziosoGiaUsato
050800120724                 FROM yCtzPra0f
050900120724                 WHERE id_contenzioso = :yCtzPra0f.idContenz
051000120724             ;
051100120509
051200120724             IF sqlCode = SQLCODE_ROW_NOT_FOUND;
051300120724               LEAVE;
051400120724             ENDIF;
051500120724
051600120724           ENDDO;
051700120724
051800120509           EXEC SQL
051900120509             INSERT INTO yCtzPra0f
052000120509                       ( profilo_utente_insert
052100120509                       , partita_iva
052200120509                       , codice_fiscale
052300120509                       , id_cliente_societa
052400120509                       , id_cliente_ksc
052500120509                       , id_contenzioso
052600120509                       )
052700120509               VALUES  ( RTRIM(:kpjba.knmus)
052800120509                       , :yCtzPra0f.partitaIVA
052900120509                       , :yCtzPra0f.codFiscale
053000120509                       , :yCtzPra0f.idCliSoc
053100120509                       , :yCtzPra0f.idCliKsc
053200120509                       , :yCtzPra0f.idContenz
053300120509                       )
053400120509               WITH RS
053500120509           ;
053600120515
053700120509         WHEN pratica.operazione = YCOCTZOUT_OPERAZIONE_CANCELLARE;
053800120509
053900120509           EXEC SQL
054000120725             DELETE FROM yCtzPra0f
054100120509               WHERE id_contenzioso = :yCtzPra0f.idContenz
054200120509               WITH RS
054300120509           ;
054400120509
054500120509         WHEN pratica.operazione = YCOCTZOUT_OPERAZIONE_ANNULLARE;
054600120509
054700120509           EXEC SQL
054800120509             UPDATE yCtzPra0f
054900120509               SET profilo_utente_delete = RTRIM(:kpjba.knmus)
055000120509                 , data_delete = CURRENT DATE
055100120509               WHERE id_contenzioso = :yCtzPra0f.idContenz
055200120509               WITH RS
055300120509           ;
055400120509
055500120509         WHEN pratica.operazione = YCOCTZOUT_OPERAZIONE_RIPRISTINARE;
055600120509
055700120509           EXEC SQL
055800120509             UPDATE yCtzPra0f
055900120509               SET profilo_utente_delete = NULL
056000120509                 , profilo_utente_restore = RTRIM(:kpjba.knmus)
056100120509                 , data_delete = NULL
056200120509                 , data_restore = CURRENT DATE
056300120509               WHERE id_contenzioso = :yCtzPra0f.idContenz
056400120509               WITH RS
056500120509           ;
056600120509
056700120509       ENDSL;
056800120509
056900120509       SELECT;
057000120509         WHEN sqlCode = SQLCODE_ROW_NOT_FOUND;
057100120509           retField = YCOCTZ_ERRORE_PRATICA_NON_TROVATA;
057200120509         WHEN sqlCode = SQLCODE_ROW_IN_USE;
057300120509           retField = YCOCTZ_ERRORE_PRATICA_ALLOCATA;
057400120509         WHEN sqlCode < *ZERO;
057500120509           DUMP(A);
057600120509           retField = sqlCode;
057700120509       ENDSL;
057800120509
057900120711       IF pratica.operazione = YCOCTZOUT_OPERAZIONE_INSERIRE
058000120711       OR pratica.operazione = YCOCTZOUT_OPERAZIONE_AGGIORNARE;
058100120711
058200120711         // Inserimento della registrazione cliente accentratore a clienti.
058300120511
058400120711         IF retField = YCOCTZ_ESITO_OK;
058500120711           retField = YCOCTZOUT_RunRegistrazione();
058600120711         ENDIF;
058700120511
058800120711         // Inserimento dei codici clienti passati in contenzioso.
058900120509
059000120711         IF retField = YCOCTZ_ESITO_OK;
059100120711           retField = YCOCTZOUT_RunClienti();
059200120711         ENDIF;
059300120509
059400120711         // E' andato tutto bene?
059500120509
059600120711         IF retField >= YCOCTZ_ESITO_OK;
059700120711           EXEC SQL COMMIT HOLD;
059800120711           RESET ndc071ds;
059900120711           ndc071ds.societa071 = yctzpra0f.idCliSoc;
060000120711           ndc071ds.rifInt071 = ndbhm00f.bhmRifInt;
060100120711           ndc071ds.dtRegDa071 = ndbhm00f.bhmDtReg;
060200120711           ndc071ds.dtRegA071 = ndbhm00f.bhmDtReg;
060300120711           ndc071ds.subNumD071 = yCtzPra0f.idContenz;
060400120711           ndc071ds.subNumA071 = yCtzPra0f.idContenz;
060500120711           kpjba.kpjbu = ndc071ds;
060600120711           Proj_PrimaNotaBatch(kpjba);
060700120711           ndc071ds = kpjba.kpjbu;
060800120711         ENDIF;
060900120509
061000120711       ELSE;
061100120711
061200120711         // E' andato tutto bene?
061300120711
061400120711         IF retField < YCOCTZ_ESITO_OK;
061500120711           EXEC SQL ROLLBACK HOLD;
061600120711         ELSE;
061700120711           EXEC SQL COMMIT HOLD;
061800120711           retField = yCtzPra0f.idContenz;
061900120711         ENDIF;
062000120711
062100120711       ENDIF;
062200120711
062300120509       done.newPratica = *OFF;
062400120509       RESET pratica;
062500120619       DEALLOC(N) partite_ptr;
062600120509       RETURN retField;
062700120509
062800120509      /END-FREE
062900120509     P YCOCTZOUT_EndPratica...
063000120509     P                 E
063100120509
063200120509
063300120509     P*--------------------------------------------------
063400120509     P* Procedure name: YCOCTZOUT_AddPartitaAlContenzioso
063500120509     P* Purpose:        Aggiunge una partita contabile al contenzioso.
063600120509     P* Returns:        Esito.
063700120509     P* Parameter:      priIdSocieta => ID società.
063800120509     P* Parameter:      priCapoConto => Capo conto.
063900120509     P* Parameter:      priSottoConto => Sotto conto.
064000120509     P* Parameter:      priDataPartita => Data partita.
064100120509     P* Parameter:      priNumeroPartita => Numero partita.
064200120509     P* Parameter:      priSeriePartita => Serie partita.
064300120509     P* Parameter:      priSaldoPartitaEuro => Saldo partita in euro (>0 = ...
064400120509     P*                          dare; <0 = avere).
064500120509     P*--------------------------------------------------
064600120509     P YCOCTZOUT_AddPartitaAlContenzioso...
064700120509     P                 B                   EXPORT
064800120509     D YCOCTZOUT_AddPartitaAlContenzioso...
064900120509     D                 PI            10I 0
065000120509     D  priIdSocieta                  3A
065100120509     D  priContabilita...
065200120509     D                                2A
065300120509     D  priCapoConto                  6A
065400120509     D  priSottoConto                 8A
065500120509     D priDataPartita                  D   DATFMT(*ISO)
065600120509     D  priNumeroPartita...
065700120509     D                                9P 0
065800120509     D  priSeriePartita...
065900120509     D                                4A
066000120509     D  priSaldoPartitaEuro...
066100120509     D                               15P 2
066200120713     D  priEsposizionePartitaEuro...
066300120713     D                               15P 2
066400121026     D  priDataScadenza...
066500121026     D                                 D   DATFMT(*ISO)
066600120509
066700120509      /FREE
066800120509
066900120509       IF NOT done.newPratica;
067000120509         RETURN YCOCTZ_ERRORE_PRATICA_NON_INIZIATA;
067100120509       ENDIF;
067200120509
067300120509       IF priIdSocieta = *BLANK OR priCapoConto = *BLANK
067400120509       OR priSottoConto = *BLANK OR priDataPartita = *LOVAL
067500120509       OR priNumeroPartita = *ZERO;
067600120509         RETURN YCOCTZ_ERRORE_PARAMETRI;
067700120509       ENDIF;
067800120509
067900120509       pratica.p += 1;
068000120509
068100120509       IF pratica.p = 1;
068200120509         partite_ptr = %ALLOC(%SIZE(partite));
068300120509       ELSE;
068400120509         partite_ptr = %REALLOC(partite_ptr : %SIZE(partite) * pratica.p);
068500120509       ENDIF;
068600120509
068700120509       partite(pratica.p).idSocieta = priIdSocieta;
068800120509       partite(pratica.p).contabilita = priContabilita;
068900120509       partite(pratica.p).capoConto = priCapoConto;
069000120509       partite(pratica.p).sottoConto = priSottoConto;
069100120509       partite(pratica.p).data = priDataPartita;
069200120509       partite(pratica.p).numero = priNumeroPartita;
069300120509       partite(pratica.p).serie = priSeriePartita;
069400120713       partite(pratica.p).saldoEuro = priSaldoPartitaEuro;
069500120713       partite(pratica.p).esposizioneEuro = priEsposizionePartitaEuro;
069600121026       partite(pratica.p).scadenza = priDataScadenza;
069700120713
069800120509       RETURN YCOCTZ_ESITO_OK;
069900120509
070000120509      /END-FREE
070100120509     P YCOCTZOUT_AddPartitaAlContenzioso...
070200120509     P                 E
070300120509
070400120509
070500120509     P*--------------------------------------------------
070600120510     P* Procedure name: YCOCTZOUT_RunClienti
070700120509     P* Purpose:        Inserisce le righe dei clienti passati in contenzioso.
070800120509     P* Returns:        Esito.
070900120509     P*--------------------------------------------------
071000120510     P YCOCTZOUT_RunClienti...
071100120509     P                 B
071200120510     D YCOCTZOUT_RunClienti...
071300120509     D                 PI            10I 0
071400120711     D priRqsOpCode                  10I 0 CONST OPTIONS(*NOPASS)
071500120509
071600120511     D retField        S             10I 0 STATIC
071700120509     D local           DS                  STATIC INZ QUALIFIED
071800120515     D  esito                        10I 0
071900120515     D  p                            10I 0
072000120622     D  nct                          10I 0
072100120622     D  ncl                          10I 0
072200120713     D  esposizioneEuro...
072300120713     D                               15P 2
072400120510     D  idSocieta                     3A
072500120510     D  sottoConto                    8A
072600120622     D resultClienti   DS                  LIKEDS(YCOCTZSEL_resultClienti)
072700120622     D                                     DIM(255) STATIC
072800120622
072900120509      /FREE
073000120509
073100120509       IF pratica.operazione <> YCOCTZOUT_OPERAZIONE_INSERIRE AND
073200120509          pratica.operazione <> YCOCTZOUT_OPERAZIONE_AGGIORNARE;
073300120509         RETURN YCOCTZ_ESITO_OK;
073400120509       ENDIF;
073500120509
073600120509       IF pratica.p = *ZERO;
073700120509         RETURN YCOCTZ_ERRORE_NO_PARTITE;
073800120509       ENDIF;
073900120510
074000120511       RESET retField;
074100120511       RESET local;
074200120511
074300120510       //SortArray( %ADDR(partite)
074400120510       //         : pratica.p
074500120510       //         : %SIZE(partite)
074600120510       //         : %PADDR('SORTARRAYPARTITE')
074700120510       //         );
074800120509
074900120510       // Scorro la schiera delle partite e totalizzo per ID cliente.
075000120510
075100120509       FOR local.p = 1 TO pratica.p;
075200120509
075300120716         IF partite(local.p).esposizioneEuro <> *ZERO;
075400120716
075500120716           local.esposizioneEuro = partite(local.p).esposizioneEuro;
075600120716           local.idSocieta = partite(local.p).idSocieta;
075700120716           local.sottoConto = partite(local.p).sottoConto;
075800120510
075900120716           EXEC SQL
076000120716             UPDATE yCtzCli0f
076100120716               SET importo_contenzioso = importo_contenzioso
076200120716                                       + :local.esposizioneEuro
076300120716               WHERE id_contenzioso = :yCtzPra0f.idContenz
076400120716                 AND id_cliente_societa = :local.idSocieta
076500120716                 AND id_cliente_ksc = :local.sottoConto
076600120716                 AND accentramento_sys = :pratica.sysProj
076700120716                 AND accentramento_numero = :pratica.nAssolutoProj
076800120716               WITH RS
076900120716             ;
077000120510
077100120716           IF sqlCode < *ZERO;
077200120716             DUMP(A);
077300120716             RETURN sqlCode;
077400120716           ENDIF;
077500120510
077600120716           IF sqlCode = SQLCODE_ROW_NOT_FOUND;
077700120510
077800120716             EXEC SQL
077900120716               INSERT INTO yCtzCli0f
078000120716                         ( profilo_utente_insert
078100120716                         , id_contenzioso
078200120716                         , id_cliente_societa
078300120716                         , id_cliente_ksc
078400120716                         , accentramento_sys
078500120716                         , accentramento_numero
078600120716                         , importo_contenzioso
078700120716                         )
078800120716                 VALUES ( RTRIM(:kpjba.knmus)
078900120716                        , :yCtzPra0f.idContenz
079000120716                        , :local.idSocieta
079100120716                        , :local.sottoConto
079200120716                        , :pratica.sysProj
079300120716                        , :pratica.nAssolutoProj
079400120716                        , :local.esposizioneEuro
079500120716                        )
079600120716                 WITH RS
079700120716               ;
079800120510
079900120716             IF sqlCode < *ZERO;
080000120716               DUMP(A);
080100120716               RETURN sqlCode;
080200120716             ENDIF;
080300120510
080400120716           ENDIF;
080500120716
080600120510         ENDIF;
080700120510
080800120509       ENDFOR;
080900120509
081000120622       // Reperisco tutti i codici cliente da contendere e li aggiorno con
081100120622       // lo stato del credito e il codice cliente di fatturazione accentratore.
081200120622
081300120718       IF pratica.operazione = YCOCTZOUT_OPERAZIONE_INSERIRE;
081400120718         local.nct = YCOCTZSEL_NewClientiByIdCliente( local.idSocieta
081500120718                                                    : yCtzPra0f.idCliKsc
081600120718                                                    : *ON
081700120718                                                    );
081800120718       ELSE;
081900120718         local.nct = YCOCTZSEL_NewClientiByIdCliente( local.idSocieta
082000120718                                                    : yCtzPra0f.idCliKsc
082100120718                                                    : *OFF
082200120718                                                    );
082300120718       ENDIF;
082400120718
082500120622
082600120622       DOU local.ncl < *ZERO;
082700120622
082800120622         local.ncl = YCOCTZSEL_FetchNextClientiByIdCliente
082900120622                     ( %ELEM(resultClienti) : resultClienti );
083000120622
083100120622         IF local.ncl < 1;
083200120622           LEAVE;
083300120622         ENDIF;
083400120622
083500120622         FOR local.p = 1 TO local.ncl;
083600120622
083700120622           local.sottoConto = resultClienti(local.p).idCliente;
083800120622
083900140923           // Imposto l'ID cliente accentratore come ID cliente fatturazione
083901140923           // se il codice non è escluso dal contenzioso.
084000120622
084100120622           EXEC SQL
084200120622             UPDATE ancln00f
084300120622               SET clnCliFatt = :yCtzPra0f.idCliKsc
084400120711                 , clnUtente = :xSoc001ds.xscCut
084500120711                 , clnDtUlMo = CURRENT_DATE
084600120622               WHERE clnSocieta = :local.idSocieta
084700120622                 AND clnKsc = :local.sottoConto
084800120622                 AND clnCliFatt <> :yCtzPra0f.idCliKsc
084801140923                 AND clnKsc IN ( SELECT rcoKsc
084802140923                                   FROM anrco00f
084803140923                                  WHERE rcoSocieta = :local.idSocieta
084804140923                                    AND rcoSNatura = 'C'
084805140923                                    AND rcoKsc = :local.sottoConto
084806140923                                    AND rcoCtgAn05 <> 'E'
084807140923                               )
084900120622               WITH RS
085000120622             ;
085100120622
085200120622           SELECT;
085300120622             WHEN sqlCode = SQLCODE_ROW_IN_USE;
085400120622               retField = sqlCode;
085500120622             WHEN sqlCode < *ZERO;
085600120622               DUMP(A);
085700120622               retField = sqlCode;
085800120622           ENDSL;
085900120622
086000120718           // Imposto stato del credito (solo per i nuovi contenziosi).
086100120622
086200120718           IF pratica.operazione = YCOCTZOUT_OPERAZIONE_INSERIRE;
086300120718             local.esito = YCOCTZOUT_SetStatoCreditoCliente( local.idSocieta
086400120718                           : local.sottoConto : STATO_CREDITO_CONTENZIOSO );
086500120718             SELECT;
086600120718               WHEN local.esito = SQLCODE_ROW_IN_USE;
086700120718                 retField = local.esito;
086800120718               WHEN local.esito < *ZERO;
086900120718                 retField = local.esito;
087000120718             ENDSL;
087100120718           ENDIF;
087200120622
087300120622         ENDFOR;
087400120622
087500120622       ENDDO;
087600120622
087700120622       local.esito = YCOCTZSEL_EndClientiByIdCliente();
087800120622
087900120511       RETURN retField;
088000120509
088100120509      /END-FREE
088200120510     P YCOCTZOUT_RunClienti...
088300120509     P                 E
088400120509
088500120509
088600120509     P*--------------------------------------------------
088700120509     P* Procedure name: SortArrayPartite
088800120509     P* Purpose:        Ordina la schiera delle partite.
088900120509     P* Returns:        Esito.
089000120509     P* Parameter:      parm1
089100120509     P* Parameter:      parm2
089200120509     P*--------------------------------------------------
089300120509     P SortArrayPartite...
089400120509     P                 B
089500120509     D SortArrayPartite...
089600120509     D                 PI            10I 0
089700120509     D  parm1                              LIKE(partite)
089800120509     D  parm2                              LIKE(partite)
089900120509
090000120509     C                   SELECT
090100120509     C                   WHEN      parm1 < parm2
090200120509     C                   RETURN    -1
090300120509     C                   WHEN      parm1 > parm2
090400120509     C                   RETURN    1
090500120509     C                   OTHER
090600120509     C                   RETURN    0
090700120509     C                   ENDSL
090800120509
090900120509     P SortArrayPartite...
091000120509     P                 E
091100120509
091200120510
091300120510     P*--------------------------------------------------
091400120510     P* Procedure name: YCOCTZOUT_RunRegistrazione
091500120510     P* Purpose:        Inserisce in Proj la registrazione di accentramento.
091600120510     P* Returns:        Esito.
091700120510     P*--------------------------------------------------
091800120510     P YCOCTZOUT_RunRegistrazione...
091900120510     P                 B
092000120510     D YCOCTZOUT_RunRegistrazione...
092100120510     D                 PI            10I 0
092200120510
092300120510     D retField        S             10I 0 STATIC
092400120510     D local           DS                  STATIC INZ QUALIFIED
092500120510     D  p                            10I 0
092600120510
092700120510      /FREE
092800120510
092900120510       IF pratica.operazione <> YCOCTZOUT_OPERAZIONE_INSERIRE AND
093000120510          pratica.operazione <> YCOCTZOUT_OPERAZIONE_AGGIORNARE;
093100120510         RETURN YCOCTZ_ESITO_OK;
093200120510       ENDIF;
093300120510
093400120510       IF pratica.p = *ZERO;
093500120510         RETURN YCOCTZ_ERRORE_NO_PARTITE;
093600120510       ENDIF;
093700120510
093800120510       RESET retField;
093900120510       RESET local;
094000120510
094100120510       // Scorro la schiera delle partite e totalizzo per ID cliente.
094200120510
094300120510       FOR local.p = 1 TO pratica.p;
094400120511
094500120713         IF partite(local.p).esposizioneEuro <> *ZERO;
094600120516
094700120516           IF YCOCTZOUT_SetPartitaInContenzioso( partite(local.p).idSocieta
094800120516                                               : partite(local.p).contabilita
094900120516                                               : partite(local.p).capoConto
095000120516                                               : partite(local.p).sottoConto
095100120516                                               : partite(local.p).data
095200120516                                               : partite(local.p).numero
095300120516                                               : partite(local.p).serie
095400120516                                               : *ON
095500120516                                               ) < YCOCTZ_ESITO_OK;
095600120516             retField = YCOCTZ_ERRORE_GENERICO;
095700120516             LEAVE;
095800120516           ENDIF;
095900120713
096000120713         ENDIF;
096100120514
096200120713         IF partite(local.p).sottoConto <> yCtzPra0f.idCliKsc AND
096300120713            partite(local.p).saldoEuro <> *ZERO;
096400120619
096500120619           IF pratica.nAssolutoProj = *ZERO;
096600120619
096700120619             YPJBH_SetLottoRegistrazione( ndbhm00f.bhmRifInt );
096800120619
096900120619             pratica.nAssolutoProj = YPJBH_NewRegistrazione( *ON
097000120619                                                           : 'CONT'
097100120619                                                           : %DATE()
097200120619                                                           : yCtzPra0f.idCliSoc
097300120619                                                           : *ON
097400120619                                                           );
097500120619
097600120619             IF pratica.nAssolutoProj < YPJBH_ESITO_OK;
097700120619               RETURN YCOCTZ_ERRORE_REGISTRAZIONE;
097800120619             ENDIF;
097900120619
098000120619             YPJBH_SetSubNumeroRegistrazione( yCtzPra0f.idContenz );
098100120619             YPJBH_SetUnitaRegistrazione( %SUBST(yCtzPra0f.idCliKsc : 2 : 3) );
098200120514
098300120619           ENDIF;
098400120619
098500120515           // Dare cliente accentratore.
098600120516
098700120516           IF partite(local.p).saldoEuro > *ZERO;
098800120516             partite(local.p).segno = PROJ_DARE;
098900120516           ELSE;
099000120516             partite(local.p).segno = PROJ_AVERE;
099100120516           ENDIF;
099200120514
099300120514           IF YPJBH_NewMovimento( pratica.capoContoCliente
099400120514                                : yCtzPra0f.idCliKsc
099500120516                                : %ABS(partite(local.p).saldoEuro)
099600120516                                : partite(local.p).segno
099700120514                                ) < YPJBH_ESITO_OK;
099800120514             retField = YCOCTZ_ERRORE_REGISTRAZIONE;
099900120514             LEAVE;
100000120514           ENDIF;
100100120515
100200120516           YPJBH_SetDocumento( yCtzPra0f.dtContenzI
100300120516                             : yCtzPra0f.idContenz
100400120516                             : SERIE
100500120516                             );
100600120516
100700120516           YPJBH_InsMovimento();
100800120510
100900120515           IF YPJBH_NewMovimentoPartita( partite(local.p).data
101000120515                                       : partite(local.p).numero
101100120515                                       : partite(local.p).serie
101200120515                                       : %ABS(partite(local.p).saldoEuro)
101300120515                                       : partite(local.p).segno
101400121026                                       : partite(local.p).scadenza
101500120515                                       ) < YPJBH_ESITO_OK;
101600120515             retField = YCOCTZ_ERRORE_REGISTRAZIONE;
101700120515             LEAVE;
101800120515           ENDIF;
101900120515
102000120516           YPJBH_SetCategoriaPartita(SERIE);
102100120516           YPJBH_InsMovimentoPartita();
102200120510
102300120515           IF YPJBH_EndMovimentoPartita() < YPJBH_ESITO_OK
102400120515           OR YPJBH_EndMovimento() < YPJBH_ESITO_OK;
102500120515             retField = YCOCTZ_ERRORE_REGISTRAZIONE;
102600120515             LEAVE;
102700120515           ENDIF;
102800120515
102900120515           // Avere cliente.
103000120515
103100120515           IF partite(local.p).saldoEuro < *ZERO;
103200120515             partite(local.p).segno = PROJ_DARE;
103300120515           ELSE;
103400120515             partite(local.p).segno = PROJ_AVERE;
103500120515           ENDIF;
103600120515
103700120515           IF YPJBH_NewMovimento( partite(local.p).capoConto
103800120515                                : partite(local.p).sottoConto
103900120516                                : %ABS(partite(local.p).saldoEuro)
104000120515                                : partite(local.p).segno
104100120515                                ) < YPJBH_ESITO_OK;
104200120515             retField = YCOCTZ_ERRORE_REGISTRAZIONE;
104300120515             LEAVE;
104400120515           ENDIF;
104500120516
104600120516           YPJBH_SetDocumento( yCtzPra0f.dtContenzI
104700120516                             : yCtzPra0f.idContenz
104800120516                             : SERIE
104900120516                             );
105000120517
105100120517           YPJBH_SetContropartitaUtente( pratica.capoContoCliente
105200120517                                       : yCtzPra0f.idCliKsc
105300120517                                       );
105400120515
105500120515           YPJBH_InsMovimento();
105600120515
105700120515           IF YPJBH_NewMovimentoPartita( partite(local.p).data
105800120515                                       : partite(local.p).numero
105900120515                                       : partite(local.p).serie
106000120515                                       : %ABS(partite(local.p).saldoEuro)
106100120515                                       : partite(local.p).segno
106200121026                                       : partite(local.p).scadenza
106300120515                                       ) < YPJBH_ESITO_OK;
106400120515             retField = YCOCTZ_ERRORE_REGISTRAZIONE;
106500120515             LEAVE;
106600120515           ENDIF;
106700120515
106800120515           YPJBH_InsMovimentoPartita();
106900120515
107000120515           IF YPJBH_EndMovimentoPartita() < YPJBH_ESITO_OK
107100120515           OR YPJBH_EndMovimento() < YPJBH_ESITO_OK;
107200120515             retField = YCOCTZ_ERRORE_REGISTRAZIONE;
107300120515             LEAVE;
107400120515           ENDIF;
107500120515
107600120515         ENDIF;
107700120511
107800120510       ENDFOR;
107900120510
108000120619       IF pratica.nAssolutoProj > *ZERO;
108100120619         IF retField < YCOCTZ_ESITO_OK;
108200120619           YPJBH_EndRegistrazione();
108300120619         ELSE;
108400120619           retField = YPJBH_EndRegistrazione();
108500120619         ENDIF;
108600120619       ENDIF;
108700120515
108800120510       RETURN retField;
108900120510
109000120510      /END-FREE
109100120510     P YCOCTZOUT_RunRegistrazione...
109200120510     P                 E
109300120510
109400120511
109500120511     P*--------------------------------------------------
109600120511     P* Procedure name: YCOCTZOUT_SetPartitaInContenzioso
109700120511     P* Purpose:        Imposta NDPPS00F.PPSLEGALE = *ON
109800120511     P* Returns:        Esito.
109900120511     P* Parameter:      priSocieta => ID società.
110000120511     P* Parameter:      priCtb => Contabilità
110100120511     P* Parameter:      priKcc => Capo conto.
110200120511     P* Parameter:      priKsc => Sotto conto.
110300120511     P* Parameter:      priDtPar => Data partita.
110400120511     P* Parameter:      priNrPar => Numero partita.
110500120511     P* Parameter:      priSerPar => Serie partita.
110600120511     P* Parameter:      priLegale
110700120511     P*--------------------------------------------------
110800120511     P YCOCTZOUT_SetPartitaInContenzioso...
110900120511     P                 B
111000120511     D YCOCTZOUT_SetPartitaInContenzioso...
111100120511     D                 PI            10I 0
111200120511     D  priSocieta                    3A
111300120511     D  priCtb                        2A
111400120511     D  priKcc                        6A
111500120511     D  priKsc                        8A
111600120511     D  priDtPar                       D   DATFMT(*ISO)
111700120511     D  priNrPar                      9P 0
111800120511     D  priSerPar                     4A
111900120511     D  priLegale                      N   CONST
112000120511
112100120511     D local           DS                  STATIC INZ QUALIFIED
112200120511     D  sys                           3P 0
112300120511     D  nrAsInt                       9P 0
112400120719     D  ctgAna1                       4A   INZ(SERIE)
112500120511
112600120511      /FREE
112700120511
112800120511       RESET local;
112900120511
113000120511       EXEC SQL
113100120511         SELECT ppaSys, ppaNrAsInt
113200120511           INTO :local.sys, :local.nrAsInt
113300120511           FROM ndppa00f
113400120511           WHERE ppaSocieta = :priSocieta
113500120511             AND ppaCtb = :priCtb
113600120511             AND ppaKcc = :priKcc
113700120511             AND ppaKsc = :priKsc
113800120511             AND ppaDtPar = :priDtPar
113900120511             AND ppaNrPar = :priNrPar
114000120511             AND ppaSerPar = :priSerPar
114100120511       ;
114200120511
114300120511       SELECT;
114400120511         WHEN sqlCode < *ZERO;
114500120511           DUMP(A);
114600120511           RETURN sqlCode;
114700120511         WHEN sqlCode = SQLCODE_ROW_NOT_FOUND;
114800120511           RETURN sqlCode;
114900120511       ENDSL;
115000120511
115100120719       IF NOT priLegale;
115200120719         CLEAR local.ctgAna1;
115300120516       ENDIF;
115400120516
115500120511       EXEC SQL
115600120511         UPDATE ndpps00f
115700120516           SET ppsCtgAna1 = :local.ctgAna1
115800120511           WHERE ppsSys = :local.sys
115900120511             AND ppsNrAsInt = :local.nrAsInt
116000120719             AND ppsCtgAna1 <> :local.ctgAna1
116100120511           WITH RS
116200120511       ;
116300120511
116400120511       SELECT;
116500120511         WHEN sqlCode < *ZERO;
116600120511           DUMP(A);
116700120511           RETURN sqlCode;
116800120511         WHEN sqlCode = SQLCODE_ROW_NOT_FOUND;
116900120511           RETURN sqlCode;
117000120511       ENDSL;
117100120511
117200120511       RETURN YCOCTZ_ESITO_OK;
117300120511
117400120511      /END-FREE
117500120511     P YCOCTZOUT_SetPartitaInContenzioso...
117600120511     P                 E
117700120515
117800120515
117900120515     P*--------------------------------------------------
118000120515     P* Procedure name: YCOCTZOUT_SetStatoCreditoCliente
118100120515     P* Purpose:        Imposta lo stato del credito del cliente.
118200120515     P* Returns:        Esito.
118300120515     P* Parameter:      priIdSocieta => ID società.
118400120515     P* Parameter:      priIdCliente
118500120515     P* Parameter:      priStatoDelCredito
118600120515     P*--------------------------------------------------
118700120515     P YCOCTZOUT_SetStatoCreditoCliente...
118800120515     P                 B                   EXPORT
118900120515     D YCOCTZOUT_SetStatoCreditoCliente...
119000120515     D                 PI            10I 0
119100120518     D  priIdSocieta                  3A                                        1
119200120518     D  priIdCliente                  8A                                        2
119300120518     D  priStatoDelCredito...                                                   3
119400120515     D                                4A   CONST
119500120712     D  priSoloAnagraficaCliente...
119600120712     D                                 N   CONST OPTIONS(*NOPASS)               4
119700120515
119800120515     D retField        S             10I 0 STATIC
119900120622     D local           DS                  STATIC QUALIFIED INZ
120000130313     D* bkAmm                         2A   INZ(BLOCCO_PAGAMENTO_CONTENZIOSO)
120100120711     D  idSocieta                     3A
120200120711     D  capoConto                     6A   INZ(PROJ_SOTTONATURA_CLIENTE)
120300120711     D  idCliente                     8A
120400120716     D  esito                        10I 0
120500120711     D ds4w          E DS                  STATIC QUALIFIED
120600120711     D  tblUni                 1     89A
120700120515
120800120518      /FREE
120900120515
121000120515       RESET retField;
121100120515
121200120523       IF NOT done.newPratica;
121300120523         RETURN YCOCTZ_ERRORE_PRATICA_NON_INIZIATA;
121400120523       ENDIF;
121500120523
121600120622       RESET local;
121700120711
121800120711       local.idSocieta = priIdSocieta;
121900120711       local.idCliente = priIdCliente;
122000120711
122100120711       Proj_GetCapocontoClienteFornitore( local.idSocieta
122200120711                                        : local.capoConto
122300120711                                        : local.idCliente
122400120711                                        );
122500120622
122600120711       EXEC SQL
122700120711         SELECT tblUni
122800120711           INTO :ds4w.tblUni
122900120711           FROM tabel00f
123000120711           WHERE tblKut = 1
123100120711             AND tblCod = '4W'
123200120711             AND tblKey = :priStatoDelCredito
123300120711             AND tblFlg = ''
123400120711       ;
123500120711
123600120711       SELECT;
123700120711         WHEN sqlCode < *ZERO;
123800120711           DUMP(A);
123900120711           RETURN sqlCode;
124000120711         WHEN sqlCode = SQLCODE_ROW_NOT_FOUND;
124100120711           RETURN sqlCode;
124200120711       ENDSL;
124300120711
124400130313       //IF ds4w.§4wPbl = 'N'; // Pagamenti non bloccati.
124500130313       //  CLEAR local.bkAmm;
124600130313       //ENDIF;
124700120622
124800120711       // Aggiorno l'anagrafica contabile del cliente con
124900120711       // il nuovo stato del credito e il blocco dei pagamenti.
125000130613       // solo se rcoctgan05 è diverso da 'E' (escluso da contanzioso
125001130613
125002130619       // DA VERIFICAre If rcoctgan05 <> CLIENTE_ESCLUSO_DA_CONTENZIOSO;
125100120515       EXEC SQL
125200120515         UPDATE anrco00f
125300120515           SET rcoCtgAn03 = :priStatoDelCredito
125400120622             , rcoUteUlMo = :xSoc001ds.xscCut
125500120622             , rcoDtUlMo = CURRENT_DATE
125600120515           WHERE rcoSocieta = :priIdSocieta
125700120515             AND rcoSNatura = 'C'
125800130613             AND rcoKsc = :priIdCliente
125802140923             and rcoCtgAn05 <> 'E'
125900120515           WITH UR
126000120515       ;
126001130619       // da verificare endif;
126100120515
126200120515       SELECT;
126300120515         WHEN sqlCode = SQLCODE_ROW_IN_USE;
126400120515           RETURN sqlCode;
126500120515         WHEN sqlCode < *ZERO;
126600120515           DUMP(A);
126700120515           RETURN sqlCode;
126800120515         WHEN sqlCode = SQLCODE_ROW_NOT_FOUND;
126900120515           RETURN sqlCode;
127000120515       ENDSL;
127100120711
127200130313       // Aggiorno blocco pagamento e blocco servizio con i valori congruenti
127300130313       // col nuovo stato del credito.
127400130313
127500130313       kpjba.kpjbu = local.idSocieta;
127600130313       %SUBST(kpjba.kpjbu : 37) = local.capoConto;
127700130313       %SUBST(kpjba.kpjbu : 52) = local.idCliente;
127800130313       Yco111r(kpjba : psds.pgmName : *ON);
127900130313
128000120711       // Blocco del servizio e frequenza di fatturazione settimanale.
128100120711
128200120711       EXEC SQL
128300120711         UPDATE ybacl00f
128400130403           -- Quando blocco forzo la frequenza di fatturazione settimanale
128500130403           -- ma solo se il cliente non è in autofatturazione (distinta);
128600130313           -- Quando sblocco lascio la frequenza di fatturazione presente.
128700130313           SET aclFft = CASE :ds4w.§4wTbl
128800120711                        WHEN '' THEN aclFft
128900120711                        ELSE 1
129000120711                        END
129100120711           WHERE aclSocieta = :local.idSocieta
129200120711             AND aclKcc = :local.capoConto
129300120711             AND aclKsc = :local.idCliente
129400130403             AND aclTft <> 1
129500120711           WITH UR
129600120711       ;
129700120711
129800120711       SELECT;
129900120711         WHEN sqlCode = SQLCODE_ROW_IN_USE;
130000120711           RETURN sqlCode;
130100120711         WHEN sqlCode < *ZERO;
130200120711           DUMP(A);
130300120711           RETURN sqlCode;
130400120711       ENDSL;
130500120711
130600120711       // Condizione pagamento forzata.
130700120711
130800120711       IF ds4w.§4wCdp <> *BLANK;
130900120711
131000120711         EXEC SQL
131100120711           UPDATE ancln00f
131200120711             SET clnCondPag = :ds4w.§4wCdp
131300120711               , clnUtente = :xSoc001ds.xscCut
131400120711               , clnDtUlMo = CURRENT_DATE
131500120711             WHERE clnSocieta = :local.idSocieta
131600120711               AND clnKsc = :local.idCliente
131700120711             WITH UR
131800120711         ;
131900120711
132000120711         SELECT;
132100120711           WHEN sqlCode = SQLCODE_ROW_IN_USE;
132200120711             RETURN sqlCode;
132300120711           WHEN sqlCode < *ZERO;
132400120711             DUMP(A);
132500120711             RETURN sqlCode;
132600120711           WHEN sqlCode = SQLCODE_ROW_NOT_FOUND;
132700120711             RETURN sqlCode;
132800120711         ENDSL;
132900120711
133000120711       ENDIF;
133100120515
133200120711       // Inserisco un commento anagrafico.
133300121026
133400121026       EXEC SQL
133500121026         SELECT tblUni
133600121026           INTO :ds4w.tblUni
133700121026           FROM tabel00f
133800121026           WHERE tblKut = 1
133900121026             AND tblCod = '4W'
134000121026             AND tblKey = :priStatoDelCredito
134100121026             AND tblFlg = ''
134200121026       ;
134300121026
134400121026       IF sqlCode < *ZERO;
134500121026         DUMP(A);
134600121026         RETURN sqlCode;
134700121026       ENDIF;
134800121026
134900121026       IF priStatoDelCredito = *BLANK;
135000121030         ds4w.§4wdes = 'Tolto sofferenza';
135100121026       ENDIF;
135200120711
135300121026
135400120706       RESET ana723ds;
135500120706       ana723ds.opz723 = PROJ_COMMENTO_INSERT;
135600120706       ana723ds.tpa723n = PROJ_COMMENTO_CLIENTE;
135700120706       ana723ds.soc723n = xSoc001ds.xscSoc;
135800120706       ana723ds.ky1723n = priIdCliente;
135900120706       ana723ds.ky3723n = '000';
136000120706       ana723ds.lin723n = xSoc001ds.xscLin;
136100120706       ana723ds.uso1723n = *ON;
136200121026       ana723ds.txt723n = %CHAR(%TIME())
136300121026                        + ' Inserito stato "'
136400120711                        + %SUBST(priStatoDelCredito : 1 : 3)
136500121026                        + '" ' + ds4w.§4wdes
136600120711                        ;
136700121026
136800120706       Proj_AggiornaCommento( kpjba : ana723ds );
136900120706
137000120622       IF priIdCliente <> yCtzPra0f.idCliKsc;
137100120515         RETURN YCOCTZ_ESITO_OK;
137200120515       ENDIF;
137300120716
137400120716       IF ds4w.§4wTip = *BLANK;
137500120716         retField = YCOCTZOUT_SetFineContenzioso( *ON );
137600120716       ELSE;
137700120716         retField = YCOCTZOUT_SetFineContenzioso( *OFF );
137800120716       ENDIF;
137900120712
138000120712       IF %PARMS() > 3 AND priSoloAnagraficaCliente;
138100120712         RETURN YCOCTZ_ESITO_OK;
138200120712       ENDIF;
138300120518
138400120515       EXEC SQL
138500120522         INSERT INTO yCtzSta0f
138600120515                   ( profilo_utente_insert
138700120515                   , id_stato_credito
138800120518                   , id_contenzioso
138900120515                   )
139000120515           VALUES  ( RTRIM(:kpjba.knmus)
139100120515                   , :priStatoDelCredito
139200120523                   , :yCtzPra0f.idContenz
139300120515                   )
139400120515           WITH RS
139500120515       ;
139600120515
139700120515       IF sqlCode < *ZERO;
139800120515         DUMP(A);
139900120515         RETURN sqlCode;
140000120515       ENDIF;
140100120716
140200120515       RETURN retField;
140300120515
140400120515      /END-FREE
140500120515     P YCOCTZOUT_SetStatoCreditoCliente...
140600120515     P                 E
140700120618
140800120618
140900120618     P*--------------------------------------------------
141000120618     P* Procedure name: YCOCTZOUT_AddClienteAlContenzioso
141100120618     P* Purpose:        Aggiunge un codice cliente al contenzioso.
141200120618     P* Returns:        Esito.
141300120618     P* Parameter:      priIdSocieta => ID società.
141400120618     P* Parameter:      priIdCliente => ID cliente
141500120618     P* Parameter:      priSaldoContabile => Saldo contabile portato al con...
141600120618     P*                          tenzioso.
141700120618     P* Parameter:      priNumeroPartite => Numero di partite portate al co...
141800120618     P*                          ntenzioso.
141900120618     P*--------------------------------------------------
142000120618     P YCOCTZOUT_AddClienteAlContenzioso...
142100120618     P                 B                   EXPORT
142200120618     D YCOCTZOUT_AddClienteAlContenzioso...
142300120618     D                 PI            10I 0
142400120618     D  priIdSocieta                  3A   CONST
142500120618     D  priIdCliente                  8A   CONST
142600120618     D  priSaldoContabile...
142700120618     D                               15P 2
142800120618     D  priNumeroPartite...
142900120618     D                               10I 0
143000120618
143100120618     D retField        S             10I 0 STATIC
143200120618     D local           DS                  STATIC QUALIFIED INZ
143300120618     D  idSocieta                     3A
143400120618     D  idCliente                     8A
143500120618     D  capoConto                     6A   INZ(PROJ_SOTTONATURA_CLIENTE)
143600120618     D  esito                        10I 0
143700120718     D  situazioneCompleta...
143800120718     D                                 N
143900120718     D partitaDaContendere...
144000120618     D                 DS                  STATIC QUALIFIED INZ
144100120618     D  societa                       3A
144200120618     D  ctb                           8A
144300120618     D  kcc                           6A
144400120619     D  ksc                           8A
144500120618     D  dtPar                          D
144600120618     D  nrPar                         9P 0
144700120618     D  serPar                        4A
144800120619     D  saldo                        15P 2
144900120713     D  esposizione                  15P 2
145000121026     D  dtSca                          D
145001130619     D  AnRcoDS      E DS                  extname(anrco00f)
145002130619
145300120618      /FREE
145400120618
145500120618       RESET retField;
145600120619       CLEAR priSaldoContabile;
145700120619       CLEAR priNumeroPartite;
145800120618
145900120619       IF NOT done.newPratica;
146000120618         RETURN YCOCTZ_ERRORE_PRATICA_NON_INIZIATA;
146100120618       ENDIF;
146200120618
146300120618       RESET local;
146400120618       local.idSocieta = priIdSocieta;
146500120618       local.idCliente = priIdCliente;
146600120618
146700120618       Proj_GetCapocontoClienteFornitore( local.idSocieta
146800120618                                        : local.capoConto
146900120618                                        : local.idCliente
147000120618                                        );
147100120618
147101130619       // Verifica categoria anagrafica  di anrco
147102130619       Proj_ContoContabile( local.idsocieta
147103130619                          : local.capoConto
147104130619                          : local.idcliente
147105130619                          : %date()
147106130619                          : *OFF
147107130619                          : esitoProj
147108130619                          : 'ANRCO'
147109130619                          : AnrcoDS
147110130619                          : %SIZE(AnrcoDS)
147111130619                          : *OFF
147112130619                          );
147113130619       IF esitoProj = *ON;
147114130619         RETURN YCOCTZ_ERRORE_GENERICO;
147115130619       ENDIF;
147116140923       IF rcoCtgAn05 = CLIENTE_ESCLUSO_DA_CONTENZIOSO;
147117130619         RETURN YCOCTZ_ESITO_OK;
147118130619       ENDIF;
147119130619
147200120713       // Calcolo
147300120713       // - il saldo di ogni partita del cliente con i soli movimenti definitivi;
147400120713       // - l'esposizione (senza rischio) di ogni partita del cliente.
147500120718       // In caso di nuovo contenzioso ignoro la categoria 1 di partita,
147600120718       // altrimenti prendo solo le partite con la categoria 1 vuota,
147700120718       // cioè le partite mai contese.
147800120718
147900120718       local.situazioneCompleta =
148000120718       (pratica.operazione = YCOCTZOUT_OPERAZIONE_INSERIRE);
148100120718
148200120718       EXEC SQL
148300120618         DECLARE partiteDaContendere NO SCROLL CURSOR FOR
148400120713           WITH partite ( societa, ctb, kcc, ksc
148500120713                        , dtPar, nrPar, serPar, saldo ) AS
148600120618           (
148700120713             SELECT scpSocieta, scpCtb, scpKcc, scpKsc
148800120713                  , scpDtPar, scpNrPar, scpSerPar
148900120716                  , DEC(SUM( scpTdld - scpTald ), 15, 2) AS saldo
149000120713             FROM qtemp/ndscp00f
149100120713             WHERE scpSocieta = :local.idSocieta
149200120713               AND scpCtb = 'CG'
149300120713               AND scpTpScad = '0'
149400120713               AND scpDett = 'P'
149500120713               AND scpKcc = :local.capoConto
149600120713               AND scpKsc = :local.idCliente
149700120713             GROUP BY scpSocieta, scpCtb, scpTpScad, scpDett
149800120713                    , scpKcc, scpKsc, scpDtPar, scpNrPar, scpSerPar
149900120618           )
150000120713              , scadenze ( societa, ctb, kcc, ksc
150100121026                        , dtPar, nrPar, serPar, esposizione, dtSca ) AS
150200120713           (
150300120713             SELECT scdSocieta, scdCtb, scdKcc, scdKsc
150400120713                  , scdDtPar, scdNrPar, scdSerPar
150500120716                  , DEC(SUM( scdImpRMc ), 15, 2) AS esposizione
150600121026                  , MIN(scdDtSca)
150700120713             FROM qtemp/ndscd00f
150800120713             WHERE scdSocieta = :local.idSocieta
150900120713               AND scdCtb = 'CG'
151000120713               AND scdTpScad = '0'
151100120713               AND scdKcc = :local.capoConto
151200120713               AND scdKsc = :local.idCliente
151300120716               AND CASE
151400120716                   WHEN scdMdPag <> ' '
151500120716                    AND scdDtSca <= CURRENT_DATE   THEN '0' -- Effetto scaduto.
151600120716                   WHEN SUBSTR(scdFlg, 1, 1) = 'S' THEN '0' -- Titolo a rischio.
151700120716                   WHEN scdMdPag <> ' '
151800120716                    AND SUBSTR(scdFlg, 2, 1) = ' ' THEN '0' -- Effetto non presentato.
151900120716                   ELSE '1'
152000120716                   END = '1'
152100120713             GROUP BY scdSocieta, scdCtb, scdTpScad
152200120713                    , scdKcc, scdKsc, scdDtPar, scdNrPar, scdSerPar
152300120713           )
152400120716           SELECT partite.*
152500120716                , IFNULL(scadenze.esposizione, partite.saldo)
152600121026                , IFNULL(scadenze.dtSca, '0001-01-01')
152700120713           FROM partite
152800120716           LEFT OUTER JOIN scadenze
152900120713             ON partite.societa = scadenze.societa
153000120713            AND partite.ctb = scadenze.ctb
153100120713            AND partite.kcc = scadenze.kcc
153200120713            AND partite.ksc = scadenze.ksc
153300120713            AND partite.dtPar = scadenze.dtPar
153400120713            AND partite.nrPar = scadenze.nrPar
153500120713            AND partite.serPar = scadenze.serPar
153600120618           JOIN ndppa00f
153700120713             ON partite.societa = ndppa00f.ppaSocieta
153800120713            AND partite.ctb = ndppa00f.ppaCtb
153900120713            AND partite.kcc = ndppa00f.ppaKcc
154000120713            AND partite.ksc = ndppa00f.ppaKsc
154100120713            AND partite.dtPar = ndppa00f.ppaDtPar
154200120713            AND partite.nrPar = ndppa00f.ppaNrPar
154300120713            AND partite.serPar = ndppa00f.ppaSerPar
154400120618           JOIN ndpps00f
154500120618             ON ndppa00f.ppaSys = ndpps00f.ppsSys
154600120618            AND ndppa00f.ppaNrAsInt = ndpps00f.ppsNrAsInt
154700130610       //  WHERE ndpps00f.ppsCtgAna1 = CASE :local.situazioneCompleta
154800130610       //                              WHEN '0' THEN ''
154900130610       //                              ELSE ndpps00f.ppsCtgAna1
155000130610       //                              END
155002130610           WHERE (:local.situazioneCompleta <> '0' or
155004130610                    ndpps00f.ppsCtgAna1 <> 'CTZ')
155100120718           FOR READ ONLY
155200120618       ;
155300120618
155400120618       EXEC SQL OPEN partiteDaContendere;
155500120618
155600120618       IF sqlCode < *ZERO;
155700120618         DUMP(A);
155800120618         RETURN sqlCode;
155900120618       ENDIF;
156000120618
156100120618       DOU sqlCode < *ZERO;
156200120618
156300120618         EXEC SQL
156400120618           FETCH NEXT FROM partiteDaContendere
156500120618             INTO :partitaDaContendere
156600120618         ;
156700120618
156800120618         SELECT;
156900120618           WHEN sqlCode = SQLCODE_ROW_NOT_FOUND;
157000120618             LEAVE;
157100120618           WHEN sqlCode < *ZERO;
157200120618             DUMP(A);
157300120618             retField = sqlCode;
157400120618             LEAVE;
157500120618         ENDSL;
157600120618
157700120618         local.esito = YCOCTZOUT_AddPartitaAlContenzioso
157800120618                       ( partitaDaContendere.societa
157900120618                       : partitaDaContendere.ctb
158000120618                       : partitaDaContendere.kcc
158100120618                       : partitaDaContendere.ksc
158200120618                       : partitaDaContendere.dtPar
158300120618                       : partitaDaContendere.nrPar
158400120618                       : partitaDaContendere.serPar
158500120618                       : partitaDaContendere.saldo
158600120713                       : partitaDaContendere.esposizione
158700121026                       : partitaDaContendere.dtSca
158800120618                       );
158900120618
159000120619         IF local.esito < YCOCTZ_ESITO_OK;
159100120619           retField = local.esito;
159200120619           LEAVE;
159300120619         ENDIF;
159400120619
159500120619         priNumeroPartite += 1;
159600120619         priSaldoContabile += partitaDaContendere.saldo;
159700120619
159800120618       ENDDO;
159900120618
160000120618       EXEC SQL CLOSE partiteDaContendere;
160100120618
160200120618       RETURN retField;
160300120618
160400120618      /END-FREE
160500120618     P YCOCTZOUT_AddClienteAlContenzioso...
160600120618     P                 E
160700120620
160800120620
160900120620     P*--------------------------------------------------
161000120620     P* Procedure name: YCOCTZOUT_IsFatturazioneInCorso
161100120620     P* Purpose:        Restituisce *ON se è in corso la fatturazione.
161200120620     P* Returns:        *ON = fatturazione in corso.
161300120620     P*--------------------------------------------------
161400120620     P YCOCTZOUT_IsFatturazioneInCorso...
161500120620     P                 B                   EXPORT
161600120620     D YCOCTZOUT_IsFatturazioneInCorso...
161700120620     D                 PI              N
161800120620
161900120620     D retField        S               N   STATIC
162000120620
162100120620      /FREE
162200120620
162300120620       RESET retField;
162400120620
162500120620       EXEC SQL
162600120620         SELECT CHAR('1', 1)
162700120620           INTO :retField
162800120620           FROM kpLog00f
162900120620           WHERE SUBSTR(logKpj, 60, 4) = 'SF02'
163000120620             AND logAtt = '' AND logDai > 0 AND logDaf = 0
163100120620           FETCH FIRST 1 ROW ONLY
163200120620       ;
163300120620
163400120620       IF sqlCode < *ZERO;
163500120620         DUMP(A);
163600120620       ENDIF;
163700120620
163800120620       RETURN retField;
163900120620
164000120620      /END-FREE
164100120620     P YCOCTZOUT_IsFatturazioneInCorso...
164200120620     P                 E
164300120711
164400120711
164500120711     P*--------------------------------------------------
164600120711     P* Procedure name: YCOCTZOUT_SetStatoCreditoPratica
164700120711     P* Purpose:        Imposta lo stato del credito della pratica.
164800120711     P* Returns:        Esito.
164900120711     P* Parameter:      priIdContenzioso => ID contenzioso.
165000120711     P* Parameter:      priStatoDelCredito => ID stato del credito.
165100120711     P*--------------------------------------------------
165200120711     P YCOCTZOUT_SetStatoCreditoPratica...
165300120711     P                 B                   EXPORT
165400120711     D YCOCTZOUT_SetStatoCreditoPratica...
165500120711     D                 PI            10I 0
165600120711     D  priStatoDelCredito...
165700120711     D                                4A   CONST
165800120712     D  priRqsOpCode...
165900120712     D                               10I 0 CONST OPTIONS(*NOPASS)
166000120712     D  priIdRiga...
166100120712     D                               10I 0 CONST OPTIONS(*NOPASS)
166200120711
166300120711     D retField        S             10I 0 STATIC
166400120711     D local           DS                  STATIC INZ QUALIFIED
166500120712     D  rqsOpCode                    10I 0 INZ(YCOCTZOUT_OPERAZIONE_NUOVO_STATO)
166600120712     D  idStatoCredito...
166700120712     D                                4A
166800120712     D  esito                        10I 0
166900120711     D  p                            10I 0
167000120711     D  nct                          10I 0
167100120711     D  ncl                          10I 0
167200120711     D  idSocieta                     3A
167300120711     D  sottoConto                    8A
167400130606     D  soloAnagraficaCliente...
167401130606     D                                 N
167402130606     D resultClienti   DS                  LIKEDS(YCOCTZSEL_resultClienti)
167700120711     D                                     DIM(255) STATIC
167800120711
167900120711      /FREE
168000120711
168100120711       IF NOT done.newPratica;
168200120711         RETURN YCOCTZ_ERRORE_PRATICA_NON_INIZIATA;
168300120711       ENDIF;
168400120711
168500120711       RESET retField;
168600120711       RESET local;
168700120712       local.idStatoCredito = priStatoDelCredito;
168800120712
168900120712       IF %PARMS() > 1 AND priRqsOpCode = YCOCTZOUT_OPERAZIONE_CANCELLARE_STATO;
169000120712
169100120712         EXEC SQL
169200120712           UPDATE yCtzSta0f
169300120712             SET profilo_utente_delete = :kpjba.knmus
169400120712               , data_delete = CURRENT_DATE
169500120712             WHERE id_riga = :priIdRiga
169600120712             WITH UR
169700120712         ;
169800120712
169900120712         SELECT;
170000120712           WHEN sqlCode < *ZERO;
170100120712             DUMP(A);
170200120712             RETURN sqlCode;
170300120712           WHEN sqlCode = SQLCODE_ROW_IN_USE;
170400120712             RETURN sqlCode;
170500120712           WHEN sqlCode = SQLCODE_ROW_NOT_FOUND;
170600120712             RETURN sqlCode;
170700120712         ENDSL;
170800120712
170900120712         // Adesso lo stato della pratica è ...
171000120712
171100120712         EXEC SQL
171200120712           SELECT ID_STATO_CREDITO
171300120712             INTO :local.idStatoCredito
171400120712             FROM yCtzPra1v
171500120712             WHERE id_contenzioso = :yCtzPra0f.idContenz
171600120712         ;
171700120712
171800120712         local.soloAnagraficaCliente = *ON;
171900120712
172000120712       ENDIF;
172100120712
172200120711       // Reperisco tutti i codici cliente da contendere e li aggiorno con
172300120711       // lo stato del credito e il codice cliente di fatturazione accentratore.
172400130606
172500120718       IF pratica.operazione = YCOCTZOUT_OPERAZIONE_INSERIRE;
172600120718         local.nct = YCOCTZSEL_NewClientiByIdCliente( yCtzPra0f.idCliSoc
172700120718                                                    : yCtzPra0f.idCliKsc
172800120718                                                    : *ON
172900130606                                                    : *ON);
173000120718       ELSE;
173100120718         local.nct = YCOCTZSEL_NewClientiByIdCliente( yCtzPra0f.idCliSoc
173200120718                                                    : yCtzPra0f.idCliKsc
173300120718                                                    : *OFF
173400130606                                                    : *ON);
173500120718       ENDIF;
173600120711
173700120711       DOU local.ncl < *ZERO;
173800120711
173900120711         local.ncl = YCOCTZSEL_FetchNextClientiByIdCliente
174000120711                     ( %ELEM(resultClienti) : resultClienti );
174100120711
174200120711         IF local.ncl < 1;
174300120711           LEAVE;
174400120711         ENDIF;
174500120711
174600120711         FOR local.p = 1 TO local.ncl;
174700120711
174800120711           local.sottoConto = resultClienti(local.p).idCliente;
174900120711
175000120711           // Imposto stato del credito.
175100120711
175200120712           local.esito = YCOCTZOUT_SetStatoCreditoCliente
175300120712                         ( yCtzPra0f.idCliSoc
175400120712                         : local.sottoConto
175500120712                         : local.idStatoCredito
175600120712                         : local.soloAnagraficaCliente
175700120712                         );
175800120711
175900120711           SELECT;
176000120711             WHEN local.esito = SQLCODE_ROW_IN_USE;
176100120711               retField = local.esito;
176200120711             WHEN local.esito < *ZERO;
176300120711               retField = local.esito;
176400120711           ENDSL;
176500120711
176600120711         ENDFOR;
176700120711
176800120711       ENDDO;
176900120711
177000120711       local.esito = YCOCTZSEL_EndClientiByIdCliente();
177100120711
177200120711       RETURN retField;
177300120711
177400120711      /END-FREE
177500120711     P YCOCTZOUT_SetStatoCreditoPratica...
177600120711     P                 E
177700120716
177800120716
177900120716     P*--------------------------------------------------
178000120716     P* Procedure name: YCOCTZOUT_SetFineContenzioso
178100120716     P* Purpose:        Imposta la fine del contenzioso.
178200120716     P* Returns:        Esito.
178300120716     P* Parameter:      priFineContenzioso => *ON = fine contenzioso.
178400120716     P*--------------------------------------------------
178500120716     P YCOCTZOUT_SetFineContenzioso...
178600120716     P                 B                   EXPORT
178700120716     D YCOCTZOUT_SetFineContenzioso...
178800120716     D                 PI            10I 0
178900120716     D  priFineContenzioso...
179000120716     D                                 N   CONST
179100120716
179200120716     D retField        S             10I 0 STATIC
179300120716
179400120716      /FREE
179500120716
179600120716       IF NOT done.newPratica;
179700120716         RETURN YCOCTZ_ERRORE_PRATICA_NON_INIZIATA;
179800120716       ENDIF;
179900120716
180000120716       RESET retField;
180100120716
180200120716       EXEC SQL
180300120716         UPDATE yCtzPra0f
180400120716           SET data_fine_contenzioso = CASE :priFineContenzioso
180500120716                                       WHEN '1' THEN CURRENT_DATE
180600120716                                       ELSE NULL
180700120716                                       END
180800120716             , profilo_utente_update = RTRIM(:kpjba.knmus)
180900120716             , data_update = CURRENT_DATE
181000120716           WHERE id_contenzioso = :yCtzPra0f.idContenz
181100120716           WITH RS
181200120716       ;
181300120716
181400120716       IF sqlCode < *ZERO;
181500120716         DUMP(A);
181600120716         RETURN sqlCode;
181700120716       ENDIF;
181800120716
181900120716       RETURN retField;
182000120716
182100120716      /END-FREE
182200120716     P YCOCTZOUT_SetFineContenzioso...
182300120716     P                 E
182400120716
