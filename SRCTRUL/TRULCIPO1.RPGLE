000100060301     ***********************************************************************************************
000200060228     **
000300060228     ** Questo programma controlla numero, ABI, CAB e data di un assegno.
000400060228     **
000500060228     ***********************************************************************************************
000700060228     H DATEDIT(*YMD) DECEDIT(*JOBRUN)
000800060228
000900060228     ***********************************************************************************************
001000060228     **
001100060228     ** Definizione files.
001200060228     **
001300060228     ***********************************************************************************************
001400060228     Fcnabi00f  IF   E           K DISK
001500060228     F                                     USROPN
001600060301     Ftabel00f  IF   E           K DISK
001700060301     F                                     USROPN
001800060301
001900060301     D*--------------------------------------------------
002000060301     D* Procedure name: chkIncassoPO
002100060301     D* Purpose:        Controlla i dati dell'incasso di un PO
002200060301     D* Returns:        Esito
002300060301     D* Parameter:      tic => Tipo incasso
002400060301     D* Parameter:      tpd => Tipo documento incassato
002500060301     D* Parameter:      dati1a => ds1a
002600060301     D* Parameter:      nra => Numero assegno
002700060301     D* Parameter:      abi => Codice ABI assegno
002800060301     D* Parameter:      cab => Codice CAB assegno
002900060301     D* Parameter:      dte => Data emissione assegno
003000060301     D* Parameter:      esito
003100060301     D* Parameter:      prmMsg => Messaggio di errore
003200060301     D*--------------------------------------------------
003300060301     D chkIncassoPO    PR            10I 0
003400060301     D  tic                                LIKE(diz$Tic)
003500060301     D  tpd                           1A
003600060301     D  dati1a                             LIKE(tblUni)
003700060301     D                                     OPTIONS(*OMIT)
003800060301     D  nra                                LIKE(diz$nra)
003900060301     D                                     OPTIONS(*OMIT)
004000060301     D  abi                                LIKE(diz$abi)
004100060301     D                                     OPTIONS(*OMIT)
004200060301     D  cab                                LIKE(diz$cab)
004300060301     D                                     OPTIONS(*OMIT)
004400060301     D  dte                          10D   DATFMT(*ISO)
004500060301     D                                     OPTIONS(*OMIT)
004600060301     D  esito                        10I 0
004700060301     D  prmMsg                      512A   OPTIONS(*OMIT)
004701060301
004702060301     D*--------------------------------------------------
004703060301     D* Procedure name: chkNumeroAssegno
004704060301     D* Purpose:        Controllo numero assegno
004705060301     D* Returns:        Esito
004706060301     D* Parameter:      nra => Numero assegno
004707060301     D* Parameter:      esito => Esito
004708060301     D*--------------------------------------------------
004709060301     D chkNumeroAssegno...
004710060301     D                 PR            10I 0
004711060301     D  nra                                LIKE(diz$Nra)
004712060301     D  esito                        10I 0
004713060301     D  prmMsg                      512A   OPTIONS(*OMIT)
004714060301
004715060301     D*--------------------------------------------------
004716060301     D* Procedure name: chkAbiCab
004717060301     D* Purpose:        Controllo ABI CAB
004718060301     D* Returns:        Esito
004719060301     D* Parameter:      abi
004720060301     D* Parameter:      cab
004721060301     D* Parameter:      esito
004722060301     D*--------------------------------------------------
004723060301     D chkAbiCab       PR            10I 0
004724060301     D  abi                                LIKE(diz$Abi)
004725060301     D  cab                                LIKE(diz$Cab)
004726060301     D  esito                        10I 0
004727060301     D  prmMsg                      512A   OPTIONS(*OMIT)
004728060301     D  prmCnabi                           OPTIONS(*OMIT)
004729060301     D                                     LIKEDS(cnabids)
004730060301
004731060301     D*--------------------------------------------------
004732060301     D* Procedure name: chkDataAssegno
004733060301     D* Purpose:        Controllo validità data emissione assegno
004734060301     D* Returns:        Esito
004735060301     D* Parameter:      dte => Data emissione assegno
004736060301     D* Parameter:      esito
004737060301     D* Parameter:      prmMsg    => Messaggio di errore.
004738060301     D*--------------------------------------------------
004739060301     D chkDataAssegno  PR            10I 0
004740060301     D  dte                          10D   DATFMT(*ISO)
004741060301     D  esito                        10I 0
004742060301     D  prmMsg                      512A   OPTIONS(*OMIT)
004800060228
004900060228     ***********************************************************************************************
005000060228     **
005100060228     ** Definizione costanti.
005200060228     **
005300060228     ***********************************************************************************************
005400060228     D erroreParametri...
005500060228     D                 C                   -1
005600060301     D Tabella1ANonTrovata...
005700060301     D                 C                   1
005800060301     D Tabella1AAnnullata...
005900060301     D                 C                   2
006000060301     D NumeroAssegnoObbligatorio...
006100060301     D                 C                   3
006200060301     D NumeroAssegnoNonValido...
006300060301     D                 C                   4
006400060301     D AbiCabObbligatorio...
006500060301     D                 C                   5
006600060301     D AbiCabInesistente...
006700060301     D                 C                   6
006800060301     D CabInesistente...
006900060301     D                 C                   7
007000060301     D DataAssegnoObbligatoria...
007100060301     D                 C                   8
007200060301     D AssegnoPostDatato...
007300060301     D                 C                   9
007400060228
007500060228     ***********************************************************************************************
007600060228     **
007700060228     ** Definizione strutture dati.
007800060228     **
007900060228     ***********************************************************************************************
008000060228     D cndizion      E DS
008100060228     D                                     BASED(nullPtr)
008200060228     D                                     PREFIX(diz)
008300060301     D cnabids       E DS                  EXTNAME(cnabi00f)
008400060301     D                                     INZ
008500060301     D ds1a          E DS
008600060301     D                                     INZ
008700060228
008800060228     ***********************************************************************************************
008900060228     **
009000060228     ** Definizione variabili.
009100060228     **
009200060228     ***********************************************************************************************
009300060228     D nullPtr         S               *
009301060301     D messaggio       S            512A
009400060228
009500060228      /FREE
009600060301       RETURN;
009700060228      /END-FREE
009800060301
009900060301     P*--------------------------------------------------
010000060301     P* Procedure name: chkIncassoPO
010100060301     P* Purpose:        Controlla i dati dell'incasso di un PO
010200060301     P* Returns:        Esito
010300060301     P* Parameter:      tic => Tipo incasso
010400060301     P* Parameter:      tpd => Tipo documento incassato
010500060301     P* Parameter:      dati1a = ds1a
010600060301     P* Parameter:      nra => Numero assegno
010700060301     P* Parameter:      abi => Codice ABI assegno
010800060301     P* Parameter:      cab => Codice CAB assegno
010900060301     P* Parameter:      dte => Data emissione assegno
011000060301     P* Parameter:      esito
011100060301     P* Parameter:      prmMsg    => Messaggio di errore
011200060301     P*--------------------------------------------------
011300060301     P chkIncassoPO    B                   EXPORT
011400060301     D chkIncassoPO    PI            10I 0
011500060301     D  tic                                LIKE(diz$Tic)
011600060301     D  tpd                           1A
011700060301     D  dati1a                             LIKE(tblUni)
011800060301     D                                     OPTIONS(*OMIT)
011900060301     D  nra                                LIKE(diz$nra)
012000060301     D                                     OPTIONS(*OMIT)
012100060301     D  abi                                LIKE(diz$abi)
012200060301     D                                     OPTIONS(*OMIT)
012300060301     D  cab                                LIKE(diz$cab)
012400060301     D                                     OPTIONS(*OMIT)
012500060301     D  dte                          10D   DATFMT(*ISO)
012600060301     D                                     OPTIONS(*OMIT)
012700060301     D  esito                        10I 0
012800060301     D  prmMsg                      512A   OPTIONS(*OMIT)
017100060301
017200060301     D NonObbligatorio...
017300060301     D                 C                   'N'
017400060301
017500060301     D ds1a          E DS
017600060301     D                                     INZ
017700060301
017800060301      /FREE
017900060301
018000060301       CLEAR esito;
018100060301       IF %ADDR(prmMsg) <> *NULL;
018200060301         CLEAR prmMsg;
018300060301       ENDIF;
018400060301
018500060301       IF %ADDR(dati1a) <> *NULL AND dati1a <> ' ';
018600060301         ds1a = dati1a;
018700060301       ELSE;
018800060301         IF NOT %OPEN(tabel00f);
018900060301           OPEN tabel00f;
019000060301         ENDIF;
019100060301         CHAIN (1:'1A':tic) tabel00f;
019200060301         IF NOT %FOUND;
019300060301           esito = Tabella1ANonTrovata;
019400060301           IF %ADDR(prmMsg) <> *NULL;
019500060301             prmMsg = 'Tabella 1A ' + tic + ' non trovata.';
019600060301           ENDIF;
019700060301           RETURN esito;
019800060301         ENDIF;
019900060301         IF tblFlg <> ' ';
020000060301           esito = Tabella1AAnnullata;
020100060301           IF %ADDR(prmMsg) <> *NULL;
020200060301             prmMsg = 'Tabella 1A ' + tic + ' annullata.';
020300060301           ENDIF;
020400060301           RETURN esito;
020500060301         ENDIF;
020600060301         ds1a = tblUni;
020700060301         IF %ADDR(dati1a) <> *NULL;
020800060301           dati1a = tblUni;
020900060301         ENDIF;
021000060301       ENDIF;
021100060301
021200060301       IF §1aSas <> ' ';
021300060301         IF §1aNas <> NonObbligatorio;
021400060301           IF %ADDR(nra) = *NULL;
021500060301             esito = NumeroAssegnoObbligatorio;
021600060301             IF %ADDR(prmMsg) <> *NULL;
021700060301               prmMsg = 'Numero assegno obbligatorio.';
021800060301             ENDIF;
021900060301             RETURN esito;
022000060301           ENDIF;
022100060301           IF chkNumeroAssegno(nra:esito:messaggio) <> 0;
022200060301             IF %ADDR(prmMsg) <> *NULL;
022300060301               prmMsg = messaggio;
022400060301             ENDIF;
022500060301             RETURN esito;
022600060301           ENDIF;
022700060301         ENDIF;
022800060301         IF §1aaBI <> NonObbligatorio;
022900060301           IF %ADDR(abi) = *NULL OR %ADDR(cab) = *NULL;
023000060301             esito = AbiCabObbligatorio;
023100060301             IF %ADDR(prmMsg) <> *NULL;
023200060301               prmMsg = 'ABI e CAB obbligatori.';
023300060301             ENDIF;
023400060301             RETURN esito;
023500060301           ENDIF;
023600060301           IF chkAbiCab(abi:cab:esito:messaggio) <> 0;
023601060301             IF %ADDR(prmMsg) <> *NULL;
023602060301               prmMsg = messaggio;
023603060301             ENDIF;
023700060301             RETURN esito;
023800060301           ENDIF;
023900060301         ENDIF;
024000060301         IF %ADDR(dte) = *NULL;
024100060301           esito = DataAssegnoObbligatoria;
024200060301           RETURN esito;
024300060301         ENDIF;
024400060301         IF chkDataAssegno(dte:esito:messaggio) <> 0;
024401060301           IF %ADDR(prmMsg) <> *NULL;
024402060301             prmMsg = messaggio;
024403060301           ENDIF;
024500060301           RETURN esito;
024600060301         ENDIF;
024700060301       ENDIF;
024800060301
024900060301       RETURN esito;
025000060301
025100060301      /END-FREE
025200060301     P chkIncassoPO    E
025300060301
025400060301     P*--------------------------------------------------
025500060301     P* Procedure name: chkNumeroAssegno
025600060301     P* Purpose:        Controllo numero assegno
025700060301     P* Returns:        Esito
025800060301     P* Parameter:      nra => Numero assegno
025900060301     P* Parameter:      esito => Esito
026000060301     P*--------------------------------------------------
026100060301     P chkNumeroAssegno...
026200060301     P                 B                   EXPORT
026300060301     D chkNumeroAssegno...
026400060301     D                 PI            10I 0
026500060301     D  nra                                LIKE(diz$Nra)
026600060301     D  esito                        10I 0
026700060301     D  prmMsg                      512A   OPTIONS(*OMIT)
026800060301
026900060301     D nNra            S              4P 0
027000060301
027100060301      /FREE
027200060301
027300060301       CLEAR esito;
027400060301       IF %ADDR(prmMsg) <> *NULL;
027500060301         CLEAR prmMsg;
027600060301       ENDIF;
027700060301
027800060301       IF nra = ' ';
027900060301         esito = NumeroAssegnoObbligatorio;
028000060301         IF %ADDR(prmMsg) <> *NULL;
028100060301           prmMsg = 'Numero assegno obbligatorio.';
028200060301         ENDIF;
028300060301         RETURN esito;
028400060301       ENDIF;
028500060301
028600060301       nNra = %DEC(nra:%SIZE(nNra):0);
028700060301       IF %STATUS = 105;
028800060301         esito = NumeroAssegnoNonValido;
028900060301         IF %ADDR(prmMsg) <> *NULL;
029000060301           prmMsg = 'Numero assegno ' + nra + ' non valido.';
029100060301         ENDIF;
029200060301         RETURN esito;
029300060301       ENDIF;
029400060301
029500060301       nra = %EDITC(nNra:'X');
029600060301       RETURN esito;
029700060301
029800060301      /END-FREE
029900060301     P chkNumeroAssegno...
030000060301     P                 E
030100060301
030200060301     P*--------------------------------------------------
030300060301     P* Procedure name: chkAbiCab
030400060301     P* Purpose:        Controllo ABI CAB
030500060301     P* Returns:        Esito
030600060301     P* Parameter:      abi
030700060301     P* Parameter:      cab
030800060301     P* Parameter:      esito
030900060301     P*--------------------------------------------------
031000060301     P chkAbiCab       B                   EXPORT
031100060301     D chkAbiCab       PI            10I 0
031200060301     D  abi                                LIKE(diz$Abi)
031300060301     D  cab                                LIKE(diz$Cab)
031400060301     D  esito                        10I 0
031500060301     D  prmMsg                      512A   OPTIONS(*OMIT)
031600060301     D  prmCnabi                           OPTIONS(*OMIT)
031700060301     D                                     LIKEDS(cnabids)
031800060301
031900060301      /FREE
032000060301
032100060301       CLEAR esito;
032200060301       IF %ADDR(prmMsg) <> *NULL;
032300060301         CLEAR prmMsg;
032400060301       ENDIF;
032500060301       IF %ADDR(prmCnabi) <> *NULL;
032600060301         CLEAR prmCnabi;
032700060301         CLEAR cnabids;
032800060301       ENDIF;
032900060301
033000060301       IF abi = 0 OR cab = 0;
033100060301         esito = AbiCabObbligatorio;
033200060301         IF %ADDR(prmMsg) <> *NULL;
033300060301           prmMsg = 'ABI e CAB obbligatori.';
033400060301         ENDIF;
033500060301         RETURN esito;
033600060301       ENDIF;
033700060301
033800060301       IF NOT %OPEN(cnabi00f);
033900060301         OPEN cnabi00f;
034000060301       ENDIF;
034100060301
034200060301       CHAIN (abi:cab) cnabi00f;
034300060301
034400060301       IF %FOUND;
034500060301         IF %ADDR(prmCnabi) <> *NULL;
034600060301           prmCnabi = cnabids;
034700060301         ENDIF;
034800060301         RETURN esito;
034900060301       ENDIF;
035000060301
035100060301       CHAIN (abi) cnabi00f;
035200060301       IF %FOUND;
035300060301         esito = CabInesistente;
035400060301         IF %ADDR(prmMsg) <> *NULL;
035500060301           prmMsg = 'CAB inesistente.';
035600060301         ENDIF;
035700060301       ELSE;
035800060301         esito = ABICabInesistente;
035900060301         IF %ADDR(prmMsg) <> *NULL;
036000060301           prmMsg = 'ABI e CAB inesistente.';
036100060301         ENDIF;
036200060301       ENDIF;
036300060301
036400060301       RETURN esito;
036500060301
036600060301      /END-FREE
036700060301     P chkAbiCab       E
036800060301
036900060301     P*--------------------------------------------------
037000060301     P* Procedure name: chkDataAssegno
037100060301     P* Purpose:        Controllo validità data emissione assegno
037200060301     P* Returns:        Esito
037300060301     P* Parameter:      dte => Data emissione assegno
037400060301     P* Parameter:      esito
037500060301     P* Parameter:      prmMsg => Messaggio di errore.
037600060301     P*--------------------------------------------------
037700060301     P chkDataAssegno  B                   EXPORT
037800060301     D chkDataAssegno  PI            10I 0
037900060301     D  dte                          10D   DATFMT(*ISO)
038000060301     D  esito                        10I 0
038100060301     D  prmMsg                      512A   OPTIONS(*OMIT)
038200060301
038300060301     D GiorniFranchigia...
038400060301     D                 C                   4
038500060301
038600060301      /FREE
038700060301
038800060301       CLEAR esito;
038900060301       IF %ADDR(prmMsg) <> *NULL;
039000060301         CLEAR prmMsg;
039100060301       ENDIF;
039200060301
039300060301       IF %DIFF(dte:%DATE():*DAYS) > GiorniFranchigia;
039400060301         esito = AssegnoPostDatato;
039500060301         IF %ADDR(prmMsg) <> *NULL;
039600060301           prmMsg = 'Assegno postdatato.';
039700060301         ENDIF;
039800060301         RETURN esito;
039900060301       ENDIF;
040000060301
040100060301       RETURN esito;
040200060301
040300060301      /END-FREE
040400060301     P chkDataAssegno  E
040500060301
