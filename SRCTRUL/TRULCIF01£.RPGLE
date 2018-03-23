000100060313     ***********************************************************************************************
000200060228     **
000300060302     ** Questo modulo controlla l'incasso operativo delle filiali.
000400060228     **
000500060228     ***********************************************************************************************
000600060313     H NOMAIN BNDDIR('QC2LE')
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
001900060301     ***********************************************************************************************
002000060301     **
002100060301     ** Definizione delle procedure esterne usate.
002200060301     **
002300060301     ***********************************************************************************************
002400060301     D atoi            PR            10I 0
002500060301     D                                     EXTPROC('atoi')
002600060301     D  char                           *
002700060301     D                                     VALUE
002800060301     D                                     OPTIONS(*STRING)
002900060302
003000060302     ***********************************************************************************************
003100060302     **
003200060302     ** Definizione costanti.
003300060302     **
003400060302     ***********************************************************************************************
003500060302     D Assegnato       C                   'A'
003600060302     D Contrassegno    C                   'C'
003700060302     D PrePagato       C                   'P'
003800060315     D POS             C                   'P'
003900060302     D InTabellaTM     C                   'S'
004000060302     D Mittente        C                   'M'
004100060308     D NoAssegno...
004200060308     D                 C                   ' '
004300060302     D NonObbligatorio...
004400060302     D                 C                   'N'
004500060313     D Controllo...
004600060313     D                 C                   'C'
004700060313     D ErroreParametri...
004800060302     D                 C                   -1
004900060302     D Tabella1ANonTrovata...
005000060302     D                 C                   'A'
005100060302     D Tabella1AAnnullata...
005200060302     D                 C                   'B'
005300060302     D NumeroAssegnoObbligatorio...
005400060302     D                 C                   'C'
005500060302     D NumeroAssegnoNonValido...
005600060302     D                 C                   'D'
005700060302     D AbiObbligatorio...
005800060302     D                 C                   'E'
005900060302     D AbiCabInesistente...
006000060302     D                 C                   'F'
006100060302     D CabInesistente...
006200060302     D                 C                   'G'
006300060302     D DataAssegnoObbligatoria...
006400060302     D                 C                   'H'
006500060302     D AssegnoPostDatato...
006600060302     D                 C                   'I'
006700060302     D TipoIncassoNonAmmesso...
006800060302     D                 C                   'J'
006900060302     D CabObbligatorio...
007000060302     D                 C                   'K'
007100060308     D NonImmettereNumeroAssegno...
007200060308     D                 C                   'L'
007300060308     D NonImmettereAbiCab...
007400060308     D                 C                   'M'
007500060308     D NonImmettereDataAssegno...
007600060308     D                 C                   'N'
007700060310     D SpecieAssegnoObbligatoria...
007800060310     D                 C                   'O'
007900060310     D Tabella4ENonTrovata...
008000060310     D                 C                   'P'
008100060310     D Tabella4EAnnullata...
008200060310     D                 C                   'Q'
008300060310     D AssegnoVecchio1...
008400060310     D                 C                   'R'
008500060310     D AssegnoVecchio2...
008600060310     D                 C                   'S'
008700060315     D TabellaY4ONonTrovata...
008800060315     D                 C                   'T'
008900060315     D TabellaY4OAnnullata...
009000060315     D                 C                   'U'
009100060315     D IncassoPosNonAmmesso...
009200060315     D                 C                   'V'
009300060411     D AssegnoPostDatatoNonAmmesso...
009400060411     D                 C                   'W'
009500060302
009600060302     ***********************************************************************************************
009700060302     **
009800060302     ** Definizione strutture dati.
009900060302     **
010000060302     ***********************************************************************************************
010100060302     D cndizion      E DS
010200060302     D                                     BASED(nullPtr)
010300060302     D                                     PREFIX(diz)
010400060302     D cnabids       E DS                  EXTNAME(cnabi00f)
010500060302     D                                     INZ
010600060302     D ds1a          E DS
010700060302     D                                     INZ
010800060310     D ds4e          E DS
010900060310     D                                     INZ
011000060313     D tibs02ds      E DS
011100060313     D                                     INZ
011200060313     D  t02mod       E
011300060313     D                                     INZ(Controllo)
011400060313     D kpjba         E DS
011500060313     D                                     INZ
011600060302
011700060302     ***********************************************************************************************
011800060302     **
011900060302     ** Definizione variabili.
012000060302     **
012100060302     ***********************************************************************************************
012200060302     D nullPtr         S               *
012300060302     D gblErr          S              1A
012400060302     D                                     DIM(10)
012500060302     D gblMsg          S             80A
012600060302     D                                     DIM(10)
012700060302     D lclErr          S              1A
012800060302     D                                     DIM(10)
012900060302     D lclMsg          S             80A
013000060302     D                                     DIM(10)
013100060302     D i               S              5I 0
013200060302     D j               S              5I 0
013300060313
013400060313     ***********************************************************************************************
013500060313     **
013600060313     ** Definizione procedure esterne.
013700060313     **
013800060313     ***********************************************************************************************
013900060313     D tibs02r         PR                                                       Ricerca TNTBE00F
014000060313     D                                     EXTPGM('TIBS02R')
014100060313     D  kpjba
014200060313     D                                     LIKEDS(kpjba)
014300060313     D  tibs02ds
014400060313     D                                     LIKEDS(tibs02ds)
014500060313
014600060301     D*--------------------------------------------------
014700060301     D* Procedure name: chkIncassoPO
014800060301     D* Purpose:        Controlla i dati dell'incasso di un PO
014900060301     D* Returns:        Esito
015000060301     D* Parameter:      tic => Tipo incasso
015100060301     D* Parameter:      tpd => Tipo documento incassato
015200060301     D* Parameter:      datiTic => ds1a
015300060301     D* Parameter:      nra => Numero assegno
015400060301     D* Parameter:      abi => Codice ABI assegno
015500060301     D* Parameter:      cab => Codice CAB assegno
015600060301     D* Parameter:      dte => Data emissione assegno
015700060301     D* Parameter:      esito
015800060302     D* Parameter:      lclMsg => Messaggio di errore
015900060301     D*--------------------------------------------------
016000060301     D chkIncassoPO    PR            10I 0
016100060315     D  soc                                LIKE(diz$Soc)
016200060315     D  fgs                                LIKE(diz$Fgs)
016300060301     D  tic                                LIKE(diz$Tic)
016400060301     D  tpd                           1A
016500060301     D  datiTic                            LIKE(tblUni)
016600060301     D                                     OPTIONS(*OMIT)
016700060315     D  datiY4O                            LIKE(t02Uni)
016800060315     D                                     OPTIONS(*OMIT)
016900060301     D  nra                                LIKE(diz$nra)
017000060301     D                                     OPTIONS(*OMIT)
017100060301     D  abi                                LIKE(diz$abi)
017200060301     D                                     OPTIONS(*OMIT)
017300060301     D  cab                                LIKE(diz$cab)
017400060301     D                                     OPTIONS(*OMIT)
017500060301     D  dte                          10D   DATFMT(*ISO)
017600060301     D                                     OPTIONS(*OMIT)
017700060301     D  esito                        10I 0
017800060302     D  prmErr                        1A
017900060302     D                                     DIM(10)
018000060302     D  prmMsg                       80A   OPTIONS(*OMIT)
018100060302     D                                     DIM(10)
018200060315     D  prmKpjba                           OPTIONS(*OMIT:*NOPASS)
018300060315     D                                     LIKEDS(kpjba)
018400060301
018500060301     D*--------------------------------------------------
018600060301     D* Procedure name: chkTipoIncasso
018700060301     D* Purpose:        Controlla esistenza tipo incasso su tabella 1A
018800060301     D* Returns:        Esito
018900060301     D* Parameter:      tic => Tipo incasso
019000060301     D* Parameter:      esito
019100060301     D* Parameter:      datiTic
019200060301     D*--------------------------------------------------
019300060301     D chkTipoIncasso  PR            10I 0
019400060301     D  tic                                LIKE(diz$tic)
019500060301     D  esito                        10I 0
019600060302     D  lclErr                        1A
019700060302     D                                     DIM(10)
019800060302     D  lclMsg                       80A   OPTIONS(*OMIT)
019900060302     D                                     DIM(10)
020000060301     D  datiTic                            LIKE(ds1a)
020100060301     D                                     OPTIONS(*OMIT)
020200060301
020300060301     D*--------------------------------------------------
020400060301     D* Procedure name: chkNumeroAssegno
020500060301     D* Purpose:        Controllo numero assegno
020600060301     D* Returns:        Esito
020700060301     D* Parameter:      nra => Numero assegno
020800060301     D* Parameter:      esito => Esito
020900060301     D*--------------------------------------------------
021000060301     D chkNumeroAssegno...
021100060301     D                 PR            10I 0
021200060301     D  nra                                LIKE(diz$Nra)
021300060301     D  esito                        10I 0
021400060302     D  lclErr                        1A
021500060302     D                                     DIM(10)
021600060302     D  lclMsg                       80A   OPTIONS(*OMIT)
021700060302     D                                     DIM(10)
021800060301
021900060301     D*--------------------------------------------------
022000060301     D* Procedure name: chkAbiCab
022100060301     D* Purpose:        Controllo ABI CAB
022200060301     D* Returns:        Esito
022300060301     D* Parameter:      abi
022400060301     D* Parameter:      cab
022500060301     D* Parameter:      esito
022600060301     D*--------------------------------------------------
022700060301     D chkAbiCab       PR            10I 0
022800060301     D  abi                                LIKE(diz$Abi)
022900060301     D  cab                                LIKE(diz$Cab)
023000060301     D  esito                        10I 0
023100060302     D  lclErr                        1A
023200060302     D                                     DIM(10)
023300060302     D  lclMsg                       80A   OPTIONS(*OMIT)
023400060302     D                                     DIM(10)
023500060301     D  prmCnabi                           OPTIONS(*OMIT)
023600060301     D                                     LIKEDS(cnabids)
023700060301
023800060301     D*--------------------------------------------------
023900060301     D* Procedure name: chkDataAssegno
024000060301     D* Purpose:        Controllo validità data emissione assegno
024100060301     D* Returns:        Esito
024200060301     D* Parameter:      dte => Data emissione assegno
024300060301     D* Parameter:      esito
024400060302     D* Parameter:      lclMsg    => Messaggio di errore.
024500060301     D*--------------------------------------------------
024600060301     D chkDataAssegno  PR            10I 0
024700060301     D  dte                          10D   DATFMT(*ISO)
024800060310     D  sas
024900060310     D                                     LIKE(§1aSas)
025000060301     D  esito                        10I 0
025100060302     D  lclErr                        1A
025200060302     D                                     DIM(10)
025300060302     D  lclMsg                       80A   OPTIONS(*OMIT)
025400060302     D                                     DIM(10)
025500060313
025600060313     D*--------------------------------------------------
025700060313     D* Procedure name: chkIncassoPosUsabile
025800060313     D* Purpose:        Controlla usabilità incasso POS
025900060313     D* Returns:        Esito
026000060315     D* Parameter:      soc => Società
026100060313     D* Parameter:      fgs => PO gestione
026200060313     D* Parameter:      datiY4O => Dati Y4O
026300060313     D* Parameter:      esito
026400060313     D* Parameter:      errori => Codici di errore
026500060313     D* Parameter:      errMsg => Nessaggi di errore.
026600060313     D*--------------------------------------------------
026700060313     D chkIncassoPosUsabile...
026800060313     D                 PR            10I 0
026900060315     D  soc                                LIKE(diz$soc)
027000060313     D  fgs                                LIKE(diz$fgs)
027100060313     D  datiY4O                            OPTIONS(*OMIT:*VARSIZE)
027200060313     D                                     LIKE(t02uni)
027300060313     D  prmKpjba                           OPTIONS(*OMIT)
027400060313     D                                     LIKEDS(kpjba)
027500060313     D  esito                        10I 0
027600060315     D  lclErr                        1A   DIM(10)
027700060315     D  lclMsg                       80A   DIM(10)
027800060313     D                                     OPTIONS(*OMIT)
027900060302
028000060302     D*--------------------------------------------------
028100060302     D* Procedure name: getlclErr
028200060302     D* Purpose:        Riceve gli lclErr dalle sottoprocedure
028300060302     D* Returns:        Esito
028400060302     D* Parameter:      Esito
028500060302     D*--------------------------------------------------
028600060302     D getLclErr       PR            10I 0
028700060302     D  esito                        10I 0
028800060301
028900060301     P*--------------------------------------------------
029000060301     P* Procedure name: chkIncassoPO
029100060301     P* Purpose:        Controlla i dati dell'incasso di un PO
029200060301     P* Returns:        Esito
029300060301     P* Parameter:      tic => Tipo incasso
029400060301     P* Parameter:      tpd => Tipo documento incassato
029500060301     P* Parameter:      datiTic = ds1a
029600060301     P* Parameter:      nra => Numero assegno
029700060301     P* Parameter:      abi => Codice ABI assegno
029800060301     P* Parameter:      cab => Codice CAB assegno
029900060301     P* Parameter:      dte => Data emissione assegno
030000060301     P* Parameter:      esito
030100060302     P* Parameter:      lclMsg    => Messaggio di errore
030200060301     P*--------------------------------------------------
030300060301     P chkIncassoPO    B                   EXPORT
030400060301     D chkIncassoPO    PI            10I 0
030500060315     D  soc                                LIKE(diz$Soc)
030600060315     D  fgs                                LIKE(diz$Fgs)
030700060301     D  tic                                LIKE(diz$Tic)
030800060301     D  tpd                           1A
030900060301     D  datiTic                            LIKE(tblUni)
031000060301     D                                     OPTIONS(*OMIT)
031100060315     D  datiY4O                            LIKE(t02Uni)
031200060315     D                                     OPTIONS(*OMIT)
031300060301     D  nra                                LIKE(diz$nra)
031400060301     D                                     OPTIONS(*OMIT)
031500060301     D  abi                                LIKE(diz$abi)
031600060301     D                                     OPTIONS(*OMIT)
031700060301     D  cab                                LIKE(diz$cab)
031800060301     D                                     OPTIONS(*OMIT)
031900060301     D  dte                          10D   DATFMT(*ISO)
032000060301     D                                     OPTIONS(*OMIT)
032100060301     D  esito                        10I 0
032200060302     D  prmErr                        1A
032300060302     D                                     DIM(10)
032400060302     D  prmMsg                       80A   OPTIONS(*OMIT)
032500060302     D                                     DIM(10)
032600060315     D  prmKpjba                           OPTIONS(*OMIT:*NOPASS)
032700060315     D                                     LIKEDS(kpjba)
032800060301
032900060301     D ds1a          E DS
033000060301     D                                     INZ
033100060301
033200060301      /FREE
033300060301
033400060301       CLEAR esito;
033500060302       CLEAR prmErr;
033600060302       IF %ADDR(prmMsg) <> *NULL;
033700060302         CLEAR prmMsg;
033800060301       ENDIF;
033900060302       CLEAR gblErr;
034000060302       CLEAR gblMsg;
034100060302       CLEAR i;
034200060301
034300060301       IF %ADDR(datiTic) <> *NULL AND datiTic <> ' ';
034400060301         ds1a = datiTic;
034500060301       ELSE;
034600060302         IF chkTipoIncasso(tic:esito:lclErr:lclMsg:ds1a) <> 0;
034700060302           getLclErr(esito);
034800060301         ENDIF;
034900060301         datiTic = ds1a;
035000060301       ENDIF;
035100060315
035200060315       IF %ADDR(prmKpjba) <> *NULL;
035300060315         kpjba = prmKpjba;
035400060315       ENDIF;
035500060301
035600060301       IF tpd = PrePagato AND (§1aFmb = Mittente OR §1aTtm = InTabellaTM);
035700060302         i += 1;
035800060302         gblErr(i) = TipoIncassoNonAmmesso;
035900060302         gblMsg(i) = 'Tipo incasso non ammesso.';
036000060301       ENDIF;
036100060301
036200060308       IF §1aSas = NoAssegno;
036300060308         IF %ADDR(nra) <> *NULL AND nra <> ' ';
036400060308           i += 1;
036500060308           gblErr(i) = NonImmettereNumeroAssegno;
036600060308           gblMsg(i) = 'Non immettere il numero assegno.';
036700060308         ENDIF;
036800060308         IF (%ADDR(abi) <> *NULL AND abi <> 0)
036900060308         OR (%ADDR(cab) <> *NULL AND cab <> 0);
037000060308           i += 1;
037100060308           gblErr(i) = NonImmettereAbiCab;
037200060308           gblMsg(i) = 'Non immettere ABI e CAB assegno.';
037300060308         ENDIF;
037400060308         IF %ADDR(dte) <> *NULL AND dte > *LOVAL;
037500060308           i += 1;
037600060308           gblErr(i) = NonImmettereDataAssegno;
037700060308           gblMsg(i) = 'Non immettere la data emissione assegno.';
037800060308         ENDIF;
037900060308       ELSE;
038000060301         IF §1aNas <> NonObbligatorio;
038100060301           IF %ADDR(nra) = *NULL;
038200060302             i += 1;
038300060302             gblErr(i) = NumeroAssegnoObbligatorio;
038400060302             gblMsg(i) = 'Numero assegno obbligatorio.';
038500060301           ENDIF;
038600060302           IF chkNumeroAssegno(nra:esito:lclErr:lclMsg) <> 0;
038700060302             getLclErr(esito);
038800060302           ENDIF;
038900060301         ENDIF;
039000060308         IF §1aAbi <> NonObbligatorio;
039100060301           IF %ADDR(abi) = *NULL;
039200060302             i += 1;
039300060302             gblErr(i) = AbiObbligatorio;
039400060302             gblMsg(i) = 'ABI obbligatorio.';
039500060301           ENDIF;
039600060301           IF %ADDR(cab) = *NULL;
039700060302             i += 1;
039800060302             gblErr(i) = CabObbligatorio;
039900060302             gblMsg(i) = 'CAB obbligatorio.';
040000060301           ENDIF;
040100060308           IF %ADDR(abi) <> *NULL AND %ADDR(cab) <> *NULL
040200060308           AND chkAbiCab(abi:cab:esito:lclErr:lclMsg:cnabids) <> 0;
040300060302             getLclErr(esito);
040400060302           ENDIF;
040500060301         ENDIF;
040600060301         IF %ADDR(dte) = *NULL;
040700060302           i += 1;
040800060302           gblErr(i) = DataAssegnoObbligatoria;
040900060302           gblMsg(i) = 'Data emissione assegno obbligatoria.';
041000060302         ELSE;
041100060310           IF chkDataAssegno(dte:§1aSas:esito:lclErr:lclMsg) <> 0;
041200060302             getLclErr(esito);
041300060302           ENDIF;
041400060301         ENDIF;
041500060301       ENDIF;
041600060301
041700060315       IF §1aFca = POS AND chkIncassoPosUsabile(soc:fgs:*OMIT:kpjba
041800060315       :esito:lclErr:lclMsg) <> 0;
041900060315         getLclErr(esito);
042000060315       ENDIF;
042100060315
042200060315       prmErr = gblErr;
042300060302       IF %ADDR(prmMsg) <> *NULL;
042400060302         prmMsg = gblMsg;
042500060302       ENDIF;
042600060302
042700060308       IF prmErr(1) <> ' ' AND esito = 0;
042800060308         esito = 1;
042900060308       ENDIF;
043000060308
043100060308       RETURN esito;
043200060301
043300060301      /END-FREE
043400060301     P chkIncassoPO    E
043500060301
043600060301     P*--------------------------------------------------
043700060301     P* Procedure name: chkNumeroAssegno
043800060301     P* Purpose:        Controllo numero assegno
043900060301     P* Returns:        Esito
044000060301     P* Parameter:      nra => Numero assegno
044100060301     P* Parameter:      esito => Esito
044200060301     P*--------------------------------------------------
044300060301     P chkNumeroAssegno...
044400060301     P                 B                   EXPORT
044500060301     D chkNumeroAssegno...
044600060301     D                 PI            10I 0
044700060301     D  nra                                LIKE(diz$Nra)
044800060301     D  esito                        10I 0
044900060302     D  lclErr                        1A
045000060302     D                                     DIM(10)
045100060302     D  lclMsg                       80A   OPTIONS(*OMIT)
045200060302     D                                     DIM(10)
045300060301
045400060313     D nNra4           S              4P 0
045500060313     D nNra10          S             10P 0
045600060301
045700060301      /FREE
045800060301
045900060301       CLEAR esito;
046000060302       CLEAR lclErr;
046100060302       IF %ADDR(lclMsg) <> *NULL;
046200060302         CLEAR lclMsg;
046300060301       ENDIF;
046400060302       CLEAR j;
046500060301
046600060301       IF nra = ' ';
046700060302         j += 1;
046800060302         lclErr(j) = NumeroAssegnoObbligatorio;
046900060302         IF %ADDR(lclMsg) <> *NULL;
047000060302           lclMsg(j) = 'Immettere il numero assegno (ultime 4 cifre).';
047100060301         ENDIF;
047200060313         esito = 1;
047300060313         RETURN esito;
047400060301       ENDIF;
047500060301
047600060302       MONITOR;
047700060313         nNra10 = %DEC(%TRIMR(nra):10:0);
047800060302         ON-ERROR 105;
047900060302           j += 1;
048000060302           lclErr(j) = NumeroAssegnoNonValido;
048100060302           IF %ADDR(lclMsg) <> *NULL;
048200060302             lclMsg(j) = 'Numero assegno non valido.';
048300060302           ENDIF;
048400060313         esito = 1;
048500060313         RETURN esito;
048600060302       ENDMON;
048700060302
048800060313       IF %LEN(%TRIMR(nra)) > 4;
048900060313         nra = %EDITC(nNra10:'X');
049000060313       ELSE;
049100060313         nNra4 = %DEC(%TRIMR(nra):4:0);
049200060313         nra = %EDITC(nNra4:'X');
049300060313       ENDIF;
049400060313
049500060313       RETURN esito;
049600060301
049700060301      /END-FREE
049800060301     P chkNumeroAssegno...
049900060301     P                 E
050000060301
050100060301     P*--------------------------------------------------
050200060301     P* Procedure name: chkAbiCab
050300060301     P* Purpose:        Controllo ABI CAB
050400060301     P* Returns:        Esito
050500060301     P* Parameter:      abi
050600060301     P* Parameter:      cab
050700060301     P* Parameter:      esito
050800060301     P*--------------------------------------------------
050900060301     P chkAbiCab       B                   EXPORT
051000060301     D chkAbiCab       PI            10I 0
051100060301     D  abi                                LIKE(diz$Abi)
051200060301     D  cab                                LIKE(diz$Cab)
051300060301     D  esito                        10I 0
051400060302     D  lclErr                        1A
051500060302     D                                     DIM(10)
051600060302     D  lclMsg                       80A   OPTIONS(*OMIT)
051700060302     D                                     DIM(10)
051800060301     D  prmCnabi                           OPTIONS(*OMIT)
051900060301     D                                     LIKEDS(cnabids)
052000060301
052100060301      /FREE
052200060301
052300060301       CLEAR esito;
052400060302       CLEAR lclErr;
052500060302       IF %ADDR(lclMsg) <> *NULL;
052600060302         CLEAR lclMsg;
052700060301       ENDIF;
052800060301       IF %ADDR(prmCnabi) <> *NULL;
052900060301         CLEAR prmCnabi;
053000060301         CLEAR cnabids;
053100060301       ENDIF;
053200060302       CLEAR j;
053300060301
053400060301       IF abi = 0;
053500060302         j += 1;
053600060302         lclErr(j) = AbiObbligatorio;
053700060302         IF %ADDR(lclMsg) <> *NULL;
053800060302           lclMsg(j) = 'ABI obbligatorio.';
053900060301         ENDIF;
054000060302         esito = 1;
054100060301       ENDIF;
054200060308
054300060301       IF cab = 0;
054400060302         j += 1;
054500060302         lclErr(j) = CabObbligatorio;
054600060302         IF %ADDR(lclMsg) <> *NULL;
054700060302           lclMsg(j) = 'CAB obbligatorio.';
054800060301         ENDIF;
054900060302         esito = 1;
055000060301       ENDIF;
055100060301
055200060308       IF esito <> 0;
055300060308         RETURN esito;
055400060308       ENDIF;
055500060308
055600060301       IF NOT %OPEN(cnabi00f);
055700060301         OPEN cnabi00f;
055800060301       ENDIF;
055900060301
056000060301       CHAIN (abi:cab) cnabi00f;
056100060301
056200060301       IF %FOUND;
056300060301         IF %ADDR(prmCnabi) <> *NULL;
056400060301           prmCnabi = cnabids;
056500060301         ENDIF;
056600060313         RETURN esito;
056700060301       ENDIF;
056800060301
056900060301       CHAIN (abi) cnabi00f;
057000060301       IF %FOUND;
057100060302         j += 1;
057200060302         lclErr(j) = CabInesistente;
057300060302         IF %ADDR(lclMsg) <> *NULL;
057400060302           lclMsg(j) = 'CAB inesistente.';
057500060301         ENDIF;
057600060302         esito = 1;
057700060301       ELSE;
057800060302         j += 1;
057900060302         lclErr(j) = ABICabInesistente;
058000060302         IF %ADDR(lclMsg) <> *NULL;
058100060302           lclMsg(j) = 'ABI e CAB inesistente.';
058200060301         ENDIF;
058300060302         esito = 1;
058400060301       ENDIF;
058500060301
058600060301       RETURN esito;
058700060313       CLOSE cnabi00f;
058800060301
058900060301      /END-FREE
059000060301     P chkAbiCab       E
059100060301
059200060301     P*--------------------------------------------------
059300060301     P* Procedure name: chkDataAssegno
059400060301     P* Purpose:        Controllo validità data emissione assegno
059500060301     P* Returns:        Esito
059600060301     P* Parameter:      dte => Data emissione assegno
059700060310     P* Parameter:      sas => Specie assegno.
059800060301     P* Parameter:      esito
059900060302     P* Parameter:      lclMsg => Messaggio di errore.
060000060301     P*--------------------------------------------------
060100060301     P chkDataAssegno  B                   EXPORT
060200060301     D chkDataAssegno  PI            10I 0
060300060301     D  dte                          10D   DATFMT(*ISO)
060400060310     D  sas
060500060310     D                                     LIKE(§1aSas)
060600060301     D  esito                        10I 0
060700060302     D  lclErr                        1A
060800060302     D                                     DIM(10)
060900060302     D  lclMsg                       80A   OPTIONS(*OMIT)
061000060302     D                                     DIM(10)
061100060301
061200060301      /FREE
061300060301
061400060301       CLEAR esito;
061500060302       CLEAR lclErr;
061600060302       IF %ADDR(lclMsg) <> *NULL;
061700060302         CLEAR lclMsg;
061800060301       ENDIF;
061900060302       CLEAR j;
062000060302
062100060302       IF dte = *LOVAL;
062200060302         j += 1;
062300060302         lclErr(j) = DataAssegnoObbligatoria;
062400060302         IF %ADDR(lclMsg) <> *NULL;
062500060302           lclMsg(j) = 'Immettere la data emissione assegno.';
062600060302         ENDIF;
062700060313         esito = ErroreParametri;
062800060313         RETURN esito;
062900060302       ENDIF;
063000060310
063100060310       IF sas = ' ';
063200060310         j += 1;
063300060310         lclErr(j) = SpecieAssegnoObbligatoria;
063400060310         IF %ADDR(lclMsg) <> *NULL;
063500060310           lclMsg(j) = 'Immettere la specie assegno.';
063600060310         ENDIF;
063700060313         esito = -2;
063800060313         RETURN esito;
063900060310       ENDIF;
064000060301
064100060310
064200060310       IF NOT %OPEN(tabel00f);
064300060310         OPEN tabel00f;
064400060310       ENDIF;
064500060310
064600060310       CHAIN (1:'4E':sas) tabel00f;
064700060310
064800060310       IF NOT %FOUND;
064900060310         j += 1;
065000060310         lclErr(j) = Tabella4ENonTrovata;
065100060310         IF %ADDR(lclMsg  ) <> *NULL;
065200060310           lclMsg(j) = 'La specie assegno ' + sas + ' non esiste.';
065300060310         ENDIF;
065400060313         esito = 1;
065500060313         RETURN esito;
065600060310       ELSE;
065700060310         ds4e = tblUni;
065800060310         IF tblFlg <> ' ';
065900060310           j += 1;
066000060310           lclErr(j) = Tabella4EAnnullata;
066100060310           IF %ADDR(lclMsg  ) <> *NULL;
066200060310             lclMsg(j) = 'La specie assegno ' + sas + ' è annullata.';
066300060310           ENDIF;
066400060313           esito = 1;
066500060313           RETURN esito;
066600060310         ENDIF;
066700060310       ENDIF;
066800060310
066900060411       IF §spGpd = 99 AND dte > %DATE();
067000060411         j += 1;
067100060411         lclErr(j) = AssegnoPostDatatoNonAmmesso;
067200060411         IF %ADDR(lclMsg) <> *NULL;
067300060411           lclMsg(j) = 'Assegno postdatato non ammesso.';
067400060411         ENDIF;
067500060411         esito = 1;
067600060411         RETURN esito;
067700060411       ELSE;
067800060411         IF %DIFF(dte:%DATE():*DAYS) > §spGpd;
067900060411           j += 1;
068000060411           lclErr(j) = AssegnoPostDatato;
068100060411           IF %ADDR(lclMsg) <> *NULL;
068200060411             lclMsg(j) = 'Assegno postdatato.';
068300060411           ENDIF;
068400060411           esito = 1;
068500060411           RETURN esito;
068600060411         ENDIF;
068700060301       ENDIF;
068800060310
068900060310       SELECT;
069000060313         WHEN §spGv2 < 99 AND %DIFF(%DATE():dte:*DAYS) > §spGv2;
069100060310           j += 1;
069200060310           lclErr(j) = AssegnoVecchio2;
069300060310           IF %ADDR(lclMsg) <> *NULL;
069400060310             lclMsg(j) = 'Assegno emesso da più di ' + %TRIML(%CHAR(§spGv2))
069500060310             + ' giorni.' ;
069600060310           ENDIF;
069700060310           esito = 1;
069800060313         WHEN §spGv1 < 99 AND %DIFF(%DATE():dte:*DAYS) > §spGv1;
069900060310           j += 1;
070000060310           lclErr(j) = AssegnoVecchio1;
070100060310           IF %ADDR(lclMsg) <> *NULL;
070200060310             lclMsg(j) = 'Assegno emesso da più di ' + %TRIML(%CHAR(§spGv1))
070300060310             + ' giorni.' ;
070400060310           ENDIF;
070500060310           esito = 1;
070600060310       ENDSL;
070700060301
070800060301       RETURN esito;
070900060313       CLOSE tabel00f;
071000060301
071100060301      /END-FREE
071200060301     P chkDataAssegno  E
071300060301
071400060301
071500060301     P*--------------------------------------------------
071600060301     P* Procedure name: chkTipoIncasso
071700060301     P* Purpose:        Controlla esistenza tipo incasso su tabella 1A
071800060301     P* Returns:        Esito
071900060301     P* Parameter:      tic => Tipo incasso
072000060301     P* Parameter:      esito
072100060301     P* Parameter:      datiTic
072200060301     P*--------------------------------------------------
072300060301     P chkTipoIncasso  B                   EXPORT
072400060301     D chkTipoIncasso  PI            10I 0
072500060301     D  tic                                LIKE(diz$tic)
072600060301     D  esito                        10I 0
072700060302     D  lclErr                        1A
072800060302     D                                     DIM(10)
072900060302     D  lclMsg                       80A   OPTIONS(*OMIT)
073000060302     D                                     DIM(10)
073100060301     D  datiTic                            LIKE(ds1a)
073200060301     D                                     OPTIONS(*OMIT)
073300060301
073400060301      /FREE
073500060301
073600060301       CLEAR esito;
073700060302       CLEAR lclErr;
073800060302       IF %ADDR(lclMsg) <> *NULL;
073900060302         CLEAR lclMsg;
074000060301       ENDIF;
074100060301       IF %ADDR(datiTic) <> *NULL;
074200060301         CLEAR datiTic;
074300060301       ENDIF;
074400060302       CLEAR j;
074500060301
074600060301       IF NOT %OPEN(tabel00f);
074700060301         OPEN tabel00f;
074800060301       ENDIF;
074900060301
075000060301       CHAIN (1:'1A':tic) tabel00f;
075100060301
075200060301       IF NOT %FOUND;
075300060302         j += 1;
075400060302         lclErr(j) = Tabella1ANonTrovata;
075500060302         IF %ADDR(lclMsg  ) <> *NULL;
075600060302           lclMsg(j) = 'Il tipo incasso ' + tic + ' non esiste.';
075700060301         ENDIF;
075800060313         esito = 1;
075900060313         RETURN esito;
076000060301       ENDIF;
076100060301
076200060301       IF %ADDR(datiTic) <> *NULL;
076300060301         datiTic = tblUni;
076400060301       ENDIF;
076500060301
076600060301       IF tblFlg <> ' ';
076700060302         j += 1;
076800060302         lclErr(j) = Tabella1AAnnullata;
076900060302         IF %ADDR(lclMsg) <> *NULL;
077000060302           lclMsg(j) = 'Il tipo incasso ' + tic + ' è annullato.';
077100060301         ENDIF;
077200060313         esito = 1;
077300060313         RETURN esito;
077400060301       ENDIF;
077500060301
077600060301       RETURN esito;
077700060313       CLOSE tabel00f;
077800060301
077900060301      /END-FREE
078000060301     P chkTipoIncasso  E
078100060301
078200060302
078300060302     P*--------------------------------------------------
078400060302     P* Procedure name: getlclErr
078500060302     P* Purpose:        Riceve gli lclErr dalle sottoprocedure
078600060302     P* Returns:
078700060302     P* Parameter:      lclErr
078800060302     P* Parameter:      lclMsg
078900060302     P* Parameter:      i
079000060302     P* Parameter:      lclErr
079100060302     P* Parameter:      lclMsg
079200060302     P* Parameter:      j
079300060302     P*--------------------------------------------------
079400060302     P getlclErr       B
079500060302     D getlclErr       PI            10I 0
079600060302     D  esito                        10I 0
079700060302
079800060302     D  y              S              5I 0
079900060302
080000060302      /FREE
080100060302       FOR y = 1 TO j;
080200060302         IF i = %ELEM(gblErr);
080300060302           LEAVE;
080400060302         ENDIF;
080500060302         i += 1;
080600060302         gblErr(i) = lclErr(y);
080700060302         gblMsg(i) = lclMsg(y);
080800060302       ENDFOR;
080900060302       RETURN esito;
081000060302      /END-FREE
081100060302
081200060302     P getlclErr       E
081300060313
081400060313     P*--------------------------------------------------
081500060313     P* Procedure name: chkIncassoPosUsabile
081600060313     P* Purpose:        Controlla usabilità incasso POS
081700060313     P* Returns:        Esito
081800060315     P* Parameter:      soc => Società
081900060313     P* Parameter:      fgs => PO gestione
082000060313     P* Parameter:      datiY4O => Dati Y4O
082100060313     P* Parameter:      esito
082200060313     P* Parameter:      errori => Codici di errore
082300060313     P* Parameter:      errMsg => Nessaggi di errore.
082400060313     P*--------------------------------------------------
082500060313     P chkIncassoPosUsabile...
082600060313     P                 B                   EXPORT
082700060313     D chkIncassoPosUsabile...
082800060313     D                 PI            10I 0
082900060315     D  soc                                LIKE(diz$soc)
083000060313     D  fgs                                LIKE(diz$fgs)
083100060313     D  datiY4O                            OPTIONS(*OMIT:*VARSIZE)
083200060313     D                                     LIKE(t02uni)
083300060313     D  prmKpjba                           OPTIONS(*OMIT)
083400060313     D                                     LIKEDS(kpjba)
083500060313     D  esito                        10I 0
083600060315     D  lclErr                        1A   DIM(10)
083700060315     D  lclMsg                       80A   DIM(10)
083800060313     D                                     OPTIONS(*OMIT)
083900060313     D dy4o          E DS
084000060313     D                                     INZ
084100060313      /FREE
084200060313       CLEAR esito;
084300060315       CLEAR lclErr;
084400060313       CLEAR j;
084500060313
084600060315       IF %ADDR(lclMsg) <> *NULL;
084700060315         CLEAR lclMsg;
084800060313       ENDIF;
084900060313
085000060315       IF soc = ' ' OR fgs = 0;
085100060313         esito = ErroreParametri;
085200060313         RETURN esito;
085300060313       ENDIF;
085400060313
085500060313       IF %ADDR(prmKpjba) <> *NULL;
085600060313         kpjba = prmKpjba;
085700060313       ENDIF;
085800060313
085900060313       IF %ADDR(datiY4O) <> *NULL AND datiY4O <> ' ';
086000060313         dy4o = datiY4O;
086100060313       ELSE;
086200060313         CLEAR kpjbu;
086300060313         RESET tibs02ds;
086400060313         t02cod = 'Y4O';
086500060315         t02ke1 = soc;
086600060313         t02ke2 = %EDITC(fgs:'X');
086700060313         tibs02r(kpjba:tibs02ds);
086800060313         SELECT;
086900060313           WHEN t02atb <> ' ';
087000060313             j += 1;
087100060315             lclErr(j) = TabellaY4OAnnullata;
087200060315             IF %ADDR(lclMsg) <> *NULL;
087300060315               lclMsg(j) = t02Msg;
087400060313             ENDIF;
087500060313             dy4o = t02uni;
087600060313             IF %ADDR(datiY4O) <> *NULL;
087700060313               datiY4O = dy4o;
087800060313             ENDIF;
087900060313             esito = 1;
088000060313             RETURN esito;
088100060313           WHEN t02Err <> ' ';
088200060313             j += 1;
088300060315             lclErr(j) = TabellaY4ONonTrovata;
088400060315             IF %ADDR(lclMsg) <> *NULL;
088500060315               lclMsg(j) = t02Msg;
088600060313             ENDIF;
088700060313             IF %ADDR(datiY4O) <> *NULL;
088800060313               CLEAR datiY4O;
088900060313             ENDIF;
089000060313             esito = 1;
089100060313             RETURN esito;
089200060313           OTHER;
089300060313             dy4o = t02uni;
089400060313             IF %ADDR(datiY4O) <> *NULL;
089500060313               datiY4O = dy4o;
089600060313             ENDIF;
089700060313         ENDSL;
089800060313       ENDIF;
089900060313
090000060313       IF §4oCc7 = ' ' OR §4oSc7 = ' ' OR §4oCo7 = ' ';
090100060313         j += 1;
090200060315         lclErr(j) = IncassoPosNonAmmesso;
090300060315         IF %ADDR(lclMsg) <> *NULL;
090400060315           lclMsg(j) = 'Incasso POS non ammesso per il PO ' + %TRIML(%CHAR(fgs))
090500060313           + '.';
090600060313         ENDIF;
090700060313         esito = 1;
090800060313         RETURN esito;
090900060313       ENDIF;
091000060313
091100060313       RETURN esito;
091200060313
091300060313      /END-FREE
091400060313     P chkIncassoPosUsabile...
091500060313     P                 E
091600060313
