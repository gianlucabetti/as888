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
003800060529     D Codificato      C                   'F'
003900060529     D Generico        C                   'G'
004000060315     D POS             C                   'P'
004100060302     D InTabellaTM     C                   'S'
004200060302     D Mittente        C                   'M'
004300060529     D SoloContante...
004400060529     D                 C                   'SC'
004500060308     D NoAssegno...
004600060308     D                 C                   ' '
004700060302     D NonObbligatorio...
004800060302     D                 C                   'N'
004900060313     D Controllo...
005000060313     D                 C                   'C'
005100060313     D ErroreParametri...
005200060302     D                 C                   -1
005300060302     D Tabella1ANonTrovata...
005400060302     D                 C                   'A'
005500060302     D Tabella1AAnnullata...
005600060302     D                 C                   'B'
005700060302     D NumeroAssegnoObbligatorio...
005800060302     D                 C                   'C'
005900060302     D NumeroAssegnoNonValido...
006000060302     D                 C                   'D'
006100060302     D AbiObbligatorio...
006200060302     D                 C                   'E'
006300060302     D AbiCabInesistente...
006400060302     D                 C                   'F'
006500060302     D CabInesistente...
006600060302     D                 C                   'G'
006700060302     D DataAssegnoObbligatoria...
006800060302     D                 C                   'H'
006900060302     D AssegnoPostDatato...
007000060302     D                 C                   'I'
007100060302     D TipoIncassoNonAmmesso...
007200060302     D                 C                   'J'
007300060302     D CabObbligatorio...
007400060302     D                 C                   'K'
007500060308     D NonImmettereNumeroAssegno...
007600060308     D                 C                   'L'
007700060308     D NonImmettereAbiCab...
007800060308     D                 C                   'M'
007900060308     D NonImmettereDataAssegno...
008000060308     D                 C                   'N'
008100060310     D SpecieAssegnoObbligatoria...
008200060310     D                 C                   'O'
008300060310     D Tabella4ENonTrovata...
008400060310     D                 C                   'P'
008500060310     D Tabella4EAnnullata...
008600060310     D                 C                   'Q'
008700060310     D AssegnoVecchio1...
008800060310     D                 C                   'R'
008900060310     D AssegnoVecchio2...
009000060310     D                 C                   'S'
009100060315     D TabellaY4ONonTrovata...
009200060315     D                 C                   'T'
009300060315     D TabellaY4OAnnullata...
009400060315     D                 C                   'U'
009500060315     D IncassoPosNonAmmesso...
009600060315     D                 C                   'V'
009700060411     D AssegnoPostDatatoNonAmmesso...
009800060411     D                 C                   'W'
009900060302
010000060302     ***********************************************************************************************
010100060302     **
010200060302     ** Definizione strutture dati.
010300060302     **
010400060302     ***********************************************************************************************
010500060302     D cndizion      E DS
010600060302     D                                     BASED(nullPtr)
010700060302     D                                     PREFIX(diz)
010800060302     D cnabids       E DS                  EXTNAME(cnabi00f)
010900060302     D                                     INZ
011000060302     D ds1a          E DS
011100060302     D                                     INZ
011200060310     D ds4e          E DS
011300060310     D                                     INZ
011400060313     D tibs02ds      E DS
011500060313     D                                     INZ
011600060313     D  t02mod       E
011700060313     D                                     INZ(Controllo)
011800060313     D kpjba         E DS
011900060313     D                                     INZ
012000060302
012100060302     ***********************************************************************************************
012200060302     **
012300060302     ** Definizione variabili.
012400060302     **
012500060302     ***********************************************************************************************
012600060302     D nullPtr         S               *
012700060302     D gblErr          S              1A
012800060302     D                                     DIM(10)
012900060302     D gblMsg          S             80A
013000060302     D                                     DIM(10)
013100060302     D lclErr          S              1A
013200060302     D                                     DIM(10)
013300060302     D lclMsg          S             80A
013400060302     D                                     DIM(10)
013500060302     D i               S              5I 0
013600060302     D j               S              5I 0
013700060313
013800060313     ***********************************************************************************************
013900060313     **
014000060313     ** Definizione procedure esterne.
014100060313     **
014200060313     ***********************************************************************************************
014300060313     D tibs02r         PR                                                       Ricerca TNTBE00F
014400060313     D                                     EXTPGM('TIBS02R')
014500060313     D  kpjba
014600060313     D                                     LIKEDS(kpjba)
014700060313     D  tibs02ds
014800060313     D                                     LIKEDS(tibs02ds)
014900060313
015000060301     D*--------------------------------------------------
015100060301     D* Procedure name: chkIncassoPO
015200060301     D* Purpose:        Controlla i dati dell'incasso di un PO
015300060301     D* Returns:        Esito
015400060301     D* Parameter:      tic => Tipo incasso
015500060301     D* Parameter:      tpd => Tipo documento incassato
015600060301     D* Parameter:      datiTic => ds1a
015700060301     D* Parameter:      nra => Numero assegno
015800060301     D* Parameter:      abi => Codice ABI assegno
015900060301     D* Parameter:      cab => Codice CAB assegno
016000060301     D* Parameter:      dte => Data emissione assegno
016100060301     D* Parameter:      esito
016200060302     D* Parameter:      lclMsg => Messaggio di errore
016300060301     D*--------------------------------------------------
016400060301     D chkIncassoPO    PR            10I 0
016500060315     D  soc                                LIKE(diz$Soc)
016600060315     D  fgs                                LIKE(diz$Fgs)
016700060301     D  tic                                LIKE(diz$Tic)
016800060301     D  tpd                           1A
016900060301     D  datiTic                            LIKE(tblUni)
017000060301     D                                     OPTIONS(*OMIT)
017100060315     D  datiY4O                            LIKE(t02Uni)
017200060315     D                                     OPTIONS(*OMIT)
017300060301     D  nra                                LIKE(diz$nra)
017400060301     D                                     OPTIONS(*OMIT)
017500060301     D  abi                                LIKE(diz$abi)
017600060301     D                                     OPTIONS(*OMIT)
017700060301     D  cab                                LIKE(diz$cab)
017800060301     D                                     OPTIONS(*OMIT)
017900060301     D  dte                          10D   DATFMT(*ISO)
018000060301     D                                     OPTIONS(*OMIT)
018100060301     D  esito                        10I 0
018200060302     D  prmErr                        1A
018300060302     D                                     DIM(10)
018400060302     D  prmMsg                       80A   OPTIONS(*OMIT)
018500060302     D                                     DIM(10)
018600060315     D  prmKpjba                           OPTIONS(*OMIT:*NOPASS)
018700060315     D                                     LIKEDS(kpjba)
018800060301
018900060301     D*--------------------------------------------------
019000060301     D* Procedure name: chkTipoIncasso
019100060301     D* Purpose:        Controlla esistenza tipo incasso su tabella 1A
019200060301     D* Returns:        Esito
019300060301     D* Parameter:      tic => Tipo incasso
019400060301     D* Parameter:      esito
019500060301     D* Parameter:      datiTic
019600060301     D*--------------------------------------------------
019700060301     D chkTipoIncasso  PR            10I 0
019800060301     D  tic                                LIKE(diz$tic)
019900060301     D  esito                        10I 0
020000060302     D  lclErr                        1A
020100060302     D                                     DIM(10)
020200060302     D  lclMsg                       80A   OPTIONS(*OMIT)
020300060302     D                                     DIM(10)
020400060301     D  datiTic                            LIKE(ds1a)
020500060301     D                                     OPTIONS(*OMIT)
020600060301
020700060301     D*--------------------------------------------------
020800060301     D* Procedure name: chkNumeroAssegno
020900060301     D* Purpose:        Controllo numero assegno
021000060301     D* Returns:        Esito
021100060301     D* Parameter:      nra => Numero assegno
021200060301     D* Parameter:      esito => Esito
021300060301     D*--------------------------------------------------
021400060301     D chkNumeroAssegno...
021500060301     D                 PR            10I 0
021600060301     D  nra                                LIKE(diz$Nra)
021700060301     D  esito                        10I 0
021800060302     D  lclErr                        1A
021900060302     D                                     DIM(10)
022000060302     D  lclMsg                       80A   OPTIONS(*OMIT)
022100060302     D                                     DIM(10)
022200060301
022300060301     D*--------------------------------------------------
022400060301     D* Procedure name: chkAbiCab
022500060301     D* Purpose:        Controllo ABI CAB
022600060301     D* Returns:        Esito
022700060301     D* Parameter:      abi
022800060301     D* Parameter:      cab
022900060301     D* Parameter:      esito
023000060301     D*--------------------------------------------------
023100060301     D chkAbiCab       PR            10I 0
023200060301     D  abi                                LIKE(diz$Abi)
023300060301     D  cab                                LIKE(diz$Cab)
023400060301     D  esito                        10I 0
023500060302     D  lclErr                        1A
023600060302     D                                     DIM(10)
023700060302     D  lclMsg                       80A   OPTIONS(*OMIT)
023800060302     D                                     DIM(10)
023900060301     D  prmCnabi                           OPTIONS(*OMIT)
024000060301     D                                     LIKEDS(cnabids)
024100060301
024200060301     D*--------------------------------------------------
024300060301     D* Procedure name: chkDataAssegno
024400060301     D* Purpose:        Controllo validità data emissione assegno
024500060301     D* Returns:        Esito
024600060301     D* Parameter:      dte => Data emissione assegno
024700060301     D* Parameter:      esito
024800060302     D* Parameter:      lclMsg    => Messaggio di errore.
024900060301     D*--------------------------------------------------
025000060301     D chkDataAssegno  PR            10I 0
025100060301     D  dte                          10D   DATFMT(*ISO)
025200060310     D  sas
025300060310     D                                     LIKE(§1aSas)
025400060301     D  esito                        10I 0
025500060302     D  lclErr                        1A
025600060302     D                                     DIM(10)
025700060302     D  lclMsg                       80A   OPTIONS(*OMIT)
025800060302     D                                     DIM(10)
025900060313
026000060313     D*--------------------------------------------------
026100060313     D* Procedure name: chkIncassoPosUsabile
026200060313     D* Purpose:        Controlla usabilità incasso POS
026300060313     D* Returns:        Esito
026400060315     D* Parameter:      soc => Società
026500060313     D* Parameter:      fgs => PO gestione
026600060313     D* Parameter:      datiY4O => Dati Y4O
026700060313     D* Parameter:      esito
026800060313     D* Parameter:      errori => Codici di errore
026900060313     D* Parameter:      errMsg => Nessaggi di errore.
027000060313     D*--------------------------------------------------
027100060313     D chkIncassoPosUsabile...
027200060313     D                 PR            10I 0
027300060315     D  soc                                LIKE(diz$soc)
027400060313     D  fgs                                LIKE(diz$fgs)
027500060313     D  datiY4O                            OPTIONS(*OMIT:*VARSIZE)
027600060313     D                                     LIKE(t02uni)
027700060313     D  prmKpjba                           OPTIONS(*OMIT)
027800060313     D                                     LIKEDS(kpjba)
027900060313     D  esito                        10I 0
028000060315     D  lclErr                        1A   DIM(10)
028100060315     D  lclMsg                       80A   DIM(10)
028200060313     D                                     OPTIONS(*OMIT)
028300060302
028400060302     D*--------------------------------------------------
028500060302     D* Procedure name: getlclErr
028600060302     D* Purpose:        Riceve gli lclErr dalle sottoprocedure
028700060302     D* Returns:        Esito
028800060302     D* Parameter:      Esito
028900060302     D*--------------------------------------------------
029000060302     D getLclErr       PR            10I 0
029100060302     D  esito                        10I 0
029200060301
029300060301     P*--------------------------------------------------
029400060301     P* Procedure name: chkIncassoPO
029500060301     P* Purpose:        Controlla i dati dell'incasso di un PO
029600060301     P* Returns:        Esito
029700060301     P* Parameter:      tic => Tipo incasso
029800060301     P* Parameter:      tpd => Tipo documento incassato
029900060301     P* Parameter:      datiTic = ds1a
030000060301     P* Parameter:      nra => Numero assegno
030100060301     P* Parameter:      abi => Codice ABI assegno
030200060301     P* Parameter:      cab => Codice CAB assegno
030300060301     P* Parameter:      dte => Data emissione assegno
030400060301     P* Parameter:      esito
030500060302     P* Parameter:      lclMsg    => Messaggio di errore
030600060301     P*--------------------------------------------------
030700060301     P chkIncassoPO    B                   EXPORT
030800060301     D chkIncassoPO    PI            10I 0
030900060315     D  soc                                LIKE(diz$Soc)
031000060315     D  fgs                                LIKE(diz$Fgs)
031100060301     D  tic                                LIKE(diz$Tic)
031200060301     D  tpd                           1A
031300060301     D  datiTic                            LIKE(tblUni)
031400060301     D                                     OPTIONS(*OMIT)
031500060315     D  datiY4O                            LIKE(t02Uni)
031600060315     D                                     OPTIONS(*OMIT)
031700060301     D  nra                                LIKE(diz$nra)
031800060301     D                                     OPTIONS(*OMIT)
031900060301     D  abi                                LIKE(diz$abi)
032000060301     D                                     OPTIONS(*OMIT)
032100060301     D  cab                                LIKE(diz$cab)
032200060301     D                                     OPTIONS(*OMIT)
032300060301     D  dte                          10D   DATFMT(*ISO)
032400060301     D                                     OPTIONS(*OMIT)
032500060301     D  esito                        10I 0
032600060302     D  prmErr                        1A
032700060302     D                                     DIM(10)
032800060302     D  prmMsg                       80A   OPTIONS(*OMIT)
032900060302     D                                     DIM(10)
033000060315     D  prmKpjba                           OPTIONS(*OMIT:*NOPASS)
033100060315     D                                     LIKEDS(kpjba)
033200060301
033300060301     D ds1a          E DS
033400060301     D                                     INZ
033500060301
033600060301      /FREE
033700060301
033800060301       CLEAR esito;
033900060302       CLEAR prmErr;
034000060302       IF %ADDR(prmMsg) <> *NULL;
034100060302         CLEAR prmMsg;
034200060301       ENDIF;
034300060302       CLEAR gblErr;
034400060302       CLEAR gblMsg;
034500060302       CLEAR i;
034600060301
034700060301       IF %ADDR(datiTic) <> *NULL AND datiTic <> ' ';
034800060301         ds1a = datiTic;
034900060301       ELSE;
035000060302         IF chkTipoIncasso(tic:esito:lclErr:lclMsg:ds1a) <> 0;
035100060302           getLclErr(esito);
035200060301         ENDIF;
035300060301         datiTic = ds1a;
035400060301       ENDIF;
035500060315
035600060315       IF %ADDR(prmKpjba) <> *NULL;
035700060315         kpjba = prmKpjba;
035800060315       ENDIF;
035900060301
036000060529       IF tpd = PrePagato OR  tpd = Codificato OR tpd = Generico;
036100060529         SELECT;
036200060529           WHEN tic = *BLANK;
036300060529             i += 1;
036400060529             gblErr(i) = TipoIncassoNonAmmesso;
036500060529             gblMsg(i) = 'Tipo incasso non ammesso.';
036600060529             IF %ADDR(datiTic) <> *NULL;
036700060529               CLEAR ds1a;
036800060529               datiTic = ds1a;
036900060529             ENDIF;
037000060529           WHEN tic <> SoloContante
037100060529           AND (§1aFmb = Mittente OR §1aTtm = InTabellaTM);
037200060529             i += 1;
037300060529             gblErr(i) = TipoIncassoNonAmmesso;
037400060529             gblMsg(i) = 'Tipo incasso non ammesso.';
037500060529         ENDSL;
037600060301       ENDIF;
037700060301
037800060308       IF §1aSas = NoAssegno;
037900060308         IF %ADDR(nra) <> *NULL AND nra <> ' ';
038000060308           i += 1;
038100060308           gblErr(i) = NonImmettereNumeroAssegno;
038200060308           gblMsg(i) = 'Non immettere il numero assegno.';
038300060308         ENDIF;
038400060308         IF (%ADDR(abi) <> *NULL AND abi <> 0)
038500060308         OR (%ADDR(cab) <> *NULL AND cab <> 0);
038600060308           i += 1;
038700060308           gblErr(i) = NonImmettereAbiCab;
038800060308           gblMsg(i) = 'Non immettere ABI e CAB assegno.';
038900060308         ENDIF;
039000060308         IF %ADDR(dte) <> *NULL AND dte > *LOVAL;
039100060308           i += 1;
039200060308           gblErr(i) = NonImmettereDataAssegno;
039300060308           gblMsg(i) = 'Non immettere la data emissione assegno.';
039400060308         ENDIF;
039500060308       ELSE;
039600060301         IF §1aNas <> NonObbligatorio;
039700060301           IF %ADDR(nra) = *NULL;
039800060302             i += 1;
039900060302             gblErr(i) = NumeroAssegnoObbligatorio;
040000060302             gblMsg(i) = 'Numero assegno obbligatorio.';
040100060301           ENDIF;
040200060302           IF chkNumeroAssegno(nra:esito:lclErr:lclMsg) <> 0;
040300060302             getLclErr(esito);
040400060302           ENDIF;
040500060301         ENDIF;
040600060308         IF §1aAbi <> NonObbligatorio;
040700060301           IF %ADDR(abi) = *NULL;
040800060302             i += 1;
040900060302             gblErr(i) = AbiObbligatorio;
041000060302             gblMsg(i) = 'ABI obbligatorio.';
041100060301           ENDIF;
041200060301           IF %ADDR(cab) = *NULL;
041300060302             i += 1;
041400060302             gblErr(i) = CabObbligatorio;
041500060302             gblMsg(i) = 'CAB obbligatorio.';
041600060301           ENDIF;
041700060308           IF %ADDR(abi) <> *NULL AND %ADDR(cab) <> *NULL
041800060308           AND chkAbiCab(abi:cab:esito:lclErr:lclMsg:cnabids) <> 0;
041900060302             getLclErr(esito);
042000060302           ENDIF;
042100060301         ENDIF;
042200060301         IF %ADDR(dte) = *NULL;
042300060302           i += 1;
042400060302           gblErr(i) = DataAssegnoObbligatoria;
042500060302           gblMsg(i) = 'Data emissione assegno obbligatoria.';
042600060302         ELSE;
042700060310           IF chkDataAssegno(dte:§1aSas:esito:lclErr:lclMsg) <> 0;
042800060302             getLclErr(esito);
042900060302           ENDIF;
043000060301         ENDIF;
043100060301       ENDIF;
043200060301
043300060315       IF §1aFca = POS AND chkIncassoPosUsabile(soc:fgs:*OMIT:kpjba
043400060315       :esito:lclErr:lclMsg) <> 0;
043500060315         getLclErr(esito);
043600060315       ENDIF;
043700060315
043800060315       prmErr = gblErr;
043900060302       IF %ADDR(prmMsg) <> *NULL;
044000060302         prmMsg = gblMsg;
044100060302       ENDIF;
044200060302
044300060308       IF prmErr(1) <> ' ' AND esito = 0;
044400060308         esito = 1;
044500060308       ENDIF;
044600060308
044700060308       RETURN esito;
044800060301
044900060301      /END-FREE
045000060301     P chkIncassoPO    E
045100060301
045200060301     P*--------------------------------------------------
045300060301     P* Procedure name: chkNumeroAssegno
045400060301     P* Purpose:        Controllo numero assegno
045500060301     P* Returns:        Esito
045600060301     P* Parameter:      nra => Numero assegno
045700060301     P* Parameter:      esito => Esito
045800060301     P*--------------------------------------------------
045900060301     P chkNumeroAssegno...
046000060301     P                 B                   EXPORT
046100060301     D chkNumeroAssegno...
046200060301     D                 PI            10I 0
046300060301     D  nra                                LIKE(diz$Nra)
046400060301     D  esito                        10I 0
046500060302     D  lclErr                        1A
046600060302     D                                     DIM(10)
046700060302     D  lclMsg                       80A   OPTIONS(*OMIT)
046800060302     D                                     DIM(10)
046900060301
047000060313     D nNra4           S              4P 0
047100060313     D nNra10          S             10P 0
047200060301
047300060301      /FREE
047400060301
047500060301       CLEAR esito;
047600060302       CLEAR lclErr;
047700060302       IF %ADDR(lclMsg) <> *NULL;
047800060302         CLEAR lclMsg;
047900060301       ENDIF;
048000060302       CLEAR j;
048100060301
048200060301       IF nra = ' ';
048300060302         j += 1;
048400060302         lclErr(j) = NumeroAssegnoObbligatorio;
048500060302         IF %ADDR(lclMsg) <> *NULL;
048600060302           lclMsg(j) = 'Immettere il numero assegno (ultime 4 cifre).';
048700060301         ENDIF;
048800060313         esito = 1;
048900060313         RETURN esito;
049000060301       ENDIF;
049100060301
049200060302       MONITOR;
049300060313         nNra10 = %DEC(%TRIMR(nra):10:0);
049400060302         ON-ERROR 105;
049500060302           j += 1;
049600060302           lclErr(j) = NumeroAssegnoNonValido;
049700060302           IF %ADDR(lclMsg) <> *NULL;
049800060302             lclMsg(j) = 'Numero assegno non valido.';
049900060302           ENDIF;
050000060313         esito = 1;
050100060313         RETURN esito;
050200060302       ENDMON;
050300060302
050400060313       IF %LEN(%TRIMR(nra)) > 4;
050500060313         nra = %EDITC(nNra10:'X');
050600060313       ELSE;
050700060313         nNra4 = %DEC(%TRIMR(nra):4:0);
050800060313         nra = %EDITC(nNra4:'X');
050900060313       ENDIF;
051000060313
051100060313       RETURN esito;
051200060301
051300060301      /END-FREE
051400060301     P chkNumeroAssegno...
051500060301     P                 E
051600060301
051700060301     P*--------------------------------------------------
051800060301     P* Procedure name: chkAbiCab
051900060301     P* Purpose:        Controllo ABI CAB
052000060301     P* Returns:        Esito
052100060301     P* Parameter:      abi
052200060301     P* Parameter:      cab
052300060301     P* Parameter:      esito
052400060301     P*--------------------------------------------------
052500060301     P chkAbiCab       B                   EXPORT
052600060301     D chkAbiCab       PI            10I 0
052700060301     D  abi                                LIKE(diz$Abi)
052800060301     D  cab                                LIKE(diz$Cab)
052900060301     D  esito                        10I 0
053000060302     D  lclErr                        1A
053100060302     D                                     DIM(10)
053200060302     D  lclMsg                       80A   OPTIONS(*OMIT)
053300060302     D                                     DIM(10)
053400060301     D  prmCnabi                           OPTIONS(*OMIT)
053500060301     D                                     LIKEDS(cnabids)
053600060301
053700060301      /FREE
053800060301
053900060301       CLEAR esito;
054000060302       CLEAR lclErr;
054100060302       IF %ADDR(lclMsg) <> *NULL;
054200060302         CLEAR lclMsg;
054300060301       ENDIF;
054400060301       IF %ADDR(prmCnabi) <> *NULL;
054500060301         CLEAR prmCnabi;
054600060301         CLEAR cnabids;
054700060301       ENDIF;
054800060302       CLEAR j;
054900060301
055000060301       IF abi = 0;
055100060302         j += 1;
055200060302         lclErr(j) = AbiObbligatorio;
055300060302         IF %ADDR(lclMsg) <> *NULL;
055400060302           lclMsg(j) = 'ABI obbligatorio.';
055500060301         ENDIF;
055600060302         esito = 1;
055700060301       ENDIF;
055800060308
055900060301       IF cab = 0;
056000060302         j += 1;
056100060302         lclErr(j) = CabObbligatorio;
056200060302         IF %ADDR(lclMsg) <> *NULL;
056300060302           lclMsg(j) = 'CAB obbligatorio.';
056400060301         ENDIF;
056500060302         esito = 1;
056600060301       ENDIF;
056700060301
056800060308       IF esito <> 0;
056900060308         RETURN esito;
057000060308       ENDIF;
057100060308
057200060301       IF NOT %OPEN(cnabi00f);
057300060301         OPEN cnabi00f;
057400060301       ENDIF;
057500060301
057600060301       CHAIN (abi:cab) cnabi00f;
057700060301
057800060301       IF %FOUND;
057900060301         IF %ADDR(prmCnabi) <> *NULL;
058000060301           prmCnabi = cnabids;
058100060301         ENDIF;
058200060313         RETURN esito;
058300060301       ENDIF;
058400060301
058500060301       CHAIN (abi) cnabi00f;
058600060301       IF %FOUND;
058700060302         j += 1;
058800060302         lclErr(j) = CabInesistente;
058900060302         IF %ADDR(lclMsg) <> *NULL;
059000060302           lclMsg(j) = 'CAB inesistente.';
059100060301         ENDIF;
059200060302         esito = 1;
059300060301       ELSE;
059400060302         j += 1;
059500060302         lclErr(j) = ABICabInesistente;
059600060302         IF %ADDR(lclMsg) <> *NULL;
059700060302           lclMsg(j) = 'ABI e CAB inesistente.';
059800060301         ENDIF;
059900060302         esito = 1;
060000060301       ENDIF;
060100060301
060200060301       RETURN esito;
060300060313       CLOSE cnabi00f;
060400060301
060500060301      /END-FREE
060600060301     P chkAbiCab       E
060700060301
060800060301     P*--------------------------------------------------
060900060301     P* Procedure name: chkDataAssegno
061000060301     P* Purpose:        Controllo validità data emissione assegno
061100060301     P* Returns:        Esito
061200060301     P* Parameter:      dte => Data emissione assegno
061300060310     P* Parameter:      sas => Specie assegno.
061400060301     P* Parameter:      esito
061500060302     P* Parameter:      lclMsg => Messaggio di errore.
061600060301     P*--------------------------------------------------
061700060301     P chkDataAssegno  B                   EXPORT
061800060301     D chkDataAssegno  PI            10I 0
061900060301     D  dte                          10D   DATFMT(*ISO)
062000060310     D  sas
062100060310     D                                     LIKE(§1aSas)
062200060301     D  esito                        10I 0
062300060302     D  lclErr                        1A
062400060302     D                                     DIM(10)
062500060302     D  lclMsg                       80A   OPTIONS(*OMIT)
062600060302     D                                     DIM(10)
062700060301
062800060301      /FREE
062900060301
063000060301       CLEAR esito;
063100060302       CLEAR lclErr;
063200060302       IF %ADDR(lclMsg) <> *NULL;
063300060302         CLEAR lclMsg;
063400060301       ENDIF;
063500060302       CLEAR j;
063600060302
063700060302       IF dte = *LOVAL;
063800060302         j += 1;
063900060302         lclErr(j) = DataAssegnoObbligatoria;
064000060302         IF %ADDR(lclMsg) <> *NULL;
064100060302           lclMsg(j) = 'Immettere la data emissione assegno.';
064200060302         ENDIF;
064300060313         esito = ErroreParametri;
064400060313         RETURN esito;
064500060302       ENDIF;
064600060310
064700060310       IF sas = ' ';
064800060310         j += 1;
064900060310         lclErr(j) = SpecieAssegnoObbligatoria;
065000060310         IF %ADDR(lclMsg) <> *NULL;
065100060310           lclMsg(j) = 'Immettere la specie assegno.';
065200060310         ENDIF;
065300060313         esito = -2;
065400060313         RETURN esito;
065500060310       ENDIF;
065600060301
065700060310
065800060310       IF NOT %OPEN(tabel00f);
065900060310         OPEN tabel00f;
066000060310       ENDIF;
066100060310
066200060310       CHAIN (1:'4E':sas) tabel00f;
066300060310
066400060310       IF NOT %FOUND;
066500060310         j += 1;
066600060310         lclErr(j) = Tabella4ENonTrovata;
066700060310         IF %ADDR(lclMsg  ) <> *NULL;
066800060310           lclMsg(j) = 'La specie assegno ' + sas + ' non esiste.';
066900060310         ENDIF;
067000060313         esito = 1;
067100060313         RETURN esito;
067200060310       ELSE;
067300060310         ds4e = tblUni;
067400060310         IF tblFlg <> ' ';
067500060310           j += 1;
067600060310           lclErr(j) = Tabella4EAnnullata;
067700060310           IF %ADDR(lclMsg  ) <> *NULL;
067800060310             lclMsg(j) = 'La specie assegno ' + sas + ' è annullata.';
067900060310           ENDIF;
068000060313           esito = 1;
068100060313           RETURN esito;
068200060310         ENDIF;
068300060310       ENDIF;
068400060310
068500060411       IF §spGpd = 99 AND dte > %DATE();
068600060411         j += 1;
068700060411         lclErr(j) = AssegnoPostDatatoNonAmmesso;
068800060411         IF %ADDR(lclMsg) <> *NULL;
068900060411           lclMsg(j) = 'Assegno postdatato non ammesso.';
069000060411         ENDIF;
069100060411         esito = 1;
069200060411         RETURN esito;
069300060411       ELSE;
069400060411         IF %DIFF(dte:%DATE():*DAYS) > §spGpd;
069500060411           j += 1;
069600060411           lclErr(j) = AssegnoPostDatato;
069700060411           IF %ADDR(lclMsg) <> *NULL;
069800060411             lclMsg(j) = 'Assegno postdatato.';
069900060411           ENDIF;
070000060411           esito = 1;
070100060411           RETURN esito;
070200060411         ENDIF;
070300060301       ENDIF;
070400060310
070500060310       SELECT;
070600060313         WHEN §spGv2 < 99 AND %DIFF(%DATE():dte:*DAYS) > §spGv2;
070700060310           j += 1;
070800060310           lclErr(j) = AssegnoVecchio2;
070900060310           IF %ADDR(lclMsg) <> *NULL;
071000060310             lclMsg(j) = 'Assegno emesso da più di ' + %TRIML(%CHAR(§spGv2))
071100060310             + ' giorni.' ;
071200060310           ENDIF;
071300060310           esito = 1;
071400060313         WHEN §spGv1 < 99 AND %DIFF(%DATE():dte:*DAYS) > §spGv1;
071500060310           j += 1;
071600060310           lclErr(j) = AssegnoVecchio1;
071700060310           IF %ADDR(lclMsg) <> *NULL;
071800060310             lclMsg(j) = 'Assegno emesso da più di ' + %TRIML(%CHAR(§spGv1))
071900060310             + ' giorni.' ;
072000060310           ENDIF;
072100060310           esito = 1;
072200060310       ENDSL;
072300060301
072400060301       RETURN esito;
072500060313       CLOSE tabel00f;
072600060301
072700060301      /END-FREE
072800060301     P chkDataAssegno  E
072900060301
073000060301
073100060301     P*--------------------------------------------------
073200060301     P* Procedure name: chkTipoIncasso
073300060301     P* Purpose:        Controlla esistenza tipo incasso su tabella 1A
073400060301     P* Returns:        Esito
073500060301     P* Parameter:      tic => Tipo incasso
073600060301     P* Parameter:      esito
073700060301     P* Parameter:      datiTic
073800060301     P*--------------------------------------------------
073900060301     P chkTipoIncasso  B                   EXPORT
074000060301     D chkTipoIncasso  PI            10I 0
074100060301     D  tic                                LIKE(diz$tic)
074200060301     D  esito                        10I 0
074300060302     D  lclErr                        1A
074400060302     D                                     DIM(10)
074500060302     D  lclMsg                       80A   OPTIONS(*OMIT)
074600060302     D                                     DIM(10)
074700060301     D  datiTic                            LIKE(ds1a)
074800060301     D                                     OPTIONS(*OMIT)
074900060301
075000060301      /FREE
075100060301
075200060301       CLEAR esito;
075300060302       CLEAR lclErr;
075400060302       IF %ADDR(lclMsg) <> *NULL;
075500060302         CLEAR lclMsg;
075600060301       ENDIF;
075700060301       IF %ADDR(datiTic) <> *NULL;
075800060301         CLEAR datiTic;
075900060301       ENDIF;
076000060302       CLEAR j;
076100060301
076200060301       IF NOT %OPEN(tabel00f);
076300060301         OPEN tabel00f;
076400060301       ENDIF;
076500060301
076600060301       CHAIN (1:'1A':tic) tabel00f;
076700060301
076800060301       IF NOT %FOUND;
076900060302         j += 1;
077000060302         lclErr(j) = Tabella1ANonTrovata;
077100060302         IF %ADDR(lclMsg  ) <> *NULL;
077200060302           lclMsg(j) = 'Il tipo incasso ' + tic + ' non esiste.';
077300060301         ENDIF;
077400060313         esito = 1;
077500060313         RETURN esito;
077600060301       ENDIF;
077700060301
077800060301       IF %ADDR(datiTic) <> *NULL;
077900060301         datiTic = tblUni;
078000060301       ENDIF;
078100060301
078200060301       IF tblFlg <> ' ';
078300060302         j += 1;
078400060302         lclErr(j) = Tabella1AAnnullata;
078500060302         IF %ADDR(lclMsg) <> *NULL;
078600060302           lclMsg(j) = 'Il tipo incasso ' + tic + ' è annullato.';
078700060301         ENDIF;
078800060313         esito = 1;
078900060313         RETURN esito;
079000060301       ENDIF;
079100060301
079200060301       RETURN esito;
079300060313       CLOSE tabel00f;
079400060301
079500060301      /END-FREE
079600060301     P chkTipoIncasso  E
079700060301
079800060302
079900060302     P*--------------------------------------------------
080000060302     P* Procedure name: getlclErr
080100060302     P* Purpose:        Riceve gli lclErr dalle sottoprocedure
080200060302     P* Returns:
080300060302     P* Parameter:      lclErr
080400060302     P* Parameter:      lclMsg
080500060302     P* Parameter:      i
080600060302     P* Parameter:      lclErr
080700060302     P* Parameter:      lclMsg
080800060302     P* Parameter:      j
080900060302     P*--------------------------------------------------
081000060302     P getlclErr       B
081100060302     D getlclErr       PI            10I 0
081200060302     D  esito                        10I 0
081300060302
081400060302     D  y              S              5I 0
081500060302
081600060302      /FREE
081700060302       FOR y = 1 TO j;
081800060302         IF i = %ELEM(gblErr);
081900060302           LEAVE;
082000060302         ENDIF;
082100060302         i += 1;
082200060302         gblErr(i) = lclErr(y);
082300060302         gblMsg(i) = lclMsg(y);
082400060302       ENDFOR;
082500060302       RETURN esito;
082600060302      /END-FREE
082700060302
082800060302     P getlclErr       E
082900060313
083000060313     P*--------------------------------------------------
083100060313     P* Procedure name: chkIncassoPosUsabile
083200060313     P* Purpose:        Controlla usabilità incasso POS
083300060313     P* Returns:        Esito
083400060315     P* Parameter:      soc => Società
083500060313     P* Parameter:      fgs => PO gestione
083600060313     P* Parameter:      datiY4O => Dati Y4O
083700060313     P* Parameter:      esito
083800060313     P* Parameter:      errori => Codici di errore
083900060313     P* Parameter:      errMsg => Nessaggi di errore.
084000060313     P*--------------------------------------------------
084100060313     P chkIncassoPosUsabile...
084200060313     P                 B                   EXPORT
084300060313     D chkIncassoPosUsabile...
084400060313     D                 PI            10I 0
084500060315     D  soc                                LIKE(diz$soc)
084600060313     D  fgs                                LIKE(diz$fgs)
084700060313     D  datiY4O                            OPTIONS(*OMIT:*VARSIZE)
084800060313     D                                     LIKE(t02uni)
084900060313     D  prmKpjba                           OPTIONS(*OMIT)
085000060313     D                                     LIKEDS(kpjba)
085100060313     D  esito                        10I 0
085200060315     D  lclErr                        1A   DIM(10)
085300060315     D  lclMsg                       80A   DIM(10)
085400060313     D                                     OPTIONS(*OMIT)
085500060313     D dy4o          E DS
085600060313     D                                     INZ
085700060313      /FREE
085800060313       CLEAR esito;
085900060315       CLEAR lclErr;
086000060313       CLEAR j;
086100060313
086200060315       IF %ADDR(lclMsg) <> *NULL;
086300060315         CLEAR lclMsg;
086400060313       ENDIF;
086500060313
086600060315       IF soc = ' ' OR fgs = 0;
086700060313         esito = ErroreParametri;
086800060313         RETURN esito;
086900060313       ENDIF;
087000060313
087100060313       IF %ADDR(prmKpjba) <> *NULL;
087200060313         kpjba = prmKpjba;
087300060313       ENDIF;
087400060313
087500060313       IF %ADDR(datiY4O) <> *NULL AND datiY4O <> ' ';
087600060313         dy4o = datiY4O;
087700060313       ELSE;
087800060313         CLEAR kpjbu;
087900060313         RESET tibs02ds;
088000060313         t02cod = 'Y4O';
088100060315         t02ke1 = soc;
088200060313         t02ke2 = %EDITC(fgs:'X');
088300060313         tibs02r(kpjba:tibs02ds);
088400060313         SELECT;
088500060313           WHEN t02atb <> ' ';
088600060313             j += 1;
088700060315             lclErr(j) = TabellaY4OAnnullata;
088800060315             IF %ADDR(lclMsg) <> *NULL;
088900060315               lclMsg(j) = t02Msg;
089000060313             ENDIF;
089100060313             dy4o = t02uni;
089200060313             IF %ADDR(datiY4O) <> *NULL;
089300060313               datiY4O = dy4o;
089400060313             ENDIF;
089500060313             esito = 1;
089600060313             RETURN esito;
089700060313           WHEN t02Err <> ' ';
089800060313             j += 1;
089900060315             lclErr(j) = TabellaY4ONonTrovata;
090000060315             IF %ADDR(lclMsg) <> *NULL;
090100060315               lclMsg(j) = t02Msg;
090200060313             ENDIF;
090300060313             IF %ADDR(datiY4O) <> *NULL;
090400060313               CLEAR datiY4O;
090500060313             ENDIF;
090600060313             esito = 1;
090700060313             RETURN esito;
090800060313           OTHER;
090900060313             dy4o = t02uni;
091000060313             IF %ADDR(datiY4O) <> *NULL;
091100060313               datiY4O = dy4o;
091200060313             ENDIF;
091300060313         ENDSL;
091400060313       ENDIF;
091500060313
091600060313       IF §4oCc7 = ' ' OR §4oSc7 = ' ' OR §4oCo7 = ' ';
091700060313         j += 1;
091800060315         lclErr(j) = IncassoPosNonAmmesso;
091900060315         IF %ADDR(lclMsg) <> *NULL;
092000060315           lclMsg(j) = 'Incasso POS non ammesso per il PO ' + %TRIML(%CHAR(fgs))
092100060313           + '.';
092200060313         ENDIF;
092300060313         esito = 1;
092400060313         RETURN esito;
092500060313       ENDIF;
092600060313
092700060313       RETURN esito;
092800060313
092900060313      /END-FREE
093000060313     P chkIncassoPosUsabile...
093100060313     P                 E
093200060313
