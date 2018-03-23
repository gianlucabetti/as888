000100060301     ***********************************************************************************************
000200060228     **
000300060302     ** Questo modulo controlla la bolla di cui si sta registrando gli incassi.
000400060228     **
000500060228     ***********************************************************************************************
000600060303     H NOMAIN BNDDIR('QC2LE')
000700060228     H DATEDIT(*YMD) DECEDIT(*JOBRUN)
000800060228
000900060228     ***********************************************************************************************
001000060228     **
001100060228     ** Definizione files.
001200060228     **
001300060228     ***********************************************************************************************
001400060310     Ffnblp01l  IF   E           K DISK
001500060310     F                                     USROPN
001600060310     Ffnarb01l  IF   E           K DISK
001700060310     F                                     USROPN
001800060302     Ffiar601l  IF   E           K DISK
001900060228     F                                     USROPN
002000060302     Ffiar901l  IF   E           K DISK
002100060302     F                                     USROPN
002200060301     Ftabel00f  IF   E           K DISK
002300060301     F                                     USROPN
002400060301
002500060301     ***********************************************************************************************
002600060301     **
002700060301     ** Definizione delle procedure esterne usate.
002800060301     **
002900060301     ***********************************************************************************************
003000060301     D atoi            PR            10I 0
003100060301     D                                     EXTPROC('atoi')
003200060301     D  char                           *
003300060301     D                                     VALUE
003400060301     D                                     OPTIONS(*STRING)
003500060302
003600060302     ***********************************************************************************************
003700060302     **
003800060302     ** Definizione costanti.
003900060302     **
004000060302     ***********************************************************************************************
004100060302     D Assegnato       C                   'A'
004200060322     D AssegnatoCAssegno...
004300060322     D                 C                   'T'
004400060302     D Contrassegno    C                   'C'
004500060302     D PrePagato       C                   'P'
004600060302     D Franco          C                   'F'
004700060310     D IncassoPartenza...
004800060310     D                 C                   'P'
004900060310     D IncassoArrivo...
005000060310     D                 C                   'A'
005100060302     D erroreParametri...
005200060302     D                 C                   -1
005300060303     D ContrassegnoNonTrovato...
005400060302     D                 C                   'A'
005500060303     D FatturaNonTrovata...
005600060302     D                 C                   'B'
005700060302     D FatturaAnnullata...
005800060302     D                 C                   'C'
005900060302     D FatturaNonEmessa...
006000060302     D                 C                   'D'
006100060310     D SpedizioneNonFranco...
006200060310     D                 C                   'E'
006300060310     D PortoNonAssegnato...
006400060310     D                 C                   'F'
006500060310     D PortoNonFranco...
006600060310     D                 C                   'F'
006700060302
006800060302     ***********************************************************************************************
006900060302     **
007000060302     ** Definizione strutture dati.
007100060302     **
007200060302     ***********************************************************************************************
007300060302     D cndizion      E DS
007400060302     D                                     BASED(nullPtr)
007500060302     D                                     PREFIX(diz)
007600060302     D fiar600f      E DS
007700060302     D                                     INZ
007800060302     D fiar900f      E DS
007900060302     D                                     INZ
008000060302     D ds3a          E DS
008100060302     D                                     INZ
008200060302
008300060302     ***********************************************************************************************
008400060302     **
008500060302     ** Definizione variabili.
008600060302     **
008700060302     ***********************************************************************************************
008800060302     D nullPtr         S               *
008900060302     D gblErr          S              1A
009000060302     D                                     DIM(10)
009100060302     D gblMsg          S             80A
009200060302     D                                     DIM(10)
009300060302     D lclErr          S              1A
009400060302     D                                     DIM(10)
009500060302     D lclMsg          S             80A
009600060302     D                                     DIM(10)
009700060302     D i               S              5I 0
009800060302     D j               S              5I 0
009900060310     D cbo             S                   LIKE(diz$Cbo)
010000060302
010100060302     D*--------------------------------------------------
010200060302     D* Procedure name: chkBollaIncassata
010300060302     D* Purpose:        Controlla la bolla di cui sivogliono registrare gli...
010400060302     D*                           incassi
010500060302     D* Returns:        esito
010600060302     D* Parameter:      aas => Anno spedizione
010700060302     D* Parameter:      lnp => PO spedizione
010800060302     D* Parameter:      nrs => Serie spedizione
010900060302     D* Parameter:      nsp => Numero spedizione
011000060310     D* Parameter:      prmCbo => Codice bolla
011100060310     D* Parameter:      datiCbo => Dati tabella 3A
011200060302     D* Parameter:      esito
011300060302     D* Parameter:      err => Errori
011400060302     D* Parameter:      prmMsg => Messaggi di errore
011500060302     D*--------------------------------------------------
011600060302     D chkBollaIncassata...
011700060302     D                 PR            10I 0
011800060302     D  aas                                LIKE(diz$aas)
011900060302     D  lnp                                LIKE(diz$lnp)
012000060302     D  nrs                                LIKE(diz$nrs)
012100060302     D  nsp                                LIKE(diz$nsp)
012200060310     D  prmCbo                             LIKE(diz$Cbo)
012300060310     D                                     OPTIONS(*OMIT)
012400060310     D  datiCbo                            LIKE(tblUni)
012500060302     D                                     OPTIONS(*OMIT)
012600060302     D  tpd                           1A
012700060302     D                                     CONST
012800060302     D  esito                        10I 0
012900060302     D  err                           1A   DIM(10)
013000060302     D  msg                          80A   DIM(10)
013100060302     D                                     OPTIONS(*OMIT)
013200060303     D  datiFattura               32767A
013300060303     D                                     OPTIONS(*OMIT:*VARSIZE)
013400060303     D                                     VARYING
013500060303     D  datiContrassegno...
013600060303     D                            32767A
013700060303     D                                     OPTIONS(*OMIT:*VARSIZE)
013800060303     D                                     VARYING
013900060302
014000060302     P*--------------------------------------------------
014100060302     P* Procedure name: chkBollaIncassata
014200060302     P* Purpose:        Controlla la bolla di cui sivogliono registrare gli...
014300060302     P*                           incassi
014400060302     P* Returns:        esito
014500060302     P* Parameter:      aas => Anno spedizione
014600060302     P* Parameter:      lnp => PO spedizione
014700060302     P* Parameter:      nrs => Serie spedizione
014800060302     P* Parameter:      nsp => Numero spedizione
014900060310     P* Parameter:      prmCbo => Codice bolla
015000060310     P* Parameter:      datiCbo => Dati tabella 3A
015100060302     P* Parameter:      esito
015200060302     P* Parameter:      err => Errori
015300060302     P* Parameter:      prmMsg => Messaggi di errore
015400060302     P*--------------------------------------------------
015500060302     P chkBollaIncassata...
015600060303     P                 B                   EXPORT
015700060302     D chkBollaIncassata...
015800060303     D                 PI            10I 0
015900060302     D  aas                                LIKE(diz$aas)
016000060302     D  lnp                                LIKE(diz$lnp)
016100060302     D  nrs                                LIKE(diz$nrs)
016200060302     D  nsp                                LIKE(diz$nsp)
016300060310     D  prmCbo                             LIKE(diz$Cbo)
016400060310     D                                     OPTIONS(*OMIT)
016500060310     D  datiCbo                            LIKE(tblUni)
016600060302     D                                     OPTIONS(*OMIT)
016700060302     D  tpd                           1A
016800060302     D                                     CONST
016900060302     D  esito                        10I 0
017000060302     D  err                           1A   DIM(10)
017100060302     D  msg                          80A   DIM(10)
017200060302     D                                     OPTIONS(*OMIT)
017300060303     D  datiFattura               32767A
017400060303     D                                     OPTIONS(*OMIT:*VARSIZE)
017500060303     D                                     VARYING
017600060303     D  datiContrassegno...
017700060303     D                            32767A
017800060303     D                                     OPTIONS(*OMIT:*VARSIZE)
017900060303     D                                     VARYING
018000060302
018100060302      /FREE
018200060302       CLEAR esito;
018300060302       CLEAR i;
018400060302       CLEAR err;
018500060302       IF %ADDR(msg) <> *NULL;
018600060302         CLEAR msg;
018700060302       ENDIF;
018800060303       IF %ADDR(datiFattura) <> *NULL;
018900060303         CLEAR datiFattura;
019000060303       ENDIF;
019100060303       IF %ADDR(datiContrassegno) <> *NULL;
019200060303         CLEAR datiContrassegno;
019300060303       ENDIF;
019400060302
019500060302       // ID spedizione obbligatorio.
019600060302       IF aas = 0 OR lnp = 0 OR nsp = 0;
019700060321         esito = -1;
019800060321         RETURN esito;
019900060302       ENDIF;
020000060302
020100060310       // Reperisco il codice bolla.
020200060310       IF %ADDR(prmCbo) <> *NULL AND prmCbo <> ' ';
020300060310         cbo = prmCbo;
020400060310       ELSE;
020500060310         CLEAR cbo;
020600060310         IF tpd = PrePagato;
020700060310           IF NOT %OPEN(fnblp01l);
020800060310             OPEN fnblp01l;
020900060310           ENDIF;
021000060310           CHAIN (aas:lnp:nrs:nsp) fnblp01l;
021100060310           IF %FOUND;
021200060310             cbo = blpCbo;
021300060310           ENDIF;
021400060310         ELSE;
021500060310           IF NOT %OPEN(fnarb01l);
021600060310             OPEN fnarb01l;
021700060310           ENDIF;
021800060310           CHAIN (aas:lnp:nrs:nsp) fnarb01l;
021900060310           IF %FOUND;
022000060310             cbo = arbCbo;
022100060310           ENDIF;
022200060310         ENDIF;
022300060302       ENDIF;
022400060302
022500060310       // Controllo codice bolla ricevuto.
022600060310       IF %ADDR(prmCbo) <> *NULL AND %ADDR(datiCbo) <> *NULL
022700060310       AND datiCbo <> ' ';
022800060310         ds3a = datiCbo;
022900060310       ELSE;
023000060310         IF NOT %OPEN(tabel00f);
023100060310           OPEN tabel00f;
023200060310         ENDIF;
023300060310         CHAIN (1:'3A':cbo) tabel00f;
023400060310         IF NOT %FOUND;
023500060321           esito = -3;
023600060321           RETURN esito;
023700060310         ENDIF;
023800060310         ds3a = tblUni;
023900060310         IF %ADDR(datiCbo) <> *NULL;
024000060310           datiCbo = tblUni;
024100060310         ENDIF;
024200060310         IF tblFlg <> ' ';
024300060321           esito = -4;
024400060321           RETURN esito;
024500060310         ENDIF;
024600060310       ENDIF;
024700060302
024800060310       // Controllo congruenza tra il tipo documento incassato e la bolla.
024900060310       IF tpd = PrePagato;
025000060310         IF %SUBST(§3aTb1:1:1) <> Franco AND %SUBST(§3aTb2:1:1) <> Franco;
025100060310           i += 1;
025200060310           err(i) = PortoNonFranco;
025300060310           IF %ADDR(msg) <> *NULL;
025400060310             msg(i) = 'Il porto delLa spedizione ' + %CHAR(aas) + ' '
025500060310             + %CHAR(lnp) + ' ' + %CHAR(nrs) + ' ' + %CHAR(nsp)
025600060310             + ' non è franco.';
025700060310           ENDIF;
025800060310         ENDIF;
025900060310         IF §3aTi1 <> IncassoPartenza AND §3aTi2 <> IncassoPartenza;
026000060310           i += 1;
026100060310           err(i) = FatturaNonEmessa;
026200060310           IF %ADDR(msg) <> *NULL;
026300060310             msg(i) = 'La fattura per la bolla ' + %CHAR(aas) + ' '
026400060310             + %CHAR(lnp) + ' ' + %CHAR(nrs) + ' ' + %CHAR(nsp)
026500060310             + ' non è stata emessa.';
026600060310           ENDIF;
026700060310         ENDIF;
026800060310       ENDIF;
026900060310
027000060322       IF tpd = Assegnato OR tpd = AssegnatoCAssegno;
027100060310         IF %SUBST(§3aTb1:1:1) <> Assegnato AND %SUBST(§3aTb2:1:1) <> Assegnato;
027200060310           i += 1;
027300060310           err(i) = PortoNonAssegnato;
027400060310           IF %ADDR(msg) <> *NULL;
027500060310             msg(i) = 'Il porto delLa spedizione ' + %CHAR(aas) + ' '
027600060310             + %CHAR(lnp) + ' ' + %CHAR(nrs) + ' ' + %CHAR(nsp)
027700060310             + ' non è assegnato.';
027800060310           ENDIF;
027900060310         ENDIF;
028000060310         IF §3aTi1 <> IncassoArrivo AND §3aTi2 <> IncassoArrivo;
028100060310           i += 1;
028200060310           err(i) = FatturaNonEmessa;
028300060310           IF %ADDR(msg) <> *NULL;
028400060310             msg(i) = 'La fattura per la bolla ' + %CHAR(aas) + ' '
028500060310             + %CHAR(lnp) + ' ' + %CHAR(nrs) + ' ' + %CHAR(nsp)
028600060310             + ' non è stata emessa.';
028700060310           ENDIF;
028800060310         ENDIF;
028900060310       ENDIF;
029000060310
029100060322       IF tpd = Contrassegno OR tpd = AssegnatoCAssegno;
029200060310         IF §3aFca = 0;
029300060310           i += 1;
029400060310           err(i) = ContrassegnoNonTrovato;
029500060310           IF %ADDR(msg) <> *NULL;
029600060310             msg(i) = 'Il contrassegno ' + %CHAR(aas) + ' ' + %CHAR(lnp) + ' '
029700060310             + %CHAR(nrs) + ' ' + %CHAR(nsp) + ' non esiste.';
029800060310           ENDIF;
029900060310         ENDIF;
030000060310       ENDIF;
030100060302
030200060322       // Determino il tipo record di FIAR601L e reperisco i dati della fattura.
030300060321       IF tpd <> Contrassegno;
030400060321         SELECT;
030500060321           WHEN §3aTi2 = IncassoPartenza OR §3aTi2 = IncassoArrivo;
030600060321             ar6trc = '2';
030700060321           WHEN §3aTi1 = IncassoPartenza OR §3aTi1 = IncassoArrivo;
030800060321             ar6trc = '1';
030900060321           OTHER;
031000060321             CLEAR ar6trc;
031100060321         ENDSL;
031200060321         IF ar6trc <> ' ';
031300060321           IF NOT %OPEN(fiar601l);
031400060321             OPEN fiar601l;
031500060321           ENDIF;
031600060321           CHAIN (aas:lnp:nrs:nsp:ar6trc) fiar601l;
031700060321           IF NOT %FOUND;
031800060321             i += 1;
031900060321             err(i) = FatturaNonTrovata;
032000060321               IF %ADDR(msg) <> *NULL;
032100060321                 msg(i) = 'La fattura per la bolla ' + %CHAR(aas) + ' '
032200060321                 + %CHAR(lnp) + ' ' + %CHAR(nrs) + ' ' + %CHAR(nsp) +
032300060321                 ' non esiste.';
032400060321               ENDIF;
032500060321           ELSE;
032600060321             IF %ADDR(datiFattura) <> *NULL;
032700060321               datiFattura = fiar600f;
032800060321             ENDIF;
032900060321             IF ar6atb <> ' ';
033000060321               i += 1;
033100060321               err(i) = FatturaAnnullata;
033200060321               IF %ADDR(msg) <> *NULL;
033300060321                 msg(i) = 'La fattura per la bolla ' + %CHAR(aas) + ' '
033400060321                 + %CHAR(lnp) + ' ' + %CHAR(nrs) + ' ' + %CHAR(nsp) +
033500060321                 ' è annullata.';
033600060321               ENDIF;
033700060321             ENDIF;
033800060321             IF ar6nft = 0;
033900060321               i += 1;
034000060321               err(i) = FatturaNonEmessa;
034100060321               IF %ADDR(msg) <> *NULL;
034200060321                 msg(i) = 'La fattura per la bolla ' + %CHAR(aas) + ' '
034300060321                 + %CHAR(lnp) + ' ' + %CHAR(nrs) + ' ' + %CHAR(nsp)
034400060321                 + ' non è stata emessa.';
034500060321               ENDIF;
034600060321             ENDIF;
034700060321           ENDIF;
034800060321         ENDIF;
034900060321       ENDIF;
035000060310
035100060310       // Incasso di un contrassegno.
035200060310       IF §3aFca <> 0;
035300060310         IF NOT %OPEN(fiar901l);
035400060310           OPEN fiar901l;
035500060310         ENDIF;
035600060310         CHAIN (aas:lnp:nrs:nsp) fiar901l;
035700060310         IF %FOUND;
035800060310           IF %ADDR(datiContrassegno) <> *NULL;
035900060310             datiContrassegno = fiar900f;
036000060310           ENDIF;
036100060310         ELSE;
036200060310           i += 1;
036300060310           err(i) = ContrassegnoNonTrovato;
036400060310           IF %ADDR(msg) <> *NULL;
036500060310             msg(i) = 'Il contrassegno ' + %CHAR(aas) + ' ' + %CHAR(lnp) + ' '
036600060310             + %CHAR(nrs) + ' ' + %CHAR(nsp) + ' non esiste.';
036700060310           ENDIF;
036800060310         ENDIF;
036900060310       ENDIF;
037000060302
037100060308       IF err(1) <> ' ' AND esito = 0;
037200060308         esito = 1;
037300060308       ENDIF;
037400060308
037500060302       RETURN esito;
037600060303
037700060303       CLOSE(E) tabel00f;
037800060303       CLOSE(E) fiar601l;
037900060303       CLOSE(E) fiar901l;
038000060310       CLOSE(E) fnblp01l;
038100060310       CLOSE(E) fnarb01l;
038200060302
038300060302      /END-FREE
038400060302     P chkBollaIncassata...
038500060302     P                 E
038600060302
