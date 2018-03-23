000100060316     H NOMAIN BNDDIR('QC2LE':'PJXBND')
000200060316     H DATEDIT(*YMD) DECEDIT(*JOBRUN)
000300060316
000400060316      //*********************************************************************************************
000500060316      //
000600060316      // Definizione files.
000700060316      //
000800060316      //*********************************************************************************************
000900060316     Fcnaco00f  IF   E           K DISK
001000060316     F                                     USROPN
001100060316     Fcnind00f  IF   E           K DISK
001200060316     F                                     USROPN
001300060316     Fcnclp00f  IF   E           K DISK
001400060316     F                                     USROPN
001500060316     Ffncls01l  IF   E           K DISK
001600060316     F                                     USROPN
001700060316
001800060316      //*********************************************************************************************
001900060316      //
002000060316      // Definizione costanti.
002100060316      //
002200060316      //*********************************************************************************************
002300060316     D Bartolini...
002400060316     D                 C                   '201'
002500060316     D RicercaGestionale...
002600060316     D                 C                   '1'
002700060316     D RicercaProj...
002800060316     D                 C                   '2'
002900060316     D EsitoParametriNonValidi...
003000060316     D                 C                   -1
003100060316     D EsitoRicercaProjNonAttiva...
003200060316     D                 C                   -1
003300060316     D EsitoErroreGenerico...
003400060316     D                 C                   1
003500060316     D ErroreClienteNonTrovato...
003600060316     D                 C                   'A'
003700060316     D ErroreClienteAnnullato...
003800060316     D                 C                   'B'
003900060316     D ErroreClienteAltraArea...
004000060316     D                 C                   'C'
004100060316     D ErroreClienteObbligatorio...
004200060316     D                 C                   'D'
004300060316
004400060316      //********************************************************************************************
004500060316      //
004600060316      // Definizione delle procedure esterne usate.
004700060316      //
004800060316      //********************************************************************************************
004900060316     D tibs34r         PR
005000060316     D                                     EXTPGM('TIBS34R')
005100060316     D  tibs34ds
005200060316     D                                     LIKEDS(tibs34ds)
005300060316     D atoi            PR            10I 0
005400060316     D                                     EXTPROC('atoi')
005500060316     D  char                           *
005600060316     D                                     VALUE
005700060316     D                                     OPTIONS(*STRING)
005800060316     D xAlfa1          PR
005900060316     D                                     EXTPGM('XALFA1')
006000060316     D  dut                          30A
006100060316     D  kut                                LIKE(diz$kut)
006200060316     D  rag                                LIKE(dizAcoRag)
006300060316     D  kcc                                LIKE(diz$kcc)
006400060316     D  ksc                                LIKE(diz$ksc)
006500060316     D  sta                           1P 0
006600060316     D  flr                          90A
006700060316
006800060316      //*********************************************************************************************
006900060316      //
007000060316      // Definizione strutture dati.
007100060316      //
007200060316      //*********************************************************************************************
007300060316     D cndizion      E DS
007400060316     D                                     BASED(nullPtr)
007500060316     D                                     PREFIX(diz)
007600060316     D andiz00f      E DS
007700060316     D                                     BASED(nullPtr)
007800060316     D                                     PREFIX(pjz)
007900060316     D cnacods       E DS
008000060316     D                                     EXTNAME(cnaco00f)
008100060316     D cnindds       E DS
008200060316     D                                     EXTNAME(cnind00f)
008300060316     D cnclpds       E DS
008400060316     D                                     EXTNAME(cnclp00f)
008500060316     D fnclsds       E DS
008600060316     D                                     EXTNAME(fncls00f)
008700060316     D kpjba         E DS
008800060316     D                                     INZ
008900060316     D tibs34ds      E DS
009000060316     D                                     INZ
009100060316
009200060316      //*********************************************************************************************
009300060316      //
009400060316      // Definizione variabili globali.
009500060316      //
009600060316      //*********************************************************************************************
009700060316     D nullPtr         S               *
009800060316
009900060316      //********************************************************************************************
010000060316      //
010100060316      // Definizione aree dati.
010200060316      //
010300060316      //********************************************************************************************
010400060316     D §azute        E DS
010500060316     D                                     EXTNAME(azute00f)
010600060316     D                                     DTAARA
010700060316     D §DatiUte      E DS
010800060316     D                                     EXTNAME(dDatiUte)
010900060316     D                                     DTAARA
011000060316
011100060316      //--------------------------------------------------
011200060316      // Procedure name: chkCliente
011300060316      // Purpose:        Ricerca e controllo codice cliente
011400060316      // Returns:        Esito
011500060316      // Parameter:      prmSoc => Società
011600060316      // Parameter:      prmKcc => Capo conto cliente
011700060316      // Parameter:      prmCsc => Codice cliente
011800060316      // Parameter:      prmKsc => Codice cliente Proj
011900060316      // Parameter:      prmRicerca => Ricerca e scelta cliente
012000060316      // Parameter:      prmFlr => PO area
012100060316      // Parameter:      prmEsito => Esito
012200060316      // Parameter:      prmErr => Errori
012300060316      // Parameter:      prmMsg => Messaggi di errore
012400060316      // Parameter:      prmCnaco00f => CNACO00F
012500060316      // Parameter:      prmCnind00f => CNIND00F
012600060316      // Parameter:      prmCnclp00f => CNCLP00F
012700060316      // Parameter:      prmFncls00f => FNCLS00F
012800060316      //--------------------------------------------------
012900060316     D chkCliente      PR            10I 0
013000060316     D  prmSoc                             LIKE(diz$soc)
013100060316     D                                     OPTIONS(*OMIT)
013200060316     D  prmKcc                             LIKE(diz$Kcc)
013300060316     D                                     OPTIONS(*OMIT)
013400060316     D  prmCsc                             LIKE(diz$csc)
013500060316     D                                     OPTIONS(*OMIT)
013600060316     D  prmKsc                             LIKE(pjzKsc)
013700060316     D                                     OPTIONS(*OMIT)
013800060316     D  prmRicerca                    1A   OPTIONS(*OMIT)
013900060316     D  prmRag                             LIKE(dizAcoRag)
014000060316     D                                     OPTIONS(*OMIT)
014100060316     D  prmFlr                       90A
014200060316     D                                     OPTIONS(*OMIT)
014300060316     D  prmKpjba                           LIKE(kpjba)
014400060316     D                                     OPTIONS(*OMIT)
014500060316     D  prmEsito                     10I 0
014600060316     D  prmErr                        1A   DIM(10)
014700060316     D  prmMsg                       80A   DIM(10)
014800060316     D                                     OPTIONS(*OMIT)
014900060316     D  prmCnaco00f                        LIKE(cnacods)
015000060316     D                                     OPTIONS(*OMIT)
015100060316     D  prmCnind00f                        LIKE(cnindds)
015200060316     D                                     OPTIONS(*OMIT)
015300060316     D  prmCnclp00f                        LIKE(cnclpds)
015400060316     D                                     OPTIONS(*OMIT)
015500060316     D  prmFncls00f                        LIKE(fnclsds)
015600060316     D                                     OPTIONS(*OMIT)
015700060316
015800060316      //--------------------------------------------------
015900060316      // Procedure name: chkCliente
016000060316      // Purpose:        Ricerca e controllo codice cliente
016100060316      // Returns:        Esito
016200060316      // Parameter:      prmSoc => Società
016300060316      // Parameter:      prmKcc => Capo conto cliente
016400060316      // Parameter:      prmCsc => Codice cliente
016500060316      // Parameter:      prmKsc => Codice cliente Proj
016600060316      // Parameter:      prmRicerca => Ricerca e scelta cliente
016700060316      // Parameter:      prmFlr => PO area
016800060316      // Parameter:      prmEsito => Esito
016900060316      // Parameter:      prmErr => Errori
017000060316      // Parameter:      prmMsg => Messaggi di errore
017100060316      // Parameter:      prmCnaco00f => CNACO00F
017200060316      // Parameter:      prmCnind00f => CNIND00F
017300060316      // Parameter:      prmCnclp00f => CNCLP00F
017400060316      // Parameter:      prmFncls00f => FNCLS00F
017500060316      //--------------------------------------------------
017600060316     P chkCliente      B                   EXPORT
017700060316     D chkCliente      PI            10I 0
017800060316     D  prmSoc                             LIKE(diz$soc)
017900060316     D                                     OPTIONS(*OMIT)
018000060316     D  prmKcc                             LIKE(diz$Kcc)
018100060316     D                                     OPTIONS(*OMIT)
018200060316     D  prmCsc                             LIKE(diz$csc)
018300060316     D                                     OPTIONS(*OMIT)
018400060316     D  prmKsc                             LIKE(pjzKsc)
018500060316     D                                     OPTIONS(*OMIT)
018600060316     D  prmRicerca                    1A   OPTIONS(*OMIT)
018700060316     D  prmRag                             LIKE(dizAcoRag)
018800060316     D                                     OPTIONS(*OMIT)
018900060316     D  prmFlr                       90A
019000060316     D                                     OPTIONS(*OMIT)
019100060316     D  prmKpjba                           LIKE(kpjba)
019200060316     D                                     OPTIONS(*OMIT)
019300060316     D  prmEsito                     10I 0
019400060316     D  prmErr                        1A   DIM(10)
019500060316     D  prmMsg                       80A   DIM(10)
019600060316     D                                     OPTIONS(*OMIT)
019700060316     D  prmCnaco00f                        LIKE(cnacods)
019800060316     D                                     OPTIONS(*OMIT)
019900060316     D  prmCnind00f                        LIKE(cnindds)
020000060316     D                                     OPTIONS(*OMIT)
020100060316     D  prmCnclp00f                        LIKE(cnclpds)
020200060316     D                                     OPTIONS(*OMIT)
020300060316     D  prmFncls00f                        LIKE(fnclsds)
020400060316     D                                     OPTIONS(*OMIT)
020500060316
020600060316      //*********************************************************************************************
020700060316      //
020800060316      // Definizione variabili locali.
020900060316      //
021000060316      //*********************************************************************************************
021100060316     D soc             S                   LIKE(diz$soc)
021200060316     D kcc             S                   LIKE(diz$kcc)
021300060316     D csc             S                   LIKE(diz$csc)
021400060316     D ksc             S                   LIKE(pjzKsc)
021500060316     D ricerca         S              1A
021600060316     D flr             S             90A
021700060316     D flrPtr          S               *
021800060316     D                                     INZ(%ADDR(flr))
021900060316     D flrAry          S              3A   DIM(30)
022000060316     D                                     BASED(flrPtr)
022100060316     D flrArg          S              3A
022200060316     D err             S              1A   DIM(10)
022300060316     D msg             S             80A   DIM(10)
022400060316     D dut             S             30A
022500060316     D kut             S                   LIKE(diz$kut)
022600060316     D                                     INZ(1)
022700060316     D rag             S                   LIKE(acoRag)
022800060316     D sta             S              1P 0
022900060316     D                                     INZ(9)
023000060316     D i               S              5I 0
023100060316      /FREE
023200060316
023300060316       CLEAR cnacods;
023400060316       CLEAR cnindds;
023500060316       CLEAR cnclpds;
023600060316       CLEAR fnclsds;
023700060316
023800060316       IF %ADDR(prmSoc) <> *NULL;
023900060316         soc = prmSoc;
024000060316       ELSE;
024100060316         soc = Bartolini;
024200060316       ENDIF;
024300060316
024400060316       IF %ADDR(prmCsc) <> *NULL;
024500060316         csc = prmCsc;
024600060316       ENDIF;
024700060316
024800060316       IF %ADDR(prmKsc) <> *NULL;
024900060316         ksc = prmKsc;
025000060316       ENDIF;
025100060316
025200060316       IN(E) §azute;
025300060316       IF NOT %ERROR;
025400060316         IN(E) §DatiUte;
025500060316       ENDIF;
025600060316       IF %ERROR OR rsut = ' ';
025700060316         tibs34r(tibs34ds);
025800060316         IN §azute;
025900060316         IN §DatiUte;
026000060316       ENDIF;
026100060316
026200060316       IF %ADDR(prmKcc) <> *NULL;
026300060316         kcc = prmKcc;
026400060316       ELSE;
026500060316         kcc = dutKci;
026600060316       ENDIF;
026700060316
026800060316       IF %ADDR(prmFlr) <> *NULL;
026900060316         flr = prmFlr;
027000060316       ENDIF;
027100060316
027200060316       SELECT;
027300060316         WHEN prmRicerca = RicercaGestionale;
027400060316           dut = rsut;
027500060316           IF %ADDR(prmRag) <> *NULL;
027600060316             rag = prmRag;
027700060316           ENDIF;
027800060316           CLEAR csc;
027900060316           xAlfa1(dut:kut:rag:kcc:csc:sta:flr);
028000060316           IF sta = 0;
028100060316             IF %ADDR(prmRag) <> *NULL;
028200060316               prmRag = rag;
028300060316             ENDIF;
028400060316           ENDIF;
028500060316         WHEN prmRicerca = RicercaProj;
028600060316           prmEsito = EsitoRicercaProjNonAttiva;
028700060316           RETURN prmEsito;
028800060316       ENDSL;
028900060316
029000060316       SELECT;
029100060316         WHEN csc = 0 AND ksc = ' ';
029200060316           prmEsito = EsitoErroreGenerico;
029300060316           i += 1;
029400060316           err(i) = ErroreClienteObbligatorio;
029500060316           msg(i) = 'Immettere il codice cliente oppure ? per la ricerca.';
029600060316         WHEN csc <> 0 AND ksc = ' ';
029700060316           ksc = '0' + %EDITC(csc:'X');
029800060316         WHEN csc = 0 AND ksc <> ' ';
029900060316           csc = atoi(ksc);
030000060316           ksc = '0' + %EDITC(csc:'X');
030100060316       ENDSL;
030200060316
030300060316       IF NOT %OPEN(cnaco00f);
030400060316         OPEN cnaco00f;
030500060316       ENDIF;
030600060316
030700060316       CHAIN (kut:kcc:csc) cnaco00f;
030800060316
030900060316       IF NOT %FOUND(cnaco00f);
031000060316         prmEsito = EsitoErroreGenerico;
031100060316         i += 1;
031200060316         err(i) = ErroreClienteNonTrovato;
031300060316         msg(i) = 'Cliente ' + %EDITC(csc:'X') + ' non trovato.';
031400060316       ELSE;
031500060316         IF %ADDR(prmCnaco00f) <> *NULL;
031600060316           prmCnaco00f = cnacods;
031700060316         ENDIF;
031800060316         IF acoFlg <> ' ';
031900060316           i += 1;
032000060316           err(i) = ErroreClienteAnnullato;
032100060316           msg(i) = 'Cliente ' + %EDITC(csc:'X') + ' ' + %TRIMR(acoRag)
032200060316           + ' annullato.';
032300060316         ENDIF;
032400060316         IF %ADDR(prmFlr) <> *NULL;
032500060316           flrArg = %EDITC(csc:'X');
032600060316           IF %LOOKUP(flrArg:flrAry) = 0;
032700060316             i += 1;
032800060316             err(i) = ErroreClienteAltraArea;
032900060316             msg(i) = 'Il cliente ' + %EDITC(csc:'X') + ' appartiene ad +
033000060316             un''altra area.';
033100060316           ENDIF;
033200060316         ENDIF;
033300060316         IF %ADDR(prmCnind00f) <> *NULL;
033400060316           IF NOT %OPEN(cnind00f);
033500060316             OPEN cnind00f;
033600060316           ENDIF;
033700060316           CHAIN (kut:kcc:csc) cnind00f;
033800060316           prmCnind00f = cnindds;
033900060316         ENDIF;
034000060316         IF %ADDR(prmCnclp00f) <> *NULL;
034100060316           IF NOT %OPEN(cnclp00f);
034200060316             OPEN cnclp00f;
034300060316           ENDIF;
034400060316           CHAIN (kut:kcc:csc) cnclp00f;
034500060316           prmCnclp00f = cnclpds;
034600060316         ENDIF;
034700060316         IF %ADDR(prmFncls00f) <> *NULL;
034800060316           IF NOT %OPEN(fncls01l);
034900060316             OPEN fncls01l;
035000060316           ENDIF;
035100060316           CHAIN csc fncls01l;
035200060316           prmFncls00f = fnclsds;
035300060316         ENDIF;
035400060316       ENDIF;
035500060316
035600060316       IF prmEsito = 0 AND err(1) <> ' ';
035700060316         prmEsito = EsitoErroreGenerico;
035800060316       ENDIF;
035900060316
036000060316       prmErr = err;
036100060316       IF %ADDR(prmSoc) <> *NULL;
036200060316         prmSoc = soc;
036300060316       ENDIF;
036400060316       IF %ADDR(prmKcc) <> *NULL;
036500060316         prmKcc = kcc;
036600060316       ENDIF;
036700060316       IF %ADDR(prmCsc) <> *NULL;
036800060316         prmCsc = csc;
036900060316       ENDIF;
037000060316       IF %ADDR(prmKsc) <> *NULL;
037100060316         prmKsc = ksc;
037200060316       ENDIF;
037300060316       IF %ADDR(prmMsg) <> *NULL;
037400060316         prmMsg = msg;
037500060316       ENDIF;
037600060316
037700060316       RETURN prmEsito;
037800060316
037900060316       CLOSE cnaco00f;
038000060316       CLOSE cnind00f;
038100060316       CLOSE cnclp00f;
038200060316       CLOSE fncls01l;
038300060316      /END-FREE
038400060316     P chkCliente      E
