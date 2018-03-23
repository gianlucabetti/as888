000100060317     H NOMAIN
000200060316     H DATEDIT(*YMD) DECEDIT(*JOBRUN)
000300060316
000400060316      //*********************************************************************************************
000500060316      //
000600060316      // Definizione files.
000700060316      //
000800060316      //*********************************************************************************************
000900060317     Fcnass00f  IF A E             DISK
001000060316     F                                     USROPN
001100060324     F                                     COMMIT(gblCmtCtl)
001200060317     F                                     INFDS(cnassfbk)
001300060317     Fynass10f  IF A E           K DISK
001400060316     F                                     USROPN
001500060324     F                                     COMMIT(gblCmtCtl)
001600060317     F                                     INFDS(ynassfbk)
001700060317
001800060316      //*********************************************************************************************
001900060316      //
002000060316      // Definizione costanti.
002100060316      //
002200060316      //*********************************************************************************************
002300060316     D EsitoParametriNonValidi...
002400060316     D                 C                   -1
002500060316
002600060316      //*********************************************************************************************
002700060316      //
002800060316      // Definizione strutture dati.
002900060316      //
003000060316      //*********************************************************************************************
003100060317     D cnassfbk        DS
003200060317     D                                     QUALIFIED
003300060317     D  file_name             83     92                                         * File name
003400060317     D  library               93    102                                         * Library name
003500060317     D  db_rrn               397    400I 0                                      * Relative-rcd-num
003600060317     D ynassfbk        DS
003700060317     D                                     QUALIFIED
003800060317     D  file_name             83     92                                         * File name
003900060317     D  library               93    102                                         * Library name
004000060317     D  db_rrn               397    400I 0                                      * Relative-rcd-num
004100060317
004200060316      //*********************************************************************************************
004300060316      //
004400060316      // Definizione variabili globali.
004500060316      //
004600060316      //*********************************************************************************************
004700060316     D nullPtr         S               *
004800060317
004900060317     D*--------------------------------------------------
005000060317     D* Procedure name: wrtAssegno
005100060317     D* Purpose:        Scrittura assegno.
005200060317     D* Returns:
005300060317     D* Parameter:      prmCmtCtl => File sotto COMMIT.
005400060317     D* Parameter:      prmSoc => Società
005500060317     D* Parameter:      prmFle => PO che ha incassato
005600060317     D* Parameter:      prmCuc => Codice utente
005700060317     D* Parameter:      prmAas => Anno spedizione
005800060317     D* Parameter:      prmLnp => PO spedizione
005900060317     D* Parameter:      prmNrs => Serie spedizione
006000060317     D* Parameter:      prmNsp => Numero spedizione
006100060317     D* Parameter:      prmCas => Importo assegno
006200060317     D* Parameter:      prmVca => Divisa assegno
006300060317     D* Parameter:      prmCmb => Cambio
006400060317     D* Parameter:      prmNra => Numero assegno
006500060317     D* Parameter:      prmDte => Data emissione assegno
006600060317     D* Parameter:      prmAbi => ABI assegno
006700060317     D* Parameter:      prmCab => CAB assegno
006800060317     D* Parameter:      prmTpa => Tipo assegno
006900060317     D* Parameter:      prmTpi => Tipo intestazione assegno
007000060317     D* Parameter:      prmTpp => Tipo piazza
007100060317     D* Parameter:      prmPia => Piazza assegno
007200060317     D* Parameter:      prmNot => Note
007300060317     D* Parameter:      prmFve => Assegno versabile
007400060317     D* Parameter:      prmKcc => Capo conto banca versamento
007500060317     D* Parameter:      prmKsc => Sotto conto banca versamento
007600060317     D* Parameter:      prmNdv => Numero distinta versamento
007700060317     D* Parameter:      prmDdv => Data distinta versamento
007800060317     D* Parameter:      prmFtr => Flag trasmissione
007900060317     D* Parameter:      prmEsito => Esito
008000060317     D* Parameter:      prmRrn => Numero relativo record scritto
008100060317     D* Parameter:      prmFile => File scritto
008200060317     D* Parameter:      prmLib => Libreria residenza file scritto
008300060317     D*--------------------------------------------------
008400060317     D wrtAssegno      PR
008500060317     D  prmCmtCtl                     1A
008600060317     D  prmSede                       1A
008700060317     D  prmPostDatato                 1A
008800060324     D  prmSessione                  15P 0
008900060317     D  prmSoc                             LIKE(assSoc)
009000060317     D  prmFle                             LIKE(assFle)
009100060317     D  prmTpd                             LIKE(assTpd)
009200060317     D                                     OPTIONS(*OMIT)
009300060317     D  prmCuc                             LIKE(assCuc)
009400060317     D  prmLnp                             LIKE(assLnp)
009500060317     D                                     OPTIONS(*OMIT)
009600060317     D  prmAas                             LIKE(assAas)
009700060317     D                                     OPTIONS(*OMIT)
009800060317     D  prmNrs                             LIKE(assNrs)
009900060317     D                                     OPTIONS(*OMIT)
010000060317     D  prmNsp                             LIKE(assNsp)
010100060317     D                                     OPTIONS(*OMIT)
010200060317     D  prmCas                             LIKE(assCas)
010300060317     D  prmVca                             LIKE(assVca)
010400060317     D  prmCmb                             LIKE(assCmb)
010500060317     D  prmNra                             LIKE(assNra)
010600060317     D  prmDte                             LIKE(assDte)
010700060317     D  prmAbi                             LIKE(assAbi)
010800060317     D  prmCab                             LIKE(assCab)
010900060317     D  prmTpa                             LIKE(assTpa)
011000060317     D  prmTpi                             LIKE(assTpi)
011100060317     D                                     OPTIONS(*OMIT)
011200060317     D  prmTpp                             LIKE(assTpp) OPTIONS(*OMIT)
011300060317     D  prmPia                             LIKE(assPia) OPTIONS(*OMIT)
011400060317     D  prmNot                             LIKE(assNot) OPTIONS(*OMIT)
011500060317     D  prmFve                             CONST
011600060317     D                                     LIKE(assFve) OPTIONS(*OMIT)
011700060317     D  prmKcc                             LIKE(assKcc)
011800060317     D                                     CONST
011900060317     D                                     OPTIONS(*OMIT)
012000060317     D  prmKsc                             LIKE(assKsc)
012100060317     D                                     CONST
012200060317     D                                     OPTIONS(*OMIT)
012300060317     D  prmNdv                             LIKE(assNdv)
012400060317     D                                     CONST
012500060317     D                                     OPTIONS(*OMIT)
012600060317     D  prmDdv                             LIKE(assDdv)
012700060317     D                                     CONST
012800060317     D                                     OPTIONS(*OMIT)
012900060317     D  prmFtr                             LIKE(assFtr)
013000060317     D                                     CONST
013100060317     D                                     OPTIONS(*OMIT)
013200060317     D  prmEsito                     10I 0
013300060317     D  prmRrn                       10I 0 OPTIONS(*OMIT)
013400060317     D  prmFile                      10A   OPTIONS(*OMIT)
013500060317     D  prmLib                       10A   OPTIONS(*OMIT)
013600060317
013700060317      //********************************************************************************************
013800060317      //
013900060317      // Ridenominazione campi YNASS10F.
014000060317      //
014100060317      //********************************************************************************************
014200060317     Iynass100
014300060317     I              assCas                      ynAssCas
014400060317     I              assVca                      ynAssVca
014500060317     I              assCmb                      ynAssCmb
014600060317     I              assAbi                      ynAssAbi
014700060317     I              assCab                      ynAssCab
014800060317     I              assDte                      ynAssDte
014900060317     I              assKcc                      ynAssKcc
015000060317     I              assKsc                      ynAssKsc
015100060317     I              assNdv                      ynAssNdv
015200060317     I              assDdv                      ynAssDdv
015300060317
015400060317     P*--------------------------------------------------
015500060317     P* Procedure name: wrtAssegno
015600060317     P* Purpose:        Scrittura assegno.
015700060317     P* Returns:
015800060317     P* Parameter:      prmCmtCtl => File sotto COMMIT.
015900060317     P* Parameter:      prmSoc => Società
016000060317     P* Parameter:      prmFle => PO che ha incassato
016100060317     P* Parameter:      prmCuc => Codice utente
016200060317     P* Parameter:      prmAas => Anno spedizione
016300060317     P* Parameter:      prmLnp => PO spedizione
016400060317     P* Parameter:      prmNrs => Serie spedizione
016500060317     P* Parameter:      prmNsp => Numero spedizione
016600060317     P* Parameter:      prmCas => Importo assegno
016700060317     P* Parameter:      prmVca => Divisa assegno
016800060317     P* Parameter:      prmCmb => Cambio
016900060317     P* Parameter:      prmNra => Numero assegno
017000060317     P* Parameter:      prmDte => Data emissione assegno
017100060317     P* Parameter:      prmAbi => ABI assegno
017200060317     P* Parameter:      prmCab => CAB assegno
017300060317     P* Parameter:      prmTpa => Tipo assegno
017400060317     P* Parameter:      prmTpi => Tipo intestazione assegno
017500060317     P* Parameter:      prmTpp => Tipo piazza
017600060317     P* Parameter:      prmPia => Piazza assegno
017700060317     P* Parameter:      prmNot => Note
017800060317     P* Parameter:      prmFve => Assegno versabile
017900060317     P* Parameter:      prmKcc => Capo conto banca versamento
018000060317     P* Parameter:      prmKsc => Sotto conto banca versamento
018100060317     P* Parameter:      prmNdv => Numero distinta versamento
018200060317     P* Parameter:      prmDdv => Data distinta versamento
018300060317     P* Parameter:      prmFtr => Flag trasmissione
018400060317     P* Parameter:      prmEsito => Esito
018500060317     P* Parameter:      prmRrn => Numero relativo record scritto
018600060317     P* Parameter:      prmFile => File scritto
018700060317     P* Parameter:      prmLib => Libreria residenza file scritto
018800060317     P*--------------------------------------------------
018900060317     P wrtAssegno      B                   EXPORT
019000060317     D wrtAssegno      PI
019100060317     D  prmCmtCtl                     1A
019200060317     D  prmSede                       1A
019300060317     D  prmPostDatato                 1A
019400060324     D  prmSessione                  15P 0
019500060317     D  prmSoc                             LIKE(assSoc)
019600060317     D  prmFle                             LIKE(assFle)
019700060317     D  prmTpd                             LIKE(assTpd)
019800060317     D                                     OPTIONS(*OMIT)
019900060317     D  prmCuc                             LIKE(assCuc)
020000060317     D  prmLnp                             LIKE(assLnp)
020100060317     D                                     OPTIONS(*OMIT)
020200060317     D  prmAas                             LIKE(assAas)
020300060317     D                                     OPTIONS(*OMIT)
020400060317     D  prmNrs                             LIKE(assNrs)
020500060317     D                                     OPTIONS(*OMIT)
020600060317     D  prmNsp                             LIKE(assNsp)
020700060317     D                                     OPTIONS(*OMIT)
020800060317     D  prmCas                             LIKE(assCas)
020900060317     D  prmVca                             LIKE(assVca)
021000060317     D  prmCmb                             LIKE(assCmb)
021100060317     D  prmNra                             LIKE(assNra)
021200060317     D  prmDte                             LIKE(assDte)
021300060317     D  prmAbi                             LIKE(assAbi)
021400060317     D  prmCab                             LIKE(assCab)
021500060317     D  prmTpa                             LIKE(assTpa)
021600060317     D  prmTpi                             LIKE(assTpi)
021700060317     D                                     OPTIONS(*OMIT)
021800060317     D  prmTpp                             LIKE(assTpp) OPTIONS(*OMIT)
021900060317     D  prmPia                             LIKE(assPia) OPTIONS(*OMIT)
022000060317     D  prmNot                             LIKE(assNot) OPTIONS(*OMIT)
022100060317     D  prmFve                             CONST
022200060317     D                                     LIKE(assFve) OPTIONS(*OMIT)
022300060317     D  prmKcc                             LIKE(assKcc)
022400060317     D                                     CONST
022500060317     D                                     OPTIONS(*OMIT)
022600060317     D  prmKsc                             LIKE(assKsc)
022700060317     D                                     CONST
022800060317     D                                     OPTIONS(*OMIT)
022900060317     D  prmNdv                             LIKE(assNdv)
023000060317     D                                     CONST
023100060317     D                                     OPTIONS(*OMIT)
023200060317     D  prmDdv                             LIKE(assDdv)
023300060317     D                                     CONST
023400060317     D                                     OPTIONS(*OMIT)
023500060317     D  prmFtr                             LIKE(assFtr)
023600060317     D                                     CONST
023700060317     D                                     OPTIONS(*OMIT)
023800060317     D  prmEsito                     10I 0
023900060317     D  prmRrn                       10I 0 OPTIONS(*OMIT)
024000060317     D  prmFile                      10A   OPTIONS(*OMIT)
024100060317     D  prmLib                       10A   OPTIONS(*OMIT)
024200060317      /FREE
024300060317
024400060317       CLEAR prmEsito;
024500060324       gblCmtCtl = prmCmtCtl;
024600060317       IF %ADDR(prmRrn) <> *NULL;
024700060317         CLEAR prmRrn;
024800060317       ENDIF;
024900060317       IF %ADDR(prmFile) <> *NULL;
025000060317         CLEAR prmFile;
025100060317       ENDIF;
025200060317       IF %ADDR(prmLib) <> *NULL;
025300060317         CLEAR prmLib;
025400060317       ENDIF;
025500060317
025600060317       IF prmSede = *ON OR prmPostDatato = *ON;
025700060317         CLEAR ynass100;
025800060324         assSession = prmSessione;
025900060317         assSoc = prmSoc;
026000060317         assCuc = prmCuc;
026100060317         ynAssCas = prmCas;
026200060317         ynAssVca = prmVca;
026300060317         ynAssCmb = prmCmb;
026400060317         ynAssDte = %DATE(prmDte);
026500060317         ynAssAbi = %EDITC(prmAbi:'X');
026600060317         ynAssCab = %EDITC(prmCab:'X');
026700060317         IF %ADDR(prmKcc) <> *NULL;
026800060317           ynAssKcc = '00' + %EDITC(prmKcc:'X');
026900060317         ENDIF;
027000060317         IF %ADDR(prmKsc) <> *NULL;
027100060317           ynAssKsc = '0' + %EDITC(prmKsc:'X');
027200060317         ENDIF;
027300060317         IF %ADDR(prmNdv) <> *NULL;
027400060317           ynAssNdv = prmNdv;
027500060317         ENDIF;
027600060317         IF %ADDR(prmDdv) <> *NULL;
027700060317           ynAssDdv = %DATE(prmDdv:*YMD);
027800060317         ENDIF;
027900060317       ELSE;
028000060317         CLEAR cnass000;
028100060317         assCas = prmCas;
028200060317         assVca = prmVca;
028300060317         assCmb = prmCmb;
028400060317         assDte = prmDte;
028500060317         assAbi = prmAbi;
028600060317         assCab = prmCab;
028700060317         IF %ADDR(prmTpd) <> *NULL;
028800060317           assTpd = prmTpd;
028900060317         ENDIF;
029000060317         IF %ADDR(prmFve) <> *NULL;
029100060317           assFve = prmFve;
029200060317         ENDIF;
029300060317         IF %ADDR(prmKcc) <> *NULL;
029400060317           assKcc = prmKcc;
029500060317         ENDIF;
029600060317         IF %ADDR(prmKsc) <> *NULL;
029700060317           assKsc = prmKsc;
029800060317         ENDIF;
029900060317         IF %ADDR(prmNdv) <> *NULL;
030000060317           assNdv = prmNdv;
030100060317         ENDIF;
030200060317         IF %ADDR(prmDdv) <> *NULL;
030300060317           assDdv = prmDdv;
030400060317         ENDIF;
030500060317         IF %ADDR(prmFtr) <> *NULL;
030600060317           assFtr = prmFtr;
030700060317         ENDIF;
030800060317       ENDIF;
030900060317
031000060317       assFle = prmFle;
031100060317       assNra = prmNra;
031200060317       assTpa = prmTpa;
031300060317
031400060317       IF %ADDR(prmLnp) <> *NULL;
031500060317         assLnp = prmFle;
031600060317       ENDIF;
031700060317       IF %ADDR(prmAas) <> *NULL;
031800060317         assAas = prmAas;
031900060317       ENDIF;
032000060317       IF %ADDR(prmNrs) <> *NULL;
032100060317         assNrs = prmNrs;
032200060317       ENDIF;
032300060317       IF %ADDR(prmNsp) <> *NULL;
032400060317         assNsp = prmNsp;
032500060317       ENDIF;
032600060317       IF %ADDR(prmTpi) <> *NULL;
032700060317         assTpi = prmTpi;
032800060317       ENDIF;
032900060317       IF %ADDR(prmTpp) <> *NULL;
033000060317         assTpp = prmTpp;
033100060317       ENDIF;
033200060317       IF %ADDR(prmPia) <> *NULL;
033300060317         assPia = prmPia;
033400060317       ENDIF;
033500060317       IF %ADDR(prmNot) <> *NULL;
033600060317         assNot = prmNot;
033700060317       ENDIF;
033800060317
033900060317       IF prmSede = *ON OR prmPostDatato = *ON;
034000060317         IF NOT %OPEN(ynass10f);
034100060317           OPEN ynass10f;
034200060317         ENDIF;
034300060317         WRITE ynass100;
034400060317         IF %ADDR(prmRrn) <> *NULL;
034500060317           prmRrn = ynassfbk.db_rrn;
034600060317         ENDIF;
034700060317         IF %ADDR(prmFile) <> *NULL;
034800060317           prmFile = ynassfbk.file_name;
034900060317         ENDIF;
035000060317         IF %ADDR(prmLib) <> *NULL;
035100060317           prmLib = ynassfbk.library;
035200060317         ENDIF;
035300060317       ELSE;
035400060317         IF NOT %OPEN(cnass00f);
035500060317           OPEN cnass00f;
035600060317         ENDIF;
035700060317         WRITE cnass000;
035800060317         IF %ADDR(prmRrn) <> *NULL;
035900060317           prmRrn = cnassfbk.db_rrn;
036000060317         ENDIF;
036100060317         IF %ADDR(prmFile) <> *NULL;
036200060317           prmFile = cnassfbk.file_name;
036300060317         ENDIF;
036400060317         IF %ADDR(prmLib) <> *NULL;
036500060317           prmLib = cnassfbk.library;
036600060317         ENDIF;
036700060317       ENDIF;
036800060317
036900060317       RETURN;
037000060317
037100060317       CLOSE(E) ynass10f;
037200060317       CLOSE(E) cnass00f;
037300060317
037400060317      /END-FREE
037500060317     P wrtAssegno      E
