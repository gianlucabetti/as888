000100051012      *PARMS DYNUSRPRF(*OWNER)
000200050930      *PARMS
000300050930      *PARMS
000400050930      *PARMS
000500050930      *PARMS
000600050930
000700050930      /TITLE Selettore log disposizioni.
000800050930
000900050930     H DFTACTGRP(*NO) ACTGRP(*CALLER)
001000050930
001100050930     ***************************************************************************
001200050930     **?
001300050930     **?Definizione costanti.
001400050930     **?
001500050930     ***************************************************************************
001600050930     D NonVisionate    C                   'N'
001700050930     D GiaVisionate    C                   'V'
001800050930     D Scartate        C                   'S'
001900050930     D Caricate        C                   'C'
002000050930     D Eseguito        C                   '0'
002100050930     D Errore          C                   '1'
002200050930     D NuovaRichiesta  C                   '1'
002300050930     D Continua        C                   '2'
002400050930     D FineRichiesta   C                   '9'
002500050930
002600050930     ***************************************************************************
002700050930     **?
002800050930     **?Definizione strutture dati.
002900050930     **?
003000050930     ***************************************************************************
003100050930     D fnvar10s      E DS
003200050930     D                                     BASED(NullPtr)
003300050930     D kpjba         E DS
003400050930     D fnlg9610s     E DS
003500050930     D                                     INZ
003600050930     D                                     PREFIX(Flt)
003700050930     D fnlg9630s     E DS
003800050930     D                                     PREFIX(dta)
003900050930     D  dati                         30
004000050930     D                                     DIM(99)
004100050930     D                                     OVERLAY(dtaDati)
004200050930     D row             DS
004300050930     D                                     OCCURS(99)
004400050930     D                                     INZ
004500050930     D  rowAas
004600050930     D                                     LIKE(varAas)
004700050930     D  rowLnp
004800050930     D                                     LIKE(varLnp)
004900050930     D  rowNrs
005000050930     D                                     LIKE(varNrs)
005100050930     D  rowNsp
005200050930     D                                     LIKE(varNsp)
005300050930     D  rowAgc
005400050930     D                                     LIKE(varAgc)
005500050930     D  rowFgc
005600050930     D                                     LIKE(varFgc)
005700050930     D  rowNgc
005800050930     D                                     LIKE(varNgc)
005900050930
006000050930     ***************************************************************************
006100050930     **?
006200050930     **?Definizione variabili.
006300050930     **?
006400050930     ***************************************************************************
006500050930     D NullPtr         S               *
006600050930     D wrkGetLista     S           4096
006700050930     D                                     VARYING
006800050930     D i               S             10I 0
006900050930     D Esito           S              1
007000050930     D wrkDrdd         S
007100050930     D                                     LIKE(varDrd)
007200050930     D wrkDrda         S
007300050930     D                                     LIKE(varDrd)
007400050930
007500050930     ***************************************************************************
007600050930     **?
007700050930     **?Definizione parametri procedura.
007800050930     **?
007900050930     ***************************************************************************
008000050930     C     *ENTRY        PLIST
008100050930     C                   PARM                    kpjba
008200050930     C                   PARM                    fnlg9630s
008300050930
008400050930     ***************************************************************************
008500050930     **?
008600050930     **?Main
008700050930     **?
008800050930     ***************************************************************************
008900050930     C                   EXSR      inzProc
009000050930     C                   EXSR      selFnvar
009100050930     C                   EXSR      Uscita
009200050930
009300050930     ***************************************************************************
009400050930     **?
009500050930     **?Operazioni iniziali.
009600050930     **?
009700050930     ***************************************************************************
009800050930     C     *INZSR        BEGSR
009900050930
010000050930     C                   ENDSR
010100050930
010200050930     ***************************************************************************
010300050930     **?
010400050930     **?Inizializzazione procedura.
010500050930     **?
010600050930     ***************************************************************************
010700050930     C     inzProc       BEGSR
010800050930
010900050930     C                   EVAL      fnlg9610s = KPJBU
011000050930     C                   CLEAR                   dtaEsito
011100050930     C                   CLEAR                   dtaEOF
011200050930     C                   CLEAR                   dtaRigheOut
011300050930     C                   CLEAR                   dati
011400050930
011500050930     C                   ENDSR
011600050930
011700050930     ***************************************************************************
011800050930     **?
011900050930     **?Selezione righe.
012000050930     **?
012100050930     ***************************************************************************
012200050930     C     selFnvar      BEGSR
012300050930
012400050930     **?Il chiamante mi dice di chiudere.
012500050930     C                   IF        dtaOperazione = FineRichiesta
012600050930     C                   EVAL      dtaEOF = *ON
012700050930     C                   EXSR      CloseLista
012800050930     C                   LEAVESR
012900050930     C                   ENDIF
013000050930
013100050930     **?Al ricevimento di una nuova richiesta chiudo il cursore già aperto
013200050930     **?e preparo la nuova istruzione SQL.
013300050930     C                   IF        dtaOperazione = NuovaRichiesta
013400050930
013500050930     C                   IF        %LEN(wrkGetLista) > 0
013600050930     C                   EXSR      CloseLista
013700050930     C                   ENDIF
013800050930     **?Imposto la parte fissa dell'istruzione.
013900050930     C                   EVAL      wrkGetLista =
014000050930     C                             'SELECT VARAAS, VARLNP, VARNRS, VARNSP, -
014100050930     C                             VARAGC, VARFGC, VARNGC FROM FNVAR10S'
014200050930     **?WHERE
014300050930     C                   EVAL      wrkGetLista = wrkGetLista +
014400050930     C                             ' WHERE VARVIS = ''S'''
014500050930     **?Selezione sul PO richiedente.
014600050930     C                   EVAL      wrkGetLista = wrkGetLista
014700050930     C                             + ' AND VARLNP = '
014800050930     C                             + %TRIML(%EDITC(fltPO:'Z'))
014900050930     **?Selezione sulla presa visione.
015000050930     C                   SELECT
015100050930     C                   WHEN      fltPresaVisio = NonVisionate
015200050930     C                   EVAL      wrkGetLista = wrkGetLista +
015300050930     C                             ' AND VARFLG = '' '''
015400050930     C                   WHEN      fltPresaVisio = GiaVisionate
015500050930     C                   EVAL      wrkGetLista = wrkGetLista +
015600050930     C                             ' AND VARFLG = ''S'''
015700050930     C                   ENDSL
015800050930     **?Selezione sull'esito ricezione.
015900050930     C                   SELECT
016000050930     C                   WHEN      fltEsitoRicez = Scartate
016100050930     C                   EVAL      wrkGetLista = wrkGetLista +
016200050930     C                             ' AND VARESI <> ''GOK'''
016300050930     C                   WHEN      fltEsitoRicez = Caricate
016400050930     C                   EVAL      wrkGetLista = wrkGetLista +
016500050930     C                             ' AND VARESI = ''GOK'''
016600050930     C                   ENDSL
016700050930     **?Selezione sul codice cliente.
016800050930     C                   IF        fltKsc > *ZERO
016900050930     C                   EVAL      wrkGetLista = wrkGetLista
017000050930     C                             + ' AND VARCCM = '
017100050930     C                             + %TRIML(%EDITC(fltKsc:'Z'))
017200050930     C                   ENDIF
017300050930     **?Selezione sulla data ricezione.
017400050930     C                   IF        fltDrdd <> *LOVAL OR fltDrda <> *HIVAL
017500050930     C                   MOVE      fltDrdd       wrkDrdd
017600050930     C                   MOVE      fltDrda       wrkDrda
017700050930     C                   EVAL      wrkGetLista = wrkGetLista +
017800051010     C                             ' AND VARDRD BETWEEN ' +
017900051010     C                             %EDITC(wrkDrdd:'X') + ' AND ' +
018000060103     C                             %EDITC(wrkDrda:'X')
018100050930     C                   ENDIF
018200050930     **?Selezione sull'anno giacenza.
018300060103     C                   IF        fltAgc > *ZERO
018400050930     C                   EVAL      wrkGetLista = wrkGetLista +
018500051010     C                             ' AND VARAGC = ' + %TRIML(%EDITC(fltAgc:'Z'))
018600050930     C                   ENDIF
018700050930     **?Selezione sul PO giacenza.
018800050930     C                   IF        fltFgc > *ZERO
018900050930     C                   EVAL      wrkGetLista = wrkGetLista +
019000050930     C                             ' AND VARFGC = ' + %TRIML(%EDITC(fltFgc:'Z'))
019100050930     C                   ENDIF
019200050930     **?Selezione sul n. giacenza.
019300050930     C                   IF        fltNgc > *ZERO
019400050930     C                   EVAL      wrkGetLista = wrkGetLista +
019500050930     C                             ' AND VARNGC = ' + %TRIML(%EDITC(fltNgc:'Z'))
019600050930     C                   ENDIF
019700050930     **?Selezione sull'anno spedizione.
019800050930     C                   IF        fltAas > *ZERO
019900050930     C                   EVAL      wrkGetLista = wrkGetLista +
020000050930     C                             ' AND VARAAS = ' + %TRIML(%EDITC(fltAas:'Z'))
020100050930     C                   ENDIF
020200050930     **?Selezione sul PO spedizione.
020300050930     C                   IF        fltLnp > *ZERO
020400050930     C                   EVAL      wrkGetLista = wrkGetLista +
020500050930     C                             ' AND VARLNP = ' + %TRIML(%EDITC(fltLnp:'Z'))
020600050930     C                   ENDIF
020700050930     **?Selezione sul n. spedizione.
020800050930     C                   IF        fltNsp > *ZERO
020900050930     C                   EVAL      wrkGetLista = wrkGetLista +
021000050930     C                             ' AND VARNSP = ' + %TRIML(%EDITC(fltNsp:'Z'))
021100050930     C                   ENDIF
021200050930     **?Selezione sulla serie spedizione.
021300050930     C                   SELECT
021400050930     C                   WHEN      fltNrs = *ZERO AND fltNsp > *ZERO
021500050930     C                   EVAL      wrkGetLista = wrkGetLista + ' AND VARNRS = 0'
021600050930     C                   WHEN      fltNrs > *ZERO
021700050930     C                   EVAL      wrkGetLista = wrkGetLista +
021800051010     C                             ' AND VARNRS = ' + %TRIML(%EDITC(fltNrs:'X'))
021900050930     C                   ENDSL
022000050930     **?Raggruppamento.
022100050930     C                   EVAL      wrkGetLista = wrkGetLista +
022200050930     C                             ' GROUP BY VARAAS, VARLNP, VARNRS, VARNSP, -
022300050930     C                             VARAGC, VARFGC, VARNGC'
022400050930     **?Ordinamento.
022500050930     C                   EVAL      wrkGetLista = wrkGetLista +
022600050930     C                             ' ORDER BY VARAAS, VARLNP, VARNRS, VARNSP, -
022700050930     C                             VARAGC, VARFGC, VARNGC'
022800050930     **?Solo lettura.
022900050930     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
023000050930     C
023100050930     C/EXEC SQL
023200050930     C+ PREPARE LISTA FROM :wrkGetLista
023300050930     C/END-EXEC
023400050930     C
023500050930     C/EXEC SQL
023600050930     C+ DECLARE LISTA CURSOR FOR LISTA
023700050930     C/END-EXEC
023800050930     C
023900050930     C/EXEC SQL
024000050930     C+ OPEN LISTA
024100050930     C/END-EXEC
024200050930     C
024300050930     C                   ENDIF
024400050930
024500050930     C/EXEC SQL
024600050930     C+ FETCH NEXT FROM LISTA FOR :dtaRigheRic ROWS INTO :Row
024700050930     C/END-EXEC
024800050930
024900050930     C                   SELECT
025000050930     C                   WHEN      sqlCod = 100
025100050930     C                   EVAL      dtaEOF = *ON
025200050930     C
025300050930     C                   WHEN      sqlCod < 0
025400050930     C                   EVAL      dtaEOF = *ON
025500050930     C                   EVAL      dtaEsito = Errore
025600050930     C
025700050930     C                   OTHER
025800050930     **?A questo punto nella DS multipla ROW ci possono essere da 0 a
025900050930     **?99 ricorrenze. Il numero è contenuto nella variabile SQLER3.
026000050930     **?Scorro la DS multipla e riempio le schiere.
026100050930     C                   DO        sqlEr3        i
026200050930     C     i             OCCUR     row
026300050930     C                   EVAL      dati(i) = row
026400050930     C                   ENDDO
026500050930     C
026600050930     C                   EVAL      dtaRigheOut = sqlEr3
026700050930     C                   EVAL      dtaEsito = Eseguito
026800050930     C                   EVAL      dtaEOF = (sqlEr5 = 100 OR
026900050930     C                             sqlEr3 < dtaRigheRic)
027000050930     C
027100050930     C                   ENDSL
027200050930
027300050930     C                   IF        dtaEOF = *ON
027400050930     C                   EXSR      CloseLista
027500050930     C                   ENDIF
027600050930
027700050930     C                   ENDSR
027800050930
027900050930     ***********************************************************************
028000050930     **?
028100050930     **?Chiusura cursore.
028200050930     **?
028300050930     ***********************************************************************
028400050930     C     CloseLista    BEGSR
028500050930
028600050930     C/EXEC SQL
028700050930     C+ CLOSE Lista
028800050930     C/END-EXEC
028900050930
029000050930     C                   ENDSR
029100050930
029200050930     ***************************************************************************
029300050930     **?
029400050930     **?Uscita
029500050930     **?
029600050930     ***************************************************************************
029700050930     C     Uscita        BEGSR
029800050930
029900050930     C                   EVAL      *INLR = (dtaOperazione = FineRichiesta)
030000050930     C                   RETURN
030100050930
030200050930     C                   ENDSR
