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
005900170313     D  rowDmcf                       1
006000170313     D
006100050930
006200050930     ***************************************************************************
006300050930     **?
006400050930     **?Definizione variabili.
006500050930     **?
006600050930     ***************************************************************************
006700050930     D NullPtr         S               *
006800050930     D wrkGetLista     S           4096
006900050930     D                                     VARYING
007000050930     D i               S             10I 0
007100050930     D Esito           S              1
007200050930     D wrkDrdd         S
007300050930     D                                     LIKE(varDrd)
007400050930     D wrkDrda         S
007500050930     D                                     LIKE(varDrd)
007600050930
007700050930     ***************************************************************************
007800050930     **?
007900050930     **?Definizione parametri procedura.
008000050930     **?
008100050930     ***************************************************************************
008200050930     C     *ENTRY        PLIST
008300050930     C                   PARM                    kpjba
008400050930     C                   PARM                    fnlg9630s
008500050930
008600050930     ***************************************************************************
008700050930     **?
008800050930     **?Main
008900050930     **?
009000050930     ***************************************************************************
009100050930     C                   EXSR      inzProc
009200050930     C                   EXSR      selFnvar
009300050930     C                   EXSR      Uscita
009400050930
009500050930     ***************************************************************************
009600050930     **?
009700050930     **?Operazioni iniziali.
009800050930     **?
009900050930     ***************************************************************************
010000050930     C     *INZSR        BEGSR
010100050930
010200050930     C                   ENDSR
010300050930
010400050930     ***************************************************************************
010500050930     **?
010600050930     **?Inizializzazione procedura.
010700050930     **?
010800050930     ***************************************************************************
010900050930     C     inzProc       BEGSR
011000050930
011100050930     C                   EVAL      fnlg9610s = KPJBU
011200050930     C                   CLEAR                   dtaEsito
011300050930     C                   CLEAR                   dtaEOF
011400050930     C                   CLEAR                   dtaRigheOut
011500050930     C                   CLEAR                   dati
011600050930
011700050930     C                   ENDSR
011800050930
011900050930     ***************************************************************************
012000050930     **?
012100050930     **?Selezione righe.
012200050930     **?
012300050930     ***************************************************************************
012400050930     C     selFnvar      BEGSR
012500050930
012600050930     **?Il chiamante mi dice di chiudere.
012700050930     C                   IF        dtaOperazione = FineRichiesta
012800050930     C                   EVAL      dtaEOF = *ON
012900050930     C                   EXSR      CloseLista
013000050930     C                   LEAVESR
013100050930     C                   ENDIF
013200050930
013300050930     **?Al ricevimento di una nuova richiesta chiudo il cursore già aperto
013400050930     **?e preparo la nuova istruzione SQL.
013500050930     C                   IF        dtaOperazione = NuovaRichiesta
013600050930
013700050930     C                   IF        %LEN(wrkGetLista) > 0
013800050930     C                   EXSR      CloseLista
013900050930     C                   ENDIF
014000050930     **?Imposto la parte fissa dell'istruzione.
014100050930     C                   EVAL      wrkGetLista =
014200050930     C                             'SELECT VARAAS, VARLNP, VARNRS, VARNSP, -
014300170314     C                             VARAGC, VARFGC, VARNGC, ute from(-
014400170314     C                             SELECT VARAAS, VARLNP, VARNRS, VARNSP, -
014500170314     C                             VARAGC, VARFGC, VARNGC, case when -
014600170314     C                             varnpr < 900 then ''M'' else case when -
014700170314     C                             substr(vardmc, 50, 1)= '' '' then ''M'' -
014800170314     C                             else substr(vardmc, 50, 1) end end as ute -
014900170314     c                             from fnvar10s'
015000170314     **?WHERE
015100050930     C                   EVAL      wrkGetLista = wrkGetLista +
015200170314     C                             ' WHERE VARVIS = ''S'' -
015300170314     c                             and (varnpr < 900 or varnpr -
015400170314     c                             in(900, 910, 911, 914, 916, 920))'
015500050930     **?Selezione sul PO richiedente.
015600170227     c                   if        kcdaz = 'LG96'
015700050930     C                   EVAL      wrkGetLista = wrkGetLista
015800050930     C                             + ' AND VARLNP = '
015900050930     C                             + %TRIML(%EDITC(fltPO:'Z'))
016000170227     c                   else
016100170227     C                   EVAL      wrkGetLista = wrkGetLista
016200170227     C                             + ' AND VARFGC = '
016300170227     C                             + %TRIML(%EDITC(fltPO:'Z'))
016400170227     c                   endif
016500050930     **?Selezione sulla presa visione.
016600050930     C                   SELECT
016700050930     C                   WHEN      fltPresaVisio = NonVisionate
016800050930     C                   EVAL      wrkGetLista = wrkGetLista +
016900050930     C                             ' AND VARFLG = '' '''
017000050930     C                   WHEN      fltPresaVisio = GiaVisionate
017100050930     C                   EVAL      wrkGetLista = wrkGetLista +
017200050930     C                             ' AND VARFLG = ''S'''
017300050930     C                   ENDSL
017400050930     **?Selezione sull'esito ricezione.
017500050930     C                   SELECT
017600050930     C                   WHEN      fltEsitoRicez = Scartate
017700050930     C                   EVAL      wrkGetLista = wrkGetLista +
017800050930     C                             ' AND VARESI <> ''GOK'''
017900050930     C                   WHEN      fltEsitoRicez = Caricate
018000050930     C                   EVAL      wrkGetLista = wrkGetLista +
018100050930     C                             ' AND VARESI = ''GOK'''
018200050930     C                   ENDSL
018300050930     **?Selezione sul codice cliente.
018400050930     C                   IF        fltKsc > *ZERO
018500050930     C                   EVAL      wrkGetLista = wrkGetLista
018600050930     C                             + ' AND VARCCM = '
018700050930     C                             + %TRIML(%EDITC(fltKsc:'Z'))
018800050930     C                   ENDIF
018900050930     **?Selezione sulla data ricezione.
019000050930     C                   IF        fltDrdd <> *LOVAL OR fltDrda <> *HIVAL
019100050930     C                   MOVE      fltDrdd       wrkDrdd
019200050930     C                   MOVE      fltDrda       wrkDrda
019300050930     C                   EVAL      wrkGetLista = wrkGetLista +
019400051010     C                             ' AND VARDRD BETWEEN ' +
019500051010     C                             %EDITC(wrkDrdd:'X') + ' AND ' +
019600060103     C                             %EDITC(wrkDrda:'X')
019700050930     C                   ENDIF
019800050930     **?Selezione sull'anno giacenza.
019900060103     C                   IF        fltAgc > *ZERO
020000050930     C                   EVAL      wrkGetLista = wrkGetLista +
020100051010     C                             ' AND VARAGC = ' + %TRIML(%EDITC(fltAgc:'Z'))
020200050930     C                   ENDIF
020300050930     **?Selezione sul PO giacenza.
020400050930     C                   IF        fltFgc > *ZERO
020500050930     C                   EVAL      wrkGetLista = wrkGetLista +
020600050930     C                             ' AND VARFGC = ' + %TRIML(%EDITC(fltFgc:'Z'))
020700050930     C                   ENDIF
020800050930     **?Selezione sul n. giacenza.
020900050930     C                   IF        fltNgc > *ZERO
021000050930     C                   EVAL      wrkGetLista = wrkGetLista +
021100050930     C                             ' AND VARNGC = ' + %TRIML(%EDITC(fltNgc:'Z'))
021200050930     C                   ENDIF
021300050930     **?Selezione sull'anno spedizione.
021400050930     C                   IF        fltAas > *ZERO
021500050930     C                   EVAL      wrkGetLista = wrkGetLista +
021600050930     C                             ' AND VARAAS = ' + %TRIML(%EDITC(fltAas:'Z'))
021700050930     C                   ENDIF
021800050930     **?Selezione sul PO spedizione.
021900050930     C                   IF        fltLnp > *ZERO
022000050930     C                   EVAL      wrkGetLista = wrkGetLista +
022100050930     C                             ' AND VARLNP = ' + %TRIML(%EDITC(fltLnp:'Z'))
022200050930     C                   ENDIF
022300050930     **?Selezione sul n. spedizione.
022400050930     C                   IF        fltNsp > *ZERO
022500050930     C                   EVAL      wrkGetLista = wrkGetLista +
022600050930     C                             ' AND VARNSP = ' + %TRIML(%EDITC(fltNsp:'Z'))
022700050930     C                   ENDIF
022800050930     **?Selezione sulla serie spedizione.
022900050930     C                   SELECT
023000050930     C                   WHEN      fltNrs = *ZERO AND fltNsp > *ZERO
023100050930     C                   EVAL      wrkGetLista = wrkGetLista + ' AND VARNRS = 0'
023200050930     C                   WHEN      fltNrs > *ZERO
023300050930     C                   EVAL      wrkGetLista = wrkGetLista +
023400051010     C                             ' AND VARNRS = ' + %TRIML(%EDITC(fltNrs:'X'))
023500050930     C                   ENDSL
023600050930     **?Raggruppamento.
023700050930     C                   EVAL      wrkGetLista = wrkGetLista +
023800170315     c                             ') as tab '
023900170315     **?Selezione sul PO richiedente.
024000170315     c                   if        kcdaz = 'LG96'
024100170315     C                   EVAL      wrkGetLista = wrkGetLista
024200170315     C                             + ' WHERE ute = ''M'''
024300170315     c                   else
024400170315     C                   EVAL      wrkGetLista = wrkGetLista
024500170315     C                             + ' WHERE ute = ''D'''
024600170315     c                   endif
024700170315     C                   EVAL      wrkGetLista = wrkGetLista +
024800170315     C                             ' GROUP BY VARAAS, VARLNP, VARNRS, VARNSP, -
024900170314     C                             VARAGC, VARFGC, VARNGC, ute'
025000050930     **?Ordinamento.
025100050930     C                   EVAL      wrkGetLista = wrkGetLista +
025200050930     C                             ' ORDER BY VARAAS, VARLNP, VARNRS, VARNSP, -
025300170314     C                             VARAGC, VARFGC, VARNGC'
025400050930     **?Solo lettura.
025500050930     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
025600050930     C
025700050930     C/EXEC SQL
025800050930     C+ PREPARE LISTA FROM :wrkGetLista
025900050930     C/END-EXEC
026000050930     C
026100050930     C/EXEC SQL
026200050930     C+ DECLARE LISTA CURSOR FOR LISTA
026300050930     C/END-EXEC
026400050930     C
026500050930     C/EXEC SQL
026600050930     C+ OPEN LISTA
026700050930     C/END-EXEC
026800050930     C
026900050930     C                   ENDIF
027000050930
027100050930     C/EXEC SQL
027200050930     C+ FETCH NEXT FROM LISTA FOR :dtaRigheRic ROWS INTO :Row
027300050930     C/END-EXEC
027400050930
027500170314     c                   clear                   filtro            1
027600050930     C                   SELECT
027700050930     C                   WHEN      sqlCod = 100
027800050930     C                   EVAL      dtaEOF = *ON
027900050930     C
028000050930     C                   WHEN      sqlCod < 0
028100050930     C                   EVAL      dtaEOF = *ON
028200050930     C                   EVAL      dtaEsito = Errore
028300050930     C
028400050930     C                   OTHER
028500050930     **?A questo punto nella DS multipla ROW ci possono essere da 0 a
028600050930     **?99 ricorrenze. Il numero è contenuto nella variabile SQLER3.
028700050930     **?Scorro la DS multipla e riempio le schiere.
028800050930     C                   DO        sqlEr3        i
028900050930     C     i             OCCUR     row
029000050930     C                   EVAL      dati(i) = row
029100050930     C                   ENDDO
029200170228     C
029300050930     C                   EVAL      dtaRigheOut = sqlEr3
029400050930     C                   EVAL      dtaEsito = Eseguito
029500170314     C                   EVAL      dtaEOF = (sqlEr5 = 100 OR
029600170314     C                             sqlEr3 < dtaRigheRic)
029700050930     C                   ENDSL
029800050930
029900050930     C                   IF        dtaEOF = *ON
030000050930     C                   EXSR      CloseLista
030100050930     C                   ENDIF
030200050930
030300050930     C                   ENDSR
030400050930
030500050930     ***********************************************************************
030600050930     **?
030700050930     **?Chiusura cursore.
030800050930     **?
030900050930     ***********************************************************************
031000050930     C     CloseLista    BEGSR
031100050930
031200050930     C/EXEC SQL
031300050930     C+ CLOSE Lista
031400050930     C/END-EXEC
031500050930
031600050930     C                   ENDSR
031700050930
031800050930     ***************************************************************************
031900050930     **?
032000050930     **?Uscita
032100050930     **?
032200050930     ***************************************************************************
032300050930     C     Uscita        BEGSR
032400050930
032500050930     C                   EVAL      *INLR = (dtaOperazione = FineRichiesta)
032600050930     C                   RETURN
032700050930
032800050930     C                   ENDSR
