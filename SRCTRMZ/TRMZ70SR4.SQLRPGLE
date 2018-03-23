000100080327     H**********************************************************************
000200040820     H DECEDIT('0,') DATEDIT(*dmy.)
000300080327     H**********************************************************************
000400080328      *  Utility per decodificare con Società + Fornitore su PROJ i dati
000500080328      *  della RagSoc FORNITORE e Partita IVA
000600080328      *  basandosi sulla società e sull'unità preferenziale/Fornitore
000700080328      *  passate come parametri.
000800080328      *  Legge e restituisce un record JOIN di ANRCO e ANSOG .
000900020409     H**********************************************************************
001000080327     D AnRCO00f      e DS                  prefix(Out_)
001100080327     D AnSOG00f      e DS                  prefix(Out_)
001200080331     D AnIND00f      e DS                  prefix(Out_)
001300090506      *
001400090506     D trmz70s4ds    e DS
001500040820      *
001600040820     D WrkSqlCmd       S           1024
001700080327      *
001800040820     D  data_Oggi      s                   like(G02INV) INZ(0)
001900040826     D  almeno_uno     s              1
002000090506     D  daksc          s                   like(keyksc)
002100090506     D  aksc           s                   like(keyksc)
002200080331      *
002300080331     D WLBDAT          DS
002400080331     D  G02DAT                 1      8  0
002500080331     D  G02INV                 9     16  0
002600080331     D  G02ERR                17     17
002700080331     D  G02TGI                18     22  0
002800040820      *
002900040820     D digits          C                   CONST('0123456789')
003000040820      *****************************************************************
003100080327     c                   exsr      EntrySR
003200080327
003300090506     c                   if        societa <> *blank and unita <> *blank
003400090506     C/EXEC SQL
003500090506     C+ SELECT
003600090506     C+   RCOKCC, RCOKSC, RCOSOCIETA, RCOUNITA,
003700150616     C+   SOGDES, SOGDESAGG, SOGPARTIVA, SOGCDFISC, SOGNATGIUR,
003800150616     C+   INDINDRIZ, INDLOCALIT, INDCAP, INDPROV, INDSTATO
003900090506     C+  INTO
004000090506     C+   :Out_RCOKCC, :Out_RCOKSC, :Out_RCOSOCIETA,
004100090506     C+   :Out_RCOUNITA,  :Out_SOGDES, :Out_SOGDESAGG,
004200150616     C+   :Out_SOGPARTIVA, :out_SOGCDFISC, :Out_SOGNATGIUR,
004300090521     C+   :Out_INDINDRIZ, :Out_INDLOCALIT, :Out_INDCAP,
004400090521     C+   :Out_INDPROV, :Out_INDSTATO
004500090506     C+  FROM ANRCO00F JOIN ANSOG00F ON RCOSOGG = SOGSOGG JOIN
004600090506     C+ ANIND00F ON INDSOGG = SOGSOGG and INDCDIND=' ' and
004700090506     c+  INDTPIND=' '
004800090506     C+  WHERE
004900090506     C+  RCOksc between :daksc and :aksc   and
005000090506     C+  RCOSNATURA = :Sottonatur   and
005100090506     c+  RCOSOCIETA = :societa      and
005200090506     c+  RCOUNITA   = :unita
005300090506     c+  and  RCODTANN = '0001-01-01'
005400090506     c+  and  SOGDTFIVL= '0001-01-01'
005500090506     c+    ORDER BY RCODTIMM desc
005600090506     c+    FETCH FIRST  ROW ONLY
005700090506     C/END-EXEC
005800090506     c                   elseif    societa <> *blank and unita = *blank
005900090506     C/EXEC SQL
006000090506     C+ SELECT
006100090506     C+   RCOKCC, RCOKSC, RCOSOCIETA, RCOUNITA,
006200150616     C+   SOGDES, SOGDESAGG, SOGPARTIVA, SOGCDFISC, SOGNATGIUR,
006300150616     C+   INDINDRIZ, INDLOCALIT, INDCAP, INDPROV, INDSTATO
006400090506     C+  INTO
006500090506     C+   :Out_RCOKCC, :Out_RCOKSC, :Out_RCOSOCIETA,
006600090506     C+   :Out_RCOUNITA,  :Out_SOGDES, :Out_SOGDESAGG,
006700150616     C+   :Out_SOGPARTIVA, :out_SOGCDFISC, :Out_SOGNATGIUR,
006800090506     C+   :Out_INDINDRIZ, :Out_INDLOCALIT, :Out_INDCAP,
006900150616     C+   :Out_INDPROV, :Out_INDSTATO
007000090506     C+  FROM ANRCO00F JOIN ANSOG00F ON RCOSOGG = SOGSOGG JOIN
007100090506     C+ ANIND00F ON INDSOGG = SOGSOGG and INDCDIND=' ' and
007200090506     c+  INDTPIND=' '
007300090506     C+  WHERE
007400090506     C+  RCOksc between :daksc and :aksc   and
007500090506     C+  RCOSNATURA = :Sottonatur   and
007600090506     c+  RCOSOCIETA = :societa
007700090506     c+  and  RCODTANN = '0001-01-01'
007800090506     c+  and  SOGDTFIVL= '0001-01-01'
007900090506     c+    ORDER BY RCODTIMM desc
008000090506     c+    FETCH FIRST  ROW ONLY
008100090506     C/END-EXEC
008200090506     c                   elseif    societa = *blank and unita <>*blank
008300090506     C/EXEC SQL
008400090506     C+ SELECT
008500090506     C+   RCOKCC, RCOKSC, RCOSOCIETA, RCOUNITA,
008600150616     C+   SOGDES, SOGDESAGG, SOGPARTIVA, SOGCDFISC, SOGNATGIUR,
008700150616     C+   INDINDRIZ, INDLOCALIT, INDCAP, INDPROV, INDSTATO
008800090506     C+  INTO
008900090506     C+   :Out_RCOKCC, :Out_RCOKSC, :Out_RCOSOCIETA,
009000090506     C+   :Out_RCOUNITA,  :Out_SOGDES, :Out_SOGDESAGG,
009100150616     C+   :Out_SOGPARTIVA, :out_SOGCDFISC, :Out_SOGNATGIUR,
009200090506     C+   :Out_INDINDRIZ, :Out_INDLOCALIT, :Out_INDCAP,
009300150616     C+   :Out_INDPROV, :Out_INDSTATO
009400090506     C+  FROM ANRCO00F JOIN ANSOG00F ON RCOSOGG = SOGSOGG JOIN
009500090506     C+ ANIND00F ON INDSOGG = SOGSOGG and INDCDIND=' ' and
009600090506     c+  INDTPIND=' '
009700090506     C+  WHERE
009800090506     C+  RCOksc between :daksc and :aksc   and
009900090506     C+  RCOSNATURA = :Sottonatur   and
010000090506     c+ (RCOSOCIETA = :societa or :societa = '')  and
010100090506     c+ (RCOUNITA   = :unita   or :unita   = '')
010200090506     c+  and  RCODTANN = '0001-01-01'
010300090506     c+  and  SOGDTFIVL= '0001-01-01'
010400090506     c+    ORDER BY RCODTIMM desc
010500090506     c+    FETCH FIRST  ROW ONLY
010600090506     C/END-EXEC
010700090506     c                   elseif    societa = *blank and unita =*blank
010800090506     C/EXEC SQL
010900090506     C+ SELECT
011000090506     C+   RCOKCC, RCOKSC, RCOSOCIETA, RCOUNITA,
011100150616     C+   SOGDES, SOGDESAGG, SOGPARTIVA, SOGCDFISC, SOGNATGIUR,
011200150616     C+   INDINDRIZ, INDLOCALIT, INDCAP, INDPROV, INDSTATO
011300090506     C+  INTO
011400090506     C+   :Out_RCOKCC, :Out_RCOKSC, :Out_RCOSOCIETA,
011500090506     C+   :Out_RCOUNITA,  :Out_SOGDES, :Out_SOGDESAGG,
011600150616     C+   :Out_SOGPARTIVA, :out_SOGCDFISC, :Out_SOGNATGIUR,
011700090506     C+   :Out_INDINDRIZ, :Out_INDLOCALIT, :Out_INDCAP,
011800150616     C+   :Out_INDPROV, :Out_INDSTATO
011900090506     C+  FROM ANRCO00F JOIN ANSOG00F ON RCOSOGG = SOGSOGG JOIN
012000090506     C+ ANIND00F ON INDSOGG = SOGSOGG and INDCDIND=' ' and
012100090506     c+  INDTPIND=' '
012200090506     C+  WHERE
012300090506     C+  RCOksc between :daksc and :aksc   and
012400090521     C+  RCOSNATURA = :Sottonatur
012500090506     c+  and  RCODTANN = '0001-01-01'
012600090506     c+  and  SOGDTFIVL= '0001-01-01'
012700090506     c+    ORDER BY RCODTIMM desc
012800090506     c+    FETCH FIRST  ROW ONLY
012900090506     C/END-EXEC
013000090506     c                   end
013100090506
013200090506      * esegue una sola lettura e quindi restituisce 1 solo record
013300090506      * quello immesso + di recente
013400090506     C                   SELECT
013500090506     **
013600090506     C                   WHEN      SqlCod < 0
013700090506     c                   eval      Errore = '9'
013800090506     c                   eval      ErrMsg = 'Problemi di SQL. SQLCOD = '
013900090506     c                   eval      ErrMsg = %trim(ErrMsg) + %EDITC(SQLCOD:'K')
014000090506     C                   dump(A)
014100090506     c                   goto      fine
014200090506     c* se Fine File --> EoF=ON
014300090506     C                   WHEN      SqlCod = 100
014400090506     c                   eval      Trovato = *off
014500090506     c                   eval      Errore = '1'
014600090506     c                   eval      ErrMsg = 'EoF: Non trovato nessun record +
014700090506     c                             SQLCOD = 100'
014800090506     c*
014900090506     C                   OTHER
015000090506      *
015100090506     c* Lettura del 1°record --> EoF=OFF
015200090506     c                   eval      Trovato = *on
015300090506     c                   eval      Errore = '0'
015400090506     c                   eval      PartitaIVA = Out_SOGPARTIVA
015500090506     c                   eval      Societa   = Out_RCOSOCIETA
015600090506     c                   eval      Unita     = Out_RCOUNITA
015700090506     c                   eval      KeyKCC    = Out_RCOKCC
015800090506     c                   eval      KeyKSC    = Out_RCOKSC
015900090506     c                   eval      RagSocKSC = Out_SOGDES
016000090506     c                   eval      RagSocKSC1= Out_SOGDESAGG
016100090506     c                   eval      Indirizzo = Out_INDINDRIZ
016200090506     c                   eval      Localita  = Out_INDLOCALIT
016300090506     c                   eval      CAP       = Out_INDCAP
016400090506     c                   eval      Provincia = Out_INDPROV
016500090506     c                   eval      Stato     = Out_INDSTATO
016600090506     c                   eval      CodiceFisc = Out_SOGCDFISC
016700150616     c                   eval      NatGiur   = Out_SOGNATGIUR
016800090506      *
016900090506      * Eventuali mancanze di dati in Anagrafica
017000090522      *
017100090522     c                   If        Indirizzo = *blank
017200090506     c                   eval      ErrMsg = 'Manca l''indirizzo'
017300090522     c                   end
017400090522      *
017500090522     c                   If        Localita  = *blank
017600090506     c                   eval      ErrMsg = 'Manca la Località'
017700090522     c                   end
017800090522      *
017900090522     c                   If        CAP       = *blank
018000090506     c                   eval      ErrMsg = 'Manca il CAP'
018100090522     c                   end
018200090522      *
018300090522     c                   If        Provincia = *blank
018400090506     c                   eval      ErrMsg = 'Manca la provincia'
018500090522     c                   End
018600090522      *
018700090522     c                   if        CodiceFisc = *blank
018800090522     c                   eval      ErrMsg = 'Manca il Codice Fiscale'
018900090506     c                   End
019000090522      *
019100090522      *  Con segnalazione di errore
019200090522     c                   If        RagSOCKSC = *blank
019300090522     c                   eval      Errore = '2'
019400090522     c                   eval      ErrMsg = 'Manca la RagioneSociale'
019500090522     c                   End
019600090522      *
019700090522     c                   IF        PartitaIVA= *blank
019800090522     c                   eval      Errore = '2'
019900090522     c                   eval      ErrMsg = 'Manca la P.IVA'
020000090522     c                   end
020100090506      *
020200090506     c                   EndSL
020300090506     c*
020400040820
020500041007     c     fine          tag
020600080327     C                   SETON                                        RT
020700040820     C**************************************************************************
020800040820     C* INIZIALIZZAZIONE
020900040820     C**************************************************************************
021000080327     C     EntrySR       BEGSR
021100040820      **
021200040820     C     *ENTRY        PLIST
021300090506     C                   parm                    trmz70s4ds
021400090506     c* codice fornitore opzionale, se passato controllo solo quello
021500090506     c                   if        keyksc <> ' '
021600090506     c                   eval      daksc = keyksc
021700090506     c                   eval      aksc = keyksc
021800090506     c                   else
021900090506     c                   clear                   daksc
022000090506     c                   eval      aksc =*hival
022100090506     c                   end
022200090506      *
022300090506     c                   eval      Trovato = *off
022400080327      *
022500040820      *  imposta UDATE di elaborazione
022600040820     C                   move      *date         G02DAT
022700040820     C                   MOVE      *ZEROS        G02INV
022800040820     C                   MOVE      *BLANKS       G02ERR
022900040820     C                   CALL      'XSRDA8'
023000040820     C                   PARM                    WLBDAT
023100040820     C                   Z-ADD     G02INV        data_oggi
023200040820     C                   TIME                    W0140            14 0
023300040823      **
023400040823     c                   seton                                        66
023500040825     c                   clear                   prima_volta       1
023600040826     c                   clear                   almeno_uno
023700040820     c*
023800040820     C                   ENDSR
023900040820     C**************************************************************************
