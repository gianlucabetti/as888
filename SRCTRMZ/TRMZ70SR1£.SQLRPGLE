000100080327     H**********************************************************************
000200040820     H DECEDIT('0,') DATEDIT(*dmy.)
000300080327     H**********************************************************************
000400080327      *  Utility per decodificare con una PARTITA IVA su PROJ i dati
000500080327      *  del FORNITORE
000600080327      *  basandosi sulla società e sull'unità preferenziale passate come
000700080327      *  parametri.
000800080327      *  Legge la più recente x data immissione e restituisce un record JOIN
000900080327      *  di ANRCO e ANSOG.
001000020409     H**********************************************************************
001100080327     D AnRCO00f      e DS                  prefix(Out_)
001200080327     D AnSOG00f      e DS                  prefix(Out_)
001300080331     D AnIND00f      e DS                  prefix(Out_)
001400040820     D*
001500040820     D WLBDAT          DS
001600040820     D  G02DAT                 1      8  0
001700040820     D  G02INV                 9     16  0
001800040820     D  G02ERR                17     17
001900040820     D  G02TGI                18     22  0
002000080327      *
002100090408     D trmz70s1ds    e DS
002200080327      *
002300040820     D  data_Oggi      s                   like(G02INV) INZ(0)
002400040826     D  almeno_uno     s              1
002401090430     D  daksc          s                   like(keyksc)
002402090430     D  aksc           s                   like(keyksc)
002500040820      *
002600040820     D digits          C                   CONST('0123456789')
002700040820      *****************************************************************
002800080327     c                   exsr      EntrySR
002900080327
003000090316     c                   if        societa <> *blank and unita <> *blank
003100011210     C/EXEC SQL
003200090415     C+ SELECT
003300090415     C+   RCOKCC, RCOKSC, RCOSOCIETA, RCOUNITA,
003400090415     C+   SOGDES, SOGDESAGG, INDINDRIZ, INDLOCALIT,
003500090415     C+   INDCAP, INDPROV, INDSTATO, SOGCDFISC
003600090316     C+  INTO
003700090415     C+   :Out_RCOKCC, :Out_RCOKSC, :Out_RCOSOCIETA,
003800090415     C+   :Out_RCOUNITA,  :Out_SOGDES, :Out_SOGDESAGG,
003900090415     C+   :Out_INDINDRIZ, :Out_INDLOCALIT, :Out_INDCAP,
004000090415     C+   :Out_INDPROV, :Out_INDSTATO, :out_SOGCDFISC
004100090316     C+  FROM ANRCO00F JOIN ANSOG00F ON RCOSOGG = SOGSOGG JOIN
004200090409     C+ ANIND00F ON INDSOGG = SOGSOGG and INDCDIND=' ' and
004300090409     c+  INDTPIND=' '
004400090316     C+  WHERE
004500090430     C+  RCOksc between :daksc and :aksc   and
004501090430     C+  RCOSNATURA = :Sottonatur   and
004600090316     c+  SOGPARTIVA = :PartitaIVA   and
004700090316     c+  RCOSOCIETA = :societa      and
004800090316     c+  RCOUNITA   = :unita
004900090408     c+  and  RCODTANN = '0001-01-01'
005000090408     c+  and  SOGDTFIVL= '0001-01-01'
005100090316     c+    ORDER BY RCODTIMM desc
005200090316     c+    FETCH FIRST  ROW ONLY
005300011210     C/END-EXEC
005400090316     c                   elseif    societa <> *blank and unita = *blank
005500090316     C/EXEC SQL
005600090415     C+ SELECT
005700090415     C+   RCOKCC, RCOKSC, RCOSOCIETA, RCOUNITA,
005800090415     C+   SOGDES, SOGDESAGG, INDINDRIZ, INDLOCALIT,
005900090415     C+   INDCAP, INDPROV, INDSTATO, SOGCDFISC
006000090415     C+  INTO
006100090415     C+   :Out_RCOKCC, :Out_RCOKSC, :Out_RCOSOCIETA,
006200090415     C+   :Out_RCOUNITA,  :Out_SOGDES, :Out_SOGDESAGG,
006300090415     C+   :Out_INDINDRIZ, :Out_INDLOCALIT, :Out_INDCAP,
006400090415     C+   :Out_INDPROV, :Out_INDSTATO, :out_SOGCDFISC
006500090316     C+  FROM ANRCO00F JOIN ANSOG00F ON RCOSOGG = SOGSOGG JOIN
006600090409     C+ ANIND00F ON INDSOGG = SOGSOGG and INDCDIND=' ' and
006700090409     c+  INDTPIND=' '
006800090316     C+  WHERE
006801090430     C+  RCOksc between :daksc and :aksc   and
006900090408     C+  RCOSNATURA = :Sottonatur   and
007000090316     c+  SOGPARTIVA = :PartitaIVA   and
007100090316     c+  RCOSOCIETA = :societa
007200090408     c+  and  RCODTANN = '0001-01-01'
007300090408     c+  and  SOGDTFIVL= '0001-01-01'
007400090316     c+    ORDER BY RCODTIMM desc
007500090316     c+    FETCH FIRST  ROW ONLY
007600090316     C/END-EXEC
007700090316     c                   elseif    societa = *blank and unita <>*blank
007800090316     C/EXEC SQL
007900090415     C+ SELECT
008000090415     C+   RCOKCC, RCOKSC, RCOSOCIETA, RCOUNITA,
008100090415     C+   SOGDES, SOGDESAGG, INDINDRIZ, INDLOCALIT,
008200090415     C+   INDCAP, INDPROV, INDSTATO, SOGCDFISC
008300090415     C+  INTO
008400090415     C+   :Out_RCOKCC, :Out_RCOKSC, :Out_RCOSOCIETA,
008500090415     C+   :Out_RCOUNITA,  :Out_SOGDES, :Out_SOGDESAGG,
008600090415     C+   :Out_INDINDRIZ, :Out_INDLOCALIT, :Out_INDCAP,
008700090415     C+   :Out_INDPROV, :Out_INDSTATO, :out_SOGCDFISC
008800090316     C+  FROM ANRCO00F JOIN ANSOG00F ON RCOSOGG = SOGSOGG JOIN
008900090409     C+ ANIND00F ON INDSOGG = SOGSOGG and INDCDIND=' ' and
009000090409     c+  INDTPIND=' '
009100090316     C+  WHERE
009101090430     C+  RCOksc between :daksc and :aksc   and
009200090408     C+  RCOSNATURA = :Sottonatur   and
009300090316     c+  SOGPARTIVA = :PartitaIVA   and
009400090316     c+ (RCOSOCIETA = :societa or :societa = '')  and
009500090316     c+ (RCOUNITA   = :unita   or :unita   = '')
009600090408     c+  and  RCODTANN = '0001-01-01'
009700090408     c+  and  SOGDTFIVL= '0001-01-01'
009800090316     c+    ORDER BY RCODTIMM desc
009900090316     c+    FETCH FIRST  ROW ONLY
010000090316     C/END-EXEC
010100090316     c                   elseif    societa = *blank and unita =*blank
010200090316     C/EXEC SQL
010300090415     C+ SELECT
010400090415     C+   RCOKCC, RCOKSC, RCOSOCIETA, RCOUNITA,
010500090415     C+   SOGDES, SOGDESAGG, INDINDRIZ, INDLOCALIT,
010600090415     C+   INDCAP, INDPROV, INDSTATO, SOGCDFISC
010700090415     C+  INTO
010800090415     C+   :Out_RCOKCC, :Out_RCOKSC, :Out_RCOSOCIETA,
010900090415     C+   :Out_RCOUNITA,  :Out_SOGDES, :Out_SOGDESAGG,
011000090415     C+   :Out_INDINDRIZ, :Out_INDLOCALIT, :Out_INDCAP,
011100090415     C+   :Out_INDPROV, :Out_INDSTATO, :out_SOGCDFISC
011200090316     C+  FROM ANRCO00F JOIN ANSOG00F ON RCOSOGG = SOGSOGG JOIN
011300090409     C+ ANIND00F ON INDSOGG = SOGSOGG and INDCDIND=' ' and
011400090409     c+  INDTPIND=' '
011500090316     C+  WHERE
011501090430     C+  RCOksc between :daksc and :aksc   and
011600090408     C+  RCOSNATURA = :Sottonatur   and
011700090316     c+  SOGPARTIVA = :PartitaIVA
011800090408     c+  and  RCODTANN = '0001-01-01'
011900090408     c+  and  SOGDTFIVL= '0001-01-01'
012000090316     c+    ORDER BY RCODTIMM desc
012100090316     c+    FETCH FIRST  ROW ONLY
012200090316     C/END-EXEC
012300090316     c                   end
012400011210
012500080327      * esegue una sola lettura e quindi restituisce 1 solo record
012600080327      * quello immesso + di recente
012700011210     C                   SELECT
012800080327     **
012900080327     C                   WHEN      SqlCod < 0
013000090407     c                   eval      Errore = '9'
013100090407     c                   eval      ErrMsg = 'Problemi di SQL. SQLCOD = '
013200090407     c                   eval      ErrMsg = %trim(ErrMsg) + %EDITC(SQLCOD:'K')
013300080327     C                   dump(A)
013400080327     c                   goto      fine
013500080327     c* se Fine File --> EoF=ON
013600041007     C                   WHEN      SqlCod = 100
013700090317     c                   eval      Trovato = *off
013800090407     c                   eval      Errore = '1'
013900090407     c                   eval      ErrMsg = 'EoF: Non trovato nessun record +
014000090407     c                             SQLCOD = 100'
014100040820     c*
014200041007     C                   OTHER
014300080327      *
014400080327     c* Lettura del 1°record --> EoF=OFF
014500090317     c                   eval      Trovato = *on
014600090407     c                   eval      Errore = '0'
014700080327     c                   eval      Societa   = Out_RCOSOCIETA
014800080327     c                   eval      Unita     = Out_RCOUNITA
014900080327     c                   eval      KeyKCC    = Out_RCOKCC
015000080327     c                   eval      KeyKSC    = Out_RCOKSC
015100080327     c                   eval      RagSocKSC = Out_SOGDES
015200090407     c                   eval      RagSocKSC1= Out_SOGDESAGG
015300080331     c                   eval      Indirizzo = Out_INDINDRIZ
015400080331     c                   eval      Localita  = Out_INDLOCALIT
015500080331     c                   eval      CAP       = Out_INDCAP
015600080331     c                   eval      Provincia = Out_INDPROV
015700080331     c                   eval      Stato     = Out_INDSTATO
015800090408     c                   eval      CodiceFisc = Out_SOGCDFISC
015900090407      *
016000090407      * Eventuali mancanze di dati in Anagrafica
016100090408     c                   if        CodiceFisc = *blank
016200090407     c                   eval      Errore = '2'
016300090407     c                   eval      ErrMsg = 'Manca il Codice Fiscale'
016400090407     c                   elseIf    Indirizzo = *blank
016500090407     c                   eval      Errore = '2'
016600090407     c                   eval      ErrMsg = 'Manca l''indirizzo'
016700090407     c                   elseIf    Localita  = *blank
016800090407     c                   eval      Errore = '2'
016900090407     c                   eval      ErrMsg = 'Manca la Località'
017000090407     c                   elseIf    CAP       = *blank
017100090407     c                   eval      Errore = '2'
017200090407     c                   eval      ErrMsg = 'Manca il CAP'
017300090407     c                   elseIf    Provincia = *blank
017400090407     c                   eval      Errore = '2'
017500090407     c                   eval      ErrMsg = 'Manca la provincia'
017600090407     c                   elseIf    RagSOCKSC = *blank
017700090407     c                   eval      Errore = '2'
017800090407     c                   eval      ErrMsg = 'Manca la RagioneSociale'
017900090407     c                   End
018000080327      *
018100080327     c                   EndSL
018200011205     c*
018300041007     c     fine          tag
018400080327     C                   SETON                                        RT
018500040820     C**************************************************************************
018600040820     C* INIZIALIZZAZIONE
018700040820     C**************************************************************************
018800080327     C     EntrySR       BEGSR
018900040820      **
019000040820     C     *ENTRY        PLIST
019100090408     C                   parm                    trmz70s1ds
019101090430     c* codice fornitore opzionale, se passato controllo solo quello
019102090430     c                   if        keyksc <> ' '
019103090430     c                   eval      daksc = keyksc
019104090430     c                   eval      aksc = keyksc
019105090430     c                   else
019106090430     c                   clear                   daksc
019107090430     c                   eval      aksc =*hival
019108090430     c                   end
019200090330      *
019300090330     c                   eval      Trovato = *off
019400080327      *
019500040820      *  imposta UDATE di elaborazione
019600040820     C                   move      *date         G02DAT
019700040820     C                   MOVE      *ZEROS        G02INV
019800040820     C                   MOVE      *BLANKS       G02ERR
019900040820     C                   CALL      'XSRDA8'
020000040820     C                   PARM                    WLBDAT
020100040820     C                   Z-ADD     G02INV        data_oggi
020200040820     C                   TIME                    W0140            14 0
020300040823      **
020400040823     c                   seton                                        66
020500040825     c                   clear                   prima_volta       1
020600040826     c                   clear                   almeno_uno
020700040820     c*
020800040820     C                   ENDSR
