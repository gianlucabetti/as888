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
002500090430     D  daksc          s                   like(keyksc)
002600090430     D  aksc           s                   like(keyksc)
002700040820      *
002800040820     D digits          C                   CONST('0123456789')
002900040820      *****************************************************************
003000080327     c                   exsr      EntrySR
003100080327
003200090316     c                   if        societa <> *blank and unita <> *blank
003300011210     C/EXEC SQL
003400090415     C+ SELECT
003500090415     C+   RCOKCC, RCOKSC, RCOSOCIETA, RCOUNITA,
003600090415     C+   SOGDES, SOGDESAGG, INDINDRIZ, INDLOCALIT,
003700150616     C+   INDCAP, INDPROV, INDSTATO, SOGCDFISC, SOGNATGIUR
003800090316     C+  INTO
003900090415     C+   :Out_RCOKCC, :Out_RCOKSC, :Out_RCOSOCIETA,
004000090415     C+   :Out_RCOUNITA,  :Out_SOGDES, :Out_SOGDESAGG,
004100090415     C+   :Out_INDINDRIZ, :Out_INDLOCALIT, :Out_INDCAP,
004200150616     C+   :Out_INDPROV, :Out_INDSTATO, :out_SOGCDFISC,
004300150616     C+   :Out_SOGNATGIUR
004400090316     C+  FROM ANRCO00F JOIN ANSOG00F ON RCOSOGG = SOGSOGG JOIN
004500090409     C+ ANIND00F ON INDSOGG = SOGSOGG and INDCDIND=' ' and
004600090409     c+  INDTPIND=' '
004700090316     C+  WHERE
004800090430     C+  RCOksc between :daksc and :aksc   and
004900090430     C+  RCOSNATURA = :Sottonatur   and
005000090316     c+  SOGPARTIVA = :PartitaIVA   and
005100090316     c+  RCOSOCIETA = :societa      and
005200090316     c+  RCOUNITA   = :unita
005300090408     c+  and  RCODTANN = '0001-01-01'
005400090408     c+  and  SOGDTFIVL= '0001-01-01'
005500090316     c+    ORDER BY RCODTIMM desc
005600090316     c+    FETCH FIRST  ROW ONLY
005700011210     C/END-EXEC
005800090316     c                   elseif    societa <> *blank and unita = *blank
005900090316     C/EXEC SQL
006000090415     C+ SELECT
006100090415     C+   RCOKCC, RCOKSC, RCOSOCIETA, RCOUNITA,
006200090415     C+   SOGDES, SOGDESAGG, INDINDRIZ, INDLOCALIT,
006300150616     C+   INDCAP, INDPROV, INDSTATO, SOGCDFISC, SOGNATGIUR
006400090415     C+  INTO
006500090415     C+   :Out_RCOKCC, :Out_RCOKSC, :Out_RCOSOCIETA,
006600090415     C+   :Out_RCOUNITA,  :Out_SOGDES, :Out_SOGDESAGG,
006700090415     C+   :Out_INDINDRIZ, :Out_INDLOCALIT, :Out_INDCAP,
006800150616     C+   :Out_INDPROV, :Out_INDSTATO, :out_SOGCDFISC,
006900150616     C+   :Out_SOGNATGIUR
007000090316     C+  FROM ANRCO00F JOIN ANSOG00F ON RCOSOGG = SOGSOGG JOIN
007100090409     C+ ANIND00F ON INDSOGG = SOGSOGG and INDCDIND=' ' and
007200090409     c+  INDTPIND=' '
007300090316     C+  WHERE
007400090430     C+  RCOksc between :daksc and :aksc   and
007500090408     C+  RCOSNATURA = :Sottonatur   and
007600090316     c+  SOGPARTIVA = :PartitaIVA   and
007700090316     c+  RCOSOCIETA = :societa
007800090408     c+  and  RCODTANN = '0001-01-01'
007900090408     c+  and  SOGDTFIVL= '0001-01-01'
008000090316     c+    ORDER BY RCODTIMM desc
008100090316     c+    FETCH FIRST  ROW ONLY
008200090316     C/END-EXEC
008300090316     c                   elseif    societa = *blank and unita <>*blank
008400090316     C/EXEC SQL
008500090415     C+ SELECT
008600090415     C+   RCOKCC, RCOKSC, RCOSOCIETA, RCOUNITA,
008700090415     C+   SOGDES, SOGDESAGG, INDINDRIZ, INDLOCALIT,
008800150616     C+   INDCAP, INDPROV, INDSTATO, SOGCDFISC, SOGNATGIUR
008900090415     C+  INTO
009000090415     C+   :Out_RCOKCC, :Out_RCOKSC, :Out_RCOSOCIETA,
009100090415     C+   :Out_RCOUNITA,  :Out_SOGDES, :Out_SOGDESAGG,
009200090415     C+   :Out_INDINDRIZ, :Out_INDLOCALIT, :Out_INDCAP,
009300150616     C+   :Out_INDPROV, :Out_INDSTATO, :out_SOGCDFISC,
009400150616     C+   :Out_SOGNATGIUR
009500090316     C+  FROM ANRCO00F JOIN ANSOG00F ON RCOSOGG = SOGSOGG JOIN
009600090409     C+ ANIND00F ON INDSOGG = SOGSOGG and INDCDIND=' ' and
009700090409     c+  INDTPIND=' '
009800090316     C+  WHERE
009900090430     C+  RCOksc between :daksc and :aksc   and
010000090408     C+  RCOSNATURA = :Sottonatur   and
010100090316     c+  SOGPARTIVA = :PartitaIVA   and
010200090316     c+ (RCOSOCIETA = :societa or :societa = '')  and
010300090316     c+ (RCOUNITA   = :unita   or :unita   = '')
010400090408     c+  and  RCODTANN = '0001-01-01'
010500090408     c+  and  SOGDTFIVL= '0001-01-01'
010600090316     c+    ORDER BY RCODTIMM desc
010700090316     c+    FETCH FIRST  ROW ONLY
010800090316     C/END-EXEC
010900090316     c                   elseif    societa = *blank and unita =*blank
011000090316     C/EXEC SQL
011100090415     C+ SELECT
011200090415     C+   RCOKCC, RCOKSC, RCOSOCIETA, RCOUNITA,
011300090415     C+   SOGDES, SOGDESAGG, INDINDRIZ, INDLOCALIT,
011400150616     C+   INDCAP, INDPROV, INDSTATO, SOGCDFISC, SOGNATGIUR
011500090415     C+  INTO
011600090415     C+   :Out_RCOKCC, :Out_RCOKSC, :Out_RCOSOCIETA,
011700090415     C+   :Out_RCOUNITA,  :Out_SOGDES, :Out_SOGDESAGG,
011800090415     C+   :Out_INDINDRIZ, :Out_INDLOCALIT, :Out_INDCAP,
011900150616     C+   :Out_INDPROV, :Out_INDSTATO, :out_SOGCDFISC,
012000150616     C+   :Out_SOGNATGIUR
012100090316     C+  FROM ANRCO00F JOIN ANSOG00F ON RCOSOGG = SOGSOGG JOIN
012200090409     C+ ANIND00F ON INDSOGG = SOGSOGG and INDCDIND=' ' and
012300090409     c+  INDTPIND=' '
012400090316     C+  WHERE
012500090430     C+  RCOksc between :daksc and :aksc   and
012600090408     C+  RCOSNATURA = :Sottonatur   and
012700090316     c+  SOGPARTIVA = :PartitaIVA
012800090408     c+  and  RCODTANN = '0001-01-01'
012900090408     c+  and  SOGDTFIVL= '0001-01-01'
013000090316     c+    ORDER BY RCODTIMM desc
013100090316     c+    FETCH FIRST  ROW ONLY
013200090316     C/END-EXEC
013300090316     c                   end
013400011210
013500080327      * esegue una sola lettura e quindi restituisce 1 solo record
013600080327      * quello immesso + di recente
013700011210     C                   SELECT
013800080327     **
013900080327     C                   WHEN      SqlCod < 0
014000090407     c                   eval      Errore = '9'
014100090407     c                   eval      ErrMsg = 'Problemi di SQL. SQLCOD = '
014200090407     c                   eval      ErrMsg = %trim(ErrMsg) + %EDITC(SQLCOD:'K')
014300080327     C                   dump(A)
014400080327     c                   goto      fine
014500080327     c* se Fine File --> EoF=ON
014600041007     C                   WHEN      SqlCod = 100
014700090317     c                   eval      Trovato = *off
014800090407     c                   eval      Errore = '1'
014900090407     c                   eval      ErrMsg = 'EoF: Non trovato nessun record +
015000090407     c                             SQLCOD = 100'
015100040820     c*
015200041007     C                   OTHER
015300080327      *
015400080327     c* Lettura del 1°record --> EoF=OFF
015500090317     c                   eval      Trovato = *on
015600090407     c                   eval      Errore = '0'
015700080327     c                   eval      Societa   = Out_RCOSOCIETA
015800080327     c                   eval      Unita     = Out_RCOUNITA
015900080327     c                   eval      KeyKCC    = Out_RCOKCC
016000080327     c                   eval      KeyKSC    = Out_RCOKSC
016100080327     c                   eval      RagSocKSC = Out_SOGDES
016200090407     c                   eval      RagSocKSC1= Out_SOGDESAGG
016300080331     c                   eval      Indirizzo = Out_INDINDRIZ
016400080331     c                   eval      Localita  = Out_INDLOCALIT
016500080331     c                   eval      CAP       = Out_INDCAP
016600080331     c                   eval      Provincia = Out_INDPROV
016700080331     c                   eval      Stato     = Out_INDSTATO
016800090408     c                   eval      CodiceFisc = Out_SOGCDFISC
016900150616     c                   eval      NatGiur   = Out_SOGNATGIUR
017000090407      *
017100090407      * Eventuali mancanze di dati in Anagrafica
017200090408     c                   if        CodiceFisc = *blank
017300090407     c                   eval      Errore = '2'
017400090407     c                   eval      ErrMsg = 'Manca il Codice Fiscale'
017500090407     c                   elseIf    Indirizzo = *blank
017600090407     c                   eval      Errore = '2'
017700090407     c                   eval      ErrMsg = 'Manca l''indirizzo'
017800090407     c                   elseIf    Localita  = *blank
017900090407     c                   eval      Errore = '2'
018000090407     c                   eval      ErrMsg = 'Manca la Località'
018100090407     c                   elseIf    CAP       = *blank
018200090407     c                   eval      Errore = '2'
018300090407     c                   eval      ErrMsg = 'Manca il CAP'
018400090407     c                   elseIf    Provincia = *blank
018500090407     c                   eval      Errore = '2'
018600090407     c                   eval      ErrMsg = 'Manca la provincia'
018700090407     c                   elseIf    RagSOCKSC = *blank
018800090407     c                   eval      Errore = '2'
018900090407     c                   eval      ErrMsg = 'Manca la RagioneSociale'
019000090407     c                   End
019100080327      *
019200080327     c                   EndSL
019300011205     c*
019400041007     c     fine          tag
019500080327     C                   SETON                                        RT
019600040820     C**************************************************************************
019700040820     C* INIZIALIZZAZIONE
019800040820     C**************************************************************************
019900080327     C     EntrySR       BEGSR
020000040820      **
020100040820     C     *ENTRY        PLIST
020200090408     C                   parm                    trmz70s1ds
020300090430     c* codice fornitore opzionale, se passato controllo solo quello
020400090430     c                   if        keyksc <> ' '
020500090430     c                   eval      daksc = keyksc
020600090430     c                   eval      aksc = keyksc
020700090430     c                   else
020800090430     c                   clear                   daksc
020900090430     c                   eval      aksc =*hival
021000090430     c                   end
021100090330      *
021200090330     c                   eval      Trovato = *off
021300080327      *
021400040820      *  imposta UDATE di elaborazione
021500040820     C                   move      *date         G02DAT
021600040820     C                   MOVE      *ZEROS        G02INV
021700040820     C                   MOVE      *BLANKS       G02ERR
021800040820     C                   CALL      'XSRDA8'
021900040820     C                   PARM                    WLBDAT
022000040820     C                   Z-ADD     G02INV        data_oggi
022100040820     C                   TIME                    W0140            14 0
022200040823      **
022300040823     c                   seton                                        66
022400040825     c                   clear                   prima_volta       1
022500040826     c                   clear                   almeno_uno
022600040820     c*
022700040820     C                   ENDSR
