000100051007      *PARMS DYNUSRPRF(*OWNER) CLOSQLCSR(*ENDMOD)
000200051005      *PARMS
000300051005      *PARMS
000400051005      *PARMS
000500051005      *PARMS
000600051005
000700051010      /TITLE Dettaglio disposizioni ricevute e presa visione.
000800051005
000900051005     H DFTACTGRP(*NO) ACTGRP(*CALLER)
001000051005
001100051005     Ffnlg9640d CF   E             WORKSTN
001200051005     F                                     INFDS(infDspF)
001300051005     F                                     INDDS(indDspF)
001400051005     F                                     SFILE(s01:s01rrn)
001500051010     Ftabel00f  IF   E           K DISK
001600051005
001700051005     ***************************************************************************
001800051005     **?
001900051005     **?Definizione costanti.
002000051005     **?
002100051005     ***************************************************************************
002200051005     D Eseguito        C                   '0'
002300051005     D Errore          C                   '1'
002400051005     D F3              C                   X'33'
002500051005     D F6              C                   X'36'
002600051011     D F7              C                   X'37'
002700051011     D F12             C                   X'3C'
002800051011     D F14             C                   X'B2'
002900051005     D Enter           C                   X'F1'
003000051010     D NonVisionate    C                   'N'
003100051010     D GiaVisionate    C                   'V'
003200051010     D Scartate        C                   'S'
003300051010     D Caricate        C                   'C'
003400051011     D Partenza        C                   'P'
003500051011
003600051011     ***************************************************************************
003700051011     **?
003800051011     **?Definizione procedure.
003900051011     **?
004000051011     ***************************************************************************
004100051011     D fnls05r         PR
004200051011     D                                     EXTPGM('FNLS05R')
004300051011     D  a
004400051011     D                                     LIKE(kpjba)
004500051011     D  a
004600051011     D                                     LIKE(fidna1ds)
004700051011     D fnlg31r         PR
004800051011     D                                     EXTPGM('FNLG31R')
004900051011     D  a
005000051011     D                                     LIKE(kpjba)
005100051005
005200051005     ***************************************************************************
005300051005     **?
005400051005     **?Definizione strutture dati.
005500051005     **?
005600051005     ***************************************************************************
005700051005     D indDspF         DS
005800051005     D  sflDsp                 1      1N
005900051005     D  sflClr                 2      2N
006000051005     D  sflEnd                 3      3N
006100051005     D infDspF         DS
006200051005     D  dsp_aid              369    369
006300051005     D  sf_rrn               376    377I 0
006400051005     D kpjba         E DS
006500051010     D fnlg9610s     E DS
006600051010     D                                     PREFIX(Flt)
006700051005     D fnlg9640s     E DS
006800051005     D                                     INZ
006900051005     D                                     PREFIX(dtg)
007000051005     D fnvar10s      E DS
007100051005     D                                     BASED(nullPtr)
007200051010     D ds2n          E DS
007300051010     D                                     INZ
007400051011     D fnlg30ds      E DS
007500051011     D                                     INZ
007600051011     D  d30poa       E
007700051011     D                                     INZ(Partenza)
007800051011     D fidna1ds      E DS
007900051011     D                                     INZ
008000051011     D param0          DS
008100051011     D                                     INZ
008200051011     D  pa0aas                 1      4  0
008300051011     D  pa0lnp                 5      7  0
008400051011     D  pa0nrs                 8      9  0
008500051011     D  pa0nsp                10     16  0
008600051011     D  pa0f03                17     17
008700051011     D  pa0flg                18     18
008800051011     D  pa0a9                 19     19
008900051011     D  pa0B5                 20     20
009000051011     D  pa075                 21     21
009100051011     D  pa098                 22     22
009200051011     D  pa0rsu                23     42
009300051011     D  pa0cml                43     43
009400051011     D  pa0sim                44     46  0
009500051011     D  pa0cli                47     47
009600051011     D  pa0gia                48     48
009700051005
009800051005     ***************************************************************************
009900051005     **?
010000051005     **?Definizione variabili.
010100051005     **?
010200051005     ***************************************************************************
010300051005     D nullPtr         S               *
010400051005     D s01rrn          S              5I 0
010500051010     D rowDrd          S
010600051010     D                                     LIKE(varDrd)
010700051007     D wrkDtIso        S               D
010800051010     D wrkSqlStm       S           4096
010900051010     D                                     VARYING
011000051010     D wrkDrdd         S
011100051010     D                                     LIKE(varDrd)
011200051010     D wrkDrda         S
011300051010     D                                     LIKE(varDrd)
011400051011     D wrkOpCode       S             10
011500051011     D                                     INZ('DSPDETTAGL')
011600051011      /COPY QSYSINC/QRPGLESRC,QUSEC
011700051011     D LngTx01         S            512
011800051011     D LngTx02         S             10I 0
011900051011     D                                     INZ(%SIZE(LngTx01))
012000051011     D LngTx03         S              7
012100051011     D                                     INZ('BAR0019')
012200051011     D LngTx04         S             20
012300051011     D                                     INZ('YBARMSG   *LIBL')
012400051005
012500051005     ***************************************************************************
012600051005     **?
012700051005     **?Definizione parametri procedura.
012800051005     **?
012900051005     ***************************************************************************
013000051005     C     *ENTRY        PLIST
013100051005     C                   PARM                    kpjba
013200051010     C                   PARM                    fnlg9610s
013300051010
013400051010     ***************************************************************************
013500051010     **?
013600051010     **?Definizione chiavi database.
013700051010     **?
013800051010     ***************************************************************************
013900051010     C     k03tbl00      KLIST
014000051010     C                   KFLD                    tblKut
014100051010     C                   KFLD                    tblCod
014200051010     C                   KFLD                    tblKey
014300051005
014400051005     ***************************************************************************
014500051005     **?
014600051005     **?Main
014700051005     **?
014800051005     ***************************************************************************
014900051005     C                   EXSR      inzProc
015000051011
015100051011     C                   DO        *HIVAL
015200051011     C                   SELECT
015300051011     C                   WHEN      wrkOpCode = 'DSPDETTAGL'
015400051011     C                   EXSR      dspDettaglio
015500051011     C                   WHEN      wrkOpCode = 'UPDVISTO'
015600051011     C                   EXSR      updVisto
015700051011     C                   WHEN      wrkOpCode = 'VISBOLLE'
015800051011     C                   EXSR      visBolle
015900051011     C                   WHEN      wrkOpCode = 'VISGIAC'
016000051011     C                   EXSR      visGiacenze
016100051011     C                   OTHER
016200051011     C                   LEAVE
016300051011     C                   ENDSL
016400051011     C                   ENDDO
016500051011
016600051005     C                   EXSR      uscita
016700051005
016800051005     ***************************************************************************
016900051005     **?
017000051005     **?Operazioni iniziali.
017100051005     **?
017200051005     ***************************************************************************
017300051005     C     *INZSR        BEGSR
017400051005
017500051010     C                   EVAL      tblKut = 1
017600051006
017700051005     C                   ENDSR
017800051005
017900051005     ***************************************************************************
018000051005     **?
018100051005     **?Inizializzazione procedura.
018200051005     **?
018300051005     ***************************************************************************
018400051005     C     inzProc       BEGSR
018500051005
018600051005     C                   EVAL      fnlg9640s = KPJBU
018700051005     C                   CLEAR                   dtgEsito
018800051011     C                   RESET                   wrkOpCode
018900051005
019000051005     C                   ENDSR
019100051005
019200051005     ***************************************************************************
019300051005     **?
019400051005     **?Gestione subfile C01.
019500051005     **?
019600051005     ***************************************************************************
019700051011     C     dspDettaglio  BEGSR
019800051005
019900051005     C                   EVAL      sflDsp = *OFF
020000051005     C                   EVAL      sflEnd = *OFF
020100051005     C                   EVAL      sflClr = *ON
020200051005     C                   WRITE     c01
020300051005     C                   EVAL      sflClr = *OFF
020400051005     C                   EXSR      wrtS01
020500051005     C                   EVAL      sflDsp = (s01rrn > 0)
020600051005     C                   EVAL      sflEnd = (s01rrn > 0)
020700051005     C                   EVAL      c01Aas = dtgAas
020800051005     C                   EVAL      c01Lnp = dtgLnp
020900051005     C                   EVAL      c01Nrs = dtgNrs
021000051005     C                   EVAL      c01Nsp = dtgNsp
021100051005     C                   EVAL      c01Agc = dtgAgc
021200051005     C                   EVAL      c01Fgc = dtgFgc
021300051005     C                   EVAL      c01Ngc = dtgNgc
021400051005     C                   WRITE     f01
021500051005     C                   EXFMT     c01
021600051005
021700051005     C                   EVAL      dtgTasto = dsp_aid
021800051011
021900051011     C                   SELECT
022000051011     C                   WHEN      dtgTasto = F3 OR dtgTasto = F12 OR
022100051011     C                             dtgTasto = Enter
022200051011     C                   EVAL      wrkOpCode = 'FINE'
022300051011     C                   WHEN      dtgTasto = F6
022400051011     C                   EVAL      wrkOpCode = 'UPDVISTO'
022500051011     C                   WHEN      dtgTasto = F7
022600051011     C                   EVAL      wrkOpCode = 'VISBOLLE'
022700051011     C                   WHEN      dtgTasto = F14
022800051011     C                   EVAL      wrkOpCode = 'VISGIAC'
022900051011     C                   ENDSL
023000051005
023100051005     C                   ENDSR
023200051005
023300051005     ***************************************************************************
023400051005     **?
023500051005     **?Selezione righe e scrittura subfile.
023600051005     **?
023700051005     ***************************************************************************
023800051005     C     wrtS01        BEGSR
023900051005
024000051005     C                   CLEAR                   s01rrn
024100051010
024200051010     **?Imposto la parte fissa dell'istruzione.
024300051010     C                   EVAL      wrkSqlStm   =
024400051010     C                             'SELECT VARNPR, VARDMC, VARFLG, -
024500051018     C                             VARDRD, VARORZ/100, VARESI FROM FNVAR10S'
024600051010     **?WHERE
024700051010     C                   EVAL      wrkSqlStm   = wrkSqlStm   +
024800051010     C                             ' WHERE VARVIS = ''S'''
024900051010     **?Selezione sulla presa visione.
025000051010     C                   SELECT
025100051010     C                   WHEN      fltPresaVisio = NonVisionate
025200051010     C                   EVAL      wrkSqlStm   = wrkSqlStm   +
025300051010     C                             ' AND VARFLG = '' '''
025400051010     C                   WHEN      fltPresaVisio = GiaVisionate
025500051010     C                   EVAL      wrkSqlStm   = wrkSqlStm   +
025600051010     C                             ' AND VARFLG = ''S'''
025700051010     C                   ENDSL
025800051010     **?Selezione sull'esito ricezione.
025900051010     C                   SELECT
026000051010     C                   WHEN      fltEsitoRicez = Scartate
026100051010     C                   EVAL      wrkSqlStm   = wrkSqlStm   +
026200051010     C                             ' AND VARESI <> ''GOK'''
026300051010     C                   WHEN      fltEsitoRicez = Caricate
026400051010     C                   EVAL      wrkSqlStm   = wrkSqlStm   +
026500051010     C                             ' AND VARESI = ''GOK'''
026600051010     C                   ENDSL
026700051010     **?Selezione sulla data ricezione.
026800051010     C                   IF        fltDrdd <> *LOVAL OR fltDrda <> *HIVAL
026900051010     C                   MOVE      fltDrdd       wrkDrdd
027000051010     C                   MOVE      fltDrda       wrkDrda
027100051010     C                   EVAL      wrkSqlStm   = wrkSqlStm   +
027200051010     C                             ' AND VARDRD BETWEEN ' +
027300051010     C                             %EDITC(wrkDrdd:'X') + ' AND ' +
027400051010     C                             %EDITC(wrkDrdd:'X')
027500051010     C                   ENDIF
027600051010     **?Selezione sull'anno giacenza.
027700051010     C                   EVAL      wrkSqlStm   = wrkSqlStm   +
027800051010     C                             ' AND VARAGC = ' + %TRIML(%EDITC(dtgAgc:'Z'))
027900051010     **?Selezione sul PO giacenza.
028000051010     C                   EVAL      wrkSqlStm   = wrkSqlStm   +
028100051010     C                             ' AND VARFGC = ' + %TRIML(%EDITC(dtgFgc:'Z'))
028200051010     **?Selezione sul n. giacenza.
028300051010     C                   EVAL      wrkSqlStm   = wrkSqlStm   +
028400051010     C                             ' AND VARNGC = ' + %TRIML(%EDITC(dtgNgc:'Z'))
028500051010     **?Selezione sull'anno spedizione.
028600051010     C                   EVAL      wrkSqlStm   = wrkSqlStm   +
028700051010     C                             ' AND VARAAS = ' + %TRIML(%EDITC(dtgAas:'Z'))
028800051010     **?Selezione sul PO spedizione.
028900051010     C                   EVAL      wrkSqlStm   = wrkSqlStm   +
029000051010     C                             ' AND VARLNP = ' + %TRIML(%EDITC(dtgLnp:'Z'))
029100051010     **?Selezione sulla serie spedizione.
029200051010     C                   EVAL      wrkSqlStm   = wrkSqlStm   +
029300051010     C                             ' AND VARNRS = ' + %TRIML(%EDITC(dtgNrs:'X'))
029400051010     **?Selezione sul n. spedizione.
029500051010     C                   EVAL      wrkSqlStm   = wrkSqlStm   +
029600051010     C                             ' AND VARNSP = ' + %TRIML(%EDITC(dtgNsp:'Z'))
029700051010     **?Solo lettura.
029800051010     C                   EVAL      wrkSqlStm   = wrkSqlStm   + ' FOR READ ONLY'
029900051010
030000051010     C/EXEC SQL
030100051010     C+ PREPARE FNVAR10S FROM :wrkSqlStm
030200051010     C/END-EXEC
030300051010
030400051010     C/EXEC SQL
030500051010     C+ DECLARE FNVAR10S CURSOR FOR FNVAR10S
030600051010     C/END-EXEC
030700051005
030800051005     C/EXEC SQL
030900051005     C+ OPEN FNVAR10S
031000051005     C/END-EXEC
031100051005
031200051005     C                   DO        *HIVAL
031300051005
031400051005     C/EXEC SQL
031500051007     C+ FETCH NEXT FROM FNVAR10S INTO :s01Npr, :s01Dmc, :s01Flg, :rowDrd,
031600051018     C+ :s01Orz, :s01Esi
031700051005     C/END-EXEC
031800051005
031900051005     C                   SELECT
032000051010     C
032100051005     C                   WHEN      sqlCod = 100
032200051005     C                   LEAVE
032300051010     C
032400051005     C                   WHEN      sqlCod < 0
032500051005     C                   EVAL      dtgEsito = Errore
032600051005     C                   LEAVE
032700051010     C
032800051005     C                   OTHER
032900051007     C                   MOVE      rowDrd        wrkDtIso
033000051007     C     *JOBRUN       MOVE      wrkDtIso      s01Drd
033100051010     C                   EVAL      tblCod = '2N'
033200051018     C                   EVAL      tblKey = s01Esi
033300051010     C     k03tbl00      CHAIN     tabel00f
033400051010     C                   IF        %FOUND
033500051010     C                   EVAL      ds2n = tblUni
033600051010     C                   ELSE
033700051010     C                   CLEAR                   ds2n
033800051010     C                   ENDIF
033900051005     C                   EVAL      s01rrn = s01rrn + 1
034000051005     C                   WRITE     s01
034100051010     C
034200051005     C                   ENDSL
034300051005
034400051005     C                   ENDDO
034500051005
034600051005     C/EXEC SQL
034700051005     C+ CLOSE FNVAR10S
034800051005     C/END-EXEC
034900051005
035000051005     C                   ENDSR
035100051005
035200051005     ***************************************************************************
035300051005     **?
035400051005     **?Visto.
035500051005     **?
035600051005     ***************************************************************************
035700051011     C     updVisto      BEGSR
035800051005
035900051010     **?Imposto la parte fissa dell'istruzione.
036000051010     C                   EVAL      wrkSqlStm   =
036100051010     C                             'UPDATE FNVAR10S SET VARFLG = ''S'''
036200051010     **?WHERE
036300051010     C                   EVAL      wrkSqlStm = wrkSqlStm +
036400051010     C                             ' WHERE VARVIS = ''S'' AND VARFLG = '' '''
036500051010     **?Selezione sulla presa visione.
036600051010     C                   SELECT
036700051010     C                   WHEN      fltPresaVisio = NonVisionate
036800051010     C                   EVAL      wrkSqlStm   = wrkSqlStm   +
036900051010     C                             ' AND VARFLG = '' '''
037000051010     C                   WHEN      fltPresaVisio = GiaVisionate
037100051010     C                   EVAL      wrkSqlStm   = wrkSqlStm   +
037200051010     C                             ' AND VARFLG = ''S'''
037300051010     C                   ENDSL
037400051010     **?Selezione sull'esito ricezione.
037500051010     C                   SELECT
037600051010     C                   WHEN      fltEsitoRicez = Scartate
037700051010     C                   EVAL      wrkSqlStm   = wrkSqlStm   +
037800051010     C                             ' AND VARESI <> ''GOK'''
037900051010     C                   WHEN      fltEsitoRicez = Caricate
038000051010     C                   EVAL      wrkSqlStm   = wrkSqlStm   +
038100051010     C                             ' AND VARESI = ''GOK'''
038200051010     C                   ENDSL
038300051010     **?Selezione sulla data ricezione.
038400051010     C                   IF        fltDrdd <> *LOVAL OR fltDrda <> *HIVAL
038500051010     C                   MOVE      fltDrdd       wrkDrdd
038600051010     C                   MOVE      fltDrda       wrkDrda
038700051010     C                   EVAL      wrkSqlStm   = wrkSqlStm   +
038800051010     C                             ' AND VARDRD BETWEEN ' +
038900051010     C                             %EDITC(wrkDrdd:'X') + ' AND ' +
039000051010     C                             %EDITC(wrkDrdd:'X')
039100051010     C                   ENDIF
039200051010     **?Selezione sull'anno giacenza.
039300051010     C                   EVAL      wrkSqlStm   = wrkSqlStm   +
039400051010     C                             ' AND VARAGC = ' + %TRIML(%EDITC(dtgAgc:'Z'))
039500051010     **?Selezione sul PO giacenza.
039600051010     C                   EVAL      wrkSqlStm   = wrkSqlStm   +
039700051010     C                             ' AND VARFGC = ' + %TRIML(%EDITC(dtgFgc:'Z'))
039800051010     **?Selezione sul n. giacenza.
039900051010     C                   EVAL      wrkSqlStm   = wrkSqlStm   +
040000051010     C                             ' AND VARNGC = ' + %TRIML(%EDITC(dtgNgc:'Z'))
040100051010     **?Selezione sull'anno spedizione.
040200051010     C                   EVAL      wrkSqlStm   = wrkSqlStm   +
040300051010     C                             ' AND VARAAS = ' + %TRIML(%EDITC(dtgAas:'Z'))
040400051010     **?Selezione sul PO spedizione.
040500051010     C                   EVAL      wrkSqlStm   = wrkSqlStm   +
040600051010     C                             ' AND VARLNP = ' + %TRIML(%EDITC(dtgLnp:'Z'))
040700051010     **?Selezione sulla serie spedizione.
040800051010     C                   EVAL      wrkSqlStm   = wrkSqlStm   +
040900051010     C                             ' AND VARNRS = ' + %TRIML(%EDITC(dtgNrs:'X'))
041000051010     **?Selezione sul n. spedizione.
041100051010     C                   EVAL      wrkSqlStm   = wrkSqlStm   +
041200051010     C                             ' AND VARNSP = ' + %TRIML(%EDITC(dtgNsp:'Z'))
041300051011
041400051011     C/EXEC SQL
041500051011     C+ SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED, READ WRITE
041600051011     C/END-EXEC
041700051010
041800051010     C/EXEC SQL
041900051011     C+ EXECUTE IMMEDIATE :wrkSqlStm
042000051011     C/END-EXEC
042100051011
042200051011     C                   IF        sqlCod < 0
042300051011     C/EXEC SQL
042400051011     C+ ROLLBACK
042500051011     C/END-EXEC
042600051011     C                   EVAL      LngTx01 = 'Aggiornamento non riuscito. -
042700051011     C                             Almeno una riga è impegnata da un altro -
042800051011     C                             lavoro. Riprovare l''aggiornamento.'
042900051011     C                   EXSR      CalQUILNGTX
043000051011     C                   EVAL      wrkOpCode = 'DSPDETTAGL'
043100051011     C                   ELSE
043200051011     C/EXEC SQL
043300051011     C+ COMMIT
043400051011     C/END-EXEC
043500051011     C                   EVAL      wrkOpCode = 'FINE'
043600051011     C                   ENDIF
043700051011
043800051011     C/EXEC SQL
043900051011     C+ SET TRANSACTION ISOLATION LEVEL NO COMMIT
044000051011     C/END-EXEC
044100051005
044200051005     C                   ENDSR
044300051011
044400051011     ***************************************************************************
044500051011     **?
044600051011     **?Interrogazione bolle partenza.
044700051011     **?
044800051011     ***************************************************************************
044900051011     C     visBolle      BEGSR
045000051011
045100051011     C                   RESET                   fidna1ds
045200051011     C                   RESET                   param0
045300051011     C                   EVAL      pa0aas = dtgAas
045400051011     C                   EVAL      pa0lnp = dtgLnp
045500051011     C                   EVAL      pa0nrs = dtgNrs
045600051011     C                   EVAL      pa0nsp = dtgNsp
045700051011     C                   EVAL      kpjbu = param0
045800051011     C                   CALLP     fnls05r(kpjba:fidna1ds)
045900051011     C                   EVAL      param0 = kpjbu
046000051011
046100051011     C                   EVAL      wrkOpCode = 'DSPDETTAGL'
046200051011
046300051011     C                   ENDSR
046400051011
046500051011     ***************************************************************************
046600051011     **?
046700051011     **?Interrogazione giacenze partenza.
046800051011     **?
046900051011     ***************************************************************************
047000051011     C     visGiacenze   BEGSR
047100051011
047200051011     C                   RESET                   fnlg30ds
047300051011     C                   EVAL      d30agc = dtgAgc
047400051011     C                   EVAL      d30fgc = dtgFgc
047500051011     C                   EVAL      d30ngc = dtgNgc
047600051011     C                   EVAL      d30aas = dtgAas
047700051011     C                   EVAL      d30lnp = dtgLnp
047800051011     C                   EVAL      d30nrs = dtgNrs
047900051011     C                   EVAL      d30nsp = dtgNsp
048000051011     C                   EVAL      kpjbu = fnlg30ds
048100051011     C                   CALLP     fnlg31r(kpjba)
048200051011     C                   EVAL      fnlg30ds = kpjbu
048300051011
048400051011     C                   EVAL      wrkOpCode = 'DSPDETTAGL'
048500051011
048600051011     C                   ENDSR
048700051011
048800051011     ***************************************************************************
048900051011     **?
049000051011     **?Chiamata a QUILNGTX.
049100051011     **?
049200051011     ***************************************************************************
049300051011     C     CalQUILNGTX   BEGSR
049400051011
049500051011     C                   CLEAR                   QUSEC
049600051011     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
049700051011
049800051011     C                   CALL      'QUILNGTX'
049900051011     C                   PARM                    LngTx01
050000051011     C                   PARM                    LngTx02
050100051011     C                   PARM                    LngTx03
050200051011     C                   PARM                    LngTx04
050300051011     C                   PARM                    QUSEC
050400051011
050500051011     C                   ENDSR
050600051005
050700051005     ***************************************************************************
050800051005     **?
050900051005     **?Uscita
051000051005     **?
051100051005     ***************************************************************************
051200051005     C     Uscita        BEGSR
051300051005
051400051007     C                   WRITE     frcDta
051500051005     C                   EVAL      kpjbu = fnlg9640s
051600051005     C                   RETURN
051700051005
051800051005     C                   ENDSR
