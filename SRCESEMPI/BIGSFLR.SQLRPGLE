000100040405      *PARMS CLOSQLCSR(*ENDMOD)
000200040405
000300040401      /TITLE Gestione subfile con più di 9999 righe.
000400040401
000500040405     H ALWNULL(*INPUTONLY)
000600040401
000700040402     FBigSflD   CF   E             WORKSTN
000800040402     F                                     SFILE(S01:R)
000900040401     F                                     INFDS(InfDspF)
001000040401     F                                     INDDS(IndDspF)
001100040331     FQADBIFLD  IF   E             DISK
001200040401     F                                     USROPN
001300011220
001400040401     D RollDown        C                   X'F4'
001500040401     D RollUp          C                   X'F5'
001600040401
001700040401     D InfDspF         DS
001800040402     D  DSP_AID              369    369                                         Tasto funzionale
001900040401     D IndDspF         DS
002000040401     D  SflClr                 1      1N
002100040401     D  SflDsp                 2      2N
002200040401     D  SflEnd                 3      3N
002300040401     D FetchRow        DS
002400040402     D                                     INZ
002500040401     D                                     OCCURS(20)
002600040402     D  RRN                          10I 0                                      RRN di QADBIFLD
002700040401     D RowPag          DS            80
002800040402     D                                     INZ
002900040402     D  Row                          10I 0                                      Un elemento per riga
003000040401     D                                     DIM(20)
003100040402
003200040402     D Pag             S             80                                         Un elemento per pag.
003300040402     D                                     DIM(32767)
003400040402     D                                     BASED(pPag)
003500040402     D R               S              5I 0                                      Indice schiera
003600040402     D nSize           S             10I 0
003700040402     D pPag            S               *
003800011220
003900040402     ***********************************************************************************************
004000040402     **?
004100040422     **?Main.
004200040402     **?
004300040402     ***********************************************************************************************
004400040402     C                   EXSR      OpenCursor
004500040402
004600040402     **?Carico la prima pagina.
004700040405     C                   EVAL      C01GoToPag = 1
004800040402     C                   EXSR      NuovaPagina
004900040402     C                   EXSR      WrtSubFile
005000040402
005100040402     C                   WRITE     F01
005200040401
005300040402     C                   DO        *HIVAL
005400040402
005500040405     C                   CLEAR                   C01GoToPag
005600040402
005700040422     **?Attivo il subfile end e disattivo rollup se sono a fine file oppure
005800040422     **?oppure se sto visualizzando l'ultima pagina oppure se ho raggiunto la capienza.
005900040405     C                   EVAL      SflEnd = (C01CurrPag = C01LastPag AND
006000040402     C                             ((SQLCod = 100 OR SQLEr5 = 100)
006100040405     C                               OR C01LastPag = %ELEM(Pag)))
006200040402
006300040402     **?Visualizzo la pagina.
006400040402     C                   EXFMT     C01
006500040402
006600040402     C                   SELECT
006700040402     **?F3=Fine.
006800040402     C                   WHEN      *INKC
006900040402     C                   LEAVE
007000040402     **?ROLLUP Carico la pagina seguente, nuova o già memorizzata.
007100040402     C                   WHEN      DSP_AID = RollUp
007200040405     C                   IF        C01CurrPag = C01LastPag
007300040405     C                   EVAL      C01GoToPag = C01LastPag + 1
007400040402     C                   EXSR      NuovaPagina
007500040402     C                   EXSR      WrtSubFile
007600040402     C                   ELSE
007700040405     C                   EVAL      C01GoToPag = C01CurrPag + 1
007800040402     C                   EXSR      WrtSubFile
007900040402     C                   ENDIF
008000040402     **?ROLLDOWN Carico la pagina precedente.
008100040402     C                   WHEN      DSP_AID = RollDown
008200040405     C                   IF        C01CurrPag > 1
008300040405     C                   EVAL      C01GoToPag = C01CurrPag - 1
008400040402     C                   EXSR      WrtSubFile
008500040402     C                   ENDIF
008600040402     **?INVIO Vado alla pagina richiesta.
008700040402     C                   OTHER
008800040405     C                   IF        C01GoToPag > 0
008900040405     C                   IF        C01GoToPag > C01LastPag
009000040405     C                   EXSR      NuovaPagina
009100040402     C                   ENDIF
009200040402     C                   EXSR      WrtSubFile
009300040402     C                   ENDIF
009400040402     C
009500040402     C                   ENDSL
009600040401
009700040401     C                   ENDDO
009800040401
009900040405     C                   EXSR      Uscita
010000040401
010100040401     ***********************************************************************************************
010200040401     **?
010300040402     **?Carico una nuova pagina.
010400040401     **?
010500040401     ***********************************************************************************************
010600040402     C     NuovaPagina   BEGSR
010700040401
010800040405     C                   EVAL      C01CurrPag = C01LastPag
010900040405
011000040405     C                   DO        *HIVAL
011100040405
011200040422     **?Leggo 20 nuovi records.
011300040402     C                   EXSR      ReadCursor
011400040405
011500040405     C                   IF        SQLCod = 100
011600040405     C                   LEAVE
011700040405     C                   ENDIF
011800040402
011900040401     **?Memorizzo nella schiera ROW i RRN del file con cui scriverò le righe della pagina.
012000040402     C                   CLEAR                   Row
012100040401     C                   DO        SQLEr3        R
012200040401     C     R             OCCUR     FetchRow
012300040401     C                   EVAL      Row(R) = RRN
012400040401     C                   ENDDO
012500040405
012600040405     C                   EVAL      C01CurrPag = C01CurrPag + 1
012700040402
012800040422     **?Alloco la memoria necessaria per memorizzare il nuovo elemento.
012900040405     C                   EVAL      nSize = %SIZE(Pag) * C01CurrPag
013000040421     C                   IF        C01CurrPag = 1
013100040405     C                   ALLOC     nSize         pPag                           Prima allocazione
013200040402     C                   ELSE
013300040405     C                   REALLOC   nSize         pPag                           Successive allocaz.
013400040402     C                   ENDIF
013500040402
013600040402     **?Memorizzo la pagina.
013700040405     C                   EVAL      Pag(C01CurrPag) = RowPag
013800040405
013900040422     **?Fermo il caricamento di nuove pagine se ho raggiunto la pagina richiesta
014000040422     **?oppure se i records sono finiti oppure se ho raggiunto la capienza massima.
014100040405     C                   IF        C01CurrPag = C01GoToPag OR SQLEr5 = 100
014200040405     C                             OR C01CurrPag = %ELEM(Pag)
014300040405     C                   LEAVE
014400040405     C                   ENDIF
014500040405
014600040405     C                   ENDDO
014700040405
014800040405     C                   EVAL      C01LastPag = C01CurrPag
014900040405     C                   EVAL      C01GoToPag = C01CurrPag
015000040401
015100040401     C                   ENDSR
015200040401
015300040401     ***********************************************************************************************
015400040401     **?
015500040402     **?Scrittura subfile.
015600040401     **?
015700040401     ***********************************************************************************************
015800040402     C     WrtSubFile    BEGSR
015900040401
016000040402     **?Pulizia subfile.
016100040401     C                   EVAL      SflDsp = *OFF
016200040401     C                   EVAL      SflClr = *ON
016300040401     C                   WRITE     C01
016400040401     C                   EVAL      SflClr = *OFF
016500040401
016600040422     **?Reperisco i 20 RRN che compongono la pagina.
016700040405     C                   EVAL      RowPag = Pag(C01GoToPag)
016800040402
016900040422     **?La pagina è vuota.
017000040402     C                   IF        Row(1) = 0
017100040402     C
017200040402     C                   EVAL      SflDsp = *OFF
017300040405     C                   CLEAR                   C01CurrPag
017400040402     C
017500040422     **?La pagina ha almeno una riga.
017600040402     C                   ELSE
017700040402     C
017800040402     C                   EVAL      SflDsp = *ON
017900040405     C                   EVAL      C01CurrPag = C01GoToPag
018000040401     C                   OPEN      QADBIFLD
018100040422     **?Scrivo le 20 righe del subfile.
018200040401     C                   DO        20            R
018300040401     C                   IF        Row(R) = 0
018400040401     C                   LEAVE
018500040401     C                   ENDIF
018600040422     **?Completo le righe con i dati e scrivo il subfile.
018700040401     C     Row(R)        CHAIN     QADBIFLD
018800040402     C                   EVAL      S01DBITXT = DBITXT
018900040405     C                   EVAL      S01Riga = (C01CurrPag * 20) - (20 - R)
019000040401     C                   EVAL      S01RRNFld = Row(R)
019100040401     C                   WRITE     S01
019200040401     C                   ENDDO
019300040405     C
019400040401     C                   CLOSE     QADBIFLD
019500040402     C
019600040402     C                   ENDIF
019700040401
019800040401     C                   ENDSR
019900040402
020000040402     ***********************************************************************************************
020100040402     **?
020200040402     **?Apertura cursore.
020300040402     **?
020400040402     ***********************************************************************************************
020500040402     C     OpenCursor    BEGSR
020600040402
020700040402     C/EXEC SQL
020800040402     C+ DECLARE QADBIFLD CURSOR FOR SELECT RRN(QADBIFLD) FROM QADBIFLD
020900040506     C+ FOR READ ONLY OPTIMIZE FOR 20 ROWS
021000040402     C/END-EXEC
021100040402
021200040402     C/EXEC SQL
021300040402     C+ OPEN QADBIFLD
021400040402     C/END-EXEC
021500040402
021600040402     C                   ENDSR
021700040402
021800040402     ***********************************************************************************************
021900040402     **?
022000040402     **?Lettura cursore.
022100040402     **?
022200040402     ***********************************************************************************************
022300040402     C     ReadCursor    BEGSR
022400040402
022500040402     C/EXEC SQL
022600040402     C+ FETCH NEXT FROM QADBIFLD FOR 20 ROWS INTO :FetchRow
022700040402     C/END-EXEC
022800040402
022900040402     C                   ENDSR
023000040402
023100040402     ***********************************************************************************************
023200040402     **?
023300040402     **?Chiusura cursore.
023400040402     **?
023500040402     ***********************************************************************************************
023600040402     C     CloseCursor   BEGSR
023700040402
023800040402     C/EXEC SQL
023900040402     C+ CLOSE QADBIFLD
024000040402     C/END-EXEC
024100040402
024200040402     C                   ENDSR
024300040405
024400040405     ***********************************************************************************************
024500040405     **?
024600040405     **?Uscita.
024700040405     **?
024800040405     ***********************************************************************************************
024900040405     C     Uscita        BEGSR
025000040405
025100040405     C                   EXSR      CloseCursor
025200040405     C                   DEALLOC(N)              pPag
025300040405     C                   EVAL      *INLR = *ON
025400040405     C                   RETURN
025500040405
025600040405     C                   ENDSR
