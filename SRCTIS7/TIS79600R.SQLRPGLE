000100040903      /TITLE Estrazione LdV visualizzate da internet per addebito.
000200040903     ***********************************************************************************************
000300040903     **?
000400040906     **?Questo programma si collega all' iSeries web server, legge il log delle richieste
000500040906     **?internet, seleziona quelle di visualizzazione immagine lettera di vettura e per ogni
000600090420     **?LdV visualizzata scrive un record in FNRIG00F per l' addebito nella successiva fattura.
000700040906     **?
000800040906     **?Per questo programma bisogna creare anche il pacchetto SQL su AS777.
000900040906     **?CRTSQLRPGI RDB(AS777) USER(VAS777) PASSWORD([Password]) DBGVIEW(*SOURCE)
001000040906     **?Il programma non può essere spostato in GAITRAOBJ ma deve essere ricompilato perchè il
001100040906     **?pacchetto SQL su AS777 deve essere rigenerato.
001200040906     **?L'utente che esegue il programma deve esistere anche su AS777.
001300040906     **?
001400040903     ***********************************************************************************************
001500040903
001600040903     H DFTACTGRP(*NO) BNDDIR('QC2LE')
001700040903
001800040903     FTnTbe01L  UF   E           K DISK
001900040903     F                                     COMMIT
002000090420     FFnRig00F  O    E             DISK
002100040903     F                                     COMMIT
002200040903
002300040903     D RecordAlreadyLocked...
002400040903     D                 C                   01218
002500040906     D Internet        C                   'I'
002600090420     D Nessuno         C                   'N'
002700090420     d tariffa         c                   'T'
002800040903
002900040903     D KPJBA         E DS
003000040903     D CnDizion      E DS
003100040903     D                                     BASED(NullPtr)
003200040903     D                                     PREFIX(Diz)
003300040906     D DLVA1         E DS
003400040903     D                                     INZ
003500040903
003600040903     D NullPtr         S               *
003700040906     D RowRpyMmt       S               Z                                        Momento risposta
003800040906     D RowKSU          S              8                                         Cliente unificante
003900040906     D RowLdvAAS       S                                                        Anno
004000040903     D                                     LIKE(Diz$AAS)
004100040906     D RowLdvLnP       S                                                        PO partenza
004200040903     D                                     LIKE(Diz$LnP)
004300040906     D RowLdvNrS       S                                                        Serie
004400040903     D                                     LIKE(Diz$NrS)
004500040906     D RowLdvNSp       S                                                        Numero spedizione
004600040903     D                                     LIKE(Diz$NSp)
004700040903     D WrkDtISO        S               D
004800040903     D WrkOrISO        S               T
004900040903     D WrkOr60         S              6  0
005000041129     D WrkAdesso       S               Z
005100040903
005200040903     D atoi            PR            10I 0
005300040903     D                                     EXTPROC('atoi')
005400040903     D  Char                           *
005500040903     D                                     VALUE
005600040903     D                                     OPTIONS(*STRING)
005700040903
005800040903     ***********************************************************************************************
005900040903     **?
006000040903     **?Dichiarazione parametri.
006100040903     **?
006200040903     ***********************************************************************************************
006300040903     C     *ENTRY        PLIST
006400040903     C                   PARM                    KPJBA
006500040903
006600040903     ***********************************************************************************************
006700040903     **?
006800040903     **?Dichiarazioni varie.
006900040903     **?
007000040903     ***********************************************************************************************
007100040903     C     K05Tbe01      KLIST
007200040903     C                   KFLD                    TbeCod
007300040903     C                   KFLD                    TbeKe1
007400040903     C                   KFLD                    TbeKe2
007500040903     C                   KFLD                    TbeLin
007600040903     C                   KFLD                    TbeSIf
007700040903
007800040903     ***********************************************************************************************
007900040903     **?
008000040903     **?Main.
008100040903     **?
008200040903     ***********************************************************************************************
008300040903     C                   EXSR      GetLogVAS
008400040903     C                   EXSR      Uscita
008500040903
008600040903     ***********************************************************************************************
008700040903     **?
008800040903     **?Punto di arrivo in caso di errore SQL.
008900040903     **?
009000040903     ***********************************************************************************************
009100040903     C     ErroreSQL     TAG
009200040903     C                   EVAL      *INH3 = *ON
009300040903     C                   EXSR      Uscita
009400040903
009500040903     ***********************************************************************************************
009600040903     **?
009700040903     **?Estrazione record da log VAS internet.
009800040903     **?
009900040903     ***********************************************************************************************
010000040903     C     GetLogVAS     BEGSR
010100040903
010200041129     C                   TIME                    WrkAdesso
010300040903     C                   EXSR      GetTbeLVA1
010400040903
010500040903     C/EXEC SQL
010600040903     C+ WHENEVER SQLERROR GOTO ErroreSQL
010700040903     C/END-EXEC
010800040903
010900040903     C/EXEC SQL
011000040903     C+ CONNECT TO AS777
011100040903     C/END-EXEC
011200040903
011300040903     C/EXEC SQL
011400040903     C+ DECLARE TIVTL10F CURSOR FOR SELECT VTLRPYMMT, VTLKSU, 2000 +
011500050617     C+ DEC(SUBSTR(VTLRQSDTA, 1, 2), 2), DEC(SUBSTR(VTLRQSDTA, 3, 3), 3),
011600050617     C+ DEC(SUBSTR(VTLRQSDTA, 6, 2), 2), DEC(SUBSTR(VTLRQSDTA, 8, 7), 7)
011700050617     C+ FROM STRATEBAR/TIVTL10F WHERE VTLKSU <> ' ' AND LEFT(VTLCLTIPAD,
011800050617     C+ 3) <> '10.' AND VTLTIP = 'LV' AND VTLRPYOPC = 'DONE' AND
011900050617     C+ VTLRPYMMT > :§LVAUEsMmt AND VTLRPYMMT < :WrkAdesso AND VTLRQSOPC
012000050617     C+ = 'RTVIMG' AND LEFT(VTLRPYDTA, 12) = '/tempimages/' AND
012100050617     C+ SUBSTR(VTLRPYDTA, 13) LIKE '%.jpg%' UNION ALL SELECT VTLRPYMMT,
012200050617     C+ VTLKSU, 2000 + DEC(SUBSTR(VTLRQSDTA, 41, 2), 2),
012300050617     C+ DEC(SUBSTR(VTLRQSDTA, 43, 3), 3), DEC(SUBSTR(VTLRQSDTA, 46, 2),
012400050617     C+ 2), DEC(SUBSTR(VTLRQSDTA, 48, 7), 7) FROM STRATEBAR/TIVTL10F
012500050617     C+ WHERE VTLKSU <> ' ' AND LEFT(VTLCLTIPAD, 3) <> '10.' AND VTLTIP =
012600050617     C+ 'LV' AND VTLRPYOPC = 'DONE' AND VTLRPYMMT > :§LVAUEsMmt AND
012700050617     C+ VTLRPYMMT < :WrkAdesso AND VTLRQSOPC = 'RTVIMGXML' AND
012800050617     C+ SUBSTR(VTLRPYDTA, 129, 12) = '/TempImages/' AND SUBSTR(VTLRPYDTA,
012900050617     C+ 141) LIKE '%.jpg%' ORDER BY VTLRPYMMT FOR READ ONLY
013000040903     C/END-EXEC
013100040903
013200040903     C/EXEC SQL
013300040903     C+ OPEN TIVTL10F
013400040903     C/END-EXEC
013500040903
013600040903     C                   DO        *HIVAL
013700040903
013800040903     C/EXEC SQL
013900040903     C+ FETCH NEXT FROM TIVTL10F INTO :RowRpyMmt, :RowKSU, :RowLdvAAS,
014000040903     C+ :RowLdvLnP, :RowLdvNrS, :RowLdvNSp
014100040903     C/END-EXEC
014200040903
014300040903     C                   SELECT
014400040903     C                   WHEN      SQLCod = 100
014500040903     C                   LEAVE
014600040903     C                   WHEN      SQLCod < 0
014700040903     C                   EVAL      *INH3 = *ON
014800040903     C                   LEAVE
014900040903     C                   OTHER
015000040903     C                   EXSR      WrtAddebito
015100040903     C                   EXSR      UpdTbeLVA1
015200040903     C                   COMMIT
015300040903     C                   ENDSL
015400040903
015500040903     C                   ENDDO
015600040903
015700040903     C/EXEC SQL
015800040903     C+ CLOSE TIVTL10F
015900040903     C/END-EXEC
016000040903
016100040903     C/EXEC SQL
016200040903     C+ DISCONNECT AS777
016300040903     C/END-EXEC
016400040903
016500040903     C                   ENDSR
016600040903
016700040903     ***********************************************************************************************
016800040903     **?
016900041129     **?Reperisco dalla tabella LVA il momento dell'ultimo record estratto nella precedente estraz.
017000040903     **?
017100040903     ***********************************************************************************************
017200040903     C     GetTbeLVA1    BEGSR
017300040903
017400040903     C     K05Tbe01      CHAIN(N)  TnTbe01L
017500040903
017600040903     C                   IF        NOT %FOUND
017700040903     C                   EVAL      *INH1 = *ON
017800040903     C                   EXSR      Uscita
017900040903     C                   ENDIF
018000040903
018100040903     C                   EVAL      DLVA1 = TbeUni
018200040903
018300040903     C                   ENDSR
018400040903
018500040903     ***********************************************************************************************
018600040903     **?
018700040903     **?Scrittura addebito.
018800040903     **?
018900040903     ***********************************************************************************************
019000040903     C     WrtAddebito   BEGSR
019100040903
019200040903     C                   EVAL      RigAAS = RowLdvAAS
019300040903     C                   EVAL      RigLnP = RowLdvLnP
019400040903     C                   EVAL      RigNrS = RowLdvNrS
019500040903     C                   EVAL      RigNSp = RowLdvNSp
019600090420     c                   eval      rigksc = atoi(%subst(rowksu:2:7))
019700090420     C                   EVAL      RigCau = Internet
019800090420     c                   eval      rigtas = tariffa
019900090420     c                   eval      rigncl = 1
020000090420     C                   EVAL      RigTra = Nessuno
020100090420     C                   EVAL      RigFil = atoi(%SUBST(RowKSU:2:3))
020200040903     C                   EVAL      RigPrU = RowKSU
020300040903     C                   MOVE      RowRpyMmt     WrkDtISO
020400090420     C                   MOVE      WrkDtISO      Rigdata
020500040903     C                   MOVE      RowRpyMmt     WrkOrISO
020600040903     C                   MOVE      WrkOrISO      WrkOr60
020700090420     C                   EVAL      Rigora = WrkOr60
020800090420     C                   WRITE     FnRig000
020900040903
021000040903     C                   ENDSR
021100040903
021200040903     ***********************************************************************************************
021300040903     **?
021400041129     **?Aggiorno tabella LVA con il momento dell'ultimo record addebitato.
021500040903     **?
021600040903     ***********************************************************************************************
021700040903     C     UpdTbeLVA1    BEGSR
021800040903
021900040903     C                   DO        *HIVAL
022000040903     C     K05Tbe01      CHAIN(E)  TnTbe01L
022100040903     C                   SELECT
022200040903     C                   WHEN      NOT %ERROR
022300040903     C                   LEAVE
022400040903     C                   WHEN      %ERROR AND %STATUS <> RecordAlreadyLocked
022500040903     C                   ROLBK
022600040903     C                   EVAL      *INH2 = *ON
022700040903     C                   EXSR      Uscita
022800040903     C                   ENDSL
022900040903     C                   ENDDO
023000040903
023100040903     C                   IF        NOT %FOUND
023200040903     C                   ROLBK
023300040903     C                   EVAL      *INH1 = *ON
023400040903     C                   EXSR      Uscita
023500040903     C                   ENDIF
023600040903
023700040903     C                   EVAL      DLVA1 = TbeUni
023800041129
023900041129     C                   IF        §LVAUEsMmt < RowRpyMmt
024000040903     C                   EVAL      §LVAUEsMmt = RowRpyMmt
024100040903     C                   EVAL      TbeUni = DLVA1
024200040903     C                   UPDATE    TnTbe000
024300041129     C                   ELSE
024400041129     C                   UNLOCK    TnTbe01L
024500041129     C                   ENDIF
024600040903
024700040903     C                   ENDSR
024800040903
024900040903     ***********************************************************************************************
025000040903     **?
025100040903     **?Operazioni iniziali.
025200040903     **?
025300040903     ***********************************************************************************************
025400040903     C     *INZSR        BEGSR
025500040903
025600040903     C                   EVAL      TbeCod = 'LVA'
025700040903     C                   EVAL      TbeKe1 = '1'
025800040903
025900040903     C                   ENDSR
026000040903
026100040903     ***********************************************************************************************
026200040903     **?
026300040903     **?Uscita dal programma.
026400040903     **?
026500040903     ***********************************************************************************************
026600040903     C     Uscita        BEGSR
026700040903
026800040903     C                   EVAL      *INLR = *ON
026900040903     C                   RETURN
027000040903
027100040903     C                   ENDSR
