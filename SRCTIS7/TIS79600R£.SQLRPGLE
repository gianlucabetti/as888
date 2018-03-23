000100040903      /TITLE Estrazione LdV visualizzate da internet per addebito.
000200040903     ***********************************************************************************************
000300040903     **?
000400040906     **?Questo programma si collega all' iSeries web server, legge il log delle richieste
000500040906     **?internet, seleziona quelle di visualizzazione immagine lettera di vettura e per ogni
000600040906     **?LdV visualizzata scrive un record in FIRIG00F per l' addebito nella successiva fattura.
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
002000040903     FFiRig00F  O    E             DISK
002100040903     F                                     COMMIT
002200040903
002300040903     D RecordAlreadyLocked...
002400040903     D                 C                   01218
002500040906     D Internet        C                   'I'
002600040906     D Entrambi        C                   'E'
002700040903
002800040903     D KPJBA         E DS
002900040903     D CnDizion      E DS
003000040903     D                                     BASED(NullPtr)
003100040903     D                                     PREFIX(Diz)
003200040906     D DLVA1         E DS
003300040903     D                                     INZ
003400040903
003500040903     D NullPtr         S               *
003600040906     D RowRpyMmt       S               Z                                        Momento risposta
003700040906     D RowKSU          S              8                                         Cliente unificante
003800040906     D RowLdvAAS       S                                                        Anno
003900040903     D                                     LIKE(Diz$AAS)
004000040906     D RowLdvLnP       S                                                        PO partenza
004100040903     D                                     LIKE(Diz$LnP)
004200040906     D RowLdvNrS       S                                                        Serie
004300040903     D                                     LIKE(Diz$NrS)
004400040906     D RowLdvNSp       S                                                        Numero spedizione
004500040903     D                                     LIKE(Diz$NSp)
004600040903     D WrkDtISO        S               D
004700040903     D WrkOrISO        S               T
004800040903     D WrkOr60         S              6  0
004900041129     D WrkAdesso       S               Z
005000040903
005100040903     D atoi            PR            10I 0
005200040903     D                                     EXTPROC('atoi')
005300040903     D  Char                           *
005400040903     D                                     VALUE
005500040903     D                                     OPTIONS(*STRING)
005600040903
005700040903     ***********************************************************************************************
005800040903     **?
005900040903     **?Dichiarazione parametri.
006000040903     **?
006100040903     ***********************************************************************************************
006200040903     C     *ENTRY        PLIST
006300040903     C                   PARM                    KPJBA
006400040903
006500040903     ***********************************************************************************************
006600040903     **?
006700040903     **?Dichiarazioni varie.
006800040903     **?
006900040903     ***********************************************************************************************
007000040903     C     K05Tbe01      KLIST
007100040903     C                   KFLD                    TbeCod
007200040903     C                   KFLD                    TbeKe1
007300040903     C                   KFLD                    TbeKe2
007400040903     C                   KFLD                    TbeLin
007500040903     C                   KFLD                    TbeSIf
007600040903
007700040903     ***********************************************************************************************
007800040903     **?
007900040903     **?Main.
008000040903     **?
008100040903     ***********************************************************************************************
008200040903     C                   EXSR      GetLogVAS
008300040903     C                   EXSR      Uscita
008400040903
008500040903     ***********************************************************************************************
008600040903     **?
008700040903     **?Punto di arrivo in caso di errore SQL.
008800040903     **?
008900040903     ***********************************************************************************************
009000040903     C     ErroreSQL     TAG
009100040903     C                   EVAL      *INH3 = *ON
009200040903     C                   EXSR      Uscita
009300040903
009400040903     ***********************************************************************************************
009500040903     **?
009600040903     **?Estrazione record da log VAS internet.
009700040903     **?
009800040903     ***********************************************************************************************
009900040903     C     GetLogVAS     BEGSR
010000040903
010100041129     C                   TIME                    WrkAdesso
010200040903     C                   EXSR      GetTbeLVA1
010300040903
010400040903     C/EXEC SQL
010500040903     C+ WHENEVER SQLERROR GOTO ErroreSQL
010600040903     C/END-EXEC
010700040903
010800040903     C/EXEC SQL
010900040903     C+ CONNECT TO AS777
011000040903     C/END-EXEC
011100040903
011200040903     C/EXEC SQL
011300040903     C+ DECLARE TIVTL10F CURSOR FOR SELECT VTLRPYMMT, VTLKSU, 2000 +
011400050617     C+ DEC(SUBSTR(VTLRQSDTA, 1, 2), 2), DEC(SUBSTR(VTLRQSDTA, 3, 3), 3),
011500050617     C+ DEC(SUBSTR(VTLRQSDTA, 6, 2), 2), DEC(SUBSTR(VTLRQSDTA, 8, 7), 7)
011600050617     C+ FROM STRATEBAR/TIVTL10F WHERE VTLKSU <> ' ' AND LEFT(VTLCLTIPAD,
011700050617     C+ 3) <> '10.' AND VTLTIP = 'LV' AND VTLRPYOPC = 'DONE' AND
011800050617     C+ VTLRPYMMT > :§LVAUEsMmt AND VTLRPYMMT < :WrkAdesso AND VTLRQSOPC
011900050617     C+ = 'RTVIMG' AND LEFT(VTLRPYDTA, 12) = '/tempimages/' AND
012000050617     C+ SUBSTR(VTLRPYDTA, 13) LIKE '%.jpg%' UNION ALL SELECT VTLRPYMMT,
012100050617     C+ VTLKSU, 2000 + DEC(SUBSTR(VTLRQSDTA, 41, 2), 2),
012200050617     C+ DEC(SUBSTR(VTLRQSDTA, 43, 3), 3), DEC(SUBSTR(VTLRQSDTA, 46, 2),
012300050617     C+ 2), DEC(SUBSTR(VTLRQSDTA, 48, 7), 7) FROM STRATEBAR/TIVTL10F
012400050617     C+ WHERE VTLKSU <> ' ' AND LEFT(VTLCLTIPAD, 3) <> '10.' AND VTLTIP =
012500050617     C+ 'LV' AND VTLRPYOPC = 'DONE' AND VTLRPYMMT > :§LVAUEsMmt AND
012600050617     C+ VTLRPYMMT < :WrkAdesso AND VTLRQSOPC = 'RTVIMGXML' AND
012700050617     C+ SUBSTR(VTLRPYDTA, 129, 12) = '/TempImages/' AND SUBSTR(VTLRPYDTA,
012800050617     C+ 141) LIKE '%.jpg%' ORDER BY VTLRPYMMT FOR READ ONLY
012900040903     C/END-EXEC
013000040903
013100040903     C/EXEC SQL
013200040903     C+ OPEN TIVTL10F
013300040903     C/END-EXEC
013400040903
013500040903     C                   DO        *HIVAL
013600040903
013700040903     C/EXEC SQL
013800040903     C+ FETCH NEXT FROM TIVTL10F INTO :RowRpyMmt, :RowKSU, :RowLdvAAS,
013900040903     C+ :RowLdvLnP, :RowLdvNrS, :RowLdvNSp
014000040903     C/END-EXEC
014100040903
014200040903     C                   SELECT
014300040903     C                   WHEN      SQLCod = 100
014400040903     C                   LEAVE
014500040903     C                   WHEN      SQLCod < 0
014600040903     C                   EVAL      *INH3 = *ON
014700040903     C                   LEAVE
014800040903     C                   OTHER
014900040903     C                   EXSR      WrtAddebito
015000040903     C                   EXSR      UpdTbeLVA1
015100040903     C                   COMMIT
015200040903     C                   ENDSL
015300040903
015400040903     C                   ENDDO
015500040903
015600040903     C/EXEC SQL
015700040903     C+ CLOSE TIVTL10F
015800040903     C/END-EXEC
015900040903
016000040903     C/EXEC SQL
016100040903     C+ DISCONNECT AS777
016200040903     C/END-EXEC
016300040903
016400040903     C                   ENDSR
016500040903
016600040903     ***********************************************************************************************
016700040903     **?
016800041129     **?Reperisco dalla tabella LVA il momento dell'ultimo record estratto nella precedente estraz.
016900040903     **?
017000040903     ***********************************************************************************************
017100040903     C     GetTbeLVA1    BEGSR
017200040903
017300040903     C     K05Tbe01      CHAIN(N)  TnTbe01L
017400040903
017500040903     C                   IF        NOT %FOUND
017600040903     C                   EVAL      *INH1 = *ON
017700040903     C                   EXSR      Uscita
017800040903     C                   ENDIF
017900040903
018000040903     C                   EVAL      DLVA1 = TbeUni
018100040903
018200040903     C                   ENDSR
018300040903
018400040903     ***********************************************************************************************
018500040903     **?
018600040903     **?Scrittura addebito.
018700040903     **?
018800040903     ***********************************************************************************************
018900040903     C     WrtAddebito   BEGSR
019000040903
019100040903     C                   EVAL      RigAAS = RowLdvAAS
019200040903     C                   EVAL      RigLnP = RowLdvLnP
019300040903     C                   EVAL      RigNrS = RowLdvNrS
019400040903     C                   EVAL      RigNSp = RowLdvNSp
019500040906     C                   EVAL      RigCaR = Internet
019600040906     C                   EVAL      RigTra = Entrambi
019700040903     C                   EVAL      RigPOR = atoi(%SUBST(RowKSU:2:3))
019800040903     C                   EVAL      RigPrU = RowKSU
019900040903     C                   MOVE      RowRpyMmt     WrkDtISO
020000040903     C                   MOVE      WrkDtISO      RigDIm
020100040903     C                   MOVE      RowRpyMmt     WrkOrISO
020200040903     C                   MOVE      WrkOrISO      WrkOr60
020300040903     C                   EVAL      RigHIm = WrkOr60 / 100
020400040903     C                   WRITE     FiRig000
020500040903
020600040903     C                   ENDSR
020700040903
020800040903     ***********************************************************************************************
020900040903     **?
021000041129     **?Aggiorno tabella LVA con il momento dell'ultimo record addebitato.
021100040903     **?
021200040903     ***********************************************************************************************
021300040903     C     UpdTbeLVA1    BEGSR
021400040903
021500040903     C                   DO        *HIVAL
021600040903     C     K05Tbe01      CHAIN(E)  TnTbe01L
021700040903     C                   SELECT
021800040903     C                   WHEN      NOT %ERROR
021900040903     C                   LEAVE
022000040903     C                   WHEN      %ERROR AND %STATUS <> RecordAlreadyLocked
022100040903     C                   ROLBK
022200040903     C                   EVAL      *INH2 = *ON
022300040903     C                   EXSR      Uscita
022400040903     C                   ENDSL
022500040903     C                   ENDDO
022600040903
022700040903     C                   IF        NOT %FOUND
022800040903     C                   ROLBK
022900040903     C                   EVAL      *INH1 = *ON
023000040903     C                   EXSR      Uscita
023100040903     C                   ENDIF
023200040903
023300040903     C                   EVAL      DLVA1 = TbeUni
023400041129
023500041129     C                   IF        §LVAUEsMmt < RowRpyMmt
023600040903     C                   EVAL      §LVAUEsMmt = RowRpyMmt
023700040903     C                   EVAL      TbeUni = DLVA1
023800040903     C                   UPDATE    TnTbe000
023900041129     C                   ELSE
024000041129     C                   UNLOCK    TnTbe01L
024100041129     C                   ENDIF
024200040903
024300040903     C                   ENDSR
024400040903
024500040903     ***********************************************************************************************
024600040903     **?
024700040903     **?Operazioni iniziali.
024800040903     **?
024900040903     ***********************************************************************************************
025000040903     C     *INZSR        BEGSR
025100040903
025200040903     C                   EVAL      TbeCod = 'LVA'
025300040903     C                   EVAL      TbeKe1 = '1'
025400040903
025500040903     C                   ENDSR
025600040903
025700040903     ***********************************************************************************************
025800040903     **?
025900040903     **?Uscita dal programma.
026000040903     **?
026100040903     ***********************************************************************************************
026200040903     C     Uscita        BEGSR
026300040903
026400040903     C                   EVAL      *INLR = *ON
026500040903     C                   RETURN
026600040903
026700040903     C                   ENDSR
