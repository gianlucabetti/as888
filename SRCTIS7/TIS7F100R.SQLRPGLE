000100040426      /TITLE Scrive/cancella l'elenco dei colli in TIVTB00F.
000200040426
000300040426     H DFTACTGRP(*NO) ACTGRP(*CALLER) OPTION(*NODEBUGIO) BNDDIR('QC2LE')
000400040423
000500040426     FTITAS30C  IF   E           K DISK                                         Spedizioni
000600040426     F                                     USROPN
000700040426     FTIVTB00F  O    E             DISK
000800040426     F                                     USROPN
000900040426     F                                     COMMIT(CmtCtlIF1)
001000040520     F                                     INFDS(InfTIVTB)
001100040423
001200040423     D Eseguito        C                   '0'
001300040423     D Errore          C                   '1'
001400040426     D Write           C                   'W'
001500040426     D Delete          C                   'D'
001600040426     D Update          C                   'U'
001700040423     D NuovaRichiesta  C                   '1'
001800040423     D RichiestaSuccessiva...
001900040423     D                 C                   '2'
002000040426     D FineDeiDati     C                   '9'
002100040423
002200040426     D TIS7F1DSI     E DS
002300040426     D TIS7F1DSO     E DS
002400040423     D TIS7F0DSI     E DS
002500040426     D                                     INZ
002600040423     D TIS7F0DSO     E DS
002700040426     D                                     INZ
002800040426     D  Col                          15
002900040426     D                                     DIM(25)
003000040426     D                                     OVERLAY(IdColloOF0)
003100040426     D  Chi                          35
003200040426     D                                     DIM(%ELEM(Col))
003300040426     D                                     OVERLAY(ChiSonoOF0)
003400040520     D InfTIVTB        DS
003500040520     D  Library               93    102
003600040426
003700040426     D Operazione      S              1
003800040426     D                                     INZ(NuovaRichiesta)
003900040426     D RigheOut        S              3  0
004000040426     D Esito           S              1
004100040426     D i               S              5I 0
004200040426
004300040426     D atof            PR             8F
004400040426     D                                     EXTPROC('atof')
004500040426     D   Char                          *
004600040426     D                                     VALUE
004700040426     D                                     OPTIONS(*STRING)
004800040423
004900040423     ***********************************************************************************************
005000040423     **?
005100040423     **?Main.
005200040423     **?
005300040423     ***********************************************************************************************
005400090917     C*
005500090917     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
005600090917     C
005700090917     C/EXEC SQL
005800090917     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
005900090917     C/END-EXEC
006000090917     C*
006100090917
006200040423     C     *ENTRY        PLIST
006300040426     C                   PARM                    TIS7F1DSI
006400040426     C                   PARM                    TIS7F1DSO
006500040423
006600040423     C                   EXSR      ChkParm
006700040426     C                   EXSR      ResetPgm
006800040426     C                   EXSR      GetBolla
006900040423
007000040426     C                   IF        OpeIF1 = Update OR OpeIF1 = Delete
007100040426     C                   EXSR      DelTIVTB
007200040426     C                   ENDIF
007300040426
007400040426     C                   IF        OpeIF1 = Update OR OpeIF1 = Write
007500040426     C                   EXSR      WrtTIVTB
007600040426     C                   ENDIF
007700040423
007800040423     C                   EXSR      Uscita
007900040423
008000040423     ***********************************************************************************************
008100040423     **?
008200040426     **?Operazioni iniziali.
008300040423     **?
008400040423     ***********************************************************************************************
008500040423     C     *INZSR        BEGSR
008600040423
008700040423     C     K05TAS30      KLIST
008800040423     C                   KFLD                    TASAAS
008900040423     C                   KFLD                    TASLnP
009000040423     C                   KFLD                    TASNrS
009100040423     C                   KFLD                    TASNSp
009200040423     C                   KFLD                    TASTBl
009300040423
009400040423     C     K04TAS30      KLIST
009500040423     C                   KFLD                    TASAAS
009600040423     C                   KFLD                    TASLnP
009700040423     C                   KFLD                    TASNrS
009800040423     C                   KFLD                    TASNSp
009900040423
010000040423     C                   ENDSR
010100040426
010200040426     ***********************************************************************************************
010300040426     **?
010400040426     **?Resetto le variabili del programma per una nuova richiesta.
010500040426     **?
010600040426     ***********************************************************************************************
010700040426     C     ResetPgm      BEGSR
010800040426
010900040426
011000040426     C                   ENDSR
011100040423
011200040423     ***********************************************************************************************
011300040423     **?
011400040423     **?Controllo parametri ricevuti.
011500040423     **?
011600040423     ***********************************************************************************************
011700040423     C     ChkParm       BEGSR
011800040423
011900040426     **?Pulisco i parametri di output.
012000040426     C                   CLEAR                   TIS7F1DSO
012100040423
012200040423     **?Parametri obbligatori.
012300040426     C                   IF        LnPIF1 = 0 OR NSpIF1 = 0
012400040426     C                   EVAL      EsitoOF1 = Errore
012500040423     C                   EXSR      Uscita
012600040423     C                   ENDIF
012700040423
012800040423     C                   ENDSR
012900040426
013000040426     ***********************************************************************************************
013100040426     **?
013200040426     **?Cancellazione TIVTB00F.
013300040426     **?
013400040426     ***********************************************************************************************
013500040426     C     DelTIVTB      BEGSR
013600040426
013700040426     C/EXEC SQL
013800040426     C+ WHENEVER SQLERROR GOTO :ErroreSQL
013900040426     C/END-EXEC
014000040426
014100090918     C*
014200090918     C* Elaboro x cliente unificante
014300090918     C                   IF        KSUIF1 <> *blanks AND
014400090918     C                             KSUIF1 <> *zeros
014500090918
014600040426     C/EXEC SQL
014700040426     C+ DELETE FROM TIVTB00F WHERE VTBKSU = :KSUIF1 AND VTBAAS = :TASAAS
014800040426     C+ AND VTBLNP = :TASLNP AND VTBNRS = :TASNRS AND VTBNSP = :TASNSP
014900040426     C/END-EXEC
015000090918
015100090918     C                   ELSE
015200090918     C*
015300090918     C* Elaboro x singola bolla
015400090918
015500090918     C/EXEC SQL
015600090918     C+ DELETE FROM TIVTB00F WHERE VTBAAS = :TASAAS
015700090918     C+ AND VTBLNP = :TASLNP AND VTBNRS = :TASNRS AND VTBNSP = :TASNSP
015800090918     C/END-EXEC
015900090918
016000090918     C                   ENDIF
016100040426
016200040426     C                   GOTO      EseguitoSQL
016300040426     C     ErroreSQL     TAG
016400040426     C                   EVAL      *INH1 = *ON
016500040426     C                   EVAL      EsitoOF1 = Errore
016600040426     C                   EXSR      Uscita
016700040426     C     EseguitoSQL   TAG
016800040426     C                   EVAL      RecDelOF1 = SQLEr3                           Righe cancellate
016900040426
017000040426     C                   ENDSR
017100040426
017200040426     ***********************************************************************************************
017300040426     **?
017400040426     **?Scrittura TIVTB00F.
017500040426     **?
017600040426     ***********************************************************************************************
017700040426     C     WrtTIVTB      BEGSR
017800040427
017900040520     **?In LibIF1 potrei ricevere la libreria di residenza di TIVTB00F (es. QTEMP);
018000040520     **?se è diversa dalla libreria precedente, chiudo il file per poi riaprirlo.
018100040520     C                   IF        %OPEN(TIVTB00F)
018200040520     C                             AND LibIF1 <> *BLANK AND LibIF1 <> Library
018300040520     C                   CLOSE     TIVTB00F
018400040520     C                   ENDIF
018500040520
018600040520     C                   IF        NOT %OPEN(TIVTB00F)
018700040427     C                   OPEN      TIVTB00F
018800040520     C                   EVAL      LibIF1 = Library
018900040520     C                   ENDIF
019000040520
019100040427     C                   CLEAR                   TIVTB000
019200040426
019300040427     **?Imposto i campi uguali per tutti i records che scriverò.
019400040426     C                   EVAL      VTBKSU = KSUIF1
019500040426     C                   EVAL      VTBAAS = TASAAS
019600040426     C                   EVAL      VTBLnP = TASLnP
019700040426     C                   EVAL      VTBNrS = TASNrS
019800040426     C                   EVAL      VTBNSp = TASNSp
019900040426     C                   EVAL      VTBTBl = TASTBl
020000040427     C                   EVAL      VTBDSp = (TASAAS * 10000) + TASMGS
020100040426     C                   EVAL      VTBLnA = TASLnA
020200040426
020300040426     C                   EVAL      AASIF0 = TASAAS
020400040426     C                   EVAL      LnPIF0 = TASLnP
020500040426     C                   EVAL      NrSIF0 = TASNrS
020600040426     C                   EVAL      NSpIF0 = TASNSp
020700040426     C                   EVAL      TBlIF0 = TASTBl
020800040426     C                   RESET                   Operazione
020900040426
021000040520     **?Reperisco l'elenco dei colli.
021100040426     C                   DO        *HIVAL
021200040426
021300040426     C                   CLEAR                   TIS7F0DSO
021400040426
021500040427     C                   CALL(E)   'TIS7F000R'
021600040426     C                   PARM                    TIS7F0DSI
021700040426     C                   PARM                    Operazione
021800040426     C                   PARM                    TIS7F0DSO
021900040426     C                   PARM      *ZERO         RigheOut
022000040426     C                   PARM      *BLANK        Esito
022100040426
022200040427     C                   IF        Esito = Errore OR %ERROR
022300040426     C                   EVAL      EsitoOF1 = Errore
022400040426     C                   EXSR      Uscita
022500040426     C                   ENDIF
022600040426
022700040426     C                   CLEAR                   i
022800040426
022900040426     C                   DO        *HIVAL
023000040426     C                   EVAL      i = i + 1
023100040426     C                   IF        (Col(i) = *BLANK AND Chi(i) = *BLANK)
023200040426     C                   LEAVE
023300040426     C                   ENDIF
023400040426     C                   IF        Col(i) <> *BLANK
023500040426     C                   EVAL      VTBFlS = atof(%SUBST(Col(i):1:3))
023600100308     C                   EVAL      VTBNSC = atof(%SUBST(Col(i):9:7))
023700040426     C                   ENDIF
023800040426     C                   EVAL      VTBChiSono = %TRIMR(Chi(i))
023900090805     C* Se bolla Disk B => come "chi sono" forzo il segnacollo Bartolini (15 chars)
024000090805     C                   IF        VTBNRS > *zeros
024100090805     C                   EVAL      VTBChiSono = %TRIMR(Col(i))
024200090805     C                   ENDIF
024300100308     C                   IF        VTBChiSono <> *blanks
024400040426     C                   WRITE     TIVTB000
024500040426     C                   EVAL      RecWrtOF1 = RecWrtOF1 + 1                    Righe scritte
024600100308     C                   ENDIF
024700040426     C                   IF        i = %ELEM(Col)
024800040426     C                   LEAVE
024900040426     C                   ENDIF
025000040426     C                   ENDDO
025100040426
025200040426     C                   IF        Operazione = FineDeiDati
025300040426     C                   LEAVE
025400040426     C                   ENDIF
025500040426
025600040426     C                   EVAL      Operazione = RichiestaSuccessiva
025700040426
025800040426     C                   ENDDO
025900040426
026000040520     **?Se sto usando il file in QTEMP lo chiudo per permettere al chiamante di fare CLRPFM.
026100040520     **?Se invece non sto usando il file in QTEMP lo lascio aperto perchè sto popolando il TIVTB00F
026200040520     **?residente e devo elaborare migliaia di spedizioni.
026300040520     C                   IF        LibIF1 = 'QTEMP'
026400040427     C                   CLOSE     TIVTB00F
026500040520     C                   ENDIF
026600040426
026700040426     C                   ENDSR
026800040423
026900040423     ***********************************************************************************************
027000040423     **?
027100040426     **?Reperisco i dati della spedizione.
027200040423     **?
027300040423     ***********************************************************************************************
027400040423     C     GetBolla      BEGSR
027500040423
027600040426     C                   EVAL      TASLnP = LnPIF1
027700040426     C                   EVAL      TASNrS = NrSIF1
027800040426     C                   EVAL      TASNSp = NSpIF1
027900040426     C                   EVAL      TASTBl = TBlIF1
028000040423
028100040426     **?Se non ho ricevuto l'anno faccio 2 tentativi, con l'anno corrente e precedente.
028200040426     C                   IF        AASIF1 = 0
028300040423     C                   EVAL      TASAAS = *YEAR
028400040426     C                   ELSE
028500090917     C                   EVAL      TASAAS = AASIF1
028600040423     C                   ENDIF
028700040423
028800040520     C                   IF        NOT %OPEN(TITAS30C)
028900040423     C                   OPEN      TITAS30C
029000040520     C                   ENDIF
029100040426
029200040423     C                   DO        2
029300040426     C                   IF        TBlIF1 <> *BLANK
029400040423     C     K05TAS30      CHAIN     TITAS30C
029500040423     C                   ELSE
029600040423     C     K04TAS30      CHAIN     TITAS30C
029700040423     C                   ENDIF
029800040426     C                   IF        %FOUND
029900040426     C                   EVAL      AASIF1 = TASAAS
030000040426     C                   EVAL      TBlIF1 = TASTBl
030100040426     C                   LEAVE
030200040426     C                   ELSE
030300040426     C                   IF        AASIF1 = 0
030400040423     C                   EVAL      TASAAS = TASAAS - 1
030500040426     C                   ELSE
030600040426     C                   LEAVE
030700040426     C                   ENDIF
030800040426     C                   ENDIF
030900040423     C                   ENDDO
031000040423
031100040520     C                   IF        LibIF1 = 'QTEMP'
031200040423     C                   CLOSE     TITAS30C
031300040520     C                   ENDIF
031400040423
031500040426     **?Spedizione non trovata.
031600040423     C                   IF        NOT %FOUND
031700040426     C                   EVAL      EsitoOF1 = Errore
031800040423     C                   EXSR      Uscita
031900040423     C                   ENDIF
032000040423
032100040423     C                   ENDSR
032200040423
032300040423     ***********************************************************************************************
032400040423     **?
032500040423     **?Uscita dal programma.
032600040423     **?
032700040423     ***********************************************************************************************
032800040423     C     Uscita        BEGSR
032900040427
033000040426     C                   IF        EsitoOF1 = *BLANK
033100040426     C                   EVAL      EsitoOF1 = Eseguito
033200040426     C                   ENDIF
033300040426
033400040426     C                   IF        CommitIF1 = *ON
033500040426     C                   IF        EsitoOF1 = Eseguito
033600040426     C                   COMMIT
033700040426     C                   ELSE
033800040426     C                   ROLBK
033900040426     C                   ENDIF
034000040426     C                   ENDIF
034100040426
034200040426     C                   IF        EsitoOF1 = Errore
034300040423     C                   EVAL      *INLR = *ON
034400040423     C                   ENDIF
034500040423
034600040423     C                   RETURN
034700040423
034800040423     C                   ENDSR
