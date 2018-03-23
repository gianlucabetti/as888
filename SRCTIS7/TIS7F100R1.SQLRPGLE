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
006500091207     C                   PARM                    rwrinew           6 0
006600040423
006700040423     C                   EXSR      ChkParm
006800040426     C                   EXSR      ResetPgm
006900040426     C                   EXSR      GetBolla
007000040423
007100040426     C                   IF        OpeIF1 = Update OR OpeIF1 = Delete
007200040426     C                   EXSR      DelTIVTB
007300040426     C                   ENDIF
007400040426
007500040426     C                   IF        OpeIF1 = Update OR OpeIF1 = Write
007600040426     C                   EXSR      WrtTIVTB
007700040426     C                   ENDIF
007800040423
007900040423     C                   EXSR      Uscita
008000040423
008100040423     ***********************************************************************************************
008200040423     **?
008300040426     **?Operazioni iniziali.
008400040423     **?
008500040423     ***********************************************************************************************
008600040423     C     *INZSR        BEGSR
008700040423
008800040423     C     K05TAS30      KLIST
008900040423     C                   KFLD                    TASAAS
009000040423     C                   KFLD                    TASLnP
009100040423     C                   KFLD                    TASNrS
009200040423     C                   KFLD                    TASNSp
009300040423     C                   KFLD                    TASTBl
009400040423
009500040423     C     K04TAS30      KLIST
009600040423     C                   KFLD                    TASAAS
009700040423     C                   KFLD                    TASLnP
009800040423     C                   KFLD                    TASNrS
009900040423     C                   KFLD                    TASNSp
010000040423
010100040423     C                   ENDSR
010200040426
010300040426     ***********************************************************************************************
010400040426     **?
010500040426     **?Resetto le variabili del programma per una nuova richiesta.
010600040426     **?
010700040426     ***********************************************************************************************
010800040426     C     ResetPgm      BEGSR
010900040426
011000040426
011100040426     C                   ENDSR
011200040423
011300040423     ***********************************************************************************************
011400040423     **?
011500040423     **?Controllo parametri ricevuti.
011600040423     **?
011700040423     ***********************************************************************************************
011800040423     C     ChkParm       BEGSR
011900040423
012000040426     **?Pulisco i parametri di output.
012100040426     C                   CLEAR                   TIS7F1DSO
012200040423
012300040423     **?Parametri obbligatori.
012400040426     C                   IF        LnPIF1 = 0 OR NSpIF1 = 0
012500040426     C                   EVAL      EsitoOF1 = Errore
012600040423     C                   EXSR      Uscita
012700040423     C                   ENDIF
012800040423
012900040423     C                   ENDSR
013000040426
013100040426     ***********************************************************************************************
013200040426     **?
013300040426     **?Cancellazione TIVTB00F.
013400040426     **?
013500040426     ***********************************************************************************************
013600040426     C     DelTIVTB      BEGSR
013700040426
013800040426     C/EXEC SQL
013900040426     C+ WHENEVER SQLERROR GOTO :ErroreSQL
014000040426     C/END-EXEC
014100040426
014200090918     C*
014300090918     C* Elaboro x cliente unificante
014400090918     C                   IF        KSUIF1 <> *blanks AND
014500090918     C                             KSUIF1 <> *zeros
014600090918
014700040426     C/EXEC SQL
014800040426     C+ DELETE FROM TIVTB00F WHERE VTBKSU = :KSUIF1 AND VTBAAS = :TASAAS
014900040426     C+ AND VTBLNP = :TASLNP AND VTBNRS = :TASNRS AND VTBNSP = :TASNSP
015000040426     C/END-EXEC
015100090918
015200090918     C                   ELSE
015300090918     C*
015400090918     C* Elaboro x singola bolla
015500090918
015600090918     C/EXEC SQL
015700090918     C+ DELETE FROM TIVTB00F WHERE VTBAAS = :TASAAS
015800090918     C+ AND VTBLNP = :TASLNP AND VTBNRS = :TASNRS AND VTBNSP = :TASNSP
015900090918     C/END-EXEC
016000090918
016100090918     C                   ENDIF
016200040426
016300040426     C                   GOTO      EseguitoSQL
016400040426     C     ErroreSQL     TAG
016500040426     C                   EVAL      *INH1 = *ON
016600040426     C                   EVAL      EsitoOF1 = Errore
016700040426     C                   EXSR      Uscita
016800040426     C     EseguitoSQL   TAG
016900040426     C                   EVAL      RecDelOF1 = SQLEr3                           Righe cancellate
017000040426
017100040426     C                   ENDSR
017200040426
017300040426     ***********************************************************************************************
017400040426     **?
017500040426     **?Scrittura TIVTB00F.
017600040426     **?
017700040426     ***********************************************************************************************
017800040426     C     WrtTIVTB      BEGSR
017900040427
018000040520     **?In LibIF1 potrei ricevere la libreria di residenza di TIVTB00F (es. QTEMP);
018100040520     **?se è diversa dalla libreria precedente, chiudo il file per poi riaprirlo.
018200040520     C                   IF        %OPEN(TIVTB00F)
018300040520     C                             AND LibIF1 <> *BLANK AND LibIF1 <> Library
018400040520     C                   CLOSE     TIVTB00F
018500040520     C                   ENDIF
018600040520
018700040520     C                   IF        NOT %OPEN(TIVTB00F)
018800040427     C                   OPEN      TIVTB00F
018900040520     C                   EVAL      LibIF1 = Library
019000040520     C                   ENDIF
019100040520
019200040427     C                   CLEAR                   TIVTB000
019300040426
019400040427     **?Imposto i campi uguali per tutti i records che scriverò.
019500040426     C                   EVAL      VTBKSU = KSUIF1
019600040426     C                   EVAL      VTBAAS = TASAAS
019700040426     C                   EVAL      VTBLnP = TASLnP
019800040426     C                   EVAL      VTBNrS = TASNrS
019900040426     C                   EVAL      VTBNSp = TASNSp
020000040426     C                   EVAL      VTBTBl = TASTBl
020100040427     C                   EVAL      VTBDSp = (TASAAS * 10000) + TASMGS
020200040426     C                   EVAL      VTBLnA = TASLnA
020300040426
020400040426     C                   EVAL      AASIF0 = TASAAS
020500040426     C                   EVAL      LnPIF0 = TASLnP
020600040426     C                   EVAL      NrSIF0 = TASNrS
020700040426     C                   EVAL      NSpIF0 = TASNSp
020800040426     C                   EVAL      TBlIF0 = TASTBl
020900040426     C                   RESET                   Operazione
021000040426
021100040520     **?Reperisco l'elenco dei colli.
021200040426     C                   DO        *HIVAL
021300040426
021400040426     C                   CLEAR                   TIS7F0DSO
021500040426
021600040427     C                   CALL(E)   'TIS7F000R'
021700040426     C                   PARM                    TIS7F0DSI
021800040426     C                   PARM                    Operazione
021900040426     C                   PARM                    TIS7F0DSO
022000040426     C                   PARM      *ZERO         RigheOut
022100040426     C                   PARM      *BLANK        Esito
022200040426
022300040427     C                   IF        Esito = Errore OR %ERROR
022400040426     C                   EVAL      EsitoOF1 = Errore
022500040426     C                   EXSR      Uscita
022600040426     C                   ENDIF
022700040426
022800040426     C                   CLEAR                   i
022900040426
023000040426     C                   DO        *HIVAL
023100040426     C                   EVAL      i = i + 1
023200040426     C                   IF        (Col(i) = *BLANK AND Chi(i) = *BLANK)
023300040426     C                   LEAVE
023400040426     C                   ENDIF
023500040426     C                   IF        Col(i) <> *BLANK
023600040426     C                   EVAL      VTBFlS = atof(%SUBST(Col(i):1:3))
023700100308     C                   EVAL      VTBNSC = atof(%SUBST(Col(i):9:7))
023800040426     C                   ENDIF
023900040426     C                   EVAL      VTBChiSono = %TRIMR(Chi(i))
024000090805     C* Se bolla Disk B => come "chi sono" forzo il segnacollo Bartolini (15 chars)
024100090805     C                   IF        VTBNRS > *zeros
024200090805     C                   EVAL      VTBChiSono = %TRIMR(Col(i))
024300090805     C                   ENDIF
024400091207 xxx C*
024500091207 xxx C                   if        vtbksu = *blanks
024600091207 xxx C                   eval      rwrinew = rwrinew + 1
024700091207 xxx C                   endif
024800091207 xxx C*
024900100308     C                   IF        VTBChiSono <> *blanks
025000040426     C                   WRITE     TIVTB000
025100040426     C                   EVAL      RecWrtOF1 = RecWrtOF1 + 1                    Righe scritte
025200100308     C                   ENDIF
025300040426     C                   IF        i = %ELEM(Col)
025400040426     C                   LEAVE
025500040426     C                   ENDIF
025600040426     C                   ENDDO
025700040426
025800040426     C                   IF        Operazione = FineDeiDati
025900040426     C                   LEAVE
026000040426     C                   ENDIF
026100040426
026200040426     C                   EVAL      Operazione = RichiestaSuccessiva
026300040426
026400040426     C                   ENDDO
026500040426
026600040520     **?Se sto usando il file in QTEMP lo chiudo per permettere al chiamante di fare CLRPFM.
026700040520     **?Se invece non sto usando il file in QTEMP lo lascio aperto perchè sto popolando il TIVTB00F
026800040520     **?residente e devo elaborare migliaia di spedizioni.
026900040520     C                   IF        LibIF1 = 'QTEMP'
027000040427     C                   CLOSE     TIVTB00F
027100040520     C                   ENDIF
027200040426
027300040426     C                   ENDSR
027400040423
027500040423     ***********************************************************************************************
027600040423     **?
027700040426     **?Reperisco i dati della spedizione.
027800040423     **?
027900040423     ***********************************************************************************************
028000040423     C     GetBolla      BEGSR
028100040423
028200040426     C                   EVAL      TASLnP = LnPIF1
028300040426     C                   EVAL      TASNrS = NrSIF1
028400040426     C                   EVAL      TASNSp = NSpIF1
028500040426     C                   EVAL      TASTBl = TBlIF1
028600040423
028700040426     **?Se non ho ricevuto l'anno faccio 2 tentativi, con l'anno corrente e precedente.
028800040426     C                   IF        AASIF1 = 0
028900040423     C                   EVAL      TASAAS = *YEAR
029000040426     C                   ELSE
029100090917     C                   EVAL      TASAAS = AASIF1
029200040423     C                   ENDIF
029300040423
029400040520     C                   IF        NOT %OPEN(TITAS30C)
029500040423     C                   OPEN      TITAS30C
029600040520     C                   ENDIF
029700040426
029800040423     C                   DO        2
029900040426     C                   IF        TBlIF1 <> *BLANK
030000040423     C     K05TAS30      CHAIN     TITAS30C
030100040423     C                   ELSE
030200040423     C     K04TAS30      CHAIN     TITAS30C
030300040423     C                   ENDIF
030400040426     C                   IF        %FOUND
030500040426     C                   EVAL      AASIF1 = TASAAS
030600040426     C                   EVAL      TBlIF1 = TASTBl
030700040426     C                   LEAVE
030800040426     C                   ELSE
030900040426     C                   IF        AASIF1 = 0
031000040423     C                   EVAL      TASAAS = TASAAS - 1
031100040426     C                   ELSE
031200040426     C                   LEAVE
031300040426     C                   ENDIF
031400040426     C                   ENDIF
031500040423     C                   ENDDO
031600040423
031700040520     C                   IF        LibIF1 = 'QTEMP'
031800040423     C                   CLOSE     TITAS30C
031900040520     C                   ENDIF
032000040423
032100040426     **?Spedizione non trovata.
032200040423     C                   IF        NOT %FOUND
032300040426     C                   EVAL      EsitoOF1 = Errore
032400040423     C                   EXSR      Uscita
032500040423     C                   ENDIF
032600040423
032700040423     C                   ENDSR
032800040423
032900040423     ***********************************************************************************************
033000040423     **?
033100040423     **?Uscita dal programma.
033200040423     **?
033300040423     ***********************************************************************************************
033400040423     C     Uscita        BEGSR
033500040427
033600040426     C                   IF        EsitoOF1 = *BLANK
033700040426     C                   EVAL      EsitoOF1 = Eseguito
033800040426     C                   ENDIF
033900040426
034000040426     C                   IF        CommitIF1 = *ON
034100040426     C                   IF        EsitoOF1 = Eseguito
034200040426     C                   COMMIT
034300040426     C                   ELSE
034400040426     C                   ROLBK
034500040426     C                   ENDIF
034600040426     C                   ENDIF
034700040426
034800040426     C                   IF        EsitoOF1 = Errore
034900040423     C                   EVAL      *INLR = *ON
035000040423     C                   ENDIF
035100040423
035200040423     C                   RETURN
035300040423
035400040423     C                   ENDSR
