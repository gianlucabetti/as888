000100040423      /TITLE Elenco dei colli di una spedizione.
000200040426
000300040426     H DFTACTGRP(*NO) ACTGRP(*CALLER) OPTION(*NODEBUGIO)
000400040423
000500040426     FTITAS30C  IF   E           K DISK                                         Spedizioni
000600040423     F                                     USROPN
000700040426     FTITAT30C  IF   E           K DISK                                         Colli non sequenz.
000800040423     F                                     USROPN
000900040426     FTITAH30C  IF   E           K DISK                                         Chi sono
001000040423     F                                     USROPN
001100040423
001200040423     D Eseguito        C                   '0'
001300040423     D Errore          C                   '1'
001400040423     D NuovaRichiesta  C                   '1'
001500040423     D RichiestaSuccessiva...
001600040423     D                 C                   '2'
001700040426     D FineDeiDati     C                   '9'
001800040423     D ChiSono         C                   'C'
001900040426     D ColliSequenziali...
002000040426     D                 C                   ' '
002100040426     D ColliNonSequenziali...
002200040426     D                 C                   'S'
002300040423
002400040423     D TIS7F0DSI     E DS
002500040423     D TIS7F0DSO     E DS
002600040426     D  Col                          15
002700040426     D                                     DIM(25)
002800040426     D                                     OVERLAY(IdColloOF0)
002900040426     D  Chi
003000040423     D                                     LIKE(TAHNot)
003100040426     D                                     DIM(%ELEM(Col))
003200040426     D                                     OVERLAY(ChiSonoOF0)
003300040423
003400040423     D PrmOperazione   S              1
003500040423     D PrmRigheOut     S              3  0
003600040423     D PrmEsito        S              1
003700040423     D i               S              5I 0
003800040426     D WrkCollo        S
003900091201     D                                     LIKE(TASNCD)
004000040423
004100040423     ***********************************************************************************************
004200040423     **?
004300040423     **?Main.
004400040423     **?
004500040423     ***********************************************************************************************
004600040423     C     *ENTRY        PLIST
004700040423     C                   PARM                    TIS7F0DSI
004800040423     C                   PARM                    PrmOperazione
004900040423     C                   PARM                    TIS7F0DSO
005000040426     C                   PARM      i             PrmRigheOut
005100040423     C                   PARM                    PrmEsito
005200040423
005300040423     C                   EXSR      ChkParm
005400040423
005500040426     C                   IF        PrmOperazione = NuovaRichiesta
005600040426     C                   EXSR      NewRequest
005700040423     C                   ENDIF
005800040426
005900040426     C                   EXSR      GetColli
006000040423
006100040423     C                   EXSR      Uscita
006200040423
006300040423     ***********************************************************************************************
006400040423     **?
006500040506     **?Operazioni iniziali.
006600040423     **?
006700040423     ***********************************************************************************************
006800040423     C     *INZSR        BEGSR
006900040423
007000040423     C     K05TAS30      KLIST
007100040423     C                   KFLD                    TASAAS
007200040423     C                   KFLD                    TASLnP
007300040423     C                   KFLD                    TASNrS
007400040423     C                   KFLD                    TASNSp
007500040423     C                   KFLD                    TASTBl
007600040423
007700040423     C     K04TAS30      KLIST
007800040423     C                   KFLD                    TASAAS
007900040423     C                   KFLD                    TASLnP
008000040423     C                   KFLD                    TASNrS
008100040423     C                   KFLD                    TASNSp
008200040423
008300040423     C     K04TAT30      KLIST
008400040423     C                   KFLD                    TASAAS
008500040423     C                   KFLD                    TASLnP
008600040423     C                   KFLD                    TASNrS
008700040423     C                   KFLD                    TASNSp
008800040423
008900040423     C     K05TAH30      KLIST
009000040423     C                   KFLD                    TASAAS
009100040423     C                   KFLD                    TASLnP
009200040423     C                   KFLD                    TASNrS
009300040423     C                   KFLD                    TASNSp
009400040423     C                   KFLD                    TAHTRc
009500040423
009600040423     C                   EVAL      TAHTRc = ChiSono
009700040423
009800040423     C                   ENDSR
009900040423
010000040423     ***********************************************************************************************
010100040423     **?
010200040423     **?Controllo parametri ricevuti.
010300040423     **?
010400040423     ***********************************************************************************************
010500040423     C     ChkParm       BEGSR
010600040423
010700040426     **?Pulisco i parametri di output.
010800040423     C                   CLEAR                   TIS7F0DSO
010900040423     C                   CLEAR                   PrmRigheOut
011000040423     C                   CLEAR                   PrmEsito
011100040423
011200040423     **?Parametri obbligatori.
011300040423     C                   IF        LnPIF0 = 0 OR NSpIF0 = 0
011400040423     C                   EVAL      PrmEsito = Errore
011500040423     C                   EXSR      Uscita
011600040423     C                   ENDIF
011700040426
011800040426     **?Se rispetto alla chiamata precedente l'ID spedizione è cambiato non può che essere una
011900040426     **?nuova richiesta. Non testo l'anno perchè posso riceverlo vuoto.
012000040426     C                   IF        LnPIF0 <> TASLnP AND NrSIF0 <> TASNrS AND
012100040426     C                             NSpIF0 <> TASNSp
012200040426     C                   EVAL      PrmOperazione = NuovaRichiesta
012300040426     C                   ENDIF
012400040423
012500040423     C                   ENDSR
012600040426
012700040426     ***********************************************************************************************
012800040426     **?
012900040426     **?Nuova richiesta.
013000040426     **?
013100040426     ***********************************************************************************************
013200040426     C     NewRequest    BEGSR
013300040426
013400040426     C                   EXSR      ResetPgm                                     Resetto variabili
013500040426     C                   EXSR      GetBolla                                     Reperisco spedizione
013600040426
013700040426     **?Controllo se esistono i "chi sono".
013800040426     C                   IF        NOT %OPEN(TITAH30C)
013900040426     C                   OPEN      TITAH30C
014000040426     C                   ENDIF
014100040426
014200040426     C     K05TAH30      SETLL     TITAH30C
014300040426
014400040426     C                   IF        %EQUAL                                       Esistono chisono
014500040426     C                   IF        %OPEN(TITAT30C)
014600040426     C                   CLOSE     TITAT30C
014700040426     C                   ENDIF
014800040426     C                   ELSE                                                   Non esistono chisono
014900040426     C                   CLOSE     TITAH30C
015000040426     C                   IF        TASFNS = ColliNonSequenziali
015100040426     C                   IF        NOT %OPEN(TITAT30C)
015200040426     C                   OPEN      TITAT30C
015300040426     C                   ENDIF
015400040426     C     K04TAT30      SETLL     TITAT30C
015500040426     C                   IF        NOT %EQUAL                                   Non esistono colli
015600040426     C                   EVAL      PrmOperazione = FineDeiDati
015700040426     C                   EXSR      Uscita
015800040426     C                   ENDIF
015900040426     C                   ENDIF
016000040426     C                   ENDIF
016100040426
016200040426     C                   ENDSR
016300040426
016400040426     ***********************************************************************************************
016500040426     **?
016600040426     **?Resetto le variabili del programma per una nuova richiesta.
016700040426     **?
016800040426     ***********************************************************************************************
016900040426     C     ResetPgm      BEGSR
017000040426
017100040426     C                   CLEAR                   WrkCollo
017200040426     C                   CLEAR                   i
017300040426
017400040426     C                   ENDSR
017500040423
017600040423     ***********************************************************************************************
017700040423     **?
017800040426     **?Reperisco i dati della spedizione.
017900040423     **?
018000040423     ***********************************************************************************************
018100040423     C     GetBolla      BEGSR
018200040423
018300040423     C                   EVAL      TASLnP = LnPIF0
018400040423     C                   EVAL      TASNrS = NrSIF0
018500040423     C                   EVAL      TASNSp = NSpIF0
018600040423     C                   EVAL      TASTBl = TBlIF0
018700040423
018800040426     **?Se non ho ricevuto l'anno faccio 2 tentativi, con l'anno corrente e precedente.
018900040423     C                   IF        AASIF0 = 0
019000040423     C                   EVAL      TASAAS = *YEAR
019100040426     C                   ELSE
019200040426     C                   EVAL      TASAAS = AASIF0
019300040423     C                   ENDIF
019400040423
019500040423     C                   OPEN      TITAS30C
019600040426
019700040423     C                   DO        2
019800040423     C                   IF        TBlIF0 <> *BLANK
019900040423     C     K05TAS30      CHAIN     TITAS30C
020000040423     C                   ELSE
020100040423     C     K04TAS30      CHAIN     TITAS30C
020200040423     C                   ENDIF
020300040426     C                   IF        %FOUND
020400040426     C                   EVAL      AASIF0 = TASAAS
020500040426     C                   EVAL      TBlIF0 = TASTBl
020600040426     C                   LEAVE
020700040426     C                   ELSE
020800040426     C                   IF        AASIF0 = 0
020900040423     C                   EVAL      TASAAS = TASAAS - 1
021000040426     C                   ELSE
021100040426     C                   LEAVE
021200040426     C                   ENDIF
021300040426     C                   ENDIF
021400040423     C                   ENDDO
021500040423
021600040423     C                   CLOSE     TITAS30C
021700040423
021800040426     **?Spedizione non trovata.
021900040423     C                   IF        NOT %FOUND
022000040423     C                   EVAL      PrmEsito = Errore
022100040423     C                   EXSR      Uscita
022200040423     C                   ENDIF
022300040426
022400040426     **?Se c'è solo 1 collo, imposto il collo finale uguale a quello iniziale.
022500040426     C                   IF        TASFNS = ColliSequenziali
022600040426     C                   EVAL      WrkCollo = TASNCD
022700040426     C                   IF        TASNCA = 0
022800040426     C                   EVAL      TASNCA = TASNCD
022900040426     C                   ENDIF
023000040426     C                   ENDIF
023100040423
023200040423     C                   ENDSR
023300040423
023400040423     ***********************************************************************************************
023500040423     **?
023600040423     **?Reperisco i colli della spedizione.
023700040423     **?
023800040423     ***********************************************************************************************
023900040423     C     GetColli      BEGSR
024000040423
024100040426     **?Se esistono i "chi sono" li reperisco, altrimenti reperisco i colli.
024200040426
024300040426     C                   IF        %OPEN(TITAH30C)
024400040423     C                   EXSR      GetChiSono
024500040423     C                   ELSE
024600040426     C                   IF        TASFNS = ColliSequenziali
024700040423     C                   EXSR      GetColliSeq                                  Colli sequenziali
024800040423     C                   ELSE
024900040423     C                   EXSR      GetColliTAT                                  Colli no sequenziali
025000040423     C                   ENDIF
025100040423     C                   ENDIF
025200040423
025300040423     C                   ENDSR
025400040423
025500040423     ***********************************************************************************************
025600040423     **?
025700040426     **?Compongo l'elenco dei colli sequenziali.
025800040423     **?
025900040423     ***********************************************************************************************
026000040423     C     GetColliSeq   BEGSR
026100040506
026200040506     C                   IF        TASNCD = 0                                   Non ci sono colli
026300040506
026400040506     C                   EVAL      PrmOperazione = FineDeiDati
026500040506
026600040506     C                   ELSE
026700040423
026800040426     C                   CLEAR                   i
026900040426
027000040426     C                   DO        *HIVAL
027100040423
027200040426     C                   EVAL      i = i + 1
027300040423     C                   EVAL      Col(i) = %EDITC(TASFlS:'X') +
027400100224     C                             %EDITC(TASLnA:'X')          +
027500100224     C                             %EDITC(TASNrS:'X') + %EDITC(WrkCollo:'X')
027600040423
027700040426     C                   SELECT
027800040426     C                   WHEN      WrkCollo = TASNCA
027900040426     C                   EVAL      PrmOperazione = FineDeiDati
028000040423     C                   LEAVE
028100040426     C                   WHEN      i = %ELEM(Col)
028200040426     C                   EVAL      WrkCollo = WrkCollo + 1
028300040426     C                   LEAVE
028400040426     C                   OTHER
028500040423     C                   EVAL      WrkCollo = WrkCollo + 1
028600040426     C                   ENDSL
028700040423
028800040423     C                   ENDDO
028900040506
029000040506     C                   ENDIF
029100040423
029200040423     C                   ENDSR
029300040423
029400040423     ***********************************************************************************************
029500040423     **?
029600040426     **?Compongo l'elenco dei colli non sequenziali.
029700040423     **?
029800040423     ***********************************************************************************************
029900040423     C     GetColliTAT   BEGSR
030000040426
030100040426     C                   CLEAR                   i
030200040423
030300040423     C                   DO        *HIVAL
030400040423     C     K04TAT30      READE     TITAT30C
030500040423
030600040423     C                   IF        %EOF
030700040426     C                   EVAL      PrmOperazione = FineDeiDati
030800040426     C                   CLOSE     TITAT30C
030900040423     C                   LEAVE
031000040423     C                   ENDIF
031100040423
031200040423     C                   EVAL      i = i + 1
031300040423     C                   EVAL      Col(i) = %EDITC(TATFlS:'X') +
031400100308     C                             %EDITC(TATLnA:'X') +
031500100308     C                             %EDITC(TATNrS:'X') + %EDITC(TATNSC:'X')
031600040426
031700040426     C                   IF        i = %ELEM(Col)
031800040426     C                   LEAVE
031900040426     C                   ENDIF
032000040423
032100040423     C                   ENDDO
032200040423
032300040423     C                   ENDSR
032400040423
032500040423     ***********************************************************************************************
032600040423     **?
032700040426     **?Compongo l'elenco dei "chi sono" e dei colli.
032800040423     **?
032900040423     ***********************************************************************************************
033000040423     C     GetChiSono    BEGSR
033100040426
033200040426     C                   CLEAR                   i
033300040423
033400040423     C                   DO        *HIVAL
033500040426     C     K05TAH30      READE     TITAH30C
033600040423
033700040423     C                   IF        %EOF
033800040426     C                   EVAL      PrmOperazione = FineDeiDati
033900040426     C                   CLOSE     TITAH30C
034000040423     C                   LEAVE
034100040423     C                   ENDIF
034200040423
034300040426     C                   EVAL      i = i + 1
034400040426     C                   EVAL      Chi(i) = TAHNot
034500040426     C                   EVAL      Col(i) = %EDITC(TASFlS:'X') +
034600100308     C                             %EDITC(TASLnA:'X') +
034700100308     C                             %EDITC(TAHNrS:'X') + %EDITC(TAHNSc:'X')
034800040426
034900040426     C                   IF        i = %ELEM(Chi)
035000040426     C                   LEAVE
035100040426     C                   ENDIF
035200040423
035300040423     C                   ENDDO
035400040423
035500040423     C                   ENDSR
035600040423
035700040423     ***********************************************************************************************
035800040423     **?
035900040423     **?Uscita dal programma.
036000040423     **?
036100040423     ***********************************************************************************************
036200040423     C     Uscita        BEGSR
036300040423
036400040426     C                   IF        PrmEsito = *BLANK
036500040423     C                   EVAL      PrmEsito = Eseguito
036600040426     C                   ENDIF
036700040426
036800040426     C                   IF        PrmEsito = Errore OR
036900040426     C                             PrmOperazione = FineDeiDati
037000040423     C                   EVAL      *INLR = *ON
037100040423     C                   ENDIF
037200040423
037300040423     C                   RETURN
037400040423
037500040423     C                   ENDSR
