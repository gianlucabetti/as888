000100040409      /TITLE Cerca spedizioni con codice "chi sono" per utenti anonimi.
000200040407
000300040407     FTITAH31C  IF   E           K DISK
000400040407     F                                     USROPN
000500040407     FTITAS30C  IF   E           K DISK
000600040407     F                                     USROPN
000700040407
000800040407     D Eseguito        C                   '0'
000900040407     D Errore          C                   '1'
001000040407     D NuovaRichiesta  C                   '1'
001100040407     D RichiestaSuccessiva...
001200040407     D                 C                   '2'
001300040409     D DatiFiniti      C                   '9'
001400040407     D ChiSono         C                   'C'
001500040407     D RighePagina     C                   20
001600040407
001700040407     D TIS776DSI     E DS
001800040409     D  WrkDInEur                      D
001900040409     D                                     DATFMT(*EUR)
002000040409     D                                     OVERLAY(DInI76)
002100040409     D  WrkDFiEur                      D
002200040409     D                                     DATFMT(*EUR)
002300040409     D                                     OVERLAY(DFiI76)
002400040408     D TIS173DSO     E DS
002500040408     D                                     INZ
002600040407
002700040409     D TIS173          S
002800040409     D                                     LIKE(TIS173DSO)
002900040409     D                                     DIM(RighePagina)
003000040409     D Spe             S                                                        ID spedizioni
003100040409     D                                     LIKE(SpeO73)
003200040409     D                                     DIM(32767)
003300040409     D                                     BASED(pSpe)
003400040409
003500040407     D Operazione      S              1
003600040407     D RigheRic        S              5S 0
003700040408     D DatiOut         S           9999
003800040408     D RigheOut        S              5S 0
003900040407     D Esito           S              1
004000040409     D SpeValida       S              1
004100040409     D WrkDSp          S              8  0
004200040408     D WrkDSpEur       S               D
004300040407     D                                     DATFMT(*EUR)
004400040409     D WrkSpe          S
004500040409     D                                     LIKE(SpeO73)
004600040409     D pSpe            S               *
004700040409     D nSize           S             10I 0
004800040409     D I               S              5U 0
004900040409     D LastSpe         S              5U 0                                      Ultimo elemento Spe
005000040407
005100040407     ***********************************************************************************************
005200040407     **?
005300040407     **?
005400040407     **?
005500040407     ***********************************************************************************************
005600040407     C     *ENTRY        PLIST
005700040407     C                   PARM                    TIS776DSI
005800040407     C                   PARM                    Operazione
005900040407     C                   PARM                    RigheRic
006000040408     C                   PARM                    DatiOut
006100040407     C                   PARM                    RigheOut
006200040407     C                   PARM                    Esito
006300040407
006400040409     C                   EXSR      ChkParm
006500040407     C                   EXSR      RunRicerca
006600040407     C                   EXSR      Uscita
006700040407
006800040407     ***********************************************************************************************
006900040407     **?
007000040409     **?Controllo parametri ricevuti.
007100040407     **?
007200040407     ***********************************************************************************************
007300040409     C     ChkParm       BEGSR
007400040407
007500040409     **?I parametri sono già stati controllati da TIS7760R.
007600040407
007700040407     C                   ENDSR
007800040407
007900040407     ***********************************************************************************************
008000040407     **?
008100040407     **?Esegue ricerca.
008200040407     **?
008300040407     ***********************************************************************************************
008400040407     C     RunRicerca    BEGSR
008500040407
008600040407     C                   SELECT
008700040407
008800040407     C                   WHEN      Operazione = NuovaRichiesta
008900040409     **?Rilascio la memoria già allocata per la schiera SPE.
009000040409     C                   DEALLOC(N)              pSpe
009100040409     C                   RESET                   LastSpe
009200040407     C
009300040407     C                   IF        NOT %OPEN(TITAH31C)
009400040407     C                   OPEN      TITAH31C
009500040407     C                   ENDIF
009600040407     C
009700040407     C                   IF        NOT %OPEN(TITAS30C)
009800040407     C                   OPEN      TITAS30C
009900040407     C                   ENDIF
010000040407     C
010100040409     C     K02TAH31      SETLL     TITAH31C
010200040409     C                   EXSR      GetSpedizione
010300040407
010400040407     C                   WHEN      Operazione = RichiestaSuccessiva
010500040409     C                   EXSR      GetSpedizione
010600040407
010700040407     C                   ENDSL
010800040407
010900040407     C                   ENDSR
011000040407
011100040407     ***********************************************************************************************
011200040407     **?
011300040409     **?Reperimento spedizione e impostazione TIS173.
011400040407     **?
011500040407     ***********************************************************************************************
011600040409     C     GetSpedizione BEGSR
011700040407
011800040407     C                   DO        *HIVAL
011900040407
012000040409     C                   RESET                   SpeValida
012100040407     C                   EXSR      GetTITAH
012200040407
012300040409     C                   IF        Operazione = DatiFiniti
012400040407     C                   LEAVE
012500040407     C                   ENDIF
012600040409
012700040409     C                   IF        SpeValida  = *OFF
012800040409     C                   ITER
012900040409     C                   ENDIF
013000040407
013100040407     C     K04TAS30      SETLL     TITAS30C
013200040407
013300040407     C                   DO        *HIVAL
013400040407     C
013500040407     C                   EXSR      GetTITAS
013600040407     C
013700040407     C                   SELECT
013800040408     C                   WHEN      %EOF(TITAS30C)
013900040407     C                   LEAVE
014000040409     **?Verifico ulteriori parametri di selezione.
014100040409     C                   WHEN      WrkDSpEur < WrkDInEur OR
014200040409     C                             WrkDSpEur > WrkDFiEur OR
014300040506     C                             (CCMI76 <> *BLANK AND
014400040506     C                              CCMI76 <> %EDITC(TASCCM:'X'))
014500040407     C                   ITER
014600040409     C                   OTHER
014700040409     C                   EVAL      SpeValida  = *ON
014800040407     C                   LEAVE
014900040409     C                   ENDSL
015000040407     C
015100040407     C                   ENDDO
015200040407
015300040407     **?Spedizione valida, aggiungo una riga.
015400040409     C                   IF        SpeValida  = *ON
015500040409     C
015600040407     C                   EVAL      RigheOut = RigheOut + 1
015700040407     C                   CLEAR                   TIS173DSO
015800040409     C                   EVAL      SpeO73 = WrkSpe                              ID spedizione
015900040408     C                   EVAL      DatO73 = %CHAR(WrkDSpEur)                    Data spedizione
016000040408     C                   EVAL      DesO73 = TASLoD + ' ' + TASPrD               Destinazione
016100040409     C                   EVAL      RifO73 = %TRIML(%EDITC(TASRMN:'Z'))          Riferimento numerico
016200040408     C                   EVAL      TIS173(RigheOut) = TIS173DSO
016300040409     **?Alloco la memoria necessaria a contenere gli elementi pieni
016400040409     **?poi carico la spedizione nella schiera.
016500040409     C                   EVAL      LastSpe = LastSpe + 1
016600040409     C                   EVAL      nSize = %SIZE(Spe) * LastSpe
016700040409     C                   IF        LastSpe = 1
016800040409     C                   ALLOC     nSize         pSpe                           Prima allocazione
016900040409     C                   ELSE
017000040409     C                   REALLOC   nSize         pSpe                           Successive allocaz.
017100040409     C                   ENDIF
017200040409     C                   EVAL      Spe(LastSpe) = WrkSpe
017300040409     C
017400040409     C                   ENDIF
017500040407
017600040409     C                   SELECT
017700040409     C                   WHEN      LastSpe = %ELEM(Spe)
017800040409     C                   EVAL      Operazione = DatiFiniti
017900040409     C                   LEAVE
018000040409     C                   WHEN      RigheOut = RigheRic
018100040409     C                   LEAVE
018200040409     C                   ENDSL
018300040407
018400040407     C                   ENDDO
018500040407
018600040407     C                   ENDSR
018700040407
018800040407     ***********************************************************************************************
018900040407     **?
019000040407     **?Lettura TITAH31C.
019100040407     **?
019200040407     ***********************************************************************************************
019300040407     C     GetTITAH      BEGSR
019400040407
019500040409     C     K02TAH31      READE     TITAH31C
019600040407
019700040409     C                   IF        %EOF
019800040409     C                   EVAL      Operazione = DatiFiniti
019900040409     C                   ELSE
020000040409     C                   EVAL      WrkSpe = %EDITC(TAHLnP:'X')                  ID spedizione
020100040409     C                             + %EDITC(TAHNrS:'X') + %EDITC(TAHNSp:'X')
020200040409     C                   EXSR      ChkSpeUnivoca
020300040407     C                   ENDIF
020400040407
020500040407     C                   ENDSR
020600040407
020700040407     ***********************************************************************************************
020800040407     **?
020900040407     **?Lettura TITAS30C.
021000040407     **?
021100040407     ***********************************************************************************************
021200040407     C     GetTITAS      BEGSR
021300040407
021400040407     C     K04TAS30      READE     TITAS30C
021500040407
021600040407     C                   IF        %EOF
021700040408     C                   CLEAR                   WrkDSpEur
021800040407     C                   ELSE
021900040409     C                   EVAL      WrkDSp = (TASAAS * 10000) + TASMGS           Data spedizione
022000040409     C     *ISO          MOVE      WrkDSp        WrkDSpEur
022100040407     C                   ENDIF
022200040407
022300040407     C                   ENDSR
022400040409
022500040409     ***********************************************************************************************
022600040409     **?
022700040409     **?Controllo spedizione già trovata.
022800040409     **?
022900040409     ***********************************************************************************************
023000040409     C     ChkSpeUnivoca BEGSR
023100040409
023200040409     C                   IF        SpeValida <> *OFF
023300040409
023400040409     **?Scarto la spedizione già selezionata.
023500040409     C                   DO        LastSpe       I
023600040409     C                   IF        WrkSpe = Spe(I)
023700040409     C                   EVAL      SpeValida  = *OFF
023800040409     C                   LEAVE
023900040409     C                   ENDIF
024000040409     C                   ENDDO
024100040409
024200040409     C                   ENDIF
024300040409
024400040409     C                   ENDSR
024500040407
024600040407     ***********************************************************************************************
024700040407     **?
024800040407     **?
024900040407     **?
025000040407     ***********************************************************************************************
025100040407     C     *INZSR        BEGSR
025200040407
025300040407     C     K02TAH31      KLIST
025400040407     C                   KFLD                    TAHTRC
025500040409     C                   KFLD                    ChiSonoI76
025600040407
025700040407     C                   EVAL      TAHTRC = ChiSono
025800040407
025900040407     C     K04TAS30      KLIST
026000040407     C                   KFLD                    TAHAAS
026100040407     C                   KFLD                    TAHLNP
026200040407     C                   KFLD                    TAHNRS
026300040407     C                   KFLD                    TAHNSP
026400040407
026500040407     C                   ENDSR
026600040407
026700040407     ***********************************************************************************************
026800040407     **?
026900040407     **?Uscita dal programma.
027000040407     **?
027100040407     ***********************************************************************************************
027200040407     C     Uscita        BEGSR
027300040407
027400040407     C                   IF        Esito = *BLANK
027500040408     C                   MOVEA     TIS173        DatiOut
027600040407     C                   EVAL      Esito = Eseguito
027700040407     C                   ENDIF
027800040407
027900040409     C                   IF        Operazione = DatiFiniti
028000040409     C                   DEALLOC(N)              pSpe
028100040409     C                   EVAL      *INLR = *ON
028200040409     C                   ENDIF
028300040409
028400040407     C                   RETURN
028500040407
028600040407     C                   ENDSR
