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
007500100312     ** Forzo data iniziale a 3 mesi
007600100312     ** per diminuire la probabilità di trovare più di una spedizione.
007700100312     C                   IF        DInI76 = '01.01.0001'
007800100312     C                   TIME                    WrkDInEur
007900100312     C                   SUBDUR    3:*M          WrkDInEur
008000100312     C                   ELSE
008100100312     C     '/':'.'       XLATE     DInI76        DInI76
008200100312     C                   ENDIF
008300100312
008400100312     ** Gli altri parametri sono già stati controllati da TIS7760R.
008500100312
008600040407     C                   ENDSR
008700040407
008800040407     ***********************************************************************************************
008900040407     **?
009000040407     **?Esegue ricerca.
009100040407     **?
009200040407     ***********************************************************************************************
009300040407     C     RunRicerca    BEGSR
009400040407
009500040407     C                   SELECT
009600040407
009700040407     C                   WHEN      Operazione = NuovaRichiesta
009800040409     **?Rilascio la memoria già allocata per la schiera SPE.
009900040409     C                   DEALLOC(N)              pSpe
010000040409     C                   RESET                   LastSpe
010100040407     C
010200040407     C                   IF        NOT %OPEN(TITAH31C)
010300040407     C                   OPEN      TITAH31C
010400040407     C                   ENDIF
010500040407     C
010600040407     C                   IF        NOT %OPEN(TITAS30C)
010700040407     C                   OPEN      TITAS30C
010800040407     C                   ENDIF
010900040407     C
011000040409     C     K02TAH31      SETLL     TITAH31C
011100040409     C                   EXSR      GetSpedizione
011200040407
011300040407     C                   WHEN      Operazione = RichiestaSuccessiva
011400040409     C                   EXSR      GetSpedizione
011500040407
011600040407     C                   ENDSL
011700040407
011800040407     C                   ENDSR
011900040407
012000040407     ***********************************************************************************************
012100040407     **?
012200040409     **?Reperimento spedizione e impostazione TIS173.
012300040407     **?
012400040407     ***********************************************************************************************
012500040409     C     GetSpedizione BEGSR
012600040407
012700040407     C                   DO        *HIVAL
012800040407
012900040409     C                   RESET                   SpeValida
013000040407     C                   EXSR      GetTITAH
013100040407
013200040409     C                   IF        Operazione = DatiFiniti
013300040407     C                   LEAVE
013400040407     C                   ENDIF
013500040409
013600040409     C                   IF        SpeValida  = *OFF
013700040409     C                   ITER
013800040409     C                   ENDIF
013900040407
014000040407     C     K04TAS30      SETLL     TITAS30C
014100040407
014200040407     C                   DO        *HIVAL
014300040407     C
014400040407     C                   EXSR      GetTITAS
014500040407     C
014600040407     C                   SELECT
014700040408     C                   WHEN      %EOF(TITAS30C)
014800040407     C                   LEAVE
014900040409     **?Verifico ulteriori parametri di selezione.
015000040409     C                   WHEN      WrkDSpEur < WrkDInEur OR
015100040409     C                             WrkDSpEur > WrkDFiEur OR
015200040506     C                             (CCMI76 <> *BLANK AND
015300040506     C                              CCMI76 <> %EDITC(TASCCM:'X'))
015400040407     C                   ITER
015500040409     C                   OTHER
015600040409     C                   EVAL      SpeValida  = *ON
015700040407     C                   LEAVE
015800040409     C                   ENDSL
015900040407     C
016000040407     C                   ENDDO
016100040407
016200040407     **?Spedizione valida, aggiungo una riga.
016300040409     C                   IF        SpeValida  = *ON
016400040409     C
016500040407     C                   EVAL      RigheOut = RigheOut + 1
016600040407     C                   CLEAR                   TIS173DSO
016700040409     C                   EVAL      SpeO73 = WrkSpe                              ID spedizione
016800040408     C                   EVAL      DatO73 = %CHAR(WrkDSpEur)                    Data spedizione
016900040408     C                   EVAL      DesO73 = TASLoD + ' ' + TASPrD               Destinazione
017000040409     C                   EVAL      RifO73 = %TRIML(%EDITC(TASRMN:'Z'))          Riferimento numerico
017100040408     C                   EVAL      TIS173(RigheOut) = TIS173DSO
017200040409     **?Alloco la memoria necessaria a contenere gli elementi pieni
017300040409     **?poi carico la spedizione nella schiera.
017400040409     C                   EVAL      LastSpe = LastSpe + 1
017500040409     C                   EVAL      nSize = %SIZE(Spe) * LastSpe
017600040409     C                   IF        LastSpe = 1
017700040409     C                   ALLOC     nSize         pSpe                           Prima allocazione
017800040409     C                   ELSE
017900040409     C                   REALLOC   nSize         pSpe                           Successive allocaz.
018000040409     C                   ENDIF
018100040409     C                   EVAL      Spe(LastSpe) = WrkSpe
018200040409     C
018300040409     C                   ENDIF
018400040407
018500040409     C                   SELECT
018600040409     C                   WHEN      LastSpe = %ELEM(Spe)
018700040409     C                   EVAL      Operazione = DatiFiniti
018800040409     C                   LEAVE
018900040409     C                   WHEN      RigheOut = RigheRic
019000040409     C                   LEAVE
019100040409     C                   ENDSL
019200040407
019300040407     C                   ENDDO
019400040407
019500040407     C                   ENDSR
019600040407
019700040407     ***********************************************************************************************
019800040407     **?
019900040407     **?Lettura TITAH31C.
020000040407     **?
020100040407     ***********************************************************************************************
020200040407     C     GetTITAH      BEGSR
020300040407
020400040409     C     K02TAH31      READE     TITAH31C
020500040407
020600040409     C                   IF        %EOF
020700040409     C                   EVAL      Operazione = DatiFiniti
020800040409     C                   ELSE
020900040409     C                   EVAL      WrkSpe = %EDITC(TAHLnP:'X')                  ID spedizione
021000040409     C                             + %EDITC(TAHNrS:'X') + %EDITC(TAHNSp:'X')
021100040409     C                   EXSR      ChkSpeUnivoca
021200040407     C                   ENDIF
021300040407
021400040407     C                   ENDSR
021500040407
021600040407     ***********************************************************************************************
021700040407     **?
021800040407     **?Lettura TITAS30C.
021900040407     **?
022000040407     ***********************************************************************************************
022100040407     C     GetTITAS      BEGSR
022200040407
022300040407     C     K04TAS30      READE     TITAS30C
022400040407
022500040407     C                   IF        %EOF
022600040408     C                   CLEAR                   WrkDSpEur
022700040407     C                   ELSE
022800040409     C                   EVAL      WrkDSp = (TASAAS * 10000) + TASMGS           Data spedizione
022900040409     C     *ISO          MOVE      WrkDSp        WrkDSpEur
023000040407     C                   ENDIF
023100040407
023200040407     C                   ENDSR
023300040409
023400040409     ***********************************************************************************************
023500040409     **?
023600040409     **?Controllo spedizione già trovata.
023700040409     **?
023800040409     ***********************************************************************************************
023900040409     C     ChkSpeUnivoca BEGSR
024000040409
024100040409     C                   IF        SpeValida <> *OFF
024200040409
024300040409     **?Scarto la spedizione già selezionata.
024400040409     C                   DO        LastSpe       I
024500040409     C                   IF        WrkSpe = Spe(I)
024600040409     C                   EVAL      SpeValida  = *OFF
024700040409     C                   LEAVE
024800040409     C                   ENDIF
024900040409     C                   ENDDO
025000040409
025100040409     C                   ENDIF
025200040409
025300040409     C                   ENDSR
025400040407
025500040407     ***********************************************************************************************
025600040407     **?
025700040407     **?
025800040407     **?
025900040407     ***********************************************************************************************
026000040407     C     *INZSR        BEGSR
026100040407
026200040407     C     K02TAH31      KLIST
026300040407     C                   KFLD                    TAHTRC
026400040409     C                   KFLD                    ChiSonoI76
026500040407
026600040407     C                   EVAL      TAHTRC = ChiSono
026700040407
026800040407     C     K04TAS30      KLIST
026900040407     C                   KFLD                    TAHAAS
027000040407     C                   KFLD                    TAHLNP
027100040407     C                   KFLD                    TAHNRS
027200040407     C                   KFLD                    TAHNSP
027300040407
027400040407     C                   ENDSR
027500040407
027600040407     ***********************************************************************************************
027700040407     **?
027800040407     **?Uscita dal programma.
027900040407     **?
028000040407     ***********************************************************************************************
028100040407     C     Uscita        BEGSR
028200040407
028300040407     C                   IF        Esito = *BLANK
028400040408     C                   MOVEA     TIS173        DatiOut
028500040407     C                   EVAL      Esito = Eseguito
028600040407     C                   ENDIF
028700040407
028800040409     C                   IF        Operazione = DatiFiniti
028900040409     C                   DEALLOC(N)              pSpe
029000040409     C                   EVAL      *INLR = *ON
029100040409     C                   ENDIF
029200040409
029300040407     C                   RETURN
029400040407
029500040407     C                   ENDSR
