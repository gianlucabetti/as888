000100040407      /TITLE Cerca spedizioni con codice "chi sono".
000200100312     ***************************************************************************
000300100312     **
000400100312     ** Questo programma è chiamato tante volte consecutive quante sono le
000500100312     ** pagine visualizzabili.
000600100312     **
000700100312     ** La prima volta è chiamato con operazione '1', dalla seconda volta in poi
000800100312     ** con operazione '2' e tutte le volte restituisce i dati di una pagina.
000900100312     ** Quando i dati sono finiti restituisce operazione '9'.
001000100312     **
001100100312     ***************************************************************************
001200040407
001300040409     D Eseguito        C                   '0'
001400040409     D Errore          C                   '1'
001500040409     D NuovaRichiesta  C                   '1'
001600040409     D RichiestaSuccessiva...
001700040409     D                 C                   '2'
001800040409     D DatiNonTrovati  C                   '9'
001900040409
002000040407     D TIS776DSI     E DS
002100040409
002200100311     D prmOperazione   S              1
002300100311     D Operazione      S              1
002400040407     D RigheRic        S              5S 0
002500040408     D DatiOut         S           9999
002600040408     D RigheOut        S              5S 0
002700040407     D Esito           S              1
002800040409     D WrkDtEur        S               D
002900040409     D                                     DATFMT(*EUR)
003000100311     D bivio           S              3U 0
003100040407
003200040407     ***********************************************************************************************
003300040407     **?
003400040407     **?
003500040407     **?
003600040407     ***********************************************************************************************
003700040407     C     *ENTRY        PLIST
003800040407     C                   PARM                    TIS776DSI
003900100311     C                   PARM                    prmOperazione
004000040407     C                   PARM                    RigheRic
004100040408     C                   PARM                    DatiOut
004200040407     C                   PARM                    RigheOut
004300040407     C                   PARM                    Esito
004400040407
004500040409     C                   EXSR      ChkParm
004600040409
004700100311     C                   IF        prmOperazione = NuovaRichiesta
004800100311     C                   EVAL      bivio = 61
004900100311     C                   ENDIF
005000100311
005100100311     ** Cerco su TIVTB00F.
005200100311     C                   IF        bivio = 61
005300100311     C                   CALL(E)   'TIS7761R'
005400040409     C                   PARM                    TIS776DSI
005500100311     C                   PARM      prmOperazione operazione
005600040409     C                   PARM                    RigheRic
005700100311     C                   PARM      *BLANK        DatiOut
005800100311     C                   PARM      *ZERO         RigheOut
005900100311     C                   PARM      *BLANK        Esito
006000100312     C                   IF        %ERROR()
006100100312     C                   EVAL      Esito = Errore
006200100312     C                   ELSE
006300100311     C                   IF        prmOperazione = NuovaRichiesta AND
006400100311     C                             operazione = DatiNonTrovati AND
006500100315     C                             righeOut = *ZERO AND
006600100315     C                             esito = Eseguito AND
006700100311     C                             AnonimoI76 = *ON
006800100311     C                   EVAL      bivio = 62
006900100311     C                   ENDIF
007000100312     C                   ENDIF
007100100311     C                   ENDIF
007200100311
007300100311     ** Se per l'utente anonimo ho trovato nulla in TIVTB00F tento su TITAH00F.
007400100311     C                   IF        bivio = 62
007500100311     C                   CALL(E)   'TIS7762R'
007600100311     C                   PARM                    TIS776DSI
007700100311     C                   PARM      prmOperazione operazione
007800100311     C                   PARM                    RigheRic
007900100311     C                   PARM      *BLANK        DatiOut
008000100311     C                   PARM      *ZERO         RigheOut
008100100311     C                   PARM      *BLANK        Esito
008200100312     C                   IF        %ERROR()
008300100312     C                   EVAL      Esito = Errore
008400100312     C                   ENDIF
008500100311     C                   ENDIF
008600040409
008700100311     C                   EVAL      prmOperazione = operazione
008800040407     C                   EXSR      Uscita
008900040407
009000040407     ***********************************************************************************************
009100040407     **?
009200040409     **?Controllo parametri ricevuti.
009300040407     **?
009400040407     ***********************************************************************************************
009500040409     C     ChkParm       BEGSR
009600040408
009700040409     C                   CLEAR                   DatiOut
009800040409     C                   CLEAR                   RigheOut
009900040409     C                   CLEAR                   Esito
010000040409
010100040409     C                   IF        AnonimoI76 = *BLANK
010200100311     C                   IF        rqsCidI76 = *ZERO
010300040409     C                   EVAL      AnonimoI76 = *ON                             Utente anonimo
010400040409     C                   ELSE
010500040409     C                   EVAL      AnonimoI76 = *OFF                            Utente iscritto
010600040409     C                   ENDIF
010700040409     C                   ENDIF
010800040409
010900040409     C                   IF        ParzialI76 = *BLANK
011000040409     C                   EVAL      ParzialI76 = *OFF                            No ricerca parziale
011100040409     C                   ENDIF
011200040409
011300040409     **?Parametri obbligatori e congruenza.
011400040409     C                   IF        ChiSonoI76 = *BLANK OR
011500100311     C                             (prmOperazione <> NuovaRichiesta AND
011600100311     C                              prmOperazione <> RichiestaSuccessiva AND
011700100311     C                              prmOperazione <> DatiNonTrovati) OR
011800100311     C                             (AnonimoI76 = *OFF AND rqsCidI76 = *ZERO) OR
011900040409     C                             RigheRic <= *ZERO
012000040409     C                   EVAL      Esito = Errore
012100040409     C                   EXSR      Uscita
012200040409     C                   ENDIF
012300040409
012400040409     C                   IF        RigheRic > 20
012500040409     C                   EVAL      RigheRic = 20
012600040409     C                   ENDIF
012700040409
012800040409     **?Data iniziale
012900040415     C                   IF        DInI76 = '01.01.0001'
013000100309     C*                  TIME                    WrkDtEur
013100100309     C*                  SUBDUR    3:*M          WrkDtEur
013200100309     C*                  EVAL      DInI76 = %CHAR(WrkDtEur)
013300040409     C                   ELSE
013400040409     C     '/':'.'       XLATE     DInI76        DInI76
013500040409     C                   ENDIF
013600040409
013700040409     **?Data finale.
013800040415     C                   IF        DFiI76 = '31.12.9999'
013900100309     C*                  TIME                    WrkDtEur
014000100309     C*                  EVAL      DFiI76 = %CHAR(WrkDtEur)
014100040409     C                   ELSE
014200040409     C     '/':'.'       XLATE     DFiI76        DFiI76
014300040409     C                   ENDIF
014400040407
014500040407     C                   ENDSR
014600040409
014700040409     ***********************************************************************************************
014800040409     **?
014900040409     **?
015000040409     **?
015100040409     ***********************************************************************************************
015200040409     C     *INZSR        BEGSR
015300040409
015400040409     C                   ENDSR
015500040407
015600040407     ***********************************************************************************************
015700040407     **?
015800040407     **?Uscita dal programma.
015900040407     **?
016000040407     ***********************************************************************************************
016100040407     C     Uscita        BEGSR
016200040407
016300040407     C                   IF        Esito = *BLANK
016400040407     C                   EVAL      Esito = Eseguito
016500040407     C                   ENDIF
016600040409
016700040407     C                   RETURN
016800040407
016900040407     C                   ENDSR
