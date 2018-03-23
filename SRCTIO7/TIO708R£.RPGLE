000100041126     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200041125
000300041125     ***********************************************************************************************
000400041126     **?Questo programma controlla se è possibile eseguire un ORM nella data richiesta
000500041126     **?rispetto al momento in cui l' utente internet immette l' ORM.
000600041125     ***********************************************************************************************
000700041125
000800041126     FAZCLN01L  IF   E           K DISK                                         Calendario
000900041126
001000041126     D Festa           C                   'F'
001100041126     D SiRitiroInGiornata...
001200041126     D                 C                   '01'
001300041126     D NoRitiroInGiornata...
001400041126     D                 C                   '02'
001500041126
001600041125     D TISI95DS      E DS                                                       Controllo CAP-local.
001700041124     D                                     INZ
001800041125     D  I95TCn       E
001900041125     D                                     INZ('3')
002000041124     D TIO107IDS     E DS
002100041124     D                                     BASED(NullPtr)
002200050214     D XSRLav8DS       DS
002300050214     D                                     INZ
002400050214     D  XSRLav8DtDa                   8S 0                                      Da data
002500050214     D  XSRLav8DtA                    8S 0                                      A data
002600050214     D  XSRLav8GioLav                 5S 0                                      Giorni lavorativi
002700041124
002800041125     D NullPtr         S               *                                        Puntatore nullo
002900041124     D SUN             S                                                        Strategi User Number
003000041124     D                                     LIKE(O107RQSCID)
003100041125     D KSU             S                                                        Codice cliente unif.
003200041124     D                                     LIKE(O107KSU)
003300041125     D CRO             S                                                        Codice ritiro
003400041124     D                                     LIKE(O107CRO)
003500041125     D CAR             S                                                        CAP
003600041124     D                                     LIKE(O107CAR)
003700041125     D LoR             S                                                        Località
003800041125     D                                     LIKE(O107LoR)
003900041125     D PrR             S                                                        Provincia
004000041125     D                                     LIKE(O107PrR)
004100041125     D NaR             S                                                        Nazione
004200041125     D                                     LIKE(O107NaR)
004300041125     D DaR             S                                                        Data ritiro
004400041125     D                                     LIKE(O107DaR)
004500041125     D OrR             S                                                        Ora pronta merce
004600041125     D                                     LIKE(O107OrR)
004700041124     D Esito           S              5I 0
004800041124     D Msg             S             80                                         Messaggio errore
004900041124     D dCur            S               D                                        Data corrente
005000041124     D tCur            S               T                                        Ora  corrente
005100041125     D dDomani         S               D                                        Domani
005200041125     D dDaR            S               D                                        Data pronta merce
005300041125     D tOrR            S               T                                        Ora  pronta merce
005400041125     D cOrR            S              6                                         Ora  pronta merce
005500041126     D D               S              5I 0
005600041126     D M               S              5I 0
005700041126     D Y               S              5I 0
005800041124
005900041124     ***********************************************************************************************
006000041124     **?
006100041124     **?Main.
006200041124     **?
006300041124     ***********************************************************************************************
006400041124     C     *ENTRY        PLIST
006500041124     C                   PARM                    SUN
006600041124     C                   PARM                    KSU
006700041124     C                   PARM                    CRO
006800041124     C                   PARM                    CAR
006900041125     C                   PARM                    LoR
007000041125     C                   PARM                    PrR
007100041125     C                   PARM                    NaR
007200041125     C                   PARM                    DaR
007300041125     C                   PARM                    OrR
007400041124     C                   PARM                    Esito
007500041124     C                   PARM                    Msg
007600041124
007700050214     C                   EXSR      InzVar                                       Inizial.ne variabili
007800041125     C                   EXSR      ChkParm                                      Controllo parametri
007900041126
008000041126     C                   IF        dDaR < dCur                                  Data ORM passata
008100041126     C                   EXSR      ChkDaRPassata
008200041126     C                   ELSE
008300050214     C                   EXSR      ChkDaRLontana                                Data troppo lontana
008400041126     C                   EXSR      ChkDaRFesta                                  Controllo festa
008500041126     C                   SELECT
008600041126     C                   WHEN      dDaR = dCur                                  Data ORM oggi
008700041126     C                   EXSR      ChkDaRCorrente
008800041126     C                   WHEN      dDaR = dDomani                               Data ORM domani
008900041126     C                   EXSR      ChkDaRDomani
009000041126     C                   ENDSL
009100041126     C                   ENDIF
009200041125
009300041124     C                   EXSR      Uscita
009400041124
009500041124     ***********************************************************************************************
009600041124     **?
009700041124     **?Controllo parametri ricevuti.
009800041124     **?
009900041124     ***********************************************************************************************
010000041124     C     ChkParm       BEGSR
010100041124
010200041124     **?Pulizia parametri di output.
010300041124     C                   CLEAR                   Esito
010400041124     C                   CLEAR                   Msg
010500041124
010600041124     **?Controllo formale data ritiro.
010700041125     C     *EUR0         TEST(DE)                DaR
010800041124     C                   IF        %ERROR
010900041124     C                   EVAL      Esito = -1
011000041126     C                   EVAL      Msg = '/ Data ritiro formalmente errata.'
011100041125     C                   EXSR      Uscita
011200041124     C                   ELSE
011300041125     C     *EUR0         MOVE      DaR           dDaR
011400041124     C                   ENDIF
011500041124
011600041125     **?Controllo formale ora pronta merce.
011700041125     C                   EVAL      cOrR = OrR + '00'
011800041126     C     *EUR0         TEST(TE)                cOrR
011900041124     C                   IF        %ERROR
012000041125     C                   EVAL      Esito = -2
012100041126     C                   EVAL      Msg = '/ Ora pronta merce formalmente errata-
012200041126     C                             .'
012300041124     C                   EXSR      Uscita
012400041124     C                   ELSE
012500041125     C     *EUR0         MOVE      cOrR          tOrR
012600041124     C                   ENDIF
012700041124
012800041124     C                   ENDSR
012900041124
013000041124     ***********************************************************************************************
013100041124     **?
013200041124     **?Inizializzazione variabili.
013300041124     **?
013400041124     ***********************************************************************************************
013500041124     C     InzVar        BEGSR
013600041124
013700041124     C                   TIME                    dCur
013800041124     C                   TIME                    tCur
013900041125     C     dCur          ADDDUR    1:*D          dDomani
014000041124
014100041124     C                   ENDSR
014200041125
014300041125     ***********************************************************************************************
014400041125     **?
014500041125     **?Controllo data ORM passata.
014600041125     **?
014700041125     ***********************************************************************************************
014800041125     C     ChkDaRPassata BEGSR
014900041125
015000041125     C                   EVAL      Esito = -100
015100041126     C                   EVAL      Msg = '/ La data ritiro è passata. -
015200041126     C                             Cambiare data.'
015300041125     C                   EXSR      Uscita
015400041125
015500041125     C                   ENDSR
015600041124
015700041124     ***********************************************************************************************
015800041124     **?
015900041125     **?Controllo data ORM corrente.
016000041124     **?
016100041124     ***********************************************************************************************
016200041125     C     ChkDaRCorrenteBEGSR
016300041125
016400041125     **?Reperisco il codice tempo pickup area.
016500041125     C                   EXSR      GetPickupTime
016600041124
016700041124     C                   SELECT
016800041125
016900041125     **?Non è possibile eseguire il ritiro in giornata.
017000041126     C                   WHEN      O95GF2 = NoRitiroInGiornata
017100041125     C                   EVAL      Esito = -200
017200041126     C                   EVAL      Msg = '/ Non è possibile eseguire il ritiro -
017300041126     C                             in giornata nella località indicata. -
017400041126     C                             Cambiare data.'
017500041125     C                   EXSR      Uscita
017600041125
017700041125     **?E' possibile eseguire il ritiro in giornata.
017800041126     C                   WHEN      O95GF2 = SiRitiroInGiornata
017900041125
018000041125     C                   SELECT
018100041125
018200041126     **?L' ORM in giornata ad un indirizzo non abituale oppure per gli utenti anonimi
018300041126     **?deve essere immesso prima delle 11.00.
018400041125     C                   WHEN      (SUN = *ZERO OR CRO = *ZERO) AND
018500041125     C                             tCur > T'11.00.00'
018600041125     C                   EVAL      Esito = -201
018700041126     C                   EVAL      Msg = '/ L''orario attuale non consente di -
018800041126     C                             eseguire il ritiro in giornata. -
018900041126     C                             Cambiare data.'
019000041125     C                   EXSR      Uscita
019100041125
019200041126     **?L' ORM in giornata ad un indirizzo abituale deve essere immesso prima delle 12.30.
019300041125     C                   WHEN      CRO <> *ZERO AND tCur > T'12.30.00'
019400041125     C                   EVAL      Esito = -202
019500041126     C                   EVAL      Msg = '/ L''orario attuale non consente di -
019600041126     C                             eseguire il ritiro in giornata. -
019700041126     C                             Cambiare data.'
019800041125     C                   EXSR      Uscita
019900041125
020000041125     C                   ENDSL
020100041124
020200041124     C                   ENDSL
020300041125
020400041124     C                   ENDSR
020500041125
020600041125     ***********************************************************************************************
020700041125     **?
020800041125     **?Controllo data ORM domani.
020900041125     **?
021000041125     ***********************************************************************************************
021100041125     C     ChkDaRDomani  BEGSR
021200041125
021300041125
021400041125     C                   ENDSR
021500050214
021600050214     ***********************************************************************************************
021700050214     **?
021800050214     **?Controllo data ritiro troppo lontana.
021900050214     **?
022000050214     ***********************************************************************************************
022100050214     C     ChkDaRLontana BEGSR
022200050214
022300050214     C     *ISO          MOVE      dCur          XSRLav8DtDa                    Data corrente
022400050214     C     *ISO          MOVE      dDaR          XSRLav8DtA                     Data ritiro
022500050214     C                   CLEAR                   XSRLav8GioLav
022600050214     C                   CALL(E)   'XSRLAV8'
022700050214     C                   PARM                    XSRLav8DS
022800050214     C                   IF        XSRLav8GioLav > 10
022900050214     C                   EVAL      Esito = -3
023000050214     C                   EVAL      Msg = '/ Data ritiro troppo lontana (>10 -
023100050214     C                             giorni lavorativi).'
023200050214     C                   EXSR      Uscita
023300050214     C                   ENDIF
023400050214
023500050214     C                   ENDSR
023600041126
023700041126     ***********************************************************************************************
023800041126     **?
023900041126     **?Controllo data ORM festa.
024000041126     **?
024100041126     ***********************************************************************************************
024200041126     C     ChkDaRFesta   BEGSR
024300041126
024400041126     C     K04Cln01      KLIST
024500041126     C                   KFLD                    ClnTFP
024600041126     C                   KFLD                    ClnTFA
024700041126     C                   KFLD                    ClnAnn
024800041126     C                   KFLD                    ClnMes
024900041126
025000041126     C                   EXTRCT    dDaR:*Y       Y
025100041126     C                   EXTRCT    dDaR:*M       M
025200041126
025300041126     C                   IF        M <> ClnMes OR Y <> ClnAnn
025400041126     C                   EVAL      ClnAnn = Y
025500041126     C                   EVAL      ClnMes = M
025600041126     C     K04Cln01      CHAIN     AzCln01L
025700041126     C                   ENDIF
025800041126
025900041126     C                   IF        %FOUND(AzCln01L)
026000041126     C                   EXTRCT    dDaR:*D       D
026100041126     C                   IF        %SUBST(ClnPom:D:1) = Festa
026200041126     C                   EVAL      Esito = -500
026300041126     C                   EVAL      Msg = '/ La data ritiro deve essere un -
026400041126     C                             giorno lavorativo. -
026500041126     C                             Cambiare data.'
026600041126     C                   EXSR      Uscita
026700041126     C                   ENDIF
026800041126     C                   ENDIF
026900041126
027000041126     C                   ENDSR
027100041125
027200041125     ***********************************************************************************************
027300041125     **?
027400041125     **?Reperisco il codice tempo pickup area.
027500041125     **?
027600041125     ***********************************************************************************************
027700041125     C     GetPickupTime BEGSR
027800041125
027900041125     C                   RESET                   TISI95DS
028000041126     C                   EVAL      I95Nar = NaR
028100041125     C                   EVAL      I95CAP = CAR
028200041125     C                   EVAL      I95Loc = LoR
028300041125     C                   EVAL      I95Prv = PrR
028400041125     C     *ISO          MOVE      dCur          I95Dat
028500041125
028600041125     C                   CALL(E)   'TISI95R'
028700041125     C                   PARM                    TISI95DS
028800041125
028900041125     C                   IF        %ERROR OR O95Err <> *BLANK
029000041125     C                   EVAL      Esito = -400
029100041125     C                   EVAL      Msg = O95Msg
029200041125     C                   EXSR      Uscita
029300041125     C                   ENDIF
029400041125
029500041125     C                   ENDSR
029600041124
029700041124     ***********************************************************************************************
029800041124     **?
029900041124     **?Uscita dal programma.
030000041124     **?
030100041124     ***********************************************************************************************
030200041124     C     Uscita        BEGSR
030300041124
030400041124     C                   RETURN
030500041124
030600041124     C                   ENDSR
