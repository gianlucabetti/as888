000100060609     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('TIS')
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
001600060609     D rtvMsgLang      PR          3512A                                        Messaggio in lingua
001700060612     D  msgId                         7A   CONST
001800060612     D  piLinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
001900060613     D  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
001901060613     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
002200060612     D                                     VARYING
002300060612     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
002400060612     D                                     VARYING
002500060612     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
002600060612     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
002700060612
002800041125     D TISI95DS      E DS                                                       Controllo CAP-local.
002900041124     D                                     INZ
003000041125     D  I95TCn       E
003100041125     D                                     INZ('3')
003200041124     D TIO107IDS     E DS
003300041124     D                                     BASED(NullPtr)
003400050214     D XSRLav8DS       DS
003500050214     D                                     INZ
003600050214     D  XSRLav8DtDa                   8S 0                                      Da data
003700050214     D  XSRLav8DtA                    8S 0                                      A data
003800050214     D  XSRLav8GioLav                 5S 0                                      Giorni lavorativi
003900041124
004000041125     D NullPtr         S               *                                        Puntatore nullo
004100041124     D SUN             S                                                        Strategi User Number
004200041124     D                                     LIKE(O107RQSCID)
004300041125     D KSU             S                                                        Codice cliente unif.
004400041124     D                                     LIKE(O107KSU)
004500041125     D CRO             S                                                        Codice ritiro
004600041124     D                                     LIKE(O107CRO)
004700041125     D CAR             S                                                        CAP
004800041124     D                                     LIKE(O107CAR)
004900041125     D LoR             S                                                        Località
005000041125     D                                     LIKE(O107LoR)
005100041125     D PrR             S                                                        Provincia
005200041125     D                                     LIKE(O107PrR)
005300041125     D NaR             S                                                        Nazione
005400041125     D                                     LIKE(O107NaR)
005500041125     D DaR             S                                                        Data ritiro
005600041125     D                                     LIKE(O107DaR)
005700041125     D OrR             S                                                        Ora pronta merce
005800041125     D                                     LIKE(O107OrR)
005900041124     D Esito           S              5I 0
006000041124     D Msg             S             80                                         Messaggio errore
006100041124     D dCur            S               D                                        Data corrente
006200041124     D tCur            S               T                                        Ora  corrente
006300041125     D dDomani         S               D                                        Domani
006400041125     D dDaR            S               D                                        Data pronta merce
006500041125     D tOrR            S               T                                        Ora  pronta merce
006600041125     D cOrR            S              6                                         Ora  pronta merce
006700041126     D D               S              5I 0
006800041126     D M               S              5I 0
006900041126     D Y               S              5I 0
007000060605     D lang            S              2A                                        Lingua
007100060605
007200041124     ***********************************************************************************************
007300041124     **?
007400041124     **?Main.
007500041124     **?
007600041124     ***********************************************************************************************
007700041124     C     *ENTRY        PLIST
007800041124     C                   PARM                    SUN
007900041124     C                   PARM                    KSU
008000041124     C                   PARM                    CRO
008100041124     C                   PARM                    CAR
008200041125     C                   PARM                    LoR
008300041125     C                   PARM                    PrR
008400041125     C                   PARM                    NaR
008500041125     C                   PARM                    DaR
008600041125     C                   PARM                    OrR
008700041124     C                   PARM                    Esito
008800041124     C                   PARM                    Msg
008900060605     C                   PARM                    lang
009000041124
009100050214     C                   EXSR      InzVar                                       Inizial.ne variabili
009200041125     C                   EXSR      ChkParm                                      Controllo parametri
009300041126
009400041126     C                   IF        dDaR < dCur                                  Data ORM passata
009500041126     C                   EXSR      ChkDaRPassata
009600041126     C                   ELSE
009700050214     C                   EXSR      ChkDaRLontana                                Data troppo lontana
009800041126     C                   EXSR      ChkDaRFesta                                  Controllo festa
009900041126     C                   SELECT
010000041126     C                   WHEN      dDaR = dCur                                  Data ORM oggi
010100041126     C                   EXSR      ChkDaRCorrente
010200041126     C                   WHEN      dDaR = dDomani                               Data ORM domani
010300041126     C                   EXSR      ChkDaRDomani
010400041126     C                   ENDSL
010500041126     C                   ENDIF
010600041125
010700041124     C                   EXSR      Uscita
010800041124
010900041124     ***********************************************************************************************
011000041124     **?
011100041124     **?Controllo parametri ricevuti.
011200041124     **?
011300041124     ***********************************************************************************************
011400041124     C     ChkParm       BEGSR
011500041124
011600041124     **?Pulizia parametri di output.
011700041124     C                   CLEAR                   Esito
011800041124     C                   CLEAR                   Msg
011900041124
012000041124     **?Controllo formale data ritiro.
012100041125     C     *EUR0         TEST(DE)                DaR
012200041124     C                   IF        %ERROR
012300041124     C                   EVAL      Esito = -1
012400060605     C*                  EVAL      Msg = '/ Data ritiro formalmente errata.'
012500060609     C                   EVAL      msg = rtvMsgLang('TIS0133':lang)
012600060609     C                   EXSR      Uscita
012700041124     C                   ELSE
012800041125     C     *EUR0         MOVE      DaR           dDaR
012900041124     C                   ENDIF
013000041124
013100041125     **?Controllo formale ora pronta merce.
013200041125     C                   EVAL      cOrR = OrR + '00'
013300041126     C     *EUR0         TEST(TE)                cOrR
013400041124     C                   IF        %ERROR
013500041125     C                   EVAL      Esito = -2
013600060605     C*                  EVAL      Msg = '/ Ora pronta merce formalmente errata-
013700060605     C*                            .'
013800060609     C                   EVAL      msg = rtvMsgLang('TIS0425':lang)
013900060609     C                   EXSR      Uscita
014000041124     C                   ELSE
014100041125     C     *EUR0         MOVE      cOrR          tOrR
014200041124     C                   ENDIF
014300041124
014400041124     C                   ENDSR
014500041124
014600041124     ***********************************************************************************************
014700041124     **?
014800041124     **?Inizializzazione variabili.
014900041124     **?
015000041124     ***********************************************************************************************
015100041124     C     InzVar        BEGSR
015200041124
015300041124     C                   TIME                    dCur
015400041124     C                   TIME                    tCur
015500041125     C     dCur          ADDDUR    1:*D          dDomani
015600041124
015700041124     C                   ENDSR
015800041125
015900041125     ***********************************************************************************************
016000041125     **?
016100041125     **?Controllo data ORM passata.
016200041125     **?
016300041125     ***********************************************************************************************
016400041125     C     ChkDaRPassata BEGSR
016500041125
016600041125     C                   EVAL      Esito = -100
016700060605     C*                  EVAL      Msg = '/ La data ritiro è passata. -
016800060605     C*                            Cambiare data.'
016900060609     C                   EVAL      msg = rtvMsgLang('TIS0329':lang)
017000060609     C                   EXSR      Uscita
017100041125
017200041125     C                   ENDSR
017300041124
017400041124     ***********************************************************************************************
017500041124     **?
017600041125     **?Controllo data ORM corrente.
017700041124     **?
017800041124     ***********************************************************************************************
017900041125     C     ChkDaRCorrenteBEGSR
018000041125
018100041125     **?Reperisco il codice tempo pickup area.
018200041125     C                   EXSR      GetPickupTime
018300041124
018400041124     C                   SELECT
018500041125
018600041125     **?Non è possibile eseguire il ritiro in giornata.
018700041126     C                   WHEN      O95GF2 = NoRitiroInGiornata
018800041125     C                   EVAL      Esito = -200
018900060605     C*                  EVAL      Msg = '/ Non è possibile eseguire il ritiro -
019000060605     C*                            in giornata nella località indicata. -
019100060605     C*                            Cambiare data.'
019200060609     C                   EVAL      msg = rtvMsgLang('TIS0400':lang)
019300060609     C                   EXSR      Uscita
019400041125
019500041125     **?E' possibile eseguire il ritiro in giornata.
019600041126     C                   WHEN      O95GF2 = SiRitiroInGiornata
019700041125
019800041125     C                   SELECT
019900041125
020000041126     **?L' ORM in giornata ad un indirizzo non abituale oppure per gli utenti anonimi
020100041126     **?deve essere immesso prima delle 11.00.
020200041125     C                   WHEN      (SUN = *ZERO OR CRO = *ZERO) AND
020300041125     C                             tCur > T'11.00.00'
020400041125     C                   EVAL      Esito = -201
020500060605     C*                  EVAL      Msg = '/ L''orario attuale non consente di -
020600060605     C*                            eseguire il ritiro in giornata. -
020700060605     C*                            Cambiare data.'
020800060609     C                   EVAL      msg = rtvMsgLang('TIS0303':lang)
020900060609     C                   EXSR      Uscita
021000041125
021100041126     **?L' ORM in giornata ad un indirizzo abituale deve essere immesso prima delle 12.30.
021200041125     C                   WHEN      CRO <> *ZERO AND tCur > T'12.30.00'
021300041125     C                   EVAL      Esito = -202
021400060605     C*                  EVAL      Msg = '/ L''orario attuale non consente di -
021500060605     C*                            eseguire il ritiro in giornata. -
021600060605     C*                            Cambiare data.'
021700060609     C                   EVAL      msg = rtvMsgLang('TIS0303':lang)
021800060609     C                   EXSR      Uscita
021900041125
022000041125     C                   ENDSL
022100041124
022200041124     C                   ENDSL
022300041125
022400041124     C                   ENDSR
022500041125
022600041125     ***********************************************************************************************
022700041125     **?
022800041125     **?Controllo data ORM domani.
022900041125     **?
023000041125     ***********************************************************************************************
023100041125     C     ChkDaRDomani  BEGSR
023200041125
023300041125
023400041125     C                   ENDSR
023500050214
023600050214     ***********************************************************************************************
023700050214     **?
023800050214     **?Controllo data ritiro troppo lontana.
023900050214     **?
024000050214     ***********************************************************************************************
024100050214     C     ChkDaRLontana BEGSR
024200050214
024300050214     C     *ISO          MOVE      dCur          XSRLav8DtDa                    Data corrente
024400050214     C     *ISO          MOVE      dDaR          XSRLav8DtA                     Data ritiro
024500050214     C                   CLEAR                   XSRLav8GioLav
024600050214     C                   CALL(E)   'XSRLAV8'
024700050214     C                   PARM                    XSRLav8DS
024800050214     C                   IF        XSRLav8GioLav > 10
024900050214     C                   EVAL      Esito = -3
025000060605     C*                  EVAL      Msg = '/ Data ritiro troppo lontana (>10 -
025100060605     C*                            giorni lavorativi).'
025200060609     C                   EVAL      msg = rtvMsgLang('TIS0135':lang)
025300060609     C                   EXSR      Uscita
025400050214     C                   ENDIF
025500050214
025600050214     C                   ENDSR
025700041126
025800041126     ***********************************************************************************************
025900041126     **?
026000041126     **?Controllo data ORM festa.
026100041126     **?
026200041126     ***********************************************************************************************
026300041126     C     ChkDaRFesta   BEGSR
026400041126
026500041126     C     K04Cln01      KLIST
026600041126     C                   KFLD                    ClnTFP
026700041126     C                   KFLD                    ClnTFA
026800041126     C                   KFLD                    ClnAnn
026900041126     C                   KFLD                    ClnMes
027000041126
027100041126     C                   EXTRCT    dDaR:*Y       Y
027200041126     C                   EXTRCT    dDaR:*M       M
027300041126
027400041126     C                   IF        M <> ClnMes OR Y <> ClnAnn
027500041126     C                   EVAL      ClnAnn = Y
027600041126     C                   EVAL      ClnMes = M
027700041126     C     K04Cln01      CHAIN     AzCln01L
027800041126     C                   ENDIF
027900041126
028000041126     C                   IF        %FOUND(AzCln01L)
028100041126     C                   EXTRCT    dDaR:*D       D
028200041126     C                   IF        %SUBST(ClnPom:D:1) = Festa
028300041126     C                   EVAL      Esito = -500
028400060605     C*                  EVAL      Msg = '/ La data ritiro deve essere un -
028500060605     C*                            giorno lavorativo. -
028600060605     C*                            Cambiare data.'
028700060609     C                   EVAL      msg = rtvMsgLang('TIS0328':lang)
028800060609     C                   EXSR      Uscita
028900041126     C                   ENDIF
029000041126     C                   ENDIF
029100041126
029200041126     C                   ENDSR
029300041125
029400041125     ***********************************************************************************************
029500041125     **?
029600041125     **?Reperisco il codice tempo pickup area.
029700041125     **?
029800041125     ***********************************************************************************************
029900041125     C     GetPickupTime BEGSR
030000041125
030100041125     C                   RESET                   TISI95DS
030200041126     C                   EVAL      I95Nar = NaR
030300041125     C                   EVAL      I95CAP = CAR
030400041125     C                   EVAL      I95Loc = LoR
030500041125     C                   EVAL      I95Prv = PrR
030600041125     C     *ISO          MOVE      dCur          I95Dat
030700041125
030800041125     C                   CALL(E)   'TISI95R'
030900041125     C                   PARM                    TISI95DS
031000041125
031100041125     C                   IF        %ERROR OR O95Err <> *BLANK
031200041125     C                   EVAL      Esito = -400
031300041125     C                   EVAL      Msg = O95Msg
031400041125     C                   EXSR      Uscita
031500041125     C                   ENDIF
031600041125
031700041125     C                   ENDSR
031800060605
031900041124     ***********************************************************************************************
032000041124     **?
032100041124     **?Uscita dal programma.
032200041124     **?
032300041124     ***********************************************************************************************
032400041124     C     Uscita        BEGSR
032500041124
032600041124     C                   RETURN
032700041124
032800041124     C                   ENDSR
