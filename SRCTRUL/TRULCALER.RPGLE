000100030311     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200000622
000300090428     FtrulCaleD CF   E             WORKSTN
000400000622     F                                     SFILE(S01:RrnS01)
000500000622     F                                     INDDS(IndAra)
000600000622
000700000623     D Eseguito        C                   '0'
000800000623     D Errore          C                   '1'
000900000622     D Green           C                   X'20'
001000000623     D GreenRI         C                   X'21'
001100000622     D White           C                   X'22'
001200000623     D WhiteRI         C                   X'23'
001300000622     D Red             C                   X'28'
001400000623     D RedRI           C                   X'29'
001500000703     D Pink            C                   X'38'
001600000703     D PinkRI          C                   X'39'
001700000623     D Lunedi          C                   1
001800000623     D Martedi         C                   2
001900000623     D Mercoledi       C                   3
002000000623     D Giovedi         C                   4
002100000623     D Venerdi         C                   5
002200000623     D Sabato          C                   6
002300000623     D Domenica        C                   7
002400000703     D LilianLoval     C                   D'1582-10-15'
002500030311
002600100715     D/COPY GAITRASRC/SRCPROTOPR,TRULPASQUA
002700100715
002800000623     D IndAra          DS
002900000623     D  SflClr                 1      1N
003000000623     D  RollUp                10     10N
003100000623     D  RollDown              11     11N
003200030311
003300000703     D PrmDataI        S               D
003400000703     D PrmDataO        S               D
003500000703     D PrmEsito        S              1
003600030311
003700000622     D Mese            S              9
003800000622     D                                     DIM(12)
003900000622     D                                     CTDATA
004000000623     D Feste           S              5
004100030311     D                                     DIM(10)
004200100715     D                                     ASCEND
004300000623     D                                     CTDATA
004400000623     D WrkAtr          S              1
004500000622     D RrnS01          S              5U 0
004600100715     D esito           S             10I 0
004700000622     D WrkDt           S               D
004800000622     D WrkDtRice       S               D
004900000622     D WrkDtOggi       S               D
005000000622     D WrkDtInMe       S               D
005100000622     D WrkDtFiMe       S               D
005200000622     D WrkDtInCa       S               D
005300000622     D WrkDtFiCa       S               D
005400000622     D WrkD            S              5U 0
005500000622     D WrkM            S              5U 0
005600000622     D WrkY            S              5U 0
005700000623     D WrkMD           S              5
005800000623     D WrkFesta        S              1
005900000623     D                                     INZ('0')
006000000703     D WrkFineS01      S              1
006100000703     D                                     INZ('0')
006200100715     D pasquaCattolica...
006300100715     D                 S               D
006400000623     D ChrDate         S             10
006500000622     D PictureIso      S             10
006600000703     D                                     INZ('YYYY-MM-DD')
006700000623     D LilDate         S             10I 0
006800000623     D DayOfWeek       S             10I 0
006900000622     D Fc              S             12
007000000622
007100000622     C     *ENTRY        PLIST
007200000703     C                   PARM                    PrmDataI
007300000703     C                   PARM                    PrmDataO
007400000703     C                   PARM                    PrmEsito
007500030311
007600000703     C                   TIME                    WrkDtOggi
007700000623     C                   EXSR      RcvParm
007800030311
007900090428     C                   OPEN(E)   trulCaleD
008000030311
008100030311     **?Carico il mese della data che o ricevuto.
008200000703     C                   EVAL      WrkDtRice = PrmDataI
008300000622     C                   EXSR      CarS01
008400030311
008500000622     C                   DOU       *INKC
008600000622     C                             OR
008700000622     C                             *INKL
008800030311
008900000622     C                   EXFMT     C01
009000030311
009100000623     C                   SELECT
009200030311     **?F3
009300000623     C                   WHEN      *INKC
009400090428     C                   EVAL      PrmEsito = 'C'
009500000703     C                   EVAL      *INLR = *ON
009600030311     **?F12
009700000703     C                   WHEN      *INKL
009800090428     C                   EVAL      PrmEsito = 'L'
009900030311     **?ROLLUP (mese successivo).
010000000623     C                   WHEN      RollUp
010100000630     C                             OR
010200030311     C                             (C01RtnFld = 'C01MESESUC' AND NOT RollDown)
010300030311     C                   EVAL      RollUp = *ON
010400000703     C                   ADDDUR(E) 1:*M          WrkDtRice
010500000703     C                   IF        NOT %ERROR
010600000623     C                   EXSR      CarS01
010700000703     C                   ENDIF
010800030311     **?ROLLDOWN (mese precedente).
010900000623     C                   WHEN      RollDown
011000000630     C                             OR
011100030311     C                             (C01RtnFld = 'C01MESEPRE' AND NOT RollUp)
011200030311     C                   EVAL      RollDown = *ON
011300000703     C                   SUBDUR    1:*M          WrkDtRice
011400000623     C                   EXSR      CarS01
011500030311     **?Invio col cursore nel subfile.
011600000703     C                   WHEN      C01CsrRrn > *ZERO
011700000703     C                   EXSR      WrkS01
011800030311     **?Invio col cursore fuori subfile.
011900000703     C                   OTHER
012000090428     C                   EVAL      PrmEsito = 'L'
012100000703     C                   EXSR      Uscita
012200030311
012300000623     C                   ENDSL
012400030311
012500000622     C                   ENDDO
012600030311
012700000623     C                   EXSR      Uscita
012800000622
012900030311     ***********************************************************************
013000030311     **?
013100030311     **?Caricamento SFL S01.
013200030311     **?
013300030311     ***********************************************************************
013400000622     C     CarS01        BEGSR
013500000622
013600030311     **?Primo  giorno del mese in formato *ISO.
013700000622     C                   EXTRCT    WrkDtRice:*D  WrkD
013800000622     C                   EVAL      WrkD = WrkD - 1
013900000703     C     WrkDtRice     SUBDUR(E) WrkD:*D       WrkDtInMe
014000000703     C                   IF        %ERROR
014100000703     C                   EVAL      WrkDtInMe = *LOVAL
014200000703     C                   ENDIF
014300030311     **?Primo  giorno del mese in formato lilian.
014400000623     C     *ISO          MOVE      WrkDtInMe     ChrDate
014500000623     C                   CALLB(D)  'CEEDAYS'
014600000623     C                   PARM                    ChrDate
014700000703     C                   PARM                    PictureIso
014800000623     C                   PARM      *ZERO         LilDate
014900000623     C                   PARM      *BLANK        Fc
015000030311     **?Reperisco il numero del giorno della settimana (1-7).
015100000623     C                   EXSR      CalCEEDYWK
015200030311     **?Primo giorno (luned~) del calendario.
015300000623     C                   IF        DayOfWeek > Lunedi
015400000622     C                   EVAL      WrkD = DayOfWeek - 1
015500000703     C     WrkDtInMe     SUBDUR(E) WrkD:*D       WrkDtInCa
015600000703     C                   IF        %ERROR
015700000703     C                   EVAL      WrkDtInMe = *LOVAL
015800000703     C                   ENDIF
015900000622     C                   ELSE
016000000622     C                   EVAL      WrkDtInCa = WrkDtInMe
016100000622     C                   ENDIF
016200000622
016300030311     **?Ultimo giorno del mese in formato *ISO
016400000703     C     WrkDtRice     ADDDUR(E) 1:*M          WrkDtFiMe
016500000703     C                   IF        %ERROR
016600000703     C                   EVAL      WrkDtFiMe = *HIVAL
016700000703     C                   ELSE
016800000622     C                   EXTRCT    WrkDtFiMe:*D  WrkD
016900000703     C     WrkDtFiMe     SUBDUR(E) WrkD:*D       WrkDtFiMe
017000000703     C                   IF        %ERROR
017100000703     C                   EVAL      WrkDtFiMe = *HIVAL
017200000703     C                   ENDIF
017300000703     C                   ENDIF
017400030311     **?Ultimo giorno del mese in formato lilian.
017500000623     C     *ISO          MOVE      WrkDtFiMe     ChrDate
017600000623     C                   CALLB(D)  'CEEDAYS'
017700000623     C                   PARM                    ChrDate
017800000622     C                   PARM                    PictureIso
017900000622     C                   PARM                    LilDate
018000000622     C                   PARM      *BLANK        Fc
018100030311     **?Reperisco il numero del giorno della settimana (1-7).
018200000623     C                   EXSR      CalCEEDYWK
018300030311     **?Ultimo giorno del calendario (domenica) in *ISO.
018400000623     C                   IF        DayOfWeek < Domenica
018500000703     C                             AND
018600000703     C                             DayOfWeek > *ZERO
018700000622     C                   EVAL      WrkD = 7 - DayOfWeek
018800000703     C     WrkDtFiMe     ADDDUR(E) WrkD:*D       WrkDtFiCa
018900000703     C                   IF        %ERROR
019000000703     C                   EVAL      WrkDtFiCa = *HIVAL
019100000703     C                   ENDIF
019200000622     C                   ELSE
019300000622     C                   EVAL      WrkDtFiCa = WrkDtFiMe
019400000622     C                   ENDIF
019500000622
019600030311     **?Pulisco il subfile e le variabili di gestione.
019700000622     C                   CLEAR                   RrnS01
019800000622     C                   EVAL      SflClr = *ON
019900000622     C                   WRITE     C01
020000000622     C                   EVAL      SflClr = *OFF
020100000703     C                   CLEAR                   S01
020200030311
020300000622     C                   EVAL      WrkDt = WrkDtInCa
020400000703     C                   EVAL      WrkFineS01 = *OFF
020500030311
020600000703     C                   DOU       WrkFineS01 = *ON
020700030311
020800000703     C                   IF        WrkDt >= LilianLoval
020900030311     **?Ultimo giorno del mese in formato lilian.
021000000623     C     *ISO          MOVE      WrkDt         ChrDate
021100000623     C                   CALLB(D)  'CEEDAYS'
021200000623     C                   PARM                    ChrDate
021300000622     C                   PARM                    PictureIso
021400000622     C                   PARM                    LilDate
021500000622     C                   PARM      *BLANK        Fc
021600030311     **?Reperisco il numero del giorno della settimana (1-7).
021700000623     C                   EXSR      CalCEEDYWK
021800030311
021900000623     C                   EVAL      WrkFesta = *OFF
022000000623     C                   IF        DayOfWeek = Domenica
022100000623     C                   EVAL      WrkFesta = *ON
022200000623     C                   ELSE
022300000623     C     *ISO          MOVE      WrkDt         ChrDate
022400000623     C                   MOVE      ChrDate       WrkMD
022500000623     C     WrkMD         LOOKUP    Feste                                  99
022600000623     C                   IF        %EQUAL
022700000623     C                   EVAL      WrkFesta = *ON
022800100715     C                   ELSE
022900100715     C                   IF        DayOfWeek = Lunedi
023000100715     C                   CALLP     TrulPasqua(%SUBDT(WrkDt:*Y) : esito
023100100715     C                             : pasquaCattolica)
023200100715     C                   IF        pasquaCattolica + %DAYS(1) = WrkDt           Pasquetta
023300100715     C                   EVAL      WrkFesta = *ON
023400100715     C                   ENDIF
023500100715     C                   ENDIF
023600000623     C                   ENDIF
023700000623     C                   ENDIF
023800030311     **?Visualizzo ...
023900030311     **?- Oggi in rosso.
024000030311     **?- I festivi in inversione fondo.
024100030311     **?- I giorni del mese in bianco.
024200030311     **?- I giorni fuori del mese in verde.
024300000622     C                   SELECT
024400000622     C                   WHEN      WrkDt  = WrkDtOggi
024500030311     C                   EVAL      WrkAtr = Pink
024600000623     C                   IF        WrkFesta = *ON
024700030311     C                   EVAL      WrkAtr = PinkRI
024800000623     C                   ENDIF
024900000623     C                   WHEN      WrkDt < WrkDtInMe
025000000623     C                             OR
025100000623     C                             WrkDt > WrkDtFiMe
025200000623     C                   EVAL      WrkAtr = Green
025300000623     C                   IF        WrkFesta = *ON
025400000623     C                   EVAL      WrkAtr = GreenRI
025500000623     C                   ENDIF
025600000623     C                   OTHER
025700000703     C                   EVAL      WrkAtr = White
025800000623     C                   IF        WrkFesta = *ON
025900000703     C                   EVAL      WrkAtr = WhiteRI
026000000623     C                   ENDIF
026100000622     C                   ENDSL
026200030311
026300000623     C                   EXTRCT    WrkDt:*D      WrkD
026400030311
026500000622     C                   SELECT
026600000623     C                   WHEN      DayOfWeek = Lunedi
026700000630     C                   EVAL      S01DatLun = WrkDt
026800000622     C                   EVAL      S01DayLun = WrkD
026900000622     C                   EVAL      AtrDayLun = WrkAtr
027000000623     C                   WHEN      DayOfWeek = Martedi
027100000630     C                   EVAL      S01DatMar = WrkDt
027200000622     C                   EVAL      S01DayMar = WrkD
027300000622     C                   EVAL      AtrDayMar = WrkAtr
027400000623     C                   WHEN      DayOfWeek = Mercoledi
027500000630     C                   EVAL      S01DatMer = WrkDt
027600000622     C                   EVAL      S01DayMer = WrkD
027700000622     C                   EVAL      AtrDayMer = WrkAtr
027800000623     C                   WHEN      DayOfWeek = Giovedi
027900000630     C                   EVAL      S01DatGio = WrkDt
028000000622     C                   EVAL      S01DayGio = WrkD
028100000622     C                   EVAL      AtrDayGio = WrkAtr
028200000623     C                   WHEN      DayOfWeek = Venerdi
028300000630     C                   EVAL      S01DatVen = WrkDt
028400000622     C                   EVAL      S01DayVen = WrkD
028500000622     C                   EVAL      AtrDayVen = WrkAtr
028600000623     C                   WHEN      DayOfWeek = Sabato
028700000630     C                   EVAL      S01DatSab = WrkDt
028800000622     C                   EVAL      S01DaySab = WrkD
028900000622     C                   EVAL      AtrDaySab = WrkAtr
029000000623     C                   WHEN      DayOfWeek = Domenica
029100000630     C                   EVAL      S01DatDom = WrkDt
029200000622     C                   EVAL      S01DayDom = WrkD
029300000622     C                   EVAL      AtrDayDom = WrkAtr
029400000622     C                   ENDSL
029500030311
029600000623     C                   IF        DayOfWeek = Domenica
029700000703     C                             OR
029800000703     C                             WrkDt = WrkDtFiCa
029900000622     C                   ADD       1             RrnS01
030000000622     C                   WRITE     S01
030100000703     C                   CLEAR                   S01
030200000622     C                   ENDIF
030300030311
030400000703     C                   ENDIF
030500030311
030600000703     C                   IF        WrkDt < WrkDtFiCa
030700000622     C                   ADDDUR    1:*D          WrkDt
030800000703     C                   ELSE
030900000703     C                   EVAL      WrkFineS01 = *ON
031000000703     C                   ENDIF
031100030311
031200000622     C                   ENDDO
031300030311
031400000623     C                   EXTRCT    WrkDtInMe:*Y  C01Anno
031500000623     C                   EXTRCT    WrkDtInMe:*M  WrkM
031600000623     C                   EVAL      C01Mese = Mese(WrkM)
031700030311
031800000622     C                   ENDSR
031900000630
032000030311     ***********************************************************************
032100030311     **?
032200030311     **?Selezione giorno.
032300030311     **?
032400030311     ***********************************************************************
032500000703     C     WrkS01        BEGSR
032600030311
032700000703     C                   IF        %SUBST(C01RtnFld:1:6) = 'S01DAY'
032800030311
032900000703     C                   EVAL      RrnS01 = C01CsrRrn
033000000703     C     RrnS01        CHAIN     S01
033100030311
033200000703     C                   IF        %FOUND
033300030311
033400000703     C                   SELECT
033500000703     C                   WHEN      C01RtnFld = 'S01DAYLUN'
033600000703     C                   EVAL      PrmDataO = S01DatLun
033700000703     C                   WHEN      C01RtnFld = 'S01DAYMAR'
033800000703     C                   EVAL      PrmDataO = S01DatMar
033900000703     C                   WHEN      C01RtnFld = 'S01DAYMER'
034000000703     C                   EVAL      PrmDataO = S01DatMer
034100000703     C                   WHEN      C01RtnFld = 'S01DAYGIO'
034200000703     C                   EVAL      PrmDataO = S01DatGio
034300000703     C                   WHEN      C01RtnFld = 'S01DAYVEN'
034400000703     C                   EVAL      PrmDataO = S01DatVen
034500000703     C                   WHEN      C01RtnFld = 'S01DAYSAB'
034600000703     C                   EVAL      PrmDataO = S01DatSab
034700000703     C                   WHEN      C01RtnFld = 'S01DAYDOM'
034800000703     C                   EVAL      PrmDataO = S01DatDom
034900000703     C                   ENDSL
035000030311
035100000703     C                   EXSR      Uscita
035200030311
035300000703     C                   ENDIF
035400030311
035500000630     C                   ENDIF
035600030311
035700000630     C                   ENDSR
035800000623
035900030311     ***********************************************************************
036000030311     **?
036100030311     **?Chiamo CEEDYWK.
036200030311     **?
036300030311     ***********************************************************************
036400000623     C     CalCEEDYWK    BEGSR
036500030311
036600000623     C                   CALLB(D)  'CEEDYWK'
036700000623     C                   PARM                    LilDate
036800000623     C                   PARM      *ZERO         DayOfWeek
036900000623     C                   PARM      *BLANK        Fc
037000030311     **?Sposto domenica al settimo posto.
037100000703     C                   IF        DayOfWeek > *ZERO
037200000623     C                   SELECT
037300000623     C                   WHEN      DayOfWeek = 1
037400000623     C                   EVAL      DayOfWeek = 7
037500000623     C                   OTHER
037600000623     C                   EVAL      DayOfWeek = DayOfWeek - 1
037700000623     C                   ENDSL
037800000703     C                   ENDIF
037900030311
038000000623     C                   ENDSR
038100000623
038200030311     ***********************************************************************
038300030311     **?
038400030311     **?Ricezione e controllo parametri.
038500030311     **?
038600030311     ***********************************************************************
038700000623     C     RcvParm       BEGSR
038800030311     **?Pulizia dei parametri da restituire.
038900000703     C                   CLEAR                   PrmDataO
039000000703     C                   CLEAR                   PrmEsito
039100030311     **?Controllo formale della data ricevuta.
039200000703     C                   TEST(E)                 PrmDataI
039300030311     **?Se la data } fromalmente sbagliata
039400030311     **?o } minore del 15 ottobre 1582
039500030311     **?forzo oggi.
039600000623     C                   IF        %ERROR
039700000703     C                             OR
039800000703     C                             PrmDataI < LilianLoval
039900000703     C                   EVAL      PrmDataI = WrkDtOggi
040000000623     C                   ENDIF
040100030311
040200000623     C                   ENDSR
040300000623
040400030311     ***********************************************************************
040500030311     **?
040600030311     **?Uscita programma.
040700030311     **?
040800030311     ***********************************************************************
040900000623     C     Uscita        BEGSR
041000030311
041100000623     C                   IF        PrmEsito = *BLANK
041200000623     C                   EVAL      PrmEsito = Eseguito
041300000623     C                   ENDIF
041400030311
041500090428     C                   CLOSE(E)  trulCaleD
041600030311
041700000623     C                   RETURN
041800030311
041900000623     C                   ENDSR
042000000630
042100030311     ***********************************************************************
042200030311     **?
042300030311     **?Operazioni iniziali.
042400030311     **?
042500030311     ***********************************************************************
042600000630     C     *INZSR        BEGSR
042700030311
042800000630     C                   EVAL      C01MesePre = '<'
042900000630     C                   EVAL      C01MeseSuc = '>'
043000030311
043100000630     C                   ENDSR
043200000623
043300000622**CTDATA Mese
043400000622Gennaio
043500000622Febbraio
043600000622Marzo
043700000622Aprile
043800000622Maggio
043900000622Giugno
044000000622Luglio
044100000622Agosto
044200000622Settembre
044300000622Ottobre
044400000622Novembre
044500000622Dicembre
044600000623**CTDATA Feste
04470000070301-01 Capodanno
04480000070301-06 Epifania
04490000070304-25 Liberazione
04500000070305-01 Festa del lavoro
04510003031106-02 Festa della Repubblica
04520000070308-15 Assunzione della Vergine Maria
04530000070311-01 Ognissanti
04540000070312-08 Immacolata concezione
04550000070312-25 Natale
04560000070312-26 Santo Stefano
