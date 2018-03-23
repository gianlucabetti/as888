000100030311     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200000622
000300030311     FCalendaD  CF   E             WORKSTN
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
002600000623     D IndAra          DS
002700000623     D  SflClr                 1      1N
002800000623     D  RollUp                10     10N
002900000623     D  RollDown              11     11N
003000030311
003100000703     D PrmDataI        S               D
003200000703     D PrmDataO        S               D
003300000703     D PrmEsito        S              1
003400030311
003500000622     D Mese            S              9
003600000622     D                                     DIM(12)
003700000622     D                                     CTDATA
003800000623     D Feste           S              5
003900030311     D                                     DIM(10)
004000000623     D                                     CTDATA
004100000623     D WrkAtr          S              1
004200000622     D RrnS01          S              5U 0
004300030311
004400000622     D WrkDt           S               D
004500000622     D WrkDtRice       S               D
004600000622     D WrkDtOggi       S               D
004700000622     D WrkDtInMe       S               D
004800000622     D WrkDtFiMe       S               D
004900000622     D WrkDtInCa       S               D
005000000622     D WrkDtFiCa       S               D
005100000622     D WrkD            S              5U 0
005200000622     D WrkM            S              5U 0
005300000622     D WrkY            S              5U 0
005400000623     D WrkMD           S              5
005500000623     D WrkFesta        S              1
005600000623     D                                     INZ('0')
005700000703     D WrkFineS01      S              1
005800000703     D                                     INZ('0')
005900030311
006000000623     D ChrDate         S             10
006100000622     D PictureIso      S             10
006200000703     D                                     INZ('YYYY-MM-DD')
006300000623     D LilDate         S             10I 0
006400000623     D DayOfWeek       S             10I 0
006500000622     D Fc              S             12
006600000622
006700000622     C     *ENTRY        PLIST
006800000703     C                   PARM                    PrmDataI
006900000703     C                   PARM                    PrmDataO
007000000703     C                   PARM                    PrmEsito
007100030311
007200000703     C                   TIME                    WrkDtOggi
007300000623     C                   EXSR      RcvParm
007400030311
007500030311     C                   OPEN(E)   CalendaD
007600030311
007700030311     ** Carico il mese della data che o ricevuto.
007800000703     C                   EVAL      WrkDtRice = PrmDataI
007900000622     C                   EXSR      CarS01
008000030311
008100000622     C                   DOU       *INKC
008200000622     C                             OR
008300000622     C                             *INKL
008400030311
008500000622     C                   EXFMT     C01
008600030311
008700000623     C                   SELECT
008800030311     ** F3
008900000623     C                   WHEN      *INKC
009000000703     C                   EVAL      *INLR = *ON
009100030311     ** F12
009200000703     C                   WHEN      *INKL
009300030311     ** ROLLUP (mese successivo).
009400000623     C                   WHEN      RollUp
009500000630     C                             OR
009600030311     C                             (C01RtnFld = 'C01MESESUC' AND NOT RollDown)
009700030311     C                   EVAL      RollUp = *ON
009800000703     C                   ADDDUR(E) 1:*M          WrkDtRice
009900000703     C                   IF        NOT %ERROR
010000000623     C                   EXSR      CarS01
010100000703     C                   ENDIF
010200030311     ** ROLLDOWN (mese precedente).
010300000623     C                   WHEN      RollDown
010400000630     C                             OR
010500030311     C                             (C01RtnFld = 'C01MESEPRE' AND NOT RollUp)
010600030311     C                   EVAL      RollDown = *ON
010700000703     C                   SUBDUR    1:*M          WrkDtRice
010800000623     C                   EXSR      CarS01
010900030311     ** Invio col cursore nel subfile.
011000000703     C                   WHEN      C01CsrRrn > *ZERO
011100000703     C                   EXSR      WrkS01
011200030311     ** Invio col cursore fuori subfile.
011300000703     C                   OTHER
011400000703     C                   EXSR      Uscita
011500030311
011600000623     C                   ENDSL
011700030311
011800000622     C                   ENDDO
011900030311
012000000623     C                   EXSR      Uscita
012100000622
012200030311     ***********************************************************************
012300030311     **
012400030311     ** Caricamento SFL S01.
012500030311     **
012600030311     ***********************************************************************
012700000622     C     CarS01        BEGSR
012800000622
012900030311     ** Primo  giorno del mese in formato *ISO.
013000000622     C                   EXTRCT    WrkDtRice:*D  WrkD
013100000622     C                   EVAL      WrkD = WrkD - 1
013200000703     C     WrkDtRice     SUBDUR(E) WrkD:*D       WrkDtInMe
013300000703     C                   IF        %ERROR
013400000703     C                   EVAL      WrkDtInMe = *LOVAL
013500000703     C                   ENDIF
013600030311     ** Primo  giorno del mese in formato lilian.
013700000623     C     *ISO          MOVE      WrkDtInMe     ChrDate
013800000623     C                   CALLB(D)  'CEEDAYS'
013900000623     C                   PARM                    ChrDate
014000000703     C                   PARM                    PictureIso
014100000623     C                   PARM      *ZERO         LilDate
014200000623     C                   PARM      *BLANK        Fc
014300030311     ** Reperisco il numero del giorno della settimana (1-7).
014400000623     C                   EXSR      CalCEEDYWK
014500030311     ** Primo giorno (lunedì) del calendario.
014600000623     C                   IF        DayOfWeek > Lunedi
014700000622     C                   EVAL      WrkD = DayOfWeek - 1
014800000703     C     WrkDtInMe     SUBDUR(E) WrkD:*D       WrkDtInCa
014900000703     C                   IF        %ERROR
015000000703     C                   EVAL      WrkDtInMe = *LOVAL
015100000703     C                   ENDIF
015200000622     C                   ELSE
015300000622     C                   EVAL      WrkDtInCa = WrkDtInMe
015400000622     C                   ENDIF
015500000622
015600030311     ** Ultimo giorno del mese in formato *ISO
015700000703     C     WrkDtRice     ADDDUR(E) 1:*M          WrkDtFiMe
015800000703     C                   IF        %ERROR
015900000703     C                   EVAL      WrkDtFiMe = *HIVAL
016000000703     C                   ELSE
016100000622     C                   EXTRCT    WrkDtFiMe:*D  WrkD
016200000703     C     WrkDtFiMe     SUBDUR(E) WrkD:*D       WrkDtFiMe
016300000703     C                   IF        %ERROR
016400000703     C                   EVAL      WrkDtFiMe = *HIVAL
016500000703     C                   ENDIF
016600000703     C                   ENDIF
016700030311     ** Ultimo giorno del mese in formato lilian.
016800000623     C     *ISO          MOVE      WrkDtFiMe     ChrDate
016900000623     C                   CALLB(D)  'CEEDAYS'
017000000623     C                   PARM                    ChrDate
017100000622     C                   PARM                    PictureIso
017200000622     C                   PARM                    LilDate
017300000622     C                   PARM      *BLANK        Fc
017400030311     ** Reperisco il numero del giorno della settimana (1-7).
017500000623     C                   EXSR      CalCEEDYWK
017600030311     ** Ultimo giorno del calendario (domenica) in *ISO.
017700000623     C                   IF        DayOfWeek < Domenica
017800000703     C                             AND
017900000703     C                             DayOfWeek > *ZERO
018000000622     C                   EVAL      WrkD = 7 - DayOfWeek
018100000703     C     WrkDtFiMe     ADDDUR(E) WrkD:*D       WrkDtFiCa
018200000703     C                   IF        %ERROR
018300000703     C                   EVAL      WrkDtFiCa = *HIVAL
018400000703     C                   ENDIF
018500000622     C                   ELSE
018600000622     C                   EVAL      WrkDtFiCa = WrkDtFiMe
018700000622     C                   ENDIF
018800000622
018900030311     ** Pulisco il subfile e le variabili di gestione.
019000000622     C                   CLEAR                   RrnS01
019100000622     C                   EVAL      SflClr = *ON
019200000622     C                   WRITE     C01
019300000622     C                   EVAL      SflClr = *OFF
019400000703     C                   CLEAR                   S01
019500030311
019600000622     C                   EVAL      WrkDt = WrkDtInCa
019700000703     C                   EVAL      WrkFineS01 = *OFF
019800030311
019900000703     C                   DOU       WrkFineS01 = *ON
020000030311
020100000703     C                   IF        WrkDt >= LilianLoval
020200030311     ** Ultimo giorno del mese in formato lilian.
020300000623     C     *ISO          MOVE      WrkDt         ChrDate
020400000623     C                   CALLB(D)  'CEEDAYS'
020500000623     C                   PARM                    ChrDate
020600000622     C                   PARM                    PictureIso
020700000622     C                   PARM                    LilDate
020800000622     C                   PARM      *BLANK        Fc
020900030311     ** Reperisco il numero del giorno della settimana (1-7).
021000000623     C                   EXSR      CalCEEDYWK
021100030311
021200000623     C                   EVAL      WrkFesta = *OFF
021300000623     C                   IF        DayOfWeek = Domenica
021400000623     C                   EVAL      WrkFesta = *ON
021500000623     C                   ELSE
021600000623     C     *ISO          MOVE      WrkDt         ChrDate
021700000623     C                   MOVE      ChrDate       WrkMD
021800000623     C     WrkMD         LOOKUP    Feste                                  99
021900000623     C                   IF        %EQUAL
022000000623     C                   EVAL      WrkFesta = *ON
022100000623     C                   ENDIF
022200000623     C                   ENDIF
022300030311     ** Visualizzo ...
022400030311     ** - Oggi in rosso.
022500030311     ** - I festivi in inversione fondo.
022600030311     ** - I giorni del mese in bianco.
022700030311     ** - I giorni fuori del mese in verde.
022800000622     C                   SELECT
022900000622     C                   WHEN      WrkDt  = WrkDtOggi
023000030311     C                   EVAL      WrkAtr = Pink
023100000623     C                   IF        WrkFesta = *ON
023200030311     C                   EVAL      WrkAtr = PinkRI
023300000623     C                   ENDIF
023400000623     C                   WHEN      WrkDt < WrkDtInMe
023500000623     C                             OR
023600000623     C                             WrkDt > WrkDtFiMe
023700000623     C                   EVAL      WrkAtr = Green
023800000623     C                   IF        WrkFesta = *ON
023900000623     C                   EVAL      WrkAtr = GreenRI
024000000623     C                   ENDIF
024100000623     C                   OTHER
024200000703     C                   EVAL      WrkAtr = White
024300000623     C                   IF        WrkFesta = *ON
024400000703     C                   EVAL      WrkAtr = WhiteRI
024500000623     C                   ENDIF
024600000622     C                   ENDSL
024700030311
024800000623     C                   EXTRCT    WrkDt:*D      WrkD
024900030311
025000000622     C                   SELECT
025100000623     C                   WHEN      DayOfWeek = Lunedi
025200000630     C                   EVAL      S01DatLun = WrkDt
025300000622     C                   EVAL      S01DayLun = WrkD
025400000622     C                   EVAL      AtrDayLun = WrkAtr
025500000623     C                   WHEN      DayOfWeek = Martedi
025600000630     C                   EVAL      S01DatMar = WrkDt
025700000622     C                   EVAL      S01DayMar = WrkD
025800000622     C                   EVAL      AtrDayMar = WrkAtr
025900000623     C                   WHEN      DayOfWeek = Mercoledi
026000000630     C                   EVAL      S01DatMer = WrkDt
026100000622     C                   EVAL      S01DayMer = WrkD
026200000622     C                   EVAL      AtrDayMer = WrkAtr
026300000623     C                   WHEN      DayOfWeek = Giovedi
026400000630     C                   EVAL      S01DatGio = WrkDt
026500000622     C                   EVAL      S01DayGio = WrkD
026600000622     C                   EVAL      AtrDayGio = WrkAtr
026700000623     C                   WHEN      DayOfWeek = Venerdi
026800000630     C                   EVAL      S01DatVen = WrkDt
026900000622     C                   EVAL      S01DayVen = WrkD
027000000622     C                   EVAL      AtrDayVen = WrkAtr
027100000623     C                   WHEN      DayOfWeek = Sabato
027200000630     C                   EVAL      S01DatSab = WrkDt
027300000622     C                   EVAL      S01DaySab = WrkD
027400000622     C                   EVAL      AtrDaySab = WrkAtr
027500000623     C                   WHEN      DayOfWeek = Domenica
027600000630     C                   EVAL      S01DatDom = WrkDt
027700000622     C                   EVAL      S01DayDom = WrkD
027800000622     C                   EVAL      AtrDayDom = WrkAtr
027900000622     C                   ENDSL
028000030311
028100000623     C                   IF        DayOfWeek = Domenica
028200000703     C                             OR
028300000703     C                             WrkDt = WrkDtFiCa
028400000622     C                   ADD       1             RrnS01
028500000622     C                   WRITE     S01
028600000703     C                   CLEAR                   S01
028700000622     C                   ENDIF
028800030311
028900000703     C                   ENDIF
029000030311
029100000703     C                   IF        WrkDt < WrkDtFiCa
029200000622     C                   ADDDUR    1:*D          WrkDt
029300000703     C                   ELSE
029400000703     C                   EVAL      WrkFineS01 = *ON
029500000703     C                   ENDIF
029600030311
029700000622     C                   ENDDO
029800030311
029900000623     C                   EXTRCT    WrkDtInMe:*Y  C01Anno
030000000623     C                   EXTRCT    WrkDtInMe:*M  WrkM
030100000623     C                   EVAL      C01Mese = Mese(WrkM)
030200030311
030300000622     C                   ENDSR
030400000630
030500030311     ***********************************************************************
030600030311     **
030700030311     ** Selezione giorno.
030800030311     **
030900030311     ***********************************************************************
031000000703     C     WrkS01        BEGSR
031100030311
031200000703     C                   IF        %SUBST(C01RtnFld:1:6) = 'S01DAY'
031300030311
031400000703     C                   EVAL      RrnS01 = C01CsrRrn
031500000703     C     RrnS01        CHAIN     S01
031600030311
031700000703     C                   IF        %FOUND
031800030311
031900000703     C                   SELECT
032000000703     C                   WHEN      C01RtnFld = 'S01DAYLUN'
032100000703     C                   EVAL      PrmDataO = S01DatLun
032200000703     C                   WHEN      C01RtnFld = 'S01DAYMAR'
032300000703     C                   EVAL      PrmDataO = S01DatMar
032400000703     C                   WHEN      C01RtnFld = 'S01DAYMER'
032500000703     C                   EVAL      PrmDataO = S01DatMer
032600000703     C                   WHEN      C01RtnFld = 'S01DAYGIO'
032700000703     C                   EVAL      PrmDataO = S01DatGio
032800000703     C                   WHEN      C01RtnFld = 'S01DAYVEN'
032900000703     C                   EVAL      PrmDataO = S01DatVen
033000000703     C                   WHEN      C01RtnFld = 'S01DAYSAB'
033100000703     C                   EVAL      PrmDataO = S01DatSab
033200000703     C                   WHEN      C01RtnFld = 'S01DAYDOM'
033300000703     C                   EVAL      PrmDataO = S01DatDom
033400000703     C                   ENDSL
033500030311
033600000703     C                   EXSR      Uscita
033700030311
033800000703     C                   ENDIF
033900030311
034000000630     C                   ENDIF
034100030311
034200000630     C                   ENDSR
034300000623
034400030311     ***********************************************************************
034500030311     **
034600030311     ** Chiamo CEEDYWK.
034700030311     **
034800030311     ***********************************************************************
034900000623     C     CalCEEDYWK    BEGSR
035000030311
035100000623     C                   CALLB(D)  'CEEDYWK'
035200000623     C                   PARM                    LilDate
035300000623     C                   PARM      *ZERO         DayOfWeek
035400000623     C                   PARM      *BLANK        Fc
035500030311     ** Sposto domenica al settimo posto.
035600000703     C                   IF        DayOfWeek > *ZERO
035700000623     C                   SELECT
035800000623     C                   WHEN      DayOfWeek = 1
035900000623     C                   EVAL      DayOfWeek = 7
036000000623     C                   OTHER
036100000623     C                   EVAL      DayOfWeek = DayOfWeek - 1
036200000623     C                   ENDSL
036300000703     C                   ENDIF
036400030311
036500000623     C                   ENDSR
036600000623
036700030311     ***********************************************************************
036800030311     **
036900030311     ** Ricezione e controllo parametri.
037000030311     **
037100030311     ***********************************************************************
037200000623     C     RcvParm       BEGSR
037300030311     ** Pulizia dei parametri da restituire.
037400000703     C                   CLEAR                   PrmDataO
037500000703     C                   CLEAR                   PrmEsito
037600030311     ** Controllo formale della data ricevuta.
037700000703     C                   TEST(E)                 PrmDataI
037800030311     ** Se la data è fromalmente sbagliata
037900030311     ** o è minore del 15 ottobre 1582
038000030311     ** forzo oggi.
038100000623     C                   IF        %ERROR
038200000703     C                             OR
038300000703     C                             PrmDataI < LilianLoval
038400000703     C                   EVAL      PrmDataI = WrkDtOggi
038500000623     C                   ENDIF
038600030311
038700000623     C                   ENDSR
038800000623
038900030311     ***********************************************************************
039000030311     **
039100030311     ** Uscita programma.
039200030311     **
039300030311     ***********************************************************************
039400000623     C     Uscita        BEGSR
039500030311
039600000623     C                   IF        PrmEsito = *BLANK
039700000623     C                   EVAL      PrmEsito = Eseguito
039800000623     C                   ENDIF
039900030311
040000030311     C                   CLOSE(E)  CalendaD
040100030311
040200000623     C                   RETURN
040300030311
040400000623     C                   ENDSR
040500000630
040600030311     ***********************************************************************
040700030311     **
040800030311     ** Operazioni iniziali.
040900030311     **
041000030311     ***********************************************************************
041100000630     C     *INZSR        BEGSR
041200030311
041300000630     C                   EVAL      C01MesePre = '<'
041400000630     C                   EVAL      C01MeseSuc = '>'
041500030311
041600000630     C                   ENDSR
041700000623
041800000622**CTDATA Mese
041900000622Gennaio
042000000622Febbraio
042100000622Marzo
042200000622Aprile
042300000622Maggio
042400000622Giugno
042500000622Luglio
042600000622Agosto
042700000622Settembre
042800000622Ottobre
042900000622Novembre
043000000622Dicembre
043100000623**CTDATA Feste
04320000070301-01 Capodanno
04330000070301-06 Epifania
04340000070304-25 Liberazione
04350000070305-01 Festa del lavoro
04360003031106-02 Festa della Repubblica
04370000070308-15 Assunzione della Vergine Maria
04380000070311-01 Ognissanti
04390000070312-08 Immacolata concezione
04400000070312-25 Natale
04410000070312-26 Santo Stefano
