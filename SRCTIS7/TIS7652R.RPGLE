000100060609     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('TIS')
000200051114
000300051114     Fazcln01l  IF   E           K DISK                                         Calendario
000400051114     F                                     USROPN
000500050902
000600050902     ***********************************************************************************************
000700050902     **?
000800050902     **?Definizione costanti.
000900050902     **?
001000050902     ***********************************************************************************************
001100050902     D StessoIndirizzo...
001200050902     D                 C                   '1'
001300050902     D AltroIndirizzo  C                   '2'
001400050902     D FermoDeposito   C                   '3'
001500050902     D Rendere         C                   '4'
001600050902     D Altre           C                   '5'
001700050902     D AppenaPossibile...
001800050902     D                 C                   '1'
001900051114     D InData          C                   '2'
002000050902     D DopoIl          C                   '3'
002100050902     D Appuntamento    C                   '4'
002200050902     D Italia          C                   ' '
002300051114     D Festa           C                   'F'
002400060606
002500060606     ***********************************************************************************************
002600060606     **?
002700060606     **?Definizione procedure usate.
002800060606     **?
002900060606     ***********************************************************************************************
003000060609     D rtvMsgLang      PR          3512A                                        Messaggio in lingua
003100060609     D  msgId                         7A   CONST
003200060609     D  piLinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
003300060613     D  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
003301060613     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
003600060612     D                                     VARYING
003700060612     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
003800060612     D                                     VARYING
003900060609     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
004000060609     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
004100060609
004200050902     ***********************************************************************************************
004300050902     **?
004400050902     **?Definizione strutture dati.
004500050902     **?
004600050902     ***********************************************************************************************
004700050906     D psds           SDS           170
004800050902     D tis1650dsi    E DS
004900050831     D                                     PREFIX(rqs)
005000050902     D                                     INZ
005100050902     D tisi95ds      E DS
005200050902     D                                     INZ
005300050902     D  i95tcn       E
005400050905     D                                     INZ('7')
005500050902
005600050902     ***********************************************************************************************
005700050902     **?
005800050902     **?Definizione variabili.
005900050902     **?
006000050902     ***********************************************************************************************
006100050902     D rqsDta          S           9999
006200050902     D rpyDta          S           9999
006300050902     D esito           S              5I 0
006400050902     D wrkMsg          S           9999
006500050902     D                                     VARYING
006600050902     D wrkCurDate      S               D
006700050902     D                                     DATFMT(*EUR)
006800050907     D wrkDomani       S               D
006900050907     D                                     DATFMT(*EUR)
007000050902     D wrkDtLimite     S               D
007100050902     D                                     DATFMT(*EUR)
007200050902     D wrkNdcr         S               D
007300050902     D                                     DATFMT(*EUR)
007400051114     D y               S              5I 0                                      Anno
007500051114     D m               S              3I 0                                      Mese
007600051114     D d               S              3I 0                                      Giorno
007700060606     D msgId...
007800060606     D                 S              7A
007900060606
008000051114     ***********************************************************************************************
008100051114     **?
008200051114     **?Definizione chiavi database.
008300051114     **?
008400051114     ***********************************************************************************************
008500051114     C     k04cln01      KLIST
008600051114     C                   KFLD                    clnTFP
008700051114     C                   KFLD                    clnTFA
008800051114     C                   KFLD                    clnAnn
008900051114     C                   KFLD                    clnMes
009000050831
009100050902     ***********************************************************************************************
009200050902     **?
009300050902     **?Definizione parametri.
009400050902     **?
009500050902     ***********************************************************************************************
009600050902     C     *ENTRY        PLIST
009700050902     C     tis1650dsi    PARM                    rqsDta
009800050902     C                   PARM                    rpyDta
009900050902     C                   PARM                    esito
010000050902
010100050902     ***********************************************************************************************
010200050902     **?
010300050902     **?Main.
010400050902     **?
010500050902     ***********************************************************************************************
010600050902     C                   EXSR      chkParm
010700050902     C                   EXSR      inzVar
010800050902
010900050902     C                   SELECT
011000050902     C                   WHEN      rqsTpDisposiz = StessoIndirizzo
011100050902     C                   EXSR      chkStessoInd
011200050902     C                   WHEN      rqsTpDisposiz = AltroIndirizzo
011300050902     C                   EXSR      chkAltroInd
011400050902     C                   WHEN      rqsTpDisposiz = FermoDeposito
011500050902     C                   EXSR      chkFermoDepos
011600050902     C                   WHEN      rqsTpDisposiz = Rendere
011700050902     C*                  EXSR      chkRendere
011800050902     C                   WHEN      rqsTpDisposiz = Altre
011900050902     C                   EXSR      chkAltre
012000050902     C                   OTHER
012100050902     C                   EVAL      esito = -1
012200060606     C*                  EVAL      wrkMsg = wrkMsg +
012300060606     C*                            'Il valore '' + rqsTpDisposiz + '' per il -
012400060606     C*                            parametro tipo disposizione non è valido.-
012500060606     C*                            <br>'
012600060609     C                   EVAL      wrkMsg += %TRIMR(rtvMsgLang('TIS0228'
012700060612     C                             :rqsLang:rqsTpDisposiz)) + '<br>'
012800060606     C                   ENDSL
012900050902
013000050902     C                   EVAL      rpyDta = wrkMsg
013100050902     C                   EXSR      uscita
013200050902
013300050902     ***********************************************************************************************
013400050902     **?
013500050902     **?Operazioni iniziali.
013600050902     **?
013700050902     ***********************************************************************************************
013800050902     C     *INZSR        BEGSR
013900050902
014000050902
014100050902     C                   ENDSR
014200050902
014300050902     ***********************************************************************************************
014400050902     **?
014500050902     **?Controllo parametri ricevuti.
014600050902     **?
014700050902     ***********************************************************************************************
014800050902     C     chkParm       BEGSR
014900050902
015000050902     C                   CLEAR                   rpyDta
015100050902     C                   CLEAR                   esito
015200050902
015300050902     C                   ENDSR
015400050902
015500050902     ***********************************************************************************************
015600050902     **?
015700050902     **?Inizializzazione variabili.
015800050902     **?
015900050902     ***********************************************************************************************
016000050902     C     inzVar        BEGSR
016100050902
016200050902     C                   CLEAR                   wrkMsg
016300050905     C                   TIME                    wrkCurDate
016400050902
016500050902     C                   ENDSR
016600050902
016700050902     ***********************************************************************************************
016800050902     **?
016900050902     **?Controllo consegna stesso indirizzo.
017000050902     **?
017100050902     ***********************************************************************************************
017200050902     C     chkStessoInd  BEGSR
017300050902
017400050902     C                   SELECT
017500050902     C
017600050902     C                   WHEN      rqsStessoInd = AppenaPossibile
017700050902     C
017800051114     C                   WHEN      rqsStessoInd = InData OR
017900051114     C                             rqsStessoInd = DopoIl
018000050902     C     *EUR          TEST(DE)                rqsNdcr
018100050902     C                   IF        %ERROR
018200060606     C*                  EVAL      wrkMsg = wrkMsg +
018300060606     C*                            'La data consegna richiesta ' + rqsNdcr +
018400060606     C*                            ' non è formalmente corretta.<br>'
018500060609     C                   EVAL      wrkMsg += %TRIMR(rtvMsgLang('TIS0314'
018600060612     C                             :rqsLang:rqsNdcr)) + '<br>'
018700060609     C                   ELSE
018800050902     C     *EUR          MOVE      rqsNdcr       wrkNdcr
018900050907     C     wrkCurDate    ADDDUR    1:*D          wrkDomani
019000050902     C     wrkCurDate    ADDDUR    1:*M          wrkDtLimite
019100050907     C                   IF        wrkNdcr < wrkDomani OR wrkNdcr > wrkDtLimite
019200060606     C*                  EVAL      wrkMsg = wrkMsg +
019300060606     C*                            'La data consegna richiesta ' + rqsNdcr +
019400060606     C*                            ' è fuori dal periodo ' + %CHAR(wrkDomani) +
019500060606     C*                            ' - ' + %CHAR(wrkDtLimite) + '.<br>'
019600060609     C                   EVAL      wrkMsg += %TRIMR(rtvMsgLang('TIS0315'
019700060612     C                             :rqsLang:rqsNdcr + %CHAR(wrkDomani)
019800060612     C                             + %CHAR(wrkDtLimite))) + '<br>'
019900060609     C                   ELSE
020000051114     C                   IF        rqsStessoInd = InData
020100051114     C                   EXSR      chkFesta
020200051114     C                   ENDIF
020300050902     C                   ENDIF
020400050902     C                   ENDIF
020500050902     C
020600050902     C                   WHEN      rqsStessoInd = Appuntamento
020700050902     C                   IF        rqsAptPersona = *BLANK
020800060607     C*                  EVAL      wrkMsg = wrkMsg +
020900060607     C*                            'Immettere il nome della persona da -
021000060607     C*                            contattare per l''appuntamento.<br>'
021100060609     C                   EVAL      wrkMsg += %TRIMR(rtvMsgLang('TIS0243'
021200060609     C                             :rqsLang)) + '<br>'
021300060609     C                   ENDIF
021400050902     C                   IF        rqsAptTelefon = *BLANK
021500060607     C*                  EVAL      wrkMsg = wrkMsg +
021600060607     C*                            'Immettere il numero telefonico della -
021700060607     C*                            persona da contattare per l''appuntamento.-
021800060607     C*                            <br>'
021900060609     C                   EVAL      wrkMsg += %TRIMR(rtvMsgLang('TIS0248'
022000060609     C                             :rqsLang)) + '<br>'
022100060609     C                   ENDIF
022200050902     C
022300050902     C                   OTHER
022400050902     C                   EVAL      esito = -2
022500060607     C*                  EVAL      wrkMsg = wrkMsg +
022600060607     C*                            'Il valore '' + rqsStessoInd + '' per il -
022700060607     C*                            parametro sceltaStessoIndirizzo non è -
022800060607     C*                            valido.<br>'
022900060609     C                   EVAL      wrkMsg += %TRIMR(rtvMsgLang('TIS0227'
023000060612     C                             :rqsLang:rqsStessoInd)) + '<br>'
023100060609     C                   ENDSL
023200050902
023300050902     C                   ENDSR
023400050902
023500050902     ***********************************************************************************************
023600050902     **?
023700050902     **?Controllo consegna altro indirizzo.
023800050902     **?
023900050902     ***********************************************************************************************
024000050902     C     chkAltroInd   BEGSR
024100050902
024200050902     C                   IF        rqsAinRagSoc1 = *BLANK
024300060607     C*                  EVAL      wrkMsg = wrkMsg +
024400060607     C*                            'Immettere il nome o la ragione sociale del -
024500060607     C*                            nuovo destinatario.<br>'
024600060609     C                   EVAL      wrkMsg += %TRIMR(rtvMsgLang('TIS0244'
024700060609     C                             :rqsLang)) + '<br>'
024800060609     C                   ENDIF
024900050902
025000050902     C                   IF        rqsAinIndiriz = *BLANK
025100060607     C*                  EVAL      wrkMsg = wrkMsg +
025200060607     C*                            'Immettere l''indirizzo del nuovo -
025300060607     C*                            destinatario.<br>'
025400060609     C                   EVAL      wrkMsg += %TRIMR(rtvMsgLang('TIS0254'
025500060609     C                             :rqsLang)) + '<br>'
025600060609     C                   ENDIF
025700050902
025800050902     C                   IF        rqsAinCap = *BLANK
025900060607     C*                  EVAL      wrkMsg = wrkMsg +
026000060607     C*                            'Immettere il C.A.P. del nuovo -
026100060607     C*                            destinatario.<br>'
026200060609     C                   EVAL      wrkMsg += %TRIMR(rtvMsgLang('TIS0238'
026300060609     C                             :rqsLang)) + '<br>'
026400060609     C                   ENDIF
026500050902
026600050902     C                   IF        rqsAinLocalit = *BLANK
026700060607     C*                  EVAL      wrkMsg = wrkMsg +
026800060607     C*                            'Immettere la località del nuovo -
026900060607     C*                            destinatario.<br>'
027000060609     C                   EVAL      wrkMsg += %TRIMR(rtvMsgLang('TIS0261'
027100060609     C                             :rqsLang)) + '<br>'
027200060609     C                   ENDIF
027300050902
027400050902     C                   IF        rqsAinNazione = Italia AND
027500050902     C                             rqsAinProvinc = *BLANK
027600060607     C*                  EVAL      wrkMsg = wrkMsg +
027700060607     C*                            'Scegliere la provincia del nuovo -
027800060607     C*                            destinatario.<br>'
027900060609     C                   EVAL      wrkMsg += %TRIMR(rtvMsgLang('TIS0538'
028000060609     C                             :rqsLang)) + '<br>'
028100060609     C                   ENDIF
028200050902
028300050902     C                   IF        rqsAinLocalit <> *BLANK AND
028400050902     C                             rqsAinCap <> *BLANK
028500050902     C                   EXSR      chkCapLoc
028600050902     C                   ENDIF
028700050902
028800050902     C                   ENDSR
028900050902
029000050902     ***********************************************************************************************
029100050902     **?
029200050902     **?Controllo consegna fermo deposito.
029300050902     **?
029400050902     ***********************************************************************************************
029500050902     C     chkFermoDepos BEGSR
029600050902
029700050902     C                   IF        rqsRfdPersona = *BLANK
029800060607     C*                  EVAL      wrkMsg = wrkMsg +
029900060607     C*                            'Immettere il nome della persona che passerà-
030000060607     C*                             a ritirare.<br>'
030100060609     C                   EVAL      wrkMsg += %TRIMR(rtvMsgLang('TIS0242'
030200060609     C                             :rqsLang)) + '<br>'
030300060609     C                   ENDIF
030400050902
030500050902     C                   ENDSR
030600050902
030700050902     ***********************************************************************************************
030800050902     **?
030900050902     **?Controllo altre disposizioni.
031000050902     **?
031100050902     ***********************************************************************************************
031200050902     C     chkAltre      BEGSR
031300050902
031400050902     C                   IF        rqsDmc = *BLANK
031500060607     C*                  EVAL      wrkMsg = wrkMsg +
031600060607     C*                            'Immettere le disposizioni.<br>'
031700060609     C                   EVAL      wrkMsg += %TRIMR(rtvMsgLang('TIS0264'
031800060609     C                             :rqsLang)) + '<br>'
031900060609     C                   ENDIF
032000050902
032100050902     C                   ENDSR
032200050902
032300050902     ***********************************************************************************************
032400050902     **?
032500050902     **?Controllo località e CAP.
032600050902     **?
032700050902     ***********************************************************************************************
032800050902     C     chkCapLoc     BEGSR
032900050902
033000050902     C                   RESET                   tisi95ds
033100050902     C                   EVAL      i95nar = rqsAinNazione
033200050902     C                   EVAL      i95cap = rqsAinCap
033300050902     C                   EVAL      i95loc = rqsAinLocalit
033400050902     C                   EVAL      i95prv = rqsAinProvinc
033500050905     C                   EVAL      i95ind = rqsAinIndiriz
033600050905     C     *ISO          MOVE      wrkCurDate    i95dat
033700050902
033800050902     C                   CALL(E)   'TISI95R'
033900050902     C                   PARM                    tisi95ds
034000050902
034100050902     C                   IF        %ERROR
034200050906     C                   EVAL      esito = %STATUS * -1
034300050906     C                   EVAL      wrkMsg = wrkMsg + psds + '<br>'
034400050902     C                   ELSE
034500050905     C                   IF        o95err <> *BLANK OR
034600060607     C                             (i95nar =  *BLANK AND o95lia <> '3') OR
034700060607     C                             (i95nar <> *BLANK AND o95lia <> '2')
034800050905     C                   IF        o95msg <> *BLANK
034900050905     C                   EVAL      wrkMsg = wrkMsg +
035000050905     C                             %TRIMR(%SUBST(o95msg:10:68)) + '<br>'
035100050905     C                   ELSE
035200060607     C*                  EVAL      wrkMsg = wrkMsg + 'CAP, località e provincia-
035300060607     C*                             non congruenti tra loro.<br>'
035400060609     C                   EVAL      wrkMsg += %TRIMR(rtvMsgLang('TIS0054'
035500060609     C                             :rqsLang)) + '<br>'
035600060609     C                   ENDIF
035700050902     C                   ENDIF
035800050902     C                   ENDIF
035900050902
036000050902     C                   ENDSR
036100051114
036200051114     ***********************************************************************************************
036300051114     **?
036400051114     **?Controllo data festa.
036500051114     **?
036600051114     ***********************************************************************************************
036700051114     C     chkFesta      BEGSR
036800051114
036900051114     C                   EXTRCT    wrkNdcr:*Y    y
037000051114     C                   EXTRCT    wrkNdcr:*M    m
037100051114
037200051114     C                   IF        m <> clnMes OR y <> clnAnn
037300051115     C                             OR NOT %FOUND(azcln01l)
037400051114     C                   EVAL      clnAnn = y
037500051114     C                   EVAL      clnMes = m
037600051115     C                   OPEN      azcln01l
037700051114     C     k04cln01      CHAIN     azcln01l
037800051115     C                   CLOSE     azcln01l
037900051114     C                   ENDIF
038000051114
038100051114     C                   IF        %FOUND(azcln01l)
038200051114     C                   EXTRCT    wrkNdcr:*D    D
038300051114     C                   IF        %SUBST(clnPom:d:1) = Festa
038400060607     C*                  EVAL      wrkMsg = wrkMsg + 'La data richiesta deve -
038500060607     C*                            essere un giorno lavorativo.<br>'
038600060609     C                   EVAL      wrkMsg += %TRIMR(rtvMsgLang('TIS0327'
038700060609     C                             :rqsLang)) + '<br>'
038800060609     C                   ENDIF
038900051114     C                   ENDIF
039000051114
039100051114     C                   ENDSR
039200060606
039300050902     ***********************************************************************************************
039400050902     **?
039500050902     **?Operazioni finali.
039600050902     **?
039700050902     ***********************************************************************************************
039800050902     C     uscita        BEGSR
039900050902
040000050902     C                   RETURN
040100050902
040200050902     C                   ENDSR
