000100050921     H DFTACTGRP(*NO) ACTGRP(*CALLER)
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
002400050902
002500050902     ***********************************************************************************************
002600050902     **?
002700050902     **?Definizione strutture dati.
002800050902     **?
002900050902     ***********************************************************************************************
003000050906     D psds           SDS           170
003100050902     D tis1650dsi    E DS
003200050831     D                                     PREFIX(rqs)
003300050902     D                                     INZ
003400050902     D tisi95ds      E DS
003500050902     D                                     INZ
003600050902     D  i95tcn       E
003700050905     D                                     INZ('7')
003800050902
003900050902     ***********************************************************************************************
004000050902     **?
004100050902     **?Definizione variabili.
004200050902     **?
004300050902     ***********************************************************************************************
004400050902     D rqsDta          S           9999
004500050902     D rpyDta          S           9999
004600050902     D esito           S              5I 0
004700050902     D wrkMsg          S           9999
004800050902     D                                     VARYING
004900050902     D wrkCurDate      S               D
005000050902     D                                     DATFMT(*EUR)
005100050907     D wrkDomani       S               D
005200050907     D                                     DATFMT(*EUR)
005300050902     D wrkDtLimite     S               D
005400050902     D                                     DATFMT(*EUR)
005500050902     D wrkNdcr         S               D
005600050902     D                                     DATFMT(*EUR)
005700051114     D y               S              5I 0                                      Anno
005800051114     D m               S              3I 0                                      Mese
005900051114     D d               S              3I 0                                      Giorno
006000051114
006100051114     ***********************************************************************************************
006200051114     **?
006300051114     **?Definizione chiavi database.
006400051114     **?
006500051114     ***********************************************************************************************
006600051114     C     k04cln01      KLIST
006700051114     C                   KFLD                    clnTFP
006800051114     C                   KFLD                    clnTFA
006900051114     C                   KFLD                    clnAnn
007000051114     C                   KFLD                    clnMes
007100050831
007200050902     ***********************************************************************************************
007300050902     **?
007400050902     **?Definizione parametri.
007500050902     **?
007600050902     ***********************************************************************************************
007700050902     C     *ENTRY        PLIST
007800050902     C     tis1650dsi    PARM                    rqsDta
007900050902     C                   PARM                    rpyDta
008000050902     C                   PARM                    esito
008100050902
008200050902     ***********************************************************************************************
008300050902     **?
008400050902     **?Main.
008500050902     **?
008600050902     ***********************************************************************************************
008700050902     C                   EXSR      chkParm
008800050902     C                   EXSR      inzVar
008900050902
009000050902     C                   SELECT
009100050902     C                   WHEN      rqsTpDisposiz = StessoIndirizzo
009200050902     C                   EXSR      chkStessoInd
009300050902     C                   WHEN      rqsTpDisposiz = AltroIndirizzo
009400050902     C                   EXSR      chkAltroInd
009500050902     C                   WHEN      rqsTpDisposiz = FermoDeposito
009600050902     C                   EXSR      chkFermoDepos
009700050902     C                   WHEN      rqsTpDisposiz = Rendere
009800050902     C*                  EXSR      chkRendere
009900050902     C                   WHEN      rqsTpDisposiz = Altre
010000050902     C                   EXSR      chkAltre
010100050902     C                   OTHER
010200050902     C                   EVAL      esito = -1
010300050906     C                   EVAL      wrkMsg = wrkMsg +
010400050906     C                             'Il valore '' + rqsTpDisposiz + '' per il -
010500050906     C                             parametro tipo disposizione non è valido.-
010600050906     C                             <br>'
010700050902     C                   ENDSL
010800050902
010900050902     C                   EVAL      rpyDta = wrkMsg
011000050902     C                   EXSR      uscita
011100050902
011200050902     ***********************************************************************************************
011300050902     **?
011400050902     **?Operazioni iniziali.
011500050902     **?
011600050902     ***********************************************************************************************
011700050902     C     *INZSR        BEGSR
011800050902
011900050902
012000050902     C                   ENDSR
012100050902
012200050902     ***********************************************************************************************
012300050902     **?
012400050902     **?Controllo parametri ricevuti.
012500050902     **?
012600050902     ***********************************************************************************************
012700050902     C     chkParm       BEGSR
012800050902
012900050902     C                   CLEAR                   rpyDta
013000050902     C                   CLEAR                   esito
013100050902
013200050902     C                   ENDSR
013300050902
013400050902     ***********************************************************************************************
013500050902     **?
013600050902     **?Inizializzazione variabili.
013700050902     **?
013800050902     ***********************************************************************************************
013900050902     C     inzVar        BEGSR
014000050902
014100050902     C                   CLEAR                   wrkMsg
014200050905     C                   TIME                    wrkCurDate
014300050902
014400050902     C                   ENDSR
014500050902
014600050902     ***********************************************************************************************
014700050902     **?
014800050902     **?Controllo consegna stesso indirizzo.
014900050902     **?
015000050902     ***********************************************************************************************
015100050902     C     chkStessoInd  BEGSR
015200050902
015300050902     C                   SELECT
015400050902     C
015500050902     C                   WHEN      rqsStessoInd = AppenaPossibile
015600050902     C
015700051114     C                   WHEN      rqsStessoInd = InData OR
015800051114     C                             rqsStessoInd = DopoIl
015900050902     C     *EUR          TEST(DE)                rqsNdcr
016000050902     C                   IF        %ERROR
016100050902     C                   EVAL      wrkMsg = wrkMsg +
016200050902     C                             'La data consegna richiesta ' + rqsNdcr +
016300050902     C                             ' non è formalmente corretta.<br>'
016400050902     C                   ELSE
016500050902     C     *EUR          MOVE      rqsNdcr       wrkNdcr
016600050907     C     wrkCurDate    ADDDUR    1:*D          wrkDomani
016700050902     C     wrkCurDate    ADDDUR    1:*M          wrkDtLimite
016800050907     C                   IF        wrkNdcr < wrkDomani OR wrkNdcr > wrkDtLimite
016900050902     C                   EVAL      wrkMsg = wrkMsg +
017000050902     C                             'La data consegna richiesta ' + rqsNdcr +
017100050907     C                             ' è fuori dal periodo ' + %CHAR(wrkDomani) +
017200050902     C                             ' - ' + %CHAR(wrkDtLimite) + '.<br>'
017300051114     C                   ELSE
017400051114     C                   IF        rqsStessoInd = InData
017500051114     C                   EXSR      chkFesta
017600051114     C                   ENDIF
017700050902     C                   ENDIF
017800050902     C                   ENDIF
017900050902     C
018000050902     C                   WHEN      rqsStessoInd = Appuntamento
018100050902     C                   IF        rqsAptPersona = *BLANK
018200050902     C                   EVAL      wrkMsg = wrkMsg +
018300050902     C                             'Immettere il nome della persona da -
018400050902     C                             contattare per l''appuntamento.<br>'
018500050902     C                   ENDIF
018600050902     C                   IF        rqsAptTelefon = *BLANK
018700050902     C                   EVAL      wrkMsg = wrkMsg +
018800050902     C                             'Immettere il numero telefonico della -
018900050902     C                             persona da contattare per l''appuntamento.-
019000050902     C                             <br>'
019100050902     C                   ENDIF
019200050902     C
019300050902     C                   OTHER
019400050902     C                   EVAL      esito = -2
019500050906     C                   EVAL      wrkMsg = wrkMsg +
019600050906     C                             'Il valore '' + rqsStessoInd + '' per il -
019700050906     C                             parametro sceltaStessoIndirizzo non è -
019800050906     C                             valido.<br>'
019900050902     C                   ENDSL
020000050902
020100050902     C                   ENDSR
020200050902
020300050902     ***********************************************************************************************
020400050902     **?
020500050902     **?Controllo consegna altro indirizzo.
020600050902     **?
020700050902     ***********************************************************************************************
020800050902     C     chkAltroInd   BEGSR
020900050902
021000050902     C                   IF        rqsAinRagSoc1 = *BLANK
021100050902     C                   EVAL      wrkMsg = wrkMsg +
021200050902     C                             'Immettere il nome o la ragione sociale del -
021300050902     C                             nuovo destinatario.<br>'
021400050902     C                   ENDIF
021500050902
021600050902     C                   IF        rqsAinIndiriz = *BLANK
021700050902     C                   EVAL      wrkMsg = wrkMsg +
021800050902     C                             'Immettere l''indirizzo del nuovo -
021900050902     C                             destinatario.<br>'
022000050902     C                   ENDIF
022100050902
022200050902     C                   IF        rqsAinCap = *BLANK
022300050902     C                   EVAL      wrkMsg = wrkMsg +
022400050902     C                             'Immettere il C.A.P. del nuovo -
022500050902     C                             destinatario.<br>'
022600050902     C                   ENDIF
022700050902
022800050902     C                   IF        rqsAinLocalit = *BLANK
022900050902     C                   EVAL      wrkMsg = wrkMsg +
023000050902     C                             'Immettere la località del nuovo -
023100050902     C                             destinatario.<br>'
023200050902     C                   ENDIF
023300050902
023400050902     C                   IF        rqsAinNazione = Italia AND
023500050902     C                             rqsAinProvinc = *BLANK
023600050902     C                   EVAL      wrkMsg = wrkMsg +
023700050902     C                             'Scegliere la provincia del nuovo -
023800050902     C                             destinatario.<br>'
023900050902     C                   ENDIF
024000050902
024100050902     C                   IF        rqsAinLocalit <> *BLANK AND
024200050902     C                             rqsAinCap <> *BLANK
024300050902     C                   EXSR      chkCapLoc
024400050902     C                   ENDIF
024500050902
024600050902     C                   ENDSR
024700050902
024800050902     ***********************************************************************************************
024900050902     **?
025000050902     **?Controllo consegna fermo deposito.
025100050902     **?
025200050902     ***********************************************************************************************
025300050902     C     chkFermoDepos BEGSR
025400050902
025500050902     C                   IF        rqsRfdPersona = *BLANK
025600050902     C                   EVAL      wrkMsg = wrkMsg +
025700050902     C                             'Immettere il nome della persona che passerà-
025800050902     C                              a ritirare.<br>'
025900050902     C                   ENDIF
026000050902
026100050902     C                   ENDSR
026200050902
026300050902     ***********************************************************************************************
026400050902     **?
026500050902     **?Controllo altre disposizioni.
026600050902     **?
026700050902     ***********************************************************************************************
026800050902     C     chkAltre      BEGSR
026900050902
027000050902     C                   IF        rqsDmc = *BLANK
027100050902     C                   EVAL      wrkMsg = wrkMsg +
027200050902     C                             'Immettere le disposizioni.<br>'
027300050902     C                   ENDIF
027400050902
027500050902     C                   ENDSR
027600050902
027700050902     ***********************************************************************************************
027800050902     **?
027900050902     **?Controllo località e CAP.
028000050902     **?
028100050902     ***********************************************************************************************
028200050902     C     chkCapLoc     BEGSR
028300050902
028400050902     C                   RESET                   tisi95ds
028500050902     C                   EVAL      i95nar = rqsAinNazione
028600050902     C                   EVAL      i95cap = rqsAinCap
028700050902     C                   EVAL      i95loc = rqsAinLocalit
028800050902     C                   EVAL      i95prv = rqsAinProvinc
028900050905     C                   EVAL      i95ind = rqsAinIndiriz
029000050905     C     *ISO          MOVE      wrkCurDate    i95dat
029100050902
029200050902     C                   CALL(E)   'TISI95R'
029300050902     C                   PARM                    tisi95ds
029400050902
029500050902     C                   IF        %ERROR
029600050906     C                   EVAL      esito = %STATUS * -1
029700050906     C                   EVAL      wrkMsg = wrkMsg + psds + '<br>'
029800050902     C                   ELSE
029900050905     C                   IF        o95err <> *BLANK OR
030000050905if  3c                             (i95nar =  *BLANK AND o95lia <> '3') OR
030100050905     c                             (i95nar <> *BLANK AND o95lia <> '2')
030200050905     C                   IF        o95msg <> *BLANK
030300050905     C                   EVAL      wrkMsg = wrkMsg +
030400050905     C                             %TRIMR(%SUBST(o95msg:10:68)) + '<br>'
030500050905     C                   ELSE
030600050905     C                   EVAL      wrkMsg = wrkMsg + 'CAP, località e provincia-
030700050905     C                              non congruenti tra loro.<br>'
030800050905     C                   ENDIF
030900050902     C                   ENDIF
031000050902     C                   ENDIF
031100050902
031200050902     C                   ENDSR
031300051114
031400051114     ***********************************************************************************************
031500051114     **?
031600051114     **?Controllo data festa.
031700051114     **?
031800051114     ***********************************************************************************************
031900051114     C     chkFesta      BEGSR
032200051114
032300051114     C                   EXTRCT    wrkNdcr:*Y    y
032400051114     C                   EXTRCT    wrkNdcr:*M    m
032500051114
032600051114     C                   IF        m <> clnMes OR y <> clnAnn
032601051115     C                             OR NOT %FOUND(azcln01l)
032700051114     C                   EVAL      clnAnn = y
032800051114     C                   EVAL      clnMes = m
032801051115     C                   OPEN      azcln01l
032900051114     C     k04cln01      CHAIN     azcln01l
032901051115     C                   CLOSE     azcln01l
033000051114     C                   ENDIF
033100051114
033200051114     C                   IF        %FOUND(azcln01l)
033300051114     C                   EXTRCT    wrkNdcr:*D    D
033400051114     C                   IF        %SUBST(clnPom:d:1) = Festa
033500051114     C                   EVAL      wrkMsg = wrkMsg + 'La data richiesta deve -
033600051114     C                             essere un giorno lavorativo.<br>'
033700051114     C                   ENDIF
033800051114     C                   ENDIF
033900051114
034200051114     C                   ENDSR
034300050902
034400050902     ***********************************************************************************************
034500050902     **?
034600050902     **?Operazioni finali.
034700050902     **?
034800050902     ***********************************************************************************************
034900050902     C     uscita        BEGSR
035000050902
035100050902     C                   RETURN
035200050902
035300050902     C                   ENDSR
