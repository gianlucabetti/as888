000100100308      /TITLE Cerca spedizioni con codice "chi sono" per utenti iscritti e non.
000200040505
000300060613     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('TIS')
000400040505
000500040407     D Eseguito        C                   '0'
000600040407     D Errore          C                   '1'
000700040407     D NuovaRichiesta  C                   '1'
000800040407     D RichiestaSuccessiva...
000900040407     D                 C                   '2'
001000040409     D DatiFiniti      C                   '9'
001100040407     D RighePagina     C                   20
001200060608
001300060613     D rtvMsgLang      PR          3512A                                        Messaggio in lingua
001400060613     D  msgId                         7A   CONST
001500060613     D  piLinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
001600060613     D  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
001700060613     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
001800060613     D                                     VARYING
001900060613     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
002000060613     D                                     VARYING
002100060613     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
002200060613     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
002300090831     D/COPY GAITRASRC/SRCPROTOPR,TIS7700R
002400090831
002500040505     D CnDizion      E DS                                                       Dizionario
002600040505     D                                     BASED(pNull)
002700040505     D AnDiz00F      E DS                                                       Dizionario Proj
002800040505     D                                     BASED(pNull)
002900040505     D                                     PREFIX(PJZ)
003000090828     D TIVTA10F      E DS
003100040505     D                                     BASED(pNull)
003200040505     D                                     PREFIX(Diz:3)
003300040407     D TIS776DSI     E DS
003400040409     D  WrkDInEur                      D
003500040409     D                                     DATFMT(*EUR)
003600040409     D                                     OVERLAY(DInI76)
003700040409     D  WrkDFiEur                      D
003800040409     D                                     DATFMT(*EUR)
003900040409     D                                     OVERLAY(DFiI76)
004000040408     D TIS173DSO     E DS
004100040408     D                                     INZ
004200090831     D tis7700i1     E DS                  QUALIFIED
004300090831     D                                     INZ(*EXTDFT)
004400090831     D tis7700o1     E DS                  QUALIFIED
004500090831     D                                     INZ(*EXTDFT)
004600090831     D RowTIVTB        DS
004700040505     D                                     INZ
004800040802     D                                     OCCURS(1000)
004900040505     D  VTBDSp
005000040505     D                                     LIKE($Da8)
005100040505     D  VTBLnP
005200040505     D                                     LIKE($LnP)
005300040505     D  VTBNrS
005400040505     D                                     LIKE($NrS)
005500040505     D  VTBNSp
005600040505     D                                     LIKE($NSp)
005700040505     D  VTBTBl
005800040505     D                                     LIKE($TBl)
005900040505     D  VTARMN
006000040505     D                                     LIKE(DizRMN)
006100040505     D  VTARMA
006200040505     D                                     LIKE(DizRMA)
006300040505     D  VTARSD
006400040505     D                                     LIKE(DizRSD)
006500040505     D  VTALNA
006600040505     D                                     LIKE(DizLNA)
006700040505     D  VTASta
006800040505     D                                     LIKE(DizSta)
006900040505     D  VTARSM
007000040505     D                                     LIKE(DizRSM)
007100040505     D  VTAPrD
007200040505     D                                     LIKE(DizPrD)
007300040407
007400040409     D TIS173          S
007500040409     D                                     LIKE(TIS173DSO)
007600040409     D                                     DIM(RighePagina)
007700040505
007800040407     D Operazione      S              1
007900040407     D RigheRic        S              5S 0
008000040408     D DatiOut         S           9999
008100040408     D RigheOut        S              5S 0
008200040407     D Esito           S              1
008300040505     D pNull           S               *
008400040505     D nDIn            S
008500040505     D                                     LIKE($Da8)
008600040505     D nDFi            S
008700040505     D                                     LIKE($Da8)
008800040505     D WrkDSpEur       S               D
008900040505     D                                     DATFMT(*EUR)
009000100309     D stm             S          32740A   VARYING
009100100309     D stmSelect       S          32740A   VARYING
009200100309     D stmFrom         S          32740A   VARYING
009300100309     D stmJoin         S          32740A   VARYING
009400100309     D stmWhere        S          32740A   VARYING
009500100309     D stmWhereDataSpedizione...
009600100309     D                 S            255A   VARYING
009700100309     D stmWhereChiSono...
009800100309     D                 S            255A   VARYING
009900100309     D stmWhereCli...
010000100309     D                 S            255A   VARYING
010100100309     D stmWhereKsu...
010200100309     D                 S            255A   VARYING
010300100309     D stmWhereSubK...
010400100309     D                 S            255A   VARYING
010500100309     D stmGroupBy      S          32740A   VARYING
010600100309     D stmOpzioni      S          32740A   VARYING
010700040802     D WrkNbrRow       S
010800040802     D                                     LIKE(SQLEr3)
010900040802     D O               S              5I 0
011000090831     D rpyEsito        S             10I 0
011100060608
011200040407     ***********************************************************************************************
011300040407     **?
011400040407     **?
011500040407     **?
011600040407     ***********************************************************************************************
011700040407     C     *ENTRY        PLIST
011800040407     C                   PARM                    TIS776DSI
011900040407     C                   PARM                    Operazione
012000040407     C                   PARM                    RigheRic
012100040408     C                   PARM                    DatiOut
012200040407     C                   PARM                    RigheOut
012300040407     C                   PARM                    Esito
012400040407
012500040409     C                   EXSR      ChkParm
012600040407     C                   EXSR      RunRicerca
012700040407     C                   EXSR      Uscita
012800040505
012900040505     ***********************************************************************************************
013000040505     **?
013100040505     **?Operazioni iniziali.
013200040505     **?
013300040505     ***********************************************************************************************
013400040505     C     *INZSR        BEGSR
013500060613     C/EXEC SQL
013600060613     C+ SET OPTION CLOSQLCSR = *ENDMOD, DYNUSRPRF = *OWNER
013700060613     C/END-EXEC
013800040505     C                   ENDSR
013900040407
014000040407     ***********************************************************************************************
014100040407     **?
014200040409     **?Controllo parametri ricevuti.
014300040407     **?
014400040407     ***********************************************************************************************
014500040409     C     ChkParm       BEGSR
014600040407
014700040409     **?I parametri sono già stati controllati da TIS7760R.
014800040407
014900040407     C                   ENDSR
015000040407
015100040407     ***********************************************************************************************
015200040407     **?
015300040407     **?Esegue ricerca.
015400040407     **?
015500040407     ***********************************************************************************************
015600040407     C     RunRicerca    BEGSR
015700040407
015800040504     C                   IF        Operazione = NuovaRichiesta
015900100309     C                   EXSR      PrepareStm
016000040505     C                   ENDIF
016100040504
016200040504     C                   EXSR      GetSpedizioni
016300040407
016400040407     C                   ENDSR
016500040504
016600040504     ***********************************************************************************************
016700040504     **?
016800100309     **?Preparazione istruzione SQL.
016900040504     **?
017000040504     ***********************************************************************************************
017100100309     C     PrepareStm    BEGSR
017200040505
017300040504     **?Imposto la parte fissa dell'istruzione.
017400100309     C                   EVAL      stmSelect = 'SELECT VTBDSP, VTBLNP, -
017500100309     C                             VTBNRS, VTBNSP, VTBTBL, -
017600100309     C                             MAX(VTARMN), MAX(VTARMA), MAX(VTARSD), -
017700100309     C                             MAX(VTALNA), MAX(VTASTA), MAX(VTARSM), -
017800100309     C                             MAX(VTAPRD)'
017900100309
018000100309     C                   EVAL      stmFrom = 'FROM TIVTB00F'
018100100309
018200100310     C                   EVAL      stmJoin = 'JOIN TIVTA10F -
018300100310     C                             ON VTBDSP = VTADSP AND VTBLNP = VTALNP -
018400100310     C                             AND VTBNRS = VTANRS AND VTBNSP = VTANSP -
018500100310     C                             AND VTBTBL = VTATBL'
018600040504
018700040504     **?Codice "chi sono".
018800100309     C                   EVAL      stmWhereChiSono = 'WHERE VTBCHISONO'
018900110105     C/EXEC SQL
019000110105     C+ SET :chiSonoI76 = REPLACE(:chiSonoI76 , '''' , '''''')
019100110105     C/END-EXEC
019200110105     C                   IF        ParzialI76 = *ON
019300100309     C                   EVAL      stmWhereChiSono += ' LIKE ''' +
019400100309     C                             %TRIMR(CHISONOI76) + '%'''
019500100309     C                   ELSE
019600100309     C                   EVAL      stmWhereChiSono += ' = ''' +
019700100309     C                             %TRIMR(CHISONOI76) + ''''
019800100309     C                   ENDIF
019900100309
020000100309     ** Data spedizione.
020100100309     C                   IF        WrkDInEur > *LOVAL OR WrkDFiEur < *HIVAL
020200100309     C     *ISO          MOVE      WrkDInEur     nDIn
020300100309     C     *ISO          MOVE      WrkDFiEur     nDFi
020400100309     C                   EVAL      stmWhereDataSpedizione = 'AND -
020500100309     C                             VTBDSP BETWEEN ' + %EDITC(nDIn:'X') + ' AND '
020600100309     C                             + %EDITC(nDFi:'X')
020700100309     C                   ENDIF
020800100304
020900100309     ** Codice cliente unificante e/o mittente.
021000100311     C                   IF        ksuI76 <> *BLANK
021100100311     C                   IF        anonimoI76 = *OFF
021200100309     C                   EVAL      stmWhereKsu = 'AND VTBKSU = '''
021300100304     C                             + ksuI76 + ''''
021400100311     C                   ELSE
021500100311     C                   EVAL      stmWhereKsu = 'AND VTACLI = '''
021600100311     C                             + ksuI76 + ''''
021700100311     C                   ENDIF
021800100311     C                   ELSEIF    ccmI76 <> *BLANK
021900100309     C                   EVAL      stmWhereCli = 'AND VTACLI = ''0'
022000100304     C                             + ccmI76 + ''''
022100100304     C                   ENDIF
022200090831
022300090831     ** Subunificante.
022400100311     C                   IF        ksuI76 <> *BLANK
022500090831     C                   RESET                   tis7700i1
022600090831     C                   RESET                   tis7700o1
022700090831     C                   EVAL      tis7700i1.ksu = ksuI76
022800090831     C                   EVAL      tis7700i1.sun = %EDITC(rqsCidI76 : 'X')
022900090831     C                   CALLP     Selettore_bolla_subUnificante('GETSUBKSUN'
023000090831     C                             : rpyEsito
023100090831     C                             : 'TIS7700I1' : tis7700i1 : %SIZE(tis7700i1)
023200090831     C                             : 'TIS7700O1' : tis7700o1 : %SIZE(tis7700o1)
023300090831     C                             )
023400090831     C                   IF        rpyEsito >= *ZERO AND
023500090831     C                             tis7700o1.subkSun <> *BLANK
023600100309     C                   EVAL      stmWhereSubK = 'AND VTASUBK = ''' +
023700090831     C                             %TRIMR(tis7700o1.subkSun) + ''''
023800090831     C                   ENDIF
023900100309     C                   ENDIF
024000040504
024100100309     C                   EVAL      stmWhere = stmWhereChiSono + ' ' +
024200100309     C                             stmWhereDataSpedizione + ' ' + stmWhereKsu +
024300100309     C                             ' ' + stmWhereCli + ' ' + stmWhereSubK
024400100309
024500040505     **?Ordinamento e raggruppamento.
024600100309     C                   EVAL      stmGroupBy = 'GROUP BY VTBDSP, -
024700100309     C                             VTBLNP, VTBNRS, VTBNSP, VTBTBL'
024800040505
024900100309     C                   EVAL      stmOpzioni = 'FOR READ ONLY'
025000040505
025100040504     **?Chiedo a SQL tante righe quante me ne hanno chieste.
025200040505     C                   IF        RigheRic > 0
025300100309     C                   EVAL      stmOpzioni += ' OPTIMIZE FOR '
025400040505     C                             + %TRIML(%EDITC(RigheRic:'Z')) + ' ROWS'
025500040504     C                   ENDIF
025600040505
025700100309     C                   EVAL      stm = stmSelect + ' ' + stmFrom + ' ' +
025800100309     C                             stmJoin + ' ' + stmWhere + ' ' + stmGroupBy +
025900100309     C                             ' ' + stmOpzioni
026000100309
026100100309     C                   EXSR      OpenCursor
026200100309
026300100311     ** Per l'utente anonimo non ho trovato la spedizione,
026400100311     ** allora ritento aggiustando il tiro.
026500100311     C                   IF        anonimoI76 = *ON AND sqlErrD(2) = *ZERO
026600100309     C                   RESET                   stmWhereCli
026700100311     C                   IF        ksuI76 <> *BLANK
026800100311     C                   EVAL      stmWhereKsu = 'AND VTBKSU = '''
026900100311     C                             + ksuI76 + ''''
027000100311     C                   ELSEIF    ccmI76 <> *BLANK
027100100311     C                   EVAL      stmWhereCli = 'AND VTBKSU = ''0'
027200100311     C                             + ccmI76 + ''''
027300100311     C                   ENDIF
027400100309     C                   EVAL      stmWhere = stmWhereChiSono + ' ' +
027500100309     C                             stmWhereDataSpedizione + ' ' + stmWhereKsu +
027600100309     C                             ' ' + stmWhereCli + ' ' + stmWhereSubK
027700100309     C                   EVAL      stm = stmSelect + ' ' + stmFrom + ' ' +
027800100309     C                             stmJoin + ' ' + stmWhere + ' ' + stmGroupBy +
027900100309     C                             ' ' + stmOpzioni
028000100309     C                   EXSR      CloseCursor
028100100309     C                   EXSR      OpenCursor
028200100309     C                   ENDIF
028300100309
028400100315     C                   IF        sqlErrD(2) = *ZERO
028500100315     C                   EVAL      Operazione = DatiFiniti
028600100315     C                   EXSR      Uscita
028700100315     C                   ENDIF
028800100315
028900100315     C                   EXSR      FetchCursor
029000040802
029100040504     C                   ENDSR
029200100309
029300100309     ***********************************************************************************************
029400100309     **?
029500100309     **?Dichiarazione e apertura cursore.
029600100309     **?
029700100309     ***********************************************************************************************
029800100309     C     OpenCursor    BEGSR
029900100309
030000100309     C/EXEC SQL
030100100309     C+ PREPARE TIVTB FROM :stm
030200100309     C/END-EXEC
030300100309
030400100309     C/EXEC SQL
030500100309     C+ DECLARE TIVTB INSENSITIVE NO SCROLL CURSOR FOR TIVTB
030600100309     C/END-EXEC
030700100309
030800100309     C/EXEC SQL
030900100309     C+ OPEN TIVTB
031000100309     C/END-EXEC
031100100309
031200100309     C                   IF        SQLCod < 0
031300100309     C                   DUMP(A)
031400100309     C                   EVAL      Esito = Errore
031500100309     C                   EVAL      Operazione = DatiFiniti
031600100309     C                   EXSR      Uscita
031700100309     C                   ENDIF
031800100309
031900100309     C                   ENDSR
032000040504
032100040504     ***********************************************************************************************
032200040504     **?
032300040504     **?Lettura cursore.
032400040504     **?
032500040504     ***********************************************************************************************
032600040504     C     FetchCursor   BEGSR
032700040802
032800040802     C                   CLEAR                   WrkNbrRow
032900040802     C                   CLEAR                   O
033000040504
033100040505     C/EXEC SQL
033200040802     C+ FETCH NEXT FROM TIVTB FOR 1000 ROWS INTO :RowTIVTB
033300040505     C/END-EXEC
033400040505
033500040505     C                   SELECT
033600040505     C                   WHEN      SQLCod = 100
033700040802     C                   EVAL      Operazione = DatiFiniti
033800040505     C                   WHEN      SQLCod < 0
033900040505     C                   EVAL      Esito = Errore
034000040802     C                   EVAL      Operazione = DatiFiniti
034100040802     C                   EXSR      Uscita
034200040802     C                   OTHER
034300040802     C                   EVAL      WrkNbrRow = SQLEr3
034400040505     C                   ENDSL
034500040505
034600040504     C                   ENDSR
034700040504
034800040504     ***********************************************************************************************
034900040504     **?
035000040504     **?Chiusura cursore.
035100040504     **?
035200040504     ***********************************************************************************************
035300040504     C     CloseCursor   BEGSR
035400040504
035500040505     C/EXEC SQL
035600040505     C+ CLOSE TIVTB
035700040505     C/END-EXEC
035800040504
035900040504     C                   ENDSR
036000040802
036100040802     ***********************************************************************************************
036200040802     **?
036300040802     **?Reperimento spedizione e impostazione TIS173.
036400040802     **?
036500040802     ***********************************************************************************************
036600040802     C     GetSpedizioni BEGSR
036700040802
036800120621     C                   IF        WrkNbrRow = *ZERO
036900120621     C                   EVAL      Operazione = DatiFiniti
037000120621     C                   LEAVESR
037100120621     C                   ENDIF
037200120621
037300100311     C                   FOR       RigheOut = 1 TO RigheRic
037400040802
037500040802     C                   EVAL      O = O + 1
037600100312
037700100312     C                   IF        O > WrkNbrRow
037800100312     C                   EVAL      Operazione = DatiFiniti
037900100312     C                   LEAVE
038000100312     C                   ENDIF
038100040802
038200040802     C     O             OCCUR     RowTIVTB
038300040802
038400040802     C                   CLEAR                   TIS173DSO
038500040802     C
038600110128     C                   EVAL      RifO73 = %CHAR(VTARMN)                       Riferimento mittente
038700040802     C
038800110128     C                   IF        VTARMA <> *BLANK AND
038900110128     C                             %TRIM(VTARMA) <> %CHAR(VTARMN)
039000110128     C                   EVAL      %SUBST(RifO73:16) = VTARMA                   Riferimento mittente
039100040802     C                   ENDIF
039200040802     C
039300040802     C     *ISO          MOVE      VTBDSp        WrkDSpEur
039400040802     C                   EVAL      DatO73 = %CHAR(WrkDSpEur)                    Data spedizione
039500040802     C                   EVAL      DesO73 = VTALNA                              Destinazione
039600040802     C                   EVAL      DstO73 = VTARSD                              Destinatario
039700040802     C                   EVAL      SpeO73 = %EDITC(VTBLnP:'X') +                ID spedizione
039800040802     C                             %EDITC(VTBNrS:'X') + %EDITC(VTBNSp:'X')
039900040802     C
040000040802     C                   IF        VTASta = '1'
040100060608     C*                  EVAL      StaO73 = 'Consegnata'
040200060613     C                   EVAL      StaO73 = rtvMsgLang('TIS0091':langI76)
040300060608     C                   ELSE
040400060608     C*                  EVAL      StaO73 = 'Non consegnata'
040500060613     C                   EVAL      StaO73 = rtvMsgLang('TIS0103':langI76)
040600060608     C                   ENDIF
040700060608     C
040800040802     C                   EVAL      MitO73 = VTARSM                              Mittente
040900040802     C                   EVAL      TIS173(RigheOut) = TIS173DSO
041000100311
041100100312     C                   IF        O = WrkNbrRow
041200100311     C                   EVAL      Operazione = DatiFiniti
041300100311     C                   LEAVE
041400100311     C                   ENDIF
041500040802
041600100311     C                   ENDFOR
041700040802
041800040802     C                   ENDSR
041900060608
042000040407     ***********************************************************************************************
042100040407     **?
042200040407     **?Uscita dal programma.
042300040407     **?
042400040407     ***********************************************************************************************
042500040407     C     Uscita        BEGSR
042600040407
042700100315     C                   EXSR      CloseCursor
042800100315
042900040407     C                   IF        Esito = *BLANK
043000040407     C                   EVAL      Esito = Eseguito
043100040407     C                   ENDIF
043200040505
043300040505     C                   IF        Esito = Eseguito
043400040505     C                   MOVEA     TIS173        DatiOut
043500040505     C                   ENDIF
043600040407
043700040505     C                   IF        Operazione = DatiFiniti OR Esito = Errore
043800040409     C                   EVAL      *INLR = *ON
043900040409     C                   ENDIF
044000040409
044100040407     C                   RETURN
044200040407
044300040407     C                   ENDSR
