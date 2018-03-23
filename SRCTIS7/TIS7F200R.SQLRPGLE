000100040426      /TITLE Elenco colli per T&T internet.
000200040427     ***********************************************************************************************
000300040427     **?
000400040427     **?Questo programma è chiamato tramite SQL dal server web, riceve un ID spedizione
000500040427     **?e restituisce un elenco di massimo 195 colli.
000600040427     **?
000700040427     ***********************************************************************************************
000800040426
000900040426     D Eseguito        C                   '0'
001000040426     D Errore          C                   '1'
001100040426     D Write           C                   'W'
001200040427     D Elem            C                   195
001300040426
001400040426     D TIS1F2DSI     E DS
001500040426     D TIS1F2DSO     E DS
001600040426     D  Col                          15
001700040427     D                                     DIM(Elem)
001800040426     D                                     OVERLAY(IdColloOF2)
001900040426     D  Chi                          35
002000040427     D                                     DIM(Elem)
002100040426     D                                     OVERLAY(ChiSonoOF2)
002200040426     D TIS7F1DSI     E DS
002300040426     D                                     INZ
002400040426     D  OpeIF1       E
002500040426     D                                     INZ(Write)
002600040426     D  LibIF1       E
002700040426     D                                     INZ('QTEMP')
002800040426     D  CmtCtlIF1    E
002900040426     D                                     INZ(*OFF)
003000040426     D  CommitIF1    E
003100040426     D                                     INZ(*OFF)
003200040426     D TIS7F1DSO     E DS
003300040426     D                                     INZ
003400040426     D TIVTB00F      E DS
003500040426     D                                     INZ
003600040427     D                                     OCCURS(195)
003700040426
003800040426     D PrmRigheOut     S              5S 0
003900040426     D PrmEsito        S              1
004000040426     D Esito           S              1
004100040426     D CmdExcStr       S             80
004200040426     D CmdExcLen       S             15  5
004300040426     D                                     INZ(%SIZE(CmdExcStr))
004400040426     D SysInformativo  S             10
004500040427     D                                     INZ('GAITRA201')
004600040426     D i               S              5I 0
004700040427     D File            S             10
004800040427     D FromLib         S             10
004900040427     D ToLib           S             10
005000040427     D Data            S              4
005100040430     D FlgQTempTIVTB   S              1
005200040430     D                                     INZ(*OFF)
005300091210
005400091210     D*
005500091210     D pgmstatus      SDS
005600091210     D  JOB_NAME             244    253                                         * Job name
005700091210     D  USER                 254    263                                         * User name
005800091210     D  JOB_NUM              264    269                                         * Job number
005900040426
006000040426     ***********************************************************************************************
006100040426     **?
006200040426     **?Main.
006300040426     **?
006400040426     ***********************************************************************************************
006500040426     C     *ENTRY        PLIST
006600040426     C                   PARM                    TIS1F2DSI
006700040426     C                   PARM                    TIS1F2DSO
006800040426     C                   PARM                    PrmRigheOut
006900040426     C                   PARM                    PrmEsito
007000091210
007100091210     C                   setoff                                       55
007200091210     C*
007300040426     C                   EXSR      ChkParm
007400040426
007500040430     **?Utente anonimo o iscritto.
007600040427     C                   IF        KSUIF2 = *BLANK OR KSUIF2 = *ZERO
007700040426     C                   EXSR      Anonimo
007800040430     C                   ELSE
007900040430     C                   EXSR      Iscritto
008000040426     C                   ENDIF
008100040426
008200091210     C  N55              EXSR      GetColli
008300050103
008400050103     **?Se come utente iscritto non ho trovato colli, provo come anonimo.
008500050426     C                   IF        PrmRigheOut = 0 AND
008600050103     C                             KSUIF2 <> *BLANK AND KSUIF2 <> *ZERO
008700050103     C                   EXSR      Anonimo
008800050103     C                   EXSR      GetColli
008900050103     C                   ENDIF
009000040426
009100040426     C                   EXSR      Uscita
009200040426
009300040426     ***********************************************************************************************
009400040426     **?
009500040426     **?Operazioni iniziali.
009600040426     **?
009700040426     ***********************************************************************************************
009800040426     C     *INZSR        BEGSR
009900060613     C/EXEC SQL
010000060613     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
010100060613     C/END-EXEC
010200040426     C                   ENDSR
010300040426
010400040426     ***********************************************************************************************
010500040426     **?
010600040426     **?Controllo parametri ricevuti.
010700040426     **?
010800040426     ***********************************************************************************************
010900040426     C     ChkParm       BEGSR
011000040426
011100040426     C                   CLEAR                   TIS1F2DSO
011200040426     C                   CLEAR                   PrmRigheOut
011300040426     C                   CLEAR                   PrmEsito
011400040427
011500040427     **?Parametri obbligatori.
011600040427     C                   IF        LnPIF2 <= 0 OR NSpIF2 <= 0
011700091210     C                   SETON                                        55
011800040427     C                   EVAL      PrmEsito = Errore
011900040427     C                   EXSR      Uscita
012000040427     C                   ENDIF
012100040426
012200040426     C                   ENDSR
012300040426
012400040426     ***********************************************************************************************
012500040426     **?
012600040426     **?Utente anonimo.
012700040426     **?
012800040426     ***********************************************************************************************
012900040426     C     Anonimo       BEGSR
013000040426
013100040427     **?Per l'utente anonimo creo una copia vuota di TIVTB00F in QTEMP.
013200040430     **?Se esiste già lo pulisco.
013300040430     C                   IF        FlgQTempTIVTB = *ON
013400040430     C                   EXSR      ClrTIVTB
013500040430     C                   ELSE
013600040427     C                   CALL(E)   'PCRTDUPPF'
013700040427     C                   PARM      'TIVTB00F'    File
013800040427     C                   PARM      '*LIBL'       FromLib
013900040427     C                   PARM      'QTEMP'       ToLib
014000040427     C                   PARM      '*NO'         Data
014100040427     C                   PARM      *BLANK        Esito
014200040430     C                   IF        %ERROR OR Esito = Errore
014300091210     C                   SETON                                        55
014400040430     C                   EVAL      PrmEsito = Errore
014500040430     C                   EXSR      Uscita
014600040430     C                   ENDIF
014700040430     C                   EVAL      FlgQTempTIVTB = *ON
014800040430     C                   ENDIF
014900040426
015000040427     **?Scrittura di QTEMP/TIVTB00F con i colli della spedizione.
015100040427     C                   RESET                   TIS7F1DSI
015200040427     C                   EVAL      KSUIF1 = KSUIF2
015300040427     C                   EVAL      AASIF1 = AASIF2
015400040427     C                   EVAL      LnPIF1 = LnPIF2
015500040427     C                   EVAL      NrSIF1 = NrSIF2
015600040427     C                   EVAL      NSpIF1 = NSpIF2
015700040427     C                   EVAL      TBlIF1 = TBlIF2
015800040427     C                   CLEAR                   TIS7F1DSO
015900091210     C  N55              CALL(E)   'TIS7F100R'
016000040426     C                   PARM                    TIS7F1DSI
016100040426     C                   PARM                    TIS7F1DSO
016200040426
016300040427     C                   IF        EsitoOF1 = Errore OR %ERROR
016400091210     C                   SETON                                        55
016500040426     C                   EVAL      PrmEsito = Errore
016600040426     C                   EXSR      Uscita
016700040426     C                   ENDIF
016800040426
016900040426     C                   ENDSR
017000040430
017100040430     ***********************************************************************************************
017200040430     **?
017300040430     **?Utente iscritto.
017400040430     **?
017500040430     ***********************************************************************************************
017600040430     C     Iscritto      BEGSR
017700040430
017800040430     **?Cancello TIVTB00F in QTEMP
017900040430     **?perchè per l'utente iscritto devo prendere i dati dal file residente.
018000040430     C                   EXSR      DelTIVTB
018100040430
018200040430     C                   ENDSR
018300040426
018400040426     ***********************************************************************************************
018500040426     **?
018600040426     **?Reperisco elenco colli.
018700040426     **?
018800040426     ***********************************************************************************************
018900040426     C     GetColli      BEGSR
019000050426
019100050426     C                   CLEAR                   PrmRigheOut
019200040426
019300040426     C/EXEC SQL
019400040426     C+ DECLARE TIVTB00F CURSOR FOR SELECT * FROM TIVTB00F WHERE VTBKSU =
019500050426     C+ :KSUIF2 AND VTBAAS = :AASIF2 AND VTBLNP = :LnPIF2 AND VTBNRS =
019600050426     C+ :NrSIF2 AND VTBNSP = :NSpIF2 ORDER BY VTBKSU, VTBAAS, VTBLNP,
019700050426     C+ VTBNRS, VTBNSP, VTBTBL, VTBCHISONO, VTBFLS, VTBNRS, VTBNSC,
019800050426     C+ VTBLNA FOR READ ONLY
019900040426     C/END-EXEC
020000040426
020100040426     C/EXEC SQL
020200040426     C+ OPEN TIVTB00F
020300040426     C/END-EXEC
020400040430
020500040430     C                   IF        SQLCod < 0                                   SQL error
020600040430     C                   EVAL      *INH1 = *ON
020700040430     C                   EVAL      PrmEsito = Errore
020800040430     C                   EXSR      Uscita
020900040430     C                   ENDIF
021000040426
021100040426     C/EXEC SQL
021200040427     C+ FETCH NEXT FROM TIVTB00F FOR 195 ROWS INTO :TIVTB00F
021300040426     C/END-EXEC
021400040426
021500040426     C                   SELECT
021600040426     C                   WHEN      SQLCod = 100                                 Row not found
021700040426     C                   WHEN      SQLCod < 0                                   SQL error
021800040426     C                   EVAL      *INH1 = *ON
021900040426     C                   EVAL      PrmEsito = Errore
022000040426     C                   EXSR      Uscita
022100040426     C                   OTHER
022200040426     C                   DO        SqlEr3        i
022300040426     C     i             OCCUR     TIVTB00F
022400040426     C                   EVAL      Chi(i) = VTBChiSono
022500040506     C                   EVAL      Col(i) =
022600040506     C                             %EDITC(VTBFlS:'X') + %EDITC(VTBLnA:'X') +
022700040506     C                             %EDITC(VTBNrS:'X') + %EDITC(VTBNSC:'X')
022800040426     C                   ENDDO
022900040426     C                   EVAL      PrmRigheOut = SqlEr3
023000040426     C                   ENDSL
023100040426
023200040426     C/EXEC SQL
023300040426     C+ CLOSE TIVTB00F
023400040426     C/END-EXEC
023500040426
023600040426     C                   ENDSR
023700040427
023800040427     ***********************************************************************************************
023900040427     **?
024000040427     **?Cancellazione di QTEMP/TIVTB00F.
024100040427     **?
024200040427     ***********************************************************************************************
024300040427     C     DelTIVTB      BEGSR
024400040427
024500040430     C                   IF        FlgQTempTIVTB = *ON
024600040427     C                   EVAL      CmdExcStr = 'DLTF FILE(QTEMP/TIVTB*)'
024700040430     C                   EXSR      CalQCMDEXC
024800091210     C                   IF        *in55
024900040430     C                   EVAL      PrmEsito = Errore
025000040430     C                   EXSR      Uscita
025100040430     C                   ENDIF
025200040430     C                   EVAL      FlgQTempTIVTB = *OFF
025300040430     C                   ENDIF
025400040427
025500040427     C                   ENDSR
025600040430
025700040430     ***********************************************************************************************
025800040430     **?
025900040430     **?Pulizia di QTEMP/TIVTB00F.
026000040430     **?
026100040430     ***********************************************************************************************
026200040430     C     ClrTIVTB      BEGSR
026300040430
026400040430     C                   IF        FlgQTempTIVTB = *ON
026500040430     C                   EVAL      CmdExcStr = 'CLRPFM FILE(QTEMP/TIVTB00F)'
026600040430     C                   EXSR      CalQCMDEXC
026700091210     C                   IF        *in55
026800040430     C                   EVAL      PrmEsito = Errore
026900040430     C                   EXSR      Uscita
027000040430     C                   ENDIF
027100040430     C                   ENDIF
027200040430
027300040430     C                   ENDSR
027400040430
027500040430     ***********************************************************************************************
027600040430     **?
027700040430     **?Chiamata a QCMDEXC.
027800040430     **?
027900040430     ***********************************************************************************************
028000040430     C     CalQCMDEXC    BEGSR
028100040430
028200040430     C                   CALL(E)   'QCMDEXC'
028300040430     C                   PARM                    CmdExcStr
028400040430     C                   PARM                    CmdExcLen
028500091210     C*
028600091210     C* Gestisco eventuale errore
028700091210     C                   if        %error
028800091210     C* ...stampo il dump
028900091210     C                   dump(a)
029000091210     C* ...accendo l'indicatore d procedura
029100091210     C                   seton                                        55
029200091210     C* ...mando una mail d avviso
029300091210     C                   exsr      sndeml
029400091210     C                   endif
029500040430
029600040430     C                   ENDSR
029700040426
029800040426     ***********************************************************************************************
029900040426     **?
030000040426     **?Uscita dal programma.
030100040426     **?
030200040426     ***********************************************************************************************
030300040426     C     Uscita        BEGSR
030400040427
030500040426     C                   IF        PrmEsito = *BLANK
030600040426     C                   EVAL      PrmEsito = Eseguito
030700040426     C                   ENDIF
030800040426
030900040430     C                   EVAL      *INLR = (PrmEsito = Errore)
031000040426     C                   RETURN
031100040426
031200040426     C                   ENDSR
031300091210
031400091210
031500091210     c*--------------------------------------------------------------------------------------------*
031600091210     c* /TITLE Compongo il testo e spedisco una e-m@ail
031700091210     c*--------------------------------------------------------------------------------------------*
031800091210     c     sndeml        begsr
031900091210     c*
032000091210     c* Inizializzo variabili
032100091210     c                   movel     *blanks       wrkEml          253
032200091210     c                   movel     *blanks       wrkEmlMsg      5000
032300091210     c                   movel     *blanks       wrkEmlOgg        44
032400091210     c* Valorizzo i campi della e-m@ail
032500120306     c                   eval      wrkEml='cedalert@brt.it'
032600091210     c                   eval      wrkEmlOgg='T&T dettaglio colli'
032700091210     c                   EVAL      wrkEmlMsg = 'ERRORE IMPREVISTO IN '   +
032800091210     c                             'INTERROGAZIONE DETTAGLIO COLLI '     +':/N'+
032900091210     c                             ' '                                   +':/N'+
033000091210     c                             'Verificare dump del job: '           +':/N'+
033100091210     c                             %trim(JOB_NUM)+'/'+%trim(USER)+'/'          +
033200091210     c                             %trim(JOB_NAME)                       +':/N'+
033300091210     c                             ' '                                   +':/N'
033400091210     c*
033500091210     c                   call(e)   'TIS701C'
033600091210     c                   parm                    wrkEml
033700091210     c                   parm                    wrkEmlOgg
033800091210     c                   parm                    wrkEmlMsg
033900091210     c*
034000091210     c                   endsr
