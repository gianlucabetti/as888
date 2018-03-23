000100060623     H DFTACTGRP(*NO) BNDDIR('TIO7')
000200040729
000300040729     ***********************************************************************************************
000400040729     **?29.07.2004 Cussini
000500040729     **?A causa di un errore (?) di SQL sulla V5R2 sono stato costretto a modificare il metodo
000600040729     **?della lettura dei dati. Per motivi misteriosi, tra la prima e la seconda chiamata il cursore
000700040729     **?si chiude, quindi la FETCH eseguita nella seconda chiamata trova il cursore chiuso (errore
000800040729     **?-501).
000900040729     **?
001000040729     **?Ho aggirato il problema facendo una FETCH unica di 1000 righe (pari a 50 pagine di 20 righe)
001100040729     **?che poi ad ogni chiamata restituisco a gruppi di 20.
001200040729     ***********************************************************************************************
001300030616
001400030617     D Eseguito        C                   '0'
001500030617     D Errore          C                   '1'
001600030616     D RichiestaNuova  C                   '1'
001700030616     D RichiestaSuccessiva...
001800030616     D                 C                   '2'
001900030616     D FineDati        C                   '9'
002000030616     D Elementi        C                   20
002100030716     D WLBDA8          DS                  INZ
002200030716     D  G08DAT                 1      8  0
002300030716     D  G08INV                 9     16  0
002400030716     D  G08ERR                17     17
002500030716     D  G08TGI                18     22  0
002600030716     D                 DS
002700030716     D findata                         D   DATFMT(*ISO) INZ(D'1999-01-01')
002800030616
002900030616     D NullPtr         S               *
003000030617     D TiOri00F      E DS
003100030617     D                                     BASED(NullPtr)
003200030616     D FnOrm00F      E DS
003300030616     D                                     BASED(NullPtr)
003400030616     D DFAR          E DS
003500030616     D                                     BASED(NullPtr)
003600030617
003700030616     D TIO1G2IDS     E DS
003800030616     D                                     PREFIX(Inp)
003900030617
004000030616     D TIO1G2PDS     E DS
004100030616     D                                     PREFIX(Pag)
004200030616     D  RSR                          30
004300030616     D                                     DIM(Elementi)
004400030616     D                                     OVERLAY(PagRsr)
004500030616     D  RSC                          30
004600030616     D                                     DIM(Elementi)
004700030619     D                                     OVERLAY(PagRsc)
004800030616     D  DER                          10
004900030616     D                                     DIM(Elementi)
005000030616     D                                     OVERLAY(PagDer)
005100030616     D  DAR                          10
005200030616     D                                     DIM(Elementi)
005300030616     D                                     OVERLAY(PagDar)
005400030616     D  RFA
005500030616     D                                     LIKE(OrmRfa)
005600030616     D                                     DIM(Elementi)
005700030616     D                                     OVERLAY(PagRfa)
005800030711     D  ORM                          14S 0
005900030616     D                                     DIM(Elementi)
006000030711     D                                     OVERLAY(PagOrm)
006100030616     D  FARDES
006200030616     D                                     LIKE(D§FARDES)
006300030616     D                                     DIM(Elementi)
006400030616     D                                     OVERLAY(PagFarDes)
006500030616
006600030616     D Operazione      S              1
006700030616     D RigheOut        S              5S 0
006800030616     D RigheRic        S              5S 0
006900030616     D Esito           S              1
007000030616
007100030711     D WRKORM          S             14
007200030616     D WrkRowOrm       DS
007300040729     D                                     OCCURS(1000)
007400030616     D                                     INZ
007500030617     D  WrkRowPoe                     3  0
007600030617     D  WrkRowNsr                     2  0
007700030617     D  WrkRowNor                     7  0
007800030617     D  WrkRowNrv                     2  0
007900030617     D  WrkRowRsr                    35
008000030617     D  WrkRowRsc                    35
008100030617     D  WrkRowDer                     8  0
008200030617     D  WrkRowDar                     8  0
008300030617     D  WrkRowRfa                    15
008400030617     D  WrkRowFao                     3  0
008500030616
008600030619     D WrkGetListOrm   S           1024
008700030617     D WrkDatEur       S               D
008800030616     D                                     DATFMT(*EUR)
008900030617     D I               S              5I 0
009000040729     D O               S              5I 0
009100040729     D WrkRowNbr       S
009200040729     D                                     LIKE(SQLEr3)
009300030617     D NInpDerI        S
009400030617     D                                     LIKE(OriDer)
009500030617     D                                     INZ(00010101)
009600030617     D NInpDerF        S
009700030617     D                                     LIKE(OriDer)
009800030617     D                                     INZ(99991231)
009900030617     D NInpDarI        S
010000030617     D                                     LIKE(OrmDar)
010100030617     D                                     INZ(00010101)
010200030617     D NInpDarF        S
010300030617     D                                     LIKE(OrmDar)
010400030617     D                                     INZ(99991231)
010500030616
010600030616     D GetDescrizioneFase...
010700030616     D                 PR            35A
010800030616     D ParametroI                     3P 0
010900030616     D                                     VALUE
011000030616     D ParametroO                     1
011100030616     D ParametroO                   256
011200030616     D                                     OPTIONS(*OMIT)
011300060623     D langISO2                       2A   OPTIONS(*NOPASS)
011400091125
011500091125     C/EXEC SQL
011600091125     C+ SET OPTION DYNUSRPRF = *OWNER
011700091125     C/END-EXEC
011800060623
011900030616     ***********************************************************************
012000030616     **?
012100030616     **?Main
012200030616     **?
012300030616     ***********************************************************************
012400030616     C     *ENTRY        PLIST
012500030616     C                   PARM                    TIO1G2IDS
012600030616     C                   PARM                    Operazione
012700030616     C                   PARM                    RigheRic
012800030616     C                   PARM                    TIO1G2PDS
012900030616     C                   PARM                    RigheOut
013000030616     C                   PARM                    Esito
013100030616
013200030616     C                   EXSR      RutInz
013300030617     C                   EXSR      GetListOrm
013400030617     C                   EXSR      Uscita
013500030617
013600030617     C*--------------------------------------------------------------------------------------------*
013700030617     C* operazioni iniziali -da fare sempre-
013800030617     C*--------------------------------------------------------------------------------------------*
013900030617     C     rutinz        BEGSR
014000030617     C*
014100030617     C* imposta variabli di lavoro
014200030617     C                   EVAL      RigheOut =  *zeros
014300030617     C                   IF        RigheRic <= 0
014400030617     C                   EVAL      RigheRic = Elementi
014500030617     C                   ENDIF
014600030617     C*
014700030617     C* azzera schiere di memorizzazione
014800030617     C                   CLEAR                   TIO1G2PDS
014900040729     C                   CLEAR                   I
015000030617     C*
015100030617     C                   ENDSR
015200030617
015300030617     ***********************************************************************
015400030617     **?
015500030617     **?
015600030617     **?
015700030617     ***********************************************************************
015800030617     C     GetListOrm    BEGSR
015900030617
016000030617if  1C                   IF        Operazione = RichiestaNuova
016100030617
016200030617     C                   EXSR      NewRic
016300030617
016400030617     **?Imposto la parte fissa dell'istruzione.
016500030617     C                   EVAL      WrkGetListOrm =
016600030617     C                             'SELECT ORMPOE, ORMNSR, ORMNOR, ORMNRV-
016700030617     C                             , ORMRSR, ORMRSC, ORIDER, ORMDAR, ORMRFA-
016800030617     C                             , ORMFAO'
016900030617     C                             +
017000030617     C                             ' FROM FNORM00F JOIN TIORI00F'
017100030617     C                             +
017200030617     C                             ' ON ORIPOE = ORMPOE AND ORINSR = ORMNSR -
017300030617     C                             AND ORINOR = ORMNOR AND ORINRV = ORMNRV'
017400030617     C                             +
017500030617     C                             ' WHERE ORIIDC = ''' + InpKsc + ''''
017600030617
017700030716     **?Se il cliente NON è codificato usa in modo UNIVOCO il riferimento
017800030716     **?altrimenti, se codificato, usa tutte le selezioni
017900030716     c
018000030716     C                   IF        InpKsc = '00000000' OR
018100030716     C                             InpKsc = *blanks
018200030617     C                   EVAL      WrkGetListOrm = %TRIMR(WrkGetListOrm)
018300030617     C                             +
018400030617     C                             ' AND ORMRFA = ''' + InpRFA + ''''
018500030617     C                   ELSE
018600030716     **?Selezione sul riferimento ritiro.
018700030716     C                   IF        InpRfa <> *BLANK
018800030716     C                   EVAL      WrkGetListOrm = %TRIMR(WrkGetListOrm)
018900030716     C                             +
019000030716     C                             ' AND ORMRFA LIKE UCASE(''%'
019100030716     C                             + %TRIM(InpRfa) + '%'')'
019200030716     C                   ENDIF
019300030716     **?Selezione sulla ragione sociale mittente.
019400030617     C                   IF        InpRsr <> *BLANK
019500030617     C                   EVAL      WrkGetListOrm = %TRIMR(WrkGetListOrm)
019600030617     C                             +
019700030617     C                             ' AND ORMRSR LIKE UCASE(''%'
019800030617     C                             + %TRIM(InpRsr) + '%'')'
019900030617     C                   ENDIF
020000030617     **?Selezione sulla ragione sociale destinatario.
020100030617     C                   IF        InpRsc <> *BLANK
020200030617     C                   EVAL      WrkGetListOrm = %TRIMR(WrkGetListOrm)
020300030617     C                             +
020400030617     C                             ' AND ORMRSC LIKE UCASE(''%'
020500030617     C                             + %TRIM(InpRsc) + '%'')'
020600030617     C                   ENDIF
020700030617     **?Selezione sulla data immissione
020800030617     C                   IF        NInpDerI > 00010101 OR NInpDerF < 99991231
020900030617     C                   EVAL      WrkGetListOrm = %TRIMR(WrkGetListOrm)
021000030617     C                             +
021100030617     C                             ' AND ORIDER BETWEEN '
021200030617     C                             + %EDITC(NInpDerI:'X') + ' AND '
021300030617     C                             + %EDITC(NInpDerF:'X')
021400030617     C                   ENDIF
021500030617     **?Selezione sulla data ritiro.
021600030617     C                   IF        NInpDarI > 00010101 OR NInpDarF < 99991231
021700030617     C                   EVAL      WrkGetListOrm = %TRIMR(WrkGetListOrm)
021800030617     C                             +
021900030617     C                             ' AND ORMDAR BETWEEN '
022000030617     C                             + %EDITC(NInpDarI:'X') + ' AND '
022100030617     C                             + %EDITC(NInpDarF:'X')
022200030617     C                   ENDIF
022201170825     C
022202170825     C                   EVAL      WrkGetListOrm = %TRIMR(WrkGetListOrm) +
022203170825     C                             ' ORDER BY FNORM00F.ORMDAR DESC, +
022204170825     C                             FNORM00F.ORMRSR'
022205170825     C
022300030617     C                   ENDIF
022400040728
022500040728     C                   EVAL      WrkGetListOrm = %TRIMR(WrkGetListOrm)
022600040728     C                             + ' FOR READ ONLY'
022700030617
022800030617     C/EXEC SQL
022900030617     C+ PREPARE LISTORM FROM :WrkGetListOrm
023000030617     C/END-EXEC
023100030617
023200030617     C/EXEC SQL
023300030617     C+ DECLARE LISTORM CURSOR FOR LISTORM
023400030617     C/END-EXEC
023401170825
023402170825     C                   IF        sqlCode < *ZERO
023403170825     C                   DUMP(A)
023404170825     C                   EVAL      Operazione = FineDati
023405170825     C                   EVAL      Esito = Errore
023406170825     C                   LEAVESR
023407170825     C                   ENDIF
023500030617
023600030617     C/EXEC SQL
023700030617     C+ OPEN LISTORM
023800030617     C/END-EXEC
023900040729
024000091125     C                   IF        sqlCode < *ZERO
024100091125     C                   DUMP(A)
024200091125     C                   EVAL      Operazione = FineDati
024300091125     C                   EVAL      Esito = Errore
024400091125     C                   LEAVESR
024500091125     C                   ENDIF
024600091125
024700040729     C/EXEC SQL
024800040729     C+ FETCH NEXT FROM LISTORM FOR 1000 ROWS INTO :WrkRowOrm
024900040729     C/END-EXEC
025000040729
025100040729     C                   SELECT
025200040729     C                   WHEN      SQLCod = 100
025300040729if  1C                   EVAL      Operazione = FineDati
025400040729     C                   WHEN      SQLCod < 0
025500091125     C                   DUMP(A)
025600040729if  1C                   EVAL      Operazione = FineDati
025700040729if  1C                   EVAL      Esito = Errore
025800091125     C                   LEAVESR
025900040729     C                   OTHER
026000040729     C                   EVAL      WrkRowNbr = SQLEr3
026100040729if  1C                   EVAL      Esito = Eseguito
026200040729     C                   ENDSL
026300040729
026400040729     C/EXEC SQL
026500040729     C+ CLOSE LISTORM
026600040729     C/END-EXEC
026700030617
026800030617     C                   ENDIF
026900030617
027000030617     **?A questo punto nella DS multipla WRKROWORM ci possono essere da 0 a
027100040729     **?1000 ricorrenze. Il numero è contenuto nella variabile WrkRowNbr.
027200030617     **?Scorro la DS multipla e riempio le schiere.
027300040729     C                   DO        Elementi
027400040729     C                   EVAL      O = O + 1
027500040729     C                   IF        O > WrkRowNbr
027600040729if  1C                   EVAL      Operazione = FineDati
027700040729     C                   LEAVE
027800040729     C                   ENDIF
027900040729     C     O             OCCUR     WrkRowOrm
028000040729     C                   EVAL      I = I + 1
028100030617     C                   EVAL      Rsr(I) = WrkRowRsr
028200030619     C                   EVAL      Rsc(I) = WrkRowRsc
028300030617     C     *ISO          MOVE      WrkRowDer     WrkDatEur
028400030617     C                   MOVE      WrkDatEur     Der(I)
028500030617     C     *ISO          MOVE      WrkRowDar     WrkDatEur
028600030617     C                   MOVE      WrkDatEur     Dar(I)
028700030617     C                   EVAL      Rfa(I) = WrkRowRfa
028800030711     C                   EVAL      WrkOrm = %EDITC(WrkRowPoe:'X')
028900030711     C                                    + %EDITC(WrkRowNsr:'X')
029000030711     C                                    + %EDITC(WrkRowNor:'X')
029100030711     C                                    + %EDITC(WrkRowNrv:'X')
029200030711     C                   MOVEL     WrkORM        Orm(I)
029300030617     C                   EVAL      FarDes(I) = GetDescrizioneFase(WrkRowFao
029400060623     C                             :Esito:*OMIT:inpLang)
029500030617     C                   ENDDO
029600030617
029700040729     C                   EVAL      RigheOut = I
029800030617if  1C                   EVAL      Esito = Eseguito
029900030617
030000030617     C                   ENDSR
030100030616
030200030616     C*--------------------------------------------------------------------------------------------*
030300030616     C* operazioni per nuova richiesta
030400030616     C*--------------------------------------------------------------------------------------------*
030500030616     C     newric        BEGSR
030600030616     C*
030700040729     C                   CLEAR                   O
030800060923     C                   CLEAR                   WrkRowNbr
030900030616     C* normalizza i campi in Input
031000030616     C                   EXSR      NormInput
031100030616     C*
031200030616     C                   ENDSR
031300030617
031400030616     C*--------------------------------------------------------------------------------------------*
031500030616     C* normalizza i campi in Input
031600030616     C*--------------------------------------------------------------------------------------------*
031700030616     C     NormInput     BEGSR
031800030617
031900040625     **?Converto la data ritiro iniziale dal formato 'gg/mm/aaaa' al formato *ISO numerico.
032000040625     C                   IF        %SUBST(InpDerI:1:2) <> *ZERO OR
032100040625     C                             %SUBST(InpDerI:4:2) <> *ZERO OR
032200040625     C                             %SUBST(InpDerI:7:4) <> *ZERO
032300040309     C                   EVAL      %SUBST(InpDerI:3:1) = '.'
032400040309     C                   EVAL      %SUBST(InpDerI:6:1) = '.'
032500040625     C     *EUR          TEST(DE)                InpDerI
032600040625     C                   IF        %ERROR
032700040625     C                   EVAL      Esito = Errore
032800040625     C                   EXSR      Uscita
032900040625     C                   ENDIF
033000030617     C                   MOVE      InpDerI       WrkDatEur
033100030617     C     *ISO          MOVE      WrkDatEur     NInpDerI
033200030617     C                   ELSE
033300030617     C                   RESET                   NInpDerI
033400030617     C                   ENDIF
033500040625
033600040625     **?Se la data ritiro iniziale è troppo vecchia la cambio.
033700030716     c                   IF        NInpDeri < findata2
033800030716     c                   Z-ADD     findata2      NInpDeri
033900040309     c                   ENDIF
034000040309
034100040625     **?Converto la data ritiro finale   dal formato 'gg/mm/aaaa' al formato *ISO numerico.
034200040625     C                   IF        %SUBST(InpDerF:1:2) <> *ZERO OR
034300040625     C                             %SUBST(InpDerF:4:2) <> *ZERO OR
034400040625     C                             %SUBST(InpDerF:7:4) <> *ZERO
034500040310     C                   EVAL      %SUBST(InpDerF:3:1) = '.'
034600030704     C                   EVAL      %SUBST(InpDerF:6:1) = '.'
034700040625     C     *EUR          TEST(DE)                InpDerF
034800040625     C                   IF        %ERROR
034900040625     C                   EVAL      Esito = Errore
035000040625     C                   EXSR      Uscita
035100040625     C                   ENDIF
035200040310     C                   MOVE      InpDerF       WrkDatEur
035300040310     C     *ISO          MOVE      WrkDatEur     NInpDerF
035400040310     C                   ELSE
035500030617     C                   RESET                   NInpDerF
035600030617     C                   ENDIF
035700030617
035800040625     **?Converto la data immissione iniziale dal formato 'gg/mm/aaaa' al formato *ISO numerico.
035900040625     C                   IF        %SUBST(InpDarI:1:2) <> *ZERO OR
036000040625     C                             %SUBST(InpDarI:4:2) <> *ZERO OR
036100040625     C                             %SUBST(InpDarI:7:4) <> *ZERO
036200030704     C                   EVAL      %SUBST(InpDarI:3:1) = '.'
036300030704     C                   EVAL      %SUBST(InpDarI:6:1) = '.'
036400040625     C     *EUR          TEST(DE)                InpDarI
036500040625     C                   IF        %ERROR
036600040625     C                   EVAL      Esito = Errore
036700040625     C                   EXSR      Uscita
036800040625     C                   ENDIF
036900030617     C                   MOVE      InpDarI       WrkDatEur
037000030617     C     *ISO          MOVE      WrkDatEur     NInpDarI
037100030617     C                   ELSE
037200030617     C                   RESET                   NInpDarI
037300030617     C                   ENDIF
037400030617
037500040625     **?Converto la data immissione finale   dal formato 'gg/mm/aaaa' al formato *ISO numerico.
037600040625     C                   IF        %SUBST(InpDarF:1:2) <> *ZERO OR
037700040625     C                             %SUBST(InpDarF:4:2) <> *ZERO OR
037800040625     C                             %SUBST(InpDarF:7:4) <> *ZERO
037900030704     C                   EVAL      %SUBST(InpDarF:3:1) = '.'
038000030704     C                   EVAL      %SUBST(InpDarF:6:1) = '.'
038100040625     C     *EUR          TEST(DE)                InpDarF
038200040625     C                   IF        %ERROR
038300040625     C                   EVAL      Esito = Errore
038400040625     C                   EXSR      Uscita
038500040625     C                   ENDIF
038600030617     C                   MOVE      InpDarF       WrkDatEur
038700030617     C     *ISO          MOVE      WrkDatEur     NInpDarF
038800040309     C                   ELSE
038900040309     C                   RESET                   NInpDarF
039000030617     C                   ENDIF
039100091126
039200091126     ** Raddoppio gli apici.
039300091126     C                   IF        inpRsr <> *BLANK
039400091126     C/EXEC SQL
039500091126     C+ SET :inpRsr = REPLACE(:inpRsr , '''' , '''''')
039600091126     C/END-EXEC
039700091126     C                   ENDIF
039800091126     C                   IF        inpRsc <> *BLANK
039900091126     C/EXEC SQL
040000091126     C+ SET :inpRsc = REPLACE(:inpRsc , '''' , '''''')
040100091126     C/END-EXEC
040200091126     C                   ENDIF
040202180306     C                   IF        inpRfa <> *BLANK
040203180306     C/EXEC SQL
040204180306     C+ SET :inpRfa = REPLACE(:inpRfa , '''' , '''''')
040205180306     C/END-EXEC
040206180306     C                   ENDIF
040300091126
040400030616     C                   ENDSR
040500030617
040600030616     C*--------------------------------------------------------------------------------------------*
040700030616     C* impostazioni iniziali -la prima volta-
040800030616     C*--------------------------------------------------------------------------------------------*
040900030616     C     *inzsr        BEGSR
041000030716      * calcola la data corrente - 2mesi
041100030716     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
041200030716     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
041300030716     C                   Z-ADD     WN8           G08DAT
041400040309     C                   Z-ADD     *ZEROS        G08INV
041500030716     C                   MOVEL     '0'           G08ERR
041600040309     C                   CALL      'XSRDA8'
041700040309     C                   PARM                    WLBDA8
041800030716     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
041900030716      *
042000030716     C     *ISO          MOVE      DATCOR        findata
042100030716     C     findata       SUBDUR    2:*M          findata
042200030716     C     *ISO          MOVE      findata       findata2          8 0
042300030716     C*
042400030716     C                   ENDSR
042500030617
042600030617     ***********************************************************************
042700030617     **?
042800030617     **?Operazioni finali.
042900030617     **?
043000030617     ***********************************************************************
043100030617     C     Uscita        BEGSR
043200030617
043300030617     C                   IF        Esito = *BLANK
043400030617     C                   EVAL      Esito = Eseguito
043500030617     C                   ENDIF
043600040310
043700040310     C                   RETURN
043800040310
043900030617     C                   ENDSR
