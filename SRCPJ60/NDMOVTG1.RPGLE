000100981214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(*caller)
000200981214     H*PARMS BNDDIR(PJXBND PJCBND)
000300951124     H*PARMS CVTOPT(*DATETIME)
000400000119C1309H DECEDIT('0,') DATEDIT(*DMY/)
000500000119C1309H***** DECEDIT('0,') DATEDIT(*YMD/)
000600960710      *----------------------------------------------------------------*********
000700960710      *  Funzione di trigger legata al file NDMOV00F
000800960710      *  unica per tutti i tipi transazione
000900960710      *----------------------------------------------------------------*********
001000960709     FNdSlg00f  O    E             DISK    COMMIT(ifcommit) usropn
001100980115     FNdTgr00f  O    E             DISK    COMMIT(ifcommit) usropn
001200050317C1813X*ex C1768 *NDMOI03L  UF A E           K DISK    COMMIT(ifcommit) usropn
001300050317  "  FNDMOI03L  IF   E           K DISK
001400050317  "  F                                     rename(NDMOI000:NDMOI03)
001500050317  "  F                                     infds(RrnMoi03DS)                    *
001600050317  "  FNDMOI00F  UF A E             DISK    COMMIT(ifcommit) usropn
001700050317C1813 *                                                                usropn
001800960710     FAnOpe11L  IF   E           K DISK
001900960710      *----------------------------------------------------------------
002000960710      * Struttura fissa parametri trigger
002100951128     DPARM1            DS          5000
002200951128     D  FNAME                  1     10
002300951128     D  LNAME                 11     20
002400951128     D  MNAME                 21     30
002500951128     D  TEVEN                 31     31
002600951128     D  TTIME                 32     32
002700951128     D  CMTLCK                33     33
002800951128     D  OLDOFF                49     52B 0
002900951128     D  OLDLEN                53     56B 0
003000951128     D  ONOFF                 57     60B 0
003100951128     D  ONLEN                 61     64B 0
003200951128     D  NOFF                  65     68B 0
003300951128     D  NEWLEN                69     72B 0
003400951128     D  NNOFF                 73     76B 0
003500951128     D  NNLEN                 77     80B 0
003600960710      *
003700960709     D NdMovDs       E Ds                  ExtName(NdMov00f)
003800960709    >D X10eseDs      E DS
003900960710      *
004000960710     D oldsocieta      s                   like(movsocieta)
004100960710     D oldctb          s                   like(movctb)
004200960710     D oldunita        s                   like(movunita)
004300960710     D oldclasse       s                   like(movclasse)
004400960710     D oldcaustes      s                   like(movcaustes)
004500960710     D oldsocval       s                   like(movsocval)
004600960710     D oldtpregz       s                   like(movtpregz)
004700960710     D olddtreg        s                   like(movdtreg)
004800970528     D olddtcoco       s                   like(movdtcoco)
004900960710     D oldkcc          s                   like(movkcc)
005000960710     D oldksc          s                   like(movksc)
005100960710     D olddare         s                   like(movdare)
005200960710     D oldavere        s                   like(movavere)
005300960710     D oldimporto      s                   like(movimporto)
005400980203     D oldimportd      s                   like(movimportd)
005500050927D1873D oldcdrateo      s                   like(movcdrateo)
005600980115      *
005700980116     D xxxsocieta      s                   like(movsocieta)
005800980116     D yyysocieta      s                   like(movsocieta)
005900980115     D savcdrateo      s                   like(movcdrateo)
006000031106C1702D savConto        s                   like(conto)
006100960710      *
006200960710      * dimensione skiere causali apertura/chiusura
006300960710     D dimskcau        C                   const(30)
006400960710      * contatori
006500960710     D x               s              3  0
006600960710     D nraper          s              3  0
006700960710     D nrchiu          s              3  0
006800980115     D nrrett          s              3  0
006900960710      * skiere causali apertura/chiusura
007000960710     D skaper          s              4A   dim(dimskcau)
007100960710     D skchiu          s              4A   dim(dimskcau)
007200980115      * skiere causali apertura/chiusura
007300980115     D skrett          s              4A   dim(dimskcau)
007400960710      *
007500960710    >D DataDs          DS            10
007600960710     D AnnoDs                  1      4S 0
007700960710     D MeseDs                  6      7S 0
007800970528      *
007900970528      * Data Comp. Co.Ge.
008000970528    >D DataCoCo        DS            10
008100970528     D AnnoCoCo                1      4S 0
008200970528     D MeseCoCo                6      7S 0
008300960710      *
008400960710     D rcdok           S              1
008500980115     D FlagTest        S              1
008600960710      *
008700961108     D DatAgg          S               D
008800961108     D DatAgg6         S              6  0
008900980403     D OraAgg          S                   like(TgrOraAgg)
009000980403     D PgmAgg          S                   like(TgrPgmAgg)
009100961108      *
009200960710     D PARM2           S              9B 0
009300960710     D oldrec          S           2048
009400960710     D newrec          S           2048
009500960710     D savcmt          S              1
009600960710     D ifcommit        S              1
009700961108      *
009800961108     D DataTime        S             12  0
009900961108     D WrkData         S              6  0
010000000118C1309D ZDateISO        S               D
010100960710      *
010200951128     D                SDS
010300951128     D  NAMPGM           *PROC
010400951124     D  STSUSR               254    263
010500041008C1768 *-------------------------------------------------------*
010600041008  "  D XSoc001Ds     e ds
010700041008  "  D    XScRegInt                   1    overlay(XScSkp:7)
010800041008  "   *------------
010900041008  "   * DS Interna per dati di output di XSOC
011000041008  "  D XSocDs          ds          1000
011100041008  "   *------------
011200041008  "   * KPJBA fittizia (cioè non valorizzata ma che serve a XSOC)
011300041008  "  D KPJBA         E DS
011400041008  "   *------------
011500041008  "  D $SAVSocieta     s                   like(MOVSocieta) inz
011600041008  "   *------------
011700041008  "   * dati per chiamata nddrvreg
011800041008  "  D StrutturaO      s             10
011900041008  "  D Lunghezza       s              9B 0
012000041008  "  D Codoper         s              1
012100041008  "  D NDREGDS       e ds                  extname(NDREG00F)
012200050317C1768D $DataIso        s               D
012300050317C1813 *---------------------------------------------------*                     *
012400050317  "   * Nr rec letto da ndmoi03lper aggiornare ndmoi00f
012500050317  "  D RrnMoi03DS      DS           512                                         *
012600050317C1813D  Rrn03                397    400B 0                                      *
012700050317C1768 *****************************************************************
012800960710      *----------------------------------------------------------------
012900950201     C     *ENTRY        PLIST
013000951128     C                   PARM                    parm1
013100951128     C                   PARM                    parm2
013200960710      *
013300951128     c                   if        cmtlck <> savcmt
013400960709     c                   close     NdSlg00f
013500980115     c                   close     NdTgr00f
013600050317C1813X* ex C1768 ******* close     NDMOI03L
013700050317C1813C                   close     NDMOI00F
013800951128     c                   if        cmtlck = '0'
013900951128     c                   eval      ifcommit = '0'
014000951128     c                   else
014100951128     c                   eval      ifcommit = '1'
014200960710     c                   endif
014300960709     c                   open      NdSlg00f
014400980115     c                   open      NdTgr00f
014500050317C1813X* ex C1768 ******* open      NDMOI03L
014600050317C1813C                   open      NDMOI00F
014700951128     c                   eval      savcmt = cmtlck
014800960710     c                   endif
014900960710      *
015000951128     c                   eval       noff =  noff + 1
015100951128     c                   eval      oldoff = oldoff + 1
015200960710      *
015300951128     c                   eval      oldrec = %subst(parm1: oldoff: oldlen)
015400951128     c                   eval      newrec = %subst(parm1: noff: newlen)
015500960710      *
015600960710      * reperisco data/ora/nome programma
015700961108     C                   time                    DataTime
015800961108     C                   Movel     DataTime      OraAgg
015900961108     C                   Move      DataTime      WrkData
016000000118C1309C******dmy          Move      WrkData       DatAgg
016100000118C1309C     *JobRun       Move      WrkData       DatAgg
016200961108     C     *ymd          Move      DatAgg        DatAgg6
016300961108     C                   Eval      PgmAgg = NamPgm
016400980115      *
016500980115     C                   Clear                   SavCdRateo
016600960710      *
016700960710      * scrittura file  Insert
016800951128     C                   SELECT
016900951128     C     teven         WHENEQ    '1'
017000961108     c                   exsr      After
017100980116     c                   exsr      AfterTgr
017200960710      *
017300960710      * scrittura file  Delete
017400951128     C     teven         WHENEQ    '2'
017500961108     c                   exsr      Before
017600980116     c                   exsr      BeforeTgr
017700960710      *
017800960710      * scrittura file  Update
017900951128     C     teven         WHENEQ    '3'
018000980403      * test campi variati (solo per ndslg)
018100980115     C                   Eval      FlagTest = *on
018200960710     C                   Exsr      test
018300980115     c                   If        FlagTest = *on
018400961108     c                   exsr      Before
018500961108     c                   exsr      After
018600980115     c                   endif
018700980115      * Ratei
018800050927D1873C* anche per i ratei stesso test + il codice rateo
018900050927  "  c                   If        FlagTest = *on
019000050927D1873c                             or Movcdrateo <> OldCdrateo
019100980115     c                   exsr      BeforeTgr
019200980115     c                   exsr      AfterTgr
019300050927D1873c                   endif
019400980116     c                   endsl
019500960710      *
019600041008C1768 *   gestione NDMOI
019700041008  "  C                   exsr      GesMOI
019800041008C1768 *
019900951128     c                   return
020000960710      ****************************************************************
020100960710      *  Subroutine per immagine Before
020200960710      ****************************************************************
020300960710     c     Before        BegSr
020400960710     c                   eval      rcdok = *on
020500960709     c                   eval      NdMovDs = oldrec
020600960709     c                   Exsr      RieNdSlg
020700981006      *
020800981006      * Se esercizio bloccato non eseguo aggiornamenti
020900981006     c                   If        X10Rettifp = *on
021000981006     c                   Goto      EndBefore
021100981006     c                   Endif
021200981006      *
021300960709     c                   Mult      -1            ImpDare
021400960709     c                   Mult      -1            ImpAvere
021500980406     c                   Mult      -1            ImpDareD
021600980406     c                   Mult      -1            ImpAvereD
021700960710     c                   if        rcdok = *on
021800970603     c                   Eval      TipoData = 'R'
021900980202     c                   Eval      Divisa = *blanks
022000960710     c                   write     NdSlg00Fr
022100031106C1702c                   exsr      WriteKcc
022200031113      * scrivo anche saldo in divisa (ma non i capoconti)
022300031113D1493x***                if        movsnatura <> 'C' and
022400031113D1493x***                          movsnatura <> 'F'
022500980202     c                   eval      Divisa = MovDivisa
022600980202     c                   write     NdSlg00Fr
022700031113D1493x***                endif
022800970528      * scrivo anche record per competenza
022900090504>>>>>C**                 Eval      §Anno      = AnnoCoCo
023000090504>>>>>C**                 Eval      §nper      = MeseCoCo
023100090504>>>>>c**                 Eval      TipoData = 'C'
023200090504>>>>>c**                 Eval      Divisa = *blanks
023300090504>>>>>c**                 write     NdSlg00Fr
023400031106C1702c                   exsr      WriteKcc
023500990713     c*********          endif
023600031113      * scrivo anche saldo in divisa (ma non i capoconti)
023700031113D1493x***                if        movsnatura <> 'C' and
023800031113D1493x***                          movsnatura <> 'F'
023900090504>>>>>c*                  Eval      Divisa = MovDivisa
024000090504>>>>>c*                  write     NdSlg00Fr
024100031113D1493x***                endif
024200990713      *
024300990713     c                   endif
024400980202      *
024500981006     c     EndBefore     EndSr
024600960710      ****************************************************************
024700960710      *  Subroutine per immagine After
024800960710      ****************************************************************
024900960710     c     After         BegSr
025000960710     c                   eval      rcdok = *on
025100960709     c                   eval      NdMovDs = newrec
025200960709     c                   Exsr      RieNdSlg
025300981006      *
025400981006      * Se esercizio bloccato non eseguo aggiornamenti
025500981006     c                   If        X10Rettifp = *on
025600981006     c                   Goto      EndAfter
025700981006     c                   Endif
025800980202      *
025900970603     c                   if        rcdok = *on
026000970603     c                   Eval      TipoData = 'R'
026100980202     c                   Eval      Divisa = *blanks
026200970528     c                   write     NdSlg00Fr
026300031106C1702c                   exsr      WriteKcc
026400031113      * scrivo anche saldo in divisa (ma non i capoconti)
026500031113D1493x***                if        movsnatura <> 'C' and
026600031113D1493x***                          movsnatura <> 'F'
026700980202     c                   eval      Divisa = MovDivisa
026800980202     c                   write     NdSlg00Fr
026900031113D1493x***                endif
027000980202      *
027100970603      * scrivo anche record per competenza
027200090504>>>>>c**                 Eval      §Anno      = AnnoCoCo
027300090504>>>>>c**                 Eval      §nper      = MeseCoCo
027400090504>>>>>c**                 Eval      TipoData = 'C'
027500090504>>>>>c**                 Eval      Divisa = *blanks
027600090504>>>>>c**                 write     NdSlg00Fr
027700031106C1702c                   exsr      WriteKcc
027800031113      * scrivo anche saldo in divisa  (ma non i capoconti)
027900031113D1493x***                if        movsnatura <> 'C' and
028000031113D1493x***                          movsnatura <> 'F'
028100090504>>>>>c*                  eval      Divisa = MovDivisa
028200090504>>>>>c*                  write     NdSlg00Fr
028300031113D1493x***                endif
028400980202      *
028500960710     c                   endif
028600981006      *
028700981006     c     EndAfter      EndSr
028800031106C1702 ****************************************************************
028900031105      *  scrittura record capoconto
029000031106C1702 ****************************************************************
029100031105     c     WriteKcc      BegSr
029200031105     c*
029300031105     C                   Eval      conto      = movkcc
029400031105     c                   write     NdSlg00Fr
029500031105     C                   Eval      conto      = savconto
029600031105     c*
029700031106C1702c                   EndSr
029800960710      ****************************************************************
029900960710      *  Riempimento campi NdSlg
030000960710      ****************************************************************
030100960710     c     RieNdSlg      BegSr
030200960710      *
030300960710      * Se cambia società carico skiere causali
030400980116     c                   If        yyySocieta <> MovSocieta
030500960710     c                   exsr      Carica
030600980116     c                   eval      yyySocieta = MovSocieta
030700960710     c                   endif
030800960710      *
030900990415      * test se contabilità è diversa da MD (modelli)
031000990415     C                   If        MovCtb = 'MD'
031100990415     C                   eval      rcdok = *off
031200990415     c                   goto      FinSlg
031300990415     C                   EndIf
031400990415      *
031500960710      * test se causale chiusura
031600960710     C                   do        nrchiu        x
031700960710     c                   if        skchiu(x) = MovCausTes
031800960710     c                   eval      rcdok = *off
031900960710     c                   goto      FinSlg
032000960710     c                   endif
032100960710     c                   enddo
032200960710      *
032300960729      * test se causale apertura (solo se mov. definitivo)
032400960710     c                   eval      tiposaldo = 'N'
032500960729     c                   if        movtpregz = 'D'
032600960710     c                   do        nraper        x
032700960710     c                   if        skaper(x) = MovCausTes
032800960710     c                   eval      tiposaldo = 'B'
032900960710     c                   endif
033000960710     c                   enddo
033100960729     c                   endif
033200960710      *
033300960710      * reperisco esercizio
033400970528      * (viene fatto solo x la data registrazione in quanto la data
033500970528      *  competenza è sicuramente nello stesso esercizio)
033600970528      *
033700960710     c                   If        MovSocieta <> X10Societa    or
033800960710     c                             MovCtb     <> X10Ctb        or
033900960710     c                             MovDtReg    < X10DtInEs     or
034000960710     c                             MovDtReg    > X10DtFiEs
034100960710     c                   Exsr      RepEserciz
034200960710     c                   endif
034300960710      *
034400960710     C                   Movel     X10Eserciz    esercizio
034500960710     C                   Move      X10EseProg    esercizio
034600960709     C                   Move      MovDtReg      DataDs
034700970528     C                   Move      MovDtCoCo     DataCoCo
034800130604     C***                Eval      §Anno      = AnnoDs
034900130604     C***                Eval      §nper      = MeseDs
035000130604     C***                Eval      §tper      = '3'
035100130604     C                   Eval      SAnno      = AnnoDs
035200130604     C                   Eval      Snper      = MeseDs
035300130604     C                   Eval      Stper      = '3'
035400970528     c**********         eval      §data      = DatAgg6
035500970528     c**********         eval      §oraa      = OraAgg
035600970528     c**********         eval      §pgma      = PgmAgg
035700960709     C                   Eval      Societa    = MovSocieta
035800960709     C                   Eval      unita      = MovUnita
035900960709     C                   Eval      contabilit = MovCtb
036000960709     C                   Eval      classe     = MovClasse
036100960709     C                   Eval      capoconto  = movkcc
036200960709     C                   Eval      conto      = movkcc + movksc
036300031106C1702C                   Eval      SavConto   = Conto
036400960709     C                   Eval      Socavalere = movsocval
036500960709     C                   Eval      Tiporegz   = movtpregz
036600960709     C                   Eval      ImpDare    = movimporto * movdare
036700960709     C                   Eval      ImpAvere   = movimporto * movavere
036800980203     C                   Eval      ImpDareD   = movimportd * movdare
036900980203     C                   Eval      ImpAvereD  = movimportd * movavere
037000960710     C                   Eval      SaldoProvv  = '1'
037100960709      *
037200960710     c     FinSlg        EndSr
037300960710      ****************************************************************
037400980115      * Test campi variati
037500960710      ****************************************************************
037600960710     c     Test          BegSr
037700960710      * immagine before
037800960710     c                   eval      ndmovds    = oldrec
037900960710      *
038000960710     c                   eval      oldsocieta = movsocieta
038100960710     c                   eval      oldctb     = movctb
038200960710     c                   eval      oldunita   = movunita
038300960710     c                   eval      oldclasse  = movclasse
038400960710     c                   eval      oldcaustes = movcaustes
038500960710     c                   eval      oldsocval  = movsocval
038600960710     c                   eval      oldtpregz  = movtpregz
038700960710     c                   eval      olddtreg   = movdtreg
038800970528     c                   eval      olddtcoco  = movdtcoco
038900960710     c                   eval      oldkcc     = movkcc
039000960710     c                   eval      oldksc     = movksc
039100960710     c                   eval      olddare    = movdare
039200960710     c                   eval      oldavere   = movavere
039300960710     c                   eval      oldimporto = movimporto
039400980203     c                   eval      oldimportd = movimportd
039500050927D1873c                   eval      oldcdrateo = movcdrateo
039600960710      * immagine after
039700960710     c                   eval      ndmovds    = newrec
039800960710      *
039900960710     c                   if        oldsocieta = movsocieta      and
040000960710     c                             oldctb     = movctb          and
040100960710     c                             oldunita   = movunita        and
040200960710     c                             oldclasse  = movclasse       and
040300960710     c                             oldcaustes = movcaustes      and
040400960710     c                             oldsocval  = movsocval       and
040500960710     c                             oldtpregz  = movtpregz       and
040600960710     c                             olddtreg   = movdtreg        and
040700970528     c                             olddtcoco  = movdtcoco       and
040800960710     c                             oldkcc     = movkcc          and
040900960710     c                             oldksc     = movksc          and
041000960710     c                             olddare    = movdare         and
041100960710     c                             oldavere   = movavere        and
041200980203     c                             oldimporto = movimporto      and
041300980203     c                             oldimportd = movimportd
041400980115     c                   eval      FlagTest = *off
041500960710     c                   endif
041600960710      *
041700960710     c                   EndSr
041800960710      ************************************************************
041900960710      * reperisco esercizio
042000960710      ************************************************************
042100960710     C     RepEserciz    BegSr
042200970528      *
042300960709     C                   move      MovDtReg      x10data
042400960709     C                   move      MovSocieta    X10societa
042500960709     C                   move      MovCtb        X10ctb
042600960709     C                   call      'X10ESER'
042700960709     C                   parm                    x10eseDs
042800960709      *
042900960710     C                   EndSr
043000960710      *----------------------------------------------------
043100960710      * carica skiere causali apertura/chiusura
043200960710      * skaper = causali apertura
043300960710      * skchiu = causali chiusura
043400960710      *----------------------------------------------------
043500960710     C     Carica        BegSr
043600960710      *
043700960710     C                   clear                   nraper
043800960710     C                   clear                   nrchiu
043900960710     C                   clear                   skaper
044000960710     C                   clear                   skchiu
044100960710      * skiera causali apertura
044200960710     C                   eval      opesocieta = movsocieta
044300960710     C                   eval      opetpdocni = '8'
044400960710     C     keyope        setll     anope000
044500960710     C                   do        *hival
044600970217     C     keyope        reade     anope000                               10
044700960710     C   10              leave
044800960710     C                   add       1             nraper
044900960710     C                   move      opecausale    skaper(nraper)
045000960710     C                   enddo
045100980115      *
045200960710      * skiera causali chiusura
045300960710     C                   eval      opesocieta = movsocieta
045400960710     C                   eval      opetpdocni = '7'
045500960710     C     keyope        setll     anope000
045600960710     C                   do        *hival
045700970217     C     keyope        reade     anope000                               10
045800960710     C   10              leave
045900960710     C                   add       1             nrchiu
046000960710     C                   move      opecausale    skchiu(nrchiu)
046100960710     C                   enddo
046200960710      *
046300960710     C     KeyOpe        Klist
046400960710     C                   Kfld                    OpeSocieta
046500960710     C                   Kfld                    OpeTpDocNi
046600960710      *
046700960710     C                   EndSr
046800980116      *----------------------------------------------------
046900980116      * carica skiere causali apertura/chiusura
047000980116      * skrett = causali rettifica
047100980116      *----------------------------------------------------
047200980116     C     CaricaRet     BegSr
047300980116      *
047400980116     C                   clear                   nrrett
047500980116     C                   clear                   skrett
047600980116      *
047700980116      * skiera causali rettifica
047800980116     C                   eval      opesocieta = movsocieta
047900980116     C                   eval      opetpdocni = '3'
048000980116     C     keyope        setll     anope000
048100980116     C                   do        *hival
048200980116     C     keyope        reade     anope000                               10
048300980116     C   10              leave
048400980116     C                   add       1             nrrett
048500980116     C                   move      opecausale    skrett(nrrett)
048600980116     C                   enddo
048700980116      *
048800980116     C                   EndSr
048900980115      ****************************************************************
049000980115      *  Subroutine per immagine Before Rateo/risconti
049100980115      ****************************************************************
049200980115     C     BeforeTgr     BegSr
049300980116      *
049400980116      * Se cambia società carico skiere causali rettifica
049500980116     c                   If        xxxSocieta <> MovSocieta
049600980116     c                   exsr      CaricaRet
049700980116     c                   eval      xxxSocieta = MovSocieta
049800980116     c                   endif
049900980115      *
050000980115     c                   eval      NdMovDs = oldrec
050100980115     c                   If        MovCdRateo <> *blanks
050200050927D1873C                             and MovCdRateo <> *all'+'
050300980116      * test se causale rettifica
050400980116     c                   do        nrrett        x
050500980116     c                   if        skrett(x) = MovCausTes
050600980116     c                   goto      FinBefTgr
050700980116     c                   endif
050800980116     c                   enddo
050900980116      *
051000980115     c                   eval      SavCdRateo = MovCdRateo
051100980115     c                   Eval      TgrSys     = Movsys
051200980115     c                   Eval      TgrNrAsReg = MovNrAsReg
051300980115     c                   Eval      TgrSocieta = MovSocieta
051400980115     c                   Eval      TgrCdRateo = MovCdRateo
051500980115     c                   Move      DatAgg        TgrDtAgg
051600980115     c                   Eval      TgrOraAgg  = OraAgg
051700980115     c                   Eval      TgrPgmAgg  = PgmAgg
051800980115     c                   Eval      TgrAftBef  = 'B'
051900980115     c                   write     NdTgr000
052000980115     c                   Endif
052100980115      *
052200980116     c     FinBefTgr     EndSr
052300980115      ****************************************************************
052400980115      *  Subroutine per immagine After Ratei/risconti
052500980115      ****************************************************************
052600980115     c     AfterTgr      BegSr
052700980116      *
052800980116      * Se cambia società carico skiere causali rettifica
052900980116     c                   If        xxxSocieta <> MovSocieta
053000980116     c                   exsr      CaricaRet
053100980116     c                   eval      xxxSocieta = MovSocieta
053200980116     c                   endif
053300980115      *
053400980115      * Ratei
053500980115     c                   eval      NdMovDs = newrec
053600980115     c                   If        MovCdRateo <> *blanks
053700980115     c                             and MovCdRateo <> SavCdRateo
053800050927D1873C                             and MovCdRateo <> *all'+'
053900980116      * test se causale rettifica
054000980116     c                   do        nrrett        x
054100980116     c                   if        skrett(x) = MovCausTes
054200980116     c                   goto      FinAftTgr
054300980116     c                   endif
054400980116     c                   enddo
054500980116      *
054600980115     c                   Eval      TgrSys     = Movsys
054700980115     c                   Eval      TgrNrAsReg = MovNrAsReg
054800980115     c                   Eval      TgrSocieta = MovSocieta
054900980115     c                   Eval      TgrCdRateo = MovCdRateo
055000980115     c                   Move      DatAgg        TgrDtAgg
055100980115     c                   Eval      TgrOraAgg  = OraAgg
055200980115     C                   Eval      TgrPgmAgg  = PgmAgg
055300980115     c                   Eval      TgrAftBef  = 'A'
055400980115     c                   write     NdTgr000
055500980115     c                   endif
055600980115      *
055700980116     C     FinAftTgr     EndSr
055800041008C1768 ****************************************************************
055900041008  "   *  Gestione NDMOI
056000041008  "   ****************************************************************
056100041008  "  C     GesMOI        Begsr
056200041008  "   *
056300041008  "   * reperisco dati società movimento
056400041008  "  C                   exsr      RepSoc
056500041008  "   *
056600041008  "   * se la società gestisce le REG. INTERCOMPANY
056700050114C1768C                   if        XScRegInt = '1'
056800050114C1813 * oppure è la società di CONSOLIDAMENTO
056900050114C1813C                             or XScRegInt = '2'
057000050114C1768 *
057100041008  "   *
057200041008  "  C                   select
057300041008  "   * ---------------------------------------------------------------
057400041008  "   * Write NDMOV
057500041008  "  C                   when      TEven = '1'
057600041008  "   *   Write NDMOI
057700041008  "  C                   exsr      WrtMOI
057800041008  "   * ---------------------------------------------------------------
057900041008  "   * Delete NDMOV
058000041008  "  C                   when      TEven = '2'
058100041008  "   *   Delete NDMOI
058200041008  "  C                   exsr      DelMOI
058300041008  "   *
058400041008  "   * ---------------------------------------------------------------
058500041008  "   * Update NDMOV
058600041008  "  C                   when      TEven = '3'
058700041008  "   *   Delete NDMOI
058800041008  "  C                   exsr      DelMOI
058900041008  "   *   Write NDMOI
059000041008  "  C                   exsr      WrtMOI
059100041008  "   *
059200041008  "  C                   endsl
059300041008  "   *
059400041008  "  C                   endif
059500041008  "   *
059600041008C1768C     XGesMOI       Endsr
059700041008C1768 ***************************************************************
059800041008  "   * reperisco dati società movimento
059900041008  "   ***************************************************************
060000041008  "  C     RepSoc        Begsr
060100050105C1768 *
060200050105C1772 *
060300050105  "  C                   select
060400050127  "   * se siamo in inserimento, recupero
060500050105  "  C                   when      TEven = '1'
060600050127  "   *   dati nuovo   rec
060700050105  "  C                   eval      NDMOVDS = NewRec
060800050105  "   * se siamo in cancellazione o aggiornamento, recupero
060900050127C1772C                   when      TEven = '2' or  TEven = '3'
061000050105C1768 *   dati vecchio rec
061100050105C1768C                   eval      NDMOVDS = OldRec
061200050105C1772C                   endsl
061300050105C1768 *
061400041008  "   *   società del movimento non ancora reperita
061500041008  "  C                   if        MOVSocieta <> $SAVSocieta
061600041008  "   *
061700041008  "  C                   movel     MOVSocieta    SocXsc
061800041008  "   *
061900041008  "  C                   clear                   XSoc001ds
062000041008  "  C                   callb     'XSOC'
062100041008  "  C                   parm      'SOC002'      TipXsc            6
062200041008  "  C                   parm                    SocXsc            3
062300041008  "  C                   parm                    CdsXsc            9 0
062400041008  "  C                   parm      *blank        ModXsc            3
062500041008  "  C                   parm      *blank        RtnXsc            1
062600041008  "  C                   parm                    XSocDs
062700041008  "  C                   parm                    Kpjba
062800041008  "   *
062900041008  "  C                   if        RtnXsc <> '1'
063000041008  "  C                   movel     XSocDs        XSoc001ds
063100041008  "   *
063200041008  "   *   memorizzo che ho reperito i dati della societa
063300041008  "  C                   movel     MOVSocieta    $SAVSocieta
063400041008  "   *
063500041008  "  C                   endif
063600041008  "   *
063700041008  "  C                   endif
063800041008  "   *
063900041008C1768C     XRepSoc       Endsr
064000041008C1768 ***************************************************************
064100041008  "   * Delete NDMOI
064200041008  "   ***************************************************************
064300041008  "  C     DelMOI        Begsr
064400041008  "   *
064500041008  "   * dati vecchio rec
064600041008  "  C                   eval      NDMOVDS = OldRec
064700041008  "   *
064800041008  "   * reperisco movimento in NDMOI
064900041008  "  C                   z-add     MOVSys        MOISys
065000041008  "  C                   z-add     MOVNrAsReg    MOINrAsReg
065100041008  "  C                   z-add     MOVNrRigaM    MOINrRigaM
065200041008  "  C     K03MOI03      chain     NDMOI03L                           21
065300041008  "   *
065400050317C1768C                   if        *in21 = *off
065500050317C1813C     Rrn03         chain     NDMOI00F                           21
065600050317C1813C                   if        *in21 = *off
065700050317C1768C                   delete    NDMOI000
065800050317C1813C                   endif
065900050317C1768C                   endif
066000041008  "   *
066100041008C1768C     XDelMOI       Endsr
066200041008C1768 ***************************************************************
066300041008  "   * Write NDMOI
066400041008  "   ***************************************************************
066500041008  "  C     WrtMOI        Begsr
066600041008  "   *
066700041008  "   * dati nuovo rec
066800041008  "  C                   eval      NDMOVDS = NewRec
066900041008  "   *
067000041008  "   * scrivo NDMOI03L da Movimento con Società a valere
067100041008  "  C                   if        MOVSocVal <> *blank
067200041008  "   *
067300041008  "  C                   exsr      RieMOI
067400041008  "   *
067500041008  "  C                   write     NDMOI000
067600041008  "   *
067700041008  "  C                   endif
067800041008  "   *
067900041008C1768C     XWrtMOI       Endsr
068000041008C1768 ***************************************************************
068100041008  "   *  Riempimento NDMOI
068200041008  "   ***************************************************************
068300041008  "  C     RieMOI        Begsr
068400041008  "   *
068500041008  "  C                   clear                   NDMOI000
068600041008  "   *
068700041008  "   * dati NDMOV
068800041008  "  C                   movel     MOVSocieta    MOISocieta
068900041008  "  C                   z-add     MOVSys        MOISys
069000041008  "  C                   z-add     MOVNrAsReg    MOINrAsReg
069100041008  "  C                   z-add     MOVNrRigaM    MOINrRigaM
069200041008  "  C                   movel     MOVCtb        MOICtb
069300041008  "  C                   movel     MOVUnita      MOIUnita
069400041008  "  C                   movel     MOVClasse     MOIClasse
069500041008  "  C                   movel     MOVTpRegZ     MOITpRegZ
069600041008  "  C                   movel     MOVCausTes    MOICausTes
069700041008  "  C                   move      MOVDtReg      MOIDtReg
069800041008  "  C                   movel     MOVSerReg     MOISerReg
069900041008  "  C                   movel     MOVKcc        MOIKcc
070000041008  "  C                   movel     MOVKsc        MOIKsc
070100041008  "  C                   movel     MOVCausRig    MOICausRig
070200041008  "  C                   z-add     MOVImporto    MOIImporto
070300041008  "  C                   z-add     MOVDare       MOIDare
070400041008  "  C                   z-add     MOVAvere      MOIAvere
070500041008  "  C                   movel     MOVDivisa     MOIDivisa
070600041008  "  C                   z-add     MOVImportD    MOIImportD
070700041008  "  C                   z-add     MOVCambio     MOICambio
070800041008  "  C                   movel     MOVSNatura    MOISNatura
070900041008  "  C                   movel     MOVKccCpu     MOIKccCpu
071000041008  "  C                   movel     MOVKscCpu     MOIKscCpu
071100041008  "  C                   movel     MOVSocVal     MOISocVal
071200041008  "  C                   movel     MOVUntVal     MOIUntVal
071300041008  "  C                   movel     MOVDesBrev    MOIDesBrev
071400041008  "  C                   move      MOVDtLGio     MOIDtLGio
071500041008  "  C                   move      MOVDtCoCo     MOIDtCoCo
071600041008  "   *
071700041008  "   * dati NDREG
071800041008  "  C                   clear                   NDREGDS
071900041008  "  C                   eval      Lunghezza   = %size(NDREGDS)
072000041008  "  C                   callb     'NDDRVREG'
072100041008  "  C                   parm                    MOVSys
072200041008  "  C                   parm                    MOVNrAsReg
072300041008  "  C                   parm      'NDREG'       StrutturaO
072400041008  "  C                   parm                    Lunghezza
072500041008  "  C                   parm      *blank        CodOper
072600041008  "  C                   parm                    NDREGDS
072700041008  "   *  ndreg trovato
072800041008  "  C                   if        CodOper <> '9'
072900041008  "  C                   movel     REGDocIva     MOIDocIva
073000041008  "  C                   z-add     REGNrAsCol    MOINrAsCol
073100041008  "  C                   z-add     REGNrReg      MOINrReg
073200041008  "  C                   move      REGDtDoc      MOIDtDoc
073300041008  "  C                   z-add     REGNrDoc      MOINrDoc
073400041008  "  C                   movel     REGSerDoc     MOISerDoc
073500041008  "  C                   movel     REGCondPag    MOICondPag
073600041008  "  C                   movel     REGUteImm     MOIUteImm
073700041008  "  C                   move      REGDtImm      MOIDtImm
073800041008  "  C                   z-add     REGKnraz      MOIKnraz
073900041008  "  C                   movel     REGFunGen     MOIFunGen
074000041008  "   *  ndreg non trovato
074100041008  "  C                   else
074200041008  "  C                   move      *loval        $DataIso
074300041008  "  C                   move      $DataIso      MOIDtDoc
074400041008  "  C                   move      $DataIso      MOIDtImm
074500041008  "  C                   endif
074600041008  "   *
074700041008C1768C     XRieMOI       Endsr
074800041008C1768 ************************************************************
074900041008  "   * Definizione klist
075000041008  "   ************************************************************
075100041008  "  C     DefKlist      Begsr
075200041008  "   *
075300041008  "   * klist
075400041008  "   *
075500041008  "   * NDMOI03L
075600041008  "  C     K03MOI03      klist
075700041008  "  C                   kfld                    MOISys
075800041008  "  C                   kfld                    MOINrAsReg
075900041008  "  C                   kfld                    MOINrRigaM
076000041008  "   *
076100041008C1768C     XDefKList     Endsr
