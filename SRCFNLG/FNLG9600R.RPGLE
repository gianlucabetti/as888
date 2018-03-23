000100051012     H DFTACTGRP(*NO)
000200050930
000300050930     ***************************************************************************
000400050930     **?
000500050930     **?Definizione costanti.
000600050930     **?
000700050930     ***************************************************************************
000800050930     D F3              C                   X'33'
000900050930     D F5              C                   X'35'
001000050930     D F12             C                   X'3C'
001100050930     D Enter           C                   X'F1'
001200050930     D RollUp          C                   X'F5'
001300050930     D Eseguito        C                   '0'
001400050930     D Errore          C                   '1'
001500050930     D NuovaRichiesta  C                   '1'
001600050930     D Continua        C                   '2'
001700051012     D FineRichiesta   C                   '9'
001800050930     D Put             C                   'P'
001900050930     D Get             C                   'G'
002000050930     D Display         C                   'D'
002100050930     D Filtro          C                   1
002200050930     D GetDati         C                   3
002300050930     D PutLista        C                   5
002400050930     D GetOpzione      C                   7
002500050930     D Gestione        C                   9
002600050930     D Fine            C                   11
002700050930
002800050930     ***************************************************************************
002900050930     **?
003000050930     **?Definizione procedure.
003100050930     **?
003200050930     ***************************************************************************
003300050930     D fnlg9610R       PR
003400050930     D                                     EXTPGM('FNLG9610R')
003500050930     D  a
003600050930     D                                     LIKE(kpjba)
003700050930     D fnlg9620R       PR
003800050930     D                                     EXTPGM('FNLG9620R')
003900050930     D  a
004000050930     D                                     LIKE(kpjba)
004100050930     D  a
004200050930     D                                     LIKE(fnlg9630s)
004300051010     D  a
004400051010     D                                     LIKE(fnlg9610s)
004500050930     D fnlg9630R       PR
004600050930     D                                     EXTPGM('FNLG9630R')
004700050930     D  a
004800050930     D                                     LIKE(kpjba)
004900050930     D  a
005000050930     D                                     LIKE(fnlg9630s)
005100050930
005200050930     ***************************************************************************
005300050930     **?
005400050930     **?Definizione strutture dati.
005500050930     **?
005600050930     ***************************************************************************
005700050930     D fnvar10s      E DS
005800050930     D                                     BASED(NullPtr)
005900050930     D kpjba         E DS
006000050930     D fnlg9610s     E DS
006100050930     D                                     INZ
006200050930     D                                     PREFIX(Flt)
006300050930     D  po           E
006400050930     D  presaVisio   E
006500050930     D                                     INZ('N')
006600050930     D  esitoRicez   E
006700050930     D                                     INZ('S')
006800050930     D  drda         E
006900050930     D                                     INZ(*HIVAL)
007000050930     D fnlg9620s     E DS
007100050930     D                                     PREFIX(lst)
007200050930     D  putGet       E
007300050930     D                                     INZ('P')
007400050930     D  operazione   E
007500050930     D                                     INZ('1')
007600050930     D fnlg9630s     E DS
007700050930     D                                     PREFIX(Dta)
007800050930     D  operazione   E
007900050930     D                                     INZ('1')
008000050930     D  righeRic     E
008100051012     D                                     INZ(18)
008200050930     D  dati                         30
008300050930     D                                     DIM(99)
008400050930     D                                     OVERLAY(dtaDati)
008500050930     D row             DS
008600050930     D                                     INZ
008700050930     D  rowAas
008800050930     D                                     LIKE(varAas)
008900050930     D  rowLnp
009000050930     D                                     LIKE(varLnp)
009100050930     D  rowNrs
009200050930     D                                     LIKE(varNrs)
009300050930     D  rowNsp
009400050930     D                                     LIKE(varNsp)
009500050930     D  rowAgc
009600050930     D                                     LIKE(varAgc)
009700050930     D  rowFgc
009800050930     D                                     LIKE(varFgc)
009900050930     D  rowNgc
010000050930     D                                     LIKE(varNgc)
010100050930
010200050930     ***************************************************************************
010300050930     **?
010400050930     **?Definizione variabili.
010500050930     **?
010600050930     ***************************************************************************
010700050930     D NullPtr         S               *
010800050930     D WrkGetLista     S           4096
010900050930     D                                     VARYING
011000050930     D i               S             10I 0
011100050930     D Esito           S              1
011200050930     D wrkDrdd         S
011300050930     D                                     LIKE(varDrd)
011400050930     D wrkDrda         S
011500050930     D                                     LIKE(varDrd)
011600050930     **?In FASECURR c'è la fase corrente in esecuzione.
011700050930     **?In FASENEXT c'è la prossima fase da eseguire.
011800050930     **?In FASEPREV c'è la fase a cui tornare se premuto F12.
011900050930     D FaseCurr        S              5I 0
012000050930     D FaseNext        S
012100050930     D                                     LIKE(FaseCurr)
012200050930     D                                     INZ(Filtro)
012300050930     D FasePrev        S
012400050930     D                                     LIKE(FaseCurr)
012500050930     D                                     INZ(Fine)
012600050930
012700050930     ***************************************************************************
012800050930     **?
012900050930     **?Definizione parametri procedura.
013000050930     **?
013100050930     ***************************************************************************
013200050930     C     *ENTRY        PLIST
013300050930     C                   PARM                    kpjba
013400050930
013500050930     ***************************************************************************
013600050930     **?
013700050930     **?Main
013800050930     **?
013900050930     ***************************************************************************
014000050930     C                   DO        *HIVAL
014100050930
014200050930     C                   SELECT
014300050930     **?Emissione filtro.
014400050930     C                   WHEN      FaseNext = Filtro
014500050930     C                   EXSR      RunFiltro
014600050930     **?Reperimento dati per lista.
014700050930     C                   WHEN      FaseNext = GetDati
014800050930     C                   EXSR      RunGetDati
014900051012     C                   EVAL      FaseNext = PutLista
015000050930     **?Emissione lista.
015100050930     C                   WHEN      FaseNext = PutLista
015200050930     C                   EXSR      RunPutLista
015300050930     **?Elaborazione opzioni su lista.
015400050930     C                   WHEN      FaseNext = GetOpzione
015500050930     C                   EXSR      RunGetOpzione
015600050930     **?Se non so cosa fare esco.
015700050930     C                   OTHER
015800050930     C                   LEAVE
015900050930     C                   ENDSL
016000050930     C
016100050930     C                   ENDDO
016200050930
016300050930     C                   EXSR      Uscita
016400050930
016500050930     ***************************************************************************
016600050930     **?
016700050930     **?Operazioni iniziali.
016800050930     **?
016900050930     ***************************************************************************
017000050930     C     *INZSR        BEGSR
017100050930
017200050930     C                   ENDSR
017300050930
017400050930     ***************************************************************************
017500050930     **?
017600050930     **?Inizializzazione procedura.
017700050930     **?
017800050930     ***************************************************************************
017900050930     C     inzProc       BEGSR
018000050930
018100050930     C                   ENDSR
018200050930
018300050930     ***************************************************************************
018400050930     **?
018500050930     **?Richiesta filtro.
018600050930     **?
018700050930     ***************************************************************************
018800050930     C     RunFiltro     BEGSR
018900050930
019000050930     C                   EVAL      FaseCurr = FaseNext
019100050930
019200050930     C                   EVAL      kpjbu = fnlg9610s
019300050930     C                   CALLP     fnlg9610r(kpjba)
019400050930     C                   EVAL      fnlg9610s = kpjbu
019500050930
019600050930     C                   SELECT
019700050930     C*
019800050930     C                   WHEN      FltTasto = Enter
019900050930     C                   EVAL      FaseNext = GetDati
020000050930     C                   RESET                   fnlg9620s
020100050930     C                   RESET                   fnlg9630s
020200050930     C*
020300050930     C                   WHEN      FltTasto = F3
020400050930     C                   EVAL      FaseNext = Fine
020500050930     C*
020600050930     C                   WHEN      FltTasto = F12
020700050930     C                   EVAL      FaseNext = Fine
020800050930     C*
020900050930     C                   ENDSL
021000050930
021100050930     C                   EVAL      FasePrev = FaseCurr
021200050930
021300050930     C                   ENDSR
021400050930
021500050930     ***************************************************************************
021600050930     **?
021700050930     **?Reperisco i dati per la lista.
021800050930     **?
021900050930     ***************************************************************************
022000050930     C     RunGetDati    BEGSR
022100050930
022200050930     C                   EVAL      FaseCurr = FaseNext
022300050930
022400050930     C                   EVAL      kpjbu = fnlg9610s
022500050930     C                   CALLP     fnlg9630r(kpjba:fnlg9630s)
022600050930     C                   EVAL      fnlg9610s = kpjbu
022700050930
022800050930     C                   EVAL      LstPutGet = Put
022900050930
023000050930     C                   ENDSR
023100050930
023200050930     ***************************************************************************
023300050930     **?
023400050930     **?Emetto la lista delle note credito selezionate.
023500050930     **?
023600050930     ***************************************************************************
023700050930     C     RunPutLista   BEGSR
023800050930
023900050930     C                   EVAL      FaseCurr = FaseNext
024000050930
024100050930     C                   EVAL      LstOperazione = DtaOperazione
024200050930     C                   EVAL      LstEOF = DtaEOF
024300050930
024400050930     C                   EVAL      kpjbu = fnlg9620s
024500051010     C                   CALLP     fnlg9620r(kpjba:fnlg9630s:
024600051010     C                             fnlg9610s)
024700050930     C                   EVAL      fnlg9620s = kpjbu
024800050930
024900050930     C                   SELECT
025000050930     C*
025100050930     C                   WHEN      LstTasto = Enter
025200050930     C                   EVAL      FasePrev = FaseCurr
025300050930     C                   EVAL      FaseNext = GetOpzione
025400050930     C*
025500050930     C                   WHEN      LstTasto = F3
025600050930     C                   EVAL      FaseNext = Fine
025700050930     C*
025800050930     C                   WHEN      LstTasto = F5
025900050930     C                   EVAL      FaseNext = GetDati
026000050930     C                   RESET                   fnlg9630s
026100050930     C*
026200050930     C                   WHEN      LstTasto = F12
026300050930     C                   EVAL      FaseNext = Filtro
026400050930     C* E' stato premuto ROLLUP quindi predispongo il caricamento
026500050930     C* di un'altra pagina.
026600050930     C                   WHEN      LstTasto = RollUp
026700050930     C                   EVAL      FaseNext = GetDati
026800050930     C                   EVAL      DtaOperazione = Continua
026900050930     C*
027000050930     C                   ENDSL
027100050930
027200050930     C                   ENDSR
027300050930
027400050930     ***************************************************************************
027500050930     **?
027600050930     **?Reperisco le scelte fatte sulla lista.
027700050930     **?
027800050930     ***************************************************************************
027900050930     C     RunGetOpzione BEGSR
028000050930
028100050930     C                   EVAL      FaseCurr = FaseNext
028200050930
028300050930     C                   EVAL      LstPutGet = Get
028400050930     C                   EVAL      kpjbu = fnlg9620s
028500051010     C                   CALLP     fnlg9620r(kpjba:fnlg9630s:
028600051010     C                             fnlg9610s)
028700050930     C                   EVAL      fnlg9620s = kpjbu
028800050930
028900050930     **?Opzione immessa sulla riga del subfile.
029000050930     C                   SELECT
029100050930     C
029200050930     C                   OTHER
029300050930     C
029400050930     C                   IF        LstEOF = *ON
029500050930     C                   EVAL      FaseNext = FasePrev
029600050930     C                   EVAL      LstPutGet = Display
029700050930     C                   ENDIF
029800050930     C
029900050930     C                   ENDSL
030000050930
030100050930     C                   ENDSR
030200051012
030300051012     ***************************************************************************
030400051012     **?
030500051012     **?Uscita
030600051012     **?
030700051012     ***************************************************************************
030800051012     C     Uscita        BEGSR
030900051012
031000051012     C                   EVAL      *INLR = *ON
031100051012     C                   RETURN
031200051012
031300051012     C                   ENDSR
