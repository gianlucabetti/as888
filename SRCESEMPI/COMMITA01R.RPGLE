000100050623     H DFTACTGRP(*NO) ACTGRP('MIOACTGRP')
000200050624     ***********************************************************************************************
000300050624     **?
000400050624     **?Questo programma è un esempio di utilizzo del controllo di sincronia.
000500050624     **?L'esempio è incentrato su 3 file:
000600050624     **? COMMITA00F padre
000700050624     **? COMMITB00F figli
000800050624     **? COMMITC00F figli
000900050624     **?
001000050624     **?L'applicazione scrive/aggiorna i record figli con un importo, ne calcola il totale
001100050624     **?e con l'importo ottenuto aggiorna il record padre con la seguente regola:
001200050624     **? COMMITA00F.IMPORTOB totale importi di COMMITB00F
001300050624     **? COMMITA00F.IMPORTOC totale importi di COMMITC00F
001400050624     **?
001500050624     **?Alla fine l'utente deve confermare gli aggiornamenti premendo F6
001600050624     **?oppure rimuoverli premendo F5.
001700050624     **?
001800050624     **?Per eseguire l'applicazione CALL PGM(COMMITA01C)
001900050624     **?
002000050624     ***********************************************************************************************
002100050624     Fcommita01lIF   E           K DISK                                         |
002200050623     Fcommita01dCF   E             WORKSTN
002300050623     F                                     INFDS(infdspf)
002400050623     F                                     INDDS(inddspf)
002500050623
002600050623     D F3              C                   X'33'
002700050623     D F5              C                   X'35'
002800050623     D F6              C                   X'36'
002900050623     D F12             C                   X'3C'
003000050623     D Enter           C                   X'F1'
003100050624     D RecordAlreadyLocked...
003200050624     D                 C                   01218
003300050623
003400050623     D infdspf         DS
003500050623     D  dsp_aid              369    369                                         AID byte
003600050623     D inddspf         DS
003700050623     D  erroregenerico...
003800050623     D                        99     99N
003900050622
004000050622     D prmcommitfile   S              1
004100050622     D                                     INZ(*ON)
004200050622     D prmdocommit     S              1
004300050622     D                                     INZ(*OFF)
004400050622     D prmesito        S             10I 0
004500050623     D prmtasto        S              1
004600050622     D wrkesito        S             10I 0
004700050622     D cmdexc1         S            256
004800050622     D cmdexc2         S             15  5
004900050622     D                                     INZ(%SIZE(cmdexc1))
005000050623      /COPY QSYSINC/QRPGLESRC,QUSEC
005100050623     D LngTx01         S            512                                         Text string
005200050623     D LngTx02         S             10I 0                                      Length text string
005300050623     D                                     INZ(%SIZE(LngTx01))
005400050623     D LngTx03         S              7                                         Message ID
005500050623     D                                     INZ('BAR0019')
005600050623     D LngTx04         S             20                                         Qualified MSGF
005700050623     D                                     INZ('YBARMSG   *LIBL')
005800050622
005900050622     ***********************************************************************************************
006000050622     **?
006100050624     **?Main.
006200050622     **?
006300050622     ***********************************************************************************************
006400050623
006500050623     C                   DO        *HIVAL
006600050623
006700050623     C                   EXFMT     w01
006800050623
006900050623     C                   SELECT
007000050623     C                   WHEN      dsp_aid = F3
007100050623     C                   ROLBK
007200050623     C                   LEAVE
007300050623     C                   WHEN      dsp_aid = F5
007400050623     C                   ROLBK
007500050623     C                   EXSR      inzw01
007600050623     C                   WHEN      dsp_aid = F6
007700050623     C                   COMMIT
007800050623     C                   LEAVE
007900050623     C                   WHEN      dsp_aid = Enter
008000050623     C                   EXSR      runw01
008100050624     **?In uno dei programmi di manutenzione l'utente ha premuto F3,
008200050624     **?quindi rimuovo le modifiche ed esco.
008300050623     C                   IF        prmtasto = F3
008400050623     C                   ROLBK
008500050623     C                   LEAVE
008600050623     C                   ENDIF
008700050623     C                   EXSR      inzw01
008800050623     C                   ENDSL
008900050623
009000050623     C                   ENDDO
009100050623
009200050623     C                   EXSR      uscita
009300050623
009400050623     ***********************************************************************************************
009500050623     **?
009600050624     **?Inizializzo la finestra con i dati di COMMITA00F.
009700050623     **?
009800050623     ***********************************************************************************************
009900050623     C     inzw01        BEGSR
010000050623
010100050624     C                   CLEAR                   w01opzb
010200050624     C                   CLEAR                   w01opzc
010300050623     C                   CLEAR                   importob
010400050623     C                   CLEAR                   importoc
010500050623     C                   EXSR      getcommita
010600050623
010700050623     C                   ENDSR
010800050623
010900050623     ***********************************************************************************************
011000050623     **?
011100050624     **?Reperisco COMMITA00F in cui ci sono i totali B e C.
011200050623     **?
011300050623     ***********************************************************************************************
011400050623     C     getcommita    BEGSR
011500050623
011600050623     C     ida           CHAIN     commita01l
011700050623
011800050623     C                   ENDSR
011900050623
012000050623     ***********************************************************************************************
012100050623     **?
012200050624     **?L'utente ha premuto invio.
012300050623     **?
012400050623     ***********************************************************************************************
012500050623     C     runw01        BEGSR
012600050623
012700050623     C                   CLEAR                   wrkesito
012800050622
012900050623     **?Gestione dell'importo B.
013000050623     C                   IF        w01opzb <> *BLANK
013100050623     C                   EXSR      runopzb
013200050624     C                   SELECT
013300050624     C                   WHEN      wrkesito < 0
013400050624     C                   IF        wrkesito = RecordAlreadyLocked * -1
013500050624     C                   EVAL      LngTx01 = 'Mi spiace, ma i dati sono -
013600050624     C                             allocati da un altro lavoro.'
013700050624     C                   ELSE
013800050623     C                   EVAL      LngTx01 = 'La manutenzione dell''importo B -
013900050624     C                             è terminata in modo anomalo.'
014000050624     C                   ENDIF
014100050623     C                   EXSR      CalQUILNGTX
014200050623     C                   LEAVESR
014300050624     C                   WHEN      prmtasto = F3
014400050623     C                   LEAVESR
014500050624     C                   WHEN      prmtasto = F12
014600050624     C                   LEAVESR
014700050624     C                   ENDSL
014800050623     C                   ENDIF
014900050623
015000050623     **?Gestione dell'importo C.
015100050623     C                   IF        w01opzc <> *BLANK
015200050623     C                   EXSR      runopzc
015300050624     C                   SELECT
015400050624     C                   WHEN      wrkesito < 0
015500050624     C                   IF        wrkesito = RecordAlreadyLocked * -1
015600050624     C                   EVAL      LngTx01 = 'Mi spiace, ma i dati sono -
015700050624     C                             allocati da un altro lavoro.'
015800050624     C                   ELSE
015900050624     C                   EVAL      LngTx01 = 'La manutenzione dell''importo C -
016000050624     C                             è terminata in modo anomalo.'
016100050624     C                   ENDIF
016200050623     C                   EXSR      CalQUILNGTX
016300050623     C                   LEAVESR
016400050624     C                   WHEN      prmtasto = F3
016500050623     C                   LEAVESR
016600050624     C                   WHEN      prmtasto = F12
016700050624     C                   LEAVESR
016800050624     C                   ENDSL
016900050623     C                   ENDIF
017000050622
017100050623     C                   ENDSR
017200050623
017300050623     ***********************************************************************************************
017400050623     **?
017500050623     **?Chiamo il programma di gestione dell'importo B
017600050623     **?dicendogli di aprire i file sotto controllo sincronia e di non dare COMMIT.
017700050623     **?
017800050623     ***********************************************************************************************
017900050623     C     runopzb       BEGSR
018000050623
018100050623     C                   CALL(E)   'COMMITB01R'
018200050623     C                   PARM                    ida
018300050623     C                   PARM                    prmcommitfile
018400050623     C                   PARM                    prmdocommit
018500050623     C                   PARM      0             prmesito
018600050623     C                   PARM      *BLANK        prmtasto
018700050623     C
018800050623     C                   SELECT
018900050623     C                   WHEN      %ERROR
019000050623     C                   EVAL      wrkesito = %STATUS * -1
019100050623     C                   WHEN      prmesito < 0
019200050623     C                   EVAL      wrkesito = prmesito
019300050623     C                   ENDSL
019400050623
019500050623     C                   ENDSR
019600050623
019700050623     ***********************************************************************************************
019800050623     **?
019900050623     **?Chiamo il programma di gestione dell'importo C
020000050623     **?dicendogli di aprire i file sotto controllo sincronia e di non dare COMMIT.
020100050623     **?
020200050623     ***********************************************************************************************
020300050623     C     runopzc       BEGSR
020400050623
020500050623     C                   CALL(E)   'COMMITC01R'
020600050623     C                   PARM                    ida
020700050623     C                   PARM                    prmcommitfile
020800050623     C                   PARM                    prmdocommit
020900050623     C                   PARM      0             prmesito
021000050623     C                   PARM      *BLANK        prmtasto
021100050623
021200050623     C                   SELECT
021300050623     C                   WHEN      %ERROR
021400050623     C                   EVAL      wrkesito = %STATUS * -1
021500050623     C                   WHEN      prmesito < 0
021600050623     C                   EVAL      wrkesito = prmesito
021700050623     C                   ENDSL
021800050623
021900050623     C                   ENDSR
022000050623
022100050623     ***********************************************************************************************
022200050623     **?
022300050623     **?Chiamata a QUILNGTX.
022400050623     **?
022500050623     ***********************************************************************************************
022600050623     C     CalQUILNGTX   BEGSR
022700050623
022800050623     C                   CLEAR                   QUSEC
022900050623     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
023000050623
023100050623     C                   CALL      'QUILNGTX'
023200050623     C                   PARM                    LngTx01
023300050623     C                   PARM                    LngTx02
023400050623     C                   PARM                    LngTx03
023500050623     C                   PARM                    LngTx04
023600050623     C                   PARM                    QUSEC
023700050623
023800050623     C                   ENDSR
023900050623
024000050623     ***********************************************************************************************
024100050623     **?
024200050624     **?Operazioni iniziali.
024300050623     **?
024400050623     ***********************************************************************************************
024500050623     C     *INZSR        BEGSR
024600050623
024700050623     **?Attivo il controllo di sincronia nel mio gruppo attivazione.
024800050624     C                   EVAL      cmdexc1 = 'STRCMTCTL LCKLVL(*ALL)'
024900050623     C                   CALL(E)   'QCMDEXC'
025000050623     C                   PARM                    cmdexc1
025100050623     C                   PARM                    cmdexc2
025200050623
025300050623     C                   ENDSR
025400050622
025500050622     ***********************************************************************************************
025600050622     **?
025700050624     **?Operazioni finali.
025800050622     **?
025900050622     ***********************************************************************************************
026000050622     C     uscita        BEGSR
026100050623
026200050623     C                   EVAL      cmdexc1 = 'ENDCMTCTL'
026300050623     C                   CALL(E)   'QCMDEXC'
026400050623     C                   PARM                    cmdexc1
026500050623     C                   PARM                    cmdexc2
026600050622
026700050622     C                   EVAL      *INLR = *ON
026800050622     C                   RETURN
026900050622
027000050622     C                   ENDSR
