000100990507     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200020412     H*PARMS ACTGRP(YFICN51)
000300020415     H/TITLE  dettaglio autotrasportatori x fornitore
000400951009?     *--------------------------------------------------------------*
000500020415      * ficn52R                                                      *
000600951009      *     *----------------------------------------------*         *
000700020404      *         CONTABILIZZAZIONE conteggi                           *
000800951009      *     *---------------------------------------------*          *
000900951009      *                                                              *
001000951009?     *--------------------------------------------------------------*
001100951009      * KC - FINE LAVORO                                             *
001200951009      * KL - RITORNO                                                 *
001300951009?     *--------------------------------------------------------------*
001400951009      * 20 - SFLDSP                                                  *
001500951009      * 21 - SFLDSPCTL/CLEAR                                         *
001600951009      * 22 - SFLNXTCHG                                               *
001700951009      * 28 - Visualizzazione messaggio di errore                     *
001800951011      * 29 - READC su subfile                                        *
001900951011      * 32 - Indicatore di comodo per READ/CHAIN/SCAN/LOKUP          *
002000951009?     *--------------------------------------------------------------*
002100021203     Ffiapd01L  IF   E           K DISK
002200020415     F*--------
002300020415     Fficn52D   CF   E             WORKSTN
002400980911     F                                     SFILE(y350S01:NRR)
002500951009     D*---------------------------------------------------------------*
002600951009     D* SCHIERA
002700951009     D*---------------------------------------------------------------*
002800020411     D* Reperimento nome PGM
002900020411     D STATUS         SDS           333
003000020411     D  nompgm           *PROC
003100020411     D*---------------------------------------------------------------*
003200020411     D* CAMPI INTERNI
003300020411     D*---------------------------------------------------------------*
003400020411     D dataiso         s               d   datfmt(*iso)
003500020411     D dataeur         s               d   datfmt(*eur)
003600951009?     *--------------------------------------------------------------*
003700951009?     *  DS                                                          *
003800951009?     *--------------------------------------------------------------*
003900951009     D KPJBA         E DS
004000020411     d* parametri ricevuti
004100020404     D ficn50ds      E DS
004200020411     c* SQL
004300020405     D WrkSqlCmd       S           1024
004400020415     D fiftt           DS
004500020415     d pdrs                           7  0
004600020415     d ddcs                           8  0
004700020415     d tots                          12  3
004800030827     D fiatt           DS
004900030827     d pdrsa                          7  0
005000030827     d ddcsa                          8  0
005100030827     d vadsa                          1    inz
005200030827     d totsa                         12  3
005300951009?     *--------------------------------------------------------------*
005400951009?     *  CICLO PRINCIPALE                                            *
005500951009?     *--------------------------------------------------------------*
005600951009      *  Impostazione parametri prima videata
005700951010     C                   EXSR      CARSFL
005800951009      *  Loop gestione videata
005900951009     C     WFINE         DOUEQ     'S'
006000000426     C     WTPVID        CASEQ     '1'           GESS01                         sfl pieno
006100951012     C                   END
006200020415     C                   END
006300980312     C*
006400020419     C                   MOVEL(P)  ficn50ds      KPJBU
006500951009     C                   SETON                                        LR
006600951009?     *--------------------------------------------------------------*
006700951012?     *  CARSFL: CARICAMENNTO SUBFILE                                *
006800951009?     *--------------------------------------------------------------*
006900951012     C     CARSFL        BEGSR
007000951009      *
007100951009      *  Pulisco dati SFL
007200020405     C                   SETOFF                                       2120
007300980911     C                   WRITE     y350C01
007400951009     C                   SETON                                        2021
007500951009     C                   MOVEL     '1'           WTPVID
007600020415     C                   Z-ADD     0             NRR               6 0
007700020417     C                   Z-ADD     0             v1citt
007800020415     c* questo sql mi restituisce per ogni fornitore il dettaglio degli
007900020415     c* autotrasp. e data conteggio
008000020412     c*
008100030827     c* Lettura fiftt06l/fiatt06l
008200020405     C/EXEC SQL
008300020405     C+ PREPARE S1 FROM :WrkSqlCmd
008400020405     C/END-EXEC
008500020405
008600020405     C/EXEC SQL
008700020405     C+ DECLARE A1 CURSOR FOR S1
008800020405     C/END-EXEC
008900020405
009000020405     C/EXEC SQL
009100020405     C+ OPEN A1
009200020405     C/END-EXEC
009300020405
009400020405     C                   DOU       SqlCod <> 0
009500030827     c                   select
009600030827     c                   when      f50tip = 'A'
009700020405     C/EXEC SQL
009800020405     C+ FETCH NEXT FROM A1 INTO :fiftt
009900020405     C/END-EXEC
010000030827     c                   when      f50tip = 'D'
010100030827     C/EXEC SQL
010200030827     C+ FETCH NEXT FROM A1 INTO :fiatt
010300030827     C/END-EXEC
010400030827     c                   endsl
010500020405     c*
010600020405     C                   SELECT
010700020405     C                   WHEN      SqlCod = 0
010800020502     c*                  if        tots <> 0
010900951010     C                   EXSR      CARTRN
011000020502     c*                  end
011100020405     C                   WHEN      SqlCod = 100
011200020405     C                   WHEN      SqlCod <> 0
011300020417      * Forzo la stampa del JOBLOG.
011400020417     C                   CALL      'X66CHGJOB'
011500020405     C                   ENDSL
011600020404     C                   ENDDO
011700020417     C/EXEC SQL
011800020417     C+ CLOSE A1
011900020417     C/END-EXEC
012000020405      *  Imposto dati SFLCTL
012100020405     C                   EXSR      INZ01
012200951009     C*
012300020415     C                   ENDSR
012400951013?     *--------------------------------------------------------------*
012500020415?     *  INZ01: Inizializzazione videata                             *
012600951013?     *--------------------------------------------------------------*
012700951013     C     INZ01         BEGSR
012800000426      *  Inizializzo testata sfl in base a quanto
012900000426      *  passato dal programma chiamante
013000951013     C                   MOVEL     '1'           WTPVID
013100951016     C                   MOVEL     NOMPGM        V1CPGM
013200020415     C                   MOVEL     F50DP1        V1Dfor
013300020415     C                   move      f50PD1        V1Cfor
013400020415     C                   if        f50DTI > 0
013500020415     C                   move      f50DTI        dataiso
013600020415     C                   else
013700020415     C                   move      '0001-01-01'  dataiso
013800020415     C                   end
013900020415     C                   MOVEL     dataiso       dataeur
014000020415     C                   movel     dataeur       V1CDTI
014100020415     C                   if        f50DTF > 0
014200020415     C                   movel     f50DTF        dataiso
014300020415     C                   else
014400020415     C                   move      '0001-01-01'  dataiso
014500020415     C                   end
014600020415     C                   MOVEL     dataiso       dataeur
014700020415     C                   movel     dataeur       V1CDTF
014800951013      *
014900951013     C                   ENDSR
015000951010?     *--------------------------------------------------------------*
015100020405?     *  CARTRN: Gestione caricamento dati                           *
015200951010?     *--------------------------------------------------------------*
015300951010     C     CARTRN        BEGSR
015400951010      *
015500030827     c                   select
015600030827     c                   when      f50tip = 'A'
015700020415     c                   MOVEL     pdrs          V1Cpdr
015800020415     C                   Z-ADD     Tots          v1cico
015900020415     c                   move      ddcs          dataiso
016000030827     c                   when      f50tip = 'D'
016100030827     c                   MOVEL     pdrsa         V1Cpdr
016200030827     C                   Z-ADD     Totsa         v1cico
016300030827     c                   move      ddcsa         dataiso
016400030827     c                   if        vadsa = 'A'
016500030827     c                   eval      v1cvad = 'AFF'
016600030827     c                   else
016700030827     c                   eval      v1cvad = 'DEF'
016800030827     c                   end
016900030827     c                   endsl
017000030827     C                   add       v1cico        v1citt
017100030827     c                   move      dataiso       dataeur
017200030827     c                   move      dataeur       v1cddc
017300030827     C     kapd          CHAIN     fiapd01L                           30
017400030827     C                   MOVEL     apdrsf        v1dpdr
017500020408     c*
017600020405     C                   ADD       1             NRR
017700020405     C                   WRITE     y350S01
017800951010      *
017900951010     C                   ENDSR
018000951009?     *--------------------------------------------------------------*
018100020405?     *  srcmd   Gestione comandi                                    *
018200951009?     *--------------------------------------------------------------*
018300020405     C     srcmd         BEGSR
018400020415     c                   clear                   f50cmd
018500020405      *  Fine Lavoro
018600020405     C     *INKC         IFEQ      '1'
018700020405     C                   MOVEL     'S'           WFINE
018800020405     C                   MOVEL     '03'          f50CMD
018900020405     C                   END
019000020405      *  Ritorno
019100020405     C     *INKL         IFEQ      '1'
019200020405     C                   MOVEL     'S'           WFINE
019300020405     C                   MOVEL     '12'          f50CMD
019400020405     C                   END
019500951010      *
019600020405     C                   ENDSR
019700020405?     *--------------------------------------------------------------*
019800020405?     *  GESS01: Gestione prima videata                              *
019900020405?     *--------------------------------------------------------------*
020000020405     C     GESS01        BEGSR
020100020405      *
020200980914     C                   WRITE     y350T01
020300980914     C                   EXFMT     y350C01
020400020405     c* gestione comandi
020500020405     c                   exsr      srcmd
020600951009      *
020700020417     C                   ENDSR
020800951011?     *--------------------------------------------------------------*
020900951011?     *  *INZSR: Operazioni di inizializzazione dati                 *
021000951011?     *--------------------------------------------------------------*
021100951011     C     *INZSR        BEGSR
021200951011      *
021300951011     C     *ENTRY        PLIST
021400951011     C                   PARM                    KPJBA
021500020408     C                   MOVEL     KPJBU         ficn50ds
021600030827     c* compongo istruzione sql
021700030827     c                   exsr      istruz
021800020408      *  Inizializzo variabili
021900020408     C                   MOVEL     '1'           WTPVID            1
022000020408     C                   MOVEL     'N'           WFINE             1
022100021203     c     kapd          klist
022200021203     c                   kfld                    apdtip
022300030827     c                   kfld                    v1cpdr
022400030827     c                   movel     f50tip        apdtip
022500951012     C                   ENDSR
022600030827?     *--------------------------------------------------------------*
022700030827?     *  istruz: Operazioni di inizializzazione dati                 *
022800030827?     *--------------------------------------------------------------*
022900030827     C     istruz        BEGSR
023000030827     c                   select
023100030827     c* autotrasportatori
023200030827     c                   when      f50tip = 'A'
023300030827     C                   EVAL      WrkSqlCmd
023400030827     C                             =
023500030827     C                             'SELECT fttpdr, fttddc,'
023600030827     C                             +
023700030827     C                             ' sum(fttitt+fttita+ftttpe+ftttbn+ftttim+'
023800030827     C                             +
023900030827     C                             'fttmnt)'
024000030827     C                             +
024100030827     C                             ' from fiftt06l where fttsoc = '
024200030827     C                             +
024300030827     C                             '''' + f50soc  + ''''
024400030827     C                             +
024500030827     C                             ' AND '
024600030827     C                             +
024700030827     C                             ' fttcdf between '
024800030827     C                             +
024900030827     C                             '''' + f50pd1 + ''''
025000030827     C                             +
025100030827     C                             ' AND '
025200030827     C                             +
025300030827     C                             '''' + f50pd2 + ''''
025400030827     C                             +
025500030827     C                             ' AND '
025600030827     C                             +
025700030827     C                             ' ftttsr = '' '''
025800030827     C                             +
025900030827     C                             ' AND '
026000030827     C                             +
026100030827     C                             ' fttfvl = ''C'''
026200030827     C                             +
026300030827     C                             ' AND '
026400030827     C                             +
026500030827     C                             ' Fttddc BETWEEN '
026600030827     C                             + %editc(f50dti:'X')
026700030827     C                             +
026800030827     C                             ' AND ' + %editc(f50dtf:'X')
026900030827     C                             +
027000030827     C                             ' AND '
027100030827     C                             +
027200030827     C                             'fttnff = 0 group by fttpdr, fttddc'
027300030827     c                             +
027400030827     C                             ' ORDER BY fttpdr, fttddc'
027500030827     c* aff/defl.
027600030827     c                   when      f50tip = 'D'
027700030827     C                   EVAL      WrkSqlCmd
027800030827     C                             =
027900030827     C                             'SELECT attpdr, attddc, attvad,'
028000030827     C                             +
028100030827     C                             ' sum(attimpp)'
028200030827     C                             +
028300030827     C                             ' from fiatt06l where attsoc = '
028400030827     C                             +
028500030827     C                             '''' + f50soc  + ''''
028600030827     C                             +
028700030827     C                             ' AND '
028800030827     C                             +
028900030827     C                             ' attcdf between '
029000030827     C                             +
029100030827     C                             '''' + f50pd1 + ''''
029200030827     C                             +
029300030827     C                             ' AND '
029400030827     C                             +
029500030827     C                             '''' + f50pd2 + ''''
029600030827     C                             +
029700030827     C                             ' AND '
029800030827     C                             +
029900030827     C                             ' attdco <> 0'
030000030827     C                             +
030100030827     C                             ' AND '
030200030827     C                             +
030300030827     C                             ' attddc BETWEEN '
030400030827     C                             + %editc(f50dti:'X')
030500030827     C                             +
030600030827     C                             ' AND ' + %editc(f50dtf:'X')
030700030827     C                             +
030800030827     C                             ' AND '
030900030827     C                             +
031000030827     C                             'attnff = 0 group by attpdr, attddc, attvad'
031100030827     c                             +
031200030827     C                             ' ORDER BY attpdr, attddc, attvad'
031300030827     c                   endsl
031400030827     C                   ENDSR
