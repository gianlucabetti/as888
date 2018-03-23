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
004300020412     D cmd             s             70    ctdata dim(10)                       FIL. COMODO
004400020405     D WrkSqlCmd       S           1024
004500020415     D fiftt           DS
004600020415     d pdrs                           7  0
004700020415     d ddcs                           8  0
004800020415     d tots                          12  3
004900951009?     *--------------------------------------------------------------*
005000951009?     *  CICLO PRINCIPALE                                            *
005100951009?     *--------------------------------------------------------------*
005200951009      *  Impostazione parametri prima videata
005300951010     C                   EXSR      CARSFL
005400951009      *  Loop gestione videata
005500951009     C     WFINE         DOUEQ     'S'
005600000426     C     WTPVID        CASEQ     '1'           GESS01                         sfl pieno
005700951012     C                   END
005800020415     C                   END
005900980312     C*
006000020419     C                   MOVEL(P)  ficn50ds      KPJBU
006100951009     C                   SETON                                        LR
006200951009?     *--------------------------------------------------------------*
006300951012?     *  CARSFL: CARICAMENNTO SUBFILE                                *
006400951009?     *--------------------------------------------------------------*
006500951012     C     CARSFL        BEGSR
006600951009      *
006700951009      *  Pulisco dati SFL
006800020405     C                   SETOFF                                       2120
006900980911     C                   WRITE     y350C01
007000951009     C                   SETON                                        2021
007100951009     C                   MOVEL     '1'           WTPVID
007200020415     C                   Z-ADD     0             NRR               6 0
007300020417     C                   Z-ADD     0             v1citt
007400020415     c* questo sql mi restituisce per ogni fornitore il dettaglio degli
007500020415     c* autotrasp. e data conteggio
007600020412     c*
007700020415     c* Lettura fiftt06l
007800020405     C/EXEC SQL
007900020405     C+ PREPARE S1 FROM :WrkSqlCmd
008000020405     C/END-EXEC
008100020405
008200020405     C/EXEC SQL
008300020405     C+ DECLARE A1 CURSOR FOR S1
008400020405     C/END-EXEC
008500020405
008600020405     C/EXEC SQL
008700020405     C+ OPEN A1
008800020405     C/END-EXEC
008900020405
009000020405     C                   DOU       SqlCod <> 0
009100020405     C/EXEC SQL
009200020405     C+ FETCH NEXT FROM A1 INTO :fiftt
009300020405     C/END-EXEC
009400020405     c*
009500020405     C                   SELECT
009600020405     C                   WHEN      SqlCod = 0
009700020502     c*                  if        tots <> 0
009800951010     C                   EXSR      CARTRN
009900020502     c*                  end
010000020405     C                   WHEN      SqlCod = 100
010100020405     C                   WHEN      SqlCod <> 0
010200020417      * Forzo la stampa del JOBLOG.
010300020417     C                   CALL      'X66CHGJOB'
010400020405     C                   ENDSL
010500020404     C                   ENDDO
010600020417     C/EXEC SQL
010700020417     C+ CLOSE A1
010800020417     C/END-EXEC
010900020405      *  Imposto dati SFLCTL
011000020405     C                   EXSR      INZ01
011100951009     C*
011200020415     C                   ENDSR
011300951013?     *--------------------------------------------------------------*
011400020415?     *  INZ01: Inizializzazione videata                             *
011500951013?     *--------------------------------------------------------------*
011600951013     C     INZ01         BEGSR
011700000426      *  Inizializzo testata sfl in base a quanto
011800000426      *  passato dal programma chiamante
011900951013     C                   MOVEL     '1'           WTPVID
012000951016     C                   MOVEL     NOMPGM        V1CPGM
012100020415     C                   MOVEL     F50DP1        V1Dfor
012200020415     C                   move      f50PD1        V1Cfor
012300020415     C                   if        f50DTI > 0
012400020415     C                   move      f50DTI        dataiso
012500020415     C                   else
012600020415     C                   move      '0001-01-01'  dataiso
012700020415     C                   end
012800020415     C                   MOVEL     dataiso       dataeur
012900020415     C                   movel     dataeur       V1CDTI
013000020415     C                   if        f50DTF > 0
013100020415     C                   movel     f50DTF        dataiso
013200020415     C                   else
013300020415     C                   move      '0001-01-01'  dataiso
013400020415     C                   end
013500020415     C                   MOVEL     dataiso       dataeur
013600020415     C                   movel     dataeur       V1CDTF
013700951013      *
013800951013     C                   ENDSR
013900951010?     *--------------------------------------------------------------*
014000020405?     *  CARTRN: Gestione caricamento dati                           *
014100951010?     *--------------------------------------------------------------*
014200951010     C     CARTRN        BEGSR
014300951010      *
014400020415     c                   MOVEL     pdrs          V1Cpdr
014500021203     C     kapd          CHAIN     fiapd01L                           30
014600030414     C                   MOVEL     apdrsf        v1dpdr
014700020415     C                   Z-ADD     Tots          v1cico
014800020417     C                   add       Tots          v1citt
014900020415     c                   move      ddcs          dataiso
015000020415     c                   move      dataiso       dataeur
015100020415     c                   move      dataeur       v1cddc
015200020408     c*
015300020405     C                   ADD       1             NRR
015400020405     C                   WRITE     y350S01
015500951010      *
015600951010     C                   ENDSR
015700951009?     *--------------------------------------------------------------*
015800020405?     *  srcmd   Gestione comandi                                    *
015900951009?     *--------------------------------------------------------------*
016000020405     C     srcmd         BEGSR
016100020415     c                   clear                   f50cmd
016200020405      *  Fine Lavoro
016300020405     C     *INKC         IFEQ      '1'
016400020405     C                   MOVEL     'S'           WFINE
016500020405     C                   MOVEL     '03'          f50CMD
016600020405     C                   END
016700020405      *  Ritorno
016800020405     C     *INKL         IFEQ      '1'
016900020405     C                   MOVEL     'S'           WFINE
017000020405     C                   MOVEL     '12'          f50CMD
017100020405     C                   END
017200951010      *
017300020405     C                   ENDSR
017400020405?     *--------------------------------------------------------------*
017500020405?     *  GESS01: Gestione prima videata                              *
017600020405?     *--------------------------------------------------------------*
017700020405     C     GESS01        BEGSR
017800020405      *
017900980914     C                   WRITE     y350T01
018000980914     C                   EXFMT     y350C01
018100020405     c* gestione comandi
018200020405     c                   exsr      srcmd
018300951009      *
018400020417     C                   ENDSR
018500951011?     *--------------------------------------------------------------*
018600951011?     *  *INZSR: Operazioni di inizializzazione dati                 *
018700951011?     *--------------------------------------------------------------*
018800951011     C     *INZSR        BEGSR
018900951011      *
019000951011     C     *ENTRY        PLIST
019100951011     C                   PARM                    KPJBA
019200020408     C                   MOVEL     KPJBU         ficn50ds
019300020415     c                   eval      %subst(cmd(2): 31: 3) = F50SOC
019400020415     c                   eval      %subst(cmd(2): 50: 8) = F50pd1
019500020415     c                   move      f50dti        com8              8
019600020415     c                   eval      %subst(cmd(4): 11: 8) = com8
019700020415     c                   move      f50dtf        com8
019800020415     c                   eval      %subst(cmd(4): 34: 8) = com8
019900020412     c* MI COMPONGO L'ISTRUZIONE SQL
020000020412     c                   EVAL      WRKSQLCMD = CMD(1)+CMD(2)+CMD(3)
020100020415     C                             +CMD(4)+CMD(5)+CMD(6)
020200020408      *  Inizializzo variabili
020300020408     C                   MOVEL     '1'           WTPVID            1
020400020408     C                   MOVEL     'N'           WFINE             1
020401021203     c     kapd          klist
020402021203     c                   kfld                    apdtip
020403021203     c                   kfld                    pdrs
020404021203     c                   movel     'A'           apdtip
020500951012     C                   ENDSR
020600020405**
020700020415SELECT fttpdr, fttddc, sum(fttitt+fttita+ftttpe+ftttbn+ftttim+fttmnt) 1
020800020415from fiftt06l WHERE fttsoc = '000' and fttcdf = '00000000' and        2
020900020415ftttsr = ' ' and fttfvl = 'C' and                                     3
021000020415fttddc >= 00000000 and fttddc <= 00000000 and fttnff = 0              4
021100020415GROUP BY fttpdr, fttddc                                               5
021200020415ORDER BY fttpdr, fttddc                                               60
