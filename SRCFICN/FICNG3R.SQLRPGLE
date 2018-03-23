000100020415     H/TITLE  dettaglio autotrasportatori x fornitore
000200121122     H dftactgrp(*no) actgrp(*caller)
000300951009?     *--------------------------------------------------------------*
000400120911      * ficnG3R                                                      *
000500951009      *     *----------------------------------------------*         *
000600120911      *         GESTIONE FIRME CONTEGGI DETTAGLIO                    *
000700951009      *     *---------------------------------------------*          *
000800951009      *                                                              *
000900951009?     *--------------------------------------------------------------*
001000951009      * KC - FINE LAVORO                                             *
001100951009      * KL - RITORNO                                                 *
001200951009?     *--------------------------------------------------------------*
001300951009      * 20 - SFLDSP                                                  *
001400951009      * 21 - SFLDSPCTL/CLEAR                                         *
001500951009      * 22 - SFLNXTCHG                                               *
001600951009      * 28 - Visualizzazione messaggio di errore                     *
001700951011      * 29 - READC su subfile                                        *
001800951011      * 32 - Indicatore di comodo per READ/CHAIN/SCAN/LOKUP          *
001900951009?     *--------------------------------------------------------------*
002000021203     Ffiapd01L  IF   E           K DISK
002100020415     F*--------
002200121023     FTntbe01L  IF   E           K DISK
002300120911     FficnG3D   CF   E             WORKSTN
002400980911     F                                     SFILE(y350S01:NRR)
002500951009     D*---------------------------------------------------------------*
002600951009     D* SCHIERA
002700120926     D ERR             S             70    DIM(1) CTDATA PERRCD(1)              Errori
002800951009     D*---------------------------------------------------------------*
002900020411     D* Reperimento nome PGM
003000020411     D STATUS         SDS           333
003100020411     D  nompgm           *PROC
003200020411     D*---------------------------------------------------------------*
003300020411     D* CAMPI INTERNI
003400020411     D*---------------------------------------------------------------*
003500121023      *
003600120913     D ddatiute      e ds
003700120912     D azuteds       e ds                  extname(AZUTE00F)
003800120912     D tibs34ds      E DS                  inz
003900020411     D dataiso         s               d   datfmt(*iso)
004000020411     D dataeur         s               d   datfmt(*eur)
004100951009?     *--------------------------------------------------------------*
004200951009?     *  DS                                                          *
004300951009?     *--------------------------------------------------------------*
004400951009     D KPJBA         E DS
004500020411     d* parametri ricevuti
004600120911     D ficnG1ds      E DS
004700120925     D ficn40ds      E DS
004800120911     D fiFTTDS       E DS                  EXTNAME(FIFTT00F)
004900120911     D fiATTDS       E DS                  EXTNAME(FIATT00F)
005000020411     c* SQL
005100020405     D WrkSqlCmd       S           1024
005200020415     D fiftt           DS
005300020415     d pdrs                           7  0
005400020415     d ddcs                           8  0
005500120911     d firmatof                            like(ftttim)
005600120911     d dafirmaref                          like(ftttim)
005700120911     d retdafirf                           like(ftttim)
005800030827     D fiatt           DS
005900030827     d pdrsa                          7  0
006000030827     d ddcsa                          8  0
006100030827     d vadsa                          1    inz
006200120911     d firmatoa                            like(attimpp)
006300120911     d dafirmarea                          like(attimpp)
006400951009?     *--------------------------------------------------------------*
006500951009?     *  CICLO PRINCIPALE                                            *
006600951009?     *--------------------------------------------------------------*
006700951009      *  Impostazione parametri prima videata
006800951010     C                   EXSR      CARSFL
006900120912     c                   if        nrr <>0
007000951009      *  Loop gestione videata
007100951009     C     WFINE         DOUEQ     'S'
007200000426     C     WTPVID        CASEQ     '1'           GESS01                         sfl pieno
007300951012     C                   END
007400020415     C                   END
007500120912     C                   END
007600980312     C*
007700120911     C                   MOVEL(P)  ficnG1ds      KPJBU
007800951009     C                   SETON                                        LR
007900951009?     *--------------------------------------------------------------*
008000951012?     *  CARSFL: CARICAMENNTO SUBFILE                                *
008100951009?     *--------------------------------------------------------------*
008200951012     C     CARSFL        BEGSR
008300951009      *
008400951009      *  Pulisco dati SFL
008500020405     C                   SETOFF                                       2120
008600980911     C                   WRITE     y350C01
008700951009     C                   SETON                                        2021
008800951009     C                   MOVEL     '1'           WTPVID
008900020415     C                   Z-ADD     0             NRR               6 0
009000120912     C                   Z-ADD     0             v1cittf
009100120912     C                   Z-ADD     0             v1ctref
009200020415     c* questo sql mi restituisce per ogni fornitore il dettaglio degli
009300020415     c* autotrasp. e data conteggio
009400020412     c*
009500030827     c* Lettura fiftt06l/fiatt06l
009600020405     C/EXEC SQL
009700020405     C+ PREPARE S1 FROM :WrkSqlCmd
009800020405     C/END-EXEC
009900020405
010000020405     C/EXEC SQL
010100020405     C+ DECLARE A1 CURSOR FOR S1
010200020405     C/END-EXEC
010300020405
010400020405     C/EXEC SQL
010500020405     C+ OPEN A1
010600020405     C/END-EXEC
010700020405
010800020405     C                   DOU       SqlCod <> 0
010900030827     c                   select
011000120911     c                   when      §G1tip = 'A'
011100020405     C/EXEC SQL
011200020405     C+ FETCH NEXT FROM A1 INTO :fiftt
011300020405     C/END-EXEC
011400120911     c                   when      §G1tip = 'D'
011500030827     C/EXEC SQL
011600030827     C+ FETCH NEXT FROM A1 INTO :fiatt
011700030827     C/END-EXEC
011800030827     c                   endsl
011900020405     c*
012000020405     C                   SELECT
012100020405     C                   WHEN      SqlCod = 0
012200020502     c*                  if        tots <> 0
012300951010     C                   EXSR      CARTRN
012400020502     c*                  end
012500020405     C                   WHEN      SqlCod = 100
012600020405     C                   WHEN      SqlCod <> 0
012700020417      * Forzo la stampa del JOBLOG.
012800020417     C                   CALL      'X66CHGJOB'
012900020405     C                   ENDSL
013000020404     C                   ENDDO
013100020417     C/EXEC SQL
013200020417     C+ CLOSE A1
013300020417     C/END-EXEC
013400020405      *  Imposto dati SFLCTL
013500020405     C                   EXSR      INZ01
013600951009     C*
013700020415     C                   ENDSR
013800951013?     *--------------------------------------------------------------*
013900020415?     *  INZ01: Inizializzazione videata                             *
014000951013?     *--------------------------------------------------------------*
014100951013     C     INZ01         BEGSR
014200000426      *  Inizializzo testata sfl in base a quanto
014300000426      *  passato dal programma chiamante
014400951013     C                   MOVEL     '1'           WTPVID
014500951016     C                   MOVEL     NOMPGM        V1CPGM
014600120911     C                   MOVEL     §G1DP1        V1Dfor
014700120911     C                   move      §G1PD1        V1Cfor
014800120911     C                   if        §G1DTF > 0
014900120911     C                   movel     §G1DTF        dataiso
015000020415     C                   else
015100020415     C                   move      '0001-01-01'  dataiso
015200020415     C                   end
015300020415     C                   MOVEL     dataiso       dataeur
015400020415     C                   movel     dataeur       V1CDTF
015500951013      *
015600951013     C                   ENDSR
015700951010?     *--------------------------------------------------------------*
015800020405?     *  CARTRN: Gestione caricamento dati                           *
015900951010?     *--------------------------------------------------------------*
016000951010     C     CARTRN        BEGSR
016100951010      *
016200030827     c                   select
016300120912     c* autotrasportatori
016400120911     c                   when      §G1tip = 'A'
016500020415     c                   MOVEL     pdrs          V1Cpdr
016600120912     c                   if        §g1fir = 'S' and dafirmaref = 0
016700120912     c                             and retdafirf = 0
016800120912     c                   leavesr
016900120912     c                   end
017000120911     C                   Z-ADD     dafirmaref    v1cicof
017100120911     C                   Z-ADD     retdafirf     v1cretf
017200020415     c                   move      ddcs          dataiso
017300120912     c* aff/def
017400120911     c                   when      §G1tip = 'D'
017500120912     c                   if        §g1fir = 'S' and dafirmarea = 0
017600120912     c                   leavesr
017700120912     c                   end
017800030827     c                   MOVEL     pdrsa         V1Cpdr
017900030827     c                   move      ddcsa         dataiso
018000120911     C                   Z-ADD     dafirmarea    v1cicof
018100120911     C                   Z-ADD     0             v1cretf
018200030827     c                   if        vadsa = 'A'
018300030827     c                   eval      v1cvad = 'AFF'
018400030827     c                   else
018500030827     c                   eval      v1cvad = 'DEF'
018600030827     c                   end
018700030827     c                   endsl
018800120911     C                   ADD       V1CICOf       V1CITTf
018900120911     C                   ADD       V1Cretf       V1Ctref
019000030827     c                   move      dataiso       dataeur
019100030827     c                   move      dataeur       v1cddc
019200030827     C     kapd          CHAIN     fiapd01L                           30
019300030827     C                   MOVEL     apdrsf        v1dpdr
019400020408     c*
019500020405     C                   ADD       1             NRR
019600020405     C                   WRITE     y350S01
019700951010      *
019800951010     C                   ENDSR
019900951009?     *--------------------------------------------------------------*
020000020405?     *  srcmd   Gestione comandi                                    *
020100951009?     *--------------------------------------------------------------*
020200020405     C     srcmd         BEGSR
020300120911     c                   clear                   §G1cmd
020400020405      *  Fine Lavoro
020500020405     C     *INKC         IFEQ      '1'
020600020405     C                   MOVEL     'S'           WFINE
020700120911     C                   MOVEL     '03'          §G1CMD
020800020405     C                   END
020900020405      *  Ritorno
021000020405     C     *INKL         IFEQ      '1'
021100020405     C                   MOVEL     'S'           WFINE
021200120911     C                   MOVEL     '12'          §G1CMD
021300020405     C                   END
021400951010      *
021500020405     C                   ENDSR
021600020405?     *--------------------------------------------------------------*
021700020405?     *  GESS01: Gestione prima videata                              *
021800020405?     *--------------------------------------------------------------*
021900020405     C     GESS01        BEGSR
022000020405      *
022100980914     C                   WRITE     y350T01
022200980914     C                   EXFMT     y350C01
022300020405     c* gestione comandi
022400020405     c                   exsr      srcmd
022500120925      *  Controlli
022600120925     C                   EXSR      CTR01
022700120926     C   28              GOTO      FINVD1
022800951009      *
022900120926     C     finvd1        ENDSR
023000120925?     *--------------------------------------------------------------*
023100120925?     *  CTR01: Controlli prima videata                              *
023200120925?     *--------------------------------------------------------------*
023300120925     C     CTR01         BEGSR
023400120925      *
023500120925     C                   READC     y350S01                                29
023600120925     C     *IN29         DOWEQ     '0'
023700120926     c                   select
023800120926     C                   when      v1csce = '5' and v1cretf = 0
023900120926     C                   SETON                                        28
024000120926     C                   MOVEL     ERR(1)        $MSG
024100120926     C                   GOTO      FINCT1
024200120926     C                   when      v1csce = '5' and v1cretf <> 0
024300120925     c*
024400120925     C                   clear                   ficn40ds
024500120925     C                   MOVEL     §g1tip        §40tip
024600120925     C                   MOVEL     'L'           §40tla
024700120925     C                   MOVEL     'C'           §40flg
024800120925     C                   MOVEL     v1cpdr        §40PDR
024900120925     C                   MOVEL     v1cddc        dataeur
025000120925     C                   MOVEL     dataeur       dataiso
025100120925     C                   MOVEL     dataiso       §40ddc
025200120925     C                   MOVEL     v1ctsr        §40tsr
025300120925     C                   MOVEL(p)  ficn40ds      kpjbu
025400120925     c                   call      'FICN40R'
025500120925     c                   parm                    kpjba
025600120925     c                   clear                   v1csce
025700120926     C                   ENDsl
025800120925     C                   SETOFF                                       22
025900120925     C                   UPDATE    y350S01
026000120925     C                   READC     Y350S01                                29
026100120925     C                   ENDdo
026200120925      *
026300120926     C     finct1        ENDSR
026400951011?     *--------------------------------------------------------------*
026500951011?     *  *INZSR: Operazioni di inizializzazione dati                 *
026600951011?     *--------------------------------------------------------------*
026700951011     C     *INZSR        BEGSR
026800951011      *
026900951011     C     *ENTRY        PLIST
027000951011     C                   PARM                    KPJBA
027100120912     c     *dtaara       define    §azute        azuteds
027200120912     c     *dtaara       define    §datiute      ddatiute
027300120912     C                   in(E)     *dtaara
027400120912     C                   IF        %Error  or  RSUT = *blanks
027500120912     C                   call      'TIBS34R'
027600120912     C                   parm                    Tibs34Ds
027700120912     C                   in        *dtaara
027800120912     c                   ENDIF
027900120911     C                   MOVEL     KPJBU         ficnG1ds
028000030827     c* compongo istruzione sql
028100030827     c                   exsr      istruz
028200020408      *  Inizializzo variabili
028300020408     C                   MOVEL     '1'           WTPVID            1
028400020408     C                   MOVEL     'N'           WFINE             1
028500120912     c     §g1fir        comp      'S'                                    62
028600021203     c     kapd          klist
028700021203     c                   kfld                    apdtip
028800030827     c                   kfld                    v1cpdr
028900121023      *
029000121023     C     Ktbe          KLIST
029100121023     C                   KFLD                    tbecod
029200121023     C                   KFLD                    tbeke1
029300121023     C                   KFLD                    tbeke2
029400121023     C*
029500120911     c                   movel     §G1tip        apdtip
029600121023     C*
029700951012     C                   ENDSR
029800030827?     *--------------------------------------------------------------*
029900030827?     *  istruz: Operazioni di inizializzazione dati                 *
030000030827?     *--------------------------------------------------------------*
030100030827     C     istruz        BEGSR
030200030827     c                   select
030300030827     c* autotrasportatori
030400120911     c                   when      §G1tip = 'A'
030500030827     C                   EVAL      WrkSqlCmd
030600030827     C                             =
030700030827     C                             'SELECT fttpdr, fttddc,'
030800120911     C                             +
030900120911     C                             ' sum( case when substr( '
031000120911     C                             +
031100130307     C                             ' FTTFLR, 1, 1) =''S'' and fttNFF = 0 then '
031200120911     C                             +
031300120911     C                             'fttitt+fttita+ftttpe+ftttbn+ftttim+fttmnt '
031400120911     C                             +
031500120911     C                             ' else 0 end ) as firmato, '
031600120911     C                             +
031700120911     C                             ' sum( case when substr( '
031800120911     C                             +
031900120911     C                             ' FTTFLR, 1, 1) ='' '' then '
032000120911     C                             +
032100120911     C                             'fttitt+fttita+ftttpe+ftttbn+ftttim+fttmnt '
032200120911     C                             +
032300120911     C                             ' else 0 end ) as dafirmare, '
032400120911     C                             +
032500120911     C                             ' sum( case when substr( '
032600120911     C                             +
032700120911     C                             ' FTTFLR, 1, 1) ='' '' then '
032800120911     C                             +
032900120911     C                             'ftttim '
033000120911     C                             +
033100120911     C                             ' else 0 end ) as retdafir '
033200120911     C                             +
033300030827     C                             ' from fiftt06l where fttsoc = '
033400030827     C                             +
033500120911     C                             '''' + §G1soc  + ''''
033600030827     C                             +
033700030827     C                             ' AND '
033800030827     C                             +
033900030827     C                             ' fttcdf between '
034000030827     C                             +
034100120911     C                             '''' + §G1pd1 + ''''
034200030827     C                             +
034300030827     C                             ' AND '
034400030827     C                             +
034500120911     C                             '''' + §G1pd2 + ''''
034600030827     C                             +
034700030827     C                             ' AND '
034800030827     C                             +
034900030827     C                             ' ftttsr = '' '''
035000030827     C                             +
035100030827     C                             ' AND '
035200030827     C                             +
035300030827     C                             ' fttfvl = ''C'''
035400030827     C                             +
035500030827     C                             ' AND '
035600030827     C                             +
035700030827     C                             ' Fttddc BETWEEN '
035800120911     C                             + %editc(§G1dti:'X')
035900030827     C                             +
036000120911     C                             ' AND ' + %editc(§G1dtf:'X')
036100030827     C                             +
036200121025     C                             ' group by fttpdr, fttddc'
036300030827     c                             +
036400030827     C                             ' ORDER BY fttpdr, fttddc'
036500121025     C*******   vecchia istruzione di GROUP BY con FATTURA
036600121025     C*******                      + ' and fttnff = 0  group by fttpdr, fttddc''
036700030827     c* aff/defl.
036800120911     c                   when      §G1tip = 'D'
036900030827     C                   EVAL      WrkSqlCmd
037000030827     C                             =
037100030827     C                             'SELECT attpdr, attddc, attvad,'
037200120911     C                             +
037300120911     C                             ' sum( case when substr( '
037400120911     C                             +
037500130307     C                             ' aTTFLR, 1, 1) =''S'' and ATTnff = 0 then '
037600120911     C                             +
037700120911     C                             ' attimpp'
037800120911     C                             +
037900120911     C                             ' else 0 end ) as firmato, '
038000120911     C                             +
038100120911     C                             ' sum( case when substr( '
038200120911     C                             +
038300120911     C                             ' aTTFLR, 1, 1) ='' '' then '
038400120911     C                             +
038500120911     C                             ' attimpp'
038600120911     C                             +
038700120911     C                             ' else 0 end ) as dafirmare '
038800120911     C                             +
038900030827     C                             ' from fiatt06l where attsoc = '
039000030827     C                             +
039100120911     C                             '''' + §G1soc  + ''''
039200030827     C                             +
039300030827     C                             ' AND '
039400030827     C                             +
039500030827     C                             ' attcdf between '
039600030827     C                             +
039700120911     C                             '''' + §G1pd1 + ''''
039800030827     C                             +
039900030827     C                             ' AND '
040000030827     C                             +
040100120911     C                             '''' + §G1pd2 + ''''
040200030827     C                             +
040300030827     C                             ' AND '
040400030827     C                             +
040500030827     C                             ' attdco <> 0'
040600030827     C                             +
040700030827     C                             ' AND '
040800030827     C                             +
040900030827     C                             ' attddc BETWEEN '
041000120911     C                             + %editc(§G1dti:'X')
041100030827     C                             +
041200120911     C                             ' AND ' + %editc(§G1dtf:'X')
041300030827     C                             +
041400121025     C                             ' group by attpdr, attddc, attvad'
041500030827     c                             +
041600030827     C                             ' ORDER BY attpdr, attddc, attvad'
041700121025     C*******   vecchia istruzione di GROUP BY con FATTURA
041800121025     C********                     + ' and attnff = 0 group by attpdr, attddc, attvad'
041900030827     c                   endsl
042000030827     C                   ENDSR
042100120926      *------------------------------------------------------------------------
042200120926** ERR
042300120926Non ci sono variazioni !!!!                                           1
