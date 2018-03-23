000100120912      /TITLE  dettaglio variaz. x fornitore
000200121122     H dftactgrp(*no) actgrp(*caller)
000300951009?     *--------------------------------------------------------------*
000400120911      * ficnG4R                                                      *
000500951009      *     *----------------------------------------------*         *
000600120911      *         GESTIONE FIRME CONTEGGI DETTAGLIO variazioni         *
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
002100120911     Ftabel00f  if   E           K DISK
002200121023     FTntbe01L  IF   E           K DISK
002300020415     F*--------
002400120911     FficnG4D   CF   E             WORKSTN
002500980911     F                                     SFILE(y350S01:NRR)
002600951009     D*---------------------------------------------------------------*
002700951009     D* SCHIERA
002800951009     D*---------------------------------------------------------------*
002900020411     D* Reperimento nome PGM
003000020411     D STATUS         SDS           333
003100020411     D  nompgm           *PROC
003200020411     D*---------------------------------------------------------------*
003300020411     D* CAMPI INTERNI
003400020411     D*---------------------------------------------------------------*
003500120913     D ddatiute      e ds
003600120912     D azuteds       e ds                  extname(AZUTE00F)
003700120912     D tibs34ds      E DS                  inz
003800020411     D dataiso         s               d   datfmt(*iso)
003900020411     D dataeur         s               d   datfmt(*eur)
004000951009?     *--------------------------------------------------------------*
004100951009?     *  DS                                                          *
004200951009?     *--------------------------------------------------------------*
004300951009     D KPJBA         E DS
004400120911     D ds1z          e ds                  inz
004500020411     d* parametri ricevuti
004600120911     D ficn40ds      E DS
004700120911     D ficnG1ds      E DS
004800120911     D fiFTTDS       E DS                  EXTNAME(FIFTT00F)
004900120911     D ficTTDS       E DS                  EXTNAME(FIcTT00F)
005000020411     c* SQL
005100020405     D WrkSqlCmd       S           1024
005200020415     D fiftt           DS
005300020415     d pdrs                           7  0
005400020415     d ddcs                           8  0
005500120912     d firmatof                            like(ftttim)
005600120911     d dafirmaref                          like(ftttim)
005700120911     d retdafirf                           like(ftttim)
005800120911     c* sfl 1 coop accorpate
005900120911     D fictt           DS
006000120911     d pdrsc                               like(cttpdr)
006100120911     d ddcsc                          8  0
006200120912     d firmatoc                            like(cttitc)
006300120911     d dafirmarec                          like(cttitc)
006400120911     d retdafirc                           like(cttitc)
006500120911     c* sfl 1 coop non accorpate
006600120911     D fictttsr        DS
006700120911     d pdrsc1                              like(cttpdr)
006800120911     d ddcsc1                         8  0
006900120912     d firmato1                            like(cttitc)
007000120911     d dafirmare1                          like(cttitc)
007100120911     d retdafir1                           like(cttitc)
007200120911     d tsrsc1                              like(ctttsr)
007300951009?     *--------------------------------------------------------------*
007400951009?     *  CICLO PRINCIPALE                                            *
007500951009?     *--------------------------------------------------------------*
007600951009      *  Impostazione parametri prima videata
007700951010     C                   EXSR      CARSFL
007800120911     c                   if        nrr <>0
007900951009      *  Loop gestione videata
008000951009     C     WFINE         DOUEQ     'S'
008100000426     C     WTPVID        CASEQ     '1'           GESS01                         sfl pieno
008200951012     C                   END
008300020415     C                   END
008400120911     c                   end
008500980312     C*
008600120911     C                   MOVEL(P)  ficnG1ds      KPJBU
008700951009     C                   SETON                                        LR
008800951009?     *--------------------------------------------------------------*
008900951012?     *  CARSFL: CARICAMENNTO SUBFILE                                *
009000951009?     *--------------------------------------------------------------*
009100951012     C     CARSFL        BEGSR
009200951009      *
009300951009      *  Pulisco dati SFL
009400020405     C                   SETOFF                                       2120
009500980911     C                   WRITE     y350C01
009600951009     C                   SETON                                        2021
009700951009     C                   MOVEL     '1'           WTPVID
009800020415     C                   Z-ADD     0             NRR               6 0
009900120912     C                   Z-ADD     0             v1cittf
010000120911     C                   Z-ADD     0             v1ctref
010100020415     c* questo sql mi restituisce per ogni fornitore il dettaglio degli
010200020415     c* autotrasp. e data conteggio
010300020412     c*
010400030827     c* Lettura fiftt06l/fiatt06l
010500020405     C/EXEC SQL
010600020405     C+ PREPARE S1 FROM :WrkSqlCmd
010700020405     C/END-EXEC
010800020405
010900020405     C/EXEC SQL
011000020405     C+ DECLARE A1 CURSOR FOR S1
011100020405     C/END-EXEC
011200020405
011300020405     C/EXEC SQL
011400020405     C+ OPEN A1
011500020405     C/END-EXEC
011600020405
011700020405     C                   DOU       SqlCod <> 0
011800030827     c                   select
011900120911     c                   when      §G1tip = 'A'
012000020405     C/EXEC SQL
012100020405     C+ FETCH NEXT FROM A1 INTO :fiftt
012200020405     C/END-EXEC
012300120911     c* cooperative      senza tipo servizio
012400120911     c                   when      §G1tip = 'C'
012500120911     c                   if        §G1tsr = 'S'
012600120911     C/EXEC SQL
012700120911     C+ FETCH NEXT FROM A1 INTO :fictt
012800120911     C/END-EXEC
012900120911     c                   else
013000120911     C/EXEC SQL
013100120911     C+ FETCH NEXT FROM A1 INTO :fictttsr
013200120911     C/END-EXEC
013300120911     c* cooperative      con tipo servizio
013400120911     c                   end
013500030827     c                   endsl
013600020405     c*
013700020405     C                   SELECT
013800020405     C                   WHEN      SqlCod = 0
013900951010     C                   EXSR      CARTRN
014000020502     c*                  end
014100020405     C                   WHEN      SqlCod = 100
014200020405     C                   WHEN      SqlCod <> 0
014300020417      * Forzo la stampa del JOBLOG.
014400020417     C                   CALL      'X66CHGJOB'
014500020405     C                   ENDSL
014600020404     C                   ENDDO
014700020417     C/EXEC SQL
014800020417     C+ CLOSE A1
014900020417     C/END-EXEC
015000020405      *  Imposto dati SFLCTL
015100020405     C                   EXSR      INZ01
015200951009     C*
015300020415     C                   ENDSR
015400951013?     *--------------------------------------------------------------*
015500020415?     *  INZ01: Inizializzazione videata                             *
015600951013?     *--------------------------------------------------------------*
015700951013     C     INZ01         BEGSR
015800000426      *  Inizializzo testata sfl in base a quanto
015900000426      *  passato dal programma chiamante
016000951013     C                   MOVEL     '1'           WTPVID
016100951016     C                   MOVEL     NOMPGM        V1CPGM
016200120911     C                   MOVEL     §G1DP1        V1Dfor
016300120911     C                   move      §G1PD1        V1Cfor
016400120911     C                   if        §G1DTF > 0
016500120911     C                   movel     §G1DTF        dataiso
016600020415     C                   else
016700020415     C                   move      '0001-01-01'  dataiso
016800020415     C                   end
016900020415     C                   MOVEL     dataiso       dataeur
017000020415     C                   movel     dataeur       V1CDTF
017100951013      *
017200951013     C                   ENDSR
017300951010?     *--------------------------------------------------------------*
017400020405?     *  CARTRN: Gestione caricamento dati                           *
017500951010?     *--------------------------------------------------------------*
017600951010     C     CARTRN        BEGSR
017700951010      *
017800030827     c                   select
017900120912     c* aut
018000120911     c                   when      §G1tip = 'A'
018100120912     c                   if        §g1fir = 'S' and
018200120912     c                             dafirmaref = 0 and retdafirf = 0
018300120912     c                   leavesr
018400120912     c                   end
018500020415     c                   MOVEL     pdrs          V1Cpdr
018600120912     C                   Z-ADD     dafirmaref    v1cicof
018700120911     C                   Z-ADD     retdafirf     v1cretf
018800020415     c                   move      ddcs          dataiso
018900120912     c* coop
019000120911     c                   when      §G1tip = 'C'
019100120911     C                   if        §G1tsr = 'S'
019200120912     c                   if        §g1fir = 'S' and
019300120912     c                             dafirmarec = 0 and retdafirc = 0
019400120912     c                   leavesr
019500120912     c                   end
019600120911     c                   MOVEL     pdrsc         V1Cpdr
019700120911     c                   move      ddcsc         dataiso
019800120912     C                   Z-ADD     dafirmarec    v1cicof
019900120911     C                   Z-ADD     retdafirc     v1cretf
020000120911     c                   clear                   v1ctsr
020100120911     c                   clear                   v1dtsr
020200120911     c                   else
020300120912     c                   if        §g1fir = 'S' and
020400120912     c                             dafirmare1 = 0 and retdafir1 = 0
020500120912     c                   leavesr
020600120912     c                   end
020700120912     c                   if        §g1tsr <>tsrsc1
020800120912     c                   leavesr
020900120912     c                   end
021000120911     c* cooperativa  no accorpam.
021100120912     C                   Z-ADD     dafirmare1    v1cicof
021200120911     C                   Z-ADD     retdafir1     v1cretf
021300120911     C                   MOVE      pdrsc1        V1Cpdr
021400120911     C                   MOVEL     tsrsc1        V1Ctsr
021500120911     C                   MOVEL(p)  tsrsc1        tblkey
021600120912     c                   move      ddcsc1        dataiso
021700120911     c* decodifico tipo servizio
021800120911     c     ktab          chain     tabel00f
021900120911     c                   if        %found(tabel00f)
022000120911     c                   movel     tbluni        ds1z
022100120911     c                   movel     §1zdes        v1dtsr
022200120911     c                   else
022300120911     c                   clear                   v1dtsr
022400120911     c                   end
022500120911     c                   end
022600030827     c                   endsl
022700120912     C                   ADD       V1CICOf       V1CITTf
022800120911     C                   ADD       V1Cretf       V1Ctref
022900030827     c                   move      dataiso       dataeur
023000030827     c                   move      dataeur       v1cddc
023100030827     C     kapd          CHAIN     fiapd01L                           30
023200030827     C                   MOVEL     apdrsf        v1dpdr
023300020408     c*
023400020405     C                   ADD       1             NRR
023500020405     C                   WRITE     y350S01
023600951010      *
023700951010     C                   ENDSR
023800951009?     *--------------------------------------------------------------*
023900020405?     *  srcmd   Gestione comandi                                    *
024000951009?     *--------------------------------------------------------------*
024100020405     C     srcmd         BEGSR
024200120911     c                   clear                   §G1cmd
024300020405      *  Fine Lavoro
024400020405     C     *INKC         IFEQ      '1'
024500020405     C                   MOVEL     'S'           WFINE
024600120911     C                   MOVEL     '03'          §G1CMD
024700020405     C                   END
024800020405      *  Ritorno
024900020405     C     *INKL         IFEQ      '1'
025000020405     C                   MOVEL     'S'           WFINE
025100120911     C                   MOVEL     '12'          §G1CMD
025200020405     C                   END
025300951010      *
025400020405     C                   ENDSR
025500020405?     *--------------------------------------------------------------*
025600020405?     *  GESS01: Gestione prima videata                              *
025700020405?     *--------------------------------------------------------------*
025800020405     C     GESS01        BEGSR
025900020405      *
026000980914     C                   WRITE     y350T01
026100980914     C                   EXFMT     y350C01
026200020405     c* gestione comandi
026300020405     c                   exsr      srcmd
026400120911      *  Controlli
026500120911     C                   EXSR      CTR01
026600951009      *
026700020417     C                   ENDSR
026800120911?     *--------------------------------------------------------------*
026900120911?     *  CTR01: Controlli prima videata                              *
027000120911?     *--------------------------------------------------------------*
027100120911     C     CTR01         BEGSR
027200120911      *
027300120911     C                   READC     y350S01                                29
027400120911     C     *IN29         DOWEQ     '0'
027500120911     C                   if        v1csce = '5'
027600120911     c*
027700120911     C                   clear                   ficn40ds
027800120911     C                   MOVEL     §g1tip        §40tip
027900120913     C                   MOVEL     'L'           §40tla
028000120913     C                   MOVEL     'C'           §40flg
028100120911     C                   MOVEL     v1cpdr        §40PDR
028200120912     C                   MOVEL     v1cddc        dataeur
028300120912     C                   MOVEL     dataeur       dataiso
028400120911     C                   MOVEL     dataiso       §40ddc
028500120920     C                   MOVEL     v1ctsr        §40tsr
028600120911     C                   MOVEL(p)  ficn40ds      kpjbu
028700120911     c                   call      'FICN40R'
028800120911     c                   parm                    kpjba
028900120911     c                   clear                   v1csce
029000120911     C                   END
029100120911     C                   SETOFF                                       22
029200120911     C                   UPDATE    y350S01
029300120911     C                   READC     Y350S01                                29
029400120911     C                   ENDdo
029500120911      *
029600120911     C                   ENDSR
029700951011?     *--------------------------------------------------------------*
029800951011?     *  *INZSR: Operazioni di inizializzazione dati                 *
029900951011?     *--------------------------------------------------------------*
030000951011     C     *INZSR        BEGSR
030100951011      *
030200951011     C     *ENTRY        PLIST
030300951011     C                   PARM                    KPJBA
030400120912     c     *dtaara       define    §azute        azuteds
030500120912     c     *dtaara       define    §datiute      ddatiute
030600120912     C                   in(E)     *dtaara
030700120912     C                   IF        %Error  or  RSUT = *blanks
030800120912     C                   call      'TIBS34R'
030900120912     C                   parm                    Tibs34Ds
031000120912     C                   in        *dtaara
031100120912     c                   ENDIF
031200120911     C                   MOVEL     KPJBU         ficnG1ds
031300120912     c     §g1fir        comp      'S'                                    62
031400120912     c                   if        §g1tip = 'C'
031500120912     c                   seton                                        60
031600120912     c                   eval      *inki ='1'
031700120912     c                   end
031800030827     c* compongo istruzione sql
031900030827     c                   exsr      istruz
032000020408      *  Inizializzo variabili
032100020408     C                   MOVEL     '1'           WTPVID            1
032200020408     C                   MOVEL     'N'           WFINE             1
032300120911     C     Ktab          KLIST
032400120911     C                   KFLD                    tblkut
032500120911     C                   KFLD                    tblcod
032600120911     C                   KFLD                    tblkey
032700120911     c                   eval      tblkut = 1
032800120911     c                   eval      tblcod = '1Z'
032900021203     c     kapd          klist
033000021203     c                   kfld                    apdtip
033100030827     c                   kfld                    v1cpdr
033200121023      *
033300121023     C     Ktbe          KLIST
033400121023     C                   KFLD                    tbecod
033500121023     C                   KFLD                    tbeke1
033600121023     C                   KFLD                    tbeke2
033700121023     C*
033800120911     c                   movel     §G1tip        apdtip
033900951012     C                   ENDSR
034000030827?     *--------------------------------------------------------------*
034100030827?     *  istruz: Operazioni di inizializzazione dati                 *
034200030827?     *--------------------------------------------------------------*
034300030827     C     istruz        BEGSR
034400030827     c                   select
034500030827     c* autotrasportatori
034600120911     c                   when      §G1tip = 'A'
034700030827     C                   EVAL      WrkSqlCmd
034800030827     C                             =
034900030827     C                             'SELECT fttpdr, fttddc,'
035000120912     C                             +
035100120912     C                             ' sum( case when substr( '
035200120912     C                             +
035300130307     C                             ' FTTFLR, 1, 1) =''S'' and fttNFF = 0 then '
035400120912     C                             +
035500120912     C                             'fttitt+fttita+ftttpe+ftttbn+ftttim+fttmnt '
035600120912     C                             +
035700120912     C                             ' else 0 end ) as firmato, '
035800120912     C                             +
035900120912     C                             ' sum( case when substr( '
036000120912     C                             +
036100120912     C                             ' FTTFLR, 1, 1) ='' '' then '
036200120912     C                             +
036300120912     C                             'fttitt+fttita+ftttpe+ftttbn+ftttim+fttmnt '
036400120912     C                             +
036500120912     C                             ' else 0 end ) as dafirmare, '
036600120912     C                             +
036700120912     C                             ' sum( case when substr( '
036800120912     C                             +
036900120912     C                             ' FTTFLR, 1, 1) ='' '' then '
037000120912     C                             +
037100120912     C                             'ftttim '
037200120912     C                             +
037300120912     C                             ' else 0 end ) as retdafir '
037400120911     C                             +
037500030827     C                             ' from fiftt06l where fttsoc = '
037600030827     C                             +
037700120911     C                             '''' + §G1soc  + ''''
037800030827     C                             +
037900030827     C                             ' AND '
038000030827     C                             +
038100030827     C                             ' fttcdf between '
038200030827     C                             +
038300120911     C                             '''' + §G1pd1 + ''''
038400030827     C                             +
038500030827     C                             ' AND '
038600030827     C                             +
038700120911     C                             '''' + §G1pd2 + ''''
038800030827     C                             +
038900030827     C                             ' AND '
039000030827     C                             +
039100030827     C                             ' ftttsr = '' '''
039200030827     C                             +
039300030827     C                             ' AND '
039400030827     C                             +
039500030827     C                             ' fttfvl = ''C'''
039600030827     C                             +
039700030827     C                             ' AND '
039800030827     C                             +
039900030827     C                             ' Fttddc BETWEEN '
040000120911     C                             + %editc(§G1dti:'X')
040100030827     C                             +
040200120911     C                             ' AND ' + %editc(§G1dtf:'X')
040300120911     C                             +
040400120912     C                             ' AND ftttim <> 0'
040500120912     C                             +
040600121025     C                             ' group by fttpdr, fttddc'
040700030827     c                             +
040800030827     C                             ' ORDER BY fttpdr, fttddc'
040900121025     C*******   vecchia istruzione di GROUP BY con FATTURA
041000121025     C*******                      + ' and fttnff = 0 group by fttpdr, fttddc'
041100120911     c* cooperative
041200120911     c                   when      §G1tip = 'C'
041300120911     C                   EVAL      WrkSqlCmd
041400120911     C                             =
041500120911     C                             'SELECT cttpdr, cttddc,'
041600120912     C                             +
041700130307     C                             ' sum( case when '
041800120912     C                             +
041900130307     C                             ' cTTfsr =''S'' and cttnff = 0 then '
042000120912     C                             +
042100120912     C                             ' cttitc+cttitma+cttitmp '
042200120912     C                             +
042300120912     C                             ' else 0 end ) as firmato, '
042400120912     C                             +
042500130307     C                             ' sum( case when '
042600120912     C                             +
042700130307     C                             ' cTTFSR ='' '' then '
042800120912     C                             +
042900120912     C                             ' cttitc+cttitma+cttitmp '
043000120912     C                             +
043100120912     C                             ' else 0 end ) as dafirmare, '
043200120912     C                             +
043300130307     C                             ' sum( case when '
043400120912     C                             +
043500130307     C                             ' cTTFSR ='' '' then '
043600120912     C                             +
043700120912     C                             ' cttitma+cttitmp '
043800120912     C                             +
043900120912     C                             ' else 0 end ) as retdafir '
044000120912     C                   IF        §G1tsr <>'S'
044100120911     C                   EVAL      WrkSqlCmd
044200120911     C                             =
044300120911     C                             %TRIMR(WrkSqlCmd)
044400120911     C                             +
044500120911     C                             ', ctttsr '
044600120911     C                   ENDIF
044700120911     C                   EVAL      WrkSqlCmd
044800120911     C                             =
044900120911     C                             %TRIMR(WrkSqlCmd)
045000120911     C                             +
045100120911     C                             ' from fictt06l where cttsoc = '
045200120911     C                             +
045300120911     C                             '''' + §G1soc  + ''''
045400120911     C                             +
045500120911     C                             ' AND '
045600120911     C                             +
045700120911     C                             ' cttcdf between '
045800120911     C                             +
045900120911     C                             '''' + §G1pd1 + ''''
046000120911     C                             +
046100120911     C                             ' AND '
046200120911     C                             +
046300120911     C                             '''' + §G1pd2 + ''''
046400120911     C                             +
046500120911     C                             ' AND '
046600120911     C                             +
046700120911     C                             ' cttfvl = ''C'''
046800120911     C                             +
046900120911     C                             ' AND '
047000120911     C                             +
047100120911     C                             ' cttddc BETWEEN '
047200120911     C                             + %editc(§G1dti:'X')
047300120911     C                             +
047400120911     C                             ' AND ' + %editc(§g1dtf:'X')
047500120911     C                             +
047600120911     C                             ' AND (cttitma <> 0 or cttitmp <> 0) '
047700120911     C                             +
047800121025     C                             ' group by cttpdr, cttddc '
047900121025     C*******   vecchia istruzione di GROUP BY con FATTURA
048000121025     C*******                      + ' and cttnff = 0  group by cttpdr, cttddc '
048100120912     C                   IF        §G1tsr <>'S'
048200120911     C                   EVAL      WrkSqlCmd
048300120911     C                             =
048400120911     C                             %TRIMR(WrkSqlCmd)
048500120911     C                             +
048600120911     C                             ', ctttsr'
048700120911     C                   ENDIF
048800120911     C                   EVAL      WrkSqlCmd
048900120911     C                             =
049000120911     C                             %TRIMR(WrkSqlCmd)
049100120911     C                             +
049200120911     C                             ' ORDER BY cttpdr, cttddc '
049300120912     C                   IF        §G1tsr <>'S'
049400120911     C                   EVAL      WrkSqlCmd
049500120911     C                             =
049600120911     C                             %TRIMR(WrkSqlCmd)
049700120911     C                             +
049800120911     C                             ', ctttsr '
049900120911     C                   ENDIF
050000030827     c                   endsl
050100030827     C                   ENDSR
