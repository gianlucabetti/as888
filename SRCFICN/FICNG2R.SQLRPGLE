000100120910     H/TITLE  gestione firma nei conteggi
000200121122     H dftactgrp(*no) actgrp(*caller)
000300951009?     *--------------------------------------------------------------*
000400120910      * ficng2R                                                      *
000500951009      *     *----------------------------------------------*         *
000600120910      *         gestione firma conteggi                              *
000700951009      *     *---------------------------------------------*          *
000800951009      *                                                              *
000900951009?     *--------------------------------------------------------------*
001000980916      * KA - SELEZIONA TUTTO                                        *
001100951009      * KC - FINE LAVORO                                             *
001200951009      * KF - CONFERMA                                                *
001300951009      * KL - RITORNO                                                 *
001400951009?     *--------------------------------------------------------------*
001500951009      * 20 - SFLDSP                                                  *
001600951009      * 21 - SFLDSPCTL/CLEAR                                         *
001700951009      * 22 - SFLNXTCHG                                               *
001800951009      * 28 - Visualizzazione messaggio di errore                     *
001900951011      * 29 - READC su subfile                                        *
002000020404      * 30 - READ e CHAIN su fiftt01L                                *
002100951011      * 32 - Indicatore di comodo per READ/CHAIN/SCAN/LOKUP          *
002200951009?     *--------------------------------------------------------------*
002300120911     Fansog01l  IF   E           K DISK
002400120911     Fanfrn01l  IF   E           K DISK
002500120911     Fanrco01l  IF   E           K DISK
002600120911     Ffiftt22L  iF   E           K DISK    rename(fiftt000:fiftt22) prefix(£)
002700120911     Ffiftt06L  UF   E           K DISK
002800120911     Ffictt06L  UF   E           K DISK
002900120911     Ffiatt06L  UF   E           K DISK
003000120911     Ffifre02l  uf   E           K DISK
003100120913     Ffifir00f  o    E           K DISK
003200080924     Ftntbe01l  if   E           K DISK
003300120912     Ffiapd01l  if   E           K DISK
003400030213     Ftabel00f  if   E           K DISK
003500020405     F*--------
003600120910     Fficng2D   CF   E             WORKSTN
003700980911     F                                     SFILE(y350S01:NRR)
003800951009     D*---------------------------------------------------------------*
003900951009     D* SCHIERA
004000951009     D*---------------------------------------------------------------*
004100951011      * Schiere per errori e stampa
004200120921     D ERR             S             70    DIM(6) CTDATA PERRCD(1)              Errori
004300020411     D* Reperimento nome PGM
004400020411     D STATUS         SDS           333
004500020411     D  nompgm           *PROC
004600020411     D*---------------------------------------------------------------*
004700020411     D* CAMPI INTERNI
004800020411     D*---------------------------------------------------------------*
004900120913     D ddatiute      e ds
005000120911     D azuteds       e ds                  extname(AZUTE00F)
005100120911     D tibs34ds      E DS                  inz
005200020411     D dataiso         s               d   datfmt(*iso)
005300020411     D dataeur         s               d   datfmt(*eur)
005400030620     d noconf          s              1    inz
005500120911     d compdrda        s                   like(fttpdr)
005600120911     d compdra         s                   like(fttpdr)
005700120912     d savtsr          s                   like(§g1tsr)
005800951009?     *--------------------------------------------------------------*
005900951009?     *  DS                                                          *
006000951009?     *--------------------------------------------------------------*
006100120913     d comtip          s                   like(firtip)
006200120913     d compdr          s                   like(firpdr)
006300120913     d comddc          s                   like(firddc)
006400120914     d comtsr          s                   like(firtsr)
006500120911     D ficnB30DS     E DS
006600120913     D fifirds       e DS                  extname(fifir00f)
006700120913     D dftt01        e DS
006800120911     D datt01        e DS
006900030213     D ds1z          e ds                  inz
007000040914     D dBLC          e ds                  inz
007100040914     D dBLD          e ds                  inz
007200040914     D dBLO          e ds                  inz
007300951009     D KPJBA         E DS
007400020514     D dtlz01        E DS
007500020411     d* parametri ricevuti
007600120911     D ficng1ds      E DS
007700120911     D salds         E DS                  extname(ficng1ds) prefix(£)
007800020411     c* SQL
007900020405     D WrkSqlCmd       S           1024
008000030827     c* sfl 1 aut.
008100020405     D fiftt           DS
008200120911     d socs                                like(fttsoc)
008300120911     d cdfs                                like(fttcdf)
008400120911     d firmatof                            like(ftttim)
008500120911     d dafirmaref                          like(ftttim)
008600120911     d retdafirf                           like(ftttim)
008700030827     c* sfl 1 coop accorpate
008800030128     D fictt           DS
008900120911     d socsc                               like(cttsoc)
009000120911     d cdfsc                               like(cttcdf)
009100120911     d firmatoc                            like(cttitc)
009200120911     d dafirmarec                          like(cttitc)
009300120911     d retdafirc                           like(cttitc)
009400030827     c* sfl 1 coop non accorpate
009500030128     D fictttsr        DS
009600120911     d socsc1                              like(cttsoc)
009700120911     d cdfsc1                              like(cttcdf)
009800120911     d firmato1                            like(cttitc)
009900120911     d dafirmare1                          like(cttitc)
010000120911     d retdafir1                           like(cttitc)
010100030128     d tsrsc1                              like(ctttsr)
010200030827     c* sfl 1 aff/defl
010300030827     D fiatt           DS
010400120911     d socsa                               like(attsoc)
010500120911     d cdfsa                               like(attcdf)
010600120911     d firmatoa                            like(attimpp)
010700120911     d dafirmarea                          like(attimpp)
010800951009?     *--------------------------------------------------------------*
010900951009?     *  CICLO PRINCIPALE                                            *
011000951009?     *--------------------------------------------------------------*
011100951009      *  Impostazione parametri prima videata
011200951010     C                   EXSR      CARSFL
011300951009      *  Loop gestione videata
011400951009     C     WFINE         DOUEQ     'S'
011500000426     C     WTPVID        CASEQ     '1'           GESS01                         sfl pieno
011600000426     C     WTPVID        CASEQ     '2'           GESD02                         sfl vuoto
011700951009     C                   END
011800951012     C                   END
011900980312     C*
012000120911     C                   MOVEL(p)  ficng1ds      KPJBU
012100951009     C                   SETON                                        LR
012200951009?     *--------------------------------------------------------------*
012300951012?     *  CARSFL: CARICAMENNTO SUBFILE                                *
012400951009?     *--------------------------------------------------------------*
012500951012     C     CARSFL        BEGSR
012600951009      *
012700951009      *  Pulisco dati SFL
012800020405     C                   SETOFF                                       2120
012900980911     C                   WRITE     y350C01
013000951009     C                   SETON                                        2021
013100951009     C                   MOVEL     '1'           WTPVID
013200020415     C                   Z-ADD     0             NRR               6 0
013300120910     c* questo sql mi restituisce per ogni fornitore
013400120910     c* la sommatoria dei conteggi totali e da firmare
013500020412     c*
013600120910     c* Lettura fiftt00f/fictt00f/fiatt00f
013700020405     C/EXEC SQL
013800020405     C+ PREPARE S1 FROM :WrkSqlCmd
013900020405     C/END-EXEC
014000020405
014100020405     C/EXEC SQL
014200020405     C+ DECLARE A1 CURSOR FOR S1
014300020405     C/END-EXEC
014400020405
014500020405     C/EXEC SQL
014600020405     C+ OPEN A1
014700020405     C/END-EXEC
014800020405
014900020405     C                   DOU       SqlCod <> 0
015000030827     C                   SELECT
015100031121     c* autotrasportatori
015200120911     c                   when      §G1tip = 'A'
015300020405     C/EXEC SQL
015400020405     C+ FETCH NEXT FROM A1 INTO :fiftt
015500020405     C/END-EXEC
015600030128     c* cooperative      senza tipo servizio
015700120911     c                   when      §G1tip = 'C'
015800120911     c                   if        §G1tsr = 'S'
015900030128     C/EXEC SQL
016000030128     C+ FETCH NEXT FROM A1 INTO :fictt
016100030128     C/END-EXEC
016200030128     c                   else
016300030128     C/EXEC SQL
016400030128     C+ FETCH NEXT FROM A1 INTO :fictttsr
016500030128     C/END-EXEC
016600030128     c* cooperative      con tipo servizio
016700030128     c                   end
016800030827     c* aff/defl.
016900120911     c                   when      §G1tip = 'D'
017000030827     C/EXEC SQL
017100030827     C+ FETCH NEXT FROM A1 INTO :fiatt
017200030827     C/END-EXEC
017300030827     c                   endsl
017400020405     c*
017500020405     C                   SELECT
017600020405     C                   WHEN      SqlCod = 0
017700951010     C                   EXSR      CARTRN
017800020405     C                   WHEN      SqlCod = 100
017900020405     C                   WHEN      SqlCod <> 0
018000020417      * Forzo la stampa del JOBLOG.
018100020417     C                   CALL      'X66CHGJOB'
018200020405     C                   ENDSL
018300020404     C                   ENDDO
018400020417     C/EXEC SQL
018500020417     C+ CLOSE A1
018600020417     C/END-EXEC
018700020405      *  Imposto dati SFLCTL
018800020405     C                   EXSR      INZ01
018900951009      *  Se non ho dati da caricare emetto videata vuota
019000951009     C     NRR           IFEQ      0
019100020405     C                   MOVEL     '2'           WTPVID
019200951009     C                   END
019300951009     C*
019400951009     C                   ENDSR
019500951013?     *--------------------------------------------------------------*
019600020415?     *  INZ01: Inizializzazione videata                             *
019700951013?     *--------------------------------------------------------------*
019800951013     C     INZ01         BEGSR
019900951013      *
020000000426      *  Inizializzo testata sfl in base a quanto
020100000426      *  passato dal programma chiamante
020200951013     C                   MOVEL     '1'           WTPVID
020300951016     C                   MOVEL     NOMPGM        V1CPGM
020400120911     C                   MOVEL     §G1DP1        V1DPDR
020500120911     C                   move      §G1PD1        V1CPDR
020600120911     C                   MOVEL     §G1DP2        V2DPDR
020700120911     C                   move      §G1PD2        V2CPDR
020800120911     C                   if        §g1dtf > 0
020900120911     C                   movel     §g1dtf        dataiso
021000000911     C                   else
021100000911     C                   move      '0001-01-01'  dataiso
021200000911     C                   end
021300980911     C                   MOVEL     dataiso       dataeur
021400120910     C                   movel     dataeur       V1CDtf
021500120912     c* controllo se esistono dei conteggi non confermati in tal cas
021600120912     c* segnalo l'anmalia ma non blocco
021700120912     c                   eval      noconf = ' '
021800120912     c                   clear                   compdrda
021900120912     c                   eval      compdra = 9999999
022000120912     c                   movel     §G1fgs        compdrda
022100120912     c                   movel     §G1fgs        compdra
022200120912     c                   select
022300120912     c                   when      §G1tip = 'A'
022400120912     c                   exsr      ctrftt
022500120912     c                   when      §G1tip = 'C'
022600120912     c                   exsr      ctrctt
022700120912     c                   when      §G1tip = 'D'
022800120912     c                   exsr      ctratt
022900120912     c                   endsl
023000120912     c                   if        noconf = '1'
023100120912     c                   seton                                        28
023200120912     C                   MOVEL     ERR(3)        $MSG
023300120912     c                   end
023400951013      *
023500951013     C                   ENDSR
023600951010?     *--------------------------------------------------------------*
023700020405?     *  CARTRN: Gestione caricamento dati                           *
023800951010?     *--------------------------------------------------------------*
023900951010     C     CARTRN        BEGSR
024000030827     c                   select
024100031121     c* autotrasportatore
024200120911     c                   when      §G1tip = 'A'
024300120911     C                   MOVE      socs          V1Csoc
024400120911     C                   MOVE      cdfs          V1Cfor
024500120912     c                   if        §g1fir = 'S' and
024600120912     c                             dafirmaref = 0 and retdafirf = 0
024700120912     c                   leavesr
024800120912     c                   end
024900120911     C                   Z-ADD     dafirmaref    v1cicof
025000120911     C                   Z-ADD     retdafirf     v1cretf
025100120911     C                   Z-ADD     firmatof      v1cico
025200030128     c* cooperativa  accorpam.
025300120911     c                   when      §G1tip = 'C'
025400120911     C                   if        §G1tsr = 'S'
025500120912     c                   if        §g1fir = 'S' and
025600120912     c                             dafirmarec = 0 and retdafirc = 0
025700120912     c                   leavesr
025800120912     c                   end
025900120911     C                   Z-ADD     dafirmarec    v1cicof
026000120911     C                   Z-ADD     retdafirc     v1cretf
026100120911     C                   Z-ADD     firmatoc      v1cico
026200030129     C                   MOVE      cdfsc         V1Cfor
026300120911     C                   MOVE      socsc         V1Csoc
026400120910     c                   clear                   v1ctsr
026500120910     c                   clear                   v1dtsr
026600030128     c                   else
026700030128     c* cooperativa  no accorpam.
026800120912     c                   if        §g1fir = 'S' and
026900120912     c                             dafirmare1 = 0 and retdafir1 = 0
027000120912     c                   leavesr
027100120912     c                   end
027200120911     C                   Z-ADD     dafirmare1    v1cicof
027300120911     C                   Z-ADD     retdafir1     v1cretf
027400120911     C                   Z-ADD     firmato1      v1cico
027500030129     C                   MOVE      cdfsc1        V1Cfor
027600120911     C                   MOVE      socsc1        V1Csoc
027700030128     C                   MOVEL     tsrsc1        V1Ctsr
027800030213     C                   MOVEL(p)  tsrsc1        tblkey
027900030213     c* decodifico tipo servizio
028000030213     c     ktab          chain     tabel00f
028100030213     c                   if        %found(tabel00f)
028200030213     c                   movel     tbluni        ds1z
028300030213     c                   movel     §1zdes        v1dtsr
028400030213     c                   else
028500030213     c                   clear                   v1dtsr
028600030213     c                   end
028700030128     c                   end
028800030827     c* aff/defl.
028900120911     c                   when      §G1tip = 'D'
029000120912     c                   if        §g1fir = 'S' and
029100120913     c                             dafirmarea = 0
029200120912     c                   leavesr
029300120912     c                   end
029400120911     C                   Z-ADD     dafirmarea    v1cicof
029500120911     C                   Z-ADD     0             v1cretf
029600120911     C                   Z-ADD     firmatoa      v1cico
029700120911     C                   MOVE      cdfsa         V1Cfor
029800120911     C                   MOVE      socsa         V1Csoc
029900030827     c                   endsl
030000951010     C                   MOVEL     *BLANKS       V1CSCE
030100021015     c* 23 ind. che protegge il campo di scelta per i fornitori senza p.IVA
030200120816     c*    oppure i fornitori di cui non è specificata la tipologia
030300021015     c                   setoff                                       23
030400120911     c                   movel     §g1fgs        comfgs            3
030500020405     C     Kfrn          CHAIN     anfrn01l                           30
030600120910     C  N30Krco          CHAIN     anrco01l                           30
030700120911     c                   if        not *in30 and
030800120911     c                             %subst(rcounita: 1: 3) <> comfgs
030900120910     c                   seton                                        30
031000120910     c                   end
031100120911     c                   if        not *in30 and §G1tip = 'A'
031200120910     C                   if        rcoctgan02 <> 'N   ' and
031300120910     C                             rcoctgan02 <> 'I   ' and
031400120910     C                             rcoctgan02 <> 'M   '
031500120910      * tipologia fornitore se non presente
031600120910      * modificato prima accendeva il 30 per non scrivere la riga ora il 23 per proteggere scelta
031700120910     C                   SETON                                        23
031800120910     c                   end
031900120910     c                   end
032000120910     C  N30rcosogg       CHAIN     ansog01l                           30
032100020514     c*
032200020423     c                   if        not *in30
032300120911      * se non trovata tipologia il 23 è già acceso e do errore per la tipologia
032400120911      * se spento verifica la presenza della partita iva
032500120911     c                   if        *in23
032600120911     C                   SETON                                        28
032700120911     c                   move      '1'           ind23
032800120911     C                   MOVEL     ERR(5)        $MSG
032900120911     c                   else
033000120911     c     sogpartiva    comp      *blanks                                23
033100120911     c                   if        *in23
033200120911     C                   SETON                                        28
033300120911     c                   move      '1'           ind23
033400120911     C                   MOVEL     ERR(2)        $MSG
033500120911     c                   else
033600120911     c                   move      '0'           ind23
033700120911     c                   end
033800120911     c                   endif
033900020405     C                   MOVEL     sogdes        v1dksc
034000030827     c                   select
034100120911     c                   when      §G1tip = 'A'
034200031121     C                   MOVEL     rcoctgan02    v1tipf
034300120911     c                   when      §G1tip = 'C'
034400120911     c                   if        §G1tsr = 'N'
034500030203     c                   movel     tsrsc1        v1tipf
034600030203     c                   else
034700030203     c                   movel     'X'           v1tipf
034800030203     c                   end
034900120911     c                   when      §G1tip = 'D'
035000030827     C                   MOVEL     'D'           v1tipf
035100030827     c                   endsl
035200020408     c*
035300020405     C                   ADD       1             NRR
035400020405     C                   WRITE     y350S01
035500020423     c                   end
035600951010      *
035700951010     C                   ENDSR
035800951009?     *--------------------------------------------------------------*
035900020405?     *  srcmd   Gestione comandi                                    *
036000951009?     *--------------------------------------------------------------*
036100020405     C     srcmd         BEGSR
036200120911     c                   clear                   §G1cmd
036300020405      *  Fine Lavoro
036400020405     C     *INKC         IFEQ      '1'
036500020405     C                   MOVEL     'S'           WFINE
036600120911     C                   MOVEL     '03'          §G1CMD
036700020405     C                   END
036800020405      *  Ritorno
036900020405     C     *INKL         IFEQ      '1'
037000020405     C                   MOVEL     'S'           WFINE
037100120911     C                   MOVEL     '12'          §G1CMD
037200020405     C                   END
037300951010      *
037400020405     C                   ENDSR
037500020405?     *--------------------------------------------------------------*
037600020405?     *  GESS01: Gestione prima videata                              *
037700020405?     *--------------------------------------------------------------*
037800020405     C     GESS01        BEGSR
037900020405      *
038000980914     C                   WRITE     y350T01
038100980914     C                   EXFMT     y350C01
038200020405     c* gestione comandi
038300020405     c                   exsr      srcmd
038400120911     c                   if        §G1cmd <> *blanks
038500020415     c                   goto      finvd1
038600020415     c                   end
038700951009      *  Controlli
038800951009     C                   EXSR      CTR01
038900020405     C   28              GOTO      FINVD1
039000020408      *  CONFERMA
039100951013     C     *INKF         IFEQ      '1'
039200120911      * aggiornamenti
039300120911     C                   EXSR      aggio
039400951013      *  Se quando esco dal programma di prima nota è tutto O.K
039500980914     C                   MOVEL     'S'           WFINE
039600120911     C                   MOVEL     '06'          §G1CMD
039700980911     C                   END                                                    V3CTOT = y35FAT
039800951009      *
039900951009     C     FINVD1        ENDSR
040000951010?     *--------------------------------------------------------------*
040100951010?     *  GESD02: Gestione seconda videata                            *
040200951010?     *--------------------------------------------------------------*
040300951010     C     GESD02        BEGSR
040400951010      *
040500980914     C                   EXFMT     y350D02
040600020405     c* gestione comandi
040700020405     c                   exsr      srcmd
040800951010      *
040900951010     C                   ENDSR
041000951009?     *--------------------------------------------------------------*
041100951009?     *  CTR01: Controlli prima videata                              *
041200951009?     *--------------------------------------------------------------*
041300951009     C     CTR01         BEGSR
041400021016     c                   setoff                                       28
041500951009      *
041600020405     C                   Z-ADD     0             V1CITT
041700120912     C                   Z-ADD     0             V1CITTf
041800120912     C                   Z-ADD     0             V1Ctref
041900951013     C                   MOVEL     'N'           WSELTR            1
042000020610     C                   MOVEL     *BLANKS       $MSG
042100020415      *  Controllo se devo selezionare tutte i fornitori
042200951010     C     *INKA         IFEQ      '1'
042300951010     C                   Z-ADD     1             NRR
042400980911     C     NRR           CHAIN     y350S01                            29
042500951010     C     *IN29         DOWEQ     '0'
042600021016     c* non scelgo i record con ind23 = 1 xchè manca p.iva
042700120913     c                   if        ind23 = '0' and v1cicof <> 0
042800951013     C                   MOVEL     'S'           WSELTR            1
042900951010     C                   MOVEL     '1'           V1CSCE
043000951010     C                   ADD       V1CICO        V1CITT
043100120910     C                   ADD       V1CICOf       V1CITTf
043200120910     C                   ADD       V1Cretf       V1Ctref
043300951010     C                   SETON                                        22
043400021016     c                   movea     ind23         *in(23)
043500980911     C                   UPDATE    y350S01
043600951010     C                   SETOFF                                       22
043700021016     c                   end
043800951010     C                   ADD       1             NRR
043900980911     C     NRR           CHAIN     y350S01                            29
044000011017     C                   ENDdo
044100951010     C                   ELSE
044200951010      *  Solo i selezionati
044300980911     C                   READC     y350S01                                29
044400951010     C     *IN29         DOWEQ     '0'
044500020405     C                   select
044600030128     C                   when      v1csce = 'D'
044700120911     c                   movel     ficng1ds      salds
044800120914     c                   movel     v1csoc        §G1soc
044900120914     c                   movel     v1cfor        §G1pd2
045000120911     c                   movel     v1dksc        §G1dp2
045100120911     c                   movel     v1cfor        §G1pd1
045200120911     c                   movel     v1dksc        §G1dp1
045300020528     c*
045400120911     c                   movel(p)  ficng1ds      kpjbu
045500030827     c* dettaglio aut.
045600120911     C                   if        (§G1TIP='A' or §G1tip='D')
045700120910     c                   call      'FICNG3R'
045800020417     c                   parm                    kpjba
045900030827     C                   END
046000120911     c                   movel     kpjbu         ficng1ds
046100120911     c                   movel     §G1cmd        £§G1cmd
046200120911     c                   movel     salds         ficng1ds
046300020408     c                   movel     *blanks       v1csce
046400020610     C                   SETOFF                                       22
046500021016     c                   movea     ind23         *in(23)
046600020408     C                   UPDATE    y350S01
046700120911     c                   if        §G1cmd = '03'
046800020415     c                   move      *on           *inkc
046900020405     c                   exsr      srcmd
047000020415     c                   goto      finct1
047100020415     c                   end
047200020405     c*
047300120921     C                   when      v1csce = 'V' and v1cretf = 0
047400120921     C                   SETON                                        28
047500120921     C                   MOVEL     ERR(6)        $MSG
047600120921     C                   GOTO      FINCT1
047700120921     c*
047800120911     C                   when      v1csce = 'V' and v1cretf <> 0
047900120911     c                   movel     ficng1ds      salds
048000120914     c                   movel     v1csoc        §G1soc
048100120914     c                   movel     v1cfor        §G1pd2
048200120911     c                   movel     v1dksc        §G1dp2
048300120911     c                   movel     v1cfor        §G1pd1
048400120911     c                   movel     v1dksc        §G1dp1
048500120912     c* per coop se richiesto accorpamento no, passo il tipo servizio
048600120912     c* nel campo
048700120912     c                   eval      savtsr = §g1tsr
048800120912     c                   if        §g1tip = 'C' and §g1tsr = 'N'
048900120912     c                   eval      §g1tsr = v1ctsr
049000120912     c                   end
049100120911     c*
049200120911     c                   movel(p)  ficng1ds      kpjbu
049300120911     c* dettaglio aut.
049400120911     C                   if        (§G1TIP='A' or §G1tip='C')
049500120911     c                   call      'FICNG4R'
049600120911     c                   parm                    kpjba
049700120911     C                   END
049800120911     c                   movel     kpjbu         ficng1ds
049900120912     c                   eval      §g1tsr = savtsr
050000120911     c                   movel     §G1cmd        £§G1cmd
050100120911     c                   movel     salds         ficng1ds
050200120911     c                   movel     *blanks       v1csce
050300120911     C                   SETOFF                                       22
050400120911     c                   movea     ind23         *in(23)
050500120911     C                   UPDATE    y350S01
050600120911     c                   if        §G1cmd = '03'
050700120911     c                   move      *on           *inkc
050800120911     c                   exsr      srcmd
050900120911     c                   goto      finct1
051000120911     c                   end
051100120911     c*
051200020405     C     V1CSCE        wheneq    '1'
051300121207     c                   if        v1cicof = 0 and v1cretf = 0
051400120913     C                   SETON                                        28
051500120913     C                   MOVEL     ERR(4)        $MSG
051600120913     C                   GOTO      FINCT1
051700120913     c                   else
051800020405     C                   MOVEL     'S'           WSELTR            1
051900951010     C                   ADD       V1CICO        V1CITT
052000120910     C                   ADD       V1CICOf       V1CITTf
052100120910     C                   ADD       V1Cretf       V1Ctref
052200120913     c                   end
052300951010     C                   SETON                                        22
052400021016     c                   movea     ind23         *in(23)
052500980911     C                   UPDATE    y350S01
052600951010     C                   SETOFF                                       22
052700020610     C                   OTHER
052800020610     C                   SETOFF                                       22
052900021016     c                   movea     ind23         *in(23)
053000020610     C                   UPDATE    y350S01
053100020405     C                   ENDsl
053200980911     C                   READC     Y350S01                                29
053300011017     C                   ENDdo
053400951010     C                   END
053500020415      *  Nessun fornitore selezionato
053600020408     C     WSELTR        IFeq      'N'
053700020405     C     *INKF         andeq     '1'
053800951013     C                   SETON                                        28
053900020415     C                   MOVEL     ERR(1)        $MSG
054000951013     C                   GOTO      FINCT1
054100951013     C                   END
054200951010      *
054300951010     C     FINCT1        ENDSR
054400951011?     *--------------------------------------------------------------*
054500120911?     *  Aggiornamenti
054600951011?     *--------------------------------------------------------------*
054700120911     C     aggio         BEGSR
054800980914      *
054900020408     C                   READC     y350S01                                29
055000020408     C     *IN29         DOWEQ     '0'
055100020408     C     V1CSCE        IFEQ      '1'
055200121207     C                   if         V1Cicof <> 0 or V1Cretf <> 0
055300120910      * imposto il flag di firma nel file FIFTT00F/FICTT00F/FIATT00F/FIFRE00
055400030827     c                   select
055500120911     c                   when      §G1tip = 'A'
055600031121     C                   EXSR      AGGftt
055700120911     c                   when      §G1tip = 'C'
055800030128     C                   EXSR      AGGctt
055900120911     c                   when      §G1tip = 'D'
056000030827     C                   EXSR      AGGatt
056100030827     c                   endsl
056200030204     c                   end
056300020408      *  Lettura record successivo
056400020610     C                   SETOFF                                       22
056500021016     c                   movea     ind23         *in(23)
056600020408     C                   UPDATE    y350S01
056700020614     c                   end
056800020408     C                   READC     y350S01                                29
056900020409     C                   enddo                                                  *IN29 = 0
057000951011     C                   ENDSR
057100031121?     *--------------------------------------------------------------*
057200120911?     *  AGGftt: Aggiorno autotrasportatori con flag di firmato
057300031121?     *--------------------------------------------------------------*
057400031121     C     AGGftt        BEGSR
057500031121     c* se trovata registrazione
057600031121     C     Kftt          setll     fiftt06L
057700031121     c                   do        *hival
057800031121     C     Kftt          reade     fiftt06L
057900031121     C                   if        %eof(fiftt06l)
058000031121     c                   leave
058100031121     c                   end
058200031121     c* data conteggio compresa nei limiti richiesti
058300120911     c                   if        fttddc > §g1dtf or fttddc < §G1dti
058400031121     c                   iter
058500031121     c                   end
058600121025      *
058700031121     c* se già fatturato iter
058800121025     c******             if        fttnff <> 0
058900121025     c*******            iter
059000121025     c********           end
059100121025     c********  Avendo attivato la VPO deve saltare il test sul nr.fattura
059200121025     c********  Perchè andiamo a controllare anche dei records che possono
059300121025     c********  essere stati comunque fatturati il mese precedente regolarmente.
059400121025      *
059500120911     c* solo le non firmate
059600120911     c                   movel     fttflr        dftt01
059700120911     c                   if        §fttsta ='S'
059800120911     c                   iter
059900120911     c                   end
060000120911     c*
060100120911     c                   eval      §fttsta ='S'
060200031121     C                   movel(p)  dftt01        fttflr
060300031121     C                   EXCEPT    UPDftt
060400120911     c* se ci sono rettifiche confermo anche FIFRE00F
060500120911     c                   if        ftttim <> 0
060600120911     c                   eval      frepdr = fttpdr
060700120911     c                   eval      freddc = fttddc
060800120911     c                   eval      fretip = 'A'
060900120911     c                   exsr      aggfre
061000120911     c                   end
061100120913     c* scrivo file firma
061200120913     c                   eval      comtip = 'A'
061300120913     c                   eval      compdr =fttpdr
061400120913     c                   eval      comddc =fttddc
061500120914     c                   clear                   comtsr
061600120913     c                   exsr      aggfir
061700031121     C                   enddo
061800031121      *
061900031121     C                   ENDSR
062000030128?     *--------------------------------------------------------------*
062100120911?     *  AGGctt: Aggiorno cooperative con flag di firmato
062200030128?     *--------------------------------------------------------------*
062300030128     C     AGGctt        BEGSR
062400030620     c                   move      'C'           cttfvl
062500030128     c* se trovata registrazione
062600120911     c                   if        §G1tsr = 'S'
062700030128     C     Kctt          setll     fictt06L
062800030128     c                   else
062900030128     C     Kctt1         setll     fictt06L
063000030128     c                   end
063100030128     c                   do        *hival
063200120911     c                   if        §G1tsr = 'S'
063300030128     C     Kctt          reade     fictt06L
063400030128     c                   else
063500030128     C     Kctt1         reade     fictt06L
063600030128     c                   end
063700030128     C                   if        %eof(fictt06l)
063800030128     c                   leave
063900030128     c                   end
064000030128     c* data conteggio compresa nei limiti richiesti
064100120911     c                   if        cttddc > §g1dtf or cttddc < §G1dti
064200030128     c                   iter
064300030128     c                   end
064400121025      *
064500030128     c* se già fatturato iter
064600121025     c*****              if        cttnff <> 0
064700121025     c******             iter
064800121025     c*******            end
064900121025     c********  Avendo attivato la VPO deve saltare il test sul nr.fattura
065000121025     c********  Perchè andiamo a controllare anche dei records che possono
065100121025     c********  essere stati comunque fatturati il mese precedente regolarmente.
065200121025      *
065300120911     c* solo le non firmate
065400120911     c                   if        cttfsr ='S'
065500120911     c                   iter
065600120911     c                   end
065700120911     c                   eval      cttfsr = 'S'
065800030128     C                   EXCEPT    UPDctt
065900120911     c* se ci sono rettifiche confermo anche FIFRE00F
066000120911     c                   if        cttitma <> 0 or cttitmp <> 0
066100120911     c                   eval      frepdr = cttpdr
066200120911     c                   eval      freddc = cttddc
066300120911     c                   eval      fretip = 'C'
066400120911     c                   exsr      aggfre
066500120911     c                   end
066600120913     c* scrivo file firma
066700120913     c                   eval      comtip = 'C'
066800120913     c                   eval      compdr =cttpdr
066900120913     c                   eval      comddc =cttddc
067000120914     c                   eval      comtsr =ctttsr
067100120913     c                   exsr      aggfir
067200030128     C                   enddo
067300030128      *
067400030128     C                   ENDSR
067500030827?     *--------------------------------------------------------------*
067600120911?     *  AGGatt: Aggiorno aff/defl. con flag di firmato
067700030827?     *--------------------------------------------------------------*
067800030827     C     AGGatt        BEGSR
067900030828     c                   eval      attflg = 'C'
068000030827     c* se trovata registrazione
068100030827     C     Katt          setll     fiatt06L
068200030827     c                   do        *hival
068300030828     C     Katt          reade     fiatt06L
068400030827     C                   if        %eof(fiatt06l)
068500030827     c                   leave
068600030827     c                   end
068700030827     c* data conteggio compresa nei limiti richiesti
068800120911     c                   if        attddc > §g1dtf or attddc < §G1dti
068900030827     c                   iter
069000030827     c                   end
069100121025      *
069200030827     c* se già fatturato iter
069300121025     c*****              if        attnff <> 0
069400121025     c******             iter
069500121025     c*******            end
069600121025     c********  Avendo attivato la VPO deve saltare il test sul nr.fattura
069700121025     c********  Perchè andiamo a controllare anche dei records che possono
069800121025     c********  essere stati comunque fatturati il mese precedente regolarmente.
069900121025      *
070000120911     c* solo le non firmate
070100120911     c                   movel     attflr        datt01
070200120911     c                   if        §attsta ='S'
070300120911     c                   iter
070400120911     c                   end
070500120911     c                   eval      §attsta = 'S'
070600120913     c                   eval      attflr = datt01
070700030827     C                   EXCEPT    UPDatt
070800120913     c* scrivo file firma
070900120913     c                   eval      comtip = 'D'
071000120913     c                   eval      compdr =attpdr
071100120913     c                   eval      comddc =attddc
071200120914     c                   clear                   comtsr
071300120913     c                   exsr      aggfir
071400030827     C                   enddo
071500030827      *
071600030827     C                   ENDSR
071700120911?     *--------------------------------------------------------------*
071800120911?     *  AGGfre: Aggiorno rettifiche con flag di firmato
071900120911?     *--------------------------------------------------------------*
072000120911     C     AGGfre        BEGSR
072100120911     c* se trovata registrazione
072200120911     C     Kfre          setll     fifre02L
072300120911     c                   do        *hival
072400120911     C     Kfre          reade     fifre02L
072500120911     C                   if        %eof(fifre02l)
072600120911     c                   leave
072700120911     c                   end
072800120911     c* solo le non firmate
072900120911     c                   if        fredds <> 0
073000120911     c                   iter
073100120911     c                   end
073200120911     c*
073300120911     c                   move      *date         dataiso
073400120911     c                   move      dataiso       fredds
073500120911     C                   EXCEPT    UPDfre
073600120911     C                   enddo
073700120911      *
073800120911     C                   ENDSR
073900120913?     *--------------------------------------------------------------*
074000120913?     *  AGGfir: scrivo il file firma
074100120913?     *--------------------------------------------------------------*
074200120913     C     AGGfir        BEGSR
074300120913     c*
074400120913     c                   clear                   fifirds
074500120913     c                   eval      FIRTRC = 'FIR'
074600120913     c                   eval      FIRTIP = comtip
074700120913     c                   eval      FIRPDR = compdr
074800120913     c                   eval      FIRDDC = comddc
074900120914     c                   eval      FIRtsr = comtsr
075000120913     c                   move      *date         dataiso
075100120913     c                   move      dataiso       firdfi
075200120913     c                   eval      firute = knmus
075300120913     c                   eval      firvid = KNMTD
075400120913     C                   write     fifir000
075500120913      *
075600120913     C                   ENDSR
075700120911?     *--------------------------------------------------------------*
075800120911?     *  ctrftt: Controllo se ho dei conteggi non confermati
075900120911      *          in tal caso segnalo l'anomalia
076000120911?     *--------------------------------------------------------------*
076100120911     C     ctrftt        BEGSR
076200120911     c* se trovata registrazione
076300120911     C     compdrda      setll     fiftt22L
076400120911     c                   do        *hival
076500120911     C                   read      fiftt22L
076600120911     C                   if        %eof(fiftt22l)
076700120911     c                   leave
076800120911     c                   end
076900120911     c* autista
077000120911     c                   if        £fttpdr > compdra
077100120911     c                   leave
077200120911     c                   end
077300120911     c* data conteggio compresa nei limiti richiesti
077400120911     c                   if        £fttddc > §g1dtf or £fttddc < §G1dti
077500120911     c                   iter
077600120911     c                   end
077700120912     c* controllo che non sia fitizio
077800120912     c     kapd          chain     fiapd01l
077900120912     c                   if        %found(fiapd01l) and apdpdd = 'S'
078000120912     c                   iter
078100120912     c                   end
078200120911     c                   eval      noconf = '1'
078300120911     c                   leave
078400120911     C                   enddo
078500120912     c*
078600120911     C                   ENDSR
078700120911?     *--------------------------------------------------------------*
078800120911?     *  ctrctt: Controllo se ho dei conteggi non confermati
078900120911      *          in tal caso segnalo l'anomalia
079000120911?     *--------------------------------------------------------------*
079100120911     C     ctrctt        BEGSR
079200120912     c                   clear                   conta             5 0
079300120912     C/EXEC SQL
079400120912     C+ SELECT count(*)        into :conta
079500120912     C+    FROM fictt00F WHERE cttpdr between :compdrda and :compdra
079600120912     C+    and  cttddc between :§g1dti and :§g1dtf and
079700120912     C+    cttdcv = 0 fetch first row only
079800120912     C/END-EXEC
079900120912     c                   if        conta > 0
080000120912     c                   eval      noconf = '1'
080100120912     c                   end
080200120912     c                   clear                   sqlcod
080300120911      *
080400120911     C                   ENDSR
080500030827?     *--------------------------------------------------------------*
080600030827?     *  ctratt: Controllo se ho dei conteggi non confermati
080700120911      *          in tal caso segnalo l'anomalia
080800030827?     *--------------------------------------------------------------*
080900030827     C     ctratt        BEGSR
081000120912     c                   clear                   conta             5 0
081100120912     C/EXEC SQL
081200120912     C+ SELECT count(*)        into :conta
081300120912     C+    FROM fiatt00F WHERE attpdr between :compdrda and :compdra
081400120912     C+    and  attddc between :§g1dti and :§g1dtf and
081500130221     C+         ATTIMPP > 0,001 and
081600120912     C+    attflg = ' ' fetch first row only
081700120912     C/END-EXEC
081800120912     c                   if        conta > 0
081900120912     c                   eval      noconf = '1'
082000120912     c                   end
082100120912     c                   clear                   sqlcod
082200030827      *
082300030827     C                   ENDSR
082400020527?     *--------------------------------------------------------------*
082500030128?     *  istruz: Operazioni di inizializzazione dati                 *
082600020527?     *--------------------------------------------------------------*
082700030128     C     istruz        BEGSR
082800030827     c                   select
082900031121     c* autotrasportatori
083000120911     c                   when      §G1tip = 'A'
083100031121     C                   EVAL      WrkSqlCmd
083200031121     C                             =
083300120911     C                             'SELECT fttsoc, fttcdf,'
083400031121     C                             +
083500120911     C                             ' sum( case when substr( '
083600120911     C                             +
083700130307     C                             ' FTTFLR, 1, 1) =''S'' and fttNFF = 0 then '
083800031121     C                             +
083900120911     C                             'fttitt+fttita+ftttpe+ftttbn+ftttim+fttmnt '
084000031121     C                             +
084100120911     C                             ' else 0 end ) as firmato, '
084200120911     C                             +
084300120911     C                             ' sum( case when substr( '
084400120911     C                             +
084500120911     C                             ' FTTFLR, 1, 1) ='' '' then '
084600120911     C                             +
084700120911     C                             'fttitt+fttita+ftttpe+ftttbn+ftttim+fttmnt '
084800120911     C                             +
084900120911     C                             ' else 0 end ) as dafirmare, '
085000120911     C                             +
085100120911     C                             ' sum( case when substr( '
085200120911     C                             +
085300120911     C                             ' FTTFLR, 1, 1) ='' '' then '
085400130307     C                             +
085500130307     C                             'ftttim '
085600130307     C                             +
085700130307     C                             ' else 0 end ) as retdafir '
085800130307     C                             +
085900120914     C                             ' from fiftt06l where '
086000120914     C                   IF        §G1soc <>' '
086100120914     C                   EVAL      WrkSqlCmd
086200120914     C                             =
086300120914     C                             %TRIMR(WrkSqlCmd)
086400120914     C                             +
086500120914     C                             ' fttsoc = '
086600031121     C                             +
086700120911     C                             '''' + §G1soc  + ''''
086800031121     C                             +
086900031121     C                             ' AND '
087000120914     c                   end
087100120914     C                   EVAL      WrkSqlCmd
087200120914     C                             =
087300120914     C                             %TRIMR(WrkSqlCmd)
087400120914     C                             +
087500031121     C                             ' fttcdf between '
087600031121     C                             +
087700120911     C                             '''' + §G1pd1 + ''''
087800031121     C                             +
087900031121     C                             ' AND '
088000031121     C                             +
088100120911     C                             '''' + §G1pd2 + ''''
088200031121     C                             +
088300031121     C                             ' AND '
088400031121     C                             +
088500031121     C                             ' ftttsr = '' '''
088600031121     C                             +
088700031121     C                             ' AND '
088800031121     C                             +
088900031121     C                             ' fttfvl = ''C'''
089000031121     C                             +
089100031121     C                             ' AND '
089200031121     C                             +
089300031121     C                             ' Fttddc BETWEEN '
089400120911     C                             + %editc(§G1dti:'X')
089500031121     C                             +
089600120911     C                             ' AND ' + %editc(§g1dtf:'X')
089700031121     C                             +
089800121025     C                             ' group by fttsoc, fttcdf '
089900031121     c                             +
090000031121     C                             ' ORDER BY fttcdf'
090100121025      *
090200121025     C*******   vecchia istruzione di GROUP BY con FATTURA
090300121025     C*******                      + ' and fttnff = 0  group by fttsoc, fttcdf '
090400030128     c* cooperative
090500120911     c                   when      §G1tip = 'C'
090600030128     C                   EVAL      WrkSqlCmd
090700030128     C                             =
090800120911     C                             'SELECT cttsoc, cttcdf,'
090900120911     C                             +
091000120913     C                             ' sum( case when '
091100120911     C                             +
091200130307     C                             ' cTTfsr =''S'' and cttnff = 0 then '
091300120911     C                             +
091400120911     C                             ' cttitc+cttitma+cttitmp '
091500120911     C                             +
091600120911     C                             ' else 0 end ) as firmato, '
091700120911     C                             +
091800120913     C                             ' sum( case when  '
091900120911     C                             +
092000120913     C                             ' cttfsr ='' '' then '
092100120911     C                             +
092200120911     C                             ' cttitc+cttitma+cttitmp '
092300120911     C                             +
092400120911     C                             ' else 0 end ) as dafirmare, '
092500120911     C                             +
092600120913     C                             ' sum( case when  '
092700120911     C                             +
092800120913     C                             ' cttfsr ='' '' then '
092900120911     C                             +
093000120911     C                             ' cttitma+cttitmp '
093100120911     C                             +
093200120911     C                             ' else 0 end ) as retdafir '
093300130307      *
093400120911     C                   IF        §G1tsr = 'N'
093500030128     C                   EVAL      WrkSqlCmd
093600030128     C                             =
093700030128     C                             %TRIMR(WrkSqlCmd)
093800030128     C                             +
093900030205     C                             ', ctttsr '
094000030128     C                   ENDIF
094100030128     C                   EVAL      WrkSqlCmd
094200030128     C                             =
094300030128     C                             %TRIMR(WrkSqlCmd)
094400030128     C                             +
094500120914     C                             ' from fictt06l where '
094600120914     C                   IF        §G1soc <>' '
094700120914     C                   EVAL      WrkSqlCmd
094800120914     C                             =
094900120914     C                             %TRIMR(WrkSqlCmd)
095000120914     C                             +
095100120914     C                             ' cttsoc = '
095200030128     C                             +
095300120911     C                             '''' + §G1soc  + ''''
095400030128     C                             +
095500030128     C                             ' AND '
095600120914     c                   end
095700120914     C                   EVAL      WrkSqlCmd
095800120914     C                             =
095900120914     C                             %TRIMR(WrkSqlCmd)
096000030128     C                             +
096100030129     C                             ' cttcdf between '
096200030128     C                             +
096300120911     C                             '''' + §G1pd1 + ''''
096400030128     C                             +
096500030129     C                             ' AND '
096600030128     C                             +
096700120911     C                             '''' + §G1pd2 + ''''
096800030128     C                             +
096900030128     C                             ' AND '
097000030128     C                             +
097100030128     C                             ' cttfvl = ''C'''
097200030128     C                             +
097300030128     C                             ' AND '
097400030128     C                             +
097500030128     C                             ' cttddc BETWEEN '
097600120911     C                             + %editc(§G1dti:'X')
097700030129     C                             +
097800120911     C                             ' AND ' + %editc(§g1dtf:'X')
097900030128     C                             +
098000121025     C                             ' group by cttsoc, cttcdf '
098100121025      *
098200121025     C*******   vecchia istruzione di GROUP BY con FATTURA
098300121025     C*******                      + ' and cttnff = 0  group by cttsoc, cttcdf '
098400120911     C                   IF        §G1tsr = 'N'
098500030128     C                   EVAL      WrkSqlCmd
098600030128     C                             =
098700030128     C                             %TRIMR(WrkSqlCmd)
098800030128     C                             +
098900030203     C                             ', ctttsr'
099000030128     C                   ENDIF
099100030128     C                   EVAL      WrkSqlCmd
099200030128     C                             =
099300030128     C                             %TRIMR(WrkSqlCmd)
099400030128     C                             +
099500030203     C                             ' ORDER BY cttcdf '
099600120911     C                   IF        §G1tsr = 'N'
099700030128     C                   EVAL      WrkSqlCmd
099800030128     C                             =
099900030128     C                             %TRIMR(WrkSqlCmd)
100000030128     C                             +
100100030203     C                             ', ctttsr '
100200030128     C                   ENDIF
100300030827     c* aff/defl.
100400120911     c                   when      §G1tip = 'D'
100500030827     C                   EVAL      WrkSqlCmd
100600030827     C                             =
100700120911     C                             'SELECT attsoc, attcdf,'
100800120911     C                             +
100900120911     C                             ' sum( case when substr( '
101000120911     C                             +
101100130307     C                             ' aTTFLR, 1, 1) =''S'' and ATTnff = 0 then '
101200120911     C                             +
101300130307     C                             ' attimpp '
101400130307     C                             +
101500130307     C                             ' else 0 end ) as firmato, '
101600120911     C                             +
101700120911     C                             ' sum( case when substr( '
101800120911     C                             +
101900120911     C                             ' aTTFLR, 1, 1) ='' '' then '
102000120911     C                             +
102100130307     C                             ' attimpp '
102200030827     C                             +
102300130307     C                             ' else 0 end ) as dafirmare '
102400130307     C                             +
102500120914     C                             ' from fiatt06l where'
102600120914     C                   IF        §G1soc <>' '
102700120914     C                   EVAL      WrkSqlCmd
102800120914     C                             =
102900120914     C                             %TRIMR(WrkSqlCmd)
103000120914     C                             +
103100120914     C                             ' attsoc = '
103200030827     C                             +
103300120911     C                             '''' + §G1soc  + ''''
103400030827     C                             +
103500030827     C                             ' AND '
103600120914     c                   end
103700120914     C                   EVAL      WrkSqlCmd
103800120914     C                             =
103900120914     C                             %TRIMR(WrkSqlCmd)
104000030827     C                             +
104100030827     C                             ' attcdf between '
104200030827     C                             +
104300120911     C                             '''' + §G1pd1 + ''''
104400030827     C                             +
104500030827     C                             ' AND '
104600030827     C                             +
104700120911     C                             '''' + §G1pd2 + ''''
104800030827     C                             +
104900030827     C                             ' AND '
105000030827     C                             +
105100030827     C                             ' attdco <> 0'
105200030827     C                             +
105300030827     C                             ' AND '
105400030827     C                             +
105500030827     C                             ' attddc BETWEEN '
105600120911     C                             + %editc(§G1dti:'X')
105700030827     C                             +
105800120911     C                             ' AND ' + %editc(§g1dtf:'X')
105900030827     C                             +
106000130221     C                             ' AND ATTIMPP > 0,001 '
106100121120     C                             +
106200121025     C                             ' group by attsoc, attcdf '
106300030827     c                             +
106400030827     C                             ' ORDER BY attcdf'
106500121025     C*******   vecchia istruzione di GROUP BY con FATTURA
106600121025     C********                     + ' and attnff = 0  group by attsoc, attcdf '
106700030827     c                   endsl
106800030128     C                   ENDSR
106900030416?     *--------------------------------------------------------------*
107000030416?     *  *INZSR: Operazioni di inizializzazione dati                 *
107100030416?     *--------------------------------------------------------------*
107200030416     C     *INZSR        BEGSR
107300020527      *
107400951011     C     *ENTRY        PLIST
107500951011     C                   PARM                    KPJBA
107600120911     C                   MOVEL     KPJBU         ficng1ds
107700120911     c     *dtaara       define    §azute        azuteds
107800120911     c     *dtaara       define    §datiute      ddatiute
107900120911     C                   in(E)     *dtaara
108000120911     C                   IF        %Error  or  RSUT = *blanks
108100120911     C                   call      'TIBS34R'
108200120911     C                   parm                    Tibs34Ds
108300120911     C                   in        *dtaara
108400120911     c                   ENDIF
108500120911     C                   movel     '000400'      forita            6
108600020417      *  Definizione chiave di accesso
108700020423     C     Kftt          KLIST
108800120911     C                   KFLD                    v1csoc
108900020423     C                   KFLD                    v1cfor
109000020417     C                   KFLD                    ftttsr
109100020417     C                   KFLD                    fttfvl
109200020417     c                   move      ' '           ftttsr
109300020417     c                   move      'C'           fttfvl
109400030128     C     Kctt          KLIST
109500120911     C                   KFLD                    v1csoc
109600030128     C                   KFLD                    v1cfor
109700030128     C                   KFLD                    cttfvl
109800030128     C     Kctt1         KLIST
109900120911     C                   KFLD                    v1csoc
110000030128     C                   KFLD                    v1cfor
110100030128     C                   KFLD                    cttfvl
110200030128     C                   KFLD                    v1ctsr
110300030827     C     Katt          KLIST
110400120911     C                   KFLD                    v1csoc
110500030827     C                   KFLD                    v1cfor
110600030828     C                   KFLD                    attflg
110700120912     C     Kapd          KLIST
110800120912     C                   KFLD                    apdtip
110900120912     C                   KFLD                    £fttpdr
111000120912     c                   eval      apdtip = 'A'
111100120911     C     Kfre          KLIST
111200120911     C                   KFLD                    fretip
111300120911     C                   KFLD                    frepdr
111400120911     C                   KFLD                    freddc
111500031121     C     Ktbe1         KLIST
111600031121     C                   KFLD                    tbecod
111700031121     C                   KFLD                    tbeke1
111800121023      *
111900121023     C     Ktbe          KLIST
112000121023     C                   KFLD                    tbecod
112100121023     C                   KFLD                    tbeke1
112200121023     C                   KFLD                    tbeke2
112300121023     C*
112400030213     C     Ktab          KLIST
112500030213     C                   KFLD                    tblkut
112600030213     C                   KFLD                    tblcod
112700030213     C                   KFLD                    tblkey
112800030213     c                   eval      tblkut = 1
112900030213     c                   eval      tblcod = '1Z'
113000020417     C     Kfrn          KLIST
113100120914     C                   KFLD                    v1csoc
113200030129     C                   KFLD                    v1cfor
113300020417     C     Krco          KLIST
113400120914     C                   KFLD                    v1csoc
113500020417     C                   KFLD                    forita
113600030129     C                   KFLD                    v1cfor
113700121023     C*
113800031121     c* MI COMPONGO L'ISTRUZIONE SQL
113900120912     c     §g1fir        comp      'S'                                    62
114000030128     c                   exsr      istruz
114100030827     c                   select
114200030128     c* decodifica a video
114300120911     c                   when      §G1tip = 'A'
114400030128     c                   eval      dectip = 'AUTOTRASPORTATORI'
114500040914     c                   clear                   dblc
114600040914     c                   movel(p)  '1'           tbeke1
114700040914     c                   movel     'BLC'         tbecod
114800040914     c     ktbe1         chain     tntbe01l
114900040914     c                   if        %found(tntbe01l)
115000040914     c                   movel     tbeuni        dblc
115100040914     c                   end
115200120911     c                   when      §G1tip = 'C'
115300120926     c                   seton                                        60
115400030128     c                   eval      dectip = '   COOPERATIVE   '
115500030704     C                   IF        NOT %OPEN(FICTT06L)
115600030620     c                   open      fiCTT06l
115700030704     C                   END
115800040914     c                   clear                   dblo
115900040914     c                   movel(p)  '1'           tbeke1
116000040914     c                   movel     'BLO'         tbecod
116100040914     c     ktbe1         chain     tntbe01l
116200040914     c                   if        %found(tntbe01l)
116300040914     c                   movel     tbeuni        dblo
116400040914     c                   end
116500120911     c                   when      §G1tip = 'D'
116600120911     c                   seton                                        61
116700030827     c                   eval      dectip = 'AFFLUEN./DEFLUEN.'
116800030827     C                   IF        NOT %OPEN(FIATT06L)
116900030827     c                   open      fiaTT06l
117000030827     C                   END
117100040914     c                   clear                   dbld
117200040914     c                   movel(p)  '1'           tbeke1
117300040914     c                   movel     'BLD'         tbecod
117400040914     c     ktbe1         chain     tntbe01l
117500040914     c                   if        %found(tntbe01l)
117600040914     c                   movel     tbeuni        dbld
117700040914     c                   end
117800030827     c                   endsl
117900020408      *  Inizializzo variabili
118000020408     C                   MOVEL     '1'           WTPVID            1
118100020408     C                   MOVEL     'N'           WFINE             1
118200951012     C                   ENDSR
118300951012     O*-----------------------------------------------------*
118400031121     Ofiftt000  E            UPDftt
118500031121     O                       fttflr
118600030128     Ofictt000  E            UPDctt
118700120911     O                       cttfsr
118800030827     Ofiatt000  E            UPDatt
118900120911     O                       attflr
119000120911     Ofifre000  E            UPDfre
119100120911     O                       fredds
119200951012      *--------------------------------------------------------------*
119300951012** ERR
119400020415Selezionare almeno un fornitore                                       1
119500120911Esistono fornitori senza partita IVA                                  2
119600030620ATTENZIONE!! Esistono valorizzazioni non confermate                   3
119700120913Scelta errata, non c'è nulla da firmare                               4
119800120816Manca tipologia fornitore in una o più anagrafiche                    5
119900120921Non ci sono variazioni !!!!                                           6
