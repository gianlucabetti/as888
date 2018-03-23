000100000713     H DECEDIT('0,') DATEDIT(*YMD) DATFMT(*ISO)
000200000712      * FNLV75R ******************************************************
000300000711      *         CONTROLLO CAP ANAGRAFICHE                - BATCH -   *
000400000711      *         -------------------------                ---------   *
000500000704      ****************************************************************
000600000712     FFNLV75P   O    E             PRINTER OFLIND(*IN55)
000700101125     FTNCPO08L  iF   E           K DISK    infds(cpo01)
000800101125     Ftncpo00f  uF   E             DISK    rename(tncpo000:tncpofis)
000900110111     FCNACO00F  iF   E           K DISK
001000101126     Fazcpc01l  iF   E           K DISK    usropn
001100000712     FAZORG01L  IF   E           K DISK
001200110111     FCNIND00F  iF   E           K DISK    RENAME(CNIND000:CNINDR)
001300110111     FTFIND00F  iF   E           K DISK    USROPN extfile(WTFIND)
001400010403     ffnspe01l  uf   e           k disk
001500010403     ffnacr01l  uf   e           k disk
001600020417     fazcae01l  if   e           k disk
001700090410     fazcep02l  if a e           k disk
001800101123     fwacpmod1l if   e           k disk    usropn extfile(WCPMOD)
001900110105     f* file di work controllo CAP obsoleti da eliminare solo EDP
002000110105     fazcpl00F  if   e             disk    usropn extfile(Wazcpl)
002100101126     f                                     RENAME(azcpl000:azcplx)
002200000705     D L1              S              3  0 DIM(30)
002300110107     D CHK             S             50    dim(1) ctdata perrcd(1)
002400020417
002500020417     d wdata           s              8  0
002600020417
002700000712     D TCUDS           DS
002800000712     D  F1                     1      1
002900000712     D  F3                     3      3
003000000712     D  F2                     2      2
003100000712     D  F4                     4      4
003200000712     D  F56                    5      6
003300000711     D PARAM           DS
003400000711     D  VIDKCU                 1      4  0
003500000711     D  VIDVIO                 5      5
003600000711     D  VIDCLP                 6      6
003700000711     D  VIDFIL                 7      9  0
003800000711     D  VIDSAL                10     10
003900000711     D  VIDTPE                11     11
004000000712     D  VIDDES                12     31
004100010403     D  vidspe                32     32
004200010403     D  vidacr                33     33
004300061116     D  viddri                34     41  0
004400090408     D  vidcep                42     42
004500100224     D  amgdri                43     50  0
004600110105     d* file di work per controllo CAP/LOC obsoleti
004700110105     D  vidFILE               51     60
004800110105     D  vidLIB                61     70
004900110803     D**LIBDATIVIS           247    256
005000000713      *  DS PER RICHIAMO FNLV13R (CONTROLLO CAP)
005100000713     D DSSI95        E DS                  EXTNAME(TISI95DS)
005200000713     D DSLV13        E DS                  EXTNAME(FNLV13DS)
005300000713      *  DS KPJBA
005400000711     D KPJBA         E DS
005500000713      *  DS PER PARAMETRI UTENTE E RICERCA CONTI
005600000704     D CNCR80        E DS
005700000704     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005800000712     D  TCU                  398    697
005900000712     D                                     DIM(50)
006000000712     D  KCU                  698    847P 0
006100000712     D                                     DIM(50)
006200000712     D                                     PACKEVEN
006300000712     D  DTU                  848    895P 0
006400000712     D                                     DIM(12)
006500000712     D                                     PACKEVEN
006600000713      * DS PER TRUL06R (CARICAMENTO £1)
006700000704     D DSUL06        E DS                  EXTNAME(TRUL06DS)
006800000704     D  LIN                    1     90  0
006900000705     D                                     DIM(30)
007000090410     d kcap            s                   like(cepcap)
007100090410     d kloc            s                   like(ceploc)
007200101123     d klocmod         s                   like(oldl_loc)
007300101123     d kcapmod         s                   like(oldl_cap)
007400101123     d kprvmod         s                   like(oldl_prv)
007500100429     d wrkgetlista     s           4096    varying
007600100429     d
007700100429     d DataSYS         s               d   inz(*sys)
007800100429     d Dataiso         s               d   datfmt(*iso)
007900100429     d Alfaoggi        s              8
008000101123     d contaprv        s              9s 0
008100101123     d contacap        s              9s 0
008200101123     d contaloc        s              9s 0
008300101123     d contanar        s              9s 0
008400110107     d totspe          s             11s 0
008500110107     d alfaspe         s             11
008600110107     d libWFALA        s             10
008700100806
008800100922     D tivisds       E DS                  EXTname(tivis00f)
008900101123     d WCPMOD          s             22    inz('UNITRAGRU /WACPMOD1L')
009000101124     d Wtfind          s             21    inz('FILTRAGRU /TFIND00F')
009100110105     d Wazcpl          s             21    inz('xxxxxxxxxx/AZCPL37X  ')
009200101125     D cpo01           DS
009300101125     D  cpoNRR               397    400B 0
009400160831
009500160831       // -?Storicizzazione variazioni
009600160831     d TRMK30DS      e ds
009700160831     d TNCPO_30      e ds                  extname(TNCPO00F) inz
009800160831     d TNCPO1_30     e ds                  extname(TNCPO10F) inz
009900160831     d TICPI_30      e ds                  extname(TICPI00F) inz
010000160831
010100160831      //---------------------------------------------------------------
010200160831      //?Definizione procedure esterne.
010300160831      //---------------------------------------------------------------
010400160831       // -?Storicizzazione Variazioni Potenziale
010500160831     d TRMK30R         pr                  extpgm('TRMK30R')
010600160831     d  trmk30ds                           likeds(trmk30ds)
010700160831     d  tncpo_30                           likeds(tncpo_30)
010800160831     d  tncpo1_30                          likeds(tncpo1_30)
010900160831     d  ticpi_30                           likeds(ticpi_30)
011000160831
011100160831      // ----------------------------------------------------------------------
011200101125
011300000705      ****************************************************************
011400000705      * RIEPILOGO INDICATORI:                                        *
011500000705      ****************************************************************
011600000712      * 01 - ELABORO PIANO DEI CONTI                                 *
011700000712      * 02 - ELABORO ANAGRAFICA PROVVISORIA VISITE/OFFERTE           *
011800000712      * 03 - ELABORO ANAGRAFICA CLIENTI POTENZIALI                   *
011900010403      * 04 - ELABORO ANAGRAFICA SPEDIZIONIERI/LUOGHI                 *
012000010403      * 05 - ELABORO ANAGRAFICA CLIENTI RITIRO                       *
012100000712      * 21 - GRASSETTO LOCALITA'                                     *
012200000712      * 22 - GRASSETTO CAP                                           *
012300000712      * 23 - GRASSETTO PROVINCIA                                     *
012400000712      * 24 - GRASSETTO NAZIONE                                       *
012500000712      * 31 - COMODO                                                  *
012600000713      * 38 - OFF=DEVO STAMPARE TESTATA P.O.                          *
012700000713      * 39 - OFF=DEVO STAMPARE TESTATA TERMINAL                      *
012800000712      * 55 - OVERFLOW DI STAMPA                                      *
012900000713      * 98 - LETTURA FILES PRIMARI DA CONTROLLARE                    *
013000020417      * 99 - LETTURA azcae01l                                        *
013100000705      ****************************************************************
013200000704      *
013300000705      ****************************************************************
013400000704      *                                                              *
013500000704      *         O P E R A Z I O N I   I N I Z I A L I                *
013600000704      *                                                              *
013700000704      ****************************************************************
013800000704      *
013900000704     C     *ENTRY        PLIST
014000000704     C                   PARM                    KPJBA
014100000712     C                   MOVEL     KPJBU         PARAM
014200000712      *
014300000713      * PARAMETRI UTENTE (ANCHE DA STAMPARE)
014400000712      *
014500000712     C                   Z-ADD     1             CODUT
014600000712     C                   CALL      'X§PARUT'
014700000712     C                   PARM                    UT§DSE
014800000712     C                   MOVEL     RAGUT         PRSUT
014900000712     C                   MOVEL     REC80         CNCR80
015000020513     c                   if        simtpp = '2' or simtpp = *blanks
015100020513     c                   move      simpou        pfel
015200000712     C                   ELSE
015300000712     C                   MOVE      SIMFEL        PFEL
015400000712     C                   ENDIF
015500100429     c* data del giorno
015600100429     c                   eval      dataiso=datasys
015700100429     c                   eval      Alfaoggi=(%editc(%dec(dataiso):'X'))
015800000712      *
015900000712      * RICERCO CONTI
016000000712      *
016100000712     C                   DO        50            X                 2 0
016200000712     C                   MOVE      TCU(X)        TCUDS
016300000712     C     F56           CABNE     'CG'          END1
016400000712     C     F4            COMP      '1'                                    21
016500000712     C     F4            COMP      '2'                                    22
016600000712     C     F4            COMP      '3'                                    23
016700000712     C     F4            COMP      '6'                                    27
016800000712      ** 1 CLIENTI   21
016900000712      ** 2 FORNITORI 22
017000000712      ** 3 AGENTI    23
017100000712     C     F3            COMP      '0'                                242425
017200000712     C     F3            COMP      'I'                                    26
017300000712      ** 0 ITALIA   25
017400000712      ** 1 ESTERO   24
017500000712      * I CAPO CONTO IVA
017600000712     C   21
017700000712     CAN 24              Z-ADD     KCU(X)        KCE               4 0
017800000712     C   21
017900000712     CAN 25              Z-ADD     KCU(X)        KCI               4 0
018000000712     C   22
018100000712     CAN 24              Z-ADD     KCU(X)        KFE               4 0
018200000712     C   22
018300000712     CAN 25              Z-ADD     KCU(X)        KFI               4 0
018400000712     C   23
018500000712     CAN 24              Z-ADD     KCU(X)        KAE               4 0
018600000712     C   23
018700000712     CAN 25              Z-ADD     KCU(X)        KAI               4 0
018800000712     C   26              Z-ADD     KCU(X)        KIVA              4 0
018900000712     C   27              Z-ADD     KCU(X)        KBNA              4 0
019000000712     C     END1          TAG
019100000712     C                   END
019200000712     C                   SETOFF                                       212223
019300000712     C                   SETOFF                                       2425
019400000712     C                   SETOFF                                       2627
019500000712      *
019600020417      * ACCESSO A azcae01l
019700000712      *
019800020417     C     Kazcae        KLIST
019900020417     C                   KFLD                    kepa              1
020000101126     C     Kcpc          KLIST
020100101126     C                   KFLD                    kv36              5 0
020200101126     C                   KFLD                    cplnar
020300101126     C                   KFLD                    cplcap
020400000712      *
020500020417     C                   EVAL      kepa = 'P'
020600101126     C                   EVAL      kv36 = 36
020700000713      *
020800000713      * ACCESSO A CNACO00F
020900000713      *
021000000713     C     KCNACO        KLIST
021100000713     C                   KFLD                    INDKUT
021200000713     C                   KFLD                    INDKCC
021300000713     C                   KFLD                    INDKSC
021400000712      *
021500000712      * ACCESSO A CNIND00F
021600000712      *
021700000712     C     KCNIND        KLIST
021800020417     C                   KFLD                    KKUT              1 0
021900000712     C                   KFLD                    VIDKCU
022000000712     C                   KFLD                    KKSC              7 0
022100000712      *
022200000712     C     KCNIN2        KLIST
022300000712     C                   KFLD                    KKUT
022400000712     C                   KFLD                    VIDKCU
022500020417      *
022600020417     C                   EVAL      KKUT = 1
022700000712      *
022800000712      * ACCESSO A TFIND00F
022900000712      *
023000000712     C     KTFIND        KLIST
023100000712     C                   KFLD                    KKUT
023200000712     C                   KFLD                    KCI
023300000712     C                   KFLD                    VISNRV
023400000713      *
023500000713      * ACCESSO A TNCPO08L
023600000713      *
023700000713     C     KTNCPO        KLIST
023800000713     C                   KFLD                    KKSC
023900000713     C                   KFLD                    KFLT              3 0
024000010403      *
024100010403      * ACCESSO A FNSEP01L
024200010403      *
024300010403     C     KFNSPE        KLIST
024400010403     C                   KFLD                    KFLS              1
024500010403     C                   KFLD                    KKSC
024600090410     C     Kcep2         KLIST
024700090410     C                   KFLD                    cepdev
024800090410     C                   KFLD                    cepfil
024900090410     C                   KFLD                    cepnar
025000090410     C                   KFLD                    kcap
025100090410     C                   KFLD                    kloc
025200090410     C                   KFLD                    cepcev
025300101123     C     KCPMOD        KLIST
025400101123     C                   KFLD                    kprvmod
025500101123     C                   KFLD                    kcapmod
025600101123     C                   KFLD                    klocmod
025700020417
025800020417     c                   eval      wdata = *date
025900020417
026000000712      *
026100000712      * EMETTO PRIMA PAGINA CON PARAMETRI ELABORAZIONE
026200000712      *
026300000712      * .. ORARIO
026400000712     C                   TIME                    WORA              6 0
026500000712     C                   MOVEL     WORA          PHH
026600000712     C                   MOVE      WORA          PSS
026700000712     C                   MOVEL     WORA          WORK04            4
026800000712     C                   MOVE      WORK04        PMM
026900000712      *
027000000712      * .. ANAGRAFICO DA CONTROLLARE
027100000712     C                   IF        VIDKCU <> 0
027200000712     C                   CLEAR                   WORK27           27
027300000712     C                   EVAL      WORK27='Piano dei Conti: Capoconto:'
027400000712     C                   MOVE      VIDKCU        WORK04
027500000712     C     WORK27        CAT       WORK04:1      PANAGR
027600000712     C                   ENDIF
027700000712      *
027800000712     C                   IF        VIDVIO = 'S'
027900000712     C                   EVAL      PANAGR='Anagrafica Provvis. Visite/Offerte'
028000000712     C                   ENDIF
028100000712      *
028200000712     C                   IF        VIDCLP = 'S'
028300000712     C                   EVAL      PANAGR='Anagrafica Clienti Potenziali'
028400000712     C                   ENDIF
028500010403      *
028600010403     C                   IF        VIDSPE = 'S'
028700010403     C                   EVAL      PANAGR='Anagrafica Spedizionieri/Luoghi'
028800010403     C                   ENDIF
028900010403      *
029000010403     C                   IF        VIDACR = 'S'
029100010403     C                   EVAL      PANAGR='Anagrafica Clienti Ritiri'
029200010403     C                   ENDIF
029300090408     C                   IF        VIDcep = 'S'
029400090408     C                   EVAL      PANAGR='Cappario Eventi particolari:AZCEP'
029500090408     C                   ENDIF
029600100224     C***                IF        VIDcpp = 'S'
029700100224     C***                EVAL      PANAGR='Cappario Utiliz.particolari:AZCPP'
029800100224     C***                ENDIF
029900000712      *
030000000712      * .. SALTO PAGINA
030100000712     C                   IF        VIDSAL = 'S'
030200000712     C                   EVAL      PSINOS = 'SI'
030300000712     C                   ELSE
030400000712     C                   EVAL      PSINOS = 'NO'
030500000712     C                   ENDIF
030600000712      *
030700000712      * .. TIPO ELABORAZIONE
030800000712     C                   IF        VIDTPE = 'S'
030900000712     C                   EVAL      PTIPEL='Solo Stampa'
031000000712     C                   ELSE
031100000712     C                   EVAL      PTIPEL='Aggiornamento CAP in Automatico'
031200000712     C                   ENDIF
031300100224      *
031400100224      * Se immessa --> data di riferimento
031500100224     c                   if        viddri>0
031600100224     c                   seton                                        28
031700100224     c                   endif
031800101209
031900110105     c* Se non è richiesto nulla, provo ad aprire AZCPLxxx di work
032000101209     c                   if        vidvio=' ' and vidclp=' ' and
032100101209     c                             vidspe=' ' and vidacr=' ' and
032200110105     c                             vidcep=' ' and vidfile<>*blanks
032300110105     c                   eval      %subst(wazcpl:1:10)=vidlib
032400110105     c                   eval      %subst(wazcpl:12:10)=vidfile
032500110105     c                   open(e)   azcpl00F
032600110105     C                   EVAL      PANAGR='Località da cancellare da AZCPL '
032700101209     c                   endif
032800000712      *
032900000712     C                   WRITE     PRIPAG
033000000712      *                  ================
033100000713      *
033200000713      * APRO FILES VISITE/OFFERTE SOLO SE NON SONO IN SEDE
033300000713      *
033400000713     C     VIDVIO        IFEQ      'S'
033500101124     C                   OPEN(E)   TFIND00F
033600101124     c                   if        %error
033700101124     c                   eval      %subst(wTFIND:7:4)='GRPF'
033800101124     c                   open      TFIND00F
033900101124     c                   endif
034000000713     C                   ENDIF
034100101123     c
034200101123     c* Se esiste apro il file WACPMOD0F
034300101123     c                   open(e)   wacpmod1l
034400101123     c                   if        %error
034500101123     c                   eval      %subst(wcpmod:7:4)='GRP '
034600101123     c                   open(e)   wacpmod1l
034700101124     c                   endif
034800000713      *
034900000705      ****************************************************************
035000000704      *                                                              *
035100000704      *         R O U T I N E   P R I N C I P A L E                  *
035200000704      *                                                              *
035300000704      ****************************************************************
035400000704      *
035500020417      * LEGGO azcae01l PER OGNI TERMINAL DI PARTENZA VALIDO
035600000712      *
035700020417     C     kazcae        SETLL     azcae01l
035800000712      *
035900000712      *==============================================================*
036000000712      *                      CICLO 1                                 *
036100000712      *==============================================================*
036200000712      *
036300000712     C                   DO        *HIVAL
036400000712      *
036500020417     C     kazcae        READE     azcae01l                               99
036600000712     C   99              LEAVE
036700020417      * annullato
036800020417     c                   if        caeatb <> *blanks
036900020417     c                   iter
037000020417     c                   endif
037100020417      * controllo la data validità
037200020417     c                   if        caedde > wdata or caedsc < wdata
037300020417     c                   iter
037400020417     c                   endif
037500020417      * elaboro solo se è il terminal di partenza
037600020417     c                   if        caetfp <> caetfe
037700020417     c                   iter
037800020417     c                   endif
037900020417      * se sono in filiale elaboro solo se terminal di partenza = simfel
038000020417     c                   if        simfel > 0 and caetfp <> simfel
038100020417     c                   iter
038200020417     c                   endif
038300000712      *
038400000712      * CARICO L1 PER IL TERMINAL DI PARTENZA LETTO E IMPOSTO TESTATA
038500000712      *
038600000712     C                   EXSR      CARL1
038700000712      *                  ---------------
038800000712      * LEGGO LA £1
038900000712      *
039000000712      *==============================================================*
039100000712      *                      CICLO 2                                 *
039200000712      *==============================================================*
039300000712      *
039400000712     C     1             DO        30            Y                 3 0
039500000712      *
039600000712     C     L1(Y)         IFGT      0
039700000712      *
039800000712      * SE <> DA ZERO SCELGO SOLO IL P.O. IMMESSO A VIDEO
039900000712      *
040000000712     C     VIDFIL        IFNE      0
040100000712     C     VIDFIL        ANDNE     L1(Y)
040200000712     C                   ITER
040300000712     C                   ENDIF
040400000712      *
040500000712      * IMPOSTO TESTATA PUNTO OPERATIVO
040600000712      *
040700000712     C                   EVAL      PPUNTO = L1(Y)
040800100429     C     L1(Y)         CHAIN     AZORG01L
040900100429     c                   if        %found(azorg01l)
041000100429     C                   EVAL      PDESPU = ORGDES
041100100429     c                   else
041200100429     C                   EVAL      PDESPU = *BLANKS
041300100429     c                   endif
041400000713     C                   SETOFF                                       38
041500000712      *
041600000712      * ESEGUO SUBROUTINES IN BASE AL TIPO ELABORAZIONE
041700000712      *
041800000712      * .. ELABORAZIONE 01 - PIANO DEI CONTI
041900000712      *
042000000712     C     VIDKCU        IFNE      0
042100000712     C                   SETON                                          01
042200000712     C                   EXSR      ELAB01
042300000712      *                  ----------------
042400000712     C                   ENDIF
042500000712      *
042600000712      * .. ELABORAZIONE 02 - ANAGRAFICA PROVVISORIA VISITE OFFERTE
042700000712      *
042800000712     C     VIDVIO        IFEQ      'S'
042900000712     C                   SETON                                          02
043000000712     C                   EXSR      ELAB02
043100000712      *                  ----------------
043200000712     C                   ENDIF
043300000712      *
043400000712      * .. ELABORAZIONE 03 - ANAGRAFICA CLIENTI POTENZIALI
043500000712      *
043600000712     C     VIDCLP        IFEQ      'S'
043700000712     C                   SETON                                          03
043800000712     C                   EXSR      ELAB03
043900000712      *                  ----------------
044000000712     C                   ENDIF
044100010403      *
044200010403      * .. ELABORAZIONE 04 - ANAGRAFICA SPEDIZIONIERI/LUOGHI
044300010403      *
044400010403     C     VIDSPE        IFEQ      'S'
044500010403     C                   SETON                                          04
044600010403     C                   EXSR      ELAB04
044700010403      *                  ----------------
044800010403     C                   ENDIF
044900010403      *
045000010403      * .. ELABORAZIONE 05 - ANAGRAFICA CLIENTI RITIRO
045100010403      *
045200010403     C     VIDACR        IFEQ      'S'
045300010403     C                   SETON                                          05
045400010403     C                   EXSR      ELAB05
045500010403      *                  ----------------
045600010403     C                   ENDIF
045700090408      *
045800000712     C                   ENDIF
045900000712      *
046000000712     C                   ENDDO
046100000712      *
046200000712      *==============================================================*
046300000712      *                    FINE CICLO 2                              *
046400000712      *==============================================================*
046500000712      *
046600000712     C                   ENDDO
046700000712      *
046800000712      *==============================================================*
046900000712      *                    FINE CICLO 1                              *
047000000712      *==============================================================*
047100090408      *
047200090408      * .. ELABORAZIONE 06 - Cappario eventi particolari
047300090408      *                      non si elabora in base al p.o.
047400090408      *                      ma tutto insieme
047500090408      *
047600090408     C     VIDcep        IFEQ      'S'
047700090408     C                   SETON                                          06
047800090408     C                   EXSR      ELAB06
047900090408      *                  ----------------
048000090408     C                   ENDIF
048100101126     c
048200101126     c
048300101126     c                   if        vidvio=' ' and vidclp=' ' and
048400101126     c                             vidspe=' ' and vidacr=' ' and
048500110105     c                             vidcep=' ' and %open(azcpl00F)
048600101126     C                   SETON                                          0607
048700110105     c                   open      azcpc01l
048800110107     c* Verifico in quale libreria esiste WFALA
048900110107     c                   clear                   comman
049000110107     c                   z-add     80            lenght
049100110107     c                   movel     'CHKOBJ OBJ(' comman
049200110107     c                   eval      %subst(comman:12:10)='GAITRAAZP'
049300110107     c                   eval      %subst(chk(1):2:10)='WFALA00F'
049400110107     c                   eval      comman=%trim(comman)+chk(1)
049500110107     c                   Call      'QCMDEXC'                            99
049600110107     c                   Parm                    comman           80
049700110107     c                   Parm                    lenght           15 5
049800110107     c
049900110107     c                   if        *in99
050000110107     c                   eval      libWFALA='GAITRAAZM'
050100110107     c                   else
050200110107     c                   eval      libWFALA='GAITRAAZP'
050300110107     c                   endif
050400101126     c
050500110105     c                   read      azcpl00F
050600101126     c                   dow       not %eof
050700101126     c     kcpc          chain     azcpc01l
050800101126     c                   eval      ceploc=cplloc
050900101126     c                   eval      cepprv=cpcprv
051000101126     c                   eval      cepcap=cplcap
051100101126     c                   eval      cepnar=cplnar
051200101126     c                   eval      TENTATIVO='SECONDO2'
051300110107     c* Intanto verifico in WFAKLA quante spedizioni ci sono
051400110107     c                   exsr      contWFALA
051500110107     c
051600101126     C                   EXSR      CHKCAP
051700101126     c
051800110105     c                   read      azcpl00f
051900101126     c                   enddo
052000101126     c                   endif
052100090408      *
052200000704      *
052300000704      * FINE PROGRAMMA
052400000704      *
052500000712     C                   WRITE     FINEST
052600000712      *                  ================
052700000712      *
052800000713     C     VIDVIO        IFEQ      'S'
052900000713     C                   CLOSE     TFIND00F
053000000713     C                   ENDIF
053100000721     c* chiudo pgm
053200000721     c                   clear                   dslv13
053300000721     c                   clear                   dssi95
053400000721     c                   movel     'C'           i13tla
053500000721     C                   CALL      'FNLV13R'
053600000721     C                   PARM                    KPJBA
053700000721     C                   PARM                    DSLV13
053800000721     C                   PARM                    DSSI95
053900000721     C                   PARM                    flgbac            1
054000000713      *
054100000711     C                   SETON                                        LR
054200000704      *
054300000705      ****************************************************************
054400000704      *                                                              *
054500000704      *         S U B R O U T I N E S                                *
054600000704      *                                                              *
054700000704      ****************************************************************
054800000704      *
054900000704      *--------------------------------------------------------------*
055000000712      * CARL1  - SUBROUTINE CARICAMENTO L1                           *
055100000704      *--------------------------------------------------------------*
055200000712      *
055300000712     C     CARL1         BEGSR
055400000712      *
055500020417      * IMPOSTO TESTATA E CARICO schiera FILIALI GESTITE £1
055600000712      *
055700000712      * .. TERMINAL PARTENZA IN STAMPA (CON DECODIFICA)
055800000712      *
055900000712     C                   SETOFF                                       39
056000020417     c                   if        simfel = 0
056100020417     C                   MOVEL     caetfp        PTERMP
056200020417     c                   else
056300020417     c                   movel     pfel          ptermp
056400020417     c                   endif
056500000712     C     PTERMP        CHAIN     AZORG01L                           31
056600000712     C  N31              EVAL      PDESTE = ORGDES
056700000712     C   31              EVAL      PDESTE = *BLANKS
056800000712      *
056900000712      * .. CARICO L1
057000000712      *
057100000712     C                   CLEAR                   DSUL06
057200000712     C                   MOVE      '£1'          D06COD
057300000712     C                   MOVEL     PTERMP        D06KEY
057400000712     C                   MOVEL     DSUL06        KPJBU
057500000712     C*
057600000712     C                   CALL      'TRUL06R'
057700000712     C                   PARM                    KPJBA
057800000712     C                   MOVEL     KPJBU         DSUL06
057900000712     C                   MOVEA     LIN           L1
058000000712     C*
058100000712     C                   ENDSR
058200000712      *
058300000712      *--------------------------------------------------------------*
058400000712      * ELAB01 - SUBROUTINE CONTROLLO PIANO DEI CONTI                *
058500000712      *--------------------------------------------------------------*
058600000712      *
058700000712     C     ELAB01        BEGSR
058800000712      *
058900000713      * LETTURA CNIND00F (ANAGRAFICA INDIRIZZI)
059000000712      *
059100000712      * .. DAL P.O.
059200000712     C                   CLEAR                   KKSC
059300000712     C                   MOVEL     PPUNTO        KKSC
059400000712      * .. AL P.O.
059500000712     C                   Z-ADD     9999999       WCODFIN           7 0
059600000712     C                   MOVEL     PPUNTO        WCODFIN
059700000712      *
059800000712     C     KCNIND        SETLL     CNIND00F
059900000712     C                   DO        *HIVAL
060000110111     c**                 if        vidtpe='S'
060100110111     C**   KCNIN2        READE(n)  CNIND00F                               98
060200110111     c**                 else
060300090408     C     KCNIN2        READE     CNIND00F                               98
060400110111     c**                 endif
060500090408     c
060600000712      *
060700000712      * ESCO PER FINE FILE O PER SUPERO LIMITI P.O.
060800000712      *
060900000712     C   98              LEAVE
061000000712     C     INDKSC        IFGT      WCODFIN
061100000712     C                   LEAVE
061200000712     C                   ENDIF
061300000712      *
061400000712      * SE CAPOCONTO E' "CLIENTI" ESCLUDO XXX8888 E XXX9999
061500000712      *
061600000712     C     VIDKCU        IFEQ      KCI
061700000712     C                   MOVE      INDKSC        WORKNUM4          4 0
061800000712     C     WORKNUM4      IFEQ      8888
061900000712     C     WORKNUM4      OREQ      9999
062000000712     C                   ITER
062100000712     C                   ENDIF
062200000712     C                   ENDIF
062300000713      *
062400000713      * ESCLUDO SE NON HO TROVATO IL RELATIVO RECORD DI CNACO00F
062500000713      *
062600110111     c**                 if        vidtpe='S'
062700110111     C**   KCNACO        CHAIN(N)  CNACO00F                           31
062800110111     c**                 else
062900110111     C     KCNACO        CHAIN     CNACO00F                           31
063000110111     c**                 endif
063100000713     C   31              ITER
063200000712      *
063300000712      * ESEGUO L'EFFETTIVO CONTROLLO DEL CAP E SE ERRATO STAMPO
063400000712      *
063500000712     C                   EXSR      CHKCAP
063600000712      *                  ----------------
063700000712      *
063800000712     C                   ENDDO
063900000712      *
064000000712     C                   ENDSR
064100000712      *
064200000712      *--------------------------------------------------------------*
064300000712      * ELAB02 - SUBROUTINE CONTROLLO ANAGR. PROVVIS. VISITE/OFFERTE *
064400000712      *--------------------------------------------------------------*
064500000712     C     ELAB02        BEGSR
064600000712      *
064700100806      * LETTURA TIVIS00F (ANAGRAFICA VISITE CLIENTI)
064800000712      *
064900000712      * .. DAL P.O.
065000000712     C                   CLEAR                   KKSC
065100000712     C                   MOVEL     PPUNTO        KKSC
065200000712      * .. AL P.O.
065300000712     C                   Z-ADD     9999999       WCODFIN           7 0
065400000712     C                   MOVEL     PPUNTO        WCODFIN
065500000712      *
065600100429     c
065700100806     c* eseguo SQL sulla TIVIS
065800100429     c                   eval      wrkgetlista =
065900110803     c                             'select VISNRV, VISRAG from '+
066000110803     c                             'TIVIS00F  +
066100100429     c                             where viscmm>=' +
066200100429     c                             %editc(kksc:'X') + ' and viscmm<=' +
066300130617     c                             %editc(wcodfin:'X') +
066400130617     c                             ' and visdch = 0 and visffz = '' '''
066500100429     C/EXEC SQL
066600100429     C+ PREPARE s1 FROM :wrkgetlista
066700100429     C/END-EXEC
066800100429     C
066900100429     C/EXEC SQL
067000100429     C+ DECLARE A1 CURSOR FOR S1
067100100429     C/END-EXEC
067200100429     C
067300100429     C/EXEC SQL
067400100429     C+ OPEN a1
067500100429     C/END-EXEC
067600100429
067700100429      *?Leggo il file
067800100429     c                   do        *hival
067900100429     C/EXEC SQL
068000100429     C+ FETCH NEXT FROM a1 INTO :VISNRV, :VISRAG
068100100429     C/END-EXEC
068200100429     c                   select
068300100429
068400100429     c                   when      sqlcod = 100
068500100429     c                   leave
068600100429     c                   when      sqlcod < 0
068700100429     c                   seton                                        H1
068800100429     c                   leave
068900100429     c                   other
069000100429     c
069100110111     c**                 if        vidtpe='S'
069200110111     C**   KTFIND        CHAIN(n)  TFIND00F                           31
069300110111     c**                 else
069400100429     C     KTFIND        CHAIN     TFIND00F                           31
069500110111     c**                 endif
069600100429     C   31              ITER
069700100429      *
069800100429      * ESEGUO L'EFFETTIVO CONTROLLO DEL CAP E SE ERRATO STAMPO
069900100429     C                   EXSR      CHKCAP
070000100429     c                   endsl
070100100429
070200100429     c                   enddo
070300000712      *
070400100429     C/EXEC SQL
070500100429     C+ CLOSE a1
070600100429     C/END-EXEC
070700100429     c
070800000712     C                   ENDSR
070900000712      *
071000000712      *--------------------------------------------------------------*
071100000712      * ELAB03 - SUBROUTINE CONTROLLO ANAGRAFICA CLIENTI POTENZIALI  *
071200000712      *--------------------------------------------------------------*
071300000713      * NB: ESEGUO 2 CICLI DI LETTURA SUL FILE PER RISPARMIARE SULLA *
071400000713      *     CREAZIONE DI NUOVE VISTE LOGICHE                         *
071500000713      *---------------------------------------------------------------
071600000713      *
071700000712     C     ELAB03        BEGSR
071800000713      *
071900000713      * LETTURA TNCPO08L (ANAGRAFICA CLIENTI POTENZIALI)
072000000713      * .. 1° CICLO PER KKSC=0 E KFLT=P.O.
072100000713      *
072200000713     C                   EVAL      KKSC = 0
072300000713     C                   EVAL      KFLT = PPUNTO
072400000713      *
072500000713     C     KTNCPO        SETLL     TNCPO08L
072600000713     C                   DO        *HIVAL
072700090408     c                   if        vidtpe='S'
072800090408     C     KTNCPO        READE(n)  TNCPO08L                               98
072900090408     c                   else
073000090408     C     KTNCPO        READE     TNCPO08L                               98
073100090408     c                   endif
073200000713      *
073300000713      * ESCO PER FINE LETTURA
073400000713      *
073500000713     C   98              LEAVE
073600040804      * non elaboro gli annullati
073700040804     c                   If        CpoAtb = *Blanks
073800000713      *
073900000713      * ESEGUO L'EFFETTIVO CONTROLLO DEL CAP E SE ERRATO STAMPO
074000000713      *
074100000713     C                   EXSR      CHKCAP
074200000713      *                  ----------------
074300000713      *
074400040804     c                   Endif
074500040804
074600000713     C                   ENDDO
074700000713      *
074800000713      * LETTURA TNCPO08L (ANAGRAFICA CLIENTI POTENZIALI)
074900000713      * .. 2° CICLO PER KKSC=XXXYYYY (DOVE XXX E' IL P.O.)
075000000713      *
075100000713      * .. DAL P.O.
075200000713     C                   CLEAR                   KKSC
075300000713     C                   MOVEL     PPUNTO        KKSC
075400000713      * .. AL P.O.
075500000713     C                   Z-ADD     9999999       WCODFIN           7 0
075600000713     C                   MOVEL     PPUNTO        WCODFIN
075700000713      *
075800000713     C     KKSC          SETLL     TNCPO08L
075900000713     C                   DO        *HIVAL
076000090408     c                   if        vidtpe='S'
076100090408     C                   READ(n)   TNCPO08L                               98
076200090408     c                   else
076300090408     C                   READ      TNCPO08L                               98
076400090408     c                   endif
076500000713      *
076600000713      * ESCO PER FINE FILE O PER SUPERO LIMITI P.O.
076700000713      *
076800000713     C   98              LEAVE
076900000713     C     CPOCMM        IFGT      WCODFIN
077000000713     C                   LEAVE
077100000713     C                   ENDIF
077200040804      * non elaboro gli annullati
077300040804     c                   If        CpoAtb = *Blanks
077400000713      *
077500000713      * ESEGUO L'EFFETTIVO CONTROLLO DEL CAP E SE ERRATO STAMPO
077600000713      *
077700000713     C                   EXSR      CHKCAP
077800000713      *                  ----------------
077900000713      *
078000040804     c                   Endif
078100040804
078200000713     C                   ENDDO
078300000713      *
078400000712     C                   ENDSR
078500010403      *
078600010403      *--------------------------------------------------------------*
078700010403      * ELAB04 - SUBROUTINE CONTROLLO ANAGRAFICA SPEDIZIONIERI/LUOGHI*
078800010403      *--------------------------------------------------------------*
078900010403      *
079000010403     C     ELAB04        BEGSR
079100010403      *
079200010403      * LETTURA FNSPE01L (ANAGRAFICA SPEDIZIONIERI/LUOGHI)
079300010403      *
079400010403      * SOLO I LUOGHI
079500010403     C                   EVAL      KFLS = 'L'
079600010403      * .. DAL P.O.
079700010403     C                   CLEAR                   KKSC
079800010403     C                   MOVEL     PPUNTO        KKSC
079900010403      * .. AL P.O.
080000010403     C                   Z-ADD     9999999       WCODFIN           7 0
080100010403     C                   MOVEL     PPUNTO        WCODFIN
080200010403      *
080300010403     C     KFNSPE        SETLL     FNSPE01L
080400010403     C                   DO        *HIVAL
080500090408     c                   if        vidtpe='S'
080600090408     C                   READ(n)   FNSPE01L                               98
080700090408     c                   else
080800090408     C                   READ      FNSPE01L                               98
080900090408     c                   endif
081000010403      *
081100010403      * ESCO PER FINE FILE O PER SUPERO LIMITI P.O.
081200010403      *
081300010403     C   98              LEAVE
081400010403     C     SPECLI        IFGT      WCODFIN
081500010403     C                   LEAVE
081600010403     C                   ENDIF
081700010403      *
081800010403      * ESEGUO L'EFFETTIVO CONTROLLO DEL CAP E SE ERRATO STAMPO
081900010403      *
082000010403     C                   EXSR      CHKCAP
082100010403      *                  ----------------
082200010403      *
082300010403     C                   ENDDO
082400010403      *
082500010403     C                   ENDSR
082600010403      *
082700010403      *--------------------------------------------------------------*
082800010403      * ELAB05 - SUBROUTINE CONTROLLO ANAGRAFICA CLIENTI RITIRO      *
082900010403      *--------------------------------------------------------------*
083000010403      *
083100010403     C     ELAB05        BEGSR
083200010403      *
083300010403      * LETTURA FNACR01L (ANAGRAFICA CLIENTI RITIRO)
083400010403      * .. DAL P.O.
083500010403     C                   CLEAR                   kcro             10 0
083600010403     C                   MOVEL     PPUNTO        Kcro
083700010403      * .. AL P.O.
083800010403     C                   Z-ADD     *all'9'       WCODFIN1         10 0
083900010403     C                   MOVEL     PPUNTO        WCODFIN1
084000010403      *
084100010403     C     kcro          SETLL     FNACR01L
084200010403     C                   DO        *HIVAL
084300090408     c                   if        vidtpe='S'
084400090408     C                   READ(n)   FNACR01L                               98
084500090408     c                   else
084600090408     C                   READ      FNACR01L                               98
084700090408     c                   endif
084800010403      *
084900010403      * ESCO PER FINE FILE O PER SUPERO LIMITI P.O.
085000010403      *
085100010403     C   98              LEAVE
085200010403     C     ACRCRO        IFGT      WCODFIN1
085300010403     C                   LEAVE
085400010403     C                   ENDIF
085500010403      *
085600010403      * ESEGUO L'EFFETTIVO CONTROLLO DEL CAP E SE ERRATO STAMPO
085700010403      *
085800010403     C                   EXSR      CHKCAP
085900010403      *                  ----------------
086000010403      *
086100010403     C                   ENDDO
086200010403      *
086300010403     C                   ENDSR
086400000712      *
086500090408      *--------------------------------------------------------------*
086600090408      * ELAB066- SUBROUTINE CONTROLLO cappario eventi particolari    *
086700090408      *--------------------------------------------------------------*
086800090408      *
086900090408     C     ELAB06        BEGSR
087000090408      *
087100090408      * LETTURA AZCEP02l
087200090408      * Utilizzo l'anno della data di riferimento
087300090410     c                   clear                   kdev
087400090408     C                   IF        VIDDRI > *zeros
087500100224     C                   movel     amgdri        kdev              8 0
087600090408     C                   ELSE
087700090408     C                   movel     *date         kdev
087800090408     C                   ENDIF
087900090408     c                   movel     kdev          kanno             4 0
088000090410     c                   move      0101          kdev
088100090408      *
088200090408     C     kdev          SETLL     azcep02l
088300090408     c                   move      1231          kdev
088400090408     c
088500090408     C                   DO        *HIVAL
088600090408     C                   READ      azcep02l                               98
088700090408      *
088800090408      * ESCO PER FINE FILE O PER SUPERO LIMITI data
088900090408      *
089000090408     C   98              LEAVE
089100090408     C     cepdev        IFGT      kdev
089200090408     C                   LEAVE
089300090408     C                   ENDIF
089400090408      *
089500090408      * ESEGUO L'EFFETTIVO CONTROLLO DEL CAP E SE ERRATO STAMPO
089600090408      *  se non è inserito il record per p.o.
089700090408      *
089800090408     c                   if        cepfil=0
089900090408     C                   EXSR      CHKCAP
090000090408      *                  ----------------
090100090408     c                   endif
090200090408      *
090300090408     C                   ENDDO
090400090408      *
090500090408     C                   ENDSR
090600000712      *--------------------------------------------------------------*
090700000712      * CHKCAP - SUBROUTINE CONTROLLO CAP                            *
090800000712      *--------------------------------------------------------------*
090900000713      * NB: VIENE RICHIAMATO "FNLV13R" ED IN BASE AL RITORNO DEI     *
091000000713      *     PARAMETRI STAMPO ERRORE OPPURE NO;                       *
091100000713      *     POSSONO ESSERE EFFETTUATI 2 TENTATIVI DI CONTROLLO:      *
091200000713      *     IL PRIMO: NORMALE                                        *
091300070116      *     IL SECONDO1: SE HO TROVATO CAP OBSOLETO AL PRIMO         *
091400070116      *     IL SECONDO2: SE ERRORE DI PROVINCIA AL PRIMO             *
091500000713      *---------------------------------------------------------------
091600000713      *
091700000712     C     CHKCAP        BEGSR
091800000713      *
091900000713      * INIZIALIZZO TENTATIVI
092000000713      *
092100101126     C  n07              MOVEL(P)  'PRIMO'       TENTATIVO         8
092200101122     c                   clear                   obslna            3 0
092300101122     c                   clear                   obslocnor        35
092400000713      *
092500000713      * PULISCO DS CHE SERVONO A "FNLV13R"
092600000713      *
092700000713     C     RITENT        TAG
092800000713      *
092900000713     C                   CLEAR                   DSLV13
093000000713     C                   CLEAR                   DSSI95
093100000713      *
093200000713      * PASSO PARAMETRI COMUNI A "FNLV13R"
093300000713      *
093400000713     C                   MOVEL     '7'           I95TCN
093500000713     C                   MOVEL     'S'           I13AF0
093600150206     C                   MOVEL     'Y'           I13CNV
093700000713     C                   MOVEL     'S'           I13LA3
093800000721     c                   movel     'S'           flgbac
093900090408     c* Se cappario e non c'e' località
094000090408     c                   if        *in06 and ceploc=*blanks
094100090408     C                   MOVEL     '3'           I95TCN
094200090410     c                   clear                   i13la3
094300090408     c                   endif
094400061116     C*
094500061116     C* Se passata data riferimento a video considero quella, altrimenti
094600061116     C* la data corrente
094700061116     C                   IF        VIDDRI > *zeros
094800100224     C                   EVAL      I95DAT = amgDRI
094900061116     C                   ELSE
095000000713     C                   EVAL      I95DAT = *DATE
095100061116     C                   ENDIF
095200000713      *
095300000713      * PASSO PARAMETRI IN BASE AD ANAGRAFICO A "FNLV13R"
095400000713      *
095500000713      * .. PIANO DEI CONTI oppure
095600000713      * .. ANAGRAFICA PROVVISORIA VISITE/OFFERTE
095700000713      *
095800000713     C     *IN01         IFEQ      *ON
095900000713     C     *IN02         OREQ      *ON
096000000713     C                   MOVEL     INDCIT        I95LOC
096100070116     C     TENTATIVO     IFEQ      'PRIMO'
096200000713     C                   MOVEL     INDCAE        I95CAP
096300000713     C                   ELSE
096400070116     C     TENTATIVO     IFEQ      'SECONDO1'
096500000713     C                   CLEAR                   I95CAP
096600070116     C                   ELSE
096700070116     C                   MOVEL     INDCAE        I95CAP
096800070116     C                   MOVEL     '4'           I95TCN
096900070116     C                   ENDIF
097000000713     C                   ENDIF
097100150206     c                   movel     indvia        i95ind
097200000713     C                   MOVEL     INDPRV        I95PRV
097300000713     C                   MOVEL     INDSTA        I95NAR
097400000713     C                   ENDIF
097500000713      *
097600000713      * .. ANAGRAFICA CLIENTI POTENZIALI
097700000713      *
097800000713     C     *IN03         IFEQ      *ON
097900000713     C                   MOVEL     CPOCIT        I95LOC
098000070116     C     TENTATIVO     IFEQ      'PRIMO'
098100000713     C                   MOVEL     CPOCAP        I95CAP
098200000713     C                   ELSE
098300070116     C     TENTATIVO     IFEQ      'SECONDO1'
098400070116     C                   CLEAR                   I95CAP
098500070116     C                   ELSE
098600070116     C                   MOVEL     CPOCAP        I95CAP
098700070116     C                   MOVEL     '4'           I95TCN
098800070116     C                   ENDIF
098900000713     C                   ENDIF
099000150206     C                   MOVEL     CPOvia        I95ind
099100150206     C                   MOVEL     CPOPRV        I95PRV
099200000713     C                   MOVEL     CPONAZ        I95NAR
099300000713     C                   ENDIF
099400010403      *
099500010403      * .. ANAGRAFICA SPEDIZIONIERI/LUOGHI
099600010403      *
099700010403     C     *IN04         IFEQ      *ON
099800010403     C                   MOVEL     SPELOC        I95LOC
099900070116     C     TENTATIVO     IFEQ      'PRIMO'
100000010403     C                   MOVEL     SPECAP        I95CAP
100100010403     C                   ELSE
100200070116     C     TENTATIVO     IFEQ      'SECONDO1'
100300070116     C                   CLEAR                   I95CAP
100400070116     C                   ELSE
100500070116     C                   MOVEL     SPECAP        I95CAP
100600070116     C                   MOVEL     '4'           I95TCN
100700070116     C                   ENDIF
100800010403     C                   ENDIF
100900150206     C                   MOVEL     SPEind        I95ind
101000150206     C                   MOVEL     SPEPRO        I95PRV
101100010403     C                   MOVEL     SPENAZ        I95NAR
101200010403     C                   ENDIF
101300010403      *
101400010403      * .. ANAGRAFICA CLIENTI RITIRO
101500010403      *
101600010403     C     *IN05         IFEQ      *ON
101700010403     C                   MOVEL     ACRLOR        I95LOC
101800070116     C     TENTATIVO     IFEQ      'PRIMO'
101900010403     C                   MOVEL     ACRCAR        I95CAP
102000010403     C                   ELSE
102100070116     C     TENTATIVO     IFEQ      'SECONDO1'
102200070116     C                   CLEAR                   I95CAP
102300070116     C                   ELSE
102400070116     C                   MOVEL     ACRCAR        I95CAP
102500070116     C                   MOVEL     '4'           I95TCN
102600070116     C                   ENDIF
102700010403     C                   ENDIF
102800150206     C                   MOVEL     ACRinr        I95ind
102900150206     C                   MOVEL     ACRPRR        I95PRV
103000010403     C                   MOVEL     ACRNAR        I95NAR
103100010403     C                   ENDIF
103200090408      *
103300090408      * .. cappario utilizzi particolari
103400090408      *
103500090408     C     *IN06         IFEQ      *ON
103600090408     C                   MOVEL     ceploc        I95LOC
103700090408     C     TENTATIVO     IFEQ      'PRIMO'
103800090408     C                   MOVEL     cepcap        I95CAP
103900090408     C                   ELSE
104000090408     C     TENTATIVO     IFEQ      'SECONDO1'
104100090408     C                   CLEAR                   I95CAP
104200090408     C                   ELSE
104300090408     C                   MOVEL     cepcap        I95CAP
104400090408     C                   MOVEL     '4'           I95TCN
104500090408     C                   ENDIF
104600090408     C                   ENDIF
104700090408     c
104800090408     C                   MOVEL     cepprv        I95PRV
104900090408     C                   MOVEL     cepnar        I95NAR
105000150206     C                   MOVEL     'S'           I13CNV
105100090408     C                   ENDIF
105200101122     c
105300101122     c* Per secondo tentativo per sicurezza usola località noramlizzata
105400101122     C     TENTATIVO     IFEQ      'SECONDO1'
105500101122     c                   movel     obslocnor     i95loc
105600101122     c                   endif
105700000713      *
105800000713      * LANCIO "FNLV13R"
105900000713      *
106000000713      *             --------------------------------------
106100000713     C                   CALL      'FNLV13R'
106200000713     C                   PARM                    KPJBA
106300000713     C                   PARM                    DSLV13
106400000713     C                   PARM                    DSSI95
106500000721     C                   PARM                    flgbac            1
106600000713      *             --------------------------------------
106700101122     c* Se mi è stato dato errore per secondo tentativo ma
106800101122     c*  trova i dati univoci per localit,. se corrisponde la lna
106900101122     c*  prima e dopo, tratto come se ok
107000101122     c                   if        TENTATIVO='SECONDO1 '
107100101122     C                             AND O95LNA=obslna
107200101122     c                             and %subst(O95MSG:78:1) = '9'
107300101122     c                             and o95err=' '
107400101122     c                   clear                   o13err
107500101122     c                   eval      o13rop='S'
107600101122     c                   eval      o13roc='S'
107700101122     c                   clear                   o95msg
107800101122     c                   eval      o95msg='          cambiata anche PROV.+
107900101122     c                             x località UNIVOCA'
108000101122     c                   endif
108100000713      *
108200000713      * SE VI E' ERRORE STAMPO ED EVENTUALMENTE AGGIORNO
108300110303      * non vado in stampa per errore "E" perchè è previsto un secondo
108400110303      *  tentativo il "SECONDO2"
108500000713      *
108600070116     C     O95ERR        IFNE      *BLANKS
108700110303     C     O95ERR        andne     'E'
108800110120     c
108900070116     C     TENTATIVO     ORNE      'PRIMO'
109000110120     c
109100090410     c     o13err        orne      *blanks
109200090410     c     *in06         andeq     *on
109300090410     c     ceploc        andeq     *blanks
109400101209     c*
109500101209     c                   if        not *in07 or (*in07 and o95err<>*blanks)
109600000713     C                   EXSR      STAMPA
109700101209     c                   endif
109800101209     c
109900110120     c                   leavesr
110000000713      *                  ----------------
110100000713     C                   ENDIF
110200000713      *
110300090408      * non posso fare i nuovi tentativi se non c'e' la località nel cappario
110400090408     c                   if        not *in06 or ceploc<>*blanks
110500090408     c
110600000713      * SE NON VI E' ERRORE MA CAP = OBSOLETO RITENTO CLEARANDO IL CAP
110700000713      *
110800101124     C**   O95ERR        IFEQ      *BLANKS
110900101124     C**   O95CF1        ANDNE     *BLANKS
111000101124     C**   TENTATIVO     ANDEQ     'PRIMO'
111100101124     c
111200101124     c                   if        o95err=*blanks and
111300101124     c                             tentativo='PRIMO ' and
111400101124     c                             (o95cf1<>*blanks or o95lf1<>*blanks)
111500000713      *
111600070116     C                   MOVEL     'SECONDO1'    TENTATIVO
111700101122     c* Mi salvo la linea di arrivo del cap obsoleto
111800101122     c                   eval      obslna=o95lna
111900101122     c* Mi salvo anche la località normalizzata per la seconda ricerca
112000101122     c                   eval      obslocnor=o95loc
112100101123     c
112200101123     c*
112300101123     c                   clear                   writent           1
112400101124     c
112500101124     c* Questo file di conversione è valido solo dalla versine 37 in poi
112600101124     c                   if        %open(wacpmod1l)   and o95ver>=37
112700101123     c                   exsr      cercaWACPMOD
112800101123     c                   endif
112900000713      *
113000101123     c                   if        writent=' '
113100000713     C                   GOTO      RITENT
113200101123     c                   endif
113300101123     c
113400000713      *                  °°°°°°°°°°°°°°°°
113500000713      *
113600000713     C                   ENDIF
113700070116      *
113800070116      * SE VI E' ERRORE DI PROVINCIA = RITENTO CON TIPO CONTROLLO 4
113900070116      * E LIVELLO DATI >= 3 E LIVELLO AFFIDABILITA >= 2
114000070116      *
114100070116     C                   IF        (%subst(O95MSG:78:1) = '9'  OR
114200070116     C                              %subst(O95MSG:78:1) = 'E') AND
114300070116     C                             TENTATIVO = 'PRIMO'
114400070116      *
114500070116     C                   MOVEL     'SECONDO2'    TENTATIVO
114600070116      *
114700070116     C                   GOTO      RITENT
114800070116      *                  °°°°°°°°°°°°°°°°
114900070116      *
115000070116     C                   ENDIF
115100110120     c
115200110120     c* Se il tisi95 non da errore ma lo da FNLV13, stampo lo stesso
115300110120     C     O95ERR        ifeq      *BLANKS
115400110120     C     TENTATIVO     andeq     'PRIMO'
115500110120     c     o13err        andne     *blanks
115600110120     c     *in06         andeq     *off
115700110120     c*
115800110120     C                   EXSR      STAMPA
115900110120     c                   endif
116000110120     c
116100090408     C                   ENDIF
116200000713      *
116300000712     C                   ENDSR
116400101123      *--------------------------------------------------------------*
116500101123      * Cerca sul file che ci hanno dato le poste                    *
116600101123      *--------------------------------------------------------------*
116700101123      *
116800101123     C     CercaWACPMOD  BEGSR
116900101123     c                   eval      klocmod=o95loc
117000101123     c                   eval      kcapmod=i95cap
117100101123     c                   eval      kprvmod=i95prv
117200101123     c
117300101123     c     kcpmod        chain     wacpmod1l
117400101123     c                   if        %found(wacpmod1l) and newl_prv<>*blanks
117500101123     c                             and newl_cap<>*blanks and newl_loc<>*blanks
117600101123     c                   clear                   o13err
117700101123     c                   if        o95prv<>newl_prv
117800101123     c                   eval      o13rop='S'
117900101123     c                   eval      o95prv=newl_prv
118000101123     c                   endif
118100101123     c
118200101123     c                   if        o95cap<>newl_cap
118300101123     c                   eval      o13roc='S'
118400101123     c                   eval      o95cap=newl_cap
118500101123     c                   endif
118600101123     c
118700101123     c                   clear                   o95msg
118800101123     c                   eval      o95msg='          Correzione da dati P+
118900101123     c                             OSTE                              '
119000101123     c                   exsr      Stampa
119100101123     c                   eval      writent='N'
119200101123     c                   endif
119300101123     c
119400101123     C                   ENDSR
119500000713      *
119600000713      *--------------------------------------------------------------*
119700000713      * STAMPA - SUBROUTINE STAMPA E AGGIORNAMENTO ERRORI            *
119800000713      *--------------------------------------------------------------*
119900000713      *
120000000713     C     STAMPA        BEGSR
120100110105     c* se cappario eventi particolari, se posso inserisco nuovo cap
120200090410     c*  controlando prima che non ci sia. se c'e' non stampo errore
120300090410     c                   if        *in06 and    o13roc='S'
120400090410     c                   eval      kloc=ceploc
120500090410     c                   eval      kcap=o95cap
120600090410     c     kcep2         setll     azcep02l
120700090410     c
120800090410     c* Se non trovato provo con la località dal tisi
120900090410     c                   if        not %equal(azcep02l)
121000090410     c                   eval      kloc=o95loc
121100090410     c     kcep2         setll     azcep02l
121200090410     c                   endif
121300090410     c
121400090410     c* se non trovato provo senza la località e solo col cap
121500090410     c                   if        not %equal(azcep02l)
121600090410     c                   clear                   kloc
121700090410     c     kcep2         setll     azcep02l
121800090410     c                   endif
121900090410     c
122000090415     c* Mi riposiziono sul file
122100090410     c                   eval      kcap=cepcap
122200090410     c                   eval      kloc=ceploc
122300090410     c     kcep2         chain     azcep02l
122400090415     c
122500090415     c* se trovato esco dalla routine come se non ci fosse errore
122600090415     c                   if        %equal(azcep02l)
122700090410     c                   goto      endStampa
122800090410     c                   endif
122900090410     c                   endif
123000090410     c
123100000713      *
123200000713      * SE DEVO ANCORA STAMPARE TESTATE STAMPO
123300000713      *
123400000713     C     VIDSAL        IFEQ      'S'
123500000713     C  N38              WRITE     TESTA
123600000713      *                  ===============
123700090408     c                   if        not *in06
123800000713     C  N38              WRITE     TESPO
123900090408     c                   endif
124000000713      *                  ===============
124100000713     C                   ELSE
124200000713     C  N39              WRITE     TESTA
124300000713      *                  ===============
124400090408     c                   if        not *in06
124500000713     C  N38              WRITE     TESPO
124600090408     c                   endif
124700000713      *                  ===============
124800000713     C                   ENDIF
124900000713      *
125000000713     C                   SETON                                        39  38
125100000713      *
125200000713      * SE SONO IN OVERFLOW STAMPO UGUALMENTE TESTATE
125300000713      *
125400000713     C     *IN55         IFEQ      *ON
125500000713      *
125600000713     C                   WRITE     TESTA
125700000713      *                  ===============
125800090408     c                   if        not *in06
125900000713     C                   WRITE     TESPO
126000090408     c                   endif
126100000713      *                  ===============
126200000713      *
126300000713     C                   SETOFF                                       55
126400000713     C                   ENDIF
126500000713      *
126600000713      * PASSO DATI AL FILE DI STAMPA
126700000713      *
126800000713      * .. RESET GRASSETTI
126900000713     C                   SETOFF                                       212223
127000000713     C                   SETOFF                                       24
127100000713      *
127200000713      * .. ANNULLAMENTO/BLOCCO
127300000713     C                   CLEAR                   PANNU
127400000713     C                   CLEAR                   PBLOC
127500000713     C   01ACOFLG        IFNE      ' '
127600000713     C                   MOVE      'A'           PANNU
127700000713     C                   ENDIF
127800000713     C   02INDFLG        IFNE      ' '
127900000713     C                   MOVE      'A'           PANNU
128000000713     C                   ENDIF
128100000713     C   03CPOATB        IFNE      ' '
128200000713     C                   MOVE      'A'           PANNU
128300000713     C                   ENDIF
128400000713     C   01ACOABL        IFNE      ' '
128500000713     C                   MOVE      'B'           PBLOC
128600000713     C                   ENDIF
128700000713      *
128800000713      * .. DESCRIZIONE
128900090408     c                   clear                   prag
129000000713     C   01              MOVEL     ACORAG        PRAG
129100000713     C   02              MOVEL     VISRAG        PRAG
129200000713     C   03              MOVEL     CPORAG        PRAG
129300010403     C   04              MOVEL     SPERAG        PRAG
129400010403     C   05              MOVEL     ACRRSR        PRAG
129500090408     C   06              eval      prag=%editw(cepdev:'    /  /  ')
129600000713      *
129700000713      * .. MSG ERRORE
129800000713     C                   MOVEL     O13MSG        PERR
129900000713      *
130000000713      * ALTRI DATI:
130100000713      *
130200000713      * .. SE PIANO DEI CONTI oppure
130300000713      * .. ANAGRAFICA PROVVISORIA VISITE/OFFERTE
130400000713      *
130500000713     C     *IN01         IFEQ      *ON
130600000713     C     *IN02         OREQ      *ON
130700101125     C                   MOVEL(P)  INDKSC        PKSC
130800000713     C                   MOVEL     INDCIT        PCIT
130900000713     C                   MOVEL     INDCAE        PCAE
131000000713     C                   MOVEL     INDPRV        PPRV
131100000713     C                   MOVEL     INDSTA        PSTA
131200000713     C                   MOVEL     INDCIT        PPCIT
131300000713     C                   MOVEL     INDCAE        PPCAE
131400000713     C                   MOVEL     INDPRV        PPPRV
131500000713     C                   MOVEL     INDSTA        PPSTA
131600000713     C                   ENDIF
131700000713      *
131800000713      * .. SE ANAGRAFICA CLIENTI POTENZIALI
131900000713      *
132000000713     C     *IN03         IFEQ      *ON
132100000713     C                   MOVEL     CPOCPO        PKSC
132200000713     C                   MOVEL     CPOCIT        PCIT
132300000713     C                   MOVEL     CPOCAP        PCAE
132400000713     C                   MOVEL     CPOPRV        PPRV
132500000713     C                   MOVEL     CPONAZ        PSTA
132600000713     C                   MOVEL     CPOCIT        PPCIT
132700000713     C                   MOVEL     CPOCAP        PPCAE
132800000713     C                   MOVEL     CPOPRV        PPPRV
132900000713     C                   MOVEL     CPONAZ        PPSTA
133000000713     C                   ENDIF
133100010403      *
133200010403      * .. SE ANAGRAFICA SPEDIZIONIERI/LUOGHI
133300010403      *
133400010403     C     *IN04         IFEQ      *ON
133500010403     C                   MOVEL     SPECLI        PKSC
133600010403     C                   MOVEL     SPELOC        PCIT
133700010403     C                   MOVEL     SPECAP        PCAE
133800010403     C                   MOVEL     SPEPRO        PPRV
133900010403     C                   MOVEL     SPENAZ        PSTA
134000010403     C                   MOVEL     SPELOC        PPCIT
134100010403     C                   MOVEL     SPECAP        PPCAE
134200010403     C                   MOVEL     SPEPRO        PPPRV
134300010403     C                   MOVEL     SPENAZ        PPSTA
134400010403     C                   ENDIF
134500010403      *
134600010403      * .. SE ANAGRAFICA CLIENTI RITIRO
134700010403      *
134800010403     C     *IN05         IFEQ      *ON
134900010403     C                   MOVEL     ACRCRO        PKSC
135000010403     C                   MOVEL     ACRLOR        PCIT
135100010403     C                   MOVEL     ACRCAR        PCAE
135200010403     C                   MOVEL     ACRPRR        PPRV
135300010403     C                   MOVEL     ACRNAR        PSTA
135400010403     C                   MOVEL     ACRLOR        PPCIT
135500010403     C                   MOVEL     ACRCAR        PPCAE
135600010403     C                   MOVEL     ACRPRR        PPPRV
135700010403     C                   MOVEL     ACRNAR        PPSTA
135800010403     C                   ENDIF
135900090408      *
136000090408      * .. SE cappario utilizzi particolari
136100090408      *
136200090408     C     *IN06         IFEQ      *ON
136300090408     c                   clear                   pksc
136400090408     C                   MOVEL     ceploc        PCIT
136500090408     C                   MOVEL     cepcap        PCAE
136600090408     C                   MOVEL     cepprv        PPRV
136700090408     C                   MOVEL     cepnar        PSTA
136800090410     C                   MOVEL     ceploc        pPCIT
136900090410     C                   MOVEL     cepcap        pPCAE
137000090410     C                   MOVEL     cepprv        pPPRV
137100090410     C                   MOVEL     cepnar        pPSTA
137200090408     C                   ENDIF
137300000713      *
137400000713      * SE NON SONO RIUSCITO A CORREGGERE PULISCO
137500000713      * I DATI IN SOSTITUZIONE, AGGIORNO MESSAGGIO DI ERRORE
137600000713      * PER SECONDO TENTATIVO
137700000713      *
137800000713     C     O13ROP        IFNE      'S'
137900000713     C     O13ROC        ANDNE     'S'
138000101125     C***  O13ROL        ANDNE     'S'
138100000713     C     O13RON        ANDNE     'S'
138200000713     C                   CLEAR                   PPCIT
138300000713     C                   CLEAR                   PPCAE
138400000713     C                   CLEAR                   PPPRV
138500000713     C                   CLEAR                   PPSTA
138600070116     C     TENTATIVO     IFEQ      'SECONDO1'
138700000713     C                   CLEAR                   PERR
138800070116     C                   EVAL      PERR='C.A.P. o località obsoleti'
138900101122     c                   if        o95msg<>*blanks
139000101122     c                   eval      perr=%trim(perr)+ '-' +
139100101122     c                             %subst(o95msg:10:60)
139200101122     c                   endif
139300110107     C                   ENDIF
139400110107     c* Aggiungo numero sped in WFALA
139500110107     c                   if        *in07   and totspe>0
139600110107     c                   eval      alfaspe=%editc(totspe:'Z')
139700110107     c
139800110107     c                   eval      perr=%trim(perr)+ 'sped in WFALA: '+
139900110107     c                             %trim(alfaspe)
140000110107     c                   endif
140100101122     c
140200000713     C                   ELSE
140300000713      *
140400000713      * ALTRIMENTI MUOVO CAMPI DA DS A FILE DI STAMPA
140500000713      * EVIDENZIANDO IN GRASSETTO QUELLO ERRATO/AGGIORNATO,
140600000713      * AGGIORNO MESSAGGIO DI ERRORE PER SECONDO TENTATIVO
140700000713      *
140800070116     C     TENTATIVO     IFEQ      'SECONDO1'
140900000713     C                   CLEAR                   PERR
141000090410     C  n06              EVAL      PERR='C.A.P. obsoleto: modificato'
141100090410     C   06              EVAL      PERR='C.A.P. obsoleto: aggiunto  '
141200101122     c                   if        o95msg<>*blanks
141300101122     c                   eval      perr=%trim(perr)+ '-' +
141400101122     c                             %subst(o95msg:10:60)
141500101122     c                   endif
141600000713     C                   ENDIF
141700070116      *
141800070116     C     TENTATIVO     IFEQ      'SECONDO2'
141900070116     C                   CLEAR                   PERR
142000090410     C  n06              EVAL      PERR='Provincia/CAP modificata'
142100090410     C   06              EVAL      PERR='Provincia/CAP aggiunta  '
142200070116     C                   ENDIF
142300110119     c
142400110119     C  n06TENTATIVO     IFEQ      'PRIMO   '
142500150206     C                   EVAL      PERR='CAP modificato per loc.Inesistente +
142600150206     c                             o CAP generico'
142700110119     c                   endif
142800000713      *
142900000713     C     O13ROP        IFEQ      'S'
143000000713     C                   MOVEL     O95PRV        PPPRV
143100000713     C                   SETON                                        23
143200101123     c                   eval      contaprv=contaprv+1
143300000713     C                   ENDIF
143400000713      *
143500000713     C     O13ROC        IFEQ      'S'
143600000713     C                   MOVEL     O95CAP        PPCAE
143700000713     C                   SETON                                        22
143800101123     c                   eval      contacap=contacap+1
143900000713     C                   ENDIF
144000000713      *
144100000713     C     O13ROL        IFEQ      'S'
144200000713     C                   MOVEL     O95LOC        PPCIT
144300000713     C                   SETON                                        21
144400101123     c                   eval      contaloc=contaloc+1
144500000713     C                   ENDIF
144600000713      *
144700000713     C     O13RON        IFEQ      'S'
144800000713     C                   MOVEL     O95NAR        PPSTA
144900000713     C                   SETON                                        24
145000101123     c                   eval      contanar=contanar+1
145100000713     C                   ENDIF
145200000713      *
145300000713      * SE A VIDEO SCELTA "A" DI AGGIORNAMENTO, AGGIORNO
145400000713      * (SIA INDIRIZZI)
145500000713      * (SIA FLAGS TRASMISSIONE)
145600000713      *
145700000713IF   C     VIDTPE        IFEQ      'A'
145800000713      *
145900110111      * .. PIANO DEI CONTI
146000000713     C     *IN01         IFEQ      *ON
146100110111     c/exec sql
146200110111     c+ update cnind00f set indcae=:ppcae, indprv=:ppprv
146300110111     c+ where  indkut=1 and indkcc=: vidkcu and indksc=:indksc
146400110111     c/end-exec sql
146500110111     c/exec sql
146600110111     c+ update cnaco00f set acodtr=0, acoftr=' '
146700110111     c+ where  acokut=1 and acokcc=: vidkcu and acoksc=:indksc
146800110111     c/end-exec sql
146900110111     C***                CLEAR                   ACODTR
147000110111     C***                CLEAR                   ACOFTR
147100110111     C***                UPDATE    CNACO000
147200110111      *                  ------    ========
147300110111     C                   ENDIF
147400110111     c
147500110111      * .. ANAGRAFICA PROVVISORIA VISITE/OFFERTE
147600110111     C     *IN02         IFEQ      *ON
147700110111     c/exec sql
147800110111     c+ update tfind00f set indcae=:ppcae, indprv=:ppprv
147900110111     c+ where  indkut=1 and indkcc=: indkcc and indksc=:indksc
148000110111     c/end-exec sql
148100110111     C***                MOVEL     PPCIT         INDCIT
148200110111     C**                 MOVEL     PPCAE         INDCAE
148300110111     C**                 MOVEL     PPPRV         INDPRV
148400061109     C***                MOVEL     PPSTA         INDSTA
148500000713      *                  ------    ======
148600110111     C**                 UPDATE    CNIND000
148700000713      *                  ------    ========
148800110111     C                   ENDIF
148900000713      *
149000000713      * .. ANAGRAFICA CLIENTI POTENZIALI
149100000713     C     *IN03         IFEQ      *ON
149200101125     c     cponrr        chain     tncpo00f
149300101125   2ac                   if        %found
149400160831     c                   exsr      ImmaginePrima
149500061109     C***                MOVEL     PPCIT         CPOCIT
149600000713     C                   MOVEL     PPCAE         CPOCAP
149700061116     C                   MOVEL     PPPRV         CPOPRV
149800061109     C***                MOVEL     PPSTA         CPONAZ
149900040804     c                   Move      *date         CpoDtr
150000101125     C                   UPDATE    TNCPOfis
150100160831     c                   exsr      ScriviVar
150200101125     c                   endif
150300000713      *                  ------    ========
150400000713     C                   ENDIF
150500010403      *
150600010403      * .. ANAGRAFICA SPEDIZIONIERI/LUOGHI
150700010403     C     *IN04         IFEQ      *ON
150800061109     C***                MOVEL     PPCIT         SPELOC
150900010403     C                   MOVEL     PPCAE         SPECAP
151000061116     C                   MOVEL     PPPRV         SPEPRO
151100061109     C***                MOVEL     PPSTA         SPENAZ
151200040715     C                   move      *date         SPEDTR
151300010403     C                   UPDATE    FNSPE000
151400010403      *                  ------    ========
151500010403     C                   ENDIF
151600010403      *
151700010403      * .. ANAGRAFICA CLIENTI RITIRO
151800010403     C     *IN05         IFEQ      *ON
151900061109     C***                MOVEL     PPCIT         ACRLOR
152000010403     C                   MOVEL     PPCAE         ACRCAR
152100061116     C                   MOVEL     PPPRV         ACRPRR
152200061109     C***                MOVEL     PPSTA         ACRNAR
152300040910     C                   move      *date         ACRDTR
152400010403     C                   UPDATE    FNACR000
152500010403      *                  ------    ========
152600010403     C                   ENDIF
152700090410      *
152800090410      * .. cappario eventi particolari
152900090410     C     *IN06         IFEQ      *ON
153000090410     C                   MOVEL     PPCAE         cepcap
153100090410     C                   MOVEL     PPPRV         cepprv
153200090410     C                   write     azcep000
153300090410      *                  ------    ========
153400090410     C                   ENDIF
153500000713      *
153600110107     C                   ENDIF
153700000713      *
153800000713      *
153900000713     C                   ENDIF
154000000713      *
154100000713      * STAMPO DETTAGLIO ERRORI
154200000713      *
154300000713     C                   WRITE     DETT
154400000713      *                  ==============
154500000713      *
154600090410     C     endStampa     ENDSR
154700110107      *--------------------------------------------------------------*
154800110107      * Conto le spedizioni di quel CAP su WFALA
154900110107      *--------------------------------------------------------------*
155000110107     C     contWFALA     BEGSR
155100110107      *
155200110107     c                   clear                   wrkgetlista
155300110107     c                   eval      wrkgetlista =
155400110107     c                             'select  sum(alatos)   +
155500110107     c                             from '+ libWFALA + '/WFALA00F   +
155600110107     c                             where alacap =''' +  cplcap +
155700110107     c                             ''' and alaloc =''' + cplloc +
155800110107     c                             ''' and alanaz =''' + cplnar +
155900110107     c                             ''' and alaprv =''' + cpcprv + ''''
156000110107     C/EXEC SQL
156100110107     C+ PREPARE s2 FROM :wrkgetlista
156200110107     C/END-EXEC
156300110107     C
156400110107     C/EXEC SQL
156500110107     C+ DECLARE A2 CURSOR FOR S2
156600110107     C/END-EXEC
156700110107     C
156800110107     C/EXEC SQL
156900110107     C+ OPEN a2
157000110107     C/END-EXEC
157100110107
157200110107      *?Leggo il file
157300110107     c
157400110107     C/EXEC SQL
157500110107     C+ FETCH NEXT FROM a2 INTO :totspe
157600110107     C/END-EXEC
157700110107
157800110107     c                   if        sqlcod < 0
157900110107     c                   clear                   totspe
158000110107     c                   endif
158100110107     C/EXEC SQL
158200110107     C+ close a2
158300110107     C/END-EXEC
158400110107     c
158500110107     c                   ENDSR
158600160831      /free
158700160831       //--------------------------------------------------------------
158800160831       //?Scrivo l'immagine Precedente anagrafica Potenziale
158900160831       //--------------------------------------------------------------
159000160831       BEGSR ImmaginePrima;
159100160831
159200160831         clear TRMK30DS;
159300160831         IMK30immag = 'P';
159400160831         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
159500160831
159600160831       ENDSR;
159700160831
159800160831       //--------------------------------------------------------------
159900160831       //?Scrivo Storico Variazioni.
160000160831       //--------------------------------------------------------------
160100160831       BEGSR ScriviVar;
160200160831
160300160831         clear TRMK30DS;
160400160831         IMK30pru = knmus;
160500160831         IMK30noj = knmeb;
160600160831         IMK30pgm = 'FNLV75R';
160700160831         IMK30immag = 'D';
160800160831         IMK30cta = 'M';
160900160831
161000160831         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
161100160831
161200160831       ENDSR;
161300160831      /end-free
161400110107**
161500110107/YYYYYYYYYY) OBJTYPE(*FILE)
