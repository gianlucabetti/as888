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
009400101125
009500000705      ****************************************************************
009600000705      * RIEPILOGO INDICATORI:                                        *
009700000705      ****************************************************************
009800000712      * 01 - ELABORO PIANO DEI CONTI                                 *
009900000712      * 02 - ELABORO ANAGRAFICA PROVVISORIA VISITE/OFFERTE           *
010000000712      * 03 - ELABORO ANAGRAFICA CLIENTI POTENZIALI                   *
010100010403      * 04 - ELABORO ANAGRAFICA SPEDIZIONIERI/LUOGHI                 *
010200010403      * 05 - ELABORO ANAGRAFICA CLIENTI RITIRO                       *
010300000712      * 21 - GRASSETTO LOCALITA'                                     *
010400000712      * 22 - GRASSETTO CAP                                           *
010500000712      * 23 - GRASSETTO PROVINCIA                                     *
010600000712      * 24 - GRASSETTO NAZIONE                                       *
010700000712      * 31 - COMODO                                                  *
010800000713      * 38 - OFF=DEVO STAMPARE TESTATA P.O.                          *
010900000713      * 39 - OFF=DEVO STAMPARE TESTATA TERMINAL                      *
011000000712      * 55 - OVERFLOW DI STAMPA                                      *
011100000713      * 98 - LETTURA FILES PRIMARI DA CONTROLLARE                    *
011200020417      * 99 - LETTURA azcae01l                                        *
011300000705      ****************************************************************
011400000704      *
011500000705      ****************************************************************
011600000704      *                                                              *
011700000704      *         O P E R A Z I O N I   I N I Z I A L I                *
011800000704      *                                                              *
011900000704      ****************************************************************
012000000704      *
012100000704     C     *ENTRY        PLIST
012200000704     C                   PARM                    KPJBA
012300000712     C                   MOVEL     KPJBU         PARAM
012400000712      *
012500000713      * PARAMETRI UTENTE (ANCHE DA STAMPARE)
012600000712      *
012700000712     C                   Z-ADD     1             CODUT
012800000712     C                   CALL      'X§PARUT'
012900000712     C                   PARM                    UT§DSE
013000000712     C                   MOVEL     RAGUT         PRSUT
013100000712     C                   MOVEL     REC80         CNCR80
013200020513     c                   if        simtpp = '2' or simtpp = *blanks
013300020513     c                   move      simpou        pfel
013400000712     C                   ELSE
013500000712     C                   MOVE      SIMFEL        PFEL
013600000712     C                   ENDIF
013700100429     c* data del giorno
013800100429     c                   eval      dataiso=datasys
013900100429     c                   eval      Alfaoggi=(%editc(%dec(dataiso):'X'))
014000000712      *
014100000712      * RICERCO CONTI
014200000712      *
014300000712     C                   DO        50            X                 2 0
014400000712     C                   MOVE      TCU(X)        TCUDS
014500000712     C     F56           CABNE     'CG'          END1
014600000712     C     F4            COMP      '1'                                    21
014700000712     C     F4            COMP      '2'                                    22
014800000712     C     F4            COMP      '3'                                    23
014900000712     C     F4            COMP      '6'                                    27
015000000712      ** 1 CLIENTI   21
015100000712      ** 2 FORNITORI 22
015200000712      ** 3 AGENTI    23
015300000712     C     F3            COMP      '0'                                242425
015400000712     C     F3            COMP      'I'                                    26
015500000712      ** 0 ITALIA   25
015600000712      ** 1 ESTERO   24
015700000712      * I CAPO CONTO IVA
015800000712     C   21
015900000712     CAN 24              Z-ADD     KCU(X)        KCE               4 0
016000000712     C   21
016100000712     CAN 25              Z-ADD     KCU(X)        KCI               4 0
016200000712     C   22
016300000712     CAN 24              Z-ADD     KCU(X)        KFE               4 0
016400000712     C   22
016500000712     CAN 25              Z-ADD     KCU(X)        KFI               4 0
016600000712     C   23
016700000712     CAN 24              Z-ADD     KCU(X)        KAE               4 0
016800000712     C   23
016900000712     CAN 25              Z-ADD     KCU(X)        KAI               4 0
017000000712     C   26              Z-ADD     KCU(X)        KIVA              4 0
017100000712     C   27              Z-ADD     KCU(X)        KBNA              4 0
017200000712     C     END1          TAG
017300000712     C                   END
017400000712     C                   SETOFF                                       212223
017500000712     C                   SETOFF                                       2425
017600000712     C                   SETOFF                                       2627
017700000712      *
017800020417      * ACCESSO A azcae01l
017900000712      *
018000020417     C     Kazcae        KLIST
018100020417     C                   KFLD                    kepa              1
018200101126     C     Kcpc          KLIST
018300101126     C                   KFLD                    kv36              5 0
018400101126     C                   KFLD                    cplnar
018500101126     C                   KFLD                    cplcap
018600000712      *
018700020417     C                   EVAL      kepa = 'P'
018800101126     C                   EVAL      kv36 = 36
018900000713      *
019000000713      * ACCESSO A CNACO00F
019100000713      *
019200000713     C     KCNACO        KLIST
019300000713     C                   KFLD                    INDKUT
019400000713     C                   KFLD                    INDKCC
019500000713     C                   KFLD                    INDKSC
019600000712      *
019700000712      * ACCESSO A CNIND00F
019800000712      *
019900000712     C     KCNIND        KLIST
020000020417     C                   KFLD                    KKUT              1 0
020100000712     C                   KFLD                    VIDKCU
020200000712     C                   KFLD                    KKSC              7 0
020300000712      *
020400000712     C     KCNIN2        KLIST
020500000712     C                   KFLD                    KKUT
020600000712     C                   KFLD                    VIDKCU
020700020417      *
020800020417     C                   EVAL      KKUT = 1
020900000712      *
021000000712      * ACCESSO A TFIND00F
021100000712      *
021200000712     C     KTFIND        KLIST
021300000712     C                   KFLD                    KKUT
021400000712     C                   KFLD                    KCI
021500000712     C                   KFLD                    VISNRV
021600000713      *
021700000713      * ACCESSO A TNCPO08L
021800000713      *
021900000713     C     KTNCPO        KLIST
022000000713     C                   KFLD                    KKSC
022100000713     C                   KFLD                    KFLT              3 0
022200010403      *
022300010403      * ACCESSO A FNSEP01L
022400010403      *
022500010403     C     KFNSPE        KLIST
022600010403     C                   KFLD                    KFLS              1
022700010403     C                   KFLD                    KKSC
022800090410     C     Kcep2         KLIST
022900090410     C                   KFLD                    cepdev
023000090410     C                   KFLD                    cepfil
023100090410     C                   KFLD                    cepnar
023200090410     C                   KFLD                    kcap
023300090410     C                   KFLD                    kloc
023400090410     C                   KFLD                    cepcev
023500101123     C     KCPMOD        KLIST
023600101123     C                   KFLD                    kprvmod
023700101123     C                   KFLD                    kcapmod
023800101123     C                   KFLD                    klocmod
023900020417
024000020417     c                   eval      wdata = *date
024100020417
024200000712      *
024300000712      * EMETTO PRIMA PAGINA CON PARAMETRI ELABORAZIONE
024400000712      *
024500000712      * .. ORARIO
024600000712     C                   TIME                    WORA              6 0
024700000712     C                   MOVEL     WORA          PHH
024800000712     C                   MOVE      WORA          PSS
024900000712     C                   MOVEL     WORA          WORK04            4
025000000712     C                   MOVE      WORK04        PMM
025100000712      *
025200000712      * .. ANAGRAFICO DA CONTROLLARE
025300000712     C                   IF        VIDKCU <> 0
025400000712     C                   CLEAR                   WORK27           27
025500000712     C                   EVAL      WORK27='Piano dei Conti: Capoconto:'
025600000712     C                   MOVE      VIDKCU        WORK04
025700000712     C     WORK27        CAT       WORK04:1      PANAGR
025800000712     C                   ENDIF
025900000712      *
026000000712     C                   IF        VIDVIO = 'S'
026100000712     C                   EVAL      PANAGR='Anagrafica Provvis. Visite/Offerte'
026200000712     C                   ENDIF
026300000712      *
026400000712     C                   IF        VIDCLP = 'S'
026500000712     C                   EVAL      PANAGR='Anagrafica Clienti Potenziali'
026600000712     C                   ENDIF
026700010403      *
026800010403     C                   IF        VIDSPE = 'S'
026900010403     C                   EVAL      PANAGR='Anagrafica Spedizionieri/Luoghi'
027000010403     C                   ENDIF
027100010403      *
027200010403     C                   IF        VIDACR = 'S'
027300010403     C                   EVAL      PANAGR='Anagrafica Clienti Ritiri'
027400010403     C                   ENDIF
027500090408     C                   IF        VIDcep = 'S'
027600090408     C                   EVAL      PANAGR='Cappario Eventi particolari:AZCEP'
027700090408     C                   ENDIF
027800100224     C***                IF        VIDcpp = 'S'
027900100224     C***                EVAL      PANAGR='Cappario Utiliz.particolari:AZCPP'
028000100224     C***                ENDIF
028100000712      *
028200000712      * .. SALTO PAGINA
028300000712     C                   IF        VIDSAL = 'S'
028400000712     C                   EVAL      PSINOS = 'SI'
028500000712     C                   ELSE
028600000712     C                   EVAL      PSINOS = 'NO'
028700000712     C                   ENDIF
028800000712      *
028900000712      * .. TIPO ELABORAZIONE
029000000712     C                   IF        VIDTPE = 'S'
029100000712     C                   EVAL      PTIPEL='Solo Stampa'
029200000712     C                   ELSE
029300000712     C                   EVAL      PTIPEL='Aggiornamento CAP in Automatico'
029400000712     C                   ENDIF
029500100224      *
029600100224      * Se immessa --> data di riferimento
029700100224     c                   if        viddri>0
029800100224     c                   seton                                        28
029900100224     c                   endif
030000101209
030100110105     c* Se non è richiesto nulla, provo ad aprire AZCPLxxx di work
030200101209     c                   if        vidvio=' ' and vidclp=' ' and
030300101209     c                             vidspe=' ' and vidacr=' ' and
030400110105     c                             vidcep=' ' and vidfile<>*blanks
030500110105     c                   eval      %subst(wazcpl:1:10)=vidlib
030600110105     c                   eval      %subst(wazcpl:12:10)=vidfile
030700110105     c                   open(e)   azcpl00F
030800110105     C                   EVAL      PANAGR='Località da cancellare da AZCPL '
030900101209     c                   endif
031000000712      *
031100000712     C                   WRITE     PRIPAG
031200000712      *                  ================
031300000713      *
031400000713      * APRO FILES VISITE/OFFERTE SOLO SE NON SONO IN SEDE
031500000713      *
031600000713     C     VIDVIO        IFEQ      'S'
031700101124     C                   OPEN(E)   TFIND00F
031800101124     c                   if        %error
031900101124     c                   eval      %subst(wTFIND:7:4)='GRPF'
032000101124     c                   open      TFIND00F
032100101124     c                   endif
032200000713     C                   ENDIF
032300101123     c
032400101123     c* Se esiste apro il file WACPMOD0F
032500101123     c                   open(e)   wacpmod1l
032600101123     c                   if        %error
032700101123     c                   eval      %subst(wcpmod:7:4)='GRP '
032800101123     c                   open(e)   wacpmod1l
032900101124     c                   endif
033000000713      *
033100000705      ****************************************************************
033200000704      *                                                              *
033300000704      *         R O U T I N E   P R I N C I P A L E                  *
033400000704      *                                                              *
033500000704      ****************************************************************
033600000704      *
033700020417      * LEGGO azcae01l PER OGNI TERMINAL DI PARTENZA VALIDO
033800000712      *
033900020417     C     kazcae        SETLL     azcae01l
034000000712      *
034100000712      *==============================================================*
034200000712      *                      CICLO 1                                 *
034300000712      *==============================================================*
034400000712      *
034500000712     C                   DO        *HIVAL
034600000712      *
034700020417     C     kazcae        READE     azcae01l                               99
034800000712     C   99              LEAVE
034900020417      * annullato
035000020417     c                   if        caeatb <> *blanks
035100020417     c                   iter
035200020417     c                   endif
035300020417      * controllo la data validità
035400020417     c                   if        caedde > wdata or caedsc < wdata
035500020417     c                   iter
035600020417     c                   endif
035700020417      * elaboro solo se è il terminal di partenza
035800020417     c                   if        caetfp <> caetfe
035900020417     c                   iter
036000020417     c                   endif
036100020417      * se sono in filiale elaboro solo se terminal di partenza = simfel
036200020417     c                   if        simfel > 0 and caetfp <> simfel
036300020417     c                   iter
036400020417     c                   endif
036500000712      *
036600000712      * CARICO L1 PER IL TERMINAL DI PARTENZA LETTO E IMPOSTO TESTATA
036700000712      *
036800000712     C                   EXSR      CARL1
036900000712      *                  ---------------
037000000712      * LEGGO LA £1
037100000712      *
037200000712      *==============================================================*
037300000712      *                      CICLO 2                                 *
037400000712      *==============================================================*
037500000712      *
037600000712     C     1             DO        30            Y                 3 0
037700000712      *
037800000712     C     L1(Y)         IFGT      0
037900000712      *
038000000712      * SE <> DA ZERO SCELGO SOLO IL P.O. IMMESSO A VIDEO
038100000712      *
038200000712     C     VIDFIL        IFNE      0
038300000712     C     VIDFIL        ANDNE     L1(Y)
038400000712     C                   ITER
038500000712     C                   ENDIF
038600000712      *
038700000712      * IMPOSTO TESTATA PUNTO OPERATIVO
038800000712      *
038900000712     C                   EVAL      PPUNTO = L1(Y)
039000100429     C     L1(Y)         CHAIN     AZORG01L
039100100429     c                   if        %found(azorg01l)
039200100429     C                   EVAL      PDESPU = ORGDES
039300100429     c                   else
039400100429     C                   EVAL      PDESPU = *BLANKS
039500100429     c                   endif
039600000713     C                   SETOFF                                       38
039700000712      *
039800000712      * ESEGUO SUBROUTINES IN BASE AL TIPO ELABORAZIONE
039900000712      *
040000000712      * .. ELABORAZIONE 01 - PIANO DEI CONTI
040100000712      *
040200000712     C     VIDKCU        IFNE      0
040300000712     C                   SETON                                          01
040400000712     C                   EXSR      ELAB01
040500000712      *                  ----------------
040600000712     C                   ENDIF
040700000712      *
040800000712      * .. ELABORAZIONE 02 - ANAGRAFICA PROVVISORIA VISITE OFFERTE
040900000712      *
041000000712     C     VIDVIO        IFEQ      'S'
041100000712     C                   SETON                                          02
041200000712     C                   EXSR      ELAB02
041300000712      *                  ----------------
041400000712     C                   ENDIF
041500000712      *
041600000712      * .. ELABORAZIONE 03 - ANAGRAFICA CLIENTI POTENZIALI
041700000712      *
041800000712     C     VIDCLP        IFEQ      'S'
041900000712     C                   SETON                                          03
042000000712     C                   EXSR      ELAB03
042100000712      *                  ----------------
042200000712     C                   ENDIF
042300010403      *
042400010403      * .. ELABORAZIONE 04 - ANAGRAFICA SPEDIZIONIERI/LUOGHI
042500010403      *
042600010403     C     VIDSPE        IFEQ      'S'
042700010403     C                   SETON                                          04
042800010403     C                   EXSR      ELAB04
042900010403      *                  ----------------
043000010403     C                   ENDIF
043100010403      *
043200010403      * .. ELABORAZIONE 05 - ANAGRAFICA CLIENTI RITIRO
043300010403      *
043400010403     C     VIDACR        IFEQ      'S'
043500010403     C                   SETON                                          05
043600010403     C                   EXSR      ELAB05
043700010403      *                  ----------------
043800010403     C                   ENDIF
043900090408      *
044000000712     C                   ENDIF
044100000712      *
044200000712     C                   ENDDO
044300000712      *
044400000712      *==============================================================*
044500000712      *                    FINE CICLO 2                              *
044600000712      *==============================================================*
044700000712      *
044800000712     C                   ENDDO
044900000712      *
045000000712      *==============================================================*
045100000712      *                    FINE CICLO 1                              *
045200000712      *==============================================================*
045300090408      *
045400090408      * .. ELABORAZIONE 06 - Cappario eventi particolari
045500090408      *                      non si elabora in base al p.o.
045600090408      *                      ma tutto insieme
045700090408      *
045800090408     C     VIDcep        IFEQ      'S'
045900090408     C                   SETON                                          06
046000090408     C                   EXSR      ELAB06
046100090408      *                  ----------------
046200090408     C                   ENDIF
046300101126     c
046400101126     c
046500101126     c                   if        vidvio=' ' and vidclp=' ' and
046600101126     c                             vidspe=' ' and vidacr=' ' and
046700110105     c                             vidcep=' ' and %open(azcpl00F)
046800101126     C                   SETON                                          0607
046900110105     c                   open      azcpc01l
047000110107     c* Verifico in quale libreria esiste WFALA
047100110107     c                   clear                   comman
047200110107     c                   z-add     80            lenght
047300110107     c                   movel     'CHKOBJ OBJ(' comman
047400110107     c                   eval      %subst(comman:12:10)='GAITRAAZP'
047500110107     c                   eval      %subst(chk(1):2:10)='WFALA00F'
047600110107     c                   eval      comman=%trim(comman)+chk(1)
047700110107     c                   Call      'QCMDEXC'                            99
047800110107     c                   Parm                    comman           80
047900110107     c                   Parm                    lenght           15 5
048000110107     c
048100110107     c                   if        *in99
048200110107     c                   eval      libWFALA='GAITRAAZM'
048300110107     c                   else
048400110107     c                   eval      libWFALA='GAITRAAZP'
048500110107     c                   endif
048600101126     c
048700110105     c                   read      azcpl00F
048800101126     c                   dow       not %eof
048900101126     c     kcpc          chain     azcpc01l
049000101126     c                   eval      ceploc=cplloc
049100101126     c                   eval      cepprv=cpcprv
049200101126     c                   eval      cepcap=cplcap
049300101126     c                   eval      cepnar=cplnar
049400101126     c                   eval      TENTATIVO='SECONDO2'
049500110107     c* Intanto verifico in WFAKLA quante spedizioni ci sono
049600110107     c                   exsr      contWFALA
049700110107     c
049800101126     C                   EXSR      CHKCAP
049900101126     c
050000110105     c                   read      azcpl00f
050100101126     c                   enddo
050200101126     c                   endif
050300090408      *
050400000704      *
050500000704      * FINE PROGRAMMA
050600000704      *
050700000712     C                   WRITE     FINEST
050800000712      *                  ================
050900000712      *
051000000713     C     VIDVIO        IFEQ      'S'
051100000713     C                   CLOSE     TFIND00F
051200000713     C                   ENDIF
051300000721     c* chiudo pgm
051400000721     c                   clear                   dslv13
051500000721     c                   clear                   dssi95
051600000721     c                   movel     'C'           i13tla
051700000721     C                   CALL      'FNLV13R'
051800000721     C                   PARM                    KPJBA
051900000721     C                   PARM                    DSLV13
052000000721     C                   PARM                    DSSI95
052100000721     C                   PARM                    flgbac            1
052200000713      *
052300000711     C                   SETON                                        LR
052400000704      *
052500000705      ****************************************************************
052600000704      *                                                              *
052700000704      *         S U B R O U T I N E S                                *
052800000704      *                                                              *
052900000704      ****************************************************************
053000000704      *
053100000704      *--------------------------------------------------------------*
053200000712      * CARL1  - SUBROUTINE CARICAMENTO L1                           *
053300000704      *--------------------------------------------------------------*
053400000712      *
053500000712     C     CARL1         BEGSR
053600000712      *
053700020417      * IMPOSTO TESTATA E CARICO schiera FILIALI GESTITE £1
053800000712      *
053900000712      * .. TERMINAL PARTENZA IN STAMPA (CON DECODIFICA)
054000000712      *
054100000712     C                   SETOFF                                       39
054200020417     c                   if        simfel = 0
054300020417     C                   MOVEL     caetfp        PTERMP
054400020417     c                   else
054500020417     c                   movel     pfel          ptermp
054600020417     c                   endif
054700000712     C     PTERMP        CHAIN     AZORG01L                           31
054800000712     C  N31              EVAL      PDESTE = ORGDES
054900000712     C   31              EVAL      PDESTE = *BLANKS
055000000712      *
055100000712      * .. CARICO L1
055200000712      *
055300000712     C                   CLEAR                   DSUL06
055400000712     C                   MOVE      '£1'          D06COD
055500000712     C                   MOVEL     PTERMP        D06KEY
055600000712     C                   MOVEL     DSUL06        KPJBU
055700000712     C*
055800000712     C                   CALL      'TRUL06R'
055900000712     C                   PARM                    KPJBA
056000000712     C                   MOVEL     KPJBU         DSUL06
056100000712     C                   MOVEA     LIN           L1
056200000712     C*
056300000712     C                   ENDSR
056400000712      *
056500000712      *--------------------------------------------------------------*
056600000712      * ELAB01 - SUBROUTINE CONTROLLO PIANO DEI CONTI                *
056700000712      *--------------------------------------------------------------*
056800000712      *
056900000712     C     ELAB01        BEGSR
057000000712      *
057100000713      * LETTURA CNIND00F (ANAGRAFICA INDIRIZZI)
057200000712      *
057300000712      * .. DAL P.O.
057400000712     C                   CLEAR                   KKSC
057500000712     C                   MOVEL     PPUNTO        KKSC
057600000712      * .. AL P.O.
057700000712     C                   Z-ADD     9999999       WCODFIN           7 0
057800000712     C                   MOVEL     PPUNTO        WCODFIN
057900000712      *
058000000712     C     KCNIND        SETLL     CNIND00F
058100000712     C                   DO        *HIVAL
058200110111     c**                 if        vidtpe='S'
058300110111     C**   KCNIN2        READE(n)  CNIND00F                               98
058400110111     c**                 else
058500090408     C     KCNIN2        READE     CNIND00F                               98
058600110111     c**                 endif
058700090408     c
058800000712      *
058900000712      * ESCO PER FINE FILE O PER SUPERO LIMITI P.O.
059000000712      *
059100000712     C   98              LEAVE
059200000712     C     INDKSC        IFGT      WCODFIN
059300000712     C                   LEAVE
059400000712     C                   ENDIF
059500000712      *
059600000712      * SE CAPOCONTO E' "CLIENTI" ESCLUDO XXX8888 E XXX9999
059700000712      *
059800000712     C     VIDKCU        IFEQ      KCI
059900000712     C                   MOVE      INDKSC        WORKNUM4          4 0
060000000712     C     WORKNUM4      IFEQ      8888
060100000712     C     WORKNUM4      OREQ      9999
060200000712     C                   ITER
060300000712     C                   ENDIF
060400000712     C                   ENDIF
060500000713      *
060600000713      * ESCLUDO SE NON HO TROVATO IL RELATIVO RECORD DI CNACO00F
060700000713      *
060800110111     c**                 if        vidtpe='S'
060900110111     C**   KCNACO        CHAIN(N)  CNACO00F                           31
061000110111     c**                 else
061100110111     C     KCNACO        CHAIN     CNACO00F                           31
061200110111     c**                 endif
061300000713     C   31              ITER
061400000712      *
061500000712      * ESEGUO L'EFFETTIVO CONTROLLO DEL CAP E SE ERRATO STAMPO
061600000712      *
061700000712     C                   EXSR      CHKCAP
061800000712      *                  ----------------
061900000712      *
062000000712     C                   ENDDO
062100000712      *
062200000712     C                   ENDSR
062300000712      *
062400000712      *--------------------------------------------------------------*
062500000712      * ELAB02 - SUBROUTINE CONTROLLO ANAGR. PROVVIS. VISITE/OFFERTE *
062600000712      *--------------------------------------------------------------*
062700000712     C     ELAB02        BEGSR
062800000712      *
062900100806      * LETTURA TIVIS00F (ANAGRAFICA VISITE CLIENTI)
063000000712      *
063100000712      * .. DAL P.O.
063200000712     C                   CLEAR                   KKSC
063300000712     C                   MOVEL     PPUNTO        KKSC
063400000712      * .. AL P.O.
063500000712     C                   Z-ADD     9999999       WCODFIN           7 0
063600000712     C                   MOVEL     PPUNTO        WCODFIN
063700000712      *
063800100429     c
063900100806     c* eseguo SQL sulla TIVIS
064000100429     c                   eval      wrkgetlista =
064100110803     c                             'select VISNRV, VISRAG from '+
064200110803     c                             'TIVIS00F  +
064300100429     c                             where viscmm>=' +
064400100429     c                             %editc(kksc:'X') + ' and viscmm<=' +
064500100429     c                             %editc(wcodfin:'X')
064600100429     C/EXEC SQL
064700100429     C+ PREPARE s1 FROM :wrkgetlista
064800100429     C/END-EXEC
064900100429     C
065000100429     C/EXEC SQL
065100100429     C+ DECLARE A1 CURSOR FOR S1
065200100429     C/END-EXEC
065300100429     C
065400100429     C/EXEC SQL
065500100429     C+ OPEN a1
065600100429     C/END-EXEC
065700100429
065800100429      *?Leggo il file
065900100429     c                   do        *hival
066000100429     C/EXEC SQL
066100100429     C+ FETCH NEXT FROM a1 INTO :VISNRV, :VISRAG
066200100429     C/END-EXEC
066300100429     c                   select
066400100429
066500100429     c                   when      sqlcod = 100
066600100429     c                   leave
066700100429     c                   when      sqlcod < 0
066800100429     c                   seton                                        H1
066900100429     c                   leave
067000100429     c                   other
067100100429     c
067200110111     c**                 if        vidtpe='S'
067300110111     C**   KTFIND        CHAIN(n)  TFIND00F                           31
067400110111     c**                 else
067500100429     C     KTFIND        CHAIN     TFIND00F                           31
067600110111     c**                 endif
067700100429     C   31              ITER
067800100429      *
067900100429      * ESEGUO L'EFFETTIVO CONTROLLO DEL CAP E SE ERRATO STAMPO
068000100429     C                   EXSR      CHKCAP
068100100429     c                   endsl
068200100429
068300100429     c                   enddo
068400000712      *
068500100429     C/EXEC SQL
068600100429     C+ CLOSE a1
068700100429     C/END-EXEC
068800100429     c
068900000712     C                   ENDSR
069000000712      *
069100000712      *--------------------------------------------------------------*
069200000712      * ELAB03 - SUBROUTINE CONTROLLO ANAGRAFICA CLIENTI POTENZIALI  *
069300000712      *--------------------------------------------------------------*
069400000713      * NB: ESEGUO 2 CICLI DI LETTURA SUL FILE PER RISPARMIARE SULLA *
069500000713      *     CREAZIONE DI NUOVE VISTE LOGICHE                         *
069600000713      *---------------------------------------------------------------
069700000713      *
069800000712     C     ELAB03        BEGSR
069900000713      *
070000000713      * LETTURA TNCPO08L (ANAGRAFICA CLIENTI POTENZIALI)
070100000713      * .. 1° CICLO PER KKSC=0 E KFLT=P.O.
070200000713      *
070300000713     C                   EVAL      KKSC = 0
070400000713     C                   EVAL      KFLT = PPUNTO
070500000713      *
070600000713     C     KTNCPO        SETLL     TNCPO08L
070700000713     C                   DO        *HIVAL
070800090408     c                   if        vidtpe='S'
070900090408     C     KTNCPO        READE(n)  TNCPO08L                               98
071000090408     c                   else
071100090408     C     KTNCPO        READE     TNCPO08L                               98
071200090408     c                   endif
071300000713      *
071400000713      * ESCO PER FINE LETTURA
071500000713      *
071600000713     C   98              LEAVE
071700040804      * non elaboro gli annullati
071800040804     c                   If        CpoAtb = *Blanks
071900000713      *
072000000713      * ESEGUO L'EFFETTIVO CONTROLLO DEL CAP E SE ERRATO STAMPO
072100000713      *
072200000713     C                   EXSR      CHKCAP
072300000713      *                  ----------------
072400000713      *
072500040804     c                   Endif
072600040804
072700000713     C                   ENDDO
072800000713      *
072900000713      * LETTURA TNCPO08L (ANAGRAFICA CLIENTI POTENZIALI)
073000000713      * .. 2° CICLO PER KKSC=XXXYYYY (DOVE XXX E' IL P.O.)
073100000713      *
073200000713      * .. DAL P.O.
073300000713     C                   CLEAR                   KKSC
073400000713     C                   MOVEL     PPUNTO        KKSC
073500000713      * .. AL P.O.
073600000713     C                   Z-ADD     9999999       WCODFIN           7 0
073700000713     C                   MOVEL     PPUNTO        WCODFIN
073800000713      *
073900000713     C     KKSC          SETLL     TNCPO08L
074000000713     C                   DO        *HIVAL
074100090408     c                   if        vidtpe='S'
074200090408     C                   READ(n)   TNCPO08L                               98
074300090408     c                   else
074400090408     C                   READ      TNCPO08L                               98
074500090408     c                   endif
074600000713      *
074700000713      * ESCO PER FINE FILE O PER SUPERO LIMITI P.O.
074800000713      *
074900000713     C   98              LEAVE
075000000713     C     CPOCMM        IFGT      WCODFIN
075100000713     C                   LEAVE
075200000713     C                   ENDIF
075300040804      * non elaboro gli annullati
075400040804     c                   If        CpoAtb = *Blanks
075500000713      *
075600000713      * ESEGUO L'EFFETTIVO CONTROLLO DEL CAP E SE ERRATO STAMPO
075700000713      *
075800000713     C                   EXSR      CHKCAP
075900000713      *                  ----------------
076000000713      *
076100040804     c                   Endif
076200040804
076300000713     C                   ENDDO
076400000713      *
076500000712     C                   ENDSR
076600010403      *
076700010403      *--------------------------------------------------------------*
076800010403      * ELAB04 - SUBROUTINE CONTROLLO ANAGRAFICA SPEDIZIONIERI/LUOGHI*
076900010403      *--------------------------------------------------------------*
077000010403      *
077100010403     C     ELAB04        BEGSR
077200010403      *
077300010403      * LETTURA FNSPE01L (ANAGRAFICA SPEDIZIONIERI/LUOGHI)
077400010403      *
077500010403      * SOLO I LUOGHI
077600010403     C                   EVAL      KFLS = 'L'
077700010403      * .. DAL P.O.
077800010403     C                   CLEAR                   KKSC
077900010403     C                   MOVEL     PPUNTO        KKSC
078000010403      * .. AL P.O.
078100010403     C                   Z-ADD     9999999       WCODFIN           7 0
078200010403     C                   MOVEL     PPUNTO        WCODFIN
078300010403      *
078400010403     C     KFNSPE        SETLL     FNSPE01L
078500010403     C                   DO        *HIVAL
078600090408     c                   if        vidtpe='S'
078700090408     C                   READ(n)   FNSPE01L                               98
078800090408     c                   else
078900090408     C                   READ      FNSPE01L                               98
079000090408     c                   endif
079100010403      *
079200010403      * ESCO PER FINE FILE O PER SUPERO LIMITI P.O.
079300010403      *
079400010403     C   98              LEAVE
079500010403     C     SPECLI        IFGT      WCODFIN
079600010403     C                   LEAVE
079700010403     C                   ENDIF
079800010403      *
079900010403      * ESEGUO L'EFFETTIVO CONTROLLO DEL CAP E SE ERRATO STAMPO
080000010403      *
080100010403     C                   EXSR      CHKCAP
080200010403      *                  ----------------
080300010403      *
080400010403     C                   ENDDO
080500010403      *
080600010403     C                   ENDSR
080700010403      *
080800010403      *--------------------------------------------------------------*
080900010403      * ELAB05 - SUBROUTINE CONTROLLO ANAGRAFICA CLIENTI RITIRO      *
081000010403      *--------------------------------------------------------------*
081100010403      *
081200010403     C     ELAB05        BEGSR
081300010403      *
081400010403      * LETTURA FNACR01L (ANAGRAFICA CLIENTI RITIRO)
081500010403      * .. DAL P.O.
081600010403     C                   CLEAR                   kcro             10 0
081700010403     C                   MOVEL     PPUNTO        Kcro
081800010403      * .. AL P.O.
081900010403     C                   Z-ADD     *all'9'       WCODFIN1         10 0
082000010403     C                   MOVEL     PPUNTO        WCODFIN1
082100010403      *
082200010403     C     kcro          SETLL     FNACR01L
082300010403     C                   DO        *HIVAL
082400090408     c                   if        vidtpe='S'
082500090408     C                   READ(n)   FNACR01L                               98
082600090408     c                   else
082700090408     C                   READ      FNACR01L                               98
082800090408     c                   endif
082900010403      *
083000010403      * ESCO PER FINE FILE O PER SUPERO LIMITI P.O.
083100010403      *
083200010403     C   98              LEAVE
083300010403     C     ACRCRO        IFGT      WCODFIN1
083400010403     C                   LEAVE
083500010403     C                   ENDIF
083600010403      *
083700010403      * ESEGUO L'EFFETTIVO CONTROLLO DEL CAP E SE ERRATO STAMPO
083800010403      *
083900010403     C                   EXSR      CHKCAP
084000010403      *                  ----------------
084100010403      *
084200010403     C                   ENDDO
084300010403      *
084400010403     C                   ENDSR
084500000712      *
084600090408      *--------------------------------------------------------------*
084700090408      * ELAB066- SUBROUTINE CONTROLLO cappario eventi particolari    *
084800090408      *--------------------------------------------------------------*
084900090408      *
085000090408     C     ELAB06        BEGSR
085100090408      *
085200090408      * LETTURA AZCEP02l
085300090408      * Utilizzo l'anno della data di riferimento
085400090410     c                   clear                   kdev
085500090408     C                   IF        VIDDRI > *zeros
085600100224     C                   movel     amgdri        kdev              8 0
085700090408     C                   ELSE
085800090408     C                   movel     *date         kdev
085900090408     C                   ENDIF
086000090408     c                   movel     kdev          kanno             4 0
086100090410     c                   move      0101          kdev
086200090408      *
086300090408     C     kdev          SETLL     azcep02l
086400090408     c                   move      1231          kdev
086500090408     c
086600090408     C                   DO        *HIVAL
086700090408     C                   READ      azcep02l                               98
086800090408      *
086900090408      * ESCO PER FINE FILE O PER SUPERO LIMITI data
087000090408      *
087100090408     C   98              LEAVE
087200090408     C     cepdev        IFGT      kdev
087300090408     C                   LEAVE
087400090408     C                   ENDIF
087500090408      *
087600090408      * ESEGUO L'EFFETTIVO CONTROLLO DEL CAP E SE ERRATO STAMPO
087700090408      *  se non è inserito il record per p.o.
087800090408      *
087900090408     c                   if        cepfil=0
088000090408     C                   EXSR      CHKCAP
088100090408      *                  ----------------
088200090408     c                   endif
088300090408      *
088400090408     C                   ENDDO
088500090408      *
088600090408     C                   ENDSR
088700000712      *--------------------------------------------------------------*
088800000712      * CHKCAP - SUBROUTINE CONTROLLO CAP                            *
088900000712      *--------------------------------------------------------------*
089000000713      * NB: VIENE RICHIAMATO "FNLV13R" ED IN BASE AL RITORNO DEI     *
089100000713      *     PARAMETRI STAMPO ERRORE OPPURE NO;                       *
089200000713      *     POSSONO ESSERE EFFETTUATI 2 TENTATIVI DI CONTROLLO:      *
089300000713      *     IL PRIMO: NORMALE                                        *
089400070116      *     IL SECONDO1: SE HO TROVATO CAP OBSOLETO AL PRIMO         *
089500070116      *     IL SECONDO2: SE ERRORE DI PROVINCIA AL PRIMO             *
089600000713      *---------------------------------------------------------------
089700000713      *
089800000712     C     CHKCAP        BEGSR
089900000713      *
090000000713      * INIZIALIZZO TENTATIVI
090100000713      *
090200101126     C  n07              MOVEL(P)  'PRIMO'       TENTATIVO         8
090300101122     c                   clear                   obslna            3 0
090400101122     c                   clear                   obslocnor        35
090500000713      *
090600000713      * PULISCO DS CHE SERVONO A "FNLV13R"
090700000713      *
090800000713     C     RITENT        TAG
090900000713      *
091000000713     C                   CLEAR                   DSLV13
091100000713     C                   CLEAR                   DSSI95
091200000713      *
091300000713      * PASSO PARAMETRI COMUNI A "FNLV13R"
091400000713      *
091500000713     C                   MOVEL     '7'           I95TCN
091600000713     C                   MOVEL     'S'           I13AF0
091700000713     C                   MOVEL     'S'           I13CNV
091800000713     C                   MOVEL     'S'           I13LA3
091900000721     c                   movel     'S'           flgbac
092000090408     c* Se cappario e non c'e' località
092100090408     c                   if        *in06 and ceploc=*blanks
092200090408     C                   MOVEL     '3'           I95TCN
092300090410     c                   clear                   i13la3
092400090408     c                   endif
092500061116     C*
092600061116     C* Se passata data riferimento a video considero quella, altrimenti
092700061116     C* la data corrente
092800061116     C                   IF        VIDDRI > *zeros
092900100224     C                   EVAL      I95DAT = amgDRI
093000061116     C                   ELSE
093100000713     C                   EVAL      I95DAT = *DATE
093200061116     C                   ENDIF
093300000713      *
093400000713      * PASSO PARAMETRI IN BASE AD ANAGRAFICO A "FNLV13R"
093500000713      *
093600000713      * .. PIANO DEI CONTI oppure
093700000713      * .. ANAGRAFICA PROVVISORIA VISITE/OFFERTE
093800000713      *
093900000713     C     *IN01         IFEQ      *ON
094000000713     C     *IN02         OREQ      *ON
094100000713     C                   MOVEL     INDCIT        I95LOC
094200070116     C     TENTATIVO     IFEQ      'PRIMO'
094300000713     C                   MOVEL     INDCAE        I95CAP
094400000713     C                   ELSE
094500070116     C     TENTATIVO     IFEQ      'SECONDO1'
094600000713     C                   CLEAR                   I95CAP
094700070116     C                   ELSE
094800070116     C                   MOVEL     INDCAE        I95CAP
094900070116     C                   MOVEL     '4'           I95TCN
095000070116     C                   ENDIF
095100000713     C                   ENDIF
095200000713     C                   MOVEL     INDPRV        I95PRV
095300000713     C                   MOVEL     INDSTA        I95NAR
095400000713     C                   ENDIF
095500000713      *
095600000713      * .. ANAGRAFICA CLIENTI POTENZIALI
095700000713      *
095800000713     C     *IN03         IFEQ      *ON
095900000713     C                   MOVEL     CPOCIT        I95LOC
096000070116     C     TENTATIVO     IFEQ      'PRIMO'
096100000713     C                   MOVEL     CPOCAP        I95CAP
096200000713     C                   ELSE
096300070116     C     TENTATIVO     IFEQ      'SECONDO1'
096400070116     C                   CLEAR                   I95CAP
096500070116     C                   ELSE
096600070116     C                   MOVEL     CPOCAP        I95CAP
096700070116     C                   MOVEL     '4'           I95TCN
096800070116     C                   ENDIF
096900000713     C                   ENDIF
097000000713     C                   MOVEL     CPOPRV        I95PRV
097100000713     C                   MOVEL     CPONAZ        I95NAR
097200000713     C                   ENDIF
097300010403      *
097400010403      * .. ANAGRAFICA SPEDIZIONIERI/LUOGHI
097500010403      *
097600010403     C     *IN04         IFEQ      *ON
097700010403     C                   MOVEL     SPELOC        I95LOC
097800070116     C     TENTATIVO     IFEQ      'PRIMO'
097900010403     C                   MOVEL     SPECAP        I95CAP
098000010403     C                   ELSE
098100070116     C     TENTATIVO     IFEQ      'SECONDO1'
098200070116     C                   CLEAR                   I95CAP
098300070116     C                   ELSE
098400070116     C                   MOVEL     SPECAP        I95CAP
098500070116     C                   MOVEL     '4'           I95TCN
098600070116     C                   ENDIF
098700010403     C                   ENDIF
098800010403     C                   MOVEL     SPEPRO        I95PRV
098900010403     C                   MOVEL     SPENAZ        I95NAR
099000010403     C                   ENDIF
099100010403      *
099200010403      * .. ANAGRAFICA CLIENTI RITIRO
099300010403      *
099400010403     C     *IN05         IFEQ      *ON
099500010403     C                   MOVEL     ACRLOR        I95LOC
099600070116     C     TENTATIVO     IFEQ      'PRIMO'
099700010403     C                   MOVEL     ACRCAR        I95CAP
099800010403     C                   ELSE
099900070116     C     TENTATIVO     IFEQ      'SECONDO1'
100000070116     C                   CLEAR                   I95CAP
100100070116     C                   ELSE
100200070116     C                   MOVEL     ACRCAR        I95CAP
100300070116     C                   MOVEL     '4'           I95TCN
100400070116     C                   ENDIF
100500010403     C                   ENDIF
100600010403     C                   MOVEL     ACRPRR        I95PRV
100700010403     C                   MOVEL     ACRNAR        I95NAR
100800010403     C                   ENDIF
100900090408      *
101000090408      * .. cappario utilizzi particolari
101100090408      *
101200090408     C     *IN06         IFEQ      *ON
101300090408     C                   MOVEL     ceploc        I95LOC
101400090408     C     TENTATIVO     IFEQ      'PRIMO'
101500090408     C                   MOVEL     cepcap        I95CAP
101600090408     C                   ELSE
101700090408     C     TENTATIVO     IFEQ      'SECONDO1'
101800090408     C                   CLEAR                   I95CAP
101900090408     C                   ELSE
102000090408     C                   MOVEL     cepcap        I95CAP
102100090408     C                   MOVEL     '4'           I95TCN
102200090408     C                   ENDIF
102300090408     C                   ENDIF
102400090408     c
102500090408     C                   MOVEL     cepprv        I95PRV
102600090408     C                   MOVEL     cepnar        I95NAR
102700090408     C                   ENDIF
102800101122     c
102900101122     c* Per secondo tentativo per sicurezza usola località noramlizzata
103000101122     C     TENTATIVO     IFEQ      'SECONDO1'
103100101122     c                   movel     obslocnor     i95loc
103200101122     c                   endif
103300000713      *
103400000713      * LANCIO "FNLV13R"
103500000713      *
103600000713      *             --------------------------------------
103700000713     C                   CALL      'FNLV13R'
103800000713     C                   PARM                    KPJBA
103900000713     C                   PARM                    DSLV13
104000000713     C                   PARM                    DSSI95
104100000721     C                   PARM                    flgbac            1
104200000713      *             --------------------------------------
104300101122     c* Se mi è stato dato errore per secondo tentativo ma
104400101122     c*  trova i dati univoci per localit,. se corrisponde la lna
104500101122     c*  prima e dopo, tratto come se ok
104600101122     c                   if        TENTATIVO='SECONDO1 '
104700101122     C                             AND O95LNA=obslna
104800101122     c                             and %subst(O95MSG:78:1) = '9'
104900101122     c                             and o95err=' '
105000101122     c                   clear                   o13err
105100101122     c                   eval      o13rop='S'
105200101122     c                   eval      o13roc='S'
105300101122     c                   clear                   o95msg
105400101122     c                   eval      o95msg='          cambiata anche PROV.+
105500101122     c                             x località UNIVOCA'
105600101122     c                   endif
105700000713      *
105800000713      * SE VI E' ERRORE STAMPO ED EVENTUALMENTE AGGIORNO
105900110303      * non vado in stampa per errore "E" perchè è previsto un secondo
106000110303      *  tentativo il "SECONDO2"
106100000713      *
106200070116     C     O95ERR        IFNE      *BLANKS
106300110303     C     O95ERR        andne     'E'
106400110120     c
106500070116     C     TENTATIVO     ORNE      'PRIMO'
106600110120     c
106700090410     c     o13err        orne      *blanks
106800090410     c     *in06         andeq     *on
106900090410     c     ceploc        andeq     *blanks
107000101209     c*
107100101209     c                   if        not *in07 or (*in07 and o95err<>*blanks)
107200000713     C                   EXSR      STAMPA
107300101209     c                   endif
107400101209     c
107500110120     c                   leavesr
107600000713      *                  ----------------
107700000713     C                   ENDIF
107800000713      *
107900090408      * non posso fare i nuovi tentativi se non c'e' la località nel cappario
108000090408     c                   if        not *in06 or ceploc<>*blanks
108100090408     c
108200000713      * SE NON VI E' ERRORE MA CAP = OBSOLETO RITENTO CLEARANDO IL CAP
108300000713      *
108400101124     C**   O95ERR        IFEQ      *BLANKS
108500101124     C**   O95CF1        ANDNE     *BLANKS
108600101124     C**   TENTATIVO     ANDEQ     'PRIMO'
108700101124     c
108800101124     c                   if        o95err=*blanks and
108900101124     c                             tentativo='PRIMO ' and
109000101124     c                             (o95cf1<>*blanks or o95lf1<>*blanks)
109100000713      *
109200070116     C                   MOVEL     'SECONDO1'    TENTATIVO
109300101122     c* Mi salvo la linea di arrivo del cap obsoleto
109400101122     c                   eval      obslna=o95lna
109500101122     c* Mi salvo anche la località normalizzata per la seconda ricerca
109600101122     c                   eval      obslocnor=o95loc
109700101123     c
109800101123     c*
109900101123     c                   clear                   writent           1
110000101124     c
110100101124     c* Questo file di conversione è valido solo dalla versine 37 in poi
110200101124     c                   if        %open(wacpmod1l)   and o95ver>=37
110300101123     c                   exsr      cercaWACPMOD
110400101123     c                   endif
110500000713      *
110600101123     c                   if        writent=' '
110700000713     C                   GOTO      RITENT
110800101123     c                   endif
110900101123     c
111000000713      *                  °°°°°°°°°°°°°°°°
111100000713      *
111200000713     C                   ENDIF
111300070116      *
111400070116      * SE VI E' ERRORE DI PROVINCIA = RITENTO CON TIPO CONTROLLO 4
111500070116      * E LIVELLO DATI >= 3 E LIVELLO AFFIDABILITA >= 2
111600070116      *
111700070116     C                   IF        (%subst(O95MSG:78:1) = '9'  OR
111800070116     C                              %subst(O95MSG:78:1) = 'E') AND
111900070116     C                             TENTATIVO = 'PRIMO'
112000070116      *
112100070116     C                   MOVEL     'SECONDO2'    TENTATIVO
112200070116      *
112300070116     C                   GOTO      RITENT
112400070116      *                  °°°°°°°°°°°°°°°°
112500070116      *
112600070116     C                   ENDIF
112700110120     c
112800110120     c* Se il tisi95 non da errore ma lo da FNLV13, stampo lo stesso
112900110120     C     O95ERR        ifeq      *BLANKS
113000110120     C     TENTATIVO     andeq     'PRIMO'
113100110120     c     o13err        andne     *blanks
113200110120     c     *in06         andeq     *off
113300110120     c*
113400110120     C                   EXSR      STAMPA
113500110120     c                   endif
113600110120     c
113700090408     C                   ENDIF
113800000713      *
113900000712     C                   ENDSR
114000101123      *--------------------------------------------------------------*
114100101123      * Cerca sul file che ci hanno dato le poste                    *
114200101123      *--------------------------------------------------------------*
114300101123      *
114400101123     C     CercaWACPMOD  BEGSR
114500101123     c                   eval      klocmod=o95loc
114600101123     c                   eval      kcapmod=i95cap
114700101123     c                   eval      kprvmod=i95prv
114800101123     c
114900101123     c     kcpmod        chain     wacpmod1l
115000101123     c                   if        %found(wacpmod1l) and newl_prv<>*blanks
115100101123     c                             and newl_cap<>*blanks and newl_loc<>*blanks
115200101123     c                   clear                   o13err
115300101123     c                   if        o95prv<>newl_prv
115400101123     c                   eval      o13rop='S'
115500101123     c                   eval      o95prv=newl_prv
115600101123     c                   endif
115700101123     c
115800101123     c                   if        o95cap<>newl_cap
115900101123     c                   eval      o13roc='S'
116000101123     c                   eval      o95cap=newl_cap
116100101123     c                   endif
116200101123     c
116300101123     c                   clear                   o95msg
116400101123     c                   eval      o95msg='          Correzione da dati P+
116500101123     c                             OSTE                              '
116600101123     c                   exsr      Stampa
116700101123     c                   eval      writent='N'
116800101123     c                   endif
116900101123     c
117000101123     C                   ENDSR
117100000713      *
117200000713      *--------------------------------------------------------------*
117300000713      * STAMPA - SUBROUTINE STAMPA E AGGIORNAMENTO ERRORI            *
117400000713      *--------------------------------------------------------------*
117500000713      *
117600000713     C     STAMPA        BEGSR
117700110105     c* se cappario eventi particolari, se posso inserisco nuovo cap
117800090410     c*  controlando prima che non ci sia. se c'e' non stampo errore
117900090410     c                   if        *in06 and    o13roc='S'
118000090410     c                   eval      kloc=ceploc
118100090410     c                   eval      kcap=o95cap
118200090410     c     kcep2         setll     azcep02l
118300090410     c
118400090410     c* Se non trovato provo con la località dal tisi
118500090410     c                   if        not %equal(azcep02l)
118600090410     c                   eval      kloc=o95loc
118700090410     c     kcep2         setll     azcep02l
118800090410     c                   endif
118900090410     c
119000090410     c* se non trovato provo senza la località e solo col cap
119100090410     c                   if        not %equal(azcep02l)
119200090410     c                   clear                   kloc
119300090410     c     kcep2         setll     azcep02l
119400090410     c                   endif
119500090410     c
119600090415     c* Mi riposiziono sul file
119700090410     c                   eval      kcap=cepcap
119800090410     c                   eval      kloc=ceploc
119900090410     c     kcep2         chain     azcep02l
120000090415     c
120100090415     c* se trovato esco dalla routine come se non ci fosse errore
120200090415     c                   if        %equal(azcep02l)
120300090410     c                   goto      endStampa
120400090410     c                   endif
120500090410     c                   endif
120600090410     c
120700000713      *
120800000713      * SE DEVO ANCORA STAMPARE TESTATE STAMPO
120900000713      *
121000000713     C     VIDSAL        IFEQ      'S'
121100000713     C  N38              WRITE     TESTA
121200000713      *                  ===============
121300090408     c                   if        not *in06
121400000713     C  N38              WRITE     TESPO
121500090408     c                   endif
121600000713      *                  ===============
121700000713     C                   ELSE
121800000713     C  N39              WRITE     TESTA
121900000713      *                  ===============
122000090408     c                   if        not *in06
122100000713     C  N38              WRITE     TESPO
122200090408     c                   endif
122300000713      *                  ===============
122400000713     C                   ENDIF
122500000713      *
122600000713     C                   SETON                                        39  38
122700000713      *
122800000713      * SE SONO IN OVERFLOW STAMPO UGUALMENTE TESTATE
122900000713      *
123000000713     C     *IN55         IFEQ      *ON
123100000713      *
123200000713     C                   WRITE     TESTA
123300000713      *                  ===============
123400090408     c                   if        not *in06
123500000713     C                   WRITE     TESPO
123600090408     c                   endif
123700000713      *                  ===============
123800000713      *
123900000713     C                   SETOFF                                       55
124000000713     C                   ENDIF
124100000713      *
124200000713      * PASSO DATI AL FILE DI STAMPA
124300000713      *
124400000713      * .. RESET GRASSETTI
124500000713     C                   SETOFF                                       212223
124600000713     C                   SETOFF                                       24
124700000713      *
124800000713      * .. ANNULLAMENTO/BLOCCO
124900000713     C                   CLEAR                   PANNU
125000000713     C                   CLEAR                   PBLOC
125100000713     C   01ACOFLG        IFNE      ' '
125200000713     C                   MOVE      'A'           PANNU
125300000713     C                   ENDIF
125400000713     C   02INDFLG        IFNE      ' '
125500000713     C                   MOVE      'A'           PANNU
125600000713     C                   ENDIF
125700000713     C   03CPOATB        IFNE      ' '
125800000713     C                   MOVE      'A'           PANNU
125900000713     C                   ENDIF
126000000713     C   01ACOABL        IFNE      ' '
126100000713     C                   MOVE      'B'           PBLOC
126200000713     C                   ENDIF
126300000713      *
126400000713      * .. DESCRIZIONE
126500090408     c                   clear                   prag
126600000713     C   01              MOVEL     ACORAG        PRAG
126700000713     C   02              MOVEL     VISRAG        PRAG
126800000713     C   03              MOVEL     CPORAG        PRAG
126900010403     C   04              MOVEL     SPERAG        PRAG
127000010403     C   05              MOVEL     ACRRSR        PRAG
127100090408     C   06              eval      prag=%editw(cepdev:'    /  /  ')
127200000713      *
127300000713      * .. MSG ERRORE
127400000713     C                   MOVEL     O13MSG        PERR
127500000713      *
127600000713      * ALTRI DATI:
127700000713      *
127800000713      * .. SE PIANO DEI CONTI oppure
127900000713      * .. ANAGRAFICA PROVVISORIA VISITE/OFFERTE
128000000713      *
128100000713     C     *IN01         IFEQ      *ON
128200000713     C     *IN02         OREQ      *ON
128300101125     C                   MOVEL(P)  INDKSC        PKSC
128400000713     C                   MOVEL     INDCIT        PCIT
128500000713     C                   MOVEL     INDCAE        PCAE
128600000713     C                   MOVEL     INDPRV        PPRV
128700000713     C                   MOVEL     INDSTA        PSTA
128800000713     C                   MOVEL     INDCIT        PPCIT
128900000713     C                   MOVEL     INDCAE        PPCAE
129000000713     C                   MOVEL     INDPRV        PPPRV
129100000713     C                   MOVEL     INDSTA        PPSTA
129200000713     C                   ENDIF
129300000713      *
129400000713      * .. SE ANAGRAFICA CLIENTI POTENZIALI
129500000713      *
129600000713     C     *IN03         IFEQ      *ON
129700000713     C                   MOVEL     CPOCPO        PKSC
129800000713     C                   MOVEL     CPOCIT        PCIT
129900000713     C                   MOVEL     CPOCAP        PCAE
130000000713     C                   MOVEL     CPOPRV        PPRV
130100000713     C                   MOVEL     CPONAZ        PSTA
130200000713     C                   MOVEL     CPOCIT        PPCIT
130300000713     C                   MOVEL     CPOCAP        PPCAE
130400000713     C                   MOVEL     CPOPRV        PPPRV
130500000713     C                   MOVEL     CPONAZ        PPSTA
130600000713     C                   ENDIF
130700010403      *
130800010403      * .. SE ANAGRAFICA SPEDIZIONIERI/LUOGHI
130900010403      *
131000010403     C     *IN04         IFEQ      *ON
131100010403     C                   MOVEL     SPECLI        PKSC
131200010403     C                   MOVEL     SPELOC        PCIT
131300010403     C                   MOVEL     SPECAP        PCAE
131400010403     C                   MOVEL     SPEPRO        PPRV
131500010403     C                   MOVEL     SPENAZ        PSTA
131600010403     C                   MOVEL     SPELOC        PPCIT
131700010403     C                   MOVEL     SPECAP        PPCAE
131800010403     C                   MOVEL     SPEPRO        PPPRV
131900010403     C                   MOVEL     SPENAZ        PPSTA
132000010403     C                   ENDIF
132100010403      *
132200010403      * .. SE ANAGRAFICA CLIENTI RITIRO
132300010403      *
132400010403     C     *IN05         IFEQ      *ON
132500010403     C                   MOVEL     ACRCRO        PKSC
132600010403     C                   MOVEL     ACRLOR        PCIT
132700010403     C                   MOVEL     ACRCAR        PCAE
132800010403     C                   MOVEL     ACRPRR        PPRV
132900010403     C                   MOVEL     ACRNAR        PSTA
133000010403     C                   MOVEL     ACRLOR        PPCIT
133100010403     C                   MOVEL     ACRCAR        PPCAE
133200010403     C                   MOVEL     ACRPRR        PPPRV
133300010403     C                   MOVEL     ACRNAR        PPSTA
133400010403     C                   ENDIF
133500090408      *
133600090408      * .. SE cappario utilizzi particolari
133700090408      *
133800090408     C     *IN06         IFEQ      *ON
133900090408     c                   clear                   pksc
134000090408     C                   MOVEL     ceploc        PCIT
134100090408     C                   MOVEL     cepcap        PCAE
134200090408     C                   MOVEL     cepprv        PPRV
134300090408     C                   MOVEL     cepnar        PSTA
134400090410     C                   MOVEL     ceploc        pPCIT
134500090410     C                   MOVEL     cepcap        pPCAE
134600090410     C                   MOVEL     cepprv        pPPRV
134700090410     C                   MOVEL     cepnar        pPSTA
134800090408     C                   ENDIF
134900000713      *
135000000713      * SE NON SONO RIUSCITO A CORREGGERE PULISCO
135100000713      * I DATI IN SOSTITUZIONE, AGGIORNO MESSAGGIO DI ERRORE
135200000713      * PER SECONDO TENTATIVO
135300000713      *
135400000713     C     O13ROP        IFNE      'S'
135500000713     C     O13ROC        ANDNE     'S'
135600101125     C***  O13ROL        ANDNE     'S'
135700000713     C     O13RON        ANDNE     'S'
135800000713     C                   CLEAR                   PPCIT
135900000713     C                   CLEAR                   PPCAE
136000000713     C                   CLEAR                   PPPRV
136100000713     C                   CLEAR                   PPSTA
136200070116     C     TENTATIVO     IFEQ      'SECONDO1'
136300000713     C                   CLEAR                   PERR
136400070116     C                   EVAL      PERR='C.A.P. o località obsoleti'
136500101122     c                   if        o95msg<>*blanks
136600101122     c                   eval      perr=%trim(perr)+ '-' +
136700101122     c                             %subst(o95msg:10:60)
136800101122     c                   endif
136900110107     C                   ENDIF
137000110107     c* Aggiungo numero sped in WFALA
137100110107     c                   if        *in07   and totspe>0
137200110107     c                   eval      alfaspe=%editc(totspe:'Z')
137300110107     c
137400110107     c                   eval      perr=%trim(perr)+ 'sped in WFALA: '+
137500110107     c                             %trim(alfaspe)
137600110107     c                   endif
137700101122     c
137800000713     C                   ELSE
137900000713      *
138000000713      * ALTRIMENTI MUOVO CAMPI DA DS A FILE DI STAMPA
138100000713      * EVIDENZIANDO IN GRASSETTO QUELLO ERRATO/AGGIORNATO,
138200000713      * AGGIORNO MESSAGGIO DI ERRORE PER SECONDO TENTATIVO
138300000713      *
138400070116     C     TENTATIVO     IFEQ      'SECONDO1'
138500000713     C                   CLEAR                   PERR
138600090410     C  n06              EVAL      PERR='C.A.P. obsoleto: modificato'
138700090410     C   06              EVAL      PERR='C.A.P. obsoleto: aggiunto  '
138800101122     c                   if        o95msg<>*blanks
138900101122     c                   eval      perr=%trim(perr)+ '-' +
139000101122     c                             %subst(o95msg:10:60)
139100101122     c                   endif
139200000713     C                   ENDIF
139300070116      *
139400070116     C     TENTATIVO     IFEQ      'SECONDO2'
139500070116     C                   CLEAR                   PERR
139600090410     C  n06              EVAL      PERR='Provincia/CAP modificata'
139700090410     C   06              EVAL      PERR='Provincia/CAP aggiunta  '
139800070116     C                   ENDIF
139900110119     c
140000110119     C  n06TENTATIVO     IFEQ      'PRIMO   '
140100110119     C                   EVAL      PERR='CAP modificato per loc.Inesistente'
140200110119     c                   endif
140300000713      *
140400000713     C     O13ROP        IFEQ      'S'
140500000713     C                   MOVEL     O95PRV        PPPRV
140600000713     C                   SETON                                        23
140700101123     c                   eval      contaprv=contaprv+1
140800000713     C                   ENDIF
140900000713      *
141000000713     C     O13ROC        IFEQ      'S'
141100000713     C                   MOVEL     O95CAP        PPCAE
141200000713     C                   SETON                                        22
141300101123     c                   eval      contacap=contacap+1
141400000713     C                   ENDIF
141500000713      *
141600000713     C     O13ROL        IFEQ      'S'
141700000713     C                   MOVEL     O95LOC        PPCIT
141800000713     C                   SETON                                        21
141900101123     c                   eval      contaloc=contaloc+1
142000000713     C                   ENDIF
142100000713      *
142200000713     C     O13RON        IFEQ      'S'
142300000713     C                   MOVEL     O95NAR        PPSTA
142400000713     C                   SETON                                        24
142500101123     c                   eval      contanar=contanar+1
142600000713     C                   ENDIF
142700000713      *
142800000713      * SE A VIDEO SCELTA "A" DI AGGIORNAMENTO, AGGIORNO
142900000713      * (SIA INDIRIZZI)
143000000713      * (SIA FLAGS TRASMISSIONE)
143100000713      *
143200000713IF   C     VIDTPE        IFEQ      'A'
143300000713      *
143400110111      * .. PIANO DEI CONTI
143500000713     C     *IN01         IFEQ      *ON
143600110111     c/exec sql
143700110111     c+ update cnind00f set indcae=:ppcae, indprv=:ppprv
143800110111     c+ where  indkut=1 and indkcc=: vidkcu and indksc=:indksc
143900110111     c/end-exec sql
144000110111     c/exec sql
144100110111     c+ update cnaco00f set acodtr=0, acoftr=' '
144200110111     c+ where  acokut=1 and acokcc=: vidkcu and acoksc=:indksc
144300110111     c/end-exec sql
144400110111     C***                CLEAR                   ACODTR
144500110111     C***                CLEAR                   ACOFTR
144600110111     C***                UPDATE    CNACO000
144700110111      *                  ------    ========
144800110111     C                   ENDIF
144900110111     c
145000110111      * .. ANAGRAFICA PROVVISORIA VISITE/OFFERTE
145100110111     C     *IN02         IFEQ      *ON
145200110111     c/exec sql
145300110111     c+ update tfind00f set indcae=:ppcae, indprv=:ppprv
145400110111     c+ where  indkut=1 and indkcc=: indkcc and indksc=:indksc
145500110111     c/end-exec sql
145600110111     C***                MOVEL     PPCIT         INDCIT
145700110111     C**                 MOVEL     PPCAE         INDCAE
145800110111     C**                 MOVEL     PPPRV         INDPRV
145900061109     C***                MOVEL     PPSTA         INDSTA
146000000713      *                  ------    ======
146100110111     C**                 UPDATE    CNIND000
146200000713      *                  ------    ========
146300110111     C                   ENDIF
146400000713      *
146500000713      * .. ANAGRAFICA CLIENTI POTENZIALI
146600000713     C     *IN03         IFEQ      *ON
146700101125     c     cponrr        chain     tncpo00f
146800101125   2ac                   if        %found
146900061109     C***                MOVEL     PPCIT         CPOCIT
147000000713     C                   MOVEL     PPCAE         CPOCAP
147100061116     C                   MOVEL     PPPRV         CPOPRV
147200061109     C***                MOVEL     PPSTA         CPONAZ
147300040804     c                   Move      *date         CpoDtr
147400101125     C                   UPDATE    TNCPOfis
147500101125     c                   endif
147600000713      *                  ------    ========
147700000713     C                   ENDIF
147800010403      *
147900010403      * .. ANAGRAFICA SPEDIZIONIERI/LUOGHI
148000010403     C     *IN04         IFEQ      *ON
148100061109     C***                MOVEL     PPCIT         SPELOC
148200010403     C                   MOVEL     PPCAE         SPECAP
148300061116     C                   MOVEL     PPPRV         SPEPRO
148400061109     C***                MOVEL     PPSTA         SPENAZ
148500040715     C                   move      *date         SPEDTR
148600010403     C                   UPDATE    FNSPE000
148700010403      *                  ------    ========
148800010403     C                   ENDIF
148900010403      *
149000010403      * .. ANAGRAFICA CLIENTI RITIRO
149100010403     C     *IN05         IFEQ      *ON
149200061109     C***                MOVEL     PPCIT         ACRLOR
149300010403     C                   MOVEL     PPCAE         ACRCAR
149400061116     C                   MOVEL     PPPRV         ACRPRR
149500061109     C***                MOVEL     PPSTA         ACRNAR
149600040910     C                   move      *date         ACRDTR
149700010403     C                   UPDATE    FNACR000
149800010403      *                  ------    ========
149900010403     C                   ENDIF
150000090410      *
150100090410      * .. cappario eventi particolari
150200090410     C     *IN06         IFEQ      *ON
150300090410     C                   MOVEL     PPCAE         cepcap
150400090410     C                   MOVEL     PPPRV         cepprv
150500090410     C                   write     azcep000
150600090410      *                  ------    ========
150700090410     C                   ENDIF
150800000713      *
150900110107     C                   ENDIF
151000000713      *
151100000713      *
151200000713     C                   ENDIF
151300000713      *
151400000713      * STAMPO DETTAGLIO ERRORI
151500000713      *
151600000713     C                   WRITE     DETT
151700000713      *                  ==============
151800000713      *
151900090410     C     endStampa     ENDSR
152000110107      *--------------------------------------------------------------*
152100110107      * Conto le spedizioni di quel CAP su WFALA
152200110107      *--------------------------------------------------------------*
152300110107     C     contWFALA     BEGSR
152400110107      *
152500110107     c                   clear                   wrkgetlista
152600110107     c                   eval      wrkgetlista =
152700110107     c                             'select  sum(alatos)   +
152800110107     c                             from '+ libWFALA + '/WFALA00F   +
152900110107     c                             where alacap =''' +  cplcap +
153000110107     c                             ''' and alaloc =''' + cplloc +
153100110107     c                             ''' and alanaz =''' + cplnar +
153200110107     c                             ''' and alaprv =''' + cpcprv + ''''
153300110107     C/EXEC SQL
153400110107     C+ PREPARE s2 FROM :wrkgetlista
153500110107     C/END-EXEC
153600110107     C
153700110107     C/EXEC SQL
153800110107     C+ DECLARE A2 CURSOR FOR S2
153900110107     C/END-EXEC
154000110107     C
154100110107     C/EXEC SQL
154200110107     C+ OPEN a2
154300110107     C/END-EXEC
154400110107
154500110107      *?Leggo il file
154600110107     c
154700110107     C/EXEC SQL
154800110107     C+ FETCH NEXT FROM a2 INTO :totspe
154900110107     C/END-EXEC
155000110107
155100110107     c                   if        sqlcod < 0
155200110107     c                   clear                   totspe
155300110107     c                   endif
155400110107     C/EXEC SQL
155500110107     C+ close a2
155600110107     C/END-EXEC
155700110107     c
155800110107     c                   ENDSR
155900110107**
156000110107/YYYYYYYYYY) OBJTYPE(*FILE)
