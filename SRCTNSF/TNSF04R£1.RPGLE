000100000120     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(QILE)
000200000120     H*PARMS BNDDIR(PJXBND PJCBND)
000300981014     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000400981014     H/TITLE  creazione mov.contabili fatture porto assegnato
000500000120     H* TNSF04R *----------------------------------------------------*
000600891106     H*        - CREAZIONE MOV.CONTABILI FATTURE PORTO ASSEGNATO     *
000700000000     H*--------------------------------------------------------------*
000800000119     Ftitas04l  UF   E           K DISK
000900980704     FAZORG01L  IF   E           K DISK
001000990712     Ftita730c  IF   E           K DISK    ignore(tita7p00)
001100990712     Ftitaa30c  IF   E           K DISK    ignore(titaap00)
001200061109     Ftita430c  IF   E           K DISK    ignore(tita4p00)
001300000000     FTABEL00F  IF   E           K DISK
001400981015     fndbhm00f  o    e           k disk
001500981015     fndbhi00f  o    e           k disk
001600981015     fndbha00f  o    e           k disk
001700990517     fndbhY00f  o    e           k disk
001800981014     Fansog01l  IF   E           K DISK
001900981014     Fanind01l  IF   E           K DISK
002000981015     Fancln01l  iF   E           K DISK
002100981015     Fybacl01l  uF   E           K DISK
002200050509     Fcnclp00f  uF   E           K DISK
002300040531     fFiar531c  if   e           k Disk
002400130122     Fanriv01l  if   e           k disk
002500011116     F***TNASC00F  O  A E           K DISK
002600050623     F***TIASC00F  O  A E           K DISK
002700000000     FQSYSPRT   O    F  132        PRINTER
002800000208     FSYSPRT    O    F  132        PRINTER
002900000119     FFTWFAT89  O    F  198        PRINTER
003000000119     F                                     FORMLEN(100)
003100000119     F                                     FORMOFL(100)
003200970411     D E               S             66    DIM(10) CTDATA PERRCD(1)             STAMPA ERRORI HDG
003300000120     D RI              S             55    DIM(7) CTDATA PERRCD(1)              riservatezza
003400000121     D ST              S             66    DIM(2) CTDATA PERRCD(1)              stampa totali
003500000120     D CC              S              8    DIM(50)                              VOCI BOLLA
003600041220     D CTS             S              2    DIM(600)                             C.TASS -TABELLA
003700000121     D TS              S              1    DIM(10)                              TIPI SERVIZIO
003800000000     D TB              S              2    DIM(20)                              TIPI BOLLA
003900941129     D CEI             S              1    DIM(50)                              ESENZIONI IVA-TAB
004000130116     D DEI             S             20    DIM(30)                              ES.IVA        LA
004100130122     D DES             S             20    DIM(30)                              ES.IVA  proj
004200981015     D CCP             S             22    DIM(50)                              CONTO/S-CONTO C/P
004300000120     D ICP             S             19  2 DIM(50)                              IMPORTI CONTROPAR
004400000120     D ICPW            S             19  3 DIM(50)                              IMPORTI CONTROPAR
004500000120     D ICPC            S             13  3 DIM(50)                              IMP.CONTROP.mon/cont
004600891117     D FCP             S              1  0 DIM(50)                              FLAG COMBIN.C/FIL
004700981015     D AI              S              8    DIM(5)                               ALIQUOTE IVA O ES
004800000119     D IM              S             19  3 DIM(5)                               IMPONIBILE IVA
004900000120     D IMC             S             14  2 DIM(5)                               IMPON.IVA mon/conto
005000000119     D sim             S             15  0 DIM(5)                               stampa imp.non dec
005100000119     D simd            S             14  2 DIM(5)                               stampa imp.con dec
005200000119     D siv             S             15  0 DIM(5)                               stampa iva non dec
005300000119     D sivd            S             14  2 DIM(5)                               stampa iva con dec
005400000125     D IV              S             19  3 DIM(5)                               IVA  X AL.IVA
005500000120     D IVC             S             14  2 DIM(5)                               IVAXAL.IVAmon/conto
005600000120     D LES             S              1    DIM(20)                              LEGENDA VARIE SIGLE
005700000120     D LED             S             10    DIM(20)                              LEGENDA VARIE DESCR.
005800000121     D LINS            S              1    DIM(20)                              SEPARATORE
005900000125     D SVW             S              1    DIM(20)                              Sigle varie wrk
006000000125     D VAW             S             11  3 DIM(20)                              Importo varie wrk
006100000120     D S               S              1    DIM(24)                              SIGLA VARIE ST
006200000120     D I               S              7  0 DIM(24)                              IMPORTI VARIE ST
006300000124     D ID              S              7  3 DIM(24)                              IMPORTI VARIE ST DEC
006400000120     D S2              S              1    DIM(3)                               SIGLA VARIE ST
006500000120     D I2              S              7  0 DIM(3)                               IMPORTI VARIE ST
006600000124     D I2D             S              7  3 DIM(3)                               IMPORTI VARIE ST
006700990714     D EST             S              3  0 DIM(300)                             p.o esteri
006800000121     D DIV             S              3    DIM(3)                               divise
006900941129     D HTBLCP          DS
007000941129     D  DSCP                   1      3
007100941129     D  DSCL                   4      4
007200941129     D DSQC1         E DS
007300941129     D* DATI VARI CONTABILITA'
007400941129     D DSQC2         E DS
007500981016     D DSQt1         E DS
007600941129     D* DATI VARI CONTABILITA'
007700000119      ** DIVISA
007800000119     D DSCV          E DS
007900941129     D** CAMBIO
008000000000     D PARAM           DS
008100981020     D  VIDDFT                        8  0
008200981020     d  wsocieta                      3
008300981015     D DSCC          E DS                  OCCURS(50)
008400941129     D* VOCI BOLLA
008500941129     D DSEI          E DS                  OCCURS(50)
008600941129     D** ESENZIONI IVA
008700941128     D DSTB          E DS                  OCCURS(20)
008800941129     D DS$2          E DS
008900041220     D DSCT          E DS                  occurs(600)
009000000121     D DS5E          E DS
009100061109     D** DS codici fiscali su tita4
009200061109     D Dta4q         e ds
009300110712     D** DS tracciabilità flussi su tita4 CUP-CIG
009400110712     D DTA44         E DS                  INZ
009500970506     D** DS CALCOLO TASSAZIONE  - VARIE
009600991109     D dtasv         E DS
009700970506     D  SV                     1     20
009800941129     D                                     DIM(20)                              SIGLE VARIE
009900991109     D  VA                    21    140P 3
010000950404     D                                     DIM(20)                              IMPORTI VARIE
010100991109     D DTAS          E DS
010200991109     D  dstasflo     E                     EXTFLD(tasflo)
010300991110     D  dstasimv     E                     EXTFLD(tasimv)
010400030204     d  dstaspkc     E                     EXTFLD(taspkc)
010500030204     d  dstasncp     E                     EXTFLD(tasncp)
010600060926      ** DS Flag operativi DS DTAS
010700060926     d Dtas01        e ds
010800011129      **
010900991110     D DSVAR         E DS                  EXTNAME(DTASV) INZ
011000980525     D  WSV1         E                     EXTFLD(TASSV1)
011100980525     D  WSV2         E                     EXTFLD(TASSV2)
011200980525     D  WSV3         E                     EXTFLD(TASSV3)
011300980525     D  WSV4         E                     EXTFLD(TASSV4)
011400980525     D  WSV5         E                     EXTFLD(TASSV5)
011500980525     D  WSV6         E                     EXTFLD(TASSV6)
011600980525     D  WSV7         E                     EXTFLD(TASSV7)
011700980525     D  WSV8         E                     EXTFLD(TASSV8)
011800980525     D  WSV9         E                     EXTFLD(TASSV9)
011900980525     D  WSV0         E                     EXTFLD(TASSV0)
012000980525     D  WS11         E                     EXTFLD(TASS11)
012100980525     D  WS12         E                     EXTFLD(TASS12)
012200980525     D  WS13         E                     EXTFLD(TASS13)
012300980525     D  WS14         E                     EXTFLD(TASS14)
012400980525     D  WS15         E                     EXTFLD(TASS15)
012500980525     D  WS16         E                     EXTFLD(TASS16)
012600980525     D  WS17         E                     EXTFLD(TASS17)
012700980525     D  WS18         E                     EXTFLD(TASS18)
012800980525     D  WS19         E                     EXTFLD(TASS19)
012900980525     D  WS20         E                     EXTFLD(TASS20)
013000990713     D  WVA1         E                     EXTFLD(TASVA1)
013100990713     D  WVA2         E                     EXTFLD(TASVA2)
013200990713     D  WVA3         E                     EXTFLD(TASVA3)
013300980525     D  WVA4         E                     EXTFLD(TASVA4)
013400980525     D  WVA5         E                     EXTFLD(TASVA5)
013500980525     D  WVA6         E                     EXTFLD(TASVA6)
013600980525     D  WVA7         E                     EXTFLD(TASVA7)
013700980525     D  WVA8         E                     EXTFLD(TASVA8)
013800980525     D  WVA9         E                     EXTFLD(TASVA9)
013900980525     D  WVA0         E                     EXTFLD(TASVA0)
014000980525     D  WV11         E                     EXTFLD(TASV11)
014100980525     D  WV12         E                     EXTFLD(TASV12)
014200980525     D  WV13         E                     EXTFLD(TASV13)
014300980525     D  WV14         E                     EXTFLD(TASV14)
014400980525     D  WV15         E                     EXTFLD(TASV15)
014500980525     D  WV16         E                     EXTFLD(TASV16)
014600980525     D  WV17         E                     EXTFLD(TASV17)
014700980525     D  WV18         E                     EXTFLD(TASV18)
014800980525     D  WV19         E                     EXTFLD(TASV19)
014900980525     D  WV20         E                     EXTFLD(TASV20)
015000980525     D  WEV1         E                     EXTFLD(TASEV1)
015100980525     D  WEV2         E                     EXTFLD(TASEV2)
015200980525     D  WEV3         E                     EXTFLD(TASEV3)
015300980525     D  WEV4         E                     EXTFLD(TASEV4)
015400980525     D  WEV5         E                     EXTFLD(TASEV5)
015500980525     D  WEV6         E                     EXTFLD(TASEV6)
015600980525     D  WEV7         E                     EXTFLD(TASEV7)
015700980525     D  WEV8         E                     EXTFLD(TASEV8)
015800980525     D  WEV9         E                     EXTFLD(TASEV9)
015900980525     D  WEV0         E                     EXTFLD(TASEV0)
016000980525     D  WE11         E                     EXTFLD(TASE11)
016100980525     D  WE12         E                     EXTFLD(TASE12)
016200980525     D  WE13         E                     EXTFLD(TASE13)
016300980525     D  WE14         E                     EXTFLD(TASE14)
016400980525     D  WE15         E                     EXTFLD(TASE15)
016500980525     D  WE16         E                     EXTFLD(TASE16)
016600980525     D  WE17         E                     EXTFLD(TASE17)
016700980525     D  WE18         E                     EXTFLD(TASE18)
016800980525     D  WE19         E                     EXTFLD(TASE19)
016900980525     D  WE20         E                     EXTFLD(TASE20)
017000000119     D*
017100000121     D** CONTI CONTABILI PER REGISTRARE ARROTONDAMENTI CAMBI
017200000121     D DARR          E DS                  OCCURS(2)
017300000121     D** CAMBIO DIVISA
017400000121     D YEUR          E DS                  EXTNAME(YEURCODS)
017500030204
017600030204     d dAr5Ban       e ds
017700030911     d dAr5Bnb       e ds
017800000121      **
017900000000     D KPJBA         E DS
018000000120     D DSBS69        E DS                  EXTNAME(TIBS69DS)
018100000121     D DSBS02        E DS                  EXTNAME(TIBS02DS)
018200000121     D DSLV55        E DS                  EXTNAME(FNLV55DS)
018300000121     D DSACO         E DS                  EXTNAME(CNACO00F) prefix(c)
018400000121     D DSIND         E DS                  EXTNAME(CNIND00F) prefix(c)
018500000121     D DSCLP         E DS                  EXTNAME(CNCLP00F) prefix(c)
018600000121     D DSCLS         E DS                  EXTNAME(FNCLS00F) prefix(c)
018700000121     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
018800000121     D  TCU                  398    697
018900000121     D                                     DIM(50)
019000000121     D  KCU                  698    847P 0
019100000121     D                                     DIM(50)
019200000121     D                                     PACKEVEN
019300000121     D TCUDS           DS
019400000121     D  F1                     1      1
019500000121     D  F3                     3      3
019600000121     D  F2                     2      2
019700000121     D  F4                     4      4
019800000121     D  F56                    5      6
019900981016     D soc001        e ds                  extname(xsoc001ds)
020000981016     D xsocds          DS          1000
020100981016     D ndc071ds      E DS
020200981015     d dataiso         s               d   datfmt(*iso)
020300981015     d dataeur         s               d   datfmt(*eur)
020400000120     d datadmy         s               d   datfmt(*dmy)
020500981015     d dsconti         ds
020600981015     d  wkcc                          6
020700981015     d  wksc                          8
020800981015     d  wvoc                          8
020900981015     d wivads          ds
021000981015     d  wiva3                         3
021100981015     d  wiva4                         5
021200000121     d* ds dei totali generali
021300000121     d dstotali        ds                  occurs(3)                            * 1 ds per moneta
021400000121     d  totimpds                     19  3 inz(0)
021500000121     d  totivads                     19  3 inz(0)
021600000121     d  totbolds                     19  3 inz(0)
021700000121     d  totfatds                     19  3 inz(0)
021800000121     d  numfatds                     15  0 inz(0)
021900981016     D* Definizioni parametri chiamata moduli NDMVC104
022000981016    >D Societa         S              3
022100981016    >D Ctb             S              2
022200981016    >D Unita           S              4
022300981016     D TpReg           S              1
022400981016     D DtReg           S             10
022500981016     D DtLGio          S             10
022600981016     D RegIVA          S              1
022700981016     D TpRegIVA        S              1
022800981016     D Stato           S              2
022900981016     D Esito           S              1
023000981016     D Gesmsg          S              1
023100000120     d* campi di comodo
023200000120     d nfthiv          S                   like(tasnft)
023300000120     d div1            s                   like(tasdiv)
023400000120     d div2            s                   like(tasdiv)
023500000120     d divisa          s                   like(tasdiv)
023600000120     d impocnv         s                   like(yecimi)
023700000121     d wkey            s                   like(tblkey)
023800110712     d* campo di comodo per stampa CUP-CIG
023900110712     d StpCup          s             35
024000000120     d* stampa dati anagrafici
024100000120     d prtrsc          s             48                                         *rag.soc.
024200000120     d prtind          s             30                                         *indiriz.
024300000120     d prtloc          s             30                                         *città
024400000208     d prtcap          s              9                                         *cap
024500000120     d prtprv          s              2                                         *provincia
024600000120     d prtnaz          s              3                                         *stato
024700050623
024800050623     d rsd             s             35                                         *rag.soc.
024900050623     d ind             s             35                                         *indiriz.
025000050623     d loc             s             35                                         *città
025100050623     d cap             s              9                                         *cap
025200050623     d naz             s              3                                         *stato
025300000119     d* campi utilizzati per stampe
025400000121     d dftgma          s              6  0
025500000119     d fattot          s             15  0
025600000119     d fattotd         s             15  2
025700000120     d fatbol          s             15  0
025800000120     d fatbold         s             15  2
025900000119     d prttpi          s             15  0
026000000119     d prttpid         s             14  2
026100000119     d varie           s             15  0
026200000119     d varied          s             14  2
026300000119     d totvar          s             15  3
026400000120     d prtpor          s              9  0
026500050623     d staese          s             85
026600000120     d prtpord         s              8  3
026700000120     d fattotcnt       s             15  3                                      *tot.fat.moneta/cont
026800000120     d fatbolcnt       s             15  3                                      *bollo moneta/conto
026900000121     d wtotfat         s             19  3                                      *totale fattura
027000000120     d wtotimc         s             19  3                                      *totale imponibile
027100000120     d wtotivc         s             19  3                                      *totale iva
027200000121     d wtoticpc        s             19  3                                      *totale contropartie
027300000121     d diffe           s             19  3                                      *differenze cambio
027400000121     d*
027500000121     d esitoa          s              1
027600000121     d noctrscos       s              1
027700000124     d dsnufat         s                   like(numfatds)
027800000125     d y               s              2  0
027900060406     d cont_a          s              2  0
028000000208     d stksc           s              7
028100061109
028200120518     d §4trc           s                   like(ta4trc)                         *t.rec.ta4 Cod.fisc
028300030204
028400030911     d kAr5Trd         s                   Like(Ar5Trd)
028500050623
028600050623     D Cesenz          C                   CONST('Bollo assolto in modo virtual-
028700050623     d                                     e AUT.AG.ENTRATE BOLOGNA 3 del 27/09-
028800050623     d                                     /02 prot.2002/31599 ')
028900060719     D Cesenz_new      C                   CONST('Bollo assolto in modo virtual-
029000060719     d                                     e AUT.AG.ENTRATE MILANO 2 del 10/07/-
029100060719     d                                     06 prot.2006/45114 ')
029200000119
029300980527     D PARAMT          DS
029400980527     D  PARCA                256    256
029500000120
029600000120      * Prototipazione delle procedure
029700000120     D/COPY *LIBL/SRCCPY,PJX002PR
029800000119     C*********--------********-------*******-------********
029900000119     C* INDICATORI
030000000119     C*********--------********-------*******-------********
030100000120      * 06    - CHAIN TITAA00F
030200000120      * 05    - CHAIN AZORG / COMODO
030300000119      * 07    - ERRORE/COMODO
030400000119      * 12    - DIVISA CHE PREVEDE DECIMALI
030500000119      * 20    - CLIENTE CODIFICATO
030600000119     C*********--------********-------*******-------********
030700980115     C*
030800000000     C     START         TAG
030900941129     C                   SETOFF                                       332036
031000941129     C                   SETOFF                                       343181
031100990712     C                   READ      TITAS000                               02
031200000000     C   02              GOTO      FINE
031300990713     C** FINE LETTURA TITAS
031400980527     C                   CLEAR                   TASDSP
031500941130     C                   MOVEL     TASAAS        TASDSP            8 0
031600941130     C                   MOVE      TASMGS        TASDSP
031700000203     C****               MOVEL     TASKSC        KSCFIL            3 0
031800000203     c* controllo il libro iva e non la filiale del cliente
031900000203     C                   MOVEL     TASFIV        KSCFIL            3 0
032000941129     C                   MOVE      TASKSC        KSCCOD            4 0
032100000120     c                   move      tasccm        wccm              4 0
032200990712     C                   MOVEL     TASTBL        TIPBOL            1
032300980115     C*
032400941129     C     TASFIC        CABNE     *BLANKS       START
032500941129     C** ESCLUDO I RECORDS GIA' CONTABILIZZATI
032600980704     C     KSCFIL        IFNE      SAVFIL
032700980704     C     KSCFIL        CHAIN     AZORG01L                           05
032800010321     C   05              CLEAR                   ORGDIT
032900010321     C                   MOVEL     ORGDIT        KSCDIT
033000980704     C                   MOVEL     KSCFIL        SAVFIL
033100980704     C                   ENDIF
033200980704     C**
033300010320     C**$$**  TOLTO IL CONTROLLO DELLE BOLLE DI ALTRE AZIENDE IN QUANTO SI FATTURA TUTTO
033400010320     C**$$**  IN GAITRA201
033500980115     C** ESCLUDO QUELLE CHE NON FANNO PARTE DEL S.I.
033600980115     C**
033700950705     C                   Z-ADD     1             A
033800950705     C     TASTBL        LOOKUP    TB(A)                                  05
033900950705     C  N05              GOTO      START
034000980115     C**
034100120906      ** contabilizzo solo le spedizioni con data minore data limite fattura
034200120906     C                   If        TASDSP <= vidfat
034300950705     C     A             OCCUR     DSTB
034400950705     C     §TBFCB        IFNE      '1'
034500950705     C                   EXSR      CONBOL
034600950705     C                   EXSR      BLANK
034700950705     C                   GOTO      START
034800950705     C                   END
034900120906     c                   Endif
035000950705     C**  10 ON  TIPO BOLLA NON CONTABILIZZABILE
035100010808     C     TASNFT        CABEQ     0             START
035200010808     C** ESCLUSIONE BOLLE SENZA N.FATTURA
035300941129     C     TASFIV        CABEQ     §QCFIV        START
035400941129     C     TASFIV        CABEQ     §QCFI2        START
035500941129     C** ESCLUDO BOLLE FATTURATE IN AUTOMATICO
035600941129     C     TASDFT        CABGT     VIDFAT        START
035700941129     C** ESCLUDO TUTTE LE BOLLE CON DATA FATTURA MAGGIORE VIDFAT
035800981014     c                   move      tasdft        dataiso
035900981019     c**********         exsr      callmvc
036000981015      * ritorno da funzioni controllo date
036100981014     C                   IF        esito = *on
036200000000     C                   SETON                                        33
036300000000     C   33              EXSR      ERRORE
036400000000     C   33              GOTO      START
036500981014     C                   ENDif
036600000119     C** DATA FATTURA MINORE ULTIMO BOLLATI  (non c'è + come controllo)
036700941129     C                   EXSR      CARTA7
036800990712     C** RICERCA VARIE SU TITA7000
036900990712     C                   XFOOT     VA            TOT              19 3
037000941129     C                   ADD       TASPOR        TOT
037100930727     C     TOT           COMP      0                                      36
037200930727     C   36              EXSR      ERRORE
037300930727     C   36              GOTO      START
037400930727     C** TOTALE IMPONIBILE = 0
037500941129     C**------------------------------------------------*
037600941129     C                   MOVEL     §QCCDP        FATCDP
037700981020     c                   move      *zeros        ksc
037800981020     c                   move      tasksc        ksc
037900990712     c                   clear                   wrasoc
038000000120     c                   clear                   prtrsc
038100000121     c                   clear                   prtind
038200000120     c                   clear                   prtcap
038300000120     c                   clear                   prtloc
038400000120     c                   clear                   prtnaz
038500000120     c                   clear                   prtprv
038600110712     c                   clear                   stpcup
038700000120     C* MITTENTE NON CODIFICATO
038800000120     c                   clear                   titaa000
038900000120     c                   IF        WCCM = 8888
039000000120     c                   movel     'M'           §trc
039100000120     c                   clear                   titaa000
039200000120     c     ktaa          chain     titaa000                           06
039300000120     C                   ENDIF
039400000119     c* CLIENTI NON CODIFICATI
039500990712     c     tipbol        ifeq      'A'
039600990712     c     ksccod        andeq     9999
039700990712     c                   movel     tasrsd        wrasoc
039800000119     C                   MOVEL     TASRSD        PRTRSC
039900000119     C                   MOVEL     TASind        PRTind
040000000119     C                   MOVEL     TAScad        PRTcap
040100000119     C                   MOVEL     TASLOD        PRTLOC
040200000119     C                   MOVEL     TASnzd        PRTnaz
040300000119     C                   MOVEL     TASprd        PRTprv
040400990712     c                   GOTO      POICLI
040500990712     c                   else
040600990712     c     tipbol        ifeq      'F'
040700990712     c     ksccod        andeq     8888
040800990713     c                   movel     taarsc        wrasoc
040900000119     C                   MOVEL     TAARSC        PRTRSC
041000000119     C                   MOVEL     TAALOC        PRTLOC
041100000119     C                   MOVEL     TAAind        PRTind
041200000119     C                   MOVEL     TAAcap        PRTcap
041300000119     C                   MOVEL     TAAnaz        PRTnaz
041400000119     C                   MOVEL     TAAprv        PRTprv
041500990712     c                   GOTO      POICLI
041600990712     c                   end
041700990712     c                   end
041800941129     C**------------------------------------------------*
041900941129     C                   SETON                                        20
042000981014     C     Keycli        CHAIN     ancln01l                           34
042100981015     c                   if        clndtfirpc > *loval
042200981015     c                   seton                                        34
042300981015     c                   endif
042400981014     C  N34clnsogg       CHAIN     ansog01l                           34
042500981014     C  N34Keyind        CHAIN     anind01l                           34
042600981015     C  N34Keybac        CHAIN     ybacl01l                           34
042700891106     C   34              EXSR      ERRORE
042800891106     C   34              GOTO      START
042900941129     C* ANAGRAFICO CLIENTE
043000010321     C     *LIKE         DEFINE    ORGDIT        KSCDIT
043100980704     C     *LIKE         DEFINE    KSCFIL        SAVFIL
043200050623     C**   *LIKE         DEFINE    ASCRSD        RSD
043300050623     C**   *LIKE         DEFINE    ASCIND        IND
043400050623     C**   *LIKE         DEFINE    ASCCAP        CAP
043500050623     C**   *LIKE         DEFINE    ASCLOC        LOC
043600981015     C                   MOVEL     sogdes        RSD
043700981015     C                   MOVEL     INDindriz     IND
043800950403     C                   MOVEL     INDCAP        CAP
043900981015     C                   MOVEL     INDlocalit    LOC
044000000119     C                   MOVEL     sogdes        PRTRSC
044100000119     C                   MOVEL     indlocalit    PRTLOC
044200000120     C                   MOVEL     INDindriz     PRTind
044300000120     C                   MOVEL     INDcap        PRTcap
044400000120     C                   MOVEL     INDstato      PRTnaz
044500000120     C                   MOVEL     INDprov       PRTprv
044600981015     c                   if        tasdsp > acldus
044700981015     c                   move      tasdsp        acldus
044800981015     c                   end
044900941129     C** DATA ULTIMA SPEDIZIONE
045000981028     c                   if        acldps = 0 or tasdsp < acldps
045100981015     c                   move      tasdsp        acldps
045200981015     c                   end
045300941129     C** DATA PRIMA SPEDIZIONE
045400941129     C*
045500981015     C                   UPDATE    ybacl000
045600050509
045700050509      ** aggancio anche cnclp00f per aggiornare anche ORION senza trigger
045800050509
045900050509     c                   move      aclkcc        clpkcc
046000050509     c                   move      aclksc        clpksc
046100050509
046200050509     c     keyclp        Chain     cnclp00f
046300050509     c                   If        %found(cnclp00f)
046400050509     c                   move      acldps        clpdps
046500050509     c                   move      acldus        clpdus
046600050509     c                   update    cnclp000
046700050509     c                   endif
046800050509
046900941129     C**------------------------------------------------*
047000981015     C                   if        clnclifatt > *zeros and
047100981015     c                             clnclifatt <> clnksc
047200981015     c                   move      clnclifatt    ksc
047300981015     C     Keycli        CHAIN     ancln01l                           34
047400981015     c                   if        clndtfirpc > *loval
047500981015     c                   seton                                        34
047600981015     c                   endif
047700981015     C  N34clnsogg       CHAIN     ansog01l                           34
047800981015     C  N34Keyind        CHAIN     anind01l                           34
047900981015     C  N34Keybac        CHAIN     ybacl01l                           34
048000941129     C   34              EXSR      ERRORE
048100941129     C   34              GOTO      START
048200000120     C                   MOVEL     sogdes        PRTRSC
048300000120     C                   MOVEL     indlocalit    PRTLOC
048400000120     C                   MOVEL     INDindriz     PRTind
048500000120     C                   MOVEL     INDcap        PRTcap
048600000120     C                   MOVEL     INDstato      PRTnaz
048700000120     C                   MOVEL     INDprov       PRTprv
048800941129     C* ANAGRAFICO CLIENTE DI CONTABILITA'
048900941129     C                   END
049000000000     C     POICLI        TAG
049100941129     C**------------------------------------------------*
049200000000     C                   EXSR      CONTAB
049300891106     C** IMPUTAZIONE CONTABILE
049400000000     C                   EXSR      BLANK
049500891106     C** AZZERAMENTO CAMPI COMODO
049600000000     C                   GOTO      START
049700000000     C     FINE          TAG
049800060406      * se c'è solo una divisa non faccio 2 totali
049900060406     c                   if        div(2) = *blanks
050000060406     c                   z-add     1             cont_a
050100060406     c                   else
050200060406     c                   z-add     3             cont_a
050300060406     c                   endif
050400990714     c*
050500000121     c* stampo totali generali
050600060406     C                   DO        cont_a        A
050700000121     C     A             OCCUR     DSTOTALI
050800000121     c                   if        totimpds > 0
050900000121     c                   if        a = 1
051000000121     c                   except    gen
051100000121     c                   endif
051200000121     c                   if        a = 3
051300000121     c                   except    gengen
051400000124     c                   movel     xscdiv        div(3)
051500000121     c                   endif
051600000121     c* controllo la divisa
051700000121     C                   eval      divisa =  div(a)
051800000121     c* controllo se divisa con i decimali
051900000121     c                   exsr      ctrdiv
052000000121     c*
052100000121     C                   if        §CVFDC = 'S'                                 *DIVISA CON DECIMALI
052200000121     c                   seton                                        12
052300000121     c                   else
052400000121     c                   setoff                                       12
052500000121     c                   endif
052600000121     c*
052700000121     c                   except    gendiv
052800000121     c                   endif
052900000124     c* se la divisa è diversa dalla divisa di conto
053000000124     c     div(a)        ifne      xscdiv
053100000124     c* addiziono i valori convertiti alla divisa finale
053200000124     C                   EVAL      DIV1 = div(a)                                *
053300000124     C                   EVAL      DIV2 = XSCDIV                                *
053400000124     c                   z-add     udater        tasdft
053500000124     C*
053600000124     C* CONVERSIONE
053700000124     C                   reset                   PJX00202DS
053800000124     C                   Z-ADD     totimpds      IMPORD0202
053900000124     C                   EXSR      CNVEUR
054000000124     c                   z-add     IMPORM0202    wtotimc
054100000124     c*
054200000124     C                   reset                   PJX00202DS
054300000124     C                   Z-ADD     totivads      IMPORD0202
054400000124     C                   EXSR      CNVEUR
054500000124     c                   z-add     IMPORM0202    wtotivc
054600000124     c*
054700000124     C                   reset                   PJX00202DS
054800000124     C                   Z-ADD     totbolds      IMPORD0202
054900000124     C                   EXSR      CNVEUR
055000000124     c                   z-add     IMPORM0202    fatbolcnt
055100000124     c*
055200000124     c                   eval      dsnufat  = numfatds
055300000124     c*
055400000124     c                   eval      fattotcnt= wtotimc + wtotivc + fatbolcnt
055500000124     c     3             occur     dstotali
055600000124     c                   add       wtotimc       totimpds
055700000124     c                   add       wtotivc       totivads
055800000124     c                   add       fatbolcnt     totbolds
055900000124     c                   add       fattotcnt     totfatds
056000000124     c                   add       dsnufat       numfatds
056100000124     c                   endif
056200000124     c*
056300000124     c                   enddo
056400000121     C*
056500000121     C                   CLEAR                   DSBS02
056600000121     C                   MOVEL     'C'           T02TLA
056700000121     C                   CALL      'TIBS02R'
056800000121     C                   PARM                    KPJBA
056900000121     C                   PARM                    DSBS02
057000000121     c*
057100000121     C                   CLEAR                   DSLV55
057200000121     C                   MOVEL     'C'           D55TLA
057300000121     C                   CALL      'FNLV55R'
057400000121     C                   PARM                    DSLV55
057500000121     C*
057600000121     C                   CLEAR                   DSBS69
057700000121     C                   MOVEL     'C'           I69TLA
057800000121     C                   CALL      'TIBS69R'
057900000121     C                   PARM                    DSBS69
058000000121     C                   PARM                    DSACO
058100000121     C                   PARM                    DSIND
058200000121     C                   PARM                    DSCLP
058300000121     C                   PARM                    DSCLS
058400000121     C**
058500981020     c                   exsr      primanota
058600980527     C                   MOVEL     PARAM         KPJBU
058700000000     C                   SETON                                        LR
058800941129     C****************************************************
058900990712     C** CARICA IMPORTI VARIE DA TITA7000
059000941129     C****************************************************
059100941129     C     CARTA7        BEGSR
059200981016     C     KTA7          KLIST
059300941129     C                   KFLD                    TASAAS
059400941129     C                   KFLD                    TASLNP
059500941129     C                   KFLD                    TASNRS
059600941129     C                   KFLD                    TASNSP
059700941129     C                   KFLD                    TASTBL
059800941129     C                   DO        20            K
059900941129     C     K             IFGT      3
060000941129     C                   Z-ADD     0             VA(K)
060100941129     C                   MOVEL     *BLANKS       SV(K)
060200941129     C                   END
060300941129     C                   END
060400941129     C**
060500941129     C     TASSV3        CABEQ     *BLANKS       ENDTA7
060600990712     C     KTA7          SETLL     TITA7000                               92
060700941129     C  N92              GOTO      ENDTA7
060800941129     C                   Z-ADD     3             K
060900941129     C     SUTA7         TAG
061000990712     C     KTA7          READE     TITA7000                               92
061100941129     C   92              GOTO      ENDTA7
061200941129     C                   ADD       1             K
061300941129     C                   MOVEL     TA7SVN        SV(K)
061400941129     C                   Z-ADD     TA7VAN        VA(K)
061500941129     C                   GOTO      SUTA7
061600941129     C     ENDTA7        ENDSR
061700941129     C******************************************************
061800930314     C** CARICAMENTO SIGLE VARIE
061900941129     C******************************************************
062000941129     C     CAR$2         BEGSR
062100941129     C                   MOVEL     *BLANKS       DS$2
062200930314     C                   MOVEL     '$2'          COD
062300930314     C                   MOVEL     *BLANKS       KEY
062400930314     C                   MOVEL     '1'           KEY
062500930314     C     KTABC         CHAIN     TABEL                              07
062600981016     C   07              GOTO      FINE
062700941129     C                   MOVEL     TBLUNI        DS$2
062800930314     C                   ENDSR
062900941129     C******************************************************
063000891106     C** STAMPA ERRORI
063100941129     C******************************************************
063200891106     C     ERRORE        BEGSR
063300000000     C  N27              Z-ADD     66            CRIG
063400000000     C  N27              SETON                                        27
063500000000     C                   ADD       2             CRIG
063600000000     C     CRIG          IFGT      60
063700000000     C                   EXCEPT    HERR
063800000000     C                   Z-ADD     6             CRIG              2 0
063900000000     C                   END
064000970411     C   31              MOVEL     E(3)          TASMSG           78
064100000000     C** N.FATTURA GIA ESISTENTE SU FTNOC
064200941129     C   33              MOVEL     E(4)          TASMSG
064300000000     C** DATA FATTURA MINORE ULTIMO BOLLATO
064400941129     C   34              MOVEL     E(5)          TASMSG
064500000000     C** MANCA ANAGRAFICO CLIENTI
064600941129     C   36              MOVEL     E(6)          TASMSG
064700941129     C* IMPONIBILE = 0
064800941129     C                   EXCEPT    DERR
064900000000     C                   ENDSR
065000891106     C/SPACE
065100941128     C******************************************************
065200000000     C**         CARICAMENTO TABELLE
065300941128     C******************************************************
065400891106     C     CARTAB        BEGSR
065500941129     C** CARICAMENTO DATI VARI TASSAZIONE
065600941128     C                   EXSR      CARTB
065700941129     C** TIPI BOLLA
065800941129     C                   EXSR      CAR$2
065900941129     C** CARICA SIGLE VARIE
066000000121     C                   EXSR      CARCC
066100941129     C*+ CTB VOCI BOLLA
066200000121     C                   EXSR      CARCT
066300130122     C*+ CODICI TASSAZIONE
066400130122     C                   EXSR      CARQC
066500130122     C** TABELLA DATI VARI CONTABILITA'
066600000000     C                   EXSR      CAREI
066700941129     C* ESENZIONI IVA
066800981016     C                   EXSR      CARQt
066900990712     C** CARICAMENTO DATI VARI TASSAZIONE
067000000121     C                   EXSR      CARARR
067100000121     C** CARICAMENTO DATI imputazione contabile differenze cambio
067200000000     C                   ENDSR
067300941128     C******************************************************
067400941128     C* CARICAMENTO TIPI BOLLA
067500941128     C******************************************************
067600941128     C     CARTB         BEGSR
067700941128     C                   DO        20            A                 5 0
067800941128     C     A             OCCUR     DSTB
067900941128     C                   CLEAR                   DSTB
068000941128     C                   END
068100941128     C                   MOVEL     *BLANKS       TB
068200941128     C**
068300941128     C                   MOVEL     'TB'          COD
068400941128     C     KTAB          SETLL     TABEL                                  07
068500941128     C  N07              GOTO      ENDTB
068600941128     C                   Z-ADD     0             K                 3 0
068700941128     C                   DO        *HIVAL
068800941128     C     KTAB          READE     TABEL                                  07
068900941128     C   07              GOTO      ENDTB
069000941128     C     TBLKEY        IFNE      *BLANKS
069100941128     C                   ADD       1             K
069200941128     C                   MOVEL     TBLKEY        TB(K)
069300941128     C     K             OCCUR     DSTB
069400941128     C                   MOVEL     TBLUNI        DSTB
069500941128     C                   END
069600941128     C                   END
069700941128     C     ENDTB         ENDSR
069800941128     C******************************************************
069900941128     C** CARICAMENTO CTB VOCI BOLLA X TIPO SERVIZIO
070000941128     C******************************************************
070100981015     C     CARcc         BEGSR
070200981015     C                   DO        50            A
070300981015     C     A             OCCUR     DScc
070400981015     C                   CLEAR                   DScc
070500941129     C                   END
070600941129     C** PULIZIA DS
070700981015     C                   MOVEL     'CC'          COD
070800941129     C     KTAB          SETLL     TABEL                                  07
070900981015     C  n07              GOTO      FINE
071000941129     C                   Z-ADD     0             K
071100941129     C                   DO        *HIVAL        A
071200941129     C     KTAB          READE     TABEL                                  07
071300981015     C   07              GOTO      ENDcc
071400941129     C     TBLKEY        IFNE      *BLANKS
071500941129     C                   ADD       1             K
071600981015     C     K             OCCUR     DScc
071700981015     C                   MOVEL     TBLKEY        cC(K)
071800981015     C                   MOVEL     TBLUNI        DScc
071900941129     C                   END
072000941129     C                   END
072100000121     C     ENDcc         ENDSR
072200000121     C******************************************************
072300000121     C** CARICAMENTO CODICI TASAZIONE
072400000121     C******************************************************
072500000121     C     CARCT         BEGSR
072600041220     C                   DO        600           A
072700000121     C     A             OCCUR     DSCT
072800000121     C                   CLEAR                   DSCT
072900000121     C                   END
073000000121     C** PULIZIA DS
073100000121     C                   MOVEL     'CT'          COD
073200000121     C     KTAB          SETLL     TABEL                                  07
073300000121     C  N07              SETON                                          H7
073400000121     C   H7              GOTO      FINE
073500000121     C                   Z-ADD     0             K
073600000121     C                   DO        *HIVAL        A
073700000121     C     KTAB          READE     TABEL                                  07
073800000121     C   07              GOTO      ENDCTS
073900000121     C     TBLKEY        IFNE      *BLANKS
074000000121     C                   ADD       1             K
074100000121     C                   MOVEL     TBLKEY        CTS(K)
074200000121     C     K             OCCUR     DSCT
074300000121     C                   MOVEL     TBLUNI        DSCT
074400000121     C                   END
074500000121     C                   END
074600000121     C     ENDCTS        ENDSR
074700000121     C******************************************************
074800000121     C** CARICAMENTO TIPO SERVIZI
074900000121     C******************************************************
075000000121     C     CARTS         BEGSR
075100000121     C                   MOVEL     '5E'          COD
075200000121     C                   Z-ADD     0             K
075300000121     C     KTAB          SETLL     TABEL                                  07
075400000121     C     KTAB          READE     TABEL                                  07
075500000121    1C     *IN07         DOWEQ     *OFF
075600000121    2C     TBLKEY        IFNE      *BLANKS
075700000121     C                   MOVEL     TBLUNI        DS5E
075800000121    3C     §5EFTC        IFNE      ' '
075900000121     C                   ADD       1             K
076000000121     C                   MOVEL     TBLKEY        TS(K)
076100000121    3C                   ENDIF
076200000121    2C                   ENDIF
076300000121     C     KTAB          READE     TABEL                                  07
076400000121    1C                   ENDDO
076500000121     C*
076600000121     C                   ENDSR
076700941129     C*******************************************************
076800000000     C** CARICAMENTO DS MULTIPLA  ESENZIONI IVA
076900941129     C*******************************************************
077000891106     C     CAREI         BEGSR
077100130122
077200941129     C                   DO        50            A
077300941129     C     A             OCCUR     DSEI
077400941129     C                   CLEAR                   DSEI
077500000000     C                   END
077600941129     C                   MOVEL     *BLANKS       CEI
077700941129     C**
077800000000     C                   MOVEL     'EI'          COD
077900000000     C     KTAB          SETLL     TABEL                                  07
078000000000     C  N07              GOTO      ENDEI
078100000000     C                   Z-ADD     0             K
078200000000     C                   DO        *HIVAL        A
078300000000     C     KTAB          READE     TABEL                                  07
078400000000     C   07              GOTO      ENDEI
078500000000     C     TBLKEY        IFNE      *BLANKS
078600000000     C                   ADD       1             K
078700941129     C     K             OCCUR     DSEI
078800000000     C                   MOVEL     TBLKEY        CEI(K)
078900941129     C                   MOVEL     TBLUNI        DSEI
079000050622     C                   MOVEL     §EIDES        DEI(K)
079100130122      * mi carico la descrizione delle esenzioni iva Proj
079200130122     c                   movel     §qcriv        wtpreg                         REG.IVA VEND.
079300130122     c                   movel     §EIJEI        rivrifiva
079400130122     c                   z-add     0             rivaliq
079500130122      ** descrizione esenzione IVA
079600130122     c     Kriv          chain     anriv01l                           21
079700130122     c                   If        not  %found(anriv01l)
079800130122     c                   movel(p)  '********'    des(a)
079900130122     c                   else
080000130122     c                   movel(p)  rivdesbrev    des(a)
080100130122     c                   endif
080200130122
080300000000     C                   END
080400000000     C                   END
080500000000     C     ENDEI         TAG
080600000000     C                   ENDSR
080700000000     C**----------------------------------------
080800000000     C** ESEGUE AGGIORNAMENTO DELLE BOLLE
080900000000     C** DA NON CONTABILIZZARE
081000000000     C**----------------------------------------
081100941129     C     CONBOL        BEGSR
081200941129     C                   MOVEL     '1'           TASFTS
081300941129     C                   MOVEL     '1'           TASFIC
081400941129     C                   Z-ADD     UDATER        TASDFT
081500990712     C                   UPDATE    TITAS000
081600941129     C                   ENDSR
081700941129     C********************************************************
081800941129     C* ESEGUE IMPOSTAZIONE, RICERCA, MEMORIZZAZIONE
081900941129     C* CONTI E IMPORTI DI CONTROPARTITA CONTABILITA' E IVA
082000941129     C*  CONRIC:
082100981015     C** RICERCA IL C.CONTO/S.CONTO/VOCE NELLA DS -DSCC
082200981015     C** RELATIVA AL TIPO VOCE /TIPO BOLLA/IT-EST
082300941129     C*  CONMEM:
082400941129     C** MEMORIZZO IL CONTO E IL RELATIVO IMPORTO
082500941129     C********************************************************
082600941129     C     CON001        BEGSR
082700981015     C                   SETOFF                                       08
082800110912     C***                z-add     §Qtali        aliIVA            5 2
082900110912     c                   if        tasdft <= §qtdsa
083000110912     C                   Z-ADD     §QTALO        aliIVA            5 2
083100110912     C                   ELSE
083200110912     C                   Z-ADD     §QTALI        aliiva
083300110912     C                   ENDIF
083400981015     C                   MOVEL     *blank        eseIVA            3
083500941129     C* IMPOSTO CONTO IVA DELLA SPEDIZIONE
083600941129     C     TASFEI        IFNE      *BLANKS
083700941129     C                   Z-ADD     1             A
083800941129     C     TASFEI        LOOKUP    CEI(A)                                 05
083900941129     C  N05              DO
084000941129     C                   MOVEL     E(8)          TASMSG
084100941129     C                   SETON                                        08
084200941129     C                   EXSR      ERRORE
084300941129     C                   SETOFF                                       08
084400941129     C                   END
084500941129     C   05              DO
084600941129     C     A             OCCUR     DSEI
084700981015     C                   z-add     0             aliIVA
084800981015     C                   MOVEL     §EIjei        eseIVA
084900941129     C                   END
085000941129     C                   END
085100941129     C** CONTO IVA DELLA SPEDIZIONE
085200941129     C*---------------------------------------------------------
085300981016     C                   MOVE      *BLANKS       KCC               8            PORTO
085400981015     C                   MOVE      '1'           KCC
085500990712     C                   Z-ADD     TASPOR        WIMPOR           19 3
085600941129     C                   EXSR      CONRIC
085700941129     C                   EXSR      CONMEM
085800941129     C*
085900981015     c                   exsr      bucoiva
086000941129     C** B INDICE ALIQUOTA IVA
086100000120     C                   ADD       TASPOR        IM(B)
086200941129     C*---------------------------------------------------------
086300000125     c                   clear                   Y
086400000125     c*
086500941129     C                   DO        20            K
086600941129     C     SV(K)         IFNE      *BLANKS
086700941129     C                   MOVEL     'VARIE   '    KCC
086800981015     C                   MOVE      SV(K)         KCC                            VARIA 1
086900941129     C                   Z-ADD     VA(K)         WIMPOR
087000941129     C                   EXSR      CONRIC
087100990712     c* elaboro so se la varia fa parte del tot.imponibile
087200990712     c     §ccfl6        ifeq      'S'
087300941129     C                   EXSR      CONMEM
087400941129     C*  CTB
087500981015     c                   exsr      bucoiva
087600941129     C** B INDICE ALIQUOTA IVA
087700000119     C                   ADD       VA(K)         IM(B)
087800000119     C                   ADD       VA(K)         totvar
087900000125     c* carico la schiera di work che mi servirà per stampare solo le varie che fanno parte
088000000125     c* dell'imponibile
088100000125     c                   add       1             y
088200000125     c                   move      sv(k)         svw(y)
088300000125     c                   z-add     va(k)         vaw(y)
088400000125     c*
088500000125     c                   else
088600990712     c* se non fa parte del totale imponibile memorizzo se si tratta di
088700990712     c* bollo o iva
088800990712     c     sv(k)         ifeq      §$2bol
088900990712     c                   z-add     va(K)         tasbol
089000990712     c                   else
089100990712     c     sv(k)         ifeq      §$2iva
089200990712     c                   z-add     va(K)         tasiva
089300990712     c                   end
089400990712     c                   end
089500941129     C                   END
089600990712     C                   END
089700941129     C                   END
089800941129     C*
089900000000     C                   ENDSR
090000981015     c*-------------------------------------
090100981015     c     bucoiva       begsr
090200981015     c*-------------------------------------
090300981015     C     §ccjei        IFNE      *blank
090400981015     C                   MOVEL     §ccjei        WIVA3
090500981015     C                   move      *zeros        WIVA4
090600981015     C                   ELSE
090700981015     C                   move      aliiva        WIVA4
090800981015     C                   move      eseiva        WIVA3
090900981015     C                   END
091000981015     C** CONTO IVA
091100981015     C                   DO        5             B                 3 0
091200981015     C     AI(B)         COMP      WIVAds                                 05
091300981015     C   05              GOTO      EX1
091400981015     C     AI(B)         IFEQ      *blank
091500981015     C                   move      WIVAds        AI(B)
091600981015     C                   GOTO      EX1
091700981015     C                   END
091800981015     C                   END
091900981015     C     EX1           TAG
092000981015     c                   endsr
092100941129     C*****************************************************************
092200941129     C*    MEMORIZZA IMPORTI NELLA SCHIERA DEI CONTI
092300941129     C*****************************************************************
092400941129     C     CONMEM        BEGSR
092500941129     C                   SETOFF                                       08
092600981015     C                   MOVEL     dsconti       WCONTO           22
092700981015      *
092800941129     C                   Z-ADD     1             A
092900941129     C     WCONTO        LOOKUP    CCP(A)                                 05
093000941129     C  N05              Z-ADD     1             A
093100981015     C  N05*blank        LOOKUP    CCP(A)                                 05
093200981015     C                   movel     WCONTO        CCP(A)
093300000120     C                   ADD       WIMPOR        ICPW(A)
093400941129     C                   Z-ADD     WFCP          FCP(A)
093500941129     C                   ENDSR
093600941130     C********************************************************
093700941130     C** MEMORIZZA IVA
093800941130     C********************************************************
093900941130     C     CALIVA        BEGSR
094000941130     C                   Z-ADD     0             IV
094100941130     C     TASIVA        IFGT      0
094200941130     C                   DO        5             A
094300981015     C                   MOVEL     AI(A)         wivads
094400981020     C     wiva4         IFgt      *zeros
094500941130     C                   Z-ADD     TASIVA        IV(A)
094600941130     C                   END
094700941130     C                   END
094800941130     C                   END
094900941130     C** MEMORIZZO NEL PRIMO CONTO NON ESENTE
095000941130     C                   ENDSR
095100941129     C************************************************************
095200000000     C** CONTABILIZZAZIONE BOLLE
095300941129     C************************************************************
095400891106     C     CONTAB        BEGSR
095500990712     c                   clear                   tasiva
095600990712     c                   clear                   tasbol
095700941129     C                   EXSR      CON001
095800941129     C** IVA E CONTROPARTITE
095900981019     c                   z-add     0             riga              9 0
096000981020     c                   z-add     0             rigai             9 0
096100941130     C                   EXSR      CALIVA
096200941129     C*--------------------------------------------------------------
096300941129     C     TASBOL        IFGT      0
096400990712     C                   MOVEL     'VARIE   '    KCC
096500981015     C                   MOVE      '6'           KCC
096600941129     C                   Z-ADD     TASBOL        WIMPOR
096700941129     C                   EXSR      CONRIC
096800941129     C                   EXSR      CONMEM
096900941129     C                   END
097000941129     C** BOLLO
097100941129     C*--------------------------------------------------------------
097200000119     c* preparo i campi alla stampa
097300000119     c                   exsr      presta
097400000119     c* stampo la fattura
097500000119     c                   exsr      stafat
097600000120     c*
097700941129     C** TOTALE FATTURA
097800000120     C*---------------------------------------------------------
097900000120     c* gestione del controllo e conversione nella moneta di conto
098000000120     c                   EXSR      PREPPN
098100000120     c*
098200941129     C*---------------------------------------------------------
098300981016     c                   clear                   ndbhm000
098400981103     c                   add       1             wsubnum           9 0
098500981016     C** TIPO RECORD *A*
098600981016      * ctb generale ndbhm
098700981019     C                   MOVEL     §QCCA9        bhmcaustes
098800941129     C     KSCCOD        IFEQ      8888
098900981019     C                   MOVEL     §QCCAP        bhmcaustes
099000891117     C                   END
099100981016     c                   movel     knraz         bhmrifint
099200981103     c                   z-add     wsubnum       bhmsubnum
099300981020     c                   move      wsocieta      bhmsocieta
099400981016     c                   move      'CG'          bhmctb
099500981016     c                   move      'CG'          bhmclasse
099600981020     c                   movel     ksc           uni4              4
099700981020     c                   move      uni4          uni3              3
099800981020     c                   movel(p)  uni3          bhmunita                       uff.telecomunicaz
099900981016     c                   move      'D'           bhmtpregz
100000981020     c                   move      $kcc          bhmkcc
100100981020     c                   move      ksc           bhmksc
100200981015     c                   move      tasdft        bhmdtreg
100300981015     c                   z-add     tasnft        bhmnrreg
100400981015     c                   move      tasdft        bhmdtdoc
100500981015     C                   Z-ADD     TASNFT        bhmnrdoc
100600981015     C                   move      TASDFT        bhmdtpar
100700981015     C                   Z-ADD     TASNFT        bhmnrpar
100800981015     c                   add       1             riga
100900981015     c                   z-add     riga          bhmsubrig
101000981015     C                   movel     '2'           bhmserreg
101100981015     C                   move      TASFIV        bhmserreg
101200891107     C*
101300990712     C  N20              MOVEL     wrasoc        bhmdesbrev
101400981016     C   20              MOVEL     sogdes        bhmdesbrev
101500981016     c                   z-add     1             bhmcambio
101600000121     c                   if        xscdiv = tasdiv
101700000121     c                   movel     xscdiv        bhmdivisa
101800000121     C  n12              Z-ADD     FATTOT        bhmimportd
101900000121     C  n12              Z-ADD     FATTOT        bhmimporto
102000000121     C   12              Z-ADD     FATTOTD       bhmimportd
102100000121     C   12              Z-ADD     FATTOTD       bhmimporto
102200000121     c                   z-add     1             bhmcambio
102300000121     c                   else
102400000121     c                   movel     tasdiv        bhmdivisa
102500000121     C  n12              Z-ADD     FATTOT        bhmimportd
102600000121     C   12              Z-ADD     FATTOTD       bhmimportd
102700000121     C                   Z-ADD     FATTOTcnt     bhmimporto
102800000121     c                   z-add     cambio0202    bhmcambio
102900000121     c                   endif
103000981016     c                   move      'D'           bhmsegno
103100981019     c                   movel     §qccdp        bhmcondpag
103200981016     C                   write     ndbhm000
103300981130      *anagrafica partita
103400981130     c  n20              exsr      bh$
103500000000     C** TIPO RCORD  *C*
103600981016      * rek  IVA ndbhi
103700941129     C                   DO        5             A
103800981020     C                   CLEAR                   ndbhi000
103900981016     C     AI(A)         IFne      *blank
104000981016     c                   move      ai(a)         wivads
104100981016     c                   if        wiva3 <> *blank
104200981016     c                   move      wiva3         bhirifiva
104300981016     c                   else
104400981016     c                   move      wiva4         bhiali
104500981016     c                   end
104600981016     c                   movel     knraz         bhirifint
104700981103     c                   z-add     bhmsubnum     bhisubnum
104800981020     c                   movel     wsocieta      bhisocieta
104900981020     c                   add       1             rigai
105000981020     c                   z-add     rigai         bhisubrig
105100981016     c                   move      'A'           bhisegno
105200000121     C                   Z-ADD     IM(A)         bhiimpon
105300000121     C                   Z-ADD     IM(A)         bhiimpond
105400000121     C                   Z-ADD     IV(A)         bhiimportd
105500000121     C                   Z-ADD     IV(A)         bhiimporto
105600000121     c                   if        xscdiv <> tasdiv
105700000121     C                   Z-ADD     IMC(A)        bhiimpon
105800000121     C                   Z-ADD     IVC(A)        bhiimporto
105900000121     C                   ENDIF
106000981016     C                   move      TASFIV        bhilibro
106100981019     C                   move      '2'           bhitpreg
106200981016     C                   WRITE     ndbhi000
106300941129     C                   END
106400941129     C                   END
106500941129     C*
106600000000     C     TASBOL        IFGT      0
106700941129     C                   Z-ADD     1             A
106800990712     C                   MOVEL     'VARIE   '    KCC
106900981016     C                   MOVE      '6'           KCC
107000981016     C     KCC           LOOKUP    cC(A)                                  05
107100981016     C   05A             OCCUR     DScC
107200981016     C   05§ccjei        IFNE      *blank
107300981016     C                   move      §ccjei        bhirifiva
107400981020     C                   z-add     0             bhiali
107500981016     C                   Z-ADD     TASBOL        bhiimpon
107600981016     C                   Z-ADD     TASBOL        bhiimpond
107700981016     C                   Z-ADD     0             bhiimporto
107800981016     C                   Z-ADD     0             bhiimportd
107900000121     c                   if        xscdiv <> TASDIV
108000000121     C                   Z-ADD     FATBOLCNT     BHIIMPON
108100000121     C                   ENDIF
108200981016     C                   move      TASFIV        bhilibro
108300981203     c                   movel     knraz         bhirifint
108400981203     c                   z-add     bhmsubnum     bhisubnum
108500981203     c                   movel     wsocieta      bhisocieta
108600981203     c                   add       1             rigai
108700981203     c                   z-add     rigai         bhisubrig
108800981203     c                   move      'A'           bhisegno
108900990203     C                   move      '2'           bhitpreg
109000981016     C                   WRITE     ndbhi000
109100000000     C                   END
109200941129     C                   END
109300941129     C**  BOLLI
109400000000     C**
109500000000     C** TIPO RECORD  *D*
109600981016      * analitica ndbha
109700981016     C                   CLEAR                   ndbha000
109800941129     C                   DO        50            A
109900941129     C     ICP(A)        IFNE      *ZERO
110000981016     C                   MOVE      CCP(A)        dsconti
110100981016     c                   move      wkcc          bhmkcc
110200981016     c                   move      wksc          bhmksc
110300981016     c                   move      wvoc          bhmvoce
110400981016     C                   Z-ADD     ICP(A)        bhmimporto
110500981016     C                   Z-ADD     ICP(A)        bhmimportd
110600000121     c                   if        xscdiv <> TASDIV
110700000121     C                   Z-ADD     ICPC(A)       bhmimporto
110800000121     C                   ENDIF
110900981016     C                   Z-ADD     TASNFT        bhmnrpar
111000981016     C                   move      TASDFT        bhmdtpar
111100981016     c                   add       1             riga
111200981016     c                   z-add     riga          bhmsubrig
111300981016     c                   move      'A'           bhmsegno
111400981019      * cond pagamento
111500981019     c                   movel     §qccdp        bhmcondpag
111600981019      * ricavo data di competenza = data spedizione
111700981019      * a cambio anno = data fatturazione
111800981019     c                   move      tasdsp        bhmdtcoan
111900981019     c                   movel     tasdft        wannoft           4 0
112000981019     c                   if        wannoft <> tasaas
112100981019     c                   move      tasdft        bhmdtcoan
112200981019     c                   end
112300981016     C                   WRITE     ndbhm000
112400981016      *
112500981019     c                   move      bhmdtcoan     bhadtcoan
112600981016     c                   move      bhmrifint     bharifint
112700981016     c                   move      bhmsubnum     bhasubnum
112800981016     c                   move      bhmsubrig     bhasubrig
112900981016     c                   move      bhmvoce       bhavoce
113000981016     c                   move      'D1'          bhadimen
113100981020     c                   movel     bhmunita      bhaentit1
113200981020     c                   move      wsocieta      bhasocieta
113300981016     C                   Z-ADD     ICP(A)        bhaimporto
113400981016     C                   Z-ADD     ICP(A)        bhaimportd
113500000121     c                   if        xscdiv <> tasdiv
113600000121     C                   Z-ADD     ICPC(A)       bhaimporto
113700000121     C                   ENDIF
113800981016     c                   move      'A'           bhasegno
113900981016     c                   add       1             bhasubsubr
114000981016     c                   write     ndbha000
114100941129     C                   END
114200941129     C                   END
114300000121     c* se moneta diversa tra la divisa di fatturazione e la moneta di conto verifico se esistono
114400000121     c* delle differenze di cambio
114500000121     c                   if        xscdiv <> tasdiv
114600000121     c                   xfoot     icpc          wtoticpc
114700000121     c* sommo i  valori che mi devono dare il totale fattura
114800000121     c                   eval      wtotfat = wtoticpc + wtotivc
114900000121     c* calcolo la differenza dei totali fattura tra il convertito ed il calcolato
115000000121     c                   eval      diffe = fattotcnt - wtotfat
115100000121     C* SE ESISTE DIFFERENZA
115200000121     c*
115300000121     c                   if        diffe <> 0
115400000121     c*
115500000121     c* controllo se diffe è positivo o negativo
115600000121     c                   if        diffe < 0                                     * ARR. PASSIVO
115700000121     c* recupero conto + causale contabile
115800000121     C     2             OCCUR     DARR
115900000121     c                   move      'D'           bhmtpregz
116000000121     c                   mult      -1            diffe
116100000121     c                   else                                                    * ARR. ATTIVO
116200000121     c     1             OCCUR     DARR
116300000121     c                   move      'A'           bhmtpregz
116400000121     c                   endif
116500000121      * ctb generale ndbhm
116600000121     C                   movel     §arrcau       bhmcausrig
116700000121     c* Danilo ha detto di lasciare nft0 (ma io faccio di testa  mia)
116800000121     c                   move      §arrkcc       bhmkcc
116900000121     c                   move      §arrksc       bhmksc
117000000121     c                   move      §arrvoc       bhmvoce
117100000121     c                   add       1             riga
117200000121     c                   z-add     riga          bhmsubrig
117300000121     C*
117400000121     c                   movel     xscdiv        bhmdivisa
117500000121     c                   movel     xscdiv        bhmdivisam
117600000121     c                   z-add     1             bhmcambio
117700000121     c************       z-add     0             bhmcambiom anche questo x Danilo non è necessario
117800000121     C                   Z-ADD     diffe         bhmimporto
117900000121     C                   Z-ADD     diffe         bhmimportd
118000000121     c                   move      bhmtpregz     bhmsegno
118100000121     C                   write     ndbhm000
118200000121      *
118300000121     c                   clear                   ndbha000
118400000121     c                   move      bhmdtcoan     bhadtcoan
118500000121     c                   move      bhmrifint     bharifint
118600000121     c                   move      bhmsubnum     bhasubnum
118700000121     c                   move      bhmsubrig     bhasubrig
118800000121     c                   move      bhmvoce       bhavoce
118900000121     c                   move      'D1'          bhadimen
119000000121     c                   movel     §arrcdc       bhaentit1
119100000121     c                   move      wsocieta      bhasocieta
119200000121     C                   Z-ADD     diffe         bhaimporto
119300000121     c                   move      bhmtpregz     bhasegno
119400000121     c                   add       1             bhasubsubr
119500000121     c                   write     ndbha000
119600000121     c*
119700000121     c                   endif
119800000121     c*
119900000121     c                   endif
120000000121     c*-----------------------------------
120100000121     c* calcolo i totali
120200000121     c                   exsr      srtota
120300891107     C/SPACE
120400891107     C*-----------------------------------
120500110714    4C     KSCCOD        IFNE      9999
120600110714     C     KSCCOD        ANDNE     8888
120700941129     C                   EXSR      ASSIC
120800110714    4C                   ENDIF
120900011116     C** CREAZIONE TIASC00F ASSICURAZIONE SE OK
121000941129     C*-----------------------------------
121100941129     C                   MOVEL     '1'           TASFIC
121200991110      * aggiorno titas con excpt perchè il richiamo di TNSF20 potrebbe
121300991110      * aver sporcato dei campi con lo stesso nome del file TITAS
121400991110     C*!*                UPDATE    TITAS000
121500991110     c                   except    ugo
121600000120     C**   FLAG  INPUTAZIONE CONTABILE
121700941130     C                   ENDSR
121800000120     C***********************************************************************
121900000120     C** PREPARAZIONE IMPORTI PER REGISTRAZIONI PRIMA NOTA IN MONETA DI CONTO
122000000120     C***********************************************************************
122100000120     C     PREPPN        BEGSR                                                                     i
122200000120     c*
122300000120     c                   clear                   fattotcnt
122400000120     c                   clear                   imc
122500000120     c                   clear                   ivc
122600000120     c                   clear                   icp
122700000120     c                   clear                   icpc
122800000120     c                   clear                   fatbolcnt
122900000120     c                   clear                   wtotimc
123000000120     c                   clear                   wtotivc
123100000120     c* valorizzo i campi delle contropartite arrotondati con 2 decimali
123200000120     c* contropartite
123300000120     c                   DO        50            A
123400000120     C                   IF        icpw(a) > 0
123500000120     C                   Z-ADD(h)  icpw(a)       icp(a)
123600000120     c                   endif
123700000120     c                   enddo
123800000120     c* imponibile
123900000120     c                   DO        5             A
124000000120     C                   IF        sim(a) > 0 and not *in12
124100000120     C                   Z-ADD     sim(a)        im(a)
124200000120     c                   endif
124300000120     C                   IF        simd(a) > 0 and *in12
124400000120     c                   z-add     simd(a)       im(a)
124500000120     c                   endif
124600000120     c                   enddo
124700000120     c* iva
124800000120     c                   DO        5             A
124900000120     C                   IF        siv(a) > 0 and not *in12
125000000120     C                   Z-ADD     siv(a)        iv(a)
125100000120     c                   endif
125200000120     C                   IF        sivd(a) > 0 and *in12
125300000120     c                   z-add     sivd(a)       iv(a)
125400000120     c                   endif
125500000120     c                   enddo
125600000120     c
125700000120     C*
125800000120     C* SE MONETA DI CONTO DIVERSA DALLA MONETA DI FATTURAZIONE  CONVERSIONE DEGLI
125900000120     C* IMPORTI
126000000120     C                   IF        XSCDIV <> TASDIV
126100000120     C                   EVAL      DIV1 = tasdiv                                *
126200000120     C                   EVAL      DIV2 = XSCDIV                                *
126300000120     C*
126400000120     C* CONVERSIONE
126500000120     C* BOLLI
126600000120     c                   if        (not *in12 and fatbol > 0) or
126700000120     c                             (*in12 and fatbold > 0)
126800000120     C                   reset                   PJX00202DS
126900000120     C  n12              Z-ADD     fatbol        IMPORD0202
127000000120     C   12              Z-ADD     fatbold       IMPORD0202
127100000120     C                   EXSR      CNVEUR
127200000120     c                   z-add     IMPORM0202    fatbolcnt
127300000120     c                   endif
127400000120     C* IMPONIBILI IVA
127500000120     c*
127600000120     c                   DO        5             A
127700000120     C                   IF        im(a) > 0
127800000120     C                   reset                   PJX00202DS
127900000120     C                   Z-ADD     im(a)         IMPORD0202
128000000120     C                   EXSR      CNVEUR
128100000120     c                   z-add     IMPORM0202    imc(a)
128200000120     c                   endif
128300000120     c                   enddo
128400000120     C* IVA
128500000120     c                   DO        5             A
128600000120     C                   IF        iv(a) > 0
128700000120     c                   reset                   PJX00202DS
128800000120     C                   Z-ADD     iv(a)         IMPORD0202
128900000120     C                   EXSR      CNVEUR
129000000120     c                   z-add     IMPORM0202    ivc(a)
129100000120     c                   endif
129200000120     c                   enddo
129300000120     C* IMPORTI CONTROPARTITE
129400000120     c                   DO        50            A
129500000124     C                   IF        icp(a) > 0
129600000120     c                   reset                   PJX00202DS
129700000120     C                   Z-ADD     icp(a)        IMPORD0202
129800000120     C                   EXSR      CNVEUR
129900000120     c                   z-add     IMPORM0202    icpc(a)
130000000120     c                   endif
130100000120     c                   enddo
130200000120     c*
130300000120     c* calcolo il totale fattura da attribuire al cliente
130400000120     c* sommo imponibli iva
130500000120     c                   xfoot     imc           wtotimc
130600000120     c* sommo iva
130700000120     c                   xfoot     ivc           wtotivc
130800000120     c* sommo i  valori che mi devono dare il totale fattura
130900000120     c                   eval      fattotcnt = wtotimc + wtotivc + fatbolcnt
131000000120     c*
131100000120     c                   endif
131200000120     c*
131300000120     c                   endsr
131400000120     C*
131500000119     C******************************************************
131600000119     C**  STAMPA FATTURE
131700000119     C******************************************************
131800000119     C     STAFAT        BEGSR
131900000119     C*
132000000119     C                   Z-ADD     0             PAGE
132100000119     C**
132200000119     C                   EXCEPT    ST888
132300000119     C*
132400000119     C                   Z-ADD     4             A
132500000125     c* utilizzo la schiera delle varie da stampare
132600000119     C     A             DOWLE     20
132700000125    1C     SVW(A)        ANDNE     *BLANKS
132800000119     C                   MOVEA(P)  S(A)          S2
132900000120     C                   CLEAR                   I2
133000000120     C                   CLEAR                   I2D
133100000120     C  n12              MOVEA     I(A)          I2
133200000120     C   12              MOVEA     ID(A)         I2D
133300000119     C                   EXCEPT    ST8882
133400000119     C                   ADD       3             A
133500000119    1C                   ENDDO
133600110712      ** Verifico se presente tipo record "4" per i codici CUP-CIG
133700110712     c                   eval      §4trc = '4'
133800110712     c     Kta4          chain     tita430c
133900110712    1c                   If        %found(tita430c) and                         * codici Cup-CIG
134000110712     c                             ta4not <> *blanks
134100110712     c                   movel     ta4not        dta44
134200110712     c                   eval      StpCup = %trim(§ta4cup) + '-' +
134300110712     c                             %trim(§ta4cig)
134400110712    1c                   endif
134500110712      *
134600110712    1c                   if        StpCup <> *blanks
134700110712     C                   EXCEPT    STCUP
134800110712     c                   endif
134900110712
135000000119     C** DETTAGLIO
135100000119     C                   MOVEL     *BLANKS       LES
135200000119     C                   MOVEL     *BLANKS       LED
135300000121     C                   MOVEL     *BLANKS       LINS
135400000119     C                   MOVEA     S             LES
135500000119     C** CARICO SCHIERA SIGLE VARIE X STAMPA LEG
135600000119     C** RICERCA DELLE DESCRIZIONI
135700000119    1C     LES(1)        IFNE      *BLANKS
135800000119    2C                   DO        20            A
135900000119    3C     LES(A)        IFNE      *BLANKS
136000000119     C                   Z-ADD     1             A2                4 0
136100000119     C                   MOVEL     'VARIE   '    WKEY
136200000119     C**
136300000119     C                   MOVE      LES(A)        WKEY
136400000119     C     WKEY          LOOKUP    CC(A2)                                 05
136500000119     C   05A2            OCCUR     DSCC
136600000119     C   05              MOVEL     §CCDES        LED(A)
136700000121     C   05              MOVEL     '='           LINS(A)
136800000119    3C                   END
136900000119    2C                   ENDDO
137000000119     C** STAMPA LEGENDA SIGLE VARIE  10 SU OGNI RIGA
137100000119     C                   Z-ADD     1             A
137200000119     C     A             DOWLE     20
137300000119    2C     LES(A)        ANDNE     *BLANKS
137400000119    3C     A             IFEQ      1
137500000119     C                   EXCEPT    DCOD84
137600000119     C                   ELSE
137700000119    4C     A             IFEQ      9
137800000119     C                   EXCEPT    DCOD85
137900000119     C                   ELSE
138000000119     C                   EXCEPT    DCOD86
138100000119    4C                   END
138200000119    3C                   END
138300000119     C*
138400000119     C                   ADD       8             A
138500000119    2C                   ENDDO
138600000119    1C                   ENDIF
138700050623     C** STAMPO LE RIGHE RELATIVE AL BOLLO VIRTUALE SE ESISTE
138800050623     C                   IF        TASBOL > 0
138900060719      * verifico la data fattura se antecedente il 1/8/2006 uso la vecchia dicitura
139000060719     C                   If        TASDFT < 20060801
139100050623     C                   EVAL      STAESE = CESENZ
139200060719     C                   ELSE
139300060719     C                   EVAL      STAESE = CESENZ_NEW
139400060719     C                   ENDIF
139500050623     C                   EXCEPT    BOLVIR
139600050623     C                   ELSE
139700050623     C                   CLEAR                   STAESE
139800050623     C                   ENDIF
139900000119     C*
140000000119     C** STAMPO LE RIGHE RELATIVE ALLA RISERVATEZZA
140100000119     C                   EXCEPT    RCOD84
140200000119     C** STAMPO LA RIGA: IVA E ESIGIBILITA' IMMEDIATA
140300000119     C                   SETOFF                                       99
140400000119     C   99
140500000119     CANN99              EXCEPT    ICOD84
140600000119     C*
140700000119     C* PREPARA LA STAMPA DEI TOTALI
140800000119     C                   EXCEPT    ST88TT
140900000119     C*
141000000119     C** ESENZIONE IVA
141100130116     C                   MOVEL     *BLANKS       DESCEI           20
141200000119     C                   SETOFF                                       11
141300000119     C     TASFEI        IFNE      *BLANKS
141400000119     C                   SETON                                        11
141500000119     C                   Z-ADD     1             K
141600000119     C     TASFEI        LOOKUP    CEI(K)                                 05
141700130122     C** 05              MOVEL     DEI(K)        DESCEI
141800130122     C   05              MOVEL     DES(K)        DESCEI
141900000119     C                   END
142000000119     C*
142100000119     C                   EXCEPT    ST888T
142200000119     C                   EXCEPT    ST888G
142300000119     C                   ENDSR
142400000119     C******************************************************
142500000119     C* PREPARA I CAMPI PER LA STAMPA DETTAGLIO
142600000119     C******************************************************
142700000119     C     PRESTA        BEGSR
142800000119     C*
142900000208     c                   move      ksc           stksc
143000000208     c*
143100000120     C                   IF        tipbol = 'A'
143200000119     C** PARTITA IVA
143300000119     C                   MOVEL     TASCPD        FATIVA           16
143400000120     C                   IF        WCCM   = 8888
143500000120     C  N06              MOVEL     TAARSC        RSC13            13
143600000119     C                   ELSE
143700000119     C* Se in TITAS il mittente è codificato prendo i dati da CNIND
143800000119     C                   CLEAR                   DSBS69
143900000119     C                   MOVEL     KNSIF         I69SIF
144000000119     C                   MOVEL     KCI           I69KCC
144100000119     C                   Z-ADD     TASCCM        I69KAC
144200000119     C                   CALL      'TIBS69R'
144300000119     C                   PARM                    DSBS69
144400000119     C                   PARM                    DSACO
144500000119     C                   PARM                    DSIND
144600000119     C                   PARM                    DSCLP
144700000119     C                   PARM                    DSCLS
144800000119     C     O69ERR        IFNE      ' '
144900000119     C                   CLEAR                   DSACO
145000000119     C                   ENDIF
145100000121     C                   MOVEL     CACORAG       RSC13            13
145200000119     C                   END
145300000119     C                   ELSE
145400000120    3C     KSCCOD        IFEQ      8888
145500000120     C     *IN06         ANDEQ     *OFF
145600000119     C** PARTITA IVA
145700000119     C                   MOVEL     TAACPI        FATIVA           16
145800000119     C                   END
145900000119     C                   MOVEL     TASRSD        RSC13            13
146000000119     C                   END
146100050623      * VERIFICO SE IN FATIVA C'È LA SCRITTA DI PRIVATO E SE E' COSI PULISCO IL CAMPO
146200050623     C                   IF        %SCAN('PRIVATO':FATIVA)>0
146300050623     C                   CLEAR                   FATIVA
146400050623     C                   ENDIF
146500000119     C*
146600000119     C** DATA SPEDIZIONE
146700000119     C                   MOVE      TASMGS        GDSP              2 0
146800000119     C                   MOVEL     TASMGS        MDSP              2 0
146900000119     C                   MOVEL     *BLANKS       WTSP
147000000119     C**
147100000119     C** TIPO TARIFFA A VALORE O QUANTITA'
147200000119     C     TASCTR        COMP      400                                09  09
147300000119     C** TIPI SERVIZIO DA STAMPARE
147400000119     C     TASTSP        LOOKUP    TS                                     05
147500000119     C   05              MOVEL     TASTSP        WTSP              1
147600001215     c** Se sto elaborando una bolla della ditta SDI e il tipo servizio è C e la
147700001215     c** bolla ha data spedizione minore al 01-01-2001 e non ho trovato il
147800001215     c** il tipo servizio in tabella stampo lo stesso il tipo servizio in quanto
147900001215     c** prima della data sopra indicata il tipo servizio "C" era come E di espresso
148000001215     c*
148100010321     c                   if        not *in05 and kscdit = 'SDI' and
148200001221     c                             TAStsp = 'C' and tasdsp < 20010101
148300001215     c                   movel     TAStsp        WTSP
148400001215     c                   endif
148500001215     c*
148600000119     C**  MEMORIZZO PRIMI 3 CARATTERI DEL CODICE CLIENTE
148700000119     C                   MOVEL     TASKSC        WKSC3             3 0
148800000119     C**
148900000119     C** SIGLA AUTOM.FILIALE DI PARTENZA
149000000119     C     TASLNP        IFNE      WFIL
149100000119     C     TASLNP        CHAIN     AZORG                              05
149200000119     C                   MOVEL     TASLNP        WFIL
149300000119     C   05              CLEAR                   ORGPRO
149400000119     C                   ENDIF
149500000119     C*
149600000119     C                   MOVEL     ORGPRO        CIT2              2
149700000119     C**
149800000119     C     WKSC3         IFNE      WFIL
149900000119     C     WKSC3         CHAIN     AZORG                              05
150000000119     C                   MOVEL     WKSC3         WFIL              3 0
150100000119     C                   ENDIF
150200000119     C** PRENDO IL TERMINAL DI PARTENZA
150300060904      * chiamo Fnlv55 con tipo terminal "O" = Ottico
150400000119     C     WKSC3         IFNE      SAVTER
150500000119     C                   CLEAR                   DSLV55
150600060904     C                   MOVEL     'O'           D55TPT
150700060904     C****               MOVEL     'P'           D55TPT
150800000119     C                   MOVEL     WKSC3         D55LIN
150900000121     C                   MOVEL     UDATER        D55DRF
151000000119     C                   CALL      'FNLV55R'
151100000119     C                   PARM                    DSLV55
151200000119     C                   MOVEL     WKSC3         SAVTER            3 0
151300000119     C                   ENDIF
151400000119     C*
151500000119     C                   MOVEL     ORGDES        PFIL             20            DESCR.FILIALE
151600000119     C                   MOVEL     ORGDES        PDFI             20            DESCR.FILIALE
151700000119     C                   MOVEL     ORGIND        PVIA             20            VIA
151800000119     C                   MOVEL     ORGCPF        PCAP              5            CAP
151900000119     C                   MOVEL     ORGLOC        PLOC             20            LOCALITA
152000000119     C                   MOVEL     ORGTEL        PTEL             13            TELEFONO
152100000119     C                   MOVEL     ORGPRO        PPRV              2            PROV.
152200000119     C                   MOVEL     ORGFAX        PFAX             13            FAX
152300000119     C                   MOVEL     D55TFP        ASBOL             3 0
152400000119     C*
152500000119     C** STAMPA CAP
152600000120     C     tipbol        IFEQ      'F'
152700000119     C                   MOVEL     TASCAD        WCAP              5
152800000119     C                   ELSE
152900000119     C                   MOVEL     TASCAM        WCAP
153000000119     C                   END
153100000119     C** PROVINCIA CODICE TASSAZIONE DESTINO
153200000119     C                   Z-ADD     1             A
153300000119     C     TASCTS        LOOKUP    CTS(A)                                 05
153400000119     C   05A             OCCUR     DSCT
153500000119     C   05              MOVEL     §CTPRV        CIT3              2
153600000119     c* verifico la divisa della fattura
153700000119     C                   eval      divisa =  tasdiv
153800000119     c* controllo se divisa con i decimali
153900000119     c                   exsr      ctrdiv
154000000119     c*
154100000119     C                   if        §CVFDC = 'S'                                 *DIVISA CON DECIMALI
154200000119     c                   seton                                        12
154300000119     c                   else
154400000119     c                   setoff                                       12
154500000119     c                   endif
154600000119     c
154700000119     c* divisa senza decimali
154800000119     c                   if        not *in12
154900000120     c                   z-add     taspor        prtpor
155000000119     C                   XFOOT     IM            FATTOT
155100000119     C                   ADD       TASIVA        FATTOT
155200000119     C                   ADD       TASBOL        FATTOT
155300000119     C                   Z-ADD     TASBOL        FATBOL
155400000119     C                   Z-ADD     TASPOR        PRTTPI
155500000119     C                   Z-ADD     TOTVAR        VARIE
155600000119     c                   else
155700000120     c                   z-add     taspor        prtpord
155800000119     C                   XFOOT(h)  IM            FATTOTD
155900000119     C                   ADD(h)    TASIVA        FATTOTD
156000000119     C                   ADD(h)    TASBOL        FATTOTD
156100000119     C                   Z-ADD(h)  TASBOL        FATBOLD
156200000119     C                   Z-ADD(h)  TASPOR        PRTTPID
156300000119     C                   Z-ADD(h)  TOTVAR        VARIED
156400000119     c                   endif
156500000119     c
156600000119     C* PREPARO I CAMPI PER LA STAMPA
156700000119     C                   IF        NOT *IN12
156800000119     C                   DO        5             A
156900000119     C                   Z-ADD     IM(A)         SIM(A)
157000000119     C                   Z-ADD     IV(A)         SIV(A)
157100000119     C                   END
157200000119     C*
157300000119     C                   ELSE
157400000119     C*
157500000119     C                   DO        5             A
157600000119     C                   Z-ADD(h)  IM(A)         SIMD(A)
157700000119     C                   Z-ADD(h)  IV(A)         SIVD(A)
157800000119     C                   END
157900000119     C*
158000000119     C                   END
158100000120     C** CARICAMENTO VARIE X STAMPA
158200000120     C                   Z-ADD     0             I
158300000120     C                   Z-ADD     0             ID
158400000120     C                   MOVEL     *BLANKS       S
158500000125     C                   MOVEA     SVW           S
158600000125     C  n12              z-add     VAW           I
158700000125     C   12              z-add     VAW           ID
158800000119     C*
158900000119     C*
159000000119     C** RIF.MITTENTE NUMERICO
159100000119     C                   Z-ADD     TASRMN        RMN9              9 0
159200000119     C*
159300000119     C* VOLUME
159400000119     C     TASVLF        COMP      0                                  4242
159500000120     c* data fattura
159600000120     c     *iso          move      tasdft        datadmy
159700000121     c                   move      datadmy       dftgma
159800000121     c* recupero aliquota iva da stampare
159900000121     c                   if        tasdft <= §qtdsa
160000000121     C                   Z-ADD     §QTALO        STAALI
160100000121     C                   ELSE
160200000121     C                   Z-ADD     §QTALI        STAALI            3 1
160300000121     C                   ENDIF
160400000120     c*
160500000119     C                   ENDSR
160600891107     C/SPACE
160700000000     C**---------------------------------
160800011116     C** CREAZIONE TIASC00F
160900000000     C**---------------------------------
161000891107     C     ASSIC         BEGSR
161100980527     C                   MOVEL     *BLANKS       TASTLA
161200980527     C                   MOVEL     *BLANKS       TASERR
161300980527     C                   MOVEL     *BLANKS       TASMSG
161400991110     C                   Z-ADD     0             dsTASIMV
161500980527     C                   CLEAR                   TASIAL
161600980527     C                   CLEAR                   TASK88
161700980527     C**
161800980525     C                   Z-ADD     1             A
161900980525     C     TASTBL        LOOKUP    TB(A)                                  05
162000980525     C   05A             OCCUR     DSTB
162100980525     C   05§TBFAS        CABEQ     '0'           ENDASS
162200980525     C* CHIAMO PGM DI TASSAZIONE PER SAPERE SE E' ASSICURATO O MENO
162300980525     C*
162400980525     C* FLAG SI NO DDT
162500980525     C     TASLL1        IFEQ      'C'
162600980525     C     TASLL1        OREQ      'S'
162700980525     C                   MOVEL     'S'           TASDDT
162800980525     C                   ELSE
162900980525     C                   CLEAR                   TASDDT
163000980525     C                   ENDIF
163100980527     C* DATA CARICAMENTO TARIFFA DI CARTELLO
163200980527     C                   Z-ADD     TASDFT        TASDCT
163300011129     C* DATA TASSAZIONE FATTURA
163400030204     C**!!!              CLEAR                   DTAS01
163500011129     C                   CLEAR                   DSTASFLO
163600030204     c                   Clear                   dstaspkc
163700030204     c                   Clear                   dstasncp
163800060926     c*******            movel     tasfbr        dstasflo
163900060926     c                   clear                   dtas01
164000060926     c                   movel     tasfbr        §asfbr
164100060926     c                   movel     tascca        §ascca
164200060926     c                   eval      dstasflo = dtas01
164300020903     C****               MOVE      TASDFT        §ASDAT
164400020903     c* passo il peso vdl solo se usato anche in filiale
164500020903     c                   if        tasfpf='V'
164600030204     c**!!!              eval      §aspkc=taspkc
164700030204     c**!!!              eval      §asncp=tasncp
164800030204     c                   eval      dstaspkc=taspkc
164900030204     c                   eval      dstasncp=tasncp
165000020903     c                   endif
165100030204      * Bancali
165200030205     c                   If        %Subst(TasGva:1:1) = 'B'
165300030204     c                   Clear                   dAr5Ban
165400030911     c                   eval      kAr5Trd  = 'BAN'
165500040531     c     kFiar5        Chain     Fiar531c
165600040531If  1c                   If        %Found(Fiar531c)
165700030204     c                   Movel     Ar5Uni        dAr5Ban
165800030204    1c                   EndIf
165900030204     c                   Eval      TasBan = §Ar5Nba + §Ar5Nb2
166000030205     c                   Else
166100030205     c                   Clear                   TasBan
166200030205     c                   EndIf
166300030911      * Colli Originali
166400030911    1c                   If        %Subst(TasGva:1:1) = 'O'
166500030911     c                   Clear                   dAr5Bnb
166600030911
166700030911     c                   eval      kAr5Trd  = 'BNB'
166800040531     c     kFiar5        Chain     Fiar531c
166900040531If  1c                   If        %Found(Fiar531c)
167000030911     c                   Movel     Ar5Uni        dAr5Bnb
167100030911      *
167200030911     c                   Eval      TasNcl = §Ar5bcor
167300030911    1c                   EndIf
167400030911    1c                   EndIf
167500030204
167600980525     C* VARIA DA CALCOLARE
167700980525     C                   MOVEL     'R'           TASSVA
167800980527     C* SOLO CONTROLLO
167900980527     C                   MOVEL     'S'           PARCA
168000980527     C                   MOVEL     PARAMT        KPJBU
168100980525     C** DATA FATT X CARIC.DATI TARIFFA CARTELLO 1° VOLTA
168200980525     C                   CALL      'TNSF20R'
168300980525     C                   PARM                    KPJBA
168400991109     C                   PARM                    DTAS
168500980525     C                   PARM                    DSVAR
168600980525     C* SE NON C'E' ERRORE
168700980527    1C     TASERR        IFEQ      *BLANKS
168800990714     c* non valorizzo campo porto che ricevo da tnsf20r perchè il pgm è
168900990714     c* stato richiamato per il solo controllo della varia "R"
169000990714     c* ed essendo la bolla già tassata sarebbe un errore se il porto
169100990714     c* venisse modificato
169200980525     C                   SETOFF                                       87
169300980525     C** CLIENTE VARIO 8888 --> SOLO SE HO L'IMPORTO IN BOLLA
169400980525    2C     TASIAL        IFGT      0
169500980525     C**
169600980525    3C     TASK88        IFEQ      'S'
169700980525     C     TASIAS        ANDGT     0
169800980525     C* SOLO SE CLIENTE CODIFICATO CON MANDATO ASSICURATIVO 8888
169900980525    4C     KSCCOD        IFNE      9999
170000980525     C     KSCCOD        ANDNE     8888
170100980525     C                   SETON                                        87
170200980525    4C                   ENDIF
170300980525     C**
170400980525     C                   EXSR      MEMASS
170500980525    3C                   ENDIF
170600980525     C**
170700980525     C** CLIENTE CODIFICATO
170800980525    3C     TASK88        IFEQ      'N'
170900980525     C** MEMORIZZATA BOLLA IN TNASC00F
171000980525     C                   EXSR      MEMASS
171100980525    3C                   ENDIF
171200980525    2C                   ENDIF
171300980525    1C                   ENDIF
171400980525     C**
171500980525     C     ENDASS        ENDSR
171600891107     C**-----------------------------------------------------------
171700950331     C**  MEORIZZO SU TNASC00F LE BOLLE CON ASS. PER ASS. X CONTO -
171800891107     C**-----------------------------------------------------------
171900891107     C     MEMASS        BEGSR
172000050623     C***!!!***          Z-ADD     TASLNP        ASCLNP
172100050623     C***!!!***          Z-ADD     TASNRS        ASCNRS
172200050623     C***!!!***          Z-ADD     TASNSP        ASCNSP
172300050623     C***!!!***          MOVEL     TASTBL        ASCTBL
172400050623     C***!!!***          Z-ADD     TASKSC        ASCKSC
172500050623     C***!!!***          MOVEL     TASDSP        ANNODS            4 0
172600050623     C***!!!***          MOVE      TASDSP        MGS               4 0
172700050623     C***!!!***          MOVE      ANNODS        ASCAAS
172800050623     C***!!!***          Z-ADD     MGS           ASCMGS
172900050623     C***!!!***          Z-ADD     TASNFT        ASCNFT
173000050623     C***!!!***          Z-ADD     TASDFT        ASCDFT
173100020903     c* se devo usare il vdl lo imposto anche nel file tiasc
173200030204     c**!!!              if        §asspc='S'
173300050623     c***!!!***          if        Tasspc='S'
173400030204     c**!!!              z-add     §aspkc        ascpkg
173500050623     c***!!!***          z-add     Taspkc        ascpkg
173600050623     c***!!!***          else
173700050623     C***!!!***          Z-ADD     TASPKF        ASCPKG
173800050623     c***!!!***          endif
173900050623     C***!!!***          Z-ADD     0             ASCDST
174000050623     C***!!!***          Z-ADD     0             ASCVRR
174100050623     C***!!!***          CLEAR                   ASCVVR
174200011116      * valorizzo la divisa dell'assicurazione uguale alla divisa del cliente
174300011116     C****               MOVEL     *BLANKS       ASCVAS
174400050623     C***!!!***          MOVEL     TASDIV        ASCVAS
174500050623     C***!!!***          Z-ADD     TASIAL        ASCIAS
174600050623     C***!!!***          Z-ADD     1             A
174700980525     C* IMPOSTO LA VARIA "R"
174800050623     C***!!§$2VRR        LOOKUP    SV(A)                                  05
174900011116     C**** non è più necessario convertire in ITL in quanto passo la divisa della varia nel nuovo
175000011116     C**** file
175100011116     c**** se divisa fattura diversa da ITL converto la varia in lire
175200000120     c*
175300011116     c****               if        tasdiv <> 'ITL'
175400000120     C* CONVERTO IL LIRE ITALIANE
175500011116     C****               EVAL      DIV1 = TASDIV
175600011116     C****               EVAL      DIV2 = 'ITL'
175700011116     C****               Z-ADD     VA(A)         IMPOCNV
175800011116     C****               EXSR      SRYEUR
175900000120     C*
176000011116     C***05              Z-ADD     IMPOCNV       ASCVRR
176100011116     C****               ELSE
176200050623     C***05              Z-ADD     VA(A)         ASCVRR
176300011116      * valorizzo la divisa della varia "R"
176400050623     C***05              move      tasdiv        ascvvr
176500011116     C*****              ENDIF
176600980525     C**
176700980525     C*CLIENTE NON CODIFICATO --> 8888 O 9999
176800050623     C**   *IN20         IFEQ      *OFF
176900050623     c**   tipbol        ifeq      'A'
177000050623     c**   ksccod        andeq     9999
177100050623     C***                MOVEL     TASRSD        ASCRSD
177200050623     C***                MOVEL     TASIND        ASCIND
177300050623     C***                MOVEL     TASCAD        ASCCAP
177400050623     C***                MOVEL     TASLOD        ASCLOC
177500050623     c***                else
177600050623     c***  tipbol        ifeq      'F'
177700050623     c***  ksccod        andeq     8888
177800050623     C***                MOVEL     TAARSC        ASCRSD
177900050623     C***                MOVEL     TAAIND        ASCIND
178000050623     C***                MOVEL     TAACAP        ASCCAP
178100050623     C***                MOVEL     TAALOC        ASCLOC
178200050623     c***                end
178300050623     c***                end
178400050623     C***                ELSE
178500980525     C** CLIENTE CODIFICATO
178600050623     C***                MOVEL     *BLANKS       ASCRSD
178700050623     C***                MOVEL     *BLANKS       ASCIND
178800050623     C***                MOVEL     *BLANKS       ASCCAP
178900050623     C***                MOVEL     *BLANKS       ASCLOC
179000980525     C**
179100980525     C** 87 CODIFICATO SENZA MANDATO IN TARIFFA  CON VALORE ESPOSTO
179200050623     C** 87              DO
179300050623     C**                 MOVE      8888          ASCKSC
179400050623     C**                 MOVEL     RSD           ASCRSD
179500050623     C**                 MOVEL     IND           ASCIND
179600050623     C****               MOVEL     CAP           ASCCAP
179700050623     C***                MOVEL     LOC           ASCLOC
179800050623     C***                END
179900050623     C***                ENDIF
180000980525     C**
180100050623     C*****              WRITE     TIASC000
180200980525     C**
180300950331     C     TASIAS        IFEQ      0
180400050623     C                   Z-ADD     tasial        TASIAS
180500050623     C                   MOVE      tasdiv        TASVAS
180600011119     c                   move      'F'           TASFCM
180700950331     C                   END
180800000000     C                   ENDSR
180900891107     C/SPACE
181000000000     C**----------------------------------------
181100000000     C** AZZERAMENTO CAMPI CALCOLO
181200000000     C**----------------------------------------
181300891107     C     BLANK         BEGSR
181400000000     C                   Z-ADD     0             FATTOT
181500000119     C                   Z-ADD     0             FATTOTD
181600000121     C                   Z-ADD     0             FATTOTcnt
181700941129     C                   Z-ADD     0             IM
181800000121     C                   Z-ADD     0             IMc
181900941129     C                   Z-ADD     0             IV
182000000121     C                   Z-ADD     0             IVc
182100000119     C                   Z-ADD     0             sIM
182200000119     C                   Z-ADD     0             sIV
182300000119     C                   Z-ADD     0             sIMd
182400000120     C                   Z-ADD     0             SIVD
182500000120     C                   Z-ADD     0             prtpor
182600000120     C                   Z-ADD     0             prtpord
182700000119     C                   Z-ADD     0             prttpi
182800000119     C                   Z-ADD     0             prttpid
182900000119     C                   Z-ADD     0             varie
183000000119     C                   Z-ADD     0             varied
183100000119     C                   Z-ADD     0             fatbol
183200000119     C                   Z-ADD     0             fatbold
183300000121     C                   Z-ADD     0             fatbolcnt
183400000119     c                   Z-ADD     0             totvar
183500981015     C                   clear                   AI
183600981015     C                   clear                   CCP
183700941129     C                   Z-ADD     0             ICP
183800000121     C                   clear                   ICPW
183900000121     C                   clear                   ICPC
184000941129     C                   Z-ADD     0             FCP
184100000125     c                   CLEAR                   SVW
184200000125     C                   CLEAR                   VAW
184300000120     C                   CLEAR                   S
184400000120     C                   CLEAR                   I
184500000120     C                   CLEAR                   ID
184600000120     C                   CLEAR                   I2
184700000120     C                   CLEAR                   I2D
184800000121     C                   CLEAR                   wtotfat
184900000121     C                   CLEAR                   wtotimc
185000000121     C                   CLEAR                   wtotivc
185100000121     C                   CLEAR                   wtoticpc
185200000121     C                   CLEAR                   diffe
185300941129     C                   ENDSR
185400941129     C*****************************************************************
185500941129     C*    RICERCA CONTO RELATIVO AL TIPO IMPORTO E SPEDIZIONE        *
185600941129     C*****************************************************************
185700981015     C     cONRIC        BEGSR
185800941129     C                   SETOFF                                       08
185900941129     C                   Z-ADD     1             A
186000981015     C     KCC           LOOKUP    cC(A)                                  05
186100941129     C  N05              DO
186200941129     C                   MOVEL     E(8)          TASMSG
186300941129     C                   SETON                                        08
186400941129     C                   EXSR      ERRORE
186500941129     C                   SETOFF                                       08
186600941129     C                   Z-ADD     1             A
186700941129     C                   END
186800941129     C** TABELLA CC NON TROVATA
186900981015     C     A             OCCUR     DScC
187000981015     C                   MOVE      §cCFLP        WFCP              1 0          FLAG COMB. FIL.
187100990714     c     taslnp        lookup    est                                    05
187200990714     c  N05taslna        lookup    est                                    05
187300990714     C     *in05         ifeq      *on
187400981015     C                   move      §ccjce        WKCC
187500981015     C                   move      §ccjse        WKSC
187600981015     C                   move      §ccjve        Wvoc
187700941129     C                   ELSE
187800981015     C                   move      §ccjci        WKCC
187900981015     C                   move      §ccjsi        WKSC
188000981015     C                   move      §ccjvi        Wvoc
188100941129     C                   END
188200941129     C*
188300941129     C                   ENDSR
188400941129     C****************************************************
188500941129     C** CARICAMENTO DATI VARI CONTABILITA'
188600941129     C****************************************************
188700941129     C     CARQC         BEGSR
188800941129     C                   CLEAR                   DSQC1
188900941129     C                   CLEAR                   DSQC2
189000941129     C                   MOVEL     'QC'          COD
189100941129     C                   MOVEL     *BLANKS       KEY
189200941129     C                   MOVEL     '2'           KEY
189300941129     C     KTABC         CHAIN     TABEL                              07
189400981015     C   07              GOTO      FINE
189500941129     C                   MOVEL     TBLUNI        DSQC2
189600941129     C                   MOVEL     *BLANKS       KEY
189700941129     C                   MOVEL     '1'           KEY
189800941129     C     KTABC         CHAIN     TABEL                              07
189900981015     C   07              GOTO      FINE
190000941129     C                   MOVEL     TBLUNI        DSQC1
190100971001     C**
190200941129     C                   ENDSR
190300981016     C****************************************************
190400981016     C** CARICAMENTO DA tabella QT
190500981016     C****************************************************
190600981016     C     CARQt         BEGSR
190700981016     C                   CLEAR                   DSQt1
190800981016     C                   MOVEL     'QT'          COD
190900991109      *
191000981016     C                   MOVEL     *BLANKS       KEY
191100981016     C                   MOVEL     '1'           KEY
191200981016     C     KTABC         CHAIN     TABEL                              07
191300981016     C   07              GOTO      FINE
191400981016     C                   MOVEL     TBLUNI        DSQT1
191500981016     C**
191600981016     C                   ENDSR
191700000121     C****************************************************
191800000121     C** CARICAMENTO TABELLA ARR
191900000121     C****************************************************
192000000121     C     CARARR        BEGSR
192100000121     C*  CARICO DS ARROTONDAMENTI ATTIVI
192200000121     C                   CLEAR                   DSBS02
192300000121     C                   CLEAR                   DARR
192400000121      *
192500000121     C                   MOVEL     'L'           T02TLA
192600000121     C                   MOVEL     'C'           T02MOD
192700000121     C                   MOVEL     KNSIF         T02SIF
192800000121     C                   MOVEL     'ARR'         T02COD
192900000121     C                   MOVEL     'ATTIVO '     T02KE1
193000000121      *
193100000121     C                   CALL      'TIBS02R'
193200000121     C                   PARM                    KPJBA
193300000121     C                   PARM                    DSBS02
193400000121      *
193500000121     C     T02ERR        IFEQ      *BLANKS
193600000121     C     1             OCCUR     DARR
193700000121     C                   MOVEL     T02UNI        DARR
193800000121     C                   ENDIF
193900000121     C*  CARICO DS ARROTONDAMENTI ATTIVI
194000000121     C                   CLEAR                   DSBS02
194100000121      *
194200000121     C                   MOVEL     'L'           T02TLA
194300000121     C                   MOVEL     'C'           T02MOD
194400000121     C                   MOVEL     KNSIF         T02SIF
194500000121     C                   MOVEL     'ARR'         T02COD
194600000121     C                   MOVEL     'PASSIVO'     T02KE1
194700000121      *
194800000121     C                   CALL      'TIBS02R'
194900000121     C                   PARM                    KPJBA
195000000121     C                   PARM                    DSBS02
195100000121      *
195200000121     C     T02ERR        IFEQ      *BLANKS
195300000121     C     2             OCCUR     DARR
195400000121     C                   MOVEL     T02UNI        DARR
195500000121     C                   ENDIF
195600000121      *
195700000121     C                   ENDSR
195800000119     C****************************************************
195900000119     C** CONTROLLO DIVISA RICHIESTA
196000000119     C****************************************************
196100000119     C     CTRDIV        BEGSR
196200000119     C                   CLEAR                   DSCV
196300000119     C*
196400000119     C                   MOVEL     'CV'          COD
196500000119     C                   MOVEL     *BLANKS       KEY
196600000124     C                   MOVEL     DIVISA        KEY
196700000119     C     KTABC         CHAIN     TABEL                              07
196800000119     C   07              SETON                                        H7
196900000119     C   H7              GOTO      FINE
197000000119     C                   MOVEL     TBLUNI        DSCV
197100000119     C*
197200000119     C                   ENDSR
197300981016      *----------------------------------------------------
197400981016     C     bh$           BEGSR
197500981016     C*----------------------------------------------------
197600061109      * aggancio tita4 tipo record "Q" per un eventuale recupero del codice fiscale
197700061109     c                   clear                   dta4q
197800120518     c                   eval      §4trc = 'Q'
197900061109     c     Kta4          chain     Tita430c
198000061109     c                   If        %found(Tita430c)
198100061109     c                   eval      dta4q = ta4not
198200061109     c                   endif
198300061109
198400990517     c                   if        bhysubrig < 1
198500990517     c                   clear                   ndbhY000
198600981016     c                   end
198700990517     c                   move      knraz         bhYrifint
198800990517     c                   z-add     bhmsubnum     bhysubnum
198900990517     c                   z-add     1             bhysubrig
199000990517     c                   move      wsocieta      bhysocieta
199100990713     c* assegnato
199200990713     c                   if        tipbol = 'A' and
199300990713     c                             ksccod = 9999
199400990517     c                   movel(p)  tasrsd        bhydescr
199500990517     c                   movel     tasind        bhyindiriz
199600990713     c                   movel     tascad        bhycap
199700990517     c                   movel     taslod        bhylocalit
199800990713     c                   movel     tasprd        bhyprov
199900990517     c                   movel     'IT  '        bhystato
200000990713     c     tasnzd        ifne      *blank
200100990713     c     tasnzd        andne     'I  '
200200990713     c     tasnzd        andne     'IT '
200300990713     c                   movel     tasnzd        bhystato
200400981118     c                   endif
200500061109
200600990713     c                   movel     tascpd        bhypartiva
200700061109      * codice fiscale
200800061109     c                   movel     §ta4qcfd      bhycdfisc
200900061109
201000990713     c                   else
201100990713     c* franco
201200990713     c                   if        tipbol = 'F' and
201300990713     c                             ksccod = 8888
201400990713     c                   movel(p)  taarsc        bhydescr
201500990713     c                   movel     taaind        bhyindiriz
201600990713     c                   movel     taacap        bhycap
201700990713     c                   movel     taaloc        bhylocalit
201800990713     c                   movel     taaprv        bhyprov
201900990713     c                   movel     'IT  '        bhystato
202000990713     c     taanaz        ifne      *blank
202100990713     c     taanaz        andne     'I  '
202200990713     c     taanaz        andne     'IT '
202300990713     c                   movel     taanaz        bhystato
202400990713     c                   endif
202500990713     c                   movel     taacpi        bhypartiva
202600061109      * codice fiscale
202700061109     c                   movel     §ta4qcfm      bhycdfisc
202800990713     c                   endif
202900050623     c                   endif
203000050623      * VERIFICO SE IN BHYPARTIVA C'È LA SCRITTA DI PRIVATO  PULISCO IL CAMPO
203100050623     c                   If        %scan('PRIVATO':bhypartiva) > 0
203200050623     c                   clear                   bhypartiva
203300050623     c                   endif
203400050623
203500990517     c                   write     ndbhy000
203600981016     c                   endsr
203700981016      *----------------------------------------------------
203800981016     C     primanota     BEGSR
203900981016     C*----------------------------------------------------
204000981016     C*
204100981016     C* contabilizzazione batch
204200981020     c                   reset                   ndc071ds
204300981020     C                   MOVE      wsocieta      SOCieta071
204400981016     C                   MOVE      KNRAZ         rifint071
204500981016     c                   move      *loval        dtregda071
204600981016     c                   move      *hival        dtrega071
204700981016     c                   move      *loval        subnumd071
204800981016     c                   move      *hival        subnuma071
204900981209     C                   MOVE      *off          solcont071
205000981016    >C                   MOVE      *off          interat071
205100981016     C                   move      *off          err071
205200981203     c                   move      *off          control071
205300981016     C                   MOVEL     NDC071DS      kpjbu
205400981016     C*
205500981016     C                   CALL      'NDC071C'
205600981016     C                   parm                    kpjba
205700981016     C                   movel     kpjbu         ndc071ds
205800981016     C*
205900981016     C                   ENDSR
206000981014     C************************************************************
206100981014     C* Chiamata al modulo di controllo data registrazione
206200981014     C************************************************************
206300981014     C     CallMVC       BEGSR
206400981014     C*
206500981014     C                   move      '046'         Unita
206600981014     c                   move      dataiso       dtreg
206700981014     c                   move      dataiso       dtlgio
206800981014     C                   CALLB     'NDMVC104'
206900981020     C                   PARM      wsocieta      Societa
207000981014     C                   PARM      'CG'          CTB
207100981014     C                   PARM                    Unita
207200981014     C                   PARM      'D'           TpReg
207300981014     C                   PARM                    DTReg
207400981019     C                   PARM      '2'           RegIVA
207500981019     C                   PARM      '2'           TpRegIVA
207600981014     C                   PARM      '02'          Stato
207700981014     C                   PARM      *on           GesMsg
207800981014     C                   PARM                    Esito
207900981014     c                   if        esito = *blank
208000981014     C                   CALLB     'NDMVC106'
208100981014    >C                   PARM                    Societa
208200981014    >C                   PARM                    Ctb
208300981014    >C                   PARM                    TpReg
208400981014    >C                   PARM                    DtLGio
208500981014    >C                   PARM                    DtReg
208600981014     C                   PARM                    GesMsg
208700981014     C                   PARM                    Esito
208800981014     C*
208900981014     c                   end
209000981014     C*
209100981014     C                   ENDSR
209200981020     C*----------------------------------------------------*
209300981020     C* Reperimento dati società
209400981020     C*----------------------------------------------------*
209500981020     C     REPSOC        BEGSR
209600981020     C*
209700981020     C                   CALL      'XSOC'
209800981020     C                   PARM                    TIPXSC            6
209900981020     C                   PARM      wsocieta      SOCXSC            3
210000981020     C                   PARM                    CDSXSC            9 0
210100981020     C                   PARM                    MODXSC            3
210200981020     C                   PARM      *blanks       RTNXSC            1
210300981020     C                   PARM                    XSOCDS
210400981020     C                   PARM                    KPJBA
210500981020     C*
210600981020     C                   ENDSR
210700000121     C****************************************************
210800000121     C** CONVERSIONE IMPORTI IN UNA MONETA DIVERSA
210900000121     C****************************************************
211000000121     C     CNVEUR        BEGSR
211100000121      * conversione in moneta di conto e non
211200000121     C                   Movel     div2          DivMC0202
211300000121     C                   Movel     div1          Divisa0202
211400000121     C                   Move      *Loval        DtRif0202
211500000121     C                   Move      *On           NoCtrScos
211600000121     C                   move      tasdft        dtrif0202
211700000121     c**
211800000121     C                   CallP     X0202TestC    (PJX00202DS       :
211900000121     C                                            %Size(PJX00202DS):
212000000124     C                                            *On              :
212100000121     C                                            EsitoA           :
212200000121     C                                            NoCtrScos        :
212300000121     C                                            *Omit            :
212400000121     C                                            %Size(ANDVS00F)  :
212500000121     C                                            %Size(ANDVX00F)  :
212600000121     C                                            ANDVS00F         :
212700000121     C                                            ANDVX00F          )
212800000121     C                   endsr
212900000121      *
213000000120     c**********************************************************************
213100000120     c* COINVERSIONE SENZA TRIANGOLAZIONE
213200000120     C**********************************************************************
213300000120     C*
213400000120     C     SRYEUR        BEGSR
213500000120     C* CALCOLO L'IMPORTO D'ASSICURARE NELLA MONETA DELLA TARIFFA
213600000120     C                   CLEAR                   YEUR
213700000120     C                   movel     div1          YECDVI
213800000120     C                   movel     div2          YECDVO
213900000120     C                   z-add     impocnv       YECIMI
214000000120     C                   MOVE      '0'           YECDCO
214100000120     C*
214200000120     C                   CALL      'YEURCO'
214300000120     C                   PARM                    YEUR
214400000120     C*
214500000120     C                   IF        yecesi = ' '
214600000120     C                   Z-ADD     YECIMO        impocnv
214700000120     C                   END
214800000120     c*
214900000120     c                   ENDSR
215000000121     c**********************************************************************
215100000121     c* gestione totali per divisa e generali in moneta di conto
215200000121     C**********************************************************************
215300000121     C*
215400000121     C     SRTOTA        BEGSR
215500000121     c*
215600000121     c* verifico dove si trova la divisa in schiera
215700000121     c                   z-add     1             k
215800000121     c     tasdiv        lookup    div(k)                                 13
215900000124     c                   if        not *in13
216000000121     c                   z-add     1             k
216100000124     c     '   '         lookup    div(k)                                 13
216200000121     c                   move      tasdiv        div(k)
216300000124     c                   endif
216400000121     c   13k             occur     dstotali
216500000121     c                   do        5             a
216600000121     c                   add       im(a)         totimpds
216700000121     c                   enddo
216800000121     c*
216900000124     c                   do        5             a
217000000124     c                   add       iv(a)         totivads
217100000124     c                   enddo
217200000121     c*
217300000124     c  n12              add       fatbol        totbolds
217400000124     c   12              add       fatbold       totbolds
217500000121     c*
217600000121     c  n12              add       fattot        totfatds
217700000121     c   12              add       fattotd       totfatds
217800000121     c*
217900000121     c                   add       1             numfatds
218000000124     c*
218100000124     c                   if        tasdiv = xscdiv
218200000124     c     3             occur     dstotali
218300000124     c                   do        5             a
218400000124     c                   add       im(a)         totimpds
218500000124     c                   enddo
218600000124     c*
218700000124     c*
218800000124     c                   do        5             a
218900000124     c                   add       iv(a)         totivads
219000000124     c                   enddo
219100000124     c*
219200000124     c  n12              add       fatbol        totbolds
219300000124     c   12              add       fatbold       totbolds
219400000124     c*
219500000124     c  n12              add       fattot        totfatds
219600000124     c   12              add       fattotd       totfatds
219700000124     c*
219800000124     c                   add       1             numfatds
219900000121     c*
220000000124     c                   endif
220100000121     c*
220200000121     c                   endsr
220300000121     C*****************************************************************
220400000121     C**     SR INZIALE
220500000121     C*****************************************************************
220600000121     C     *INZSR        BEGSR
220700000121     C     *ENTRY        PLIST
220800000121     C                   PARM                    KPJBA
220900000121     C                   Z-ADD     0             VIDDFT
221000000121     C                   MOVEL     KPJBU         PARAM
221100000121     c*
221200000121     C                   Z-ADD     1             CODUT
221300000121     C                   CALL      'X§PARUT'
221400000121     C                   PARM                    UTEDSE
221500000121     C* RICERCA CAPOCONTI
221600000121     C                   DO        50            X                 2 0
221700000121     C                   MOVE      TCU(X)        TCUDS
221800000121     C     F56           CABNE     'CG'          END1
221900000121     C     F4            COMP      '1'                                    21
222000000121     C     F4            COMP      '2'                                    22
222100000121     C     F4            COMP      '3'                                    23
222200000121     C     F4            COMP      '6'                                    27
222300000121     C** 1 CLIENTI   21
222400000121     C** 2 FORNITORI 22
222500000121     C** 3 AGENTI    23
222600000121     C     F3            COMP      '0'                                242425
222700000121     C     F3            COMP      'I'                                    26
222800000121     C** 0 ITALIA   25
222900000121     C** 1 ESTERO   24
223000000121     C*  I CAPO CONTO IVA
223100000121     C   21
223200000121     CAN 24              Z-ADD     KCU(X)        KCE               4 0
223300000121     C   21
223400000121     CAN 25              Z-ADD     KCU(X)        KCI               4 0
223500000121     C   22
223600000121     CAN 24              Z-ADD     KCU(X)        KFE               4 0
223700000121     C   22
223800000121     CAN 25              Z-ADD     KCU(X)        KFI               4 0
223900000121     C   23
224000000121     CAN 24              Z-ADD     KCU(X)        KAE               4 0
224100000121     C   23
224200000121     CAN 25              Z-ADD     KCU(X)        KAI               4 0
224300000121     C   26              Z-ADD     KCU(X)        KIVA              4 0
224400000121     C   27              Z-ADD     KCU(X)        KBNA              4 0
224500000121     C     END1          TAG
224600000121     C                   END
224700000121     c                   setoff                                       212223
224800000121     c                   setoff                                       242526
224900000121     c                   setoff                                       27
225000000121     C*---------- RICERCA DITTA :
225100000121     C                   MOVEL     'SOC001'      TIPXSC
225200000121     C                   MOVEL     *BLANK        SOCXSC
225300000121     C                   EXSR      REPSOC
225400000121     C     RTNXSC        IFNE      '1'
225500000121     C                   MOVEL     XSOCDS        SOC001
225600000121     C                   MOVEL     xscrgs        RSUT             20
225700000121     c                   end
225800000121     C                   movel     wsocieta      WWWSoc            3
225900000121     C                   movel     'C     '      $kcc              6
226000000121     C                   movel     *blanks       $ksc              8
226100000121     C                   callb     'XCAPCLIFOR'
226200000121     C                   parm                    WWWSoc
226300000121     C                   parm                    $kcc
226400000121     C                   parm                    $ksc
226500000121     C*---------------------------------------------------------------*
226600000121     C* IMPOSTAZIONE CAMPI
226700000121     C     *LIKE         DEFINE    §qccdp        fatcdp
226800000121     C     *LIKE         DEFINE    TBLCOD        COD
226900000121     C     *LIKE         DEFINE    clnlineav     lin
227000000121     C     *LIKE         DEFINE    clnfiliale    fil
227100000121     C     *LIKE         DEFINE    clnKSC        KSC
227200000121     C     *LIKE         DEFINE    clnsocieta    soc
227300000121     C     *LIKE         DEFINE    indcdind      cdin
227400000121     C     *LIKE         DEFINE    indtpind      tpin
227500000121     C     *LIKE         DEFINE    tasva1        tasbol
227600000121     C     *LIKE         DEFINE    tasva1        tasiva
227700000121     C     *LIKE         DEFINE    tasrsd        wrasoc
227800000121     C     *LIKE         DEFINE    taatrc        §trc
227900000121     C*
228000000121     C/SPACE 3
228100000121     C*---------------------------------------------------------------*
228200000121     C* KLIST FILES
228300000121     C     KTAB          KLIST
228400000121     C                   KFLD                    CODUT             1 0
228500000121     C                   KFLD                    COD
228600000121     c                   z-add     1             codut
228700000121     C     KTAA          KLIST
228800000121     C                   KFLD                    TASAAS
228900000121     C                   KFLD                    TASLNP
229000000121     C                   KFLD                    TASNRS
229100000121     C                   KFLD                    TASNSP
229200000121     C                   KFLD                    §trc
229300000121     C** ACCESSO ANTAB X COD.TABELLA
229400000121     C     KTABC         KLIST
229500000121     C                   KFLD                    CODUT
229600000121     C                   KFLD                    COD
229700000121     C                   KFLD                    KEY               8
229800000121     C** ACCESSO ANTAB X COD.TABELLA
229900000121     C     Keycli        klist
230000000121     C                   kfld                    soc
230100000121     C                   kfld                    ksc
230200000121     C                   kfld                    fil
230300000121     C                   kfld                    lin
230400000121     c                   move      wsocieta      soc
230500000121     C     Keyind        KLIST
230600000121     C                   KFLD                    sogsogg
230700000121     C                   KFLD                    tpin
230800000121     C                   KFLD                    cdin
230900000121     c                   move      *blank        tpin
231000000121     c                   move      *blank        cdin
231100000121     C     Keybac        klist
231200000121     C                   kfld                    soc
231300000121     C                   kfld                    clnkcc
231400000121     C                   kfld                    clnksc
231500050509     C     Keyclp        klist
231600050509     C                   kfld                    codut
231700050509     C                   kfld                    clpkcc
231800050509     C                   kfld                    clpksc
231900000121     C     KeyTAS        klist
232000000121     C                   kfld                    kscfil
232100000121     C                   kfld                    nfthiv
232200030204
232300030204     c     kFiar5        Klist
232400030204     c                   Kfld                    TasAas
232500030204     c                   Kfld                    TasLnp
232600030204     c                   Kfld                    TasNrs
232700030204     c                   Kfld                    TasNsp
232800030204     c                   Kfld                    kAr5Trd
232900061109      * Tita4
233000061109     c     KTA4          KLIST
233100061109     c                   KFLD                    TASAAS
233200061109     c                   KFLD                    TASLNP
233300061109     c                   KFLD                    TASNRS
233400061109     c                   KFLD                    TASNSP
233500061109     c                   KFLD                    §4trc
233600130122     C     Kriv          klist
233700130122     C                   kfld                    wsocieta
233800130122     C                   kfld                    wtpreg            1
233900130122     C                   kfld                    rivaliq
234000130122     C                   kfld                    rivrifiva
234100000121     C** ACCESSO ANAGRAFICHE CLIENTI
234200000121     c                   time                    whhdat           14 0
234300000121     c                   move      whhdat        udater            8 0
234400000121     c                   move      udater        dataeur
234500000121     c                   move      dataeur       dataiso
234600000121     c                   move      dataiso       udater
234700000121     C** DATA DEL GIORNO AMG
234800000121     C     VIDDFT        CABEQ     0             FINE
234900000121     C** NON ACCETTO DATA ELABORAZIONE = 0
235000000121     C                   move      VIDDFT        dataeur
235100000121     c                   move      dataeur       dataiso
235200000121     c                   move      dataiso       vidfat            8 0
235300000121     C** DATA ELABORAZIONE RIBALTATA AMG
235400000121     C                   move      *zeros        aa                3 0
235500000121     C                   clear                   est
235600000121     C     *loval        setll     azorg01l
235700000121     C                   read      azorg01l                               07
235800000121     C     *in07         doweq     *off
235900000121     C     orgfl1        ifeq      'E'
236000000121     C                   add       1             aa
236100000121     C                   z-add     orgfil        est(aa)
236200000121     C                   end
236300000121     C                   read      azorg01l                               07
236400000121     C                   enddo
236500000121     C** CARICAMENTO SCHIERA FILIALI ESTERE
236600000121     C                   EXSR      CARTAB
236700000121     C** CARICAMENTO TABELLE
236800000121     C                   ENDSR
236900000000     OQSYSPRT   E            HERR             02
237000000000     O                       E(1)                85
237100961023     O                       VIDDFT              98 '  /  /    '
237200961023     O                                          103 '***'
237300000119     O                                          120 'TNSF04R'
237400000119     O                       PAGE1         Z    132
237500000000     O                                          128 'PAG.'
237600000000     O                       RSUT                20
237700000000     O          E            HERR             04
237800000000     O                       E(2)                66
237900941129     O          E            DERR        2
238000941129     O                                              'SPED.'
238100941129     O                       TASLNP            +  1
238200941129     O                       TASNRS        Z   +  1
238300941129     O                       TASNSP        Z   +  1
238400941129     O                       TASTBL            +  1
238500941129     O                                         +  1 'FATT.'
238600941129     O                       TASFIV            +  1
238700941129     O                                         +  0 '-'
238800941129     O                       TASNFT        Z   +  0
238900941130     O                       TASDFT            +  1 '    /  /  '
239000941129     O                       TASMSG            +  1
239100000121     O*
239200000208     OSYSPRT    E            GEN              02
239300000121     O                       RSUT                20
239400000121     O                       ST(1)               86
239500000121     O                       viddft              82 '  /  /    '
239600000121     O                                          104 'TNSF04R'
239700000121     O                       UDATE              118 '  /  /  '
239800000121     O          E            GENGEN      2
239900000121     O                       ST(2)               66
240000000121     O          E            GENDIV      2
240100000121     O                                           17 'TOTALE IMPONIBILE'
240200000124     O              N12      totimpds      2     54
240300000121     o              n12                          54 '    '
240400000124     O               12      totimpds      2     50
240500000121     O                       DIV(A)              55
240600000121     O          E            GENDIV      1
240700000121     O                                           12 'TOTALE I.V.A'
240800000124     O              N12      totivads      2     54
240900000121     o              n12                          54 '    '
241000000124     O               12      totivads      2     50
241100000121     O                       DIV(A)              55
241200000121     O          E            GENDIV      1
241300000121     O                                           12 'TOTALE BOLLI'
241400000124     O              N12      totbolds      2     54
241500000121     o              n12                          54 '    '
241600000124     O               12      totbolds      2     50
241700000121     O                       DIV(A)              55
241800000121     O          E            GENDIV      1
241900000121     O                                           16 'TOTALE FATTURATO'
242000000124     O              N12      totfatds      2     54
242100000121     o              n12                          54 '    '
242200000124     O               12      totfatds      2     50
242300000124     O                       DIV(A)              55
242400000121     O          E            GENDIV      1
242500000121     O                                           14 'NUMERO FATTURE'
242600000121     O                       numfatds      2     50
242700000120     OFTWFAT89  E            ST888            04
242800000120     O                       DFTGMA              68 '  /  /  '
242900000120     O                       TASNFT        Z     85
243000000120     O                       PAGE          Z    101
243100000120     O          E            ST888            17
243200000120     O                                              'CODICE CLIENTE:'
243300000215     O                       STKSC             +  2
243400000120     O                                         +  4 '('
243500000120     O                       ASBOL             +  0
243600000120     O                                         +  0 ')'
243700080923     O                                         +  1 '('
243800080923     O                       TASFIV            +  0
243900080923     O                                         +  0 ')'
244000000120     O          E            ST888            19
244100000120     O                                              'Per informazioni'
244200000120     O                                         +  1 'rivolgersi presso:'
244300000120     O          E            ST888            21
244400000120     O                       PDFI
244500000120     O                       PRTRSC             103
244600000120     O          E            ST888            22
244700000120     O                       PVIA
244800000120     O          E            ST888            23
244900000120     O                       PCAP
245000000120     O                       PLOC              +  2
245100000120     O                       PPRV              +  2
245200000120     O                       PRTIND              85
245300000120     O          E            ST888            24
245400000120     O                                              'Telef.:'
245500000120     O                       PTEL              +  2
245600000120     O          E            ST888            25
245700000120     O                                              'Fax   :'
245800000120     O                       PFAX              +  2
245900000120     O                       PRTCAP              64
246000000120     O                       PRTLOC            +  1
246100000120     O                       PRTPRV            +  2
246200000120     O                       PRTNAZ            +  2
246300000120     O          E            ST888            27
246400000120     O                                              'Cod.Cli.Bollettazione:'
246500000120     O                       TASKSC            +  1
246600000120     O                                         +  4 '('
246700000120     O                       ASBOL             +  0
246800000120     O                                         +  0 ')'
246900000120     O          E            ST888            29
247000061030     O*                                             'P.IVA : '
247100061030     O*                      FATIVA            +  1
247200000120     O          E            ST888            39
247300000120     O              N12      I(3)          Z    113
247400000120     O               12      ID(3)         4    113
247500000120     O                       S(3)               105
247600000120     O              N12      I(2)          Z    104
247700000120     O               12      ID(2)         4    104
247800000120     O                       S(2)                96
247900000120     O              N12      I(1)          Z     95
248000000120     O               12      ID(1)         4     95
248100000120     O                       S(1)                87
248200000120     O              N12      PRTPOR        Z     86
248300000120     O               12      PRTPORD       4     86
248400000120     O                     09TASQFT        4     77
248500000120     O                    N09TASPVL        Z     77
248600000120     O                    N09TASPKB        4     72
248700000120     O                  42N09TASVLF        2     65
248800000120     O                       TASNCL        Z     58
248900000120     O                       WCAP                53
249000000120     O                       RSC13               47
249100000120     O                       CIT3                33
249200000120     O                       CIT2                30
249300000120     O                       RMN9          Z     27
249400000120     O                       WTSP                18
249500000120     O                       MDSP                17
249600000120     O                       GDSP                15
249700000120     O                       TASNSP        Z     12
249800000120     O                       TASNRS        Z      5
249900000120     O                       TASLNP               3
250000000120     O          E            ST8882      1
250100000120     O              N12      I2(3)         Z    113
250200000120     O               12      I2D(3)        4    113
250300000120     O                       S2(3)              105
250400000120     O              N12      I2(2)         Z    104
250500000120     O               12      I2D(2)        4    104
250600000120     O                       S2(2)               96
250700000120     O              N12      I2(1)         Z     95
250800000120     O               12      I2D(1)        4     95
250900000120     O                       S2(1)               87
251000110712     O          E            STCUP       1
251100110712     O                                        +   5 'CUP-CIG :'
251200110712     O                       StpCup           +   1
251300000120     O          E            DCOD84      3
251400000120     O                                              'Legenda:'
251500000120     O          E            DCOD84      2
251600000120     O                       LES(1)
251700000121     O                       LINS(1)           +  0
251800000121     O                       LED(1)            +  0
251900000121     O                       LES(2)            +  2
252000000121     O                       LINS(2)           +  0
252100000121     O                       LED(2)            +  0
252200000121     O                       LES(3)            +  2
252300000121     O                       LINS(3)           +  0
252400000121     O                       LED(3)            +  0
252500000120     O                       LES(4)            +  2
252600000121     O                       LINS(4)           +  0
252700000120     O                       LED(4)            +  0
252800000120     O                       LES(5)            +  2
252900000121     O                       LINS(5)           +  0
253000000120     O                       LED(5)            +  0
253100000120     O                       LES(6)            +  2
253200000121     O                       LINS(6)           +  0
253300000120     O                       LED(6)            +  0
253400000120     O                       LES(7)            +  2
253500000121     O                       LINS(7)           +  0
253600000120     O                       LED(7)            +  0
253700000120     O                       LES(8)            +  2
253800000121     O                       LINS(8)           +  0
253900000120     O                       LED(8)            +  0
254000000120     O          E            DCOD85      1
254100000120     O                       LES(9)
254200000121     O                       LINS(9)           +  0
254300000120     O                       LED(9)            +  0
254400000120     O                       LES(10)           +  2
254500000121     O                       LINS(10)          +  0
254600000120     O                       LED(10)           +  0
254700000120     O                       LES(11)           +  0
254800000121     O                       LINS(11)          +  0
254900000120     O                       LED(11)           +  0
255000000120     O                       LES(12)           +  2
255100000121     O                       LINS(12)          +  0
255200000120     O                       LED(12)           +  0
255300000120     O                       LES(13)           +  2
255400000121     O                       LINS(13)          +  0
255500000120     O                       LED(13)           +  0
255600000120     O                       LES(14)           +  2
255700000121     O                       LINS(14)          +  0
255800000120     O                       LED(14)           +  0
255900000120     O                       LES(15)           +  2
256000000121     O                       LINS(15)          +  0
256100000120     O                       LED(15)           +  0
256200000120     O                       LES(16)           +  2
256300000121     O                       LINS(16)          +  0
256400000120     O                       LED(16)           +  0
256500000120     O          E            DCOD86      1
256600000120     O                       LES(17)
256700000121     O                       LINS(17)          +  0
256800000120     O                       LED(17)           +  0
256900000120     O                       LES(18)           +  2
257000000121     O                       LINS(18)          +  0
257100000120     O                       LED(18)           +  0
257200000120     O                       LES(19)           +  2
257300000121     O                       LINS(19)          +  0
257400000120     O                       LED(19)           +  0
257500000120     O                       LES(20)           +  2
257600000121     O                       LINS(20)          +  0
257700000120     O                       LED(20)           +  0
257800050623     O          E            BOLVIR      3
257900050623     O                       STAESE              85
258000000120     O          E            RCOD84      3
258100000120     O                       RI(1)
258200000120     O                       RI(2)             +  0
258300000120     O          E            RCOD84      1
258400000120     O                       RI(3)
258500000120     O                       RI(4)             +  0
258600000120     O          E            RCOD84      1
258700000120     O                       RI(5)
258800000120     O                       RI(6)             +  0
258900040702     O****      E            RCOD84      1
259000040702     O****                   RI(7)
259100000120     O          E            ICOD84           88
259200000120     O                                           72 'IVA a esigibilita'' imme'
259300000120     O                                           77 'diata'
259400000120     O          E            ST88TT           90
259500000120     O          E            ST888T      1
259600000120     O              N11n12   SIV(1)        Z     73
259700000120     O              N11 12   SIVD(1)       4     73
259800000120     O              N11      STAALI        4     61
259900130116     O               11      DESCEI              78
260000000120     O                 n12   SIM(1)        Z     55
260100000120     O                  12   SIMD(1)       4     55
260200000120     O                 n12   VARIE         Z     40
260300000120     O                  12   VARIEd        4     40
260400000120     O                 n12   PRTTPI        Z     25
260500000120     O                  12   PRTTPId       4     25
260600000120     O          E            ST888G      0
260700000120     O                 n12   FATTOT        2    112
260800000120     O                 n12   FATBOL        z     97
260900000120     O                  12   FATTOTD       4    112
261000000120     O                  12   FATBOLD       4     97
261100000124     O          E            ST888G      1
261200000124     O                       tasdiv               6
261300000124     O                                            8 ':'
261400991110     OTITAS000  E            UGO
261500991110     O                       TASfic
261600011116     O                       TASias
261700011116     O                       TASvas
261800011116     O                       TASfcm
261900941129**
262000961023             ***  IMPUTAZIONE CONTABILE FATTURE PUNTI OPERATIVI AL     ***
262100891107RILEVAZIONE ERRORI
262200891107    NUMERO DI FATTURA GIA' ESISTENTE
262300000000    DATA FATTURA MINORE ULTIMO BOLLATO
262400000000    MANCA ANAGRAFICO CLIENTE
262500930727    TOTALE IMPONIBILE = 0
262600941129
262700941129    CONTO INESISTENTE
262800000120**  DATI PERSONALI
262900040702Informativa ai sensi dell'art. 13 D.Lgs.196/03. Il trat
263000040702tamento dei dati  personali che  Vi riguardano viene
263100040702effettuato nel rispetto del D.Lgs.196/03, per finalita'
263200040702 strettamente inerenti al rapporto commerciale e fatti
263300040702salvi i diritti di accesso, opposizione e rettifica di
263400040702cui all'art. 7 del D.Lgs. stesso.
263500040702
263600000121**
263700000121            **  DISTINTA FATTURE PUNTI OPERATIVI AL   XX/XX/XX  **    1
263800000121TOTALI GENERALI                                                       2
