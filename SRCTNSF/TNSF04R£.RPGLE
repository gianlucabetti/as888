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
003500150113     D CC              S              8    DIM(90)                              VOCI BOLLA
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
008300150113     D DSCC          E DS                  OCCURS(90)
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
030100130308      * 01    - INDICA SE CLIENTE ITALIA O ESTERO
030200000120      * 06    - CHAIN TITAA00F
030300000120      * 05    - CHAIN AZORG / COMODO
030400000119      * 07    - ERRORE/COMODO
030500000119      * 12    - DIVISA CHE PREVEDE DECIMALI
030600000119      * 20    - CLIENTE CODIFICATO
030700000119     C*********--------********-------*******-------********
030800980115     C*
030900000000     C     START         TAG
031000941129     C                   SETOFF                                       332036
031100941129     C                   SETOFF                                       343181
031200990712     C                   READ      TITAS000                               02
031300000000     C   02              GOTO      FINE
031400990713     C** FINE LETTURA TITAS
031500980527     C                   CLEAR                   TASDSP
031600941130     C                   MOVEL     TASAAS        TASDSP            8 0
031700941130     C                   MOVE      TASMGS        TASDSP
031800000203     C****               MOVEL     TASKSC        KSCFIL            3 0
031900000203     c* controllo il libro iva e non la filiale del cliente
032000000203     C                   MOVEL     TASFIV        KSCFIL            3 0
032100941129     C                   MOVE      TASKSC        KSCCOD            4 0
032200000120     c                   move      tasccm        wccm              4 0
032300990712     C                   MOVEL     TASTBL        TIPBOL            1
032400980115     C*
032500941129     C     TASFIC        CABNE     *BLANKS       START
032600941129     C** ESCLUDO I RECORDS GIA' CONTABILIZZATI
032700980704     C     KSCFIL        IFNE      SAVFIL
032800980704     C     KSCFIL        CHAIN     AZORG01L                           05
032900010321     C   05              CLEAR                   ORGDIT
033000010321     C                   MOVEL     ORGDIT        KSCDIT
033100980704     C                   MOVEL     KSCFIL        SAVFIL
033200980704     C                   ENDIF
033300980704     C**
033400010320     C**$$**  TOLTO IL CONTROLLO DELLE BOLLE DI ALTRE AZIENDE IN QUANTO SI FATTURA TUTTO
033500010320     C**$$**  IN GAITRA201
033600980115     C** ESCLUDO QUELLE CHE NON FANNO PARTE DEL S.I.
033700980115     C**
033800950705     C                   Z-ADD     1             A
033900950705     C     TASTBL        LOOKUP    TB(A)                                  05
034000950705     C  N05              GOTO      START
034100980115     C**
034200120906      ** contabilizzo solo le spedizioni con data minore data limite fattura
034300120906     C                   If        TASDSP <= vidfat
034400950705     C     A             OCCUR     DSTB
034500950705     C     §TBFCB        IFNE      '1'
034600950705     C                   EXSR      CONBOL
034700950705     C                   EXSR      BLANK
034800950705     C                   GOTO      START
034900950705     C                   END
035000120906     c                   Endif
035100950705     C**  10 ON  TIPO BOLLA NON CONTABILIZZABILE
035200010808     C     TASNFT        CABEQ     0             START
035300010808     C** ESCLUSIONE BOLLE SENZA N.FATTURA
035400941129     C     TASFIV        CABEQ     §QCFIV        START
035500941129     C     TASFIV        CABEQ     §QCFI2        START
035600941129     C** ESCLUDO BOLLE FATTURATE IN AUTOMATICO
035700941129     C     TASDFT        CABGT     VIDFAT        START
035800941129     C** ESCLUDO TUTTE LE BOLLE CON DATA FATTURA MAGGIORE VIDFAT
035900981014     c                   move      tasdft        dataiso
036000981019     c**********         exsr      callmvc
036100981015      * ritorno da funzioni controllo date
036200981014     C                   IF        esito = *on
036300000000     C                   SETON                                        33
036400000000     C   33              EXSR      ERRORE
036500000000     C   33              GOTO      START
036600981014     C                   ENDif
036700000119     C** DATA FATTURA MINORE ULTIMO BOLLATI  (non c'è + come controllo)
036800941129     C                   EXSR      CARTA7
036900990712     C** RICERCA VARIE SU TITA7000
037000990712     C                   XFOOT     VA            TOT              19 3
037100941129     C                   ADD       TASPOR        TOT
037200930727     C     TOT           COMP      0                                      36
037300930727     C   36              EXSR      ERRORE
037400930727     C   36              GOTO      START
037500930727     C** TOTALE IMPONIBILE = 0
037600941129     C**------------------------------------------------*
037700941129     C                   MOVEL     §QCCDP        FATCDP
037800981020     c                   move      *zeros        ksc
037900981020     c                   move      tasksc        ksc
038000990712     c                   clear                   wrasoc
038100000120     c                   clear                   prtrsc
038200000121     c                   clear                   prtind
038300000120     c                   clear                   prtcap
038400000120     c                   clear                   prtloc
038500000120     c                   clear                   prtnaz
038600000120     c                   clear                   prtprv
038700110712     c                   clear                   stpcup
038800130308     C                   SETOFF                                       01
038900000120     C* MITTENTE NON CODIFICATO
039000000120     c                   clear                   titaa000
039100000120     c                   IF        WCCM = 8888
039200000120     c                   movel     'M'           §trc
039300000120     c                   clear                   titaa000
039400000120     c     ktaa          chain     titaa000                           06
039500000120     C                   ENDIF
039600000119     c* CLIENTI NON CODIFICATI
039700990712     c     tipbol        ifeq      'A'
039800990712     c     ksccod        andeq     9999
039900990712     c                   movel     tasrsd        wrasoc
040000000119     C                   MOVEL     TASRSD        PRTRSC
040100000119     C                   MOVEL     TASind        PRTind
040200000119     C                   MOVEL     TAScad        PRTcap
040300000119     C                   MOVEL     TASLOD        PRTLOC
040400000119     C                   MOVEL     TASnzd        PRTnaz
040500000119     C                   MOVEL     TASprd        PRTprv
040600990712     c                   GOTO      POICLI
040700990712     c                   else
040800990712     c     tipbol        ifeq      'F'
040900990712     c     ksccod        andeq     8888
041000990713     c                   movel     taarsc        wrasoc
041100000119     C                   MOVEL     TAARSC        PRTRSC
041200000119     C                   MOVEL     TAALOC        PRTLOC
041300000119     C                   MOVEL     TAAind        PRTind
041400000119     C                   MOVEL     TAAcap        PRTcap
041500000119     C                   MOVEL     TAAnaz        PRTnaz
041600000119     C                   MOVEL     TAAprv        PRTprv
041700990712     c                   GOTO      POICLI
041800990712     c                   end
041900990712     c                   end
042000941129     C**------------------------------------------------*
042100941129     C                   SETON                                        20
042200981014     C     Keycli        CHAIN     ancln01l                           34
042300981015     c                   if        clndtfirpc > *loval
042400981015     c                   seton                                        34
042500981015     c                   endif
042600981014     C  N34clnsogg       CHAIN     ansog01l                           34
042700981014     C  N34Keyind        CHAIN     anind01l                           34
042800981015     C  N34Keybac        CHAIN     ybacl01l                           34
042900891106     C   34              EXSR      ERRORE
043000891106     C   34              GOTO      START
043100941129     C* ANAGRAFICO CLIENTE
043200010321     C     *LIKE         DEFINE    ORGDIT        KSCDIT
043300980704     C     *LIKE         DEFINE    KSCFIL        SAVFIL
043400050623     C**   *LIKE         DEFINE    ASCRSD        RSD
043500050623     C**   *LIKE         DEFINE    ASCIND        IND
043600050623     C**   *LIKE         DEFINE    ASCCAP        CAP
043700050623     C**   *LIKE         DEFINE    ASCLOC        LOC
043800981015     C                   MOVEL     sogdes        RSD
043900981015     C                   MOVEL     INDindriz     IND
044000950403     C                   MOVEL     INDCAP        CAP
044100981015     C                   MOVEL     INDlocalit    LOC
044200000119     C                   MOVEL     sogdes        PRTRSC
044300000119     C                   MOVEL     indlocalit    PRTLOC
044400000120     C                   MOVEL     INDindriz     PRTind
044500000120     C                   MOVEL     INDcap        PRTcap
044600000120     C                   MOVEL     INDstato      PRTnaz
044700000120     C                   MOVEL     INDprov       PRTprv
044800981015     c                   if        tasdsp > acldus
044900981015     c                   move      tasdsp        acldus
045000981015     c                   end
045100941129     C** DATA ULTIMA SPEDIZIONE
045200981028     c                   if        acldps = 0 or tasdsp < acldps
045300981015     c                   move      tasdsp        acldps
045400981015     c                   end
045500941129     C** DATA PRIMA SPEDIZIONE
045600941129     C*
045700981015     C                   UPDATE    ybacl000
045800050509
045900050509      ** aggancio anche cnclp00f per aggiornare anche ORION senza trigger
046000050509
046100050509     c                   move      aclkcc        clpkcc
046200050509     c                   move      aclksc        clpksc
046300050509
046400050509     c     keyclp        Chain     cnclp00f
046500050509     c                   If        %found(cnclp00f)
046600050509     c                   move      acldps        clpdps
046700050509     c                   move      acldus        clpdus
046800050509     c                   update    cnclp000
046900050509     c                   endif
047000050509
047100941129     C**------------------------------------------------*
047200981015     C                   if        clnclifatt > *zeros and
047300981015     c                             clnclifatt <> clnksc
047400981015     c                   move      clnclifatt    ksc
047500981015     C     Keycli        CHAIN     ancln01l                           34
047600981015     c                   if        clndtfirpc > *loval
047700981015     c                   seton                                        34
047800981015     c                   endif
047900981015     C  N34clnsogg       CHAIN     ansog01l                           34
048000981015     C  N34Keyind        CHAIN     anind01l                           34
048100981015     C  N34Keybac        CHAIN     ybacl01l                           34
048200941129     C   34              EXSR      ERRORE
048300941129     C   34              GOTO      START
048400000120     C                   MOVEL     sogdes        PRTRSC
048500000120     C                   MOVEL     indlocalit    PRTLOC
048600000120     C                   MOVEL     INDindriz     PRTind
048700000120     C                   MOVEL     INDcap        PRTcap
048800000120     C                   MOVEL     INDstato      PRTnaz
048900000120     C                   MOVEL     INDprov       PRTprv
049000941129     C* ANAGRAFICO CLIENTE DI CONTABILITA'
049100941129     C                   END
049200130308
049300000000     C     POICLI        TAG
049400941129     C**------------------------------------------------*
049500130308      * VERIFICO SE IL CLIENTE E' ESTERO
049600130308      * Se nazione estera non stampo il codice fiscale
049700130308     c                   IF        PRTNAZ <> *blanks and PRTNAZ <> 'I' and
049800130308     c                             PRTNAZ <> 'IT'
049900130308     c                   seton                                        01
050000130308     c                   else
050100130308     c                   setoff                                       01
050200130308     c                   endif
050300130308
050400000000     C                   EXSR      CONTAB
050500891106     C** IMPUTAZIONE CONTABILE
050600000000     C                   EXSR      BLANK
050700891106     C** AZZERAMENTO CAMPI COMODO
050800000000     C                   GOTO      START
050900000000     C     FINE          TAG
051000060406      * se c'è solo una divisa non faccio 2 totali
051100060406     c                   if        div(2) = *blanks
051200060406     c                   z-add     1             cont_a
051300060406     c                   else
051400060406     c                   z-add     3             cont_a
051500060406     c                   endif
051600990714     c*
051700000121     c* stampo totali generali
051800060406     C                   DO        cont_a        A
051900000121     C     A             OCCUR     DSTOTALI
052000000121     c                   if        totimpds > 0
052100000121     c                   if        a = 1
052200000121     c                   except    gen
052300000121     c                   endif
052400000121     c                   if        a = 3
052500000121     c                   except    gengen
052600000124     c                   movel     xscdiv        div(3)
052700000121     c                   endif
052800000121     c* controllo la divisa
052900000121     C                   eval      divisa =  div(a)
053000000121     c* controllo se divisa con i decimali
053100000121     c                   exsr      ctrdiv
053200000121     c*
053300000121     C                   if        §CVFDC = 'S'                                 *DIVISA CON DECIMALI
053400000121     c                   seton                                        12
053500000121     c                   else
053600000121     c                   setoff                                       12
053700000121     c                   endif
053800000121     c*
053900000121     c                   except    gendiv
054000000121     c                   endif
054100000124     c* se la divisa è diversa dalla divisa di conto
054200000124     c     div(a)        ifne      xscdiv
054300000124     c* addiziono i valori convertiti alla divisa finale
054400000124     C                   EVAL      DIV1 = div(a)                                *
054500000124     C                   EVAL      DIV2 = XSCDIV                                *
054600000124     c                   z-add     udater        tasdft
054700000124     C*
054800000124     C* CONVERSIONE
054900000124     C                   reset                   PJX00202DS
055000000124     C                   Z-ADD     totimpds      IMPORD0202
055100000124     C                   EXSR      CNVEUR
055200000124     c                   z-add     IMPORM0202    wtotimc
055300000124     c*
055400000124     C                   reset                   PJX00202DS
055500000124     C                   Z-ADD     totivads      IMPORD0202
055600000124     C                   EXSR      CNVEUR
055700000124     c                   z-add     IMPORM0202    wtotivc
055800000124     c*
055900000124     C                   reset                   PJX00202DS
056000000124     C                   Z-ADD     totbolds      IMPORD0202
056100000124     C                   EXSR      CNVEUR
056200000124     c                   z-add     IMPORM0202    fatbolcnt
056300000124     c*
056400000124     c                   eval      dsnufat  = numfatds
056500000124     c*
056600000124     c                   eval      fattotcnt= wtotimc + wtotivc + fatbolcnt
056700000124     c     3             occur     dstotali
056800000124     c                   add       wtotimc       totimpds
056900000124     c                   add       wtotivc       totivads
057000000124     c                   add       fatbolcnt     totbolds
057100000124     c                   add       fattotcnt     totfatds
057200000124     c                   add       dsnufat       numfatds
057300000124     c                   endif
057400000124     c*
057500000124     c                   enddo
057600000121     C*
057700000121     C                   CLEAR                   DSBS02
057800000121     C                   MOVEL     'C'           T02TLA
057900000121     C                   CALL      'TIBS02R'
058000000121     C                   PARM                    KPJBA
058100000121     C                   PARM                    DSBS02
058200000121     c*
058300000121     C                   CLEAR                   DSLV55
058400000121     C                   MOVEL     'C'           D55TLA
058500000121     C                   CALL      'FNLV55R'
058600000121     C                   PARM                    DSLV55
058700000121     C*
058800000121     C                   CLEAR                   DSBS69
058900000121     C                   MOVEL     'C'           I69TLA
059000000121     C                   CALL      'TIBS69R'
059100000121     C                   PARM                    DSBS69
059200000121     C                   PARM                    DSACO
059300000121     C                   PARM                    DSIND
059400000121     C                   PARM                    DSCLP
059500000121     C                   PARM                    DSCLS
059600000121     C**
059700981020     c                   exsr      primanota
059800980527     C                   MOVEL     PARAM         KPJBU
059900000000     C                   SETON                                        LR
060000941129     C****************************************************
060100990712     C** CARICA IMPORTI VARIE DA TITA7000
060200941129     C****************************************************
060300941129     C     CARTA7        BEGSR
060400981016     C     KTA7          KLIST
060500941129     C                   KFLD                    TASAAS
060600941129     C                   KFLD                    TASLNP
060700941129     C                   KFLD                    TASNRS
060800941129     C                   KFLD                    TASNSP
060900941129     C                   KFLD                    TASTBL
061000941129     C                   DO        20            K
061100941129     C     K             IFGT      3
061200941129     C                   Z-ADD     0             VA(K)
061300941129     C                   MOVEL     *BLANKS       SV(K)
061400941129     C                   END
061500941129     C                   END
061600941129     C**
061700941129     C     TASSV3        CABEQ     *BLANKS       ENDTA7
061800990712     C     KTA7          SETLL     TITA7000                               92
061900941129     C  N92              GOTO      ENDTA7
062000941129     C                   Z-ADD     3             K
062100941129     C     SUTA7         TAG
062200990712     C     KTA7          READE     TITA7000                               92
062300941129     C   92              GOTO      ENDTA7
062400941129     C                   ADD       1             K
062500941129     C                   MOVEL     TA7SVN        SV(K)
062600941129     C                   Z-ADD     TA7VAN        VA(K)
062700941129     C                   GOTO      SUTA7
062800941129     C     ENDTA7        ENDSR
062900941129     C******************************************************
063000930314     C** CARICAMENTO SIGLE VARIE
063100941129     C******************************************************
063200941129     C     CAR$2         BEGSR
063300941129     C                   MOVEL     *BLANKS       DS$2
063400930314     C                   MOVEL     '$2'          COD
063500930314     C                   MOVEL     *BLANKS       KEY
063600930314     C                   MOVEL     '1'           KEY
063700930314     C     KTABC         CHAIN     TABEL                              07
063800981016     C   07              GOTO      FINE
063900941129     C                   MOVEL     TBLUNI        DS$2
064000930314     C                   ENDSR
064100941129     C******************************************************
064200891106     C** STAMPA ERRORI
064300941129     C******************************************************
064400891106     C     ERRORE        BEGSR
064500000000     C  N27              Z-ADD     66            CRIG
064600000000     C  N27              SETON                                        27
064700000000     C                   ADD       2             CRIG
064800000000     C     CRIG          IFGT      60
064900000000     C                   EXCEPT    HERR
065000000000     C                   Z-ADD     6             CRIG              2 0
065100000000     C                   END
065200970411     C   31              MOVEL     E(3)          TASMSG           78
065300000000     C** N.FATTURA GIA ESISTENTE SU FTNOC
065400941129     C   33              MOVEL     E(4)          TASMSG
065500000000     C** DATA FATTURA MINORE ULTIMO BOLLATO
065600941129     C   34              MOVEL     E(5)          TASMSG
065700000000     C** MANCA ANAGRAFICO CLIENTI
065800941129     C   36              MOVEL     E(6)          TASMSG
065900941129     C* IMPONIBILE = 0
066000941129     C                   EXCEPT    DERR
066100000000     C                   ENDSR
066200891106     C/SPACE
066300941128     C******************************************************
066400000000     C**         CARICAMENTO TABELLE
066500941128     C******************************************************
066600891106     C     CARTAB        BEGSR
066700941129     C** CARICAMENTO DATI VARI TASSAZIONE
066800941128     C                   EXSR      CARTB
066900941129     C** TIPI BOLLA
067000941129     C                   EXSR      CAR$2
067100941129     C** CARICA SIGLE VARIE
067200000121     C                   EXSR      CARCC
067300941129     C*+ CTB VOCI BOLLA
067400000121     C                   EXSR      CARCT
067500130122     C*+ CODICI TASSAZIONE
067600130122     C                   EXSR      CARQC
067700130122     C** TABELLA DATI VARI CONTABILITA'
067800000000     C                   EXSR      CAREI
067900941129     C* ESENZIONI IVA
068000981016     C                   EXSR      CARQt
068100990712     C** CARICAMENTO DATI VARI TASSAZIONE
068200000121     C                   EXSR      CARARR
068300000121     C** CARICAMENTO DATI imputazione contabile differenze cambio
068400000000     C                   ENDSR
068500941128     C******************************************************
068600941128     C* CARICAMENTO TIPI BOLLA
068700941128     C******************************************************
068800941128     C     CARTB         BEGSR
068900941128     C                   DO        20            A                 5 0
069000941128     C     A             OCCUR     DSTB
069100941128     C                   CLEAR                   DSTB
069200941128     C                   END
069300941128     C                   MOVEL     *BLANKS       TB
069400941128     C**
069500941128     C                   MOVEL     'TB'          COD
069600941128     C     KTAB          SETLL     TABEL                                  07
069700941128     C  N07              GOTO      ENDTB
069800941128     C                   Z-ADD     0             K                 3 0
069900941128     C                   DO        *HIVAL
070000941128     C     KTAB          READE     TABEL                                  07
070100941128     C   07              GOTO      ENDTB
070200941128     C     TBLKEY        IFNE      *BLANKS
070300941128     C                   ADD       1             K
070400941128     C                   MOVEL     TBLKEY        TB(K)
070500941128     C     K             OCCUR     DSTB
070600941128     C                   MOVEL     TBLUNI        DSTB
070700941128     C                   END
070800941128     C                   END
070900941128     C     ENDTB         ENDSR
071000941128     C******************************************************
071100941128     C** CARICAMENTO CTB VOCI BOLLA X TIPO SERVIZIO
071200941128     C******************************************************
071300981015     C     CARcc         BEGSR
071400150113     C                   DO        90            A
071500981015     C     A             OCCUR     DScc
071600981015     C                   CLEAR                   DScc
071700941129     C                   END
071800941129     C** PULIZIA DS
071900981015     C                   MOVEL     'CC'          COD
072000941129     C     KTAB          SETLL     TABEL                                  07
072100981015     C  n07              GOTO      FINE
072200941129     C                   Z-ADD     0             K
072300941129     C                   DO        *HIVAL        A
072400941129     C     KTAB          READE     TABEL                                  07
072500981015     C   07              GOTO      ENDcc
072600941129     C     TBLKEY        IFNE      *BLANKS
072700941129     C                   ADD       1             K
072800981015     C     K             OCCUR     DScc
072900981015     C                   MOVEL     TBLKEY        cC(K)
073000981015     C                   MOVEL     TBLUNI        DScc
073100941129     C                   END
073200941129     C                   END
073300000121     C     ENDcc         ENDSR
073400000121     C******************************************************
073500000121     C** CARICAMENTO CODICI TASAZIONE
073600000121     C******************************************************
073700000121     C     CARCT         BEGSR
073800041220     C                   DO        600           A
073900000121     C     A             OCCUR     DSCT
074000000121     C                   CLEAR                   DSCT
074100000121     C                   END
074200000121     C** PULIZIA DS
074300000121     C                   MOVEL     'CT'          COD
074400000121     C     KTAB          SETLL     TABEL                                  07
074500000121     C  N07              SETON                                          H7
074600000121     C   H7              GOTO      FINE
074700000121     C                   Z-ADD     0             K
074800000121     C                   DO        *HIVAL        A
074900000121     C     KTAB          READE     TABEL                                  07
075000000121     C   07              GOTO      ENDCTS
075100000121     C     TBLKEY        IFNE      *BLANKS
075200000121     C                   ADD       1             K
075300000121     C                   MOVEL     TBLKEY        CTS(K)
075400000121     C     K             OCCUR     DSCT
075500000121     C                   MOVEL     TBLUNI        DSCT
075600000121     C                   END
075700000121     C                   END
075800000121     C     ENDCTS        ENDSR
075900000121     C******************************************************
076000000121     C** CARICAMENTO TIPO SERVIZI
076100000121     C******************************************************
076200000121     C     CARTS         BEGSR
076300000121     C                   MOVEL     '5E'          COD
076400000121     C                   Z-ADD     0             K
076500000121     C     KTAB          SETLL     TABEL                                  07
076600000121     C     KTAB          READE     TABEL                                  07
076700000121    1C     *IN07         DOWEQ     *OFF
076800000121    2C     TBLKEY        IFNE      *BLANKS
076900000121     C                   MOVEL     TBLUNI        DS5E
077000000121    3C     §5EFTC        IFNE      ' '
077100000121     C                   ADD       1             K
077200000121     C                   MOVEL     TBLKEY        TS(K)
077300000121    3C                   ENDIF
077400000121    2C                   ENDIF
077500000121     C     KTAB          READE     TABEL                                  07
077600000121    1C                   ENDDO
077700000121     C*
077800000121     C                   ENDSR
077900941129     C*******************************************************
078000000000     C** CARICAMENTO DS MULTIPLA  ESENZIONI IVA
078100941129     C*******************************************************
078200891106     C     CAREI         BEGSR
078300130122
078400941129     C                   DO        50            A
078500941129     C     A             OCCUR     DSEI
078600941129     C                   CLEAR                   DSEI
078700000000     C                   END
078800941129     C                   MOVEL     *BLANKS       CEI
078900941129     C**
079000000000     C                   MOVEL     'EI'          COD
079100000000     C     KTAB          SETLL     TABEL                                  07
079200000000     C  N07              GOTO      ENDEI
079300000000     C                   Z-ADD     0             K
079400000000     C                   DO        *HIVAL        A
079500000000     C     KTAB          READE     TABEL                                  07
079600000000     C   07              GOTO      ENDEI
079700000000     C     TBLKEY        IFNE      *BLANKS
079800000000     C                   ADD       1             K
079900941129     C     K             OCCUR     DSEI
080000000000     C                   MOVEL     TBLKEY        CEI(K)
080100941129     C                   MOVEL     TBLUNI        DSEI
080200050622     C                   MOVEL     §EIDES        DEI(K)
080300130122      * mi carico la descrizione delle esenzioni iva Proj
080400130122     c                   movel     §qcriv        wtpreg                         REG.IVA VEND.
080500130122     c                   movel     §EIJEI        rivrifiva
080600130122     c                   z-add     0             rivaliq
080700130122      ** descrizione esenzione IVA
080800130122     c     Kriv          chain     anriv01l                           21
080900130122     c                   If        not  %found(anriv01l)
081000130122     c                   movel(p)  '********'    des(a)
081100130122     c                   else
081200130122     c                   movel(p)  rivdesbrev    des(a)
081300130122     c                   endif
081400130122
081500000000     C                   END
081600000000     C                   END
081700000000     C     ENDEI         TAG
081800000000     C                   ENDSR
081900000000     C**----------------------------------------
082000000000     C** ESEGUE AGGIORNAMENTO DELLE BOLLE
082100000000     C** DA NON CONTABILIZZARE
082200000000     C**----------------------------------------
082300941129     C     CONBOL        BEGSR
082400941129     C                   MOVEL     '1'           TASFTS
082500941129     C                   MOVEL     '1'           TASFIC
082600160901     C******             Z-ADD     UDATER        TASDFT
082700990712     C                   UPDATE    TITAS000
082800941129     C                   ENDSR
082900941129     C********************************************************
083000941129     C* ESEGUE IMPOSTAZIONE, RICERCA, MEMORIZZAZIONE
083100941129     C* CONTI E IMPORTI DI CONTROPARTITA CONTABILITA' E IVA
083200941129     C*  CONRIC:
083300981015     C** RICERCA IL C.CONTO/S.CONTO/VOCE NELLA DS -DSCC
083400981015     C** RELATIVA AL TIPO VOCE /TIPO BOLLA/IT-EST
083500941129     C*  CONMEM:
083600941129     C** MEMORIZZO IL CONTO E IL RELATIVO IMPORTO
083700941129     C********************************************************
083800941129     C     CON001        BEGSR
083900981015     C                   SETOFF                                       08
084000110912     C***                z-add     §Qtali        aliIVA            5 2
084100110912     c                   if        tasdft <= §qtdsa
084200110912     C                   Z-ADD     §QTALO        aliIVA            5 2
084300110912     C                   ELSE
084400110912     C                   Z-ADD     §QTALI        aliiva
084500110912     C                   ENDIF
084600981015     C                   MOVEL     *blank        eseIVA            3
084700941129     C* IMPOSTO CONTO IVA DELLA SPEDIZIONE
084800941129     C     TASFEI        IFNE      *BLANKS
084900941129     C                   Z-ADD     1             A
085000941129     C     TASFEI        LOOKUP    CEI(A)                                 05
085100941129     C  N05              DO
085200941129     C                   MOVEL     E(8)          TASMSG
085300941129     C                   SETON                                        08
085400941129     C                   EXSR      ERRORE
085500941129     C                   SETOFF                                       08
085600941129     C                   END
085700941129     C   05              DO
085800941129     C     A             OCCUR     DSEI
085900981015     C                   z-add     0             aliIVA
086000981015     C                   MOVEL     §EIjei        eseIVA
086100941129     C                   END
086200941129     C                   END
086300941129     C** CONTO IVA DELLA SPEDIZIONE
086400941129     C*---------------------------------------------------------
086500981016     C                   MOVE      *BLANKS       KCC               8            PORTO
086600981015     C                   MOVE      '1'           KCC
086700990712     C                   Z-ADD     TASPOR        WIMPOR           19 3
086800941129     C                   EXSR      CONRIC
086900941129     C                   EXSR      CONMEM
087000941129     C*
087100981015     c                   exsr      bucoiva
087200941129     C** B INDICE ALIQUOTA IVA
087300000120     C                   ADD       TASPOR        IM(B)
087400941129     C*---------------------------------------------------------
087500000125     c                   clear                   Y
087600000125     c*
087700941129     C                   DO        20            K
087800941129     C     SV(K)         IFNE      *BLANKS
087900941129     C                   MOVEL     'VARIE   '    KCC
088000981015     C                   MOVE      SV(K)         KCC                            VARIA 1
088100941129     C                   Z-ADD     VA(K)         WIMPOR
088200941129     C                   EXSR      CONRIC
088300990712     c* elaboro so se la varia fa parte del tot.imponibile
088400990712     c     §ccfl6        ifeq      'S'
088500941129     C                   EXSR      CONMEM
088600941129     C*  CTB
088700981015     c                   exsr      bucoiva
088800941129     C** B INDICE ALIQUOTA IVA
088900000119     C                   ADD       VA(K)         IM(B)
089000000119     C                   ADD       VA(K)         totvar
089100000125     c* carico la schiera di work che mi servirà per stampare solo le varie che fanno parte
089200000125     c* dell'imponibile
089300000125     c                   add       1             y
089400000125     c                   move      sv(k)         svw(y)
089500000125     c                   z-add     va(k)         vaw(y)
089600000125     c*
089700000125     c                   else
089800990712     c* se non fa parte del totale imponibile memorizzo se si tratta di
089900990712     c* bollo o iva
090000990712     c     sv(k)         ifeq      §$2bol
090100990712     c                   z-add     va(K)         tasbol
090200990712     c                   else
090300990712     c     sv(k)         ifeq      §$2iva
090400990712     c                   z-add     va(K)         tasiva
090500990712     c                   end
090600990712     c                   end
090700941129     C                   END
090800990712     C                   END
090900941129     C                   END
091000941129     C*
091100000000     C                   ENDSR
091200981015     c*-------------------------------------
091300981015     c     bucoiva       begsr
091400981015     c*-------------------------------------
091500981015     C     §ccjei        IFNE      *blank
091600981015     C                   MOVEL     §ccjei        WIVA3
091700981015     C                   move      *zeros        WIVA4
091800981015     C                   ELSE
091900981015     C                   move      aliiva        WIVA4
092000981015     C                   move      eseiva        WIVA3
092100981015     C                   END
092200981015     C** CONTO IVA
092300981015     C                   DO        5             B                 3 0
092400981015     C     AI(B)         COMP      WIVAds                                 05
092500981015     C   05              GOTO      EX1
092600981015     C     AI(B)         IFEQ      *blank
092700981015     C                   move      WIVAds        AI(B)
092800981015     C                   GOTO      EX1
092900981015     C                   END
093000981015     C                   END
093100981015     C     EX1           TAG
093200981015     c                   endsr
093300941129     C*****************************************************************
093400941129     C*    MEMORIZZA IMPORTI NELLA SCHIERA DEI CONTI
093500941129     C*****************************************************************
093600941129     C     CONMEM        BEGSR
093700941129     C                   SETOFF                                       08
093800981015     C                   MOVEL     dsconti       WCONTO           22
093900981015      *
094000941129     C                   Z-ADD     1             A
094100941129     C     WCONTO        LOOKUP    CCP(A)                                 05
094200941129     C  N05              Z-ADD     1             A
094300981015     C  N05*blank        LOOKUP    CCP(A)                                 05
094400981015     C                   movel     WCONTO        CCP(A)
094500000120     C                   ADD       WIMPOR        ICPW(A)
094600941129     C                   Z-ADD     WFCP          FCP(A)
094700941129     C                   ENDSR
094800941130     C********************************************************
094900941130     C** MEMORIZZA IVA
095000941130     C********************************************************
095100941130     C     CALIVA        BEGSR
095200941130     C                   Z-ADD     0             IV
095300941130     C     TASIVA        IFGT      0
095400941130     C                   DO        5             A
095500981015     C                   MOVEL     AI(A)         wivads
095600981020     C     wiva4         IFgt      *zeros
095700941130     C                   Z-ADD     TASIVA        IV(A)
095800941130     C                   END
095900941130     C                   END
096000941130     C                   END
096100941130     C** MEMORIZZO NEL PRIMO CONTO NON ESENTE
096200941130     C                   ENDSR
096300941129     C************************************************************
096400000000     C** CONTABILIZZAZIONE BOLLE
096500941129     C************************************************************
096600891106     C     CONTAB        BEGSR
096700990712     c                   clear                   tasiva
096800990712     c                   clear                   tasbol
096900941129     C                   EXSR      CON001
097000941129     C** IVA E CONTROPARTITE
097100981019     c                   z-add     0             riga              9 0
097200981020     c                   z-add     0             rigai             9 0
097300941130     C                   EXSR      CALIVA
097400941129     C*--------------------------------------------------------------
097500941129     C     TASBOL        IFGT      0
097600990712     C                   MOVEL     'VARIE   '    KCC
097700981015     C                   MOVE      '6'           KCC
097800941129     C                   Z-ADD     TASBOL        WIMPOR
097900941129     C                   EXSR      CONRIC
098000941129     C                   EXSR      CONMEM
098100941129     C                   END
098200941129     C** BOLLO
098300941129     C*--------------------------------------------------------------
098400000119     c* preparo i campi alla stampa
098500000119     c                   exsr      presta
098600000119     c* stampo la fattura
098700000119     c                   exsr      stafat
098800000120     c*
098900941129     C** TOTALE FATTURA
099000000120     C*---------------------------------------------------------
099100000120     c* gestione del controllo e conversione nella moneta di conto
099200000120     c                   EXSR      PREPPN
099300000120     c*
099400941129     C*---------------------------------------------------------
099500981016     c                   clear                   ndbhm000
099600981103     c                   add       1             wsubnum           9 0
099700981016     C** TIPO RECORD *A*
099800981016      * ctb generale ndbhm
099900981019     C                   MOVEL     §QCCA9        bhmcaustes
100000941129     C     KSCCOD        IFEQ      8888
100100981019     C                   MOVEL     §QCCAP        bhmcaustes
100200891117     C                   END
100300981016     c                   movel     knraz         bhmrifint
100400981103     c                   z-add     wsubnum       bhmsubnum
100500981020     c                   move      wsocieta      bhmsocieta
100600981016     c                   move      'CG'          bhmctb
100700981016     c                   move      'CG'          bhmclasse
100800981020     c                   movel     ksc           uni4              4
100900981020     c                   move      uni4          uni3              3
101000981020     c                   movel(p)  uni3          bhmunita                       uff.telecomunicaz
101100981016     c                   move      'D'           bhmtpregz
101200981020     c                   move      $kcc          bhmkcc
101300981020     c                   move      ksc           bhmksc
101400981015     c                   move      tasdft        bhmdtreg
101500981015     c                   z-add     tasnft        bhmnrreg
101600981015     c                   move      tasdft        bhmdtdoc
101700981015     C                   Z-ADD     TASNFT        bhmnrdoc
101800981015     C                   move      TASDFT        bhmdtpar
101900981015     C                   Z-ADD     TASNFT        bhmnrpar
102000981015     c                   add       1             riga
102100981015     c                   z-add     riga          bhmsubrig
102200981015     C                   movel     '2'           bhmserreg
102300981015     C                   move      TASFIV        bhmserreg
102400891107     C*
102500990712     C  N20              MOVEL     wrasoc        bhmdesbrev
102600981016     C   20              MOVEL     sogdes        bhmdesbrev
102700981016     c                   z-add     1             bhmcambio
102800000121     c                   if        xscdiv = tasdiv
102900000121     c                   movel     xscdiv        bhmdivisa
103000000121     C  n12              Z-ADD     FATTOT        bhmimportd
103100000121     C  n12              Z-ADD     FATTOT        bhmimporto
103200000121     C   12              Z-ADD     FATTOTD       bhmimportd
103300000121     C   12              Z-ADD     FATTOTD       bhmimporto
103400000121     c                   z-add     1             bhmcambio
103500000121     c                   else
103600000121     c                   movel     tasdiv        bhmdivisa
103700000121     C  n12              Z-ADD     FATTOT        bhmimportd
103800000121     C   12              Z-ADD     FATTOTD       bhmimportd
103900000121     C                   Z-ADD     FATTOTcnt     bhmimporto
104000000121     c                   z-add     cambio0202    bhmcambio
104100000121     c                   endif
104200981016     c                   move      'D'           bhmsegno
104300981019     c                   movel     §qccdp        bhmcondpag
104400981016     C                   write     ndbhm000
104500981130      *anagrafica partita
104600981130     c  n20              exsr      bh$
104700000000     C** TIPO RCORD  *C*
104800981016      * rek  IVA ndbhi
104900941129     C                   DO        5             A
105000981020     C                   CLEAR                   ndbhi000
105100981016     C     AI(A)         IFne      *blank
105200981016     c                   move      ai(a)         wivads
105300981016     c                   if        wiva3 <> *blank
105400981016     c                   move      wiva3         bhirifiva
105500981016     c                   else
105600981016     c                   move      wiva4         bhiali
105700981016     c                   end
105800981016     c                   movel     knraz         bhirifint
105900981103     c                   z-add     bhmsubnum     bhisubnum
106000981020     c                   movel     wsocieta      bhisocieta
106100981020     c                   add       1             rigai
106200981020     c                   z-add     rigai         bhisubrig
106300981016     c                   move      'A'           bhisegno
106400000121     C                   Z-ADD     IM(A)         bhiimpon
106500000121     C                   Z-ADD     IM(A)         bhiimpond
106600000121     C                   Z-ADD     IV(A)         bhiimportd
106700000121     C                   Z-ADD     IV(A)         bhiimporto
106800000121     c                   if        xscdiv <> tasdiv
106900000121     C                   Z-ADD     IMC(A)        bhiimpon
107000000121     C                   Z-ADD     IVC(A)        bhiimporto
107100000121     C                   ENDIF
107200981016     C                   move      TASFIV        bhilibro
107300981019     C                   move      '2'           bhitpreg
107400981016     C                   WRITE     ndbhi000
107500941129     C                   END
107600941129     C                   END
107700941129     C*
107800000000     C     TASBOL        IFGT      0
107900941129     C                   Z-ADD     1             A
108000990712     C                   MOVEL     'VARIE   '    KCC
108100981016     C                   MOVE      '6'           KCC
108200981016     C     KCC           LOOKUP    cC(A)                                  05
108300981016     C   05A             OCCUR     DScC
108400981016     C   05§ccjei        IFNE      *blank
108500981016     C                   move      §ccjei        bhirifiva
108600981020     C                   z-add     0             bhiali
108700981016     C                   Z-ADD     TASBOL        bhiimpon
108800981016     C                   Z-ADD     TASBOL        bhiimpond
108900981016     C                   Z-ADD     0             bhiimporto
109000981016     C                   Z-ADD     0             bhiimportd
109100000121     c                   if        xscdiv <> TASDIV
109200000121     C                   Z-ADD     FATBOLCNT     BHIIMPON
109300000121     C                   ENDIF
109400981016     C                   move      TASFIV        bhilibro
109500981203     c                   movel     knraz         bhirifint
109600981203     c                   z-add     bhmsubnum     bhisubnum
109700981203     c                   movel     wsocieta      bhisocieta
109800981203     c                   add       1             rigai
109900981203     c                   z-add     rigai         bhisubrig
110000981203     c                   move      'A'           bhisegno
110100990203     C                   move      '2'           bhitpreg
110200981016     C                   WRITE     ndbhi000
110300000000     C                   END
110400941129     C                   END
110500941129     C**  BOLLI
110600000000     C**
110700000000     C** TIPO RECORD  *D*
110800981016      * analitica ndbha
110900981016     C                   CLEAR                   ndbha000
111000941129     C                   DO        50            A
111100941129     C     ICP(A)        IFNE      *ZERO
111200981016     C                   MOVE      CCP(A)        dsconti
111300981016     c                   move      wkcc          bhmkcc
111400981016     c                   move      wksc          bhmksc
111500981016     c                   move      wvoc          bhmvoce
111600981016     C                   Z-ADD     ICP(A)        bhmimporto
111700981016     C                   Z-ADD     ICP(A)        bhmimportd
111800000121     c                   if        xscdiv <> TASDIV
111900000121     C                   Z-ADD     ICPC(A)       bhmimporto
112000000121     C                   ENDIF
112100981016     C                   Z-ADD     TASNFT        bhmnrpar
112200981016     C                   move      TASDFT        bhmdtpar
112300981016     c                   add       1             riga
112400981016     c                   z-add     riga          bhmsubrig
112500981016     c                   move      'A'           bhmsegno
112600981019      * cond pagamento
112700981019     c                   movel     §qccdp        bhmcondpag
112800981019      * ricavo data di competenza = data spedizione
112900981019      * a cambio anno = data fatturazione
113000981019     c                   move      tasdsp        bhmdtcoan
113100981019     c                   movel     tasdft        wannoft           4 0
113200981019     c                   if        wannoft <> tasaas
113300981019     c                   move      tasdft        bhmdtcoan
113400981019     c                   end
113500981016     C                   WRITE     ndbhm000
113600981016      *
113700981019     c                   move      bhmdtcoan     bhadtcoan
113800981016     c                   move      bhmrifint     bharifint
113900981016     c                   move      bhmsubnum     bhasubnum
114000981016     c                   move      bhmsubrig     bhasubrig
114100981016     c                   move      bhmvoce       bhavoce
114200981016     c                   move      'D1'          bhadimen
114300981020     c                   movel     bhmunita      bhaentit1
114400981020     c                   move      wsocieta      bhasocieta
114500981016     C                   Z-ADD     ICP(A)        bhaimporto
114600981016     C                   Z-ADD     ICP(A)        bhaimportd
114700000121     c                   if        xscdiv <> tasdiv
114800000121     C                   Z-ADD     ICPC(A)       bhaimporto
114900000121     C                   ENDIF
115000981016     c                   move      'A'           bhasegno
115100981016     c                   add       1             bhasubsubr
115200981016     c                   write     ndbha000
115300941129     C                   END
115400941129     C                   END
115500000121     c* se moneta diversa tra la divisa di fatturazione e la moneta di conto verifico se esistono
115600000121     c* delle differenze di cambio
115700000121     c                   if        xscdiv <> tasdiv
115800000121     c                   xfoot     icpc          wtoticpc
115900000121     c* sommo i  valori che mi devono dare il totale fattura
116000000121     c                   eval      wtotfat = wtoticpc + wtotivc
116100000121     c* calcolo la differenza dei totali fattura tra il convertito ed il calcolato
116200000121     c                   eval      diffe = fattotcnt - wtotfat
116300000121     C* SE ESISTE DIFFERENZA
116400000121     c*
116500000121     c                   if        diffe <> 0
116600000121     c*
116700000121     c* controllo se diffe è positivo o negativo
116800000121     c                   if        diffe < 0                                     * ARR. PASSIVO
116900000121     c* recupero conto + causale contabile
117000000121     C     2             OCCUR     DARR
117100000121     c                   move      'D'           bhmtpregz
117200000121     c                   mult      -1            diffe
117300000121     c                   else                                                    * ARR. ATTIVO
117400000121     c     1             OCCUR     DARR
117500000121     c                   move      'A'           bhmtpregz
117600000121     c                   endif
117700000121      * ctb generale ndbhm
117800000121     C                   movel     §arrcau       bhmcausrig
117900000121     c* Danilo ha detto di lasciare nft0 (ma io faccio di testa  mia)
118000000121     c                   move      §arrkcc       bhmkcc
118100000121     c                   move      §arrksc       bhmksc
118200000121     c                   move      §arrvoc       bhmvoce
118300000121     c                   add       1             riga
118400000121     c                   z-add     riga          bhmsubrig
118500000121     C*
118600000121     c                   movel     xscdiv        bhmdivisa
118700000121     c                   movel     xscdiv        bhmdivisam
118800000121     c                   z-add     1             bhmcambio
118900000121     c************       z-add     0             bhmcambiom anche questo x Danilo non è necessario
119000000121     C                   Z-ADD     diffe         bhmimporto
119100000121     C                   Z-ADD     diffe         bhmimportd
119200000121     c                   move      bhmtpregz     bhmsegno
119300000121     C                   write     ndbhm000
119400000121      *
119500000121     c                   clear                   ndbha000
119600000121     c                   move      bhmdtcoan     bhadtcoan
119700000121     c                   move      bhmrifint     bharifint
119800000121     c                   move      bhmsubnum     bhasubnum
119900000121     c                   move      bhmsubrig     bhasubrig
120000000121     c                   move      bhmvoce       bhavoce
120100000121     c                   move      'D1'          bhadimen
120200000121     c                   movel     §arrcdc       bhaentit1
120300000121     c                   move      wsocieta      bhasocieta
120400000121     C                   Z-ADD     diffe         bhaimporto
120500000121     c                   move      bhmtpregz     bhasegno
120600000121     c                   add       1             bhasubsubr
120700000121     c                   write     ndbha000
120800000121     c*
120900000121     c                   endif
121000000121     c*
121100000121     c                   endif
121200000121     c*-----------------------------------
121300000121     c* calcolo i totali
121400000121     c                   exsr      srtota
121500891107     C/SPACE
121600891107     C*-----------------------------------
121700110714    4C     KSCCOD        IFNE      9999
121800110714     C     KSCCOD        ANDNE     8888
121900941129     C                   EXSR      ASSIC
122000110714    4C                   ENDIF
122100011116     C** CREAZIONE TIASC00F ASSICURAZIONE SE OK
122200941129     C*-----------------------------------
122300941129     C                   MOVEL     '1'           TASFIC
122400991110      * aggiorno titas con excpt perchè il richiamo di TNSF20 potrebbe
122500991110      * aver sporcato dei campi con lo stesso nome del file TITAS
122600991110     C*!*                UPDATE    TITAS000
122700991110     c                   except    ugo
122800000120     C**   FLAG  INPUTAZIONE CONTABILE
122900941130     C                   ENDSR
123000000120     C***********************************************************************
123100000120     C** PREPARAZIONE IMPORTI PER REGISTRAZIONI PRIMA NOTA IN MONETA DI CONTO
123200000120     C***********************************************************************
123300000120     C     PREPPN        BEGSR                                                                     i
123400000120     c*
123500000120     c                   clear                   fattotcnt
123600000120     c                   clear                   imc
123700000120     c                   clear                   ivc
123800000120     c                   clear                   icp
123900000120     c                   clear                   icpc
124000000120     c                   clear                   fatbolcnt
124100000120     c                   clear                   wtotimc
124200000120     c                   clear                   wtotivc
124300000120     c* valorizzo i campi delle contropartite arrotondati con 2 decimali
124400000120     c* contropartite
124500000120     c                   DO        50            A
124600000120     C                   IF        icpw(a) > 0
124700000120     C                   Z-ADD(h)  icpw(a)       icp(a)
124800000120     c                   endif
124900000120     c                   enddo
125000000120     c* imponibile
125100000120     c                   DO        5             A
125200000120     C                   IF        sim(a) > 0 and not *in12
125300000120     C                   Z-ADD     sim(a)        im(a)
125400000120     c                   endif
125500000120     C                   IF        simd(a) > 0 and *in12
125600000120     c                   z-add     simd(a)       im(a)
125700000120     c                   endif
125800000120     c                   enddo
125900000120     c* iva
126000000120     c                   DO        5             A
126100000120     C                   IF        siv(a) > 0 and not *in12
126200000120     C                   Z-ADD     siv(a)        iv(a)
126300000120     c                   endif
126400000120     C                   IF        sivd(a) > 0 and *in12
126500000120     c                   z-add     sivd(a)       iv(a)
126600000120     c                   endif
126700000120     c                   enddo
126800000120     c
126900000120     C*
127000000120     C* SE MONETA DI CONTO DIVERSA DALLA MONETA DI FATTURAZIONE  CONVERSIONE DEGLI
127100000120     C* IMPORTI
127200000120     C                   IF        XSCDIV <> TASDIV
127300000120     C                   EVAL      DIV1 = tasdiv                                *
127400000120     C                   EVAL      DIV2 = XSCDIV                                *
127500000120     C*
127600000120     C* CONVERSIONE
127700000120     C* BOLLI
127800000120     c                   if        (not *in12 and fatbol > 0) or
127900000120     c                             (*in12 and fatbold > 0)
128000000120     C                   reset                   PJX00202DS
128100000120     C  n12              Z-ADD     fatbol        IMPORD0202
128200000120     C   12              Z-ADD     fatbold       IMPORD0202
128300000120     C                   EXSR      CNVEUR
128400000120     c                   z-add     IMPORM0202    fatbolcnt
128500000120     c                   endif
128600000120     C* IMPONIBILI IVA
128700000120     c*
128800000120     c                   DO        5             A
128900000120     C                   IF        im(a) > 0
129000000120     C                   reset                   PJX00202DS
129100000120     C                   Z-ADD     im(a)         IMPORD0202
129200000120     C                   EXSR      CNVEUR
129300000120     c                   z-add     IMPORM0202    imc(a)
129400000120     c                   endif
129500000120     c                   enddo
129600000120     C* IVA
129700000120     c                   DO        5             A
129800000120     C                   IF        iv(a) > 0
129900000120     c                   reset                   PJX00202DS
130000000120     C                   Z-ADD     iv(a)         IMPORD0202
130100000120     C                   EXSR      CNVEUR
130200000120     c                   z-add     IMPORM0202    ivc(a)
130300000120     c                   endif
130400000120     c                   enddo
130500000120     C* IMPORTI CONTROPARTITE
130600000120     c                   DO        50            A
130700000124     C                   IF        icp(a) > 0
130800000120     c                   reset                   PJX00202DS
130900000120     C                   Z-ADD     icp(a)        IMPORD0202
131000000120     C                   EXSR      CNVEUR
131100000120     c                   z-add     IMPORM0202    icpc(a)
131200000120     c                   endif
131300000120     c                   enddo
131400000120     c*
131500000120     c* calcolo il totale fattura da attribuire al cliente
131600000120     c* sommo imponibli iva
131700000120     c                   xfoot     imc           wtotimc
131800000120     c* sommo iva
131900000120     c                   xfoot     ivc           wtotivc
132000000120     c* sommo i  valori che mi devono dare il totale fattura
132100000120     c                   eval      fattotcnt = wtotimc + wtotivc + fatbolcnt
132200000120     c*
132300000120     c                   endif
132400000120     c*
132500000120     c                   endsr
132600000120     C*
132700000119     C******************************************************
132800000119     C**  STAMPA FATTURE
132900000119     C******************************************************
133000000119     C     STAFAT        BEGSR
133100000119     C*
133200000119     C                   Z-ADD     0             PAGE
133300000119     C**
133400000119     C                   EXCEPT    ST888
133500000119     C*
133600000119     C                   Z-ADD     4             A
133700000125     c* utilizzo la schiera delle varie da stampare
133800000119     C     A             DOWLE     20
133900000125    1C     SVW(A)        ANDNE     *BLANKS
134000000119     C                   MOVEA(P)  S(A)          S2
134100000120     C                   CLEAR                   I2
134200000120     C                   CLEAR                   I2D
134300000120     C  n12              MOVEA     I(A)          I2
134400000120     C   12              MOVEA     ID(A)         I2D
134500000119     C                   EXCEPT    ST8882
134600000119     C                   ADD       3             A
134700000119    1C                   ENDDO
134800110712      ** Verifico se presente tipo record "4" per i codici CUP-CIG
134900110712     c                   eval      §4trc = '4'
135000110712     c     Kta4          chain     tita430c
135100110712    1c                   If        %found(tita430c) and                         * codici Cup-CIG
135200110712     c                             ta4not <> *blanks
135300110712     c                   movel     ta4not        dta44
135400110712     c                   eval      StpCup = %trim(§ta4cup) + '-' +
135500110712     c                             %trim(§ta4cig)
135600110712    1c                   endif
135700110712      *
135800110712    1c                   if        StpCup <> *blanks
135900110712     C                   EXCEPT    STCUP
136000110712     c                   endif
136100110712
136200000119     C** DETTAGLIO
136300000119     C                   MOVEL     *BLANKS       LES
136400000119     C                   MOVEL     *BLANKS       LED
136500000121     C                   MOVEL     *BLANKS       LINS
136600000119     C                   MOVEA     S             LES
136700000119     C** CARICO SCHIERA SIGLE VARIE X STAMPA LEG
136800000119     C** RICERCA DELLE DESCRIZIONI
136900000119    1C     LES(1)        IFNE      *BLANKS
137000000119    2C                   DO        20            A
137100000119    3C     LES(A)        IFNE      *BLANKS
137200000119     C                   Z-ADD     1             A2                4 0
137300000119     C                   MOVEL     'VARIE   '    WKEY
137400000119     C**
137500000119     C                   MOVE      LES(A)        WKEY
137600000119     C     WKEY          LOOKUP    CC(A2)                                 05
137700000119     C   05A2            OCCUR     DSCC
137800000119     C   05              MOVEL     §CCDES        LED(A)
137900000121     C   05              MOVEL     '='           LINS(A)
138000000119    3C                   END
138100000119    2C                   ENDDO
138200000119     C** STAMPA LEGENDA SIGLE VARIE  10 SU OGNI RIGA
138300000119     C                   Z-ADD     1             A
138400000119     C     A             DOWLE     20
138500000119    2C     LES(A)        ANDNE     *BLANKS
138600000119    3C     A             IFEQ      1
138700000119     C                   EXCEPT    DCOD84
138800000119     C                   ELSE
138900000119    4C     A             IFEQ      9
139000000119     C                   EXCEPT    DCOD85
139100000119     C                   ELSE
139200000119     C                   EXCEPT    DCOD86
139300000119    4C                   END
139400000119    3C                   END
139500000119     C*
139600000119     C                   ADD       8             A
139700000119    2C                   ENDDO
139800000119    1C                   ENDIF
139900050623     C** STAMPO LE RIGHE RELATIVE AL BOLLO VIRTUALE SE ESISTE
140000050623     C                   IF        TASBOL > 0
140100060719      * verifico la data fattura se antecedente il 1/8/2006 uso la vecchia dicitura
140200060719     C                   If        TASDFT < 20060801
140300050623     C                   EVAL      STAESE = CESENZ
140400060719     C                   ELSE
140500060719     C                   EVAL      STAESE = CESENZ_NEW
140600060719     C                   ENDIF
140700050623     C                   EXCEPT    BOLVIR
140800050623     C                   ELSE
140900050623     C                   CLEAR                   STAESE
141000050623     C                   ENDIF
141100000119     C*
141200000119     C** STAMPO LE RIGHE RELATIVE ALLA RISERVATEZZA
141300000119     C                   EXCEPT    RCOD84
141400000119     C** STAMPO LA RIGA: IVA E ESIGIBILITA' IMMEDIATA
141500000119     C                   SETOFF                                       99
141600000119     C   99
141700000119     CANN99              EXCEPT    ICOD84
141800000119     C*
141900000119     C* PREPARA LA STAMPA DEI TOTALI
142000000119     C                   EXCEPT    ST88TT
142100000119     C*
142200000119     C** ESENZIONE IVA
142300130116     C                   MOVEL     *BLANKS       DESCEI           20
142400000119     C                   SETOFF                                       11
142500000119     C     TASFEI        IFNE      *BLANKS
142600000119     C                   SETON                                        11
142700000119     C                   Z-ADD     1             K
142800000119     C     TASFEI        LOOKUP    CEI(K)                                 05
142900130122     C** 05              MOVEL     DEI(K)        DESCEI
143000130122     C   05              MOVEL     DES(K)        DESCEI
143100000119     C                   END
143200000119     C*
143300000119     C                   EXCEPT    ST888T
143400000119     C                   EXCEPT    ST888G
143500000119     C                   ENDSR
143600000119     C******************************************************
143700000119     C* PREPARA I CAMPI PER LA STAMPA DETTAGLIO
143800000119     C******************************************************
143900000119     C     PRESTA        BEGSR
144000000119     C*
144100130308      * aggancio tita4 tipo record "Q" per un eventuale recupero del codice fiscale
144200130308     c                   clear                   dta4q
144300130308     c                   eval      §4trc = 'Q'
144400130308     c     Kta4          chain     Tita430c
144500130308     c                   If        %found(Tita430c)
144600130308     c                   eval      dta4q = ta4not
144700130308     c                   endif
144800130308      *
144900000208     c                   move      ksc           stksc
145000000208     c*
145100000120     C                   IF        tipbol = 'A'
145200000119     C** PARTITA IVA
145300000119     C                   MOVEL     TASCPD        FATIVA           16
145400130308      ** codice fiscale
145500130308     C                   MOVEL     §TA4QCFD      FATCDF           16
145600130308
145700000120     C                   IF        WCCM   = 8888
145800000120     C  N06              MOVEL     TAARSC        RSC13            13
145900000119     C                   ELSE
146000000119     C* Se in TITAS il mittente è codificato prendo i dati da CNIND
146100000119     C                   CLEAR                   DSBS69
146200000119     C                   MOVEL     KNSIF         I69SIF
146300000119     C                   MOVEL     KCI           I69KCC
146400000119     C                   Z-ADD     TASCCM        I69KAC
146500000119     C                   CALL      'TIBS69R'
146600000119     C                   PARM                    DSBS69
146700000119     C                   PARM                    DSACO
146800000119     C                   PARM                    DSIND
146900000119     C                   PARM                    DSCLP
147000000119     C                   PARM                    DSCLS
147100000119     C     O69ERR        IFNE      ' '
147200000119     C                   CLEAR                   DSACO
147300000119     C                   ENDIF
147400000121     C                   MOVEL     CACORAG       RSC13            13
147500000119     C                   END
147600000119     C                   ELSE
147700000120    3C     KSCCOD        IFEQ      8888
147800000120     C     *IN06         ANDEQ     *OFF
147900000119     C** PARTITA IVA
148000000119     C                   MOVEL     TAACPI        FATIVA           16
148100130308      ** codice fiscale
148200130308     C                   MOVEL     §TA4QCFM      FATCDF
148300130308
148400000119     C                   END
148500000119     C                   MOVEL     TASRSD        RSC13            13
148600000119     C                   END
148700050623      * VERIFICO SE IN FATIVA C'È LA SCRITTA DI PRIVATO E SE E' COSI PULISCO IL CAMPO
148800050623     C                   IF        %SCAN('PRIVATO':FATIVA)>0
148900050623     C                   CLEAR                   FATIVA
149000050623     C                   ENDIF
149100130308      * se codice fiscale a blank lo imposto uguale alla partita iva
149200130308     c                   if        fatcdf = *blanks
149300130308     c                   eval      fatcdf = fativa
149400130308     c                   endif
149500000119     C*
149600000119     C** DATA SPEDIZIONE
149700000119     C                   MOVE      TASMGS        GDSP              2 0
149800000119     C                   MOVEL     TASMGS        MDSP              2 0
149900000119     C                   MOVEL     *BLANKS       WTSP
150000000119     C**
150100000119     C** TIPO TARIFFA A VALORE O QUANTITA'
150200000119     C     TASCTR        COMP      400                                09  09
150300000119     C** TIPI SERVIZIO DA STAMPARE
150400000119     C     TASTSP        LOOKUP    TS                                     05
150500000119     C   05              MOVEL     TASTSP        WTSP              1
150600001215     c** Se sto elaborando una bolla della ditta SDI e il tipo servizio è C e la
150700001215     c** bolla ha data spedizione minore al 01-01-2001 e non ho trovato il
150800001215     c** il tipo servizio in tabella stampo lo stesso il tipo servizio in quanto
150900001215     c** prima della data sopra indicata il tipo servizio "C" era come E di espresso
151000001215     c*
151100010321     c                   if        not *in05 and kscdit = 'SDI' and
151200001221     c                             TAStsp = 'C' and tasdsp < 20010101
151300001215     c                   movel     TAStsp        WTSP
151400001215     c                   endif
151500001215     c*
151600000119     C**  MEMORIZZO PRIMI 3 CARATTERI DEL CODICE CLIENTE
151700000119     C                   MOVEL     TASKSC        WKSC3             3 0
151800000119     C**
151900000119     C** SIGLA AUTOM.FILIALE DI PARTENZA
152000000119     C     TASLNP        IFNE      WFIL
152100000119     C     TASLNP        CHAIN     AZORG                              05
152200000119     C                   MOVEL     TASLNP        WFIL
152300000119     C   05              CLEAR                   ORGPRO
152400000119     C                   ENDIF
152500000119     C*
152600000119     C                   MOVEL     ORGPRO        CIT2              2
152700000119     C**
152800000119     C     WKSC3         IFNE      WFIL
152900000119     C     WKSC3         CHAIN     AZORG                              05
153000000119     C                   MOVEL     WKSC3         WFIL              3 0
153100000119     C                   ENDIF
153200000119     C** PRENDO IL TERMINAL DI PARTENZA
153300060904      * chiamo Fnlv55 con tipo terminal "O" = Ottico
153400000119     C     WKSC3         IFNE      SAVTER
153500000119     C                   CLEAR                   DSLV55
153600060904     C                   MOVEL     'O'           D55TPT
153700060904     C****               MOVEL     'P'           D55TPT
153800000119     C                   MOVEL     WKSC3         D55LIN
153900000121     C                   MOVEL     UDATER        D55DRF
154000000119     C                   CALL      'FNLV55R'
154100000119     C                   PARM                    DSLV55
154200000119     C                   MOVEL     WKSC3         SAVTER            3 0
154300000119     C                   ENDIF
154400000119     C*
154500000119     C                   MOVEL     ORGDES        PFIL             20            DESCR.FILIALE
154600000119     C                   MOVEL     ORGDES        PDFI             20            DESCR.FILIALE
154700000119     C                   MOVEL     ORGIND        PVIA             20            VIA
154800000119     C                   MOVEL     ORGCPF        PCAP              5            CAP
154900000119     C                   MOVEL     ORGLOC        PLOC             20            LOCALITA
155000000119     C                   MOVEL     ORGTEL        PTEL             13            TELEFONO
155100000119     C                   MOVEL     ORGPRO        PPRV              2            PROV.
155200000119     C                   MOVEL     ORGFAX        PFAX             13            FAX
155300000119     C                   MOVEL     D55TFP        ASBOL             3 0
155400000119     C*
155500000119     C** STAMPA CAP
155600000120     C     tipbol        IFEQ      'F'
155700000119     C                   MOVEL     TASCAD        WCAP              5
155800000119     C                   ELSE
155900000119     C                   MOVEL     TASCAM        WCAP
156000000119     C                   END
156100000119     C** PROVINCIA CODICE TASSAZIONE DESTINO
156200000119     C                   Z-ADD     1             A
156300000119     C     TASCTS        LOOKUP    CTS(A)                                 05
156400000119     C   05A             OCCUR     DSCT
156500000119     C   05              MOVEL     §CTPRV        CIT3              2
156600000119     c* verifico la divisa della fattura
156700000119     C                   eval      divisa =  tasdiv
156800000119     c* controllo se divisa con i decimali
156900000119     c                   exsr      ctrdiv
157000000119     c*
157100000119     C                   if        §CVFDC = 'S'                                 *DIVISA CON DECIMALI
157200000119     c                   seton                                        12
157300000119     c                   else
157400000119     c                   setoff                                       12
157500000119     c                   endif
157600000119     c
157700000119     c* divisa senza decimali
157800000119     c                   if        not *in12
157900000120     c                   z-add     taspor        prtpor
158000000119     C                   XFOOT     IM            FATTOT
158100000119     C                   ADD       TASIVA        FATTOT
158200000119     C                   ADD       TASBOL        FATTOT
158300000119     C                   Z-ADD     TASBOL        FATBOL
158400000119     C                   Z-ADD     TASPOR        PRTTPI
158500000119     C                   Z-ADD     TOTVAR        VARIE
158600000119     c                   else
158700000120     c                   z-add     taspor        prtpord
158800000119     C                   XFOOT(h)  IM            FATTOTD
158900000119     C                   ADD(h)    TASIVA        FATTOTD
159000000119     C                   ADD(h)    TASBOL        FATTOTD
159100000119     C                   Z-ADD(h)  TASBOL        FATBOLD
159200000119     C                   Z-ADD(h)  TASPOR        PRTTPID
159300000119     C                   Z-ADD(h)  TOTVAR        VARIED
159400000119     c                   endif
159500000119     c
159600000119     C* PREPARO I CAMPI PER LA STAMPA
159700000119     C                   IF        NOT *IN12
159800000119     C                   DO        5             A
159900000119     C                   Z-ADD     IM(A)         SIM(A)
160000000119     C                   Z-ADD     IV(A)         SIV(A)
160100000119     C                   END
160200000119     C*
160300000119     C                   ELSE
160400000119     C*
160500000119     C                   DO        5             A
160600000119     C                   Z-ADD(h)  IM(A)         SIMD(A)
160700000119     C                   Z-ADD(h)  IV(A)         SIVD(A)
160800000119     C                   END
160900000119     C*
161000000119     C                   END
161100000120     C** CARICAMENTO VARIE X STAMPA
161200000120     C                   Z-ADD     0             I
161300000120     C                   Z-ADD     0             ID
161400000120     C                   MOVEL     *BLANKS       S
161500000125     C                   MOVEA     SVW           S
161600000125     C  n12              z-add     VAW           I
161700000125     C   12              z-add     VAW           ID
161800000119     C*
161900000119     C*
162000000119     C** RIF.MITTENTE NUMERICO
162100000119     C                   Z-ADD     TASRMN        RMN9              9 0
162200000119     C*
162300000119     C* VOLUME
162400000119     C     TASVLF        COMP      0                                  4242
162500000120     c* data fattura
162600000120     c     *iso          move      tasdft        datadmy
162700000121     c                   move      datadmy       dftgma
162800000121     c* recupero aliquota iva da stampare
162900000121     c                   if        tasdft <= §qtdsa
163000000121     C                   Z-ADD     §QTALO        STAALI
163100000121     C                   ELSE
163200000121     C                   Z-ADD     §QTALI        STAALI            3 1
163300000121     C                   ENDIF
163400000120     c*
163500000119     C                   ENDSR
163600891107     C/SPACE
163700000000     C**---------------------------------
163800011116     C** CREAZIONE TIASC00F
163900000000     C**---------------------------------
164000891107     C     ASSIC         BEGSR
164100980527     C                   MOVEL     *BLANKS       TASTLA
164200980527     C                   MOVEL     *BLANKS       TASERR
164300980527     C                   MOVEL     *BLANKS       TASMSG
164400991110     C                   Z-ADD     0             dsTASIMV
164500980527     C                   CLEAR                   TASIAL
164600980527     C                   CLEAR                   TASK88
164601160531      *
164602160531     c                   z-add     tasncl        tasnclb
164700980527     C**
164800980525     C                   Z-ADD     1             A
164900980525     C     TASTBL        LOOKUP    TB(A)                                  05
165000980525     C   05A             OCCUR     DSTB
165100980525     C   05§TBFAS        CABEQ     '0'           ENDASS
165200980525     C* CHIAMO PGM DI TASSAZIONE PER SAPERE SE E' ASSICURATO O MENO
165300980525     C*
165400980525     C* FLAG SI NO DDT
165500980525     C     TASLL1        IFEQ      'C'
165600980525     C     TASLL1        OREQ      'S'
165700980525     C                   MOVEL     'S'           TASDDT
165800980525     C                   ELSE
165900980525     C                   CLEAR                   TASDDT
166000980525     C                   ENDIF
166100980527     C* DATA CARICAMENTO TARIFFA DI CARTELLO
166200980527     C                   Z-ADD     TASDFT        TASDCT
166300011129     C* DATA TASSAZIONE FATTURA
166400030204     C**!!!              CLEAR                   DTAS01
166500011129     C                   CLEAR                   DSTASFLO
166600030204     c                   Clear                   dstaspkc
166700030204     c                   Clear                   dstasncp
166800060926     c*******            movel     tasfbr        dstasflo
166900060926     c                   clear                   dtas01
167000060926     c                   movel     tasfbr        §asfbr
167100060926     c                   movel     tascca        §ascca
167200060926     c                   eval      dstasflo = dtas01
167300020903     C****               MOVE      TASDFT        §ASDAT
167400020903     c* passo il peso vdl solo se usato anche in filiale
167500020903     c                   if        tasfpf='V'
167600030204     c**!!!              eval      §aspkc=taspkc
167700030204     c**!!!              eval      §asncp=tasncp
167800030204     c                   eval      dstaspkc=taspkc
167900030204     c                   eval      dstasncp=tasncp
168000020903     c                   endif
168100030204      * Bancali
168200030205     c                   If        %Subst(TasGva:1:1) = 'B'
168300030204     c                   Clear                   dAr5Ban
168400030911     c                   eval      kAr5Trd  = 'BAN'
168500040531     c     kFiar5        Chain     Fiar531c
168600040531If  1c                   If        %Found(Fiar531c)
168700030204     c                   Movel     Ar5Uni        dAr5Ban
168800030204    1c                   EndIf
168900030204     c                   Eval      TasBan = §Ar5Nba + §Ar5Nb2
169000030205     c                   Else
169100030205     c                   Clear                   TasBan
169200030205     c                   EndIf
169300030911      * Colli Originali
169400030911    1c                   If        %Subst(TasGva:1:1) = 'O'
169500030911     c                   Clear                   dAr5Bnb
169600030911
169700030911     c                   eval      kAr5Trd  = 'BNB'
169800040531     c     kFiar5        Chain     Fiar531c
169900040531If  1c                   If        %Found(Fiar531c)
170000030911     c                   Movel     Ar5Uni        dAr5Bnb
170100030911      *
170200030911     c                   Eval      TasNcl = §Ar5bcor
170300030911    1c                   EndIf
170400030911    1c                   EndIf
170500030204
170600980525     C* VARIA DA CALCOLARE
170700980525     C                   MOVEL     'R'           TASSVA
170800980527     C* SOLO CONTROLLO
170900980527     C                   MOVEL     'S'           PARCA
171000980527     C                   MOVEL     PARAMT        KPJBU
171100980525     C** DATA FATT X CARIC.DATI TARIFFA CARTELLO 1° VOLTA
171200980525     C                   CALL      'TNSF20R'
171300980525     C                   PARM                    KPJBA
171400991109     C                   PARM                    DTAS
171500980525     C                   PARM                    DSVAR
171600980525     C* SE NON C'E' ERRORE
171700980527    1C     TASERR        IFEQ      *BLANKS
171800990714     c* non valorizzo campo porto che ricevo da tnsf20r perchè il pgm è
171900990714     c* stato richiamato per il solo controllo della varia "R"
172000990714     c* ed essendo la bolla già tassata sarebbe un errore se il porto
172100990714     c* venisse modificato
172200980525     C                   SETOFF                                       87
172300980525     C** CLIENTE VARIO 8888 --> SOLO SE HO L'IMPORTO IN BOLLA
172400980525    2C     TASIAL        IFGT      0
172500980525     C**
172600980525    3C     TASK88        IFEQ      'S'
172700980525     C     TASIAS        ANDGT     0
172800980525     C* SOLO SE CLIENTE CODIFICATO CON MANDATO ASSICURATIVO 8888
172900980525    4C     KSCCOD        IFNE      9999
173000980525     C     KSCCOD        ANDNE     8888
173100980525     C                   SETON                                        87
173200980525    4C                   ENDIF
173300980525     C**
173400980525     C                   EXSR      MEMASS
173500980525    3C                   ENDIF
173600980525     C**
173700980525     C** CLIENTE CODIFICATO
173800980525    3C     TASK88        IFEQ      'N'
173900980525     C** MEMORIZZATA BOLLA IN TNASC00F
174000980525     C                   EXSR      MEMASS
174100980525    3C                   ENDIF
174200980525    2C                   ENDIF
174300980525    1C                   ENDIF
174400980525     C**
174500980525     C     ENDASS        ENDSR
174600891107     C**-----------------------------------------------------------
174700950331     C**  MEORIZZO SU TNASC00F LE BOLLE CON ASS. PER ASS. X CONTO -
174800891107     C**-----------------------------------------------------------
174900891107     C     MEMASS        BEGSR
175000050623     C***!!!***          Z-ADD     TASLNP        ASCLNP
175100050623     C***!!!***          Z-ADD     TASNRS        ASCNRS
175200050623     C***!!!***          Z-ADD     TASNSP        ASCNSP
175300050623     C***!!!***          MOVEL     TASTBL        ASCTBL
175400050623     C***!!!***          Z-ADD     TASKSC        ASCKSC
175500050623     C***!!!***          MOVEL     TASDSP        ANNODS            4 0
175600050623     C***!!!***          MOVE      TASDSP        MGS               4 0
175700050623     C***!!!***          MOVE      ANNODS        ASCAAS
175800050623     C***!!!***          Z-ADD     MGS           ASCMGS
175900050623     C***!!!***          Z-ADD     TASNFT        ASCNFT
176000050623     C***!!!***          Z-ADD     TASDFT        ASCDFT
176100020903     c* se devo usare il vdl lo imposto anche nel file tiasc
176200030204     c**!!!              if        §asspc='S'
176300050623     c***!!!***          if        Tasspc='S'
176400030204     c**!!!              z-add     §aspkc        ascpkg
176500050623     c***!!!***          z-add     Taspkc        ascpkg
176600050623     c***!!!***          else
176700050623     C***!!!***          Z-ADD     TASPKF        ASCPKG
176800050623     c***!!!***          endif
176900050623     C***!!!***          Z-ADD     0             ASCDST
177000050623     C***!!!***          Z-ADD     0             ASCVRR
177100050623     C***!!!***          CLEAR                   ASCVVR
177200011116      * valorizzo la divisa dell'assicurazione uguale alla divisa del cliente
177300011116     C****               MOVEL     *BLANKS       ASCVAS
177400050623     C***!!!***          MOVEL     TASDIV        ASCVAS
177500050623     C***!!!***          Z-ADD     TASIAL        ASCIAS
177600050623     C***!!!***          Z-ADD     1             A
177700980525     C* IMPOSTO LA VARIA "R"
177800050623     C***!!§$2VRR        LOOKUP    SV(A)                                  05
177900011116     C**** non è più necessario convertire in ITL in quanto passo la divisa della varia nel nuovo
178000011116     C**** file
178100011116     c**** se divisa fattura diversa da ITL converto la varia in lire
178200000120     c*
178300011116     c****               if        tasdiv <> 'ITL'
178400000120     C* CONVERTO IL LIRE ITALIANE
178500011116     C****               EVAL      DIV1 = TASDIV
178600011116     C****               EVAL      DIV2 = 'ITL'
178700011116     C****               Z-ADD     VA(A)         IMPOCNV
178800011116     C****               EXSR      SRYEUR
178900000120     C*
179000011116     C***05              Z-ADD     IMPOCNV       ASCVRR
179100011116     C****               ELSE
179200050623     C***05              Z-ADD     VA(A)         ASCVRR
179300011116      * valorizzo la divisa della varia "R"
179400050623     C***05              move      tasdiv        ascvvr
179500011116     C*****              ENDIF
179600980525     C**
179700980525     C*CLIENTE NON CODIFICATO --> 8888 O 9999
179800050623     C**   *IN20         IFEQ      *OFF
179900050623     c**   tipbol        ifeq      'A'
180000050623     c**   ksccod        andeq     9999
180100050623     C***                MOVEL     TASRSD        ASCRSD
180200050623     C***                MOVEL     TASIND        ASCIND
180300050623     C***                MOVEL     TASCAD        ASCCAP
180400050623     C***                MOVEL     TASLOD        ASCLOC
180500050623     c***                else
180600050623     c***  tipbol        ifeq      'F'
180700050623     c***  ksccod        andeq     8888
180800050623     C***                MOVEL     TAARSC        ASCRSD
180900050623     C***                MOVEL     TAAIND        ASCIND
181000050623     C***                MOVEL     TAACAP        ASCCAP
181100050623     C***                MOVEL     TAALOC        ASCLOC
181200050623     c***                end
181300050623     c***                end
181400050623     C***                ELSE
181500980525     C** CLIENTE CODIFICATO
181600050623     C***                MOVEL     *BLANKS       ASCRSD
181700050623     C***                MOVEL     *BLANKS       ASCIND
181800050623     C***                MOVEL     *BLANKS       ASCCAP
181900050623     C***                MOVEL     *BLANKS       ASCLOC
182000980525     C**
182100980525     C** 87 CODIFICATO SENZA MANDATO IN TARIFFA  CON VALORE ESPOSTO
182200050623     C** 87              DO
182300050623     C**                 MOVE      8888          ASCKSC
182400050623     C**                 MOVEL     RSD           ASCRSD
182500050623     C**                 MOVEL     IND           ASCIND
182600050623     C****               MOVEL     CAP           ASCCAP
182700050623     C***                MOVEL     LOC           ASCLOC
182800050623     C***                END
182900050623     C***                ENDIF
183000980525     C**
183100050623     C*****              WRITE     TIASC000
183200980525     C**
183300950331     C     TASIAS        IFEQ      0
183400050623     C                   Z-ADD     tasial        TASIAS
183500050623     C                   MOVE      tasdiv        TASVAS
183600011119     c                   move      'F'           TASFCM
183700950331     C                   END
183800000000     C                   ENDSR
183900891107     C/SPACE
184000000000     C**----------------------------------------
184100000000     C** AZZERAMENTO CAMPI CALCOLO
184200000000     C**----------------------------------------
184300891107     C     BLANK         BEGSR
184400000000     C                   Z-ADD     0             FATTOT
184500000119     C                   Z-ADD     0             FATTOTD
184600000121     C                   Z-ADD     0             FATTOTcnt
184700941129     C                   Z-ADD     0             IM
184800000121     C                   Z-ADD     0             IMc
184900941129     C                   Z-ADD     0             IV
185000000121     C                   Z-ADD     0             IVc
185100000119     C                   Z-ADD     0             sIM
185200000119     C                   Z-ADD     0             sIV
185300000119     C                   Z-ADD     0             sIMd
185400000120     C                   Z-ADD     0             SIVD
185500000120     C                   Z-ADD     0             prtpor
185600000120     C                   Z-ADD     0             prtpord
185700000119     C                   Z-ADD     0             prttpi
185800000119     C                   Z-ADD     0             prttpid
185900000119     C                   Z-ADD     0             varie
186000000119     C                   Z-ADD     0             varied
186100000119     C                   Z-ADD     0             fatbol
186200000119     C                   Z-ADD     0             fatbold
186300000121     C                   Z-ADD     0             fatbolcnt
186400000119     c                   Z-ADD     0             totvar
186500981015     C                   clear                   AI
186600981015     C                   clear                   CCP
186700941129     C                   Z-ADD     0             ICP
186800000121     C                   clear                   ICPW
186900000121     C                   clear                   ICPC
187000941129     C                   Z-ADD     0             FCP
187100000125     c                   CLEAR                   SVW
187200000125     C                   CLEAR                   VAW
187300000120     C                   CLEAR                   S
187400000120     C                   CLEAR                   I
187500000120     C                   CLEAR                   ID
187600000120     C                   CLEAR                   I2
187700000120     C                   CLEAR                   I2D
187800000121     C                   CLEAR                   wtotfat
187900000121     C                   CLEAR                   wtotimc
188000000121     C                   CLEAR                   wtotivc
188100000121     C                   CLEAR                   wtoticpc
188200000121     C                   CLEAR                   diffe
188300941129     C                   ENDSR
188400941129     C*****************************************************************
188500941129     C*    RICERCA CONTO RELATIVO AL TIPO IMPORTO E SPEDIZIONE        *
188600941129     C*****************************************************************
188700981015     C     cONRIC        BEGSR
188800941129     C                   SETOFF                                       08
188900941129     C                   Z-ADD     1             A
189000981015     C     KCC           LOOKUP    cC(A)                                  05
189100941129     C  N05              DO
189200941129     C                   MOVEL     E(8)          TASMSG
189300941129     C                   SETON                                        08
189400941129     C                   EXSR      ERRORE
189500941129     C                   SETOFF                                       08
189600941129     C                   Z-ADD     1             A
189700941129     C                   END
189800941129     C** TABELLA CC NON TROVATA
189900981015     C     A             OCCUR     DScC
190000981015     C                   MOVE      §cCFLP        WFCP              1 0          FLAG COMB. FIL.
190100990714     c     taslnp        lookup    est                                    05
190200990714     c  N05taslna        lookup    est                                    05
190300990714     C     *in05         ifeq      *on
190400981015     C                   move      §ccjce        WKCC
190500981015     C                   move      §ccjse        WKSC
190600981015     C                   move      §ccjve        Wvoc
190700941129     C                   ELSE
190800981015     C                   move      §ccjci        WKCC
190900981015     C                   move      §ccjsi        WKSC
191000981015     C                   move      §ccjvi        Wvoc
191100941129     C                   END
191200941129     C*
191300941129     C                   ENDSR
191400941129     C****************************************************
191500941129     C** CARICAMENTO DATI VARI CONTABILITA'
191600941129     C****************************************************
191700941129     C     CARQC         BEGSR
191800941129     C                   CLEAR                   DSQC1
191900941129     C                   CLEAR                   DSQC2
192000941129     C                   MOVEL     'QC'          COD
192100941129     C                   MOVEL     *BLANKS       KEY
192200941129     C                   MOVEL     '2'           KEY
192300941129     C     KTABC         CHAIN     TABEL                              07
192400981015     C   07              GOTO      FINE
192500941129     C                   MOVEL     TBLUNI        DSQC2
192600941129     C                   MOVEL     *BLANKS       KEY
192700941129     C                   MOVEL     '1'           KEY
192800941129     C     KTABC         CHAIN     TABEL                              07
192900981015     C   07              GOTO      FINE
193000941129     C                   MOVEL     TBLUNI        DSQC1
193100971001     C**
193200941129     C                   ENDSR
193300981016     C****************************************************
193400981016     C** CARICAMENTO DA tabella QT
193500981016     C****************************************************
193600981016     C     CARQt         BEGSR
193700981016     C                   CLEAR                   DSQt1
193800981016     C                   MOVEL     'QT'          COD
193900991109      *
194000981016     C                   MOVEL     *BLANKS       KEY
194100981016     C                   MOVEL     '1'           KEY
194200981016     C     KTABC         CHAIN     TABEL                              07
194300981016     C   07              GOTO      FINE
194400981016     C                   MOVEL     TBLUNI        DSQT1
194500981016     C**
194600981016     C                   ENDSR
194700000121     C****************************************************
194800000121     C** CARICAMENTO TABELLA ARR
194900000121     C****************************************************
195000000121     C     CARARR        BEGSR
195100000121     C*  CARICO DS ARROTONDAMENTI ATTIVI
195200000121     C                   CLEAR                   DSBS02
195300000121     C                   CLEAR                   DARR
195400000121      *
195500000121     C                   MOVEL     'L'           T02TLA
195600000121     C                   MOVEL     'C'           T02MOD
195700000121     C                   MOVEL     KNSIF         T02SIF
195800000121     C                   MOVEL     'ARR'         T02COD
195900000121     C                   MOVEL     'ATTIVO '     T02KE1
196000000121      *
196100000121     C                   CALL      'TIBS02R'
196200000121     C                   PARM                    KPJBA
196300000121     C                   PARM                    DSBS02
196400000121      *
196500000121     C     T02ERR        IFEQ      *BLANKS
196600000121     C     1             OCCUR     DARR
196700000121     C                   MOVEL     T02UNI        DARR
196800000121     C                   ENDIF
196900000121     C*  CARICO DS ARROTONDAMENTI ATTIVI
197000000121     C                   CLEAR                   DSBS02
197100000121      *
197200000121     C                   MOVEL     'L'           T02TLA
197300000121     C                   MOVEL     'C'           T02MOD
197400000121     C                   MOVEL     KNSIF         T02SIF
197500000121     C                   MOVEL     'ARR'         T02COD
197600000121     C                   MOVEL     'PASSIVO'     T02KE1
197700000121      *
197800000121     C                   CALL      'TIBS02R'
197900000121     C                   PARM                    KPJBA
198000000121     C                   PARM                    DSBS02
198100000121      *
198200000121     C     T02ERR        IFEQ      *BLANKS
198300000121     C     2             OCCUR     DARR
198400000121     C                   MOVEL     T02UNI        DARR
198500000121     C                   ENDIF
198600000121      *
198700000121     C                   ENDSR
198800000119     C****************************************************
198900000119     C** CONTROLLO DIVISA RICHIESTA
199000000119     C****************************************************
199100000119     C     CTRDIV        BEGSR
199200000119     C                   CLEAR                   DSCV
199300000119     C*
199400000119     C                   MOVEL     'CV'          COD
199500000119     C                   MOVEL     *BLANKS       KEY
199600000124     C                   MOVEL     DIVISA        KEY
199700000119     C     KTABC         CHAIN     TABEL                              07
199800000119     C   07              SETON                                        H7
199900000119     C   H7              GOTO      FINE
200000000119     C                   MOVEL     TBLUNI        DSCV
200100000119     C*
200200000119     C                   ENDSR
200300981016      *----------------------------------------------------
200400981016     C     bh$           BEGSR
200500981016     C*----------------------------------------------------
200600061109
200700990517     c                   if        bhysubrig < 1
200800990517     c                   clear                   ndbhY000
200900981016     c                   end
201000990517     c                   move      knraz         bhYrifint
201100990517     c                   z-add     bhmsubnum     bhysubnum
201200990517     c                   z-add     1             bhysubrig
201300990517     c                   move      wsocieta      bhysocieta
201400990713     c* assegnato
201500990713     c                   if        tipbol = 'A' and
201600990713     c                             ksccod = 9999
201700990517     c                   movel(p)  tasrsd        bhydescr
201800990517     c                   movel     tasind        bhyindiriz
201900990713     c                   movel     tascad        bhycap
202000990517     c                   movel     taslod        bhylocalit
202100990713     c                   movel     tasprd        bhyprov
202200990517     c                   movel     'IT  '        bhystato
202300990713     c     tasnzd        ifne      *blank
202400990713     c     tasnzd        andne     'I  '
202500990713     c     tasnzd        andne     'IT '
202600990713     c                   movel     tasnzd        bhystato
202700981118     c                   endif
202800061109
202900990713     c                   movel     tascpd        bhypartiva
203000061109      * codice fiscale
203100061109     c                   movel     §ta4qcfd      bhycdfisc
203200061109
203300990713     c                   else
203400990713     c* franco
203500990713     c                   if        tipbol = 'F' and
203600990713     c                             ksccod = 8888
203700990713     c                   movel(p)  taarsc        bhydescr
203800990713     c                   movel     taaind        bhyindiriz
203900990713     c                   movel     taacap        bhycap
204000990713     c                   movel     taaloc        bhylocalit
204100990713     c                   movel     taaprv        bhyprov
204200990713     c                   movel     'IT  '        bhystato
204300990713     c     taanaz        ifne      *blank
204400990713     c     taanaz        andne     'I  '
204500990713     c     taanaz        andne     'IT '
204600990713     c                   movel     taanaz        bhystato
204700990713     c                   endif
204800990713     c                   movel     taacpi        bhypartiva
204900061109      * codice fiscale
205000061109     c                   movel     §ta4qcfm      bhycdfisc
205100990713     c                   endif
205200050623     c                   endif
205300050623      * VERIFICO SE IN BHYPARTIVA C'È LA SCRITTA DI PRIVATO  PULISCO IL CAMPO
205400050623     c                   If        %scan('PRIVATO':bhypartiva) > 0
205500050623     c                   clear                   bhypartiva
205600050623     c                   endif
205700050623
205800990517     c                   write     ndbhy000
205900981016     c                   endsr
206000981016      *----------------------------------------------------
206100981016     C     primanota     BEGSR
206200981016     C*----------------------------------------------------
206300981016     C*
206400981016     C* contabilizzazione batch
206500981020     c                   reset                   ndc071ds
206600981020     C                   MOVE      wsocieta      SOCieta071
206700981016     C                   MOVE      KNRAZ         rifint071
206800981016     c                   move      *loval        dtregda071
206900981016     c                   move      *hival        dtrega071
207000981016     c                   move      *loval        subnumd071
207100981016     c                   move      *hival        subnuma071
207200981209     C                   MOVE      *off          solcont071
207300981016    >C                   MOVE      *off          interat071
207400981016     C                   move      *off          err071
207500981203     c                   move      *off          control071
207600981016     C                   MOVEL     NDC071DS      kpjbu
207700981016     C*
207800981016     C                   CALL      'NDC071C'
207900981016     C                   parm                    kpjba
208000981016     C                   movel     kpjbu         ndc071ds
208100981016     C*
208200981016     C                   ENDSR
208300981014     C************************************************************
208400981014     C* Chiamata al modulo di controllo data registrazione
208500981014     C************************************************************
208600981014     C     CallMVC       BEGSR
208700981014     C*
208800981014     C                   move      '046'         Unita
208900981014     c                   move      dataiso       dtreg
209000981014     c                   move      dataiso       dtlgio
209100981014     C                   CALLB     'NDMVC104'
209200981020     C                   PARM      wsocieta      Societa
209300981014     C                   PARM      'CG'          CTB
209400981014     C                   PARM                    Unita
209500981014     C                   PARM      'D'           TpReg
209600981014     C                   PARM                    DTReg
209700981019     C                   PARM      '2'           RegIVA
209800981019     C                   PARM      '2'           TpRegIVA
209900981014     C                   PARM      '02'          Stato
210000981014     C                   PARM      *on           GesMsg
210100981014     C                   PARM                    Esito
210200981014     c                   if        esito = *blank
210300981014     C                   CALLB     'NDMVC106'
210400981014    >C                   PARM                    Societa
210500981014    >C                   PARM                    Ctb
210600981014    >C                   PARM                    TpReg
210700981014    >C                   PARM                    DtLGio
210800981014    >C                   PARM                    DtReg
210900981014     C                   PARM                    GesMsg
211000981014     C                   PARM                    Esito
211100981014     C*
211200981014     c                   end
211300981014     C*
211400981014     C                   ENDSR
211500981020     C*----------------------------------------------------*
211600981020     C* Reperimento dati società
211700981020     C*----------------------------------------------------*
211800981020     C     REPSOC        BEGSR
211900981020     C*
212000981020     C                   CALL      'XSOC'
212100981020     C                   PARM                    TIPXSC            6
212200981020     C                   PARM      wsocieta      SOCXSC            3
212300981020     C                   PARM                    CDSXSC            9 0
212400981020     C                   PARM                    MODXSC            3
212500981020     C                   PARM      *blanks       RTNXSC            1
212600981020     C                   PARM                    XSOCDS
212700981020     C                   PARM                    KPJBA
212800981020     C*
212900981020     C                   ENDSR
213000000121     C****************************************************
213100000121     C** CONVERSIONE IMPORTI IN UNA MONETA DIVERSA
213200000121     C****************************************************
213300000121     C     CNVEUR        BEGSR
213400000121      * conversione in moneta di conto e non
213500000121     C                   Movel     div2          DivMC0202
213600000121     C                   Movel     div1          Divisa0202
213700000121     C                   Move      *Loval        DtRif0202
213800000121     C                   Move      *On           NoCtrScos
213900000121     C                   move      tasdft        dtrif0202
214000000121     c**
214100000121     C                   CallP     X0202TestC    (PJX00202DS       :
214200000121     C                                            %Size(PJX00202DS):
214300000124     C                                            *On              :
214400000121     C                                            EsitoA           :
214500000121     C                                            NoCtrScos        :
214600000121     C                                            *Omit            :
214700000121     C                                            %Size(ANDVS00F)  :
214800000121     C                                            %Size(ANDVX00F)  :
214900000121     C                                            ANDVS00F         :
215000000121     C                                            ANDVX00F          )
215100000121     C                   endsr
215200000121      *
215300000120     c**********************************************************************
215400000120     c* COINVERSIONE SENZA TRIANGOLAZIONE
215500000120     C**********************************************************************
215600000120     C*
215700000120     C     SRYEUR        BEGSR
215800000120     C* CALCOLO L'IMPORTO D'ASSICURARE NELLA MONETA DELLA TARIFFA
215900000120     C                   CLEAR                   YEUR
216000000120     C                   movel     div1          YECDVI
216100000120     C                   movel     div2          YECDVO
216200000120     C                   z-add     impocnv       YECIMI
216300000120     C                   MOVE      '0'           YECDCO
216400000120     C*
216500000120     C                   CALL      'YEURCO'
216600000120     C                   PARM                    YEUR
216700000120     C*
216800000120     C                   IF        yecesi = ' '
216900000120     C                   Z-ADD     YECIMO        impocnv
217000000120     C                   END
217100000120     c*
217200000120     c                   ENDSR
217300000121     c**********************************************************************
217400000121     c* gestione totali per divisa e generali in moneta di conto
217500000121     C**********************************************************************
217600000121     C*
217700000121     C     SRTOTA        BEGSR
217800000121     c*
217900000121     c* verifico dove si trova la divisa in schiera
218000000121     c                   z-add     1             k
218100000121     c     tasdiv        lookup    div(k)                                 13
218200000124     c                   if        not *in13
218300000121     c                   z-add     1             k
218400000124     c     '   '         lookup    div(k)                                 13
218500000121     c                   move      tasdiv        div(k)
218600000124     c                   endif
218700000121     c   13k             occur     dstotali
218800000121     c                   do        5             a
218900000121     c                   add       im(a)         totimpds
219000000121     c                   enddo
219100000121     c*
219200000124     c                   do        5             a
219300000124     c                   add       iv(a)         totivads
219400000124     c                   enddo
219500000121     c*
219600000124     c  n12              add       fatbol        totbolds
219700000124     c   12              add       fatbold       totbolds
219800000121     c*
219900000121     c  n12              add       fattot        totfatds
220000000121     c   12              add       fattotd       totfatds
220100000121     c*
220200000121     c                   add       1             numfatds
220300000124     c*
220400000124     c                   if        tasdiv = xscdiv
220500000124     c     3             occur     dstotali
220600000124     c                   do        5             a
220700000124     c                   add       im(a)         totimpds
220800000124     c                   enddo
220900000124     c*
221000000124     c*
221100000124     c                   do        5             a
221200000124     c                   add       iv(a)         totivads
221300000124     c                   enddo
221400000124     c*
221500000124     c  n12              add       fatbol        totbolds
221600000124     c   12              add       fatbold       totbolds
221700000124     c*
221800000124     c  n12              add       fattot        totfatds
221900000124     c   12              add       fattotd       totfatds
222000000124     c*
222100000124     c                   add       1             numfatds
222200000121     c*
222300000124     c                   endif
222400000121     c*
222500000121     c                   endsr
222600000121     C*****************************************************************
222700000121     C**     SR INZIALE
222800000121     C*****************************************************************
222900000121     C     *INZSR        BEGSR
223000000121     C     *ENTRY        PLIST
223100000121     C                   PARM                    KPJBA
223200000121     C                   Z-ADD     0             VIDDFT
223300000121     C                   MOVEL     KPJBU         PARAM
223400000121     c*
223500000121     C                   Z-ADD     1             CODUT
223600000121     C                   CALL      'X§PARUT'
223700000121     C                   PARM                    UTEDSE
223800000121     C* RICERCA CAPOCONTI
223900000121     C                   DO        50            X                 2 0
224000000121     C                   MOVE      TCU(X)        TCUDS
224100000121     C     F56           CABNE     'CG'          END1
224200000121     C     F4            COMP      '1'                                    21
224300000121     C     F4            COMP      '2'                                    22
224400000121     C     F4            COMP      '3'                                    23
224500000121     C     F4            COMP      '6'                                    27
224600000121     C** 1 CLIENTI   21
224700000121     C** 2 FORNITORI 22
224800000121     C** 3 AGENTI    23
224900000121     C     F3            COMP      '0'                                242425
225000000121     C     F3            COMP      'I'                                    26
225100000121     C** 0 ITALIA   25
225200000121     C** 1 ESTERO   24
225300000121     C*  I CAPO CONTO IVA
225400000121     C   21
225500000121     CAN 24              Z-ADD     KCU(X)        KCE               4 0
225600000121     C   21
225700000121     CAN 25              Z-ADD     KCU(X)        KCI               4 0
225800000121     C   22
225900000121     CAN 24              Z-ADD     KCU(X)        KFE               4 0
226000000121     C   22
226100000121     CAN 25              Z-ADD     KCU(X)        KFI               4 0
226200000121     C   23
226300000121     CAN 24              Z-ADD     KCU(X)        KAE               4 0
226400000121     C   23
226500000121     CAN 25              Z-ADD     KCU(X)        KAI               4 0
226600000121     C   26              Z-ADD     KCU(X)        KIVA              4 0
226700000121     C   27              Z-ADD     KCU(X)        KBNA              4 0
226800000121     C     END1          TAG
226900000121     C                   END
227000000121     c                   setoff                                       212223
227100000121     c                   setoff                                       242526
227200000121     c                   setoff                                       27
227300000121     C*---------- RICERCA DITTA :
227400000121     C                   MOVEL     'SOC001'      TIPXSC
227500000121     C                   MOVEL     *BLANK        SOCXSC
227600000121     C                   EXSR      REPSOC
227700000121     C     RTNXSC        IFNE      '1'
227800000121     C                   MOVEL     XSOCDS        SOC001
227900000121     C                   MOVEL     xscrgs        RSUT             20
228000000121     c                   end
228100000121     C                   movel     wsocieta      WWWSoc            3
228200000121     C                   movel     'C     '      $kcc              6
228300000121     C                   movel     *blanks       $ksc              8
228400000121     C                   callb     'XCAPCLIFOR'
228500000121     C                   parm                    WWWSoc
228600000121     C                   parm                    $kcc
228700000121     C                   parm                    $ksc
228800000121     C*---------------------------------------------------------------*
228900000121     C* IMPOSTAZIONE CAMPI
229000000121     C     *LIKE         DEFINE    §qccdp        fatcdp
229100000121     C     *LIKE         DEFINE    TBLCOD        COD
229200000121     C     *LIKE         DEFINE    clnlineav     lin
229300000121     C     *LIKE         DEFINE    clnfiliale    fil
229400000121     C     *LIKE         DEFINE    clnKSC        KSC
229500000121     C     *LIKE         DEFINE    clnsocieta    soc
229600000121     C     *LIKE         DEFINE    indcdind      cdin
229700000121     C     *LIKE         DEFINE    indtpind      tpin
229800000121     C     *LIKE         DEFINE    tasva1        tasbol
229900000121     C     *LIKE         DEFINE    tasva1        tasiva
230000000121     C     *LIKE         DEFINE    tasrsd        wrasoc
230100000121     C     *LIKE         DEFINE    taatrc        §trc
230200000121     C*
230300000121     C/SPACE 3
230400000121     C*---------------------------------------------------------------*
230500000121     C* KLIST FILES
230600000121     C     KTAB          KLIST
230700000121     C                   KFLD                    CODUT             1 0
230800000121     C                   KFLD                    COD
230900000121     c                   z-add     1             codut
231000000121     C     KTAA          KLIST
231100000121     C                   KFLD                    TASAAS
231200000121     C                   KFLD                    TASLNP
231300000121     C                   KFLD                    TASNRS
231400000121     C                   KFLD                    TASNSP
231500000121     C                   KFLD                    §trc
231600000121     C** ACCESSO ANTAB X COD.TABELLA
231700000121     C     KTABC         KLIST
231800000121     C                   KFLD                    CODUT
231900000121     C                   KFLD                    COD
232000000121     C                   KFLD                    KEY               8
232100000121     C** ACCESSO ANTAB X COD.TABELLA
232200000121     C     Keycli        klist
232300000121     C                   kfld                    soc
232400000121     C                   kfld                    ksc
232500000121     C                   kfld                    fil
232600000121     C                   kfld                    lin
232700000121     c                   move      wsocieta      soc
232800000121     C     Keyind        KLIST
232900000121     C                   KFLD                    sogsogg
233000000121     C                   KFLD                    tpin
233100000121     C                   KFLD                    cdin
233200000121     c                   move      *blank        tpin
233300000121     c                   move      *blank        cdin
233400000121     C     Keybac        klist
233500000121     C                   kfld                    soc
233600000121     C                   kfld                    clnkcc
233700000121     C                   kfld                    clnksc
233800050509     C     Keyclp        klist
233900050509     C                   kfld                    codut
234000050509     C                   kfld                    clpkcc
234100050509     C                   kfld                    clpksc
234200000121     C     KeyTAS        klist
234300000121     C                   kfld                    kscfil
234400000121     C                   kfld                    nfthiv
234500030204
234600030204     c     kFiar5        Klist
234700030204     c                   Kfld                    TasAas
234800030204     c                   Kfld                    TasLnp
234900030204     c                   Kfld                    TasNrs
235000030204     c                   Kfld                    TasNsp
235100030204     c                   Kfld                    kAr5Trd
235200061109      * Tita4
235300061109     c     KTA4          KLIST
235400061109     c                   KFLD                    TASAAS
235500061109     c                   KFLD                    TASLNP
235600061109     c                   KFLD                    TASNRS
235700061109     c                   KFLD                    TASNSP
235800061109     c                   KFLD                    §4trc
235900130122     C     Kriv          klist
236000130122     C                   kfld                    wsocieta
236100130122     C                   kfld                    wtpreg            1
236200130122     C                   kfld                    rivaliq
236300130122     C                   kfld                    rivrifiva
236400000121     C** ACCESSO ANAGRAFICHE CLIENTI
236500000121     c                   time                    whhdat           14 0
236600000121     c                   move      whhdat        udater            8 0
236700000121     c                   move      udater        dataeur
236800000121     c                   move      dataeur       dataiso
236900000121     c                   move      dataiso       udater
237000000121     C** DATA DEL GIORNO AMG
237100000121     C     VIDDFT        CABEQ     0             FINE
237200000121     C** NON ACCETTO DATA ELABORAZIONE = 0
237300000121     C                   move      VIDDFT        dataeur
237400000121     c                   move      dataeur       dataiso
237500000121     c                   move      dataiso       vidfat            8 0
237600000121     C** DATA ELABORAZIONE RIBALTATA AMG
237700000121     C                   move      *zeros        aa                3 0
237800000121     C                   clear                   est
237900000121     C     *loval        setll     azorg01l
238000000121     C                   read      azorg01l                               07
238100000121     C     *in07         doweq     *off
238200000121     C     orgfl1        ifeq      'E'
238300000121     C                   add       1             aa
238400000121     C                   z-add     orgfil        est(aa)
238500000121     C                   end
238600000121     C                   read      azorg01l                               07
238700000121     C                   enddo
238800000121     C** CARICAMENTO SCHIERA FILIALI ESTERE
238900000121     C                   EXSR      CARTAB
239000000121     C** CARICAMENTO TABELLE
239100000121     C                   ENDSR
239200000000     OQSYSPRT   E            HERR             02
239300000000     O                       E(1)                85
239400961023     O                       VIDDFT              98 '  /  /    '
239500961023     O                                          103 '***'
239600000119     O                                          120 'TNSF04R'
239700000119     O                       PAGE1         Z    132
239800000000     O                                          128 'PAG.'
239900000000     O                       RSUT                20
240000000000     O          E            HERR             04
240100000000     O                       E(2)                66
240200941129     O          E            DERR        2
240300941129     O                                              'SPED.'
240400941129     O                       TASLNP            +  1
240500941129     O                       TASNRS        Z   +  1
240600941129     O                       TASNSP        Z   +  1
240700941129     O                       TASTBL            +  1
240800941129     O                                         +  1 'FATT.'
240900941129     O                       TASFIV            +  1
241000941129     O                                         +  0 '-'
241100941129     O                       TASNFT        Z   +  0
241200941130     O                       TASDFT            +  1 '    /  /  '
241300941129     O                       TASMSG            +  1
241400000121     O*
241500000208     OSYSPRT    E            GEN              02
241600000121     O                       RSUT                20
241700000121     O                       ST(1)               86
241800000121     O                       viddft              82 '  /  /    '
241900000121     O                                          104 'TNSF04R'
242000000121     O                       UDATE              118 '  /  /  '
242100000121     O          E            GENGEN      2
242200000121     O                       ST(2)               66
242300000121     O          E            GENDIV      2
242400000121     O                                           17 'TOTALE IMPONIBILE'
242500000124     O              N12      totimpds      2     54
242600000121     o              n12                          54 '    '
242700000124     O               12      totimpds      2     50
242800000121     O                       DIV(A)              55
242900000121     O          E            GENDIV      1
243000000121     O                                           12 'TOTALE I.V.A'
243100000124     O              N12      totivads      2     54
243200000121     o              n12                          54 '    '
243300000124     O               12      totivads      2     50
243400000121     O                       DIV(A)              55
243500000121     O          E            GENDIV      1
243600000121     O                                           12 'TOTALE BOLLI'
243700000124     O              N12      totbolds      2     54
243800000121     o              n12                          54 '    '
243900000124     O               12      totbolds      2     50
244000000121     O                       DIV(A)              55
244100000121     O          E            GENDIV      1
244200000121     O                                           16 'TOTALE FATTURATO'
244300000124     O              N12      totfatds      2     54
244400000121     o              n12                          54 '    '
244500000124     O               12      totfatds      2     50
244600000124     O                       DIV(A)              55
244700000121     O          E            GENDIV      1
244800000121     O                                           14 'NUMERO FATTURE'
244900000121     O                       numfatds      2     50
245000000120     OFTWFAT89  E            ST888            04
245100000120     O                       DFTGMA              68 '  /  /  '
245200000120     O                       TASNFT        Z     85
245300130128     O                                         +  1 '('
245400130128     O                       TASFIV            +  0
245500130128     O                                         +  0 ')'
245600000120     O                       PAGE          Z    101
245700000120     O          E            ST888            17
245800000120     O                                              'CODICE CLIENTE:'
245900000215     O                       STKSC             +  2
246000000120     O                                         +  4 '('
246100000120     O                       ASBOL             +  0
246200000120     O                                         +  0 ')'
246300080923     O                                         +  1 '('
246400080923     O                       TASFIV            +  0
246500080923     O                                         +  0 ')'
246600000120     O          E            ST888            19
246700000120     O                                              'Per informazioni'
246800000120     O                                         +  1 'rivolgersi presso:'
246900000120     O          E            ST888            21
247000000120     O                       PDFI
247100000120     O                       PRTRSC             103
247200000120     O          E            ST888            22
247300000120     O                       PVIA
247400000120     O          E            ST888            23
247500000120     O                       PCAP
247600000120     O                       PLOC              +  2
247700000120     O                       PPRV              +  2
247800000120     O                       PRTIND              85
247900000120     O          E            ST888            24
248000000120     O                                              'Telef.:'
248100000120     O                       PTEL              +  2
248200000120     O          E            ST888            25
248300000120     O                                              'Fax   :'
248400000120     O                       PFAX              +  2
248500000120     O                       PRTCAP              64
248600000120     O                       PRTLOC            +  1
248700000120     O                       PRTPRV            +  2
248800000120     O                       PRTNAZ            +  2
248900000120     O          E            ST888            27
249000000120     O                                              'Cod.Cli.Bollettazione:'
249100000120     O                       TASKSC            +  1
249200000120     O                                         +  4 '('
249300000120     O                       ASBOL             +  0
249400000120     O                                         +  0 ')'
249500130308     O          E   N01      ST888            28
249600130308     O                                              'C.F.  : '
249700130308     O                       FATCDF            +  1
249800000120     O          E            ST888            29
249900130308     O                                              'P.IVA : '
250000130308     O                       FATIVA            +  1
250100000120     O          E            ST888            39
250200000120     O              N12      I(3)          Z    113
250300000120     O               12      ID(3)         4    113
250400000120     O                       S(3)               105
250500000120     O              N12      I(2)          Z    104
250600000120     O               12      ID(2)         4    104
250700000120     O                       S(2)                96
250800000120     O              N12      I(1)          Z     95
250900000120     O               12      ID(1)         4     95
251000000120     O                       S(1)                87
251100000120     O              N12      PRTPOR        Z     86
251200000120     O               12      PRTPORD       4     86
251300000120     O                     09TASQFT        4     77
251400000120     O                    N09TASPVL        Z     77
251500000120     O                    N09TASPKB        4     72
251600000120     O                  42N09TASVLF        2     65
251700000120     O                       TASNCL        Z     58
251800000120     O                       WCAP                53
251900000120     O                       RSC13               47
252000000120     O                       CIT3                33
252100000120     O                       CIT2                30
252200000120     O                       RMN9          Z     27
252300000120     O                       WTSP                18
252400000120     O                       MDSP                17
252500000120     O                       GDSP                15
252600000120     O                       TASNSP        Z     12
252700000120     O                       TASNRS        Z      5
252800000120     O                       TASLNP               3
252900000120     O          E            ST8882      1
253000000120     O              N12      I2(3)         Z    113
253100000120     O               12      I2D(3)        4    113
253200000120     O                       S2(3)              105
253300000120     O              N12      I2(2)         Z    104
253400000120     O               12      I2D(2)        4    104
253500000120     O                       S2(2)               96
253600000120     O              N12      I2(1)         Z     95
253700000120     O               12      I2D(1)        4     95
253800000120     O                       S2(1)               87
253900110712     O          E            STCUP       1
254000110712     O                                        +   5 'CUP-CIG :'
254100110712     O                       StpCup           +   1
254200000120     O          E            DCOD84      3
254300000120     O                                              'Legenda:'
254400000120     O          E            DCOD84      2
254500000120     O                       LES(1)
254600000121     O                       LINS(1)           +  0
254700000121     O                       LED(1)            +  0
254800000121     O                       LES(2)            +  2
254900000121     O                       LINS(2)           +  0
255000000121     O                       LED(2)            +  0
255100000121     O                       LES(3)            +  2
255200000121     O                       LINS(3)           +  0
255300000121     O                       LED(3)            +  0
255400000120     O                       LES(4)            +  2
255500000121     O                       LINS(4)           +  0
255600000120     O                       LED(4)            +  0
255700000120     O                       LES(5)            +  2
255800000121     O                       LINS(5)           +  0
255900000120     O                       LED(5)            +  0
256000000120     O                       LES(6)            +  2
256100000121     O                       LINS(6)           +  0
256200000120     O                       LED(6)            +  0
256300000120     O                       LES(7)            +  2
256400000121     O                       LINS(7)           +  0
256500000120     O                       LED(7)            +  0
256600000120     O                       LES(8)            +  2
256700000121     O                       LINS(8)           +  0
256800000120     O                       LED(8)            +  0
256900000120     O          E            DCOD85      1
257000000120     O                       LES(9)
257100000121     O                       LINS(9)           +  0
257200000120     O                       LED(9)            +  0
257300000120     O                       LES(10)           +  2
257400000121     O                       LINS(10)          +  0
257500000120     O                       LED(10)           +  0
257600000120     O                       LES(11)           +  0
257700000121     O                       LINS(11)          +  0
257800000120     O                       LED(11)           +  0
257900000120     O                       LES(12)           +  2
258000000121     O                       LINS(12)          +  0
258100000120     O                       LED(12)           +  0
258200000120     O                       LES(13)           +  2
258300000121     O                       LINS(13)          +  0
258400000120     O                       LED(13)           +  0
258500000120     O                       LES(14)           +  2
258600000121     O                       LINS(14)          +  0
258700000120     O                       LED(14)           +  0
258800000120     O                       LES(15)           +  2
258900000121     O                       LINS(15)          +  0
259000000120     O                       LED(15)           +  0
259100000120     O                       LES(16)           +  2
259200000121     O                       LINS(16)          +  0
259300000120     O                       LED(16)           +  0
259400000120     O          E            DCOD86      1
259500000120     O                       LES(17)
259600000121     O                       LINS(17)          +  0
259700000120     O                       LED(17)           +  0
259800000120     O                       LES(18)           +  2
259900000121     O                       LINS(18)          +  0
260000000120     O                       LED(18)           +  0
260100000120     O                       LES(19)           +  2
260200000121     O                       LINS(19)          +  0
260300000120     O                       LED(19)           +  0
260400000120     O                       LES(20)           +  2
260500000121     O                       LINS(20)          +  0
260600000120     O                       LED(20)           +  0
260700050623     O          E            BOLVIR      3
260800050623     O                       STAESE              85
260900000120     O          E            RCOD84      3
261000000120     O                       RI(1)
261100000120     O                       RI(2)             +  0
261200000120     O          E            RCOD84      1
261300000120     O                       RI(3)
261400000120     O                       RI(4)             +  0
261500000120     O          E            RCOD84      1
261600000120     O                       RI(5)
261700000120     O                       RI(6)             +  0
261800040702     O****      E            RCOD84      1
261900040702     O****                   RI(7)
262000000120     O          E            ICOD84           88
262100000120     O                                           72 'IVA a esigibilita'' imme'
262200000120     O                                           77 'diata'
262300000120     O          E            ST88TT           90
262400000120     O          E            ST888T      1
262500000120     O              N11n12   SIV(1)        Z     73
262600000120     O              N11 12   SIVD(1)       4     73
262700000120     O              N11      STAALI        4     61
262800130116     O               11      DESCEI              78
262900000120     O                 n12   SIM(1)        Z     55
263000000120     O                  12   SIMD(1)       4     55
263100130618     O******           n12   VARIE         Z     40
263200130618     O******            12   VARIEd        4     40
263300130618     O******           n12   PRTTPI        Z     25
263400130618     O******            12   PRTTPId       4     25
263500000120     O          E            ST888G      0
263600000120     O                 n12   FATTOT        2    112
263700000120     O                 n12   FATBOL        z     97
263800000120     O                  12   FATTOTD       4    112
263900000120     O                  12   FATBOLD       4     97
264000000124     O          E            ST888G      1
264100000124     O                       tasdiv               6
264200000124     O                                            8 ':'
264300991110     OTITAS000  E            UGO
264400991110     O                       TASfic
264500011116     O                       TASias
264600011116     O                       TASvas
264700011116     O                       TASfcm
264800941129**
264900961023             ***  IMPUTAZIONE CONTABILE FATTURE PUNTI OPERATIVI AL     ***
265000891107RILEVAZIONE ERRORI
265100891107    NUMERO DI FATTURA GIA' ESISTENTE
265200000000    DATA FATTURA MINORE ULTIMO BOLLATO
265300000000    MANCA ANAGRAFICO CLIENTE
265400930727    TOTALE IMPONIBILE = 0
265500941129
265600941129    CONTO INESISTENTE
265700000120**  DATI PERSONALI
265800040702Informativa ai sensi dell'art. 13 D.Lgs.196/03. Il trat
265900040702tamento dei dati  personali che  Vi riguardano viene
266000040702effettuato nel rispetto del D.Lgs.196/03, per finalita'
266100040702 strettamente inerenti al rapporto commerciale e fatti
266200040702salvi i diritti di accesso, opposizione e rettifica di
266300040702cui all'art. 7 del D.Lgs. stesso.
266400040702
266500000121**
266600000121            **  DISTINTA FATTURE PUNTI OPERATIVI AL   XX/XX/XX  **    1
266700000121TOTALI GENERALI                                                       2
