000100970225     H DECEDIT('0,') DATEDIT(*DMY.)
000200970225     H* FNLV66R *-----------------------------------------------------*
000300970225     H*         - REPORT CONTROLLO QUALITA' - BATCH                   *
000400970225     H*---------------------------------------------------------------*
000500970224     FAZORG02L  IF   E           K DISK
000600970225     FAZORG01L  IF   E           K DISK
000700970225     F                                     RENAME(AZORG:AZORG1)
000800090911     FFiSBO02L  IF   E           K DISK
000900090915     FFISPA02L  IF   E           K DISK
001000090915     FFISPA01L  IF   E           K DISK    RENAME(FISPA000: FISPAfil)
001100090916     Ffisat03L  IF   E           K DISK
001200090916     Ffisat02L  IF   E           K DISK    RENAME(fisat000:fisatfil)
001300000208     FFNSCO03L  IF   E           K DISK
001400010124     FFNSCO04L  IF   E           K DISK
001500010124     F                                     RENAME(FNSCO000:FNSCO4)
001600090930     FFISAR06L  IF   E           K DISK
001700990520     FFNANM04L  IF   E           K DISK
001800990409     FAZCLN01L  IF   E           K DISK
001900970226     FFNANT01L  IF   E           K DISK
002000970226     FFNANT02L  IF   E           K DISK
002100970226     F                                     RENAME(FNANT000:FNANT2)
002200990504     FFNANT04L  IF   E           K DISK
002300990504     F                                     RENAME(FNANT000:FNANT4)
002400970225     FTABEL00F  IF   E           K DISK
002500990504     FAZCAE05L  IF   E           K DISK
002600010124     FWFRCQ11L  UF A E           K DISK    USROPN
002700970227     FFNLV66P   O    E             PRINTER
002800970401     F*
002900091001     D ENS             S              5    DIM(4)
003000970401     D GBO             S              1    DIM(4) CTDATA PERRCD(4)
003100010124     D SCA             S              3  0 DIM(3) CTDATA PERRCD(3)
003200010125     D SCR             S              3  0 DIM(5) CTDATA PERRCD(5)
003300970401     D W65             S              1    DIM(65)
003400990503     D W21             S              1    DIM(21)
003500000626     D PT              S              3  0 DIM(100)
003600060131     D EST             S              3  0 DIM(100)
003700970225      * Le schiere che seguono hanno 3 elementi:
003800090930      *     1° elemento: TIPO SERVIZIO "H"
003900090930      *     2° elemento: TIPO SERIVZIO "E"
004000090930      *     3° elemento: TIPO SERVIZIO "C"
004100090930      *     4° elemento: TIPO SERVIZIO "D"
004200090930     D BOL             S              7  0 DIM(4)
004300090930     D BSS             S              7  0 DIM(4)
004400090930     D FAF             S              7  0 DIM(4)
004500090930     D SDA             S              7  0 DIM(4)
004600090930     D SPA             S              7  0 DIM(4)
004700090930     D NOC             S              9  0 DIM(4)
004800090930     D CSB             S              9  0 DIM(4)
004900090930     D DSC             S              9  0 DIM(4)
005000090930     D DSR             S              9  0 DIM(4)
005100990409     D*
005200990430     D* Schiere per memorizzazione tot. a livello di area
005300090930     D RIT             S              8  0 DIM(4)
005400090930     D SOS             S              7  0 DIM(4)
005500090930     D TBL             S              8  0 DIM(4)
005600090930     D B88             S              8  0 DIM(4)
005700090930     D PAR             S              8  0 DIM(4)
005800090930     D A88             S              8  0 DIM(4)
005900090930     D SFA             S              8  0 DIM(4)
006000090930     D TDA             S              8  0 DIM(4)
006100090930     D SSP             S              8  0 DIM(4)
006200090930     D DIS             S              8  0 DIM(4)
006300090930     D DISP            S              8  0 DIM(4)
006400090930     D DDT             S              7  0 DIM(4)
006500090930     D NCN             S             10  0 DIM(4)
006600090930     D SCS             S             10  0 DIM(4)
006700090930     D DCN             S             10  0 DIM(4)
006800090930     D DRT             S             10  0 DIM(4)
006900090930     D DIA             S              8  0 DIM(4)
007000990430     D* Schiere per memorizzazione tot. a livello di distretto
007100090930     D RID             S              8  0 DIM(4)
007200090930     D SOD             S              7  0 DIM(4)
007300090930     D TBD             S              8  0 DIM(4)
007400090930     D B8D             S              8  0 DIM(4)
007500090930     D PAD             S              8  0 DIM(4)
007600090930     D A8D             S              8  0 DIM(4)
007700090930     D SFD             S              8  0 DIM(4)
007800090930     D TDD             S              8  0 DIM(4)
007900090930     D SSD             S              8  0 DIM(4)
008000090930     D DID             S              8  0 DIM(4)
008100090930     D DISD            S              8  0 DIM(4)
008200090930     D DDD             S              7  0 DIM(4)
008300090930     D NCD             S             10  0 DIM(4)
008400090930     D SCD             S             10  0 DIM(4)
008500090930     D DCD             S             10  0 DIM(4)
008600090930     D DRD             S             10  0 DIM(4)
008700090930     D DSD             S              8  0 DIM(4)
008800990429     D*
008900990409     D POM             S              1    DIM(31)
009000970224     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
009100971125     D DSLV55        E DS                  EXTNAME(FNLV55DS)
009200000404     D DSUL06        E DS                  EXTNAME(TRUL06DS)
009300000404     D  LIN                    1     90  0
009400000404     D                                     DIM(30)
009500970224     D KPJBA         E DS
009600970224     D CNCR80        E DS
009700970401     D DS05          E DS
009800090930     D Dvpodeco      E DS
009900970401     D DS17          E DS
010000990409     D DS1Y          E DS
010100970603     D OG148         E DS
010200000626     D OG143         E DS
010300090605     d ds5e          e ds
010400090930     D* DS PER TIBS02R - GESTIONE TNTBE00F
010500090930     D DSBS02        E DS                  EXTNAME(TIBS02DS)
010600060131     d
010700970224     D WLBDA8          DS
010800970224     D  G08DAT                 1      8  0
010900970224     D  G08INV                 9     16  0
011000970224     D  G08ERR                17     17
011100970224     D  G08TGI                18     22  0
011200970527     D WGIDAT          DS
011300970527     D  GI8DAT                 1      8  0
011400970527     D  GI8INV                 9     16  0
011500990409     D  WAA                    9     12  0
011600990409     D  WMM                   13     14  0
011700990409     D  WGG                   15     16  0
011800970527     D  GI8TGI                17     21  0
011900970527     D                 DS
012000970527     D  PRTDAT                 1      4  0 INZ
012100970527     D  PRTGG                  1      2  0
012200970527     D  PRTMM                  3      4  0
012300970225     D PARAM           DS
012400970225     D  V1CDST                 1      1
012500970225     D  PARCAR                 2      4  0
012600970225     D  V1CLIN                 5      7  0
012700970225     D  PARDEL                 8     15  0
012800970225     D  V1CDEL                16     23  0
012900970407     D  V1CTSP                25     25
013000970526     D  V1CDAL                26     33  0
013100970526     D  PARDAL                34     41  0
013200970527     D  PARTGI                42     46  0
013300990427     D  V1CCRP                50     50
013400091001     d
013500091001     d $v              s              2  0
013600091001     d
013700990503     D COS1            C                   CONST('        di cui BAR  -
013800990503     D                                      ')
013900990503     D COS2            C                   CONST('        di cui S.D.I-
014000990503     D                                     .')
014100990503     D COS3            C                   CONST('AREA                -
014200990430     D                                      ')
014300990503     D COS4            C                   CONST('DISTRETTO           -
014400990430     D                                      ')
014500970228      *---------------------------------------------------------------*
014600970228      * INDICATORI USATI
014700970228      *---------------------------------------------------------------*
014800970228      * 01 = RICHIESTO UN P.O. SPECIFICO
014900970228      * 02 = RICHIESTO UN COD.AREA SPECIFICO
015000970228      * 03 = RICHIESTO UN COD.DISTRETTO SPECIFICO
015100970526      * 05 = RICHIESTO UN PERIODO DAL/AL
015200970527      * 06 = NON STAMPA COD+DECOD. DEL P.O.
015300990430      * 11 = ON NON PERVENUTI DATI DEL PARTITO
015400990430      * 12 = ON NON PERVENUTI DATI DELL' A TERRA
015500990430      * 13 = ON NON PERVENUTI DATI DEL BOLLETTATO
015600990430      * 14 = ON NON PERVENUTI DATI DEL CONSEGNATO
015700050831      * 15 = ON NON PERVENUTO DATO DEI RITIRI NON EFFETTUATI - eliminato
015800990430      * 16 = ON NON PERVENUTI DATI DELLA STATISTICA ARRIVI
015900990430      * 17 = ON NON PERVENUTO DATO ESECUZIONE IDD
016000990430      * 18 = NON FA MAI L'IDD
016100970228      * 30 = DI COMODO
016200970228      * 31 = DI COMODO
016300970228      * 40 = ON STAMPA RIGA DI TOTALE PER PUNTO OPERATIVO
016400990503      * 41 = CONDIZIONAMENTO E HI DICITURE IN STAMPA
016500970228      *---------------------------------------------------------------*
016600970227     C*
016700970526     C* RICHIESTO UN PERIODO DAL/AL      --> 05 ON
016800970527     C     PARDAL        COMP      PARDEL                             05
016900970526     C*
017000970225     C* RICHIESTO UN P.O. SPECIFICO      --> 01 ON
017100970225     C     V1CLIN        COMP      0                                  01
017200970226     C   01              MOVEL(P)  V1CLIN        PRTLIN
017300970226     C  N01              MOVEL     'TUTTI'       PRTLIN
017400970225     C* RICHIESTO UN COD.AREA SPECIFICO  --> 02 ON
017500970225     C     PARCAR        COMP      0                                  02
017600970226     C   02              MOVEL(P)  PARCAR        PRTCA1
017700970226     C  N02              MOVEL     'TUTTI'       PRTCA1
017800970225     C* RICHIESTO UN DISTRETTO SPECIFICO --> 03 ON
017900970227     C     V1CDST        COMP      *BLANKS                            0303
018000970226     C   03              MOVEL(P)  V1CDST        PRTDST
018100970226     C  N03              MOVEL     'TUTTI'       PRTDST
018200970227     C* STAMPO INTESTAZIONE
018300970227     C                   WRITE     LV66T1
018400970227     C                   Z-ADD     7             $RIGHE            2 0
018500970225     C*
018600970225     C   01V1CLIN        CHAIN     AZORG01L                           30
018700970314     C  N01
018800970314     CANN02V1CDST        SETLL     AZORG02L                           30
018900970226     C  N01
019000970226     CAN 02KORG2         SETLL     AZORG02L                           30
019100970225     C  N01
019200970225     CANN02
019300970225     CANN03              READ      AZORG02L                               30
019400970314     C  N01
019500970314     CANN02
019600970314     CAN 03V1CDST        READE     AZORG02L                               30
019700970225     C  N01
019800970225     CAN 02KORG2         READE     AZORG02L                               30
019900970227    1C     *IN30         DOWEQ     *OFF
020000980407     C* ESCLUDO RECORD ANNULLATI E DISTRETTO SEDE
020100970311    2C     ORGFAG        IFEQ      'F'
020200970311     C     ORGFVA        ANDEQ     *BLANKS
020300970311     C     ORGFL3        ANDNE     '9'
020400970311     C     ORGFAG        OREQ      'A'
020500970311     C     ORGFVA        ANDEQ     *BLANKS
020600970227     C     ORGFL3        ANDNE     '9'
020700970603     C                   MOVEL     ORGDE8        OG148
020800000626     C                   MOVEL     ORGDE3        OG143
020900970603   2AC     §OGLFV        IFEQ      *BLANKS
021000970603     C     *IN01         OREQ      *ON
021100110323     C* ESCLUDO I P.O. POSTE
021200110323   2BC     §OGPT         IFNE      'S'
021300970526     C*
021400990503     C* VERIFICO ROTTURA DI DISTRETTO E AREA
021500970227    3C     ORGFL3        IFNE      PRTFL3
021600970226     C                   MOVE      '1'           WBRK
021700990623     C                   MOVE      '1'           WBRKPE
021800970401     C                   MOVE      '1'           WBRKD
021900970227    3C                   END
022000970227    3C     ORGCAR        IFNE      PRTCAR
022100970226     C                   MOVE      '1'           WBRK
022200990623     C                   MOVE      '1'           WBRKPE
022300110323     C                   MOVE      '1'           WBRKA
022400970227    3C                   END
022500990429     C*
022600110323     C                   MOVEL     ORGDIT        COMDIT            1
022700990429     C*
022800990429     C* ROTTURA DI DISTRETTO O AREA:
022900970227    3C     WBRK          IFEQ      '1'
023000990429     C* - STAMPO I TOTALI
023100990503    4C     WPRT          IFEQ      '1'
023200990429     C                   EXSR      ELATAR
023300990503    5C     WBRKD         IFEQ      '1'
023400990429     C                   EXSR      ELATDS
023500990503    5C                   END
023600990503     C* Stampo 2 righe vuote
023700990503    5C     $RIGHE        IFLE      63
023800990503     C                   WRITE     LV66T2
023900990503     C                   WRITE     LV66T2
024000990503     C                   ADD       2             $RIGHE
024100990503    5C                   END
024200990503    4C                   END
024300990503     C*
024400990429     C                   CLEAR                   WPRT
024500990503     C*
024600990503     C     WBRKD         IFEQ      '1'
024700990503     C* Decodifico distretto
024800990503     C                   EXSR      DECDST
024900990503     C                   END
025000110323     C* Decodifico area
025100110323     C     WBRKA         IFEQ      '1'
025200110323     C                   EXSR      DECCAR
025300110323     C                   END
025400990429     C* - STAMPO DECODIFICHE DISTRETTO E AREA
025500970227     C                   ADD       3             $RIGHE
025600970227     C* Salto pagina se ho superato il n° massimo di righe
025700970227    4C     $RIGHE        IFGT      60
025800970226     C                   WRITE     LV66T1
025900970227     C                   Z-ADD     10            $RIGHE
026000970227    4C                   END
026100970227     C*
026200970227     C                   WRITE     LV66T3
026300970226     C                   WRITE     LV66T4
026400970227     C                   WRITE     LV66T3
026500110323    3C                   END
026600970226     C*
026700970306     C     ORGDE5        IFNE      *BLANKS
026800970306     C                   MOVEL     ORGDE5        ORGDEP
026900970306     C                   ELSE
027000970306     C                   MOVEL     ORGDES        ORGDEP
027100970306     C                   END
027200970527     C* CICLO DI ELABORAZIONE DEL P.O. PER IL PERIODO DAL/AL RICHIESTO
027300970527     C                   Z-ADD     PARDEL        WDATEL
027400970527     C                   MOVE      PARTGI        WTGI
027500970527     C                   SETOFF                                       06
027600000208     C                   CLEAR                   SAVTFP
027700000208     C                   CLEAR                   SAVTFA
027800970527     C     WDATEL        DOWLE     PARDAL
027900990503     c* Se vengono creati i dati li devo anche cancellare prima
028000990503     C     V1CCRP        IFEQ      'S'
028100990503     C     *IN31         DOUEQ     '1'
028200010124     C     KRCQ          DELETE    WFRCQ11L                           31
028300990503     C                   ENDDO
028400990503     C                   END
028500970226     C* Pulisco campi e schiere di calcolo
028600970226     C                   CLEAR                   BOL
028700970226     C                   CLEAR                   BSS
028800970226     C                   CLEAR                   SDA
028900970226     C                   CLEAR                   FAF
029000970226     C                   CLEAR                   SPA
029100970226     C                   CLEAR                   NOC
029200970226     C                   CLEAR                   CSB
029300010124     C                   CLEAR                   DSC
029400010124     C                   CLEAR                   DSR
029500970226     C                   CLEAR                   TP88B
029600970226     C                   CLEAR                   TP88A
029700970226     C                   CLEAR                   TOTPAR
029800970226     C                   CLEAR                   TOTRIT
029900970226     C                   CLEAR                   TOTBAM
030000970226     C                   CLEAR                   TOTDIS
030100040405     C                   CLEAR                   TOTDISP
030200970226     C                   CLEAR                   TOTDAR
030300970606     C                   CLEAR                   TOTNAR
030400970606     C                   CLEAR                   TOTARR
030500000208     C* DETERMINO TERMINAL DI PARTENZA E DI ARRIVO
030600000208     C                   SETOFF                                       07
030700000208     C                   CLEAR                   DSLV55
030800000208     C                   MOVE      ORGFIL        D55LIN
030900000208     C                   Z-ADD     WDATEL        D55DRF
031000000208     C                   CALL      'FNLV55R'
031100000208     C                   PARM                    DSLV55
031200000208     C                   MOVE      D55TFP        PRTTFP
031300000208     C                   MOVE      D55TFA        PRTTFA
031400000208     C* VEDO SE I TERMINAL SONO CAMBIATI (SERVE SE RICHIESTO UN PERIODO
031500000208     C* DAL AL)
031600000208     C     PRTTFP        IFNE      SAVTFP
031700000208     C                   SETON                                        07
031800000208     C                   Z-ADD     PRTTFP        SAVTFP
031900000208     C                   ENDIF
032000000208     C     PRTTFA        IFNE      SAVTFA
032100000208     C                   SETON                                        07
032200000208     C                   Z-ADD     PRTTFA        SAVTFA
032300000208     C                   ENDIF
032400970226     C* CALCOLO RITIRI NON ESEGUITI,DISGUIDI,DISGUIDI IN AREA,MANCA BAM
032500970225     C                   EXSR      ELAANT
032600970226     C* CALCOLO BOLLETTATO IN SOSPENSIONE
032700970225     C                   EXSR      ELASBO
032800990504     C***
032900990504     C* I DATI RELATIVI ALLE PARTENZE LI CALCOLO SOLO SE IL P.O. LETTO
033000060131     C* E' UN TERMINAL DI PARTENZA o un p.o. estero dalla data decorrenza
033100990504     C***
033200060131     c     orgfil        lookup    est                                    32
033300990504     C     ORGFIL        IFEQ      PRTTFP
033400060131     c     *in32         oreq      *on
033500070510     c***  wdatel        andge     §vpoqua
033600060131     c
033700990504     C* CALCOLO COLLI PARTITI CON PISTOLA 88
033800970225     C                   EXSR      ELASPA
033900970306     C* CALCOLO SPEDIZIONI A TERRA FUORI AFFIDABILITA' E SPAREGGIATE
034000970225     C                   EXSR      ELASAT
034100990506     C                   SETOFF                                       42
034200990505     C                   ELSE
034300990505     C* Se per il p.o. letto non devo calcolare i dati relativi alle
034400060131     C* partenze imposto flag='2' per non stampare "dati non pervenuti"
034500060131     C                   MOVE      '2'           WTSPA
034600060131     C                   MOVE      '2'           WTSAT
034700990506     C                   SETON                                        42
034800990504     C                   END
034900970606     C* CALCOLO COLLI PARTITI NON ARRIVATI AL TERMINAL
035000990504     C* (SOLO SE IL P.O. LETTO E' TERMINAL DI ARRIVO)
035100990504     C                   EXSR      CHKTFA
035200990504     C     WTARR         IFEQ      '1'
035300970606     C                   EXSR      ELASAR
035400990506     C                   SETOFF                                       43
035500990506     C                   ELSE
035600990506     C                   MOVE      '1'           WTSAR
035700990506     C                   SETON                                        43
035800990504     C                   END
035900970226     C* CALCOLO SPEDIZIONI NON CONSEGNATE FUORI AFFIDABILITA'
036000970225     C                   EXSR      ELASCO
036100990409     C* CALCOLO SE ESEGUITO IDD O MENO
036200990409     C                   EXSR      ELAIDD
036300970226     C* STAMPO PER PUNTO OPERATIVO
036400970225     C                   EXSR      PRTLNP
036500970527     C**
036600970527     C* INCREMENTO LA DATA (WDATEL) DI UN GIORNO
036700970527     C                   CLEAR                   WGIDAT
036800970527     C                   ADD       1             WTGI
036900970527     C                   Z-ADD     WTGI          GI8TGI
037000970527     C                   CALL      'XSRGI8'
037100970527     C                   PARM                    WGIDAT
037200970527     C                   MOVE      GI8INV        WDATEL
037300970527     C*
037400970527     C                   MOVE      *BLANKS       WBRK              1
037500990623     C                   MOVE      *BLANKS       WBRKPE            1
037600970527     C                   MOVE      *BLANKS       WBRKD             1
037700110323     C                   MOVE      *BLANKS       WBRKA             1
037800970527     C**
037900970527     C                   ENDDO
038000970527     C****
038100970527     C* STAMPO TOTALE PUNTO OPERATIVO SE RICHIESTO UN PERIODO DAL/AL
038200970527     C     *IN05         IFEQ      *ON
038300970527     C                   EXSR      PRTTOF
038400970527     C                   END
038500970225     C*
038600110323   2BC                   END
038700000626   2AC                   END
038800970227    2C                   END
038900970225     C   01              SETON                                        30
039000970225     C  N01
039100970225     CANN02
039200970225     CANN03              READ      AZORG02L                               30
039300970314     C  N01
039400970314     CANN02
039500970314     CAN 03V1CDST        READE     AZORG02L                               30
039600970225     C  N01
039700970225     CAN 02KORG2         READE     AZORG02L                               30
039800970227    1C                   ENDDO
039900990505     C* - STAMPO I TOTALI (SE NON RICHIESTO UN P.O. SPECIFICO)
040000990429     C     WPRT          IFEQ      '1'
040100990505     C     *IN01         ANDEQ     *OFF
040200990429     C                   EXSR      ELATAR
040300990505     C* STAMPO TOT.DISTR.SOLO SE NON E' STATA ELABOR. UN'AREA SPECIFICA
040400990505     C  N02              EXSR      ELATDS
040500990429     C                   END
040600970226     C* STAMPO RIGA "FINE STAMPA"
040700970227     C                   ADD       3             $RIGHE
040800970227    1C     $RIGHE        IFGT      65
040900970227     C                   WRITE     LV66T1
041000970227     C                   WRITE     LV66T3
041100970227    1C                   END
041200970226     C                   WRITE     LV66E2
041300971125     C*
041400971125     C                   MOVEL     'C'           D55TLA
041500971125     C                   CALL      'FNLV55R'
041600971125     C                   PARM                    DSLV55
041700970227     C*
041800970224     C                   SETON                                        LR
041900990504     C*****************************************************************
042000990504     C* VERIFICA SE P.O. LETTO E' TERMINAL DI ARRIVO
042100990504     C*****************************************************************
042200990504     C     CHKTFA        BEGSR
042300990504     C                   MOVEL     'A'           KEPA
042400990504     C                   MOVE      *BLANKS       WTARR             1
042500990504     C     KCAE5         SETLL     AZCAE05L
042600990504     C     KCAE5         READE     AZCAE05L                               31
042700990504     C     *IN31         DOWEQ     *OFF
042800990504     C     WTARR         ANDEQ     *BLANKS
042900990504     C     CAEATB        IFEQ      *BLANKS
043000990504     C     WDATEL        ANDGE     CAEDDE
043100990504     C     WDATEL        ANDLE     CAEDSC
043200990504     C                   MOVEL     '1'           WTARR
043300990504     C                   END
043400990504     C     KCAE5         READE     AZCAE05L                               31
043500990504     C                   ENDDO
043600990504     C                   ENDSR
043700970226     C*****************************************************************
043800970226     C* DETERMINAZIONE DISGUIDI,MANCA BAM,RITIRI NON EFFETTUATI
043900970226     C*****************************************************************
044000970226     C     ELAANT        BEGSR
044100050831     C* RITIRI NON ESEGUITI : non esiste più tolto 31/8/05    EDPES
044200000404     C* Se p.o. in £6 (gestito da altri) non segnalo dati non pervenuti
044300050831     C**                 Z-ADD     300           WCAA
044400050831     C**                 CLEAR                   WT300
044500050831     C**   ORGFIL        LOOKUP    NOG                                    31
044600050831    1C**   *IN31         IFEQ      *OFF
044700050831     C**   KANT          CHAIN     FNANT01L                           31
044800050831    2C**   *IN31         IFEQ      *OFF
044900050831     C**                 Z-ADD     ANTTOT        TOTRIT
045000050831     C**                 MOVE      '1'           WT300             1
045100050831    2C**                 END
045200050831   X1C**                 ELSE
045300050831     C**                 CLEAR                   TOTRIT
045400050831     C**                 MOVE      '1'           WT300             1
045500050831    1C**                 ENDIF
045600970305     C* MANCA DDT
045700970226     C                   Z-ADD     71            WCAA
045800970226     C     KANT          SETLL     FNANT01L
045900970226     C     KANT          READE     FNANT01L                               31
046000970226     C     *IN31         DOWEQ     *OFF
046100970226     C                   ADD       ANTTOT        TOTBAM
046200970226     C     KANT          READE     FNANT01L                               31
046300970226     C                   ENDDO
046400970226     C* DISGUIDI
046500990419     C     WDATEL        IFLE      19990418
046600970226     C                   Z-ADD     55            WCAA
046700990419     C                   ELSE
046800990419     C                   Z-ADD     56            WCAA
046900990419     C                   ENDIF
047000990419     C*
047100990504     C     KANT          SETLL     FNANT04L
047200990504     C     KANT          READE     FNANT04L                               31
047300970226     C     *IN31         DOWEQ     *OFF
047400970226     C                   ADD       ANTTOT        TOTDIS
047500990504     C     KANT          READE     FNANT04L                               31
047600970226     C                   ENDDO
047700040405     C* DISGUIDI non spuntati in partenza
047800040405     C                   Z-ADD     200           WCAA
047900040405     C*
048000040421     C     KANT          SETLL     FNANT02L
048100040421     C     KANT          READE     FNANT02L                               31
048200040405     C     *IN31         DOWEQ     *OFF
048300040405     C                   ADD       ANTTOT        TOTDISP
048400040421     C     KANT          READE     FNANT02L                               31
048500040405     C                   ENDDO
048600970226     C* DISGUIDI IN AREA
048700970226     C                   Z-ADD     145           WCAA
048800970226     C                   DO        2
048900970226     C     KANT          SETLL     FNANT02L
049000970226     C     KANT          READE     FNANT02L                               31
049100970226     C     *IN31         DOWEQ     *OFF
049200970226     C                   ADD       ANTTOT        TOTDAR
049300970226     C     KANT          READE     FNANT02L                               31
049400970226     C                   ENDDO
049500970226     C                   Z-ADD     146           WCAA
049600970226     C                   ENDDO
049700970226     C                   ENDSR
049800970225     C*****************************************************************
049900970225     C* DETERMINAZIONE BOLLETTATO IN SOSPENSIONE
050000970225     C*****************************************************************
050100970225     C     ELASBO        BEGSR
050200970225     C                   MOVE      'S'           WSOS
050300090911     C     KSBO          SETLL     FiSBO02L
050400090911     C     KSBO          READE     FiSBO02L                               31
050500970225     C     *IN31         DOWEQ     *OFF
050600090930     C* H1030
050700090930     C                   ADD       SBOShA        BSS(1)
050800090930     C                   ADD       SBOShF        BSS(1)
050900090930     C* ESPR
051000090930     C                   ADD       SBOSEA        BSS(2)
051100090930     C                   ADD       SBOSEF        BSS(2)
051200970225     C* CORR
051300090930     C                   ADD       SBOSNA        BSS(3)
051400090930     C                   ADD       SBOSNF        BSS(3)
051500970305     C* DIST
051600090930     C                   ADD       SBOSSA        BSS(4)
051700090930     C                   ADD       SBOSSF        BSS(4)
051800970225     C*
051900090911     C     KSBO          READE     FiSBO02L                               31
052000970225     C                   ENDDO
052100970225     C* CALCOLO ANCHE TOTALE BOLLETTATO NEL GIORNO
052200970225     C                   MOVE      ' '           WSOS
052300970226     C                   CLEAR                   WTSBO
052400090911     C     KSBO          SETLL     FiSBO02L
052500090911     C     KSBO          READE     Fisbo02L                               31
052600970225     C     *IN31         DOWEQ     *OFF
052700970226     C                   MOVE      '1'           WTSBO             1
052800090930     C* H1030
052900090930     C                   ADD       SBOShA        BOL(1)
053000090930     C                   ADD       SBOShF        BOL(1)
053100090930     C* ESPR
053200090930     C                   ADD       SBOSEA        BOL(2)
053300090930     C                   ADD       SBOSEF        BOL(2)
053400970225     C* CORR
053500090930     C                   ADD       SBOSNA        BOL(3)
053600090930     C                   ADD       SBOSNF        BOL(3)
053700970305     C* DIST
053800090930     C                   ADD       SBOSSA        BOL(4)
053900090930     C                   ADD       SBOSSF        BOL(4)
054000970225     C*
054100090911     C     KSBO          READE     FiSBO02L                               31
054200970225     C                   ENDDO
054300970225     C                   ENDSR
054400970226     C*****************************************************************
054500970306     C* DETERMINAZIONE COLLI PARTITI CON PISTOLA 88
054600970226     C*****************************************************************
054700970226     C     ELASPA        BEGSR
054800970226     C                   CLEAR                   WTSPA
054900070510     c                   if        orgfil=prttfp
055000090915     C     KSPA          SETLL     FISPA02L
055100090915     C     KSPA          READE     FISPA02L                               31
055200060131     c                   else
055300060131     c* Per  l'estero divido i dati per p.o.
055400090915     C     KSPAfil       SETLL     FISPA01L
055500090915     C     KSPAfil       READE     FISPA01L                               31
055600060131     c                   endif
055700060131     c
055800000626    1C     *IN31         DOWEQ     *OFF
055900970305     C* ESCLUDO I TRANSITI
056000000626    2C     SPATRE        IFNE      'T'
056100000626     C* ESCLUDO IL P.O. PARTENZA POSTE
056200000626     C     SPALNP        LOOKUP    PT                                     32
056300060131     c
056400060131     c* Se da elaborare a parte escludo i p.o. esteri
056500070510     C  n32              if        orgfil=prttfp
056600060131     C     SPALNP        LOOKUP    EST                                    32
056700060131     c                   endif
056800060131     c
056900000626    3C     *IN32         IFEQ      *OFF
057000970226     C                   MOVE      '1'           WTSPA             1
057100990504     C*  PISTOLA 88
057200970226     C                   ADD       SPA88B        TP88B
057300970226     C                   ADD       SPA88A        TP88A
057400970226     C                   ADD       SPACPC        TOTPAR
057500970226     C                   ADD       SPACPE        TOTPAR
057600970226     C                   ADD       SPACPD        TOTPAR
057700090915     C                   ADD       SPACPh        TOTPAR
057800000626    3C                   END
057900000626    2C                   END
058000070510     c                   if        orgfil=prttfp
058100090915     C     KSPA          READE     FISPA02L                               31
058200060131     c                   else
058300090915     C     KSPAfil       READE     FISPA01L                               31
058400060131     c                   endif
058500060131     c
058600000626    1C                   ENDDO
058700970226     C                   ENDSR
058800970226     C*****************************************************************
058900970226     C* DETERMINAZIONE SPEDIZIONI A TERRA SPAREGGIATE
059000970226     C*****************************************************************
059100970226     C     ELASAT        BEGSR
059200970226     C                   CLEAR                   WTSAT
059300070510     c****               if        orgfil=prttfp or wdatel<§vpoqua
059400070510     c                   if        orgfil=prttfp
059500090916     C     KSAT          SETLL     fisat03L
059600090916     C     KSAT          READE     fisat03L                               31
059700060131     c                   else
059800090916     C     KSATfil       SETLL     fisat02L
059900090916     C     KSATfil       READE     fisat02L                               31
060000060131     c                   endif
060100000626    1C     *IN31         DOWEQ     *OFF
060200970226     C                   MOVE      '1'           WTSAT             1
060300000626     C* ESCLUDO I P.O. PARTENZA POSTE
060400000626     C     SATLNP        LOOKUP    PT                                     32
060500060131     c
060600060131     c* Se da elaborare a parte escludo i p.o. esteri
060700070510     c**n32              if        orgfil=prttfp  and wdatel>=§vpoqua
060800070510     c  n32              if        orgfil=prttfp
060900060131     C     SatLNP        LOOKUP    EST                                    32
061000060131     c                   endif
061100060131     c
061200000626    2C     *IN32         IFEQ      *OFF
061300090930     C* SPAREGGIATE H1030
061400091001     C     SATSDh        ADD       SATSnh        WTERR
061500090930     C     WTERR         SUB       SATSch        WSPAR
061600970226     C                   ADD       WSPAR         SPA(1)
061700090930     C* SPAREGGIATE ESPRESSO
061800090930     C     SATSDE        ADD       SATSNE        WTERR
061900090930     C     WTERR         SUB       SATSCE        WSPAR
062000090930     C                   ADD       WSPAR         SPA(2)
062100970305     C* SPAREGGIATE CORRIERE
062200970305     C     SATSDC        ADD       SATSNC        WTERR
062300970305     C     WTERR         SUB       SATSCC        WSPAR
062400090930     C                   ADD       WSPAR         SPA(3)
062500970226     C* SPAREGGIATE DISTRIBUZIONE
062600970226     C     SATSDD        ADD       SATSND        WTERR
062700970226     C     WTERR         SUB       SATSCD        WSPAR
062800090930     C                   ADD       WSPAR         SPA(4)
062900970306     C*
063000970306     C* SPEDIZIONI FUORI AFFIDABILITA'
063100090930     C                   ADD       SATSFH        FAF(1)
063200090930     C                   ADD       SATSFE        FAF(2)
063300090930     C                   ADD       SATSFC        FAF(3)
063400090930     C                   ADD       SATSFD        FAF(4)
063500970306     C* TOTALE SPEDIZIONI DA PARTIRE
063600090930     C                   ADD       SATSPh        SDA(1)
063700090930     C                   ADD       SATSPE        SDA(2)
063800090930     C                   ADD       SATSPC        SDA(3)
063900090930     C                   ADD       SATSPD        SDA(4)
064000000626     C                   ENDIF
064100970306     C*
064200070510     c****               if        orgfil=prttfp or wdatel<§vpoqua
064300070510     c                   if        orgfil=prttfp
064400090916     C     KSAT          READE     fisat03L                               31
064500060131     c                   else
064600090916     C     KSATfil       READE     fisat02L                               31
064700060131     c                   endif
064800970226     C                   ENDDO
064900970311     C* SE NON TROVATO RECORD CERCO QUELLO FESTIVO
065000990504     C***        WTSAT     IFEQ *BLANKS
065100990504     C***                  MOVEL'F'       WTRE
065200090916     C***        KSAT2     CHAINfisat03L             31
065300990504     C**N31                MOVE '1'       WTSAT
065400990504     C***                  END
065500970226     C                   ENDSR
065600970606     C*****************************************************************
065700970606     C* DETERMINAZIONE COLLI PARTITI NON ARRIVATI AL TERMINAL
065800970606     C*****************************************************************
065900970606     C     ELASAR        BEGSR
066000970606     C                   CLEAR                   WTSAR
066100990504     C******
066200090930     C* N.B.: QUESTA ROUTINE PRIMA LEGGEVA FISAR05L ORDINATO
066300090930     C* PER LNA+TRE+DRE. ORA LEGGE FISAR06L ORDINATO PER TFA+TRE+DRE
066400990504     C* E' COMUNQUE STATA MANTENUTA LA STRUTTURA ORIGINARIA NEL
066500990504     C* CASO SI VOLESSE RITORNARE AD ATTRIBUIRE COLLI PARTITI NON
066600990504     C* SPUNTATI IN ARRIVO AL RELATIVO P.O. ANZICHE' AL TERMINAL DI ARR
066700990504     C* ANCHE I COMMENTI SOTTO RIPORTATI SI RIFERISCONO ALLA STRUTTURA
066800990504     C* ORIGINARIA DELLA ROUTINE
066900990504     C******
067000970606     C* * * * * * * * * * * * *
067100970606     C* PRIMO CICLO DI LETTURA:
067200970606     C* Leggo records di tipo 'R' cioè del terminal-arr se su altro AS
067300970606     C* Di questi records non mi interessa la colonna dei colli non
067400970606     C* spuntati in partenza e nemmeno la colonna dei colli arrivati
067500970606     C* (Faccio una read anzichè chain perchè se ci fossero delle ecce-
067600970606     C* zioni terminal di arrivo potrei avere più di un record)
067700970606     C* * * * * * * * * * * * *
067800100701     c* ES 1/07/10 --> non si dovrebbero più creare
067900100701     C                   MOVEL     'R'           WTRE
068000100701     C     KSAR          SETLL     FISAR06L
068100100701     C     KSAR          READE     FISAR06L                               31
068200100701     C     *IN31         DOWEQ     *OFF
068300100701     C                   MOVE      '1'           WTSAR             1
068400970606     C                   ADD       SARPAR        TOTNAR
068500090930     C     KSAR          READE     FISAR06L                               31
068600970606     C                   ENDDO
068700970606     C* SECONDO CICLO DI LETTURA:
068800970606     C* Leggo records di tipo ' '
068900970606     C* Sommo i colli partiti non spuntati in arrivo e sottraggo i
069000970606     C* colli non spuntati in partenza e non arrivati
069100970606     C* (Se la lna avesse il terminal su altro As i colli partiti e
069200970606     C* non spuntati in arrivo sarà probabilmente = 0 su questo tipo
069300970606     C* record ma potrebbe anche essere > 0 se ci fossero eccezioni
069400970606     C* al terminal di arrivo)
069500970606     C* * * * * * * * * * * * *
069600970606     C                   MOVEL     ' '           WTRE
069700100701     c                   clear                   wlna              3 0
069800090930     C     KSAR          SETLL     FISAR06L
069900090930     C     KSAR          READE     FISAR06L                               31
070000970606     C     *IN31         DOWEQ     *OFF
070100100701     c
070200100701     c* Escludo i record "doppi"  avvero scritti sia dal terminal di arrivo
070300100701     c*  che di partenza, quando la fil ha i 2 terminal diversi o il giorno
070400100701     c*  che la fil cambia terminal di arrivo
070500100701     c                   if        wlna=0 or wlna<>sarlna
070600970606     C                   MOVE      '1'           WTSAR             1
070700970606     C                   ADD       SARPAR        TOTNAR
070800970606     C                   SUB       SARP88        TOTNAR
070900970606     C                   ADD       SARCLL        TOTARR
071000100701     c                   eval      wlna=sarlna
071100100701     c                   endif
071200100701     c
071300090930     C     KSAR          READE     FISAR06L                               31
071400970606     C                   ENDDO
071500970606     C* TERZO CICLO DI LETTURA:
071600970606     C* Leggo records di tipo 'T' cioè relativi ai colli in TRANSITO
071700970606     C* Sommo i colli partiti non spuntati in arrivo e sottraggo i
071800970606     C* colli non spuntati in partenza e non arrivati
071900970606     C* (Questo ciclo lo faccio solo se la filiale che sto elaborando
072000970606     C* è un primo livello)
072100970606     C* * * * * * * * * * * * *
072200990504     C     ORGFIL        IFEQ      PRTTFP
072300970606     C                   MOVEL     'T'           WTRE
072400090930     C     KSAR          SETLL     FISAR06L
072500090930     C     KSAR          READE     FISAR06L                               31
072600970606     C     *IN31         DOWEQ     *OFF
072700970606     C                   MOVE      '1'           WTSAR             1
072800970606     C                   ADD       SARPAR        TOTNAR
072900970606     C                   SUB       SARP88        TOTNAR
073000970606     C                   ADD       SARCLL        TOTARR
073100090930     C     KSAR          READE     FiSAR06L                               31
073200970606     C                   ENDDO
073300990504     C                   END
073400970610     C*
073500970610     C     TOTNAR        IFLT      *ZEROS
073600970610     C                   MOVE      *ZEROS        TOTNAR
073700970610     C                   END
073800970606     C                   ENDSR
073900970226     C*****************************************************************
074000970226     C* DETERMINAZIONE SPEDIZIONI NON CONSEGNATE FUORI AFFIDABILITA'
074100970226     C*****************************************************************
074200970226     C     ELASCO        BEGSR
074300970226     C* 1° CICLO: LETTURA "NON CONSEGNATE"
074400970226     C                   MOVEL     'N'           WSCO
074500970226    1C                   DO        4             II                2 0
074600970226     C* Giacenze in ritardo           GBO=G,ORE>0
074700970226     C* lasciati avviso in ritardo    GBO=L,ORE>0
074800970226     c* chiuso per turno in ritardo   GBO=C,ORE>0
074900970226     c* non fatte in ritardo+conformi GBO=N,ORE>=0
075000970226     C                   MOVEL     GBO(II)       WGBO
075100000208     C     KSCO          SETLL     FNSCO03L
075200000208     C     KSCO          READE     FNSCO03L                               31
075300970226    2C     *IN31         DOWEQ     *OFF
075400970226     C* escludo se olt = 'A' o 'T'
075500970226    3C     SCOOLT        IFNE      'A'
075600970226     C     SCOOLT        ANDNE     'T'
075700970226     c*
075800970226    4C     SCOORE        IFGT      *ZEROS
075900970226     C     SCOORE        OREQ      *ZEROS
076000970226     C     GBO(II)       ANDEQ     'N'
076100090930     C                   ADD       SCOSS4        NOC(1)
076200090930     C                   ADD       SCOSS1        NOC(2)
076300090930     C                   ADD       SCOSS2        NOC(3)
076400090930     C                   ADD       SCOSS3        NOC(4)
076500970226    4C                   END
076600970226    3C                   END
076700000208     C     KSCO          READE     FNSCO03L                               31
076800970226    2C                   ENDDO
076900970226    1C                   ENDDO
077000970226     C*
077100970226     C* 2° CICLO: LETTURA "IN DISTINTA"
077200970226     C                   MOVEL     'D'           WSCO
077300970226     C                   MOVE      *BLANKS       WGBO
077400000208     C     KSCO          SETLL     FNSCO03L
077500000208     C     KSCO          READE     FNSCO03L                               31
077600970226    1C     *IN31         DOWEQ     *OFF
077700970226     C* escludo se olt = 'A' o 'T'
077800970226    2C     SCOOLT        IFNE      'A'
077900970226     C     SCOOLT        ANDNE     'T'
078000010124     C     SCOORE        ANDGE     *ZEROS
078100090930     C                   ADD       SCOSS4        NOC(1)
078200090930     C                   ADD       SCOSS1        NOC(2)
078300090930     C                   ADD       SCOSS2        NOC(3)
078400090930     C                   ADD       SCOSS3        NOC(4)
078500010124     C* Memorizzo anche il" Di cui in distinta conforme"
078600010124     C* e il "Di cui in distinta in ritardo"
078700010124     C     SCOORE        IFEQ      *ZEROS
078800090930     C                   ADD       SCOSS4        DSC(1)
078900090930     C                   ADD       SCOSS1        DSC(2)
079000090930     C                   ADD       SCOSS2        DSC(3)
079100090930     C                   ADD       SCOSS3        DSC(4)
079200010124     C                   ELSE
079300090930     C                   ADD       SCOSS4        DSR(1)
079400090930     C                   ADD       SCOSS1        DSR(2)
079500090930     C                   ADD       SCOSS2        DSR(3)
079600090930     C                   ADD       SCOSS3        DSR(4)
079700010124     C                   END
079800970226    2C                   END
079900000208     C     KSCO          READE     FNSCO03L                               31
080000970226    1C                   ENDDO
080100970226     C* 3° CICLO: LETTURA "NON IN CONSEGNA"
080200970226     C                   MOVEL     'O'           WSCO
080300970226     C                   MOVE      *BLANKS       WGBO
080400000208     C     KSCO          SETLL     FNSCO03L
080500000208     C     KSCO          READE     FNSCO03L                               31
080600970226    1C     *IN31         DOWEQ     *OFF
080700970226     C* escludo se olt = 'A' o 'T'
080800970226    2C     SCOOLT        IFNE      'A'
080900970226     C     SCOOLT        ANDNE     'T'
081000970226     C     SCOORE        ANDGE     *ZEROS
081100090930     C                   ADD       SCOSS4        NOC(1)
081200090930     C                   ADD       SCOSS1        NOC(2)
081300090930     C                   ADD       SCOSS2        NOC(3)
081400090930     C                   ADD       SCOSS3        NOC(4)
081500970226    2C                   END
081600000208     C     KSCO          READE     FNSCO03L                               31
081700970226    1C                   ENDDO
081800970226     C* 4° CICLO: LETTURA "CONSEGNABILI"
081900970226     C                   CLEAR                   WTSCO
082000970226     C                   MOVEL     'B'           WSCO
082100970226     C                   MOVE      *BLANKS       WGBO
082200000208     C     KSCO          SETLL     FNSCO03L
082300000208     C     KSCO          READE     FNSCO03L                               31
082400970226    1C     *IN31         DOWEQ     *OFF
082500970226     C* seleziono olt = 'T'
082600970226     C     SCOOLT        IFEQ      'T'
082700970226     C                   MOVE      '1'           WTSCO             1
082800090930     C                   ADD       SCOSS4        CSB(1)
082900090930     C                   ADD       SCOSS1        CSB(2)
083000090930     C                   ADD       SCOSS2        CSB(3)
083100090930     C                   ADD       SCOSS3        CSB(4)
083200970226    2C                   END
083300000208     C     KSCO          READE     FNSCO03L                               31
083400970226    1C                   ENDDO
083500970226     C*
083600970226     C     WTSCO         IFEQ      *BLANKS
083700970226     C                   MOVE      'F'           WSCO
083800970226     C                   MOVE      *BLANKS       WGBO
083900000208     C     KSCO          CHAIN     FNSCO03L                           31
084000970226     C     *IN31         IFEQ      *OFF
084100970226     C                   MOVE      '1'           WTSCO
084200970226     C                   END
084300970226     C                   END
084400970226     C*
084500970226     C                   ENDSR
084600990409     C*****************************************************************
084700990409     C* RICERCA DATA IDD-1 X I P.O. CHE LO FANNO IL GG DOPO
084800990409     C*****************************************************************
084900990409     C     RICDAT        BEGSR
085000990409     C* UTILIZZO IL CALENDARIO
085100990409     C                   MOVE      WDATEL        G08INV
085200990409     C                   MOVEL     '3'           G08ERR
085300990409     C                   CALL      'XSRDA8'
085400990409     C                   PARM                    WLBDA8
085500990409     C*
085600990409     C                   MOVEL     'S'           WFEST             1
085700990409     C                   CLEAR                   KTFP
085800990409     C* CERCO IL PRIMO GIORNO LAVORATIVO PRECEDENTE
085900990409     C     WFEST         DOWEQ     'S'
086000990409     C                   SUB       1             G08TGI
086100990409     C                   Z-ADD     G08TGI        GI8TGI
086200990409     C                   CALL      'XSRGI8'
086300990409     C                   PARM                    WGIDAT
086400990409     C                   Z-ADD     GI8INV        WDATAM
086500990409     C* IMPOSTO ANNO E MESE
086600990409     C                   Z-ADD     WAA           KANN
086700990409     C                   Z-ADD     WMM           KMES
086800990409     C*
086900990409     C                   CLEAR                   WFEST
087000990409     C     KCLN          CHAIN     AZCLN01L                           31
087100990409    1C     *IN31         IFEQ      *OFF
087200990511     C                   MOVEA     CLNPOM        POM
087300990511   1AC     POM(WGG)      IFNE      *BLANKS
087400990409     C                   MOVEL     '1Y'          KCOD
087500990409     C                   MOVEL(P)  POM(WGG)      KKEY
087600990409     C     KTAB          CHAIN     TABEL                              31
087700990409    2C     *IN31         IFEQ      *OFF
087800990409     C                   MOVEL     TBLUNI        DS1Y
087900990409    3C     §1YFEI        IFEQ      'S'
088000990409     C                   MOVEL     'S'           WFEST
088100990409    3C                   ENDIF
088200990409    2C                   ENDIF
088300990511   1AC                   ENDIF
088400990511    1C                   ENDIF
088500990409     C                   ENDDO
088600990409     C                   ENDSR
088700990409     C*****************************************************************
088800990409     C* DETERMINAZIONE SE ELABORATO IDD O MENO
088900990409     C*****************************************************************
089000990409     C     ELAIDD        BEGSR
089100990409     C                   SETOFF                                       18
089200990409     C* SE NON FA L'IDD LASCIO IN BIANCO
089300050831    1C     §OGNID        IFEQ      ' '
089400050831     c
089500050906     c* Escludo i p.o. esteri e la 101
089600050831    2c                   if        §ogntw<>'EEX' and §ogntw<>'EUP' and
089700050906     c                             §ogntw<>'DPD' and §ogntw<>'FED' and
089800050906     c                             orgfil<>101
089900050831     c
090000990409     C                   Z-ADD     95            KCAA
090100990409     C* PER I P.O. CHE FANNO L'IDD IL GIORNO DOPO
090200990409     C*  CERCO LA DATA PRECEDENTE
090300990409     C                   EXSR      RICDAT
090400990409     C*
090500990409     C                   MOVEL     ORGFIL        KLNA
090600990409     C* IMPOSTO LA DATA
090700990409     C     §OGEID        IFEQ      ' '
090800990409     C                   Z-ADD     WDATEL        KDAO
090900990409     C                   ELSE
091000990409     C                   Z-ADD     WDATAM        KDAO
091100990409     C                   ENDIF
091200050831     c
091300050831     c* Escludo i p.o. in £6
091400050831     C                   MOVEL     '6'           D55TPT
091500050831     C                   MOVEL     ORGFIL        D55LIN
091600050831     C                   Z-ADD     kdao          D55DRF
091700050831     C                   CALL      'FNLV55R'
091800050831     C                   PARM                    DSLV55
091900050831     c
092000050831     c* P.o. gestito--> accendo il 18 come p.o. che non fa mai l'idd
092100050831     c                   if        d55tfa<>orgfil
092200050831     C                   SETON                                        18
092300050831     c                   else
092400990409     C*
092500990520     C     KANM4         SETLL     FNANM04L                               31
092600990409     C* NON C'E' ANOMALIA
092700990409    6C     *IN31         IFEQ      *OFF
092800990409     C                   SETON                                        17
092900990409     C                   ELSE
093000990409     C                   SETOFF                                       17
093100990409     C                   ENDIF
093200050831     C                   ENDIF
093300050831     c
093400050831   x2C                   ELSE
093500050831     C                   SETON                                        18
093600050831    2C                   ENDIF
093700990409     C*
093800050831   x1C                   ELSE
093900990409     C                   SETON                                        18
094000050831    1C                   ENDIF
094100990409     C                   ENDSR
094200970225     C*****************************************************************
094300970225     C* STAMPA DATI DEL PUNTO OPERATIVO
094400970225     C*****************************************************************
094500970225     C     PRTLNP        BEGSR
094600970225      * ------------------ PRIMA RIGA, TOTALI DI FILIALE --------------
094700970225     C*
094800970225     C                   SETON                                        40
094900970527     C*
095000970527     C     *IN05         IFEQ      *ON
095100990517     C                   MOVE      WDATEL        PRTGG
095200990517     C                   MOVE      WDATEL        W0040             4 0
095300970527     C                   MOVEL     W0040         PRTMM
095400970527     C                   END
095500970226     C* VERIFICO SE DATI NON PERVENUTI
095600970226     C     WTSPA         COMP      *BLANKS                                11
095700970226     C     WTSAT         COMP      *BLANKS                                12
095800970226     C     WTSBO         COMP      *BLANKS                                13
095900970226     C     WTSCO         COMP      *BLANKS                                14
096000050831     C**   WT300         COMP      *BLANKS                                15
096100970606     C     WTSAR         COMP      *BLANKS                                16
096200050831     c                   setoff                                       15
096300970226     C*
096400990505     C* PULISCO I CAMPI DI STAMPA CHE POTREBBERO RIMANERE SPORCHI
096500990505     C                   MOVE      *ZEROS        PRT88B
096600990505     C                   MOVE      *ZEROS        PRT88P
096700990505     C                   MOVE      *ZEROS        PRT88A
096800990505     C                   MOVE      *ZEROS        PRT88C
096900990505     C                   MOVE      *ZEROS        PRTFAF
097000990505     C                   MOVE      *ZEROS        PRTFAP
097100990505     C                   MOVE      *ZEROS        PRTSSP
097200010123     C**                   MOVE *ZEROS    PRTNAR
097300010123     C**                   MOVE *ZEROS    PRTNRP
097400990505     C                   MOVE      *ZEROS        SDA123
097500990505     C*
097600060131     c* Stampo per terminal e p.o. esteri, se <> da '2'
097700060131    1C***  ORGFIL        IFEQ      PRTTFP
097800060131    1C**   *IN11         ANDEQ     *OFF
097900060131     c                   if        wtspa<>'2'
098000970226     C* SPUNTE PISTOLA 88
098100970226     C* Disk B
098200970226     C                   Z-ADD     TP88B         PRT88B
098300970226     C                   MOVE      *ZEROS        PRT88P
098400970226    2C     TOTPAR        IFGT      *ZEROS
098500970226     C     PRT88B        DIV(H)    TOTPAR        $XX               7 4
098600970226     C     $XX           MULT      100           PRT88P
098700970226    2C                   END
098800970226     C* Altri
098900970226     C                   Z-ADD     TP88A         PRT88A
099000970226     C                   MOVE      *ZEROS        PRT88C
099100970226    2C     TOTPAR        IFGT      *ZEROS
099200970226     C     PRT88A        DIV(H)    TOTPAR        $XX               7 4
099300970226     C     $XX           MULT      100           PRT88C
099400970226    2C                   END
099500970226    1C                   END
099600970225     C* BOLLETTATO IN SOSPENSIONE
099700970227    1C     *IN13         IFEQ      *OFF
099800970225     C                   XFOOT     BOL           TOTBOL
099900970225     C                   XFOOT     BSS           PRTBSS
100000970226     C                   MOVE      *ZEROS        PRTBSP
100100970227    2C     TOTBOL        IFGT      *ZEROS
100200970225     C     PRTBSS        DIV(H)    TOTBOL        $XX               7 4
100300970226     C     $XX           MULT      100           PRTBSP
100400970227    2C                   END
100500970227    1C                   END
100600970306     C* SPEDIZIONI A TERRA
100700060131     c* Stampo per terminal e p.o. esteri, se <> da '2'
100800060131    1C**   ORGFIL        IFEQ      PRTTFP
100900060131    1C**   *IN12         ANDEQ     *OFF
101000060131     c                   if        wtsat<>'2'
101100970306     C* ... FUORI AFFIDABILITA'
101200970306     C                   XFOOT     SDA           SDA123
101300970306     C                   XFOOT     FAF           PRTFAF
101400970306     C                   MOVE      *ZEROS        PRTFAP
101500970306    2C     SDA123        IFGT      *ZEROS
101600970306     C     PRTFAF        DIV(H)    SDA123        $XX               7 4
101700970306     C     $XX           MULT      100           PRTFAP
101800970306    2C                   END
101900970306     C* ... SPAREGGIATE
102000970306     C                   XFOOT     SPA           PRTSSP
102100970306    1C                   END
102200970606     C*
102300970606     C* COLLI PARTITI E NON SPUNTATI DAL TERMINAL DI ARRIVO
102400990504    1C     WTARR         IFEQ      '1'
102500990504    1C     *IN16         ANDEQ     *OFF
102600010123     C**                   Z-ADDTOTNAR    PRTNAR
102700010123     C**                   MOVE *ZEROS    PRTNRP
102800010123    2C**         TOTARR    IFGT *ZEROS
102900010123     C**         PRTNAR    DIV  TOTARR    $XX     74H
103000010123     C**         $XX       MULT 100       PRTNRP
103100010123    2C**                   END
103200970606    1C                   END
103300970606     C*
103400010124     C* SPEDIZIONI NON CONSEGNATE fuori affidabilita'
103500970227    1C     *IN14         IFEQ      *OFF
103600970226     C                   XFOOT     CSB           CSB123
103700970226     C                   XFOOT     NOC           PRTNCN
103800970226     C                   MOVE      *ZEROS        PRTNCP
103900970227    2C     CSB123        IFGT      *ZEROS
104000970226     C     PRTNCN        DIV(H)    CSB123        $XX               7 4
104100970226     C     $XX           MULT      100           PRTNCP
104200970227    2C                   END
104300010124     C                   XFOOT     DSC           PRTDSC
104400010124     C                   XFOOT     DSR           PRTDSR
104500970227    1C                   END
104600970226     C* RITIRI NON ESEGUITI
104700050831     C**N15              Z-ADD     TOTRIT        PRTRIT
104800970226     C* MANCA BAM
104900970226     C                   Z-ADD     TOTBAM        PRTBAM
105000970226     C* DISGUIDI
105100970226     C                   Z-ADD     TOTDIS        PRTDIS
105200040405     C                   Z-ADD     TOTDISP       PRTDISP
105300970226     C* DISGUIDI IN AREA
105400970226     C                   Z-ADD     TOTDAR        PRTDIA
105500990503     C*
105600990503     C* SE RICHIESTO SCARICO DATI SU FILE DI WORK
105700990503     C     V1CCRP        IFEQ      'S'
105800990503     C                   EXSR      RIERCQ
105900990503     C                   END
106000990503     C*
106100970225     C*
106200990623     C     WBRKPE        IFEQ      '1'
106300970528     C     *IN05         OREQ      *ON
106400970528     C     V1CTSP        ANDEQ     'N'
106500970227     C                   ADD       1             $RIGHE
106600970227     C                   ELSE
106700970227     C                   ADD       2             $RIGHE
106800970227     C                   END
106900970401     C*
107000970401     C*
107100970227     C*
107200990621     C     $RIGHE        IFGT      60
107300970602     C     *IN05         ANDEQ     *OFF
107400990621     C     $RIGHE        ORGT      60
107500970602     C     *IN05         ANDEQ     *ON
107600970602     C     V1CTSP        ANDEQ     'N'
107700970602     C     $RIGHE        ORGT      58
107800970602     C     *IN05         ANDEQ     *ON
107900970602     C     V1CTSP        ANDEQ     'S'
108000970227     C                   WRITE     LV66T1
108100970227     C                   WRITE     LV66T3
108200970227     C                   WRITE     LV66T4
108300970227     C                   WRITE     LV66T3
108400970227     C                   Z-ADD     11            $RIGHE
108500990623     C                   MOVE      '1'           WBRKPE
108600970227    1C                   END
108700970227     C* Stampo la riga vuota solo se non ho appena stampato la riga
108800970528     C* Distretto/area
108900970528     C* Se la stampa non è dettagliata per tipo servizio ed è stato
109000970528     C* richiesto un periodo dal/al non stampo MAI la riga vuota
109100990623     C     WBRKPE        IFEQ      *BLANKS
109200970528     C     *IN05         ANDEQ     *OFF
109300990623     C     WBRKPE        OREQ      *BLANKS
109400970528     C     *IN05         ANDEQ     *ON
109500970528     C     V1CTSP        ANDEQ     'S'
109600970227     C                   WRITE     LV66T2
109700970227     C                   END
109800970227     C*
109900970227     C                   WRITE     LV66D1
110000990429     c* memorizzo che ho stampato almeno una riga per l'area
110100990429     C                   MOVE      '1'           WPRT              1
110200970527     C*
110300970527     C* MEMORIZZO I TOTALI STAMPATI IN CAMPI PER STAMPA TOTALI DI
110400970527     C* FILIALE
110500970527     C     *IN05         IFEQ      *ON
110600970527     C* ACCENDO *IN06 PER NON STAMPARE COD.+DECOD. DEL PUNTO OPERATIVO
110700970527     C* NELLE RIGHE SUCCESSIVE
110800970527     C                   SETON                                        06
110900970527     C  N13              ADD       PRTBSS        TOFBSS
111000970527     C  N13              ADD       TOTBOL        TOFBOL            8 0
111100970527     C  N11              ADD       TP88B         TOF88B
111200970527     C  N11              ADD       TOTPAR        TOFPAR            8 0
111300970527     C  N11              ADD       TP88A         TOF88A
111400970527     C  N12              ADD       PRTFAF        TOFFAF
111500970527     C  N12              ADD       SDA123        TOFSDA            8 0
111600970527     C  N12              ADD       PRTSSP        TOFSSP
111700970527     C                   ADD       TOTDIS        TOFDIS
111800040405     C                   ADD       TOTDISP       TOFDISP
111900970527     C                   ADD       TOTBAM        TOFBAM
112000970527     C  N14              ADD       PRTNCN        TOFNCN
112100970527     C  N14              ADD       CSB123        TOFCSB           10 0
112200010124     C  N14              ADD       PRTDSC        TOFDSC
112300010124     C  N14              ADD       PRTDSR        TOFDSR
112400970527     C                   ADD       TOTDAR        TOFDIA
112500970527     C                   END
112600990429     C* MEMORIZZO I TOTALI STAMPATI IN CAMPI PER STAMPA TOTALI DI
112700990505     C* AREA (SE NON E' STATA RICHIESTO UN P.O. SPECIFICO)
112800990505     C     *IN01         IFEQ      *OFF
112900990429     C                   SELECT
113000990429     C* NETWORK BARTOLINI
113100990429     C     COMDIT        WHENEQ    'B'
113200990430     C                   Z-ADD     2             NT
113300990430     C                   Z-ADD     1             NA                1 0
113400990430     C     COMDIT        WHENEQ    'S'
113500990430     C                   Z-ADD     3             NT
113600990430     C                   Z-ADD     1             NA
113700990430     C                   ENDSL
113800990430     C*
113900990505     C*
114000990430     C  N13              ADD       PRTBSS        SOS(NT)
114100990505     C  N13              ADD       TOTBOL        TBL(NT)
114200990430     C  N11              ADD       TP88B         B88(NT)
114300990505     C  N11              ADD       TOTPAR        PAR(NT)
114400990430     C  N11              ADD       TP88A         A88(NT)
114500990430     C  N12              ADD       PRTFAF        SFA(NT)
114600990505     C  N12              ADD       SDA123        TDA(NT)
114700990430     C  N12              ADD       PRTSSP        SSP(NT)
114800990430     C                   ADD       TOTDIS        DIS(NT)
114900040405     C                   ADD       TOTDISP       DISP(NT)
115000990430     C                   ADD       TOTBAM        DDT(NT)
115100990430     C  N14              ADD       PRTNCN        NCN(NT)
115200990505     C  N14              ADD       CSB123        SCS(NT)
115300010124     C  N14              ADD       PRTDSC        DCN(NT)
115400010124     C  N14              ADD       PRTDSR        DRT(NT)
115500990430     C                   ADD       TOTDAR        DIA(NT)
115600990429     C* MEMORIZZO I TOTALI PER LA STAMPA DEI TOTALI GENERALI
115700990430     C  N13              ADD       PRTBSS        SOS(NA)
115800990505     C  N13              ADD       TOTBOL        TBL(NA)
115900990430     C  N11              ADD       TP88B         B88(NA)
116000990505     C  N11              ADD       TOTPAR        PAR(NA)
116100990430     C  N11              ADD       TP88A         A88(NA)
116200990430     C  N12              ADD       PRTFAF        SFA(NA)
116300990505     C  N12              ADD       SDA123        TDA(NA)
116400990430     C  N12              ADD       PRTSSP        SSP(NA)
116500990430     C                   ADD       TOTDIS        DIS(NA)
116600040405     C                   ADD       TOTDISP       DISP(NA)
116700990430     C                   ADD       TOTBAM        DDT(NA)
116800990430     C  N14              ADD       PRTNCN        NCN(NA)
116900990505     C  N14              ADD       CSB123        SCS(NA)
117000010124     C  N14              ADD       PRTDSC        DCN(NA)
117100010124     C  N14              ADD       PRTDSR        DRT(NA)
117200990430     C                   ADD       TOTDAR        DIA(NA)
117300990505     C                   END
117400970527     C*
117500970225      *
117600090930      * ------------------ ESPR./DIST./CORR./H1030 --------------------
117700970225      *
117800970407     C* Il dettaglio per tipo servizio lo stampo solo se richiesto
117900970407   1AC     V1CTSP        IFEQ      'S'
118000970225     C                   SETOFF                                       40
118100091001     c  n10              z-add     2             $v
118200091001     c   10              z-add     1             $v
118300091001     c
118400091001    1C     $v            DOwle     4
118500970225     C                   MOVE      ENS($V)       TIPOP
118600970225     C* BOLLETTATO IN SOSPENSIONE
118700970227    2C     *IN13         IFEQ      *OFF
118800970225     C                   Z-ADD     BSS($V)       PRTBSS
118900970226     C                   MOVE      *ZEROS        PRTBSP
119000970227    3C     BOL($V)       IFGT      *ZEROS
119100970225     C     BSS($V)       DIV(H)    BOL($V)       $XX               7 4
119200970226     C     $XX           MULT      100           PRTBSP
119300970227    3C                   END
119400970227    2C                   END
119500970306     C* SPEDIZIONI A TERRA
119600970306     C* ... FUORI AFFIDABILITA'
119700970306    2C     *IN12         IFEQ      *OFF
119800970226     C                   Z-ADD     FAF($V)       PRTFAF
119900970226     C                   MOVE      *ZEROS        PRTFAP
120000970227    3C     SDA($V)       IFGT      *ZEROS
120100970226     C     FAF($V)       DIV(H)    SDA($V)       $XX               7 4
120200970226     C     $XX           MULT      100           PRTFAP
120300970227    3C                   END
120400970306     C* ... SPAREGGIATE
120500970306     C                   Z-ADD     SPA($V)       PRTSSP
120600970306    2C                   END
120700970226     C* SPEDIZIONI NON CONSEGNATE
120800970227    2C     *IN14         IFEQ      *OFF
120900970226     C                   Z-ADD     NOC($V)       PRTNCN
121000970226     C                   MOVE      *ZEROS        PRTNCP
121100970227    3C     CSB($V)       IFGT      *ZEROS
121200970226     C     PRTNCN        DIV(H)    CSB($V)       $XX               7 4
121300970226     C     $XX           MULT      100           PRTNCP
121400970227    3C                   END
121500010124     C                   Z-ADD     DSC($V)       PRTDSC
121600010124     C                   Z-ADD     DSR($V)       PRTDSR
121700970227    2C                   END
121800970225     C*
121900970227     C                   ADD       1             $RIGHE
122000970226     C                   WRITE     LV66D1
122100970401     C*
122200091001     c                   eval      $v=$v+1
122300970227    1C                   ENDDO
122400970407   1AC                   END
122500970226     C*
122600970226     C                   ENDSR
122700990503     C*****************************************************************
122800990503     C* CREAZIONE DATI REPORT SU FILE DI DATA BASE
122900990503     C*****************************************************************
123000990503     C     RIERCQ        BEGSR
123100990504     C                   CLEAR                   WFRCQ000
123200990504     C                   MOVE      PRTFL3        RCQDIV
123300990503     C                   Z-ADD     PRTCAR        RCQARE
123400990503     C                   Z-ADD     ORGFIL        RCQFIL
123500990503     C                   MOVEL     ORGDIT        RCQNTW
123600990503     C                   Z-ADD     WDATEL        RCQDAT
123700990504     C  N17
123800990504     CANN18              Z-ADD     1             RCQIDD
123900990504     C   17
124000990504     CANN18              Z-ADD     0             RCQIDD
124100010124     C   17
124200010124     CANN18              MOVEL     'N'           RCQIDN
124300990504     C   18              Z-ADD     9             RCQIDD
124400050831     C**N15              Z-ADD     PRTRIT        RCQRIT
124500050831     C** 15              MOVEL     'N'           RCQRIN
124600050831     C                   MOVEL     'N'           RCQRIN
124700990504     C  N13              Z-ADD     PRTBSS        RCQBSS
124800990504     C  N13              Z-ADD     TOTBOL        RCQBOL
124900010124     C   13              MOVEL     'N'           RCQBON
125000990504     C  N11              Z-ADD     PRT88B        RCQ88B
125100990504     C  N11              Z-ADD     PRT88A        RCQ88A
125200990504     C  N11              Z-ADD     TOTPAR        RCQPAR
125300010124     C   11              MOVEL     'N'           RCQPAN
125400990504     C  N12              Z-ADD     PRTFAF        RCQFAF
125500990504     C  N12              Z-ADD     SDA123        RCQSDA
125600990504     C  N12              Z-ADD     PRTSSP        RCQSSP
125700010124     C   12              MOVEL     'N'           RCQSTN
125800990503     C                   Z-ADD     PRTDIS        RCQDIS
125900040405     C                   Z-ADD     PRTDISP       RCQDISP
126000990503     C                   Z-ADD     PRTBAM        RCQDDT
126100010123     C  N16              Z-ADD     TOTNAR        RCQNAR
126200990504     C  N16              Z-ADD     TOTARR        RCQARR
126300010124     C   16              MOVEL     'N'           RCQARN
126400990504     C  N14              Z-ADD     PRTNCN        RCQNCN
126500990504     C  N14              Z-ADD     CSB123        RCQCSB
126600010124     C   14              MOVEL     'N'           RCQCSN
126700990503     C                   Z-ADD     PRTDIA        RCQDIA
126800010124     C* DEVO LEGGERE IL FILE DELLE STATISTICHE CONSEGNE PER VALORIZZARE
126900010124     C* I CAMPI RESTANTI
127000010124     C                   EXSR      RIESCO
127100010124     C*
127200990503     C                   WRITE     WFRCQ000
127300990503     C                   ENDSR
127400010124     C*****************************************************************
127500010124     C* RIEMPIMENTO CAMPI DEL WORK FILE RELATIVI ALLA STAT.CONSEGNE
127600010124     C*****************************************************************
127700010124     C     RIESCO        BEGSR
127800010124     C     KSCO4         SETLL     FNSCO04L
127900010124     C     KSCO4         READE     FNSCO04L                               31
128000010124    1C     *IN31         DOWEQ     *OFF
128100010124     C* ESCLUDO TOTALI E FESTIVI
128200010124    2C     SCOOLT        IFNE      'T'
128300010124     C     SCOOLT        ANDNE     'F'
128400010124     C* VALORE ASSOLUTO DELLE ORE DI RITARDO
128500010124    3C     SCOORE        IFLT      *ZEROS
128600010124     C                   Z-SUB     SCOORE        WORRIT
128700010124    3C                   ENDIF
128800010124     C* GLI SCAGLIONI DI RITARDO/ANTICIPO DEL REPORT CONTROLLO QUALITÀ
128900010124     C* PER IL MOMENTO SONO GLI STESSI DELLA STATISTICA CONSEGNE MA IN
129000010124     C* FUTURO POTREBBERO CAMBIARE DAL MOMENTO CHE QUELLI DELLA STATIS-
129100010124     C* TICA CONSEGNE SONO TABELLATI. DEVO QUINDI DETERMINARE LO
129200010124     C* SCAGLIONE DI RITARDO/ANTICIPO
129300010124     C                   Z-ADD     1             YY                1 0
129400010124    3C     SCOORE        IFLT      *ZEROS
129500010124     C     WORRIT        LOOKUP    SCA(YY)                                32
129600010124    4C     *IN32         IFEQ      '0'
129700010124     C                   Z-ADD     1             YY
129800010124    4C                   ENDIF
129900010124    4C     SCOOLT        IFEQ      'A'
130000010124     C     YY            ANDGT     1
130100010124     C                   SUB       1             YY
130200010124    4C                   ENDIF
130300010124   X3C                   ELSE
130400010124    4C     SCOORE        IFGT      *ZEROS
130500010124     C     SCOORE        LOOKUP    SCR(YY)                                32
130600010124    5C     *IN32         IFEQ      '0'
130700010124     C                   Z-ADD     5             YY
130800010124    5C                   ENDIF
130900010124    5C     SCOOLT        IFEQ      'O'
131000010124     C     YY            ANDLT     5
131100010124     C                   ADD       1             YY
131200010124    5C                   ENDIF
131300010124    4C                   ENDIF
131400010124    3C                   ENDIF
131500010124     C*
131600010124    3C                   SELECT
131700010124     C***********************
131800010124     C* SPEDIZIONI CONSEGNATE
131900010124     C***********************
132000010124     C     SCOSCO        WHENEQ    'C'
132100010124    4C     SCOGBO        IFEQ      *BLANKS
132200010124     C* Anticipo
132300010124    5C     SCOORE        IFLT      *ZEROS
132400010124    6C                   SELECT
132500010124     C* Oltre 24 ore
132600010124     C     YY            WHENEQ    1
132700091002     C                   Z-ADD     SCOSS4        RCQCOh
132800091002     C                   Z-ADD     SCOSS1        RCQCOE
132900010124     C                   Z-ADD     SCOSS2        RCQCOC
133000010124     C                   ADD       SCOSS3        RCQCOC
133100010124     c* 24 ore
133200010124     C     YY            WHENEQ    2
133300091002     C                   Z-ADD     SCOSS4        RCQCAh
133400091002     C                   Z-ADD     SCOSS1        RCQCAE
133500010124     C                   Z-ADD     SCOSS2        RCQCAC
133600010124     C                   ADD       SCOSS3        RCQCAC
133700010124     C                   OTHER
133800010124     c* 12 ore
133900091002     C                   Z-ADD     SCOSS4        RCQCBh
134000091002     C                   Z-ADD     SCOSS1        RCQCBE
134100010124     C                   Z-ADD     SCOSS2        RCQCBC
134200010124     C                   ADD       SCOSS3        RCQCBC
134300010124    6C                   ENDSL
134400010124   X5C                   ELSE
134500010124     c* Conforme
134600010124    6C     SCOORE        IFEQ      *ZEROS
134700091002     C                   Z-ADD     SCOSS4        RCQC0h
134800091002     C                   Z-ADD     SCOSS1        RCQC0E
134900010124     C                   Z-ADD     SCOSS2        RCQC0C
135000010124     C                   ADD       SCOSS3        RCQC0C
135100010124   X6C                   ELSE
135200010124     c* Ritardo
135300010124    7C                   SELECT
135400010124     c* 12 ore
135500010124     C     YY            WHENEQ    1
135600091002     C                   Z-ADD     SCOSS4        RCQC1h
135700091002     C                   Z-ADD     SCOSS1        RCQC1E
135800010124     C                   Z-ADD     SCOSS2        RCQC1C
135900010124     C                   ADD       SCOSS3        RCQC1C
136000010124     c* 24 ore
136100010124     C     YY            WHENEQ    2
136200091002     C                   Z-ADD     SCOSS4        RCQC2h
136300091002     C                   Z-ADD     SCOSS1        RCQC2E
136400010124     C                   Z-ADD     SCOSS2        RCQC2C
136500010124     C                   ADD       SCOSS3        RCQC2C
136600010124     c* 36 ore
136700010124     C     YY            WHENEQ    3
136800091002     C                   Z-ADD     SCOSS4        RCQC3h
136900091002     C                   Z-ADD     SCOSS1        RCQC3E
137000010124     C                   Z-ADD     SCOSS2        RCQC3C
137100010124     C                   ADD       SCOSS3        RCQC3C
137200010124     c* 48 ore
137300010124     C     YY            WHENEQ    4
137400091002     C                   Z-ADD     SCOSS4        RCQC4h
137500091002     C                   Z-ADD     SCOSS1        RCQC4E
137600010124     C                   Z-ADD     SCOSS2        RCQC4C
137700010124     C                   ADD       SCOSS3        RCQC4C
137800010124     C                   OTHER
137900010124     c* oltre 48 ore
138000091002     C                   Z-ADD     SCOSS4        RCQCTh
138100091002     C                   Z-ADD     SCOSS1        RCQCTE
138200010124     C                   Z-ADD     SCOSS2        RCQCTC
138300010124     C                   ADD       SCOSS3        RCQCTC
138400010124    7C                   ENDSL
138500010124    6C                   ENDIF
138600010124    5C                   ENDIF
138700010124   X4C                   ELSE
138800010124     C* ...........DI CUI CONSEGNE PARZIALI
138900010124    5C     SCOGBO        IFEQ      'P'
139000091002     C                   Z-ADD     SCOSS4        RCQCPh
139100091002     C                   Z-ADD     SCOSS1        RCQCPE
139200010124     C                   Z-ADD     SCOSS2        RCQCPC
139300010124     C                   ADD       SCOSS3        RCQCPC
139400010124   X5C                   ELSE
139500010124     C* ...........DI CUI RISERVA DI CONSEGNA
139600010124    6C     SCOGBO        IFEQ      'R'
139700091002     C                   Z-ADD     SCOSS4        RCQCRh
139800091002     C                   Z-ADD     SCOSS1        RCQCRE
139900010124     C                   Z-ADD     SCOSS2        RCQCRC
140000010124     C                   ADD       SCOSS3        RCQCRC
140100010124    6C                   ENDIF
140200010124    5C                   ENDIF
140300010124    4C                   ENDIF
140400010124     C***************************
140500010124     C* SPEDIZIONI NON CONSEGNATE
140600010124     C***************************
140700010124     C     SCOSCO        WHENEQ    'N'
140800010124    4C                   SELECT
140900010124     C*............DI CUI GIACENZE APERTE
141000010124     C     SCOGBO        WHENEQ    'G'
141100010124     C* Anticipo
141200010124    5C     SCOORE        IFLT      *ZEROS
141300010124    6C                   SELECT
141400010124     C* Oltre 24 ore
141500010124     C     YY            WHENEQ    1
141600091002     C                   Z-ADD     SCOSS4        RCQGOh
141700091002     C                   Z-ADD     SCOSS1        RCQGOE
141800010124     C                   Z-ADD     SCOSS2        RCQGOC
141900010124     C                   ADD       SCOSS3        RCQGOC
142000010124     c* 24 ore
142100010124     C     YY            WHENEQ    2
142200091002     C                   Z-ADD     SCOSS4        RCQGAh
142300091002     C                   Z-ADD     SCOSS1        RCQGAE
142400010124     C                   Z-ADD     SCOSS2        RCQGAC
142500010124     C                   ADD       SCOSS3        RCQGAC
142600010124     C                   OTHER
142700010124     c* 12 ore
142800091002     C                   Z-ADD     SCOSS4        RCQGBh
142900091002     C                   Z-ADD     SCOSS1        RCQGBE
143000010124     C                   Z-ADD     SCOSS2        RCQGBC
143100010124     C                   ADD       SCOSS3        RCQGBC
143200010124    6C                   ENDSL
143300010124   X5C                   ELSE
143400010124     c* Conforme
143500010124    6C     SCOORE        IFEQ      *ZEROS
143600091002     C                   Z-ADD     SCOSS4        RCQG0h
143700091002     C                   Z-ADD     SCOSS1        RCQG0E
143800010124     C                   Z-ADD     SCOSS2        RCQG0C
143900010124     C                   ADD       SCOSS3        RCQG0C
144000010124   X6C                   ELSE
144100010124     c* Ritardo
144200010124    7C                   SELECT
144300010124     c* 12 ore
144400010124     C     YY            WHENEQ    1
144500091002     C                   Z-ADD     SCOSS4        RCQG1h
144600091002     C                   Z-ADD     SCOSS1        RCQG1E
144700010124     C                   Z-ADD     SCOSS2        RCQG1C
144800010124     C                   ADD       SCOSS3        RCQG1C
144900010124     c* 24 ore
145000010124     C     YY            WHENEQ    2
145100091002     C                   Z-ADD     SCOSS4        RCQG2h
145200091002     C                   Z-ADD     SCOSS1        RCQG2E
145300010124     C                   Z-ADD     SCOSS2        RCQG2C
145400010124     C                   ADD       SCOSS3        RCQG2C
145500010124     c* 36 ore
145600010124     C     YY            WHENEQ    3
145700091002     C                   Z-ADD     SCOSS4        RCQG3h
145800091002     C                   Z-ADD     SCOSS1        RCQG3E
145900010124     C                   Z-ADD     SCOSS2        RCQG3C
146000010124     C                   ADD       SCOSS3        RCQG3C
146100010124     c* 48 ore
146200010124     C     YY            WHENEQ    4
146300091002     C                   Z-ADD     SCOSS4        RCQG4h
146400091002     C                   Z-ADD     SCOSS1        RCQG4E
146500010124     C                   Z-ADD     SCOSS2        RCQG4C
146600010124     C                   ADD       SCOSS3        RCQG4C
146700010124     C                   OTHER
146800010124     c* oltre 48 ore
146900091002     C                   Z-ADD     SCOSS4        RCQGTh
147000091002     C                   Z-ADD     SCOSS1        RCQGTE
147100010124     C                   Z-ADD     SCOSS2        RCQGTC
147200010124     C                   ADD       SCOSS3        RCQGTC
147300010124    7C                   ENDSL
147400010124    6C                   ENDIF
147500010124    5C                   ENDIF
147600010124     C*............DI CUI LASCIATI AVVISO
147700010124     C     SCOGBO        WHENEQ    'L'
147800010124     C* Anticipo
147900010124    5C     SCOORE        IFLT      *ZEROS
148000010124    6C                   SELECT
148100010124     C* Oltre 24 ore
148200010124     C     YY            WHENEQ    1
148300091002     C                   Z-ADD     SCOSS4        RCQLOh
148400091002     C                   Z-ADD     SCOSS1        RCQLOE
148500010124     C                   Z-ADD     SCOSS2        RCQLOC
148600010124     C                   ADD       SCOSS3        RCQLOC
148700010124     c* 24 ore
148800010124     C     YY            WHENEQ    2
148900091002     C                   Z-ADD     SCOSS4        RCQLAh
149000091002     C                   Z-ADD     SCOSS1        RCQLAE
149100010124     C                   Z-ADD     SCOSS2        RCQLAC
149200010124     C                   ADD       SCOSS3        RCQLAC
149300010124     C                   OTHER
149400010124     c* 12 ore
149500091002     C                   Z-ADD     SCOSS4        RCQLBh
149600091002     C                   Z-ADD     SCOSS1        RCQLBE
149700010124     C                   Z-ADD     SCOSS2        RCQLBC
149800010124     C                   ADD       SCOSS3        RCQLBC
149900010124    6C                   ENDSL
150000010124   X5C                   ELSE
150100010124     c* Conforme
150200010124    6C     SCOORE        IFEQ      *ZEROS
150300091002     C                   Z-ADD     SCOSS4        RCQL0h
150400091002     C                   Z-ADD     SCOSS1        RCQL0E
150500010124     C                   Z-ADD     SCOSS2        RCQL0C
150600010124     C                   ADD       SCOSS3        RCQL0C
150700010124   X6C                   ELSE
150800010124     c* Ritardo
150900010124    7C                   SELECT
151000010124     c* 12 ore
151100010124     C     YY            WHENEQ    1
151200091002     C                   Z-ADD     SCOSS4        RCQL1h
151300091002     C                   Z-ADD     SCOSS1        RCQL1E
151400010124     C                   Z-ADD     SCOSS2        RCQL1C
151500010124     C                   ADD       SCOSS3        RCQL1C
151600010124     c* 24 ore
151700010124     C     YY            WHENEQ    2
151800091002     C                   Z-ADD     SCOSS4        RCQL2h
151900091002     C                   Z-ADD     SCOSS1        RCQL2E
152000010124     C                   Z-ADD     SCOSS2        RCQL2C
152100010124     C                   ADD       SCOSS3        RCQL2C
152200010124     c* 36 ore
152300010124     C     YY            WHENEQ    3
152400091002     C                   Z-ADD     SCOSS4        RCQL3h
152500091002     C                   Z-ADD     SCOSS1        RCQL3E
152600010124     C                   Z-ADD     SCOSS2        RCQL3C
152700010124     C                   ADD       SCOSS3        RCQL3C
152800010124     c* 48 ore
152900010124     C     YY            WHENEQ    4
153000091002     C                   Z-ADD     SCOSS4        RCQL4h
153100091002     C                   Z-ADD     SCOSS1        RCQL4E
153200010124     C                   Z-ADD     SCOSS2        RCQL4C
153300010124     C                   ADD       SCOSS3        RCQL4C
153400010124     C                   OTHER
153500010124     c* oltre 48 ore
153600091002     C                   Z-ADD     SCOSS4        RCQLTh
153700091002     C                   Z-ADD     SCOSS1        RCQLTE
153800010124     C                   Z-ADD     SCOSS2        RCQLTC
153900010124     C                   ADD       SCOSS3        RCQLTC
154000010124    7C                   ENDSL
154100010124    6C                   ENDIF
154200010124    5C                   ENDIF
154300010124     C*............DI CUI CHIUSO PER TURNO
154400010124     C     SCOGBO        WHENEQ    'C'
154500010124     C* Anticipo
154600010124    5C     SCOORE        IFLT      *ZEROS
154700010124    6C                   SELECT
154800010124     C* Oltre 24 ore
154900010124     C     YY            WHENEQ    1
155000091002     C                   Z-ADD     SCOSS4        RCQHOh
155100091002     C                   Z-ADD     SCOSS1        RCQHOE
155200010124     C                   Z-ADD     SCOSS2        RCQHOC
155300010124     C                   ADD       SCOSS3        RCQHOC
155400010124     c* 24 ore
155500010124     C     YY            WHENEQ    2
155600091002     C                   Z-ADD     SCOSS4        RCQHAh
155700091002     C                   Z-ADD     SCOSS1        RCQHAE
155800010124     C                   Z-ADD     SCOSS2        RCQHAC
155900010124     C                   ADD       SCOSS3        RCQHAC
156000010124     C                   OTHER
156100010124     c* 12 ore
156200091002     C                   Z-ADD     SCOSS4        RCQHBh
156300091002     C                   Z-ADD     SCOSS1        RCQHBE
156400010124     C                   Z-ADD     SCOSS2        RCQHBC
156500010124     C                   ADD       SCOSS3        RCQHBC
156600010124    6C                   ENDSL
156700010124   X5C                   ELSE
156800010124     c* Conforme
156900010124    6C     SCOORE        IFEQ      *ZEROS
157000091002     C                   Z-ADD     SCOSS4        RCQH0h
157100091002     C                   Z-ADD     SCOSS1        RCQH0E
157200010124     C                   Z-ADD     SCOSS2        RCQH0C
157300010124     C                   ADD       SCOSS3        RCQH0C
157400010124   X6C                   ELSE
157500010124     c* Ritardo
157600010124    7C                   SELECT
157700010124     c* 12 ore
157800010124     C     YY            WHENEQ    1
157900091002     C                   Z-ADD     SCOSS4        RCQH1h
158000091002     C                   Z-ADD     SCOSS1        RCQH1E
158100010124     C                   Z-ADD     SCOSS2        RCQH1C
158200010124     C                   ADD       SCOSS3        RCQH1C
158300010124     c* 24 ore
158400010124     C     YY            WHENEQ    2
158500091002     C                   Z-ADD     SCOSS4        RCQH2h
158600091002     C                   Z-ADD     SCOSS1        RCQH2E
158700010124     C                   Z-ADD     SCOSS2        RCQH2C
158800010124     C                   ADD       SCOSS3        RCQH2C
158900010124     c* 36 ore
159000010124     C     YY            WHENEQ    3
159100091002     C                   Z-ADD     SCOSS4        RCQH3h
159200091002     C                   Z-ADD     SCOSS1        RCQH3E
159300010124     C                   Z-ADD     SCOSS2        RCQH3C
159400010124     C                   ADD       SCOSS3        RCQH3C
159500010124     c* 48 ore
159600010124     C     YY            WHENEQ    4
159700091002     C                   Z-ADD     SCOSS4        RCQH4h
159800091002     C                   Z-ADD     SCOSS1        RCQH4E
159900010124     C                   Z-ADD     SCOSS2        RCQH4C
160000010124     C                   ADD       SCOSS3        RCQH4C
160100010124     C                   OTHER
160200010124     c* oltre 48 ore
160300091002     C                   Z-ADD     SCOSS4        RCQHth
160400091002     C                   Z-ADD     SCOSS1        RCQHTE
160500010124     C                   Z-ADD     SCOSS2        RCQHTC
160600010124     C                   ADD       SCOSS3        RCQHTC
160700010124    7C                   ENDSL
160800010124    6C                   ENDIF
160900010124    5C                   ENDIF
161000010124     C*............DI CUI NON FATTE
161100010124     C     SCOGBO        WHENEQ    'N'
161200010124     C* Anticipo
161300010124    5C     SCOORE        IFLT      *ZEROS
161400010124    6C                   SELECT
161500010124     C* Oltre 24 ore
161600010124     C     YY            WHENEQ    1
161700091002     C                   Z-ADD     SCOSS4        RCQFOh
161800091002     C                   Z-ADD     SCOSS1        RCQFOE
161900010124     C                   Z-ADD     SCOSS2        RCQFOC
162000010124     C                   ADD       SCOSS3        RCQFOC
162100010124     c* 24 ore
162200010124     C     YY            WHENEQ    2
162300091002     C                   Z-ADD     SCOSS4        RCQFAh
162400091002     C                   Z-ADD     SCOSS1        RCQFAE
162500010124     C                   Z-ADD     SCOSS2        RCQFAC
162600010124     C                   ADD       SCOSS3        RCQFAC
162700010124     C                   OTHER
162800010124     c* 12 ore
162900091002     C                   Z-ADD     SCOSS4        RCQFBh
163000091002     C                   Z-ADD     SCOSS1        RCQFBE
163100010124     C                   Z-ADD     SCOSS2        RCQFBC
163200010124     C                   ADD       SCOSS3        RCQFBC
163300010124    6C                   ENDSL
163400010124   X5C                   ELSE
163500010124     c* Conforme
163600010124    6C     SCOORE        IFEQ      *ZEROS
163700091002     C                   Z-ADD     SCOSS4        RCQF0h
163800091002     C                   Z-ADD     SCOSS1        RCQF0E
163900010124     C                   Z-ADD     SCOSS2        RCQF0C
164000010124     C                   ADD       SCOSS3        RCQF0C
164100010124   X6C                   ELSE
164200010124     c* Ritardo
164300010124    7C                   SELECT
164400010124     c* 12 ore
164500010124     C     YY            WHENEQ    1
164600091002     C                   Z-ADD     SCOSS4        RCQF1h
164700091002     C                   Z-ADD     SCOSS1        RCQF1E
164800010124     C                   Z-ADD     SCOSS2        RCQF1C
164900010124     C                   ADD       SCOSS3        RCQF1C
165000010124     c* 24 ore
165100010124     C     YY            WHENEQ    2
165200091002     C                   Z-ADD     SCOSS4        RCQF2h
165300091002     C                   Z-ADD     SCOSS1        RCQF2E
165400010124     C                   Z-ADD     SCOSS2        RCQF2C
165500010124     C                   ADD       SCOSS3        RCQF2C
165600010124     c* 36 ore
165700010124     C     YY            WHENEQ    3
165800091002     C                   Z-ADD     SCOSS4        RCQF3h
165900091002     C                   Z-ADD     SCOSS1        RCQF3E
166000010124     C                   Z-ADD     SCOSS2        RCQF3C
166100010124     C                   ADD       SCOSS3        RCQF3C
166200010124     c* 48 ore
166300010124     C     YY            WHENEQ    4
166400091002     C                   Z-ADD     SCOSS4        RCQF4h
166500091002     C                   Z-ADD     SCOSS1        RCQF4E
166600010124     C                   Z-ADD     SCOSS2        RCQF4C
166700010124     C                   ADD       SCOSS3        RCQF4C
166800010124     C                   OTHER
166900010124     c* oltre 48 ore
167000091002     C                   Z-ADD     SCOSS4        RCQFTh
167100091002     C                   Z-ADD     SCOSS1        RCQFTE
167200010124     C                   Z-ADD     SCOSS2        RCQFTC
167300010124     C                   ADD       SCOSS3        RCQFTC
167400010124    7C                   ENDSL
167500010124    6C                   ENDIF
167600010124    5C                   ENDIF
167700010124    4C                   ENDSL
167800010124     C*******************************
167900010124     C* SPEDIZIONI ANCORA IN DISTINTA
168000010124     C*******************************
168100010124     C     SCOSCO        WHENEQ    'D'
168200010124     C* Anticipo
168300010124    5C     SCOORE        IFLT      *ZEROS
168400010124    6C                   SELECT
168500010124     C* Oltre 24 ore
168600010124     C     YY            WHENEQ    1
168700091002     C                   Z-ADD     SCOSS4        RCQDOh
168800091002     C                   Z-ADD     SCOSS1        RCQDOE
168900010124     C                   Z-ADD     SCOSS2        RCQDOC
169000010124     C                   ADD       SCOSS3        RCQDOC
169100010124     c* 24 ore
169200010124     C     YY            WHENEQ    2
169300091002     C                   Z-ADD     SCOSS4        RCQDAh
169400091002     C                   Z-ADD     SCOSS1        RCQDAE
169500010124     C                   Z-ADD     SCOSS2        RCQDAC
169600010124     C                   ADD       SCOSS3        RCQDAC
169700010124     C                   OTHER
169800010124     c* 12 ore
169900091002     C                   Z-ADD     SCOSS4        RCQDBh
170000091002     C                   Z-ADD     SCOSS1        RCQDBE
170100010124     C                   Z-ADD     SCOSS2        RCQDBC
170200010124     C                   ADD       SCOSS3        RCQDBC
170300010124    6C                   ENDSL
170400010124   X5C                   ELSE
170500010124     c* Conforme
170600010124    6C     SCOORE        IFEQ      *ZEROS
170700091002     C                   Z-ADD     SCOSS4        RCQD0h
170800091002     C                   Z-ADD     SCOSS1        RCQD0E
170900010124     C                   Z-ADD     SCOSS2        RCQD0C
171000010124     C                   ADD       SCOSS3        RCQD0C
171100010124   X6C                   ELSE
171200010124     c* Ritardo
171300010124    7C                   SELECT
171400010124     c* 12 ore
171500010124     C     YY            WHENEQ    1
171600091002     C                   Z-ADD     SCOSS4        RCQD1h
171700091002     C                   Z-ADD     SCOSS1        RCQD1E
171800010124     C                   Z-ADD     SCOSS2        RCQD1C
171900010124     C                   ADD       SCOSS3        RCQD1C
172000010124     c* 24 ore
172100010124     C     YY            WHENEQ    2
172200091002     C                   Z-ADD     SCOSS4        RCQD2h
172300091002     C                   Z-ADD     SCOSS1        RCQD2E
172400010124     C                   Z-ADD     SCOSS2        RCQD2C
172500010124     C                   ADD       SCOSS3        RCQD2C
172600010124     c* 36 ore
172700010124     C     YY            WHENEQ    3
172800091002     C                   Z-ADD     SCOSS4        RCQD3h
172900091002     C                   Z-ADD     SCOSS1        RCQD3E
173000010124     C                   Z-ADD     SCOSS2        RCQD3C
173100010124     C                   ADD       SCOSS3        RCQD3C
173200010124     c* 48 ore
173300010124     C     YY            WHENEQ    4
173400091002     C                   Z-ADD     SCOSS4        RCQD4h
173500091002     C                   Z-ADD     SCOSS1        RCQD4E
173600010124     C                   Z-ADD     SCOSS2        RCQD4C
173700010124     C                   ADD       SCOSS3        RCQD4C
173800010124     C                   OTHER
173900010124     c* oltre 48 ore
174000091002     C                   Z-ADD     SCOSS4        RCQDTh
174100091002     C                   Z-ADD     SCOSS1        RCQDTE
174200010124     C                   Z-ADD     SCOSS2        RCQDTC
174300010124     C                   ADD       SCOSS3        RCQDTC
174400010124    7C                   ENDSL
174500010124    6C                   ENDIF
174600010124    5C                   ENDIF
174700010124     C***************************
174800010124     C* SPEDIZIONI IN CONSEGNA
174900010124     C***************************
175000010124     C     SCOSCO        WHENEQ    'I'
175100010124     C* Anticipo
175200010124    5C     SCOORE        IFLT      *ZEROS
175300010124    6C                   SELECT
175400010124     C* Oltre 24 ore
175500010124     C     YY            WHENEQ    1
175600091002     C                   Z-ADD     SCOSS4        RCQIOh
175700091002     C                   Z-ADD     SCOSS1        RCQIOE
175800010124     C                   Z-ADD     SCOSS2        RCQIOC
175900010124     C                   ADD       SCOSS3        RCQIOC
176000010124     c* 24 ore
176100010124     C     YY            WHENEQ    2
176200091002     C                   Z-ADD     SCOSS4        RCQIAh
176300091002     C                   Z-ADD     SCOSS1        RCQIAE
176400010124     C                   Z-ADD     SCOSS2        RCQIAC
176500010124     C                   ADD       SCOSS3        RCQIAC
176600010124     C                   OTHER
176700010124     c* 12 ore
176800091002     C                   Z-ADD     SCOSS4        RCQIBh
176900091002     C                   Z-ADD     SCOSS1        RCQIBE
177000010124     C                   Z-ADD     SCOSS2        RCQIBC
177100010124     C                   ADD       SCOSS3        RCQIBC
177200010124    6C                   ENDSL
177300010124   X5C                   ELSE
177400010124     c* Conforme
177500010124    6C     SCOORE        IFEQ      *ZEROS
177600091002     C                   Z-ADD     SCOSS4        RCQI0h
177700091002     C                   Z-ADD     SCOSS1        RCQI0E
177800010124     C                   Z-ADD     SCOSS2        RCQI0C
177900010124     C                   ADD       SCOSS3        RCQI0C
178000010124   X6C                   ELSE
178100010124     c* Ritardo
178200010124    7C                   SELECT
178300010124     c* 12 ore
178400010124     C     YY            WHENEQ    1
178500091002     C                   Z-ADD     SCOSS4        RCQI1h
178600091002     C                   Z-ADD     SCOSS1        RCQI1E
178700010124     C                   Z-ADD     SCOSS2        RCQI1C
178800010124     C                   ADD       SCOSS3        RCQI1C
178900010124     c* 24 ore
179000010124     C     YY            WHENEQ    2
179100091002     C                   Z-ADD     SCOSS4        RCQI2h
179200091002     C                   Z-ADD     SCOSS1        RCQI2E
179300010124     C                   Z-ADD     SCOSS2        RCQI2C
179400010124     C                   ADD       SCOSS3        RCQI2C
179500010124     c* 36 ore
179600010124     C     YY            WHENEQ    3
179700091002     C                   Z-ADD     SCOSS4        RCQI3h
179800091002     C                   Z-ADD     SCOSS1        RCQI3E
179900010124     C                   Z-ADD     SCOSS2        RCQI3C
180000010124     C                   ADD       SCOSS3        RCQI3C
180100010124     c* 48 ore
180200010124     C     YY            WHENEQ    4
180300091002     C                   Z-ADD     SCOSS4        RCQI4h
180400091002     C                   Z-ADD     SCOSS1        RCQI4E
180500010124     C                   Z-ADD     SCOSS2        RCQI4C
180600010124     C                   ADD       SCOSS3        RCQI4C
180700010124     C                   OTHER
180800010124     c* oltre 48 ore
180900091002     C                   Z-ADD     SCOSS4        RCQITh
181000091002     C                   Z-ADD     SCOSS1        RCQITE
181100010124     C                   Z-ADD     SCOSS2        RCQITC
181200010124     C                   ADD       SCOSS3        RCQITC
181300010124    7C                   ENDSL
181400010124    6C                   ENDIF
181500010124    5C                   ENDIF
181600010124     C***************************
181700010124     C* SPEDIZIONI NON IN CONSEGNA
181800010124     C***************************
181900010124     C     SCOSCO        WHENEQ    'O'
182000010124     C* Anticipo
182100010124    5C     SCOORE        IFLT      *ZEROS
182200010124    6C                   SELECT
182300010124     C* Oltre 24 ore
182400010124     C     YY            WHENEQ    1
182500091002     C                   Z-ADD     SCOSS4        RCQOOh
182600091002     C                   Z-ADD     SCOSS1        RCQOOE
182700010124     C                   Z-ADD     SCOSS2        RCQOOC
182800010124     C                   ADD       SCOSS3        RCQOOC
182900010124     c* 24 ore
183000091002     C     YY            WHENEQ    2
183100091002     C                   Z-ADD     SCOSS4        RCQOAh
183200091002     C                   Z-ADD     SCOSS1        RCQOAE
183300010124     C                   Z-ADD     SCOSS2        RCQOAC
183400010124     C                   ADD       SCOSS3        RCQOAC
183500010124     C                   OTHER
183600010124     c* 12 ore
183700091002     C                   Z-ADD     SCOSS4        RCQOBh
183800091002     C                   Z-ADD     SCOSS1        RCQOBE
183900010124     C                   Z-ADD     SCOSS2        RCQOBC
184000010124     C                   ADD       SCOSS3        RCQOBC
184100010124    6C                   ENDSL
184200010124   X5C                   ELSE
184300010124     c* Conforme
184400010124    6C     SCOORE        IFEQ      *ZEROS
184500091002     C                   Z-ADD     SCOSS4        RCQO0h
184600091002     C                   Z-ADD     SCOSS1        RCQO0E
184700010124     C                   Z-ADD     SCOSS2        RCQO0C
184800010124     C                   ADD       SCOSS3        RCQO0C
184900010124   X6C                   ELSE
185000010124     c* Ritardo
185100010124    7C                   SELECT
185200010124     c* 12 ore
185300010124     C     YY            WHENEQ    1
185400091002     C                   Z-ADD     SCOSS4        RCQO1h
185500091002     C                   Z-ADD     SCOSS1        RCQO1E
185600010124     C                   Z-ADD     SCOSS2        RCQO1C
185700010124     C                   ADD       SCOSS3        RCQO1C
185800010124     c* 24 ore
185900010124     C     YY            WHENEQ    2
186000091002     C                   Z-ADD     SCOSS4        RCQO2h
186100091002     C                   Z-ADD     SCOSS1        RCQO2E
186200010124     C                   Z-ADD     SCOSS2        RCQO2C
186300010124     C                   ADD       SCOSS3        RCQO2C
186400010124     c* 36 ore
186500010124     C     YY            WHENEQ    3
186600091002     C                   Z-ADD     SCOSS4        RCQO3h
186700091002     C                   Z-ADD     SCOSS1        RCQO3E
186800010124     C                   Z-ADD     SCOSS2        RCQO3C
186900010124     C                   ADD       SCOSS3        RCQO3C
187000010124     c* 48 ore
187100010124     C     YY            WHENEQ    4
187200091002     C                   Z-ADD     SCOSS4        RCQO4h
187300091002     C                   Z-ADD     SCOSS1        RCQO4E
187400010124     C                   Z-ADD     SCOSS2        RCQO4C
187500010124     C                   ADD       SCOSS3        RCQO4C
187600010124     C                   OTHER
187700010124     c* oltre 48 ore
187800091002     C                   Z-ADD     SCOSS4        RCQOTh
187900091002     C                   Z-ADD     SCOSS1        RCQOTE
188000010124     C                   Z-ADD     SCOSS2        RCQOTC
188100010124     C                   ADD       SCOSS3        RCQOTC
188200010124    7C                   ENDSL
188300010124    6C                   ENDIF
188400010124    5C                   ENDIF
188500010124     C***************************
188600010124     C* SPEDIZIONI CONSEGNABILI
188700010124     C***************************
188800010124     C     SCOSCO        WHENEQ    'B'
188900010124    4C                   SELECT
189000010124     C* Anticipo
189100010124     C     SCOORE        WHENLT    0
189200091002     C                   Z-ADD     SCOSS4        RCQBAh
189300091002     C                   Z-ADD     SCOSS1        RCQBAE
189400010124     C                   Z-ADD     SCOSS2        RCQBAC
189500010124     C                   ADD       SCOSS3        RCQBAC
189600010124     c* Conformi
189700010124     C     SCOORE        WHENEQ    0
189800091002     C                   Z-ADD     SCOSS4        RCQB0h
189900091002     C                   Z-ADD     SCOSS1        RCQB0E
190000010124     C                   Z-ADD     SCOSS2        RCQB0C
190100010124     C                   ADD       SCOSS3        RCQB0C
190200010124     C                   OTHER
190300010124     c* Ritardo
190400091002     C                   Z-ADD     SCOSS4        RCQB1h
190500091002     C                   Z-ADD     SCOSS1        RCQB1E
190600010124     C                   Z-ADD     SCOSS2        RCQB1C
190700010124     C                   ADD       SCOSS3        RCQB1C
190800010124    4C                   ENDSL
190900010124    3C                   ENDSL
191000010124    2C                   ENDIF
191100010124     C     KSCO4         READE     FNSCO04L                               31
191200010124    1C                   ENDDO
191300010124     C                   ENDSR
191400970527     C*****************************************************************
191500970527     C* STAMPA RIGA DI TOTALE P.O. SE RICHIESTO DAL/AL
191600970527     C*****************************************************************
191700970527     C     PRTTOF        BEGSR
191800970527     C* SPUNTE PISTOLA 88
191900970527     C* Disk B
192000970527     C                   MOVE      *ZEROS        TOF88P
192100970623     C     TOFPAR        IFGT      *ZEROS
192200970527     C     TOF88B        DIV(H)    TOFPAR        $XX               7 4
192300970527     C     $XX           MULT      100           TOF88P
192400970527     C                   END
192500970527     C* Altri
192600970527     C                   MOVE      *ZEROS        TOF88C
192700970623     C     TOFPAR        IFGT      *ZEROS
192800970527     C     TOF88A        DIV(H)    TOFPAR        $XX               7 4
192900970527     C     $XX           MULT      100           TOF88C
193000970527     C                   END
193100970527     C* BOLLETTATO IN SOSPENSIONE
193200970527     C                   MOVE      *ZEROS        TOFBSP
193300970623     C     TOFBOL        IFGT      *ZEROS
193400970527     C     TOFBSS        DIV(H)    TOFBOL        $XX               7 4
193500970527     C     $XX           MULT      100           TOFBSP
193600970527     C                   END
193700970527     C* SPEDIZIONI A TERRA FUORI AFFIDABILITA'
193800970527     C                   MOVE      *ZEROS        TOFFAP
193900970623     C     TOFSDA        IFGT      *ZEROS
194000970527     C     TOFFAF        DIV(H)    TOFSDA        $XX               7 4
194100970527     C     $XX           MULT      100           TOFFAP
194200970527     C                   END
194300970606     C* COLLI PARTITI NON SPUNTATI IN ARRIVO
194400010123     C**                   MOVE *ZEROS    TOFNRP
194500010123     C**         TOFARR    IFGT *ZEROS
194600010123     C**         TOFNAR    DIV  TOFARR    $XX     74H
194700010123     C**         $XX       MULT 100       TOFNRP
194800010123     C**                   END
194900970527     C* SPEDIZIONI NON CONSEGNATE
195000970527     C                   MOVE      *ZEROS        TOFNCP
195100970623     C     TOFCSB        IFGT      *ZEROS
195200970527     C     TOFNCN        DIV(H)    TOFCSB        $XX               7 4
195300970527     C     $XX           MULT      100           TOFNCP
195400970527     C                   END
195500970527     C*
195600970527     C                   ADD       3             $RIGHE
195700970527     C                   WRITE     LV66TS
195800970527     C                   WRITE     LV66TF
195900970527     C                   WRITE     LV66TS
196000970527     C*
196100970527     C* AZZERAMENTO CAMPI DI TOTALE
196200970527     C                   CLEAR                   TOFBSS
196300970527     C                   CLEAR                   TOFBOL
196400970527     C                   CLEAR                   TOF88B
196500970527     C                   CLEAR                   TOFPAR
196600970527     C                   CLEAR                   TOF88A
196700970527     C                   CLEAR                   TOFFAF
196800970527     C                   CLEAR                   TOFSDA
196900970527     C                   CLEAR                   TOFSSP
197000970527     C                   CLEAR                   TOFDIS
197100040405     C                   CLEAR                   TOFDISP
197200970527     C                   CLEAR                   TOFBAM
197300970527     C                   CLEAR                   TOFNCN
197400970527     C                   CLEAR                   TOFCSB
197500010124     C                   CLEAR                   TOFDSC
197600010124     C                   CLEAR                   TOFDSR
197700970527     C                   CLEAR                   TOFDIA
197800970527     C                   ENDSR
197900990429     C*****************************************************************
198000990429     C* STAMPA TOTALI AREA
198100990429     C*****************************************************************
198200990429     C     ELATAR        BEGSR
198300990430     C                   SETON                                        41
198400990503     C                   ADD       5             $RIGHE
198500990429     C*   Salto pagina se ho superato il n° massimo di righe
198600990503     C     $RIGHE        IFGT      60
198700990429     C                   WRITE     LV66T1
198800990503     C                   Z-ADD     12            $RIGHE
198900990429    4C                   END
199000990430     C                   WRITE     LV66TST
199100990430     C*    Totali AREA   (BAR+SDI)
199200990430     C*    (imposto indice schiera totali AREA)
199300990430     C                   Z-ADD     1             NT                1 0
199400990503     C                   CLEAR                   W21
199500990503     C                   MOVEA     COS3          W21
199600990503     C                   MOVEA     PRTDAR        W21(6)
199700990503     C                   MOVEA(P)  W21           TOAVAR
199800990430     C                   EXSR      RIETAR
199900990429     C                   WRITE     LV66TA
200000990430     C                   SETOFF                                       41
200100990430     C*    Totali BARTOLINI
200200990430     C*    (imposto indice schiera totali network BAR)
200300990430     C                   Z-ADD     2             NT                1 0
200400990430     C                   MOVEL     COS1          TOAVAR
200500990430     C                   EXSR      RIETAR
200600990429     C                   WRITE     LV66TA
200700990430     C*    TOTALI SDI
200800990430     C*    (imposto indice schiera totali network SDI)
200900990430     C                   Z-ADD     3             NT                1 0
201000990503     C                   MOVEL     COS2          TOAVAR
201100990430     C                   EXSR      RIETAR
201200990429     C                   WRITE     LV66TA
201300990430     C                   WRITE     LV66TST
201400990430     C* MEMORIZZO SCHIERE TOTALI DISTRETTO
201500990430     C                   DO        3             NT
201600990430     C                   EXSR      RIEDIS
201700990430     C                   ENDDO
201800990429     C* AZZERO CAMPI DI TOTALE
201900990429     C                   EXSR      AZZTAR
202000990429     C                   ENDSR
202100990429     C*****************************************************************
202200990430     C* IMPOSTO CAMPI STAMPA DEI TOTALI AREA
202300990429     C*****************************************************************
202400990430     C     RIETAR        BEGSR
202500990429     C* BOLLETTATO IN SOSPENSIONE
202600990430     C                   Z-ADD     SOS(NT)       TOABSS
202700990429     C                   MOVE      *ZEROS        TOABSP
202800990430     C     TBL(NT)       IFGT      *ZEROS
202900990430     C     TOABSS        DIV(H)    TBL(NT)       $XX               7 4
203000990429     C     $XX           MULT      100           TOABSP
203100990429     C                   END
203200990429     C* SPUNTE PISTOLA 88
203300990429     C* Disk B
203400990429     C                   MOVE      *ZEROS        TOA88P
203500990430     C                   Z-ADD     B88(NT)       TOA88B
203600990430     C     PAR(NT)       IFGT      *ZEROS
203700990430     C     TOA88B        DIV(H)    PAR(NT)       $XX               7 4
203800990429     C     $XX           MULT      100           TOA88P
203900990429     C                   END
204000990429     C* Altri
204100990429     C                   MOVE      *ZEROS        TOA88C
204200990430     C                   Z-ADD     A88(NT)       TOA88A
204300990430     C     PAR(NT)       IFGT      *ZEROS
204400990430     C     TOA88A        DIV(H)    PAR(NT)       $XX               7 4
204500990429     C     $XX           MULT      100           TOA88C
204600990429     C                   END
204700990429     C* SPEDIZIONI A TERRA FUORI AFFIDABILITA'
204800990430     C                   Z-ADD     SFA(NT)       TOAFAF
204900990429     C                   MOVE      *ZEROS        TOAFAP
205000990430     C     TDA(NT)       IFGT      *ZEROS
205100990430     C     TOAFAF        DIV(H)    TDA(NT)       $XX               7 4
205200990429     C     $XX           MULT      100           TOAFAP
205300990429     C                   END
205400990429     C* SPEDIZIONI SPAREGGIATE
205500990430     C                   Z-ADD     SSP(NT)       TOASSP
205600990429     C* COLLI PARTITI IN DISGUIDO
205700990430     C                   Z-ADD     DIS(NT)       TOADIS
205800040405     C* disguidi non spuntati in partenza
205900040405     C                   Z-ADD     DISP(NT)      TOADISP
206000990429     C* MANCA DDT
206100990430     C                   Z-ADD     DDT(NT)       TOABAM
206200990429     C* SPEDIZIONI NON CONSEGNATE
206300990429     C                   MOVE      *ZEROS        TOANCP
206400990430     C                   Z-ADD     NCN(NT)       TOANCN
206500990430     C     SCS(NT)       IFGT      *ZEROS
206600990430     C     TOANCN        DIV(H)    SCS(NT)       $XX               7 4
206700990429     C     $XX           MULT      100           TOANCP
206800990429     C                   END
206900010124     C                   Z-ADD     DCN(NT)       TOADSC
207000010124     C                   Z-ADD     DRT(NT)       TOADSR
207100990429     C* DISGUIDI IN AREA
207200990430     C                   Z-ADD     DIA(NT)       TOADIA
207300990429     C                   ENDSR
207400990430     C*****************************************************************
207500990430     C* IMPOSTO SCHIERE TOTALI DISTRETTO
207600990430     C*****************************************************************
207700990430     C     RIEDIS        BEGSR
207800990430     C                   ADD       SOS(NT)       SOD(NT)
207900990430     C                   ADD       TBL(NT)       TBD(NT)
208000990430     C                   ADD       B88(NT)       B8D(NT)
208100990430     C                   ADD       PAR(NT)       PAD(NT)
208200990430     C                   ADD       A88(NT)       A8D(NT)
208300990430     C                   ADD       SFA(NT)       SFD(NT)
208400990430     C                   ADD       TDA(NT)       TDD(NT)
208500990430     C                   ADD       SSP(NT)       SSD(NT)
208600990430     C                   ADD       DIS(NT)       DID(NT)
208700040405     C                   ADD       DISP(NT)      DISD(NT)
208800990430     C                   ADD       DDT(NT)       DDD(NT)
208900990430     C                   ADD       NCN(NT)       NCD(NT)
209000990430     C                   ADD       SCS(NT)       SCD(NT)
209100010124     C                   ADD       DCN(NT)       DCD(NT)
209200010124     C                   ADD       DRT(NT)       DRD(NT)
209300990430     C                   ADD       DIA(NT)       DSD(NT)
209400990430     C                   ENDSR
209500990430     C*****************************************************************
209600990430     C* STAMPA TOTALI DISTRETTO
209700990430     C*****************************************************************
209800990430     C     ELATDS        BEGSR
209900990503     C                   SETON                                        41
210000990430     C                   ADD       4             $RIGHE
210100990430     C*   Salto pagina se ho superato il n° massimo di righe
210200990430     C     $RIGHE        IFGT      60
210300990430     C                   WRITE     LV66T1
210400990430     C                   Z-ADD     11            $RIGHE
210500990430    4C                   END
210600990430     C*    Totali DISTRETTO (BAR+SDI)
210700990430     C*    (imposto indice schiera totali network Bar)
210800990430     C                   Z-ADD     1             NT                1 0
210900990503     C                   CLEAR                   W21
211000990503     C                   MOVEA     COS4          W21
211100990506     C                   MOVEA     PRTDED        W21(11)
211200990503     C                   MOVEA(P)  W21           TOAVAR
211300990430     C                   EXSR      RIETDS
211400990430     C                   WRITE     LV66TA
211500990503     C                   SETOFF                                       41
211600990430     C*    Totali BARTOLINI
211700990430     C*    (imposto indice schiera totali network BAR)
211800990430     C                   Z-ADD     2             NT                1 0
211900990430     C                   MOVEL     COS1          TOAVAR
212000990430     C                   EXSR      RIETDS
212100990430     C                   WRITE     LV66TA
212200990430     C*    TOTALI SDI
212300990430     C                   Z-ADD     3             NT                1 0
212400990430     C                   MOVEL     COS2          TOAVAR
212500990430     C                   EXSR      RIETDS
212600990430     C                   WRITE     LV66TA
212700990430     C                   WRITE     LV66TST
212800990430     C* AZZERO CAMPI DI TOTALE
212900990430     C                   EXSR      AZZTDS
213000990430     C                   ENDSR
213100990430     C*****************************************************************
213200990430     C* IMPOSTO CAMPI STAMPA DEI TOTALI DISTRETTO
213300990430     C*****************************************************************
213400990430     C     RIETDS        BEGSR
213500990430     C* BOLLETTATO IN SOSPENSIONE
213600990430     C                   Z-ADD     SOD(NT)       TOABSS
213700990430     C                   MOVE      *ZEROS        TOABSP
213800990430     C     TBD(NT)       IFGT      *ZEROS
213900990430     C     TOABSS        DIV(H)    TBD(NT)       $XX               7 4
214000990430     C     $XX           MULT      100           TOABSP
214100990430     C                   END
214200990430     C* SPUNTE PISTOLA 88
214300990430     C* Disk B
214400990430     C                   MOVE      *ZEROS        TOA88P
214500990430     C                   Z-ADD     B8D(NT)       TOA88B
214600990430     C     PAD(NT)       IFGT      *ZEROS
214700990430     C     TOA88B        DIV(H)    PAD(NT)       $XX               7 4
214800990430     C     $XX           MULT      100           TOA88P
214900990430     C                   END
215000990430     C* Altri
215100990430     C                   MOVE      *ZEROS        TOA88C
215200990430     C                   Z-ADD     A8D(NT)       TOA88A
215300990430     C     PAD(NT)       IFGT      *ZEROS
215400990430     C     TOA88A        DIV(H)    PAD(NT)       $XX               7 4
215500990430     C     $XX           MULT      100           TOA88C
215600990430     C                   END
215700990430     C* SPEDIZIONI A TERRA FUORI AFFIDABILITA'
215800990430     C                   Z-ADD     SFD(NT)       TOAFAF
215900990430     C                   MOVE      *ZEROS        TOAFAP
216000990430     C     TDD(NT)       IFGT      *ZEROS
216100990430     C     TOAFAF        DIV(H)    TDD(NT)       $XX               7 4
216200990430     C     $XX           MULT      100           TOAFAP
216300990430     C                   END
216400990430     C* SPEDIZIONI SPAREGGIATE
216500990430     C                   Z-ADD     SSD(NT)       TOASSP
216600990430     C* COLLI PARTITI IN DISGUIDO
216700990430     C                   Z-ADD     DID(NT)       TOADIS
216800040405     c* disguidi non spuntati in partenza
216900040405     C                   Z-ADD     DISD(NT)      TOADISP
217000990430     C* MANCA DDT
217100990430     C                   Z-ADD     DDD(NT)       TOABAM
217200990430     C* SPEDIZIONI NON CONSEGNATE
217300990430     C                   MOVE      *ZEROS        TOANCP
217400990430     C                   Z-ADD     NCD(NT)       TOANCN
217500990430     C     SCD(NT)       IFGT      *ZEROS
217600990430     C     TOANCN        DIV(H)    SCD(NT)       $XX               7 4
217700990430     C     $XX           MULT      100           TOANCP
217800990430     C                   END
217900010124     C                   Z-ADD     DCD(NT)       TOADSC
218000010124     C                   Z-ADD     DRD(NT)       TOADSR
218100990430     C* DISGUIDI IN AREA
218200990430     C                   Z-ADD     DSD(NT)       TOADIA
218300990430     C                   ENDSR
218400990429     C*****************************************************************
218500990429     C* AZZERO CAMPI DI TOTALE A LIVELLO DI AREA
218600990429     C*****************************************************************
218700990429     C     AZZTAR        BEGSR
218800990430     C                   CLEAR                   RIT
218900990430     C                   CLEAR                   SOS
219000990430     C                   CLEAR                   TBL
219100990430     C                   CLEAR                   B88
219200990430     C                   CLEAR                   PAR
219300990430     C                   CLEAR                   A88
219400990430     C                   CLEAR                   SFA
219500990430     C                   CLEAR                   TDA
219600990430     C                   CLEAR                   SSP
219700990430     C                   CLEAR                   DIS
219800040405     C                   CLEAR                   DISP
219900990430     C                   CLEAR                   DDT
220000990430     C                   CLEAR                   NCN
220100990430     C                   CLEAR                   SCS
220200010124     C                   CLEAR                   DCN
220300010124     C                   CLEAR                   DRT
220400990430     C                   CLEAR                   DIA
220500990429     C                   ENDSR
220600990430     C*****************************************************************
220700990430     C* AZZERO CAMPI DI TOTALE A LIVELLO DI DISTRETTO
220800990430     C*****************************************************************
220900990430     C     AZZTDS        BEGSR
221000990430     C                   CLEAR                   RID
221100990430     C                   CLEAR                   SOD
221200990430     C                   CLEAR                   TBD
221300990430     C                   CLEAR                   B8D
221400990430     C                   CLEAR                   PAD
221500990430     C                   CLEAR                   A8D
221600990430     C                   CLEAR                   SFD
221700990430     C                   CLEAR                   TDD
221800990430     C                   CLEAR                   SSD
221900990430     C                   CLEAR                   DID
222000040405     C                   CLEAR                   DISD
222100990430     C                   CLEAR                   DDD
222200990430     C                   CLEAR                   NCD
222300990430     C                   CLEAR                   SCD
222400010124     C                   CLEAR                   DCD
222500010124     C                   CLEAR                   DRD
222600990430     C                   CLEAR                   DSD
222700990430     C                   ENDSR
222800970225     C*****************************************************************
222900970225     C* DECODIFICA CODICE DISTRETTO
223000970225     C*****************************************************************
223100970225     C     DECDST        BEGSR
223200970225     C* Chaino tabella 17 per decodifica
223300970225     C                   MOVEL     '17'          KCOD
223400970225     C                   MOVEL(P)  ORGFL3        KKEY
223500970225     C     KTAB          CHAIN     TABEL00F                           30
223600970225    2C     *IN30         IFEQ      *OFF
223700970225     C     TBLFLG        ANDEQ     *BLANKS
223800970401     C                   MOVEL     TBLUNI        DS17
223900970401     C                   MOVEL     §17DES        PRTDED
224000970227     C                   ELSE
224100970227     C                   MOVE      *BLANKS       PRTDED
224200970225    2C                   END
224300990503     C                   MOVE      ORGFL3        PRTFL3
224400970225     C                   ENDSR
224500970225     C*****************************************************************
224600970225     C* DECODIFICA CODICE AREA
224700970225     C*****************************************************************
224800970225     C     DECCAR        BEGSR
224900970225     C* Chaino tabella 05 per decodifica
225000970225     C                   MOVEL     '05'          KCOD
225100970225     C                   MOVEL(P)  ORGCAR        KKEY
225200970225     C     KTAB          CHAIN     TABEL00F                           30
225300970225    2C     *IN30         IFEQ      *OFF
225400970225     C     TBLFLG        ANDEQ     *BLANKS
225500970401     C                   MOVEL     TBLUNI        DS05
225600970401     C                   MOVEL     §05DES        PRTDAR
225700970227     C                   ELSE
225800970227     C                   MOVE      *BLANKS       PRTDAR
225900970225    2C                   END
226000990503     C                   MOVE      ORGCAR        PRTCAR
226100970225     C                   ENDSR
226200970225     C*****************************************************************
226300970224     C* OPERAZIONI INIZIALI
226400970224     C*****************************************************************
226500970224     C     *INZSR        BEGSR
226600970224     C*
226700970224     C     *ENTRY        PLIST
226800970224     C                   PARM                    KPJBA
226900970225     C                   MOVEL     KPJBU         PARAM
227000970224     C*
227100970224     C                   Z-ADD     1             CODUT
227200970224     C                   CALL      'X§PARUT'
227300970224     C                   PARM                    UT§DSE
227400970224     C                   MOVEL     REC80         CNCR80
227500970224     C                   MOVEL     RAGUT         RSUT
227600970226     C*
227700970527     C     PARDAL        IFEQ      *ZEROS
227800970527     C                   Z-ADD     PARDEL        PARDAL
227900970527     C                   END
228000970224     C*
228100970224     C                   TIME                    WTIME            14 0
228200970224     C                   MOVE      WTIME         WDATE             8 0
228300970311     C                   MOVEL     WTIME         UTIME             6 0
228400970224     C                   CLEAR                   WLBDA8
228500970224     C                   MOVE      WDATE         G08DAT
228600970224     C                   CALL      'XSRDA8'
228700970224     C                   PARM                    WLBDA8
228800970224     C                   MOVE      G08INV        WDATEU            8 0
228900990503     C*
229000990503     C     V1CCRP        IFEQ      'S'
229100010124     C                   OPEN      WFRCQ11L
229200990503     C                   END
229300990504     C* Inizializzo schiere totali area visto che le somme sulle
229400990504     C* stesse sono condizionate dagli indicatori di "dati non pervenu-
229500990504     C* ti mentre
229600990504     C*                    EXSR AZZTAR
229700000626     C*
229800000626     C* LEGGO AZORG E CARICO I TOTALI
229900000626     C                   CLEAR                   CC
230000060131     C                   CLEAR                   dd                4 0
230100000626     C     *LOVAL        SETLL     AZORG01L
230200000626     C                   READ      AZORG01L                               31
230300000626     C     *IN31         DOWEQ     *OFF
230400000626     C     ORGFAG        IFEQ      'F'
230500000626     C     ORGFAG        OREQ      'A'
230600000626     C     ORGFVA        IFEQ      ' '
230700000626     C                   MOVEL     ORGDE3        OG143
230800000626     C     §OGPT         IFEQ      'S'
230900000626     C                   ADD       1             CC                3 0
231000000626     C                   Z-ADD     ORGFIL        PT(CC)
231100000626     C                   ENDIF
231200060131     c* Schiera dei p.o. esteri
231300060131     c                   if        §ogntw='EEX' or §ogntw=
231400060131     c                             'DPD' or §ogntw='FED'
231500060131     C                   ADD       1             dd
231600060131     C                   Z-ADD     ORGFIL        EST(DD)
231700060131     c                   endif
231800060131     c
231900000626     C                   ENDIF
232000000626     C                   ENDIF
232100000626     C                   READ      AZORG01L                               31
232200000626     C                   ENDDO
232300060131     c*
232400060131     c* Reperisco data decorrenza separazione dati partenza italia/estero
232500090930     C                   CLEAR                   dsbs02
232600090930     c                   clear                   dvpodeco
232700090930     C                   MOVEL     'L'           T02tla
232800090930     C                   MOVEL     'C'           T02MOD
232900090930     C                   MOVEL     KNSIF         T02SIF
233000090930     C                   MOVEL     'VPO'         T02COD
233100090930     c                   movel(P)  'DECO'        t02ke1
233200090930     C                   CALL      'TIBS02R'
233300090930     C                   PARM                    KPJBA
233400090930     C                   PARM                    dsbs02
233500090930    2C                   IF        T02ERR = *BLANKS
233600090930     C                   MOVEL     T02uni        dvpodeco
233700090930    2C                   ENDIF
233800091001     c
233900091001     c* Se tipo servizio H attivo, stampo lo colonna
234000091001     c                   if        pardal>=§vpotsph
234100091001     c                   seton                                        10
234200091001     c                   endif
234300060131     c
234400090605      * carico le descrizioni fisse dei tipi servizio
234500090605      * chiodo l'indice di schiera per ogni tipo servizio
234600090930      * 1=H - 2=E - 3=C  4=D
234700090605     c                   clear                   ds5e
234800090605     c                   clear                   ens
234900090605     c                   eval      kcod = '5E'
235000090605     c     ktab2         setll     tabel00f
235100090605     c                   do        *hival
235200090605     c     ktab2         reade(n)  tabel00f
235300090605     c                   if        %eof(tabel00f)
235400090605     c                   leave
235500090605     c                   endif
235600090605     c                   eval      ds5e = tbluni
235700090605     c                   if        tblkey = 'C'
235800090930     c                   eval      ens(03) = §5ed05
235900090605     c                   endif
236000090605     c                   if        tblkey = 'D'
236100090930     c                   eval      ens(04) = §5ed05
236200090605     c                   endif
236300090605     c                   if        tblkey = 'E'
236400090930     c                   eval      ens(02) = §5ed05
236500090605     c                   endif
236600090930     c                   if        tblkey = 'H'
236700090930     c                   eval      ens(01) = §5ed05
236800090930     c                   endif
236900090605     c                   enddo
237000060131     c
237100000626     C**
237200970224     C*
237300970224     C     KTAB          KLIST
237400970224     C                   KFLD                    CODUT
237500970224     C                   KFLD                    KCOD
237600970224     C                   KFLD                    KKEY
237700090605
237800090605     C     KTAB2         KLIST
237900090605     C                   KFLD                    CODUT
238000090605     C                   KFLD                    KCOD
238100970226     C*
238200970226     C     KORG2         KLIST
238300970226     C                   KFLD                    V1CDST
238400970226     C                   KFLD                    PARCAR
238500970224     C*
238600970226     C     KSBO          KLIST
238700970226     C                   KFLD                    WSOS
238800970226     C                   KFLD                    ORGFIL
238900990517     C                   KFLD                    WDATEL
239000970226     C*
239100970226     C     KSPA          KLIST
239200971125     C                   KFLD                    PRTTFP
239300970527     C                   KFLD                    WDATEL
239400060131     C     KSPAfil       KLIST
239500060131     C                   KFLD                    PRTTFP
239600060131     C                   KFLD                    orgfil
239700060131     C                   KFLD                    WDATEL
239800970226     C*
239900970226     C     KSAT          KLIST
240000971125     C                   KFLD                    PRTTFP
240100970226     C                   KFLD                    WTFA
240200970527     C                   KFLD                    WDATEL
240300060131     C     KSATfil       KLIST
240400060131     C                   KFLD                    PRTTFP
240500060131     C                   KFLD                    WTFA
240600060131     C                   KFLD                    orgfil
240700060131     C                   KFLD                    WDATEL
240800990409     C     KANM4         KLIST
240900990409     C                   KFLD                    KCAA
241000990409     C                   KFLD                    KDAO
241100990409     C                   KFLD                    KLNA
241200970311     C*
241300970226     C                   CLEAR                   WTFA
241400970226     C*
241500970226     C     KSCO          KLIST
241600970226     C                   KFLD                    ORGFIL
241700970226     C                   KFLD                    WSCO
241800970226     C                   KFLD                    WGBO
241900970527     C                   KFLD                    WDATEL
242000010124     C*
242100010124     C     KSCO4         KLIST
242200010124     C                   KFLD                    RCQFIL
242300010124     C                   KFLD                    RCQDAT
242400970226     C*
242500970226     C     KANT          KLIST
242600970226     C                   KFLD                    ORGFIL
242700970527     C                   KFLD                    WDATEL
242800970226     C                   KFLD                    WCAA
242900970606     C*
243000970606     C     KSAR          KLIST
243100970606     C                   KFLD                    ORGFIL
243200970606     C                   KFLD                    WTRE
243300990517     C                   KFLD                    WDATEL
243400990409     C     KCLN          KLIST
243500990409     C                   KFLD                    KTFP
243600990409     C                   KFLD                    KLNA
243700990409     C                   KFLD                    KANN
243800990409     C                   KFLD                    KMES
243900990504     C*
244000990504     C     KCAE5         KLIST
244100990504     C                   KFLD                    KEPA
244200990504     C                   KFLD                    ORGFIL
244300990503     C*
244400990503     C     KRCQ          KLIST
244500990503     C                   KFLD                    WDATEL
244600990503     C                   KFLD                    ORGFIL
244700970226     C*
244800990409     C     *LIKE         DEFINE    ANMCAA        KCAA
244900990409     C     *LIKE         DEFINE    ANMDAO        KDAO
245000970527     C     *LIKE         DEFINE    PARDEL        WDATEL
245100970527     C     *LIKE         DEFINE    PARTGI        WTGI
245200000208     C     *LIKE         DEFINE    PRTTFP        SAVTFP
245300000208     C     *LIKE         DEFINE    PRTTFA        SAVTFA
245400990409     C     *LIKE         DEFINE    CLNTFP        KTFP
245500990409     C     *LIKE         DEFINE    CLNANN        KANN
245600990409     C     *LIKE         DEFINE    CLNMES        KMES
245700990409     C     *LIKE         DEFINE    ANMLNA        KLNA
245800990409     C     *LIKE         DEFINE    PARDEL        WDATAM
245900970527     C*
246000970224     C     *LIKE         DEFINE    TBLCOD        KCOD
246100970224     C     *LIKE         DEFINE    TBLKEY        KKEY
246200970226     C*
246300970226     C     *LIKE         DEFINE    SBOSOS        WSOS
246400970226     C     *LIKE         DEFINE    SBOSEA        TOTBOL
246500970226     C*
246600970226     C     *LIKE         DEFINE    SPASDE        SDA123
246700970226     C     *LIKE         DEFINE    SPACPC        TOTPAR
246800970226     C     *LIKE         DEFINE    SPA88A        TP88A
246900970226     C     *LIKE         DEFINE    SPA88B        TP88B
247000970226     C*
247100970226     C     *LIKE         DEFINE    SATTFA        WTFA
247200970311     C     *LIKE         DEFINE    SATTRE        WTRE
247300970226     C     *LIKE         DEFINE    SATSDE        WTERR
247400970226     C     *LIKE         DEFINE    SATSDE        WSPAR
247500970225     C*
247600970226     C     *LIKE         DEFINE    SCOSCO        WSCO
247700970226     C     *LIKE         DEFINE    SCOGBO        WGBO
247800970226     C     *LIKE         DEFINE    SCOSS1        CSB123
247900970226     C*
248000970226     C     *LIKE         DEFINE    ANTCAA        WCAA
248100970226     C     *LIKE         DEFINE    ANTTOT        TOTBAM
248200970226     C     *LIKE         DEFINE    ANTTOT        TOTDAR
248300970226     C     *LIKE         DEFINE    ANTTOT        TOTDIS
248400040405     C     *LIKE         DEFINE    ANTTOT        TOTDISP
248500970226     C     *LIKE         DEFINE    ANTTOT        TOTRIT
248600970606     C*
248700970606     C     *LIKE         DEFINE    SARPAR        TOTNAR
248800970606     C     *LIKE         DEFINE    SARCLL        TOTARR
248900990504     C*
249000990504     C     *LIKE         DEFINE    CAEEPA        KEPA
249100010124     C     *LIKE         DEFINE    SCOORE        WORRIT
249200970226     C*
249300970224     C                   ENDSR
249400970401** GBO - Valori per impostare gbo in calcolo spedizioni non consegnate
249500970226GLCN
249600010124** SCA
249700010124999024012
249800010124** SCR
249900010124012024036048999
