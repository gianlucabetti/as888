000100970225     H DECEDIT('0,') DATEDIT(*DMY.)
000200970225     H* FNLV66R *-----------------------------------------------------*
000300970225     H*         - REPORT CONTROLLO QUALITA' - BATCH                   *
000400970401     H*                                                               *
000500970401     H* ATTENZIONE!! QUESTO PGM NECESSITA DI COMPILAZIONE INTERATTIVA *
000600970401     H*              E DOPO AVER ESEGUITO I SEGUENTI COMANDI:         *
000700970401     H*              - OVRPRTF FNLV66PA FNLV66P                       *
000800970401     H*              - OVRPRTF FNLV66PD FNLV66P                       *
000900990428     H*              - OVRPRTF FNLV66PN FNLV66P                       *
001000970225     H*---------------------------------------------------------------*
001100970224     FAZORG02L  IF   E           K DISK
001200970225     FAZORG01L  IF   E           K DISK
001300970225     F                                     RENAME(AZORG:AZORG1)
001400090911     FFiSBO02L  IF   E           K DISK
001500090915     FFISPA02L  IF   E           K DISK
001600090915     FFISPA01L  IF   E           K DISK    RENAME(FISPA000: FISPAfil)
001700090916     Ffisat03L  IF   E           K DISK
001800090916     Ffisat02L  IF   E           K DISK    RENAME(fisat000:fisatfil)
001900000208     FFNSCO03L  IF   E           K DISK
002000010124     FFNSCO04L  IF   E           K DISK
002100010124     F                                     RENAME(FNSCO000:FNSCO4)
002200090930     FFISAR06L  IF   E           K DISK
002300990520     FFNANM04L  IF   E           K DISK
002400990409     FAZCLN01L  IF   E           K DISK
002500970226     FFNANT01L  IF   E           K DISK
002600970226     FFNANT02L  IF   E           K DISK
002700970226     F                                     RENAME(FNANT000:FNANT2)
002800990504     FFNANT04L  IF   E           K DISK
002900990504     F                                     RENAME(FNANT000:FNANT4)
003000970225     FTABEL00F  IF   E           K DISK
003100990504     FAZCAE05L  IF   E           K DISK
003200010124     FWFRCQ11L  UF A E           K DISK    USROPN
003300970227     FFNLV66P   O    E             PRINTER
003400970401     FFNLV66PA  O    E             PRINTER USROPN
003500970401     F                                     RENAME(LV66T1:LV66T1A)
003600970401     F                                     RENAME(LV66T2:LV66T2A)
003700970401     F                                     RENAME(LV66T3:LV66T3A)
003800970401     F                                     RENAME(LV66T4:LV66T4A)
003900970401     F                                     RENAME(LV66D1:LV66D1A)
004000970401     F                                     RENAME(LV66E2:LV66E2A)
004100970527     F                                     RENAME(LV66TF:LV66TFA)
004200970527     F                                     RENAME(LV66TS:LV66TSA)
004300990430     F                                     IGNORE(LV66TA)
004400990430     F                                     IGNORE(LV66TST)
004500970401     FFNLV66PD  O    E             PRINTER USROPN
004600970401     F                                     RENAME(LV66T1:LV66T1D)
004700970401     F                                     RENAME(LV66T2:LV66T2D)
004800970401     F                                     RENAME(LV66T3:LV66T3D)
004900970401     F                                     RENAME(LV66T4:LV66T4D)
005000970401     F                                     RENAME(LV66D1:LV66D1D)
005100970401     F                                     RENAME(LV66E2:LV66E2D)
005200970527     F                                     RENAME(LV66TF:LV66TFD)
005300970527     F                                     RENAME(LV66TS:LV66TSD)
005400990430     F                                     IGNORE(LV66TA)
005500990430     F                                     IGNORE(LV66TST)
005600990428     FFNLV66PN  O    E             PRINTER USROPN
005700990428     F                                     RENAME(LV66T1:LV66T1N)
005800990428     F                                     RENAME(LV66T2:LV66T2N)
005900990428     F                                     RENAME(LV66T3:LV66T3N)
006000990428     F                                     RENAME(LV66T4:LV66T4N)
006100990428     F                                     RENAME(LV66D1:LV66D1N)
006200990428     F                                     RENAME(LV66E2:LV66E2N)
006300990428     F                                     RENAME(LV66TF:LV66TFN)
006400990428     F                                     RENAME(LV66TS:LV66TSN)
006500990430     F                                     IGNORE(LV66TA)
006600990430     F                                     IGNORE(LV66TST)
006700970401     F*
006800090605     D**!!! ENS             S              5    DIM(3) CTDATA PERRCD(3)
006900091001     D ENS             S              5    DIM(4)
007000970401     D GBO             S              1    DIM(4) CTDATA PERRCD(4)
007100990428     D CMD             S             65    DIM(3) CTDATA PERRCD(1)
007200010124     D SCA             S              3  0 DIM(3) CTDATA PERRCD(3)
007300010125     D SCR             S              3  0 DIM(5) CTDATA PERRCD(5)
007400970401     D W65             S              1    DIM(65)
007500990503     D W21             S              1    DIM(21)
007600000626     D PT              S              3  0 DIM(100)
007700060131     D EST             S              3  0 DIM(100)
007800970225      * Le schiere che seguono hanno 3 elementi:
007900090930      *     1° elemento: TIPO SERVIZIO "H"
008000090930      *     2° elemento: TIPO SERIVZIO "E"
008100090930      *     3° elemento: TIPO SERVIZIO "C"
008200090930      *     4° elemento: TIPO SERVIZIO "D"
008300090930     D BOL             S              7  0 DIM(4)
008400090930     D BSS             S              7  0 DIM(4)
008500090930     D FAF             S              7  0 DIM(4)
008600090930     D SDA             S              7  0 DIM(4)
008700090930     D SPA             S              7  0 DIM(4)
008800090930     D NOC             S              9  0 DIM(4)
008900090930     D CSB             S              9  0 DIM(4)
009000090930     D DSC             S              9  0 DIM(4)
009100090930     D DSR             S              9  0 DIM(4)
009200990409     D*
009300990430     D* Schiere per memorizzazione tot. a livello di area
009400090930     D RIT             S              8  0 DIM(4)
009500090930     D SOS             S              7  0 DIM(4)
009600090930     D TBL             S              8  0 DIM(4)
009700090930     D B88             S              8  0 DIM(4)
009800090930     D PAR             S              8  0 DIM(4)
009900090930     D A88             S              8  0 DIM(4)
010000090930     D SFA             S              8  0 DIM(4)
010100090930     D TDA             S              8  0 DIM(4)
010200090930     D SSP             S              8  0 DIM(4)
010300090930     D DIS             S              8  0 DIM(4)
010400090930     D DISP            S              8  0 DIM(4)
010500090930     D DDT             S              7  0 DIM(4)
010600010123     D**                  ARR         3  8 0             TOT ARRIVATO
010700010123     D**                  NAR         3  8 0             NO SPUNT.ARR
010800090930     D NCN             S             10  0 DIM(4)
010900090930     D SCS             S             10  0 DIM(4)
011000090930     D DCN             S             10  0 DIM(4)
011100090930     D DRT             S             10  0 DIM(4)
011200090930     D DIA             S              8  0 DIM(4)
011300990430     D* Schiere per memorizzazione tot. a livello di distretto
011400090930     D RID             S              8  0 DIM(4)
011500090930     D SOD             S              7  0 DIM(4)
011600090930     D TBD             S              8  0 DIM(4)
011700090930     D B8D             S              8  0 DIM(4)
011800090930     D PAD             S              8  0 DIM(4)
011900090930     D A8D             S              8  0 DIM(4)
012000090930     D SFD             S              8  0 DIM(4)
012100090930     D TDD             S              8  0 DIM(4)
012200090930     D SSD             S              8  0 DIM(4)
012300090930     D DID             S              8  0 DIM(4)
012400090930     D DISD            S              8  0 DIM(4)
012500090930     D DDD             S              7  0 DIM(4)
012600010123     D**                  ARD         3  8 0             TOT ARRIVATO
012700010123     D**                  NAD         3  8 0             NO SPUNT.ARR
012800090930     D NCD             S             10  0 DIM(4)
012900090930     D SCD             S             10  0 DIM(4)
013000090930     D DCD             S             10  0 DIM(4)
013100090930     D DRD             S             10  0 DIM(4)
013200090930     D DSD             S              8  0 DIM(4)
013300990429     D*
013400990409     D POM             S              1    DIM(31)
013500970224     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
013600971125     D DSLV55        E DS                  EXTNAME(FNLV55DS)
013700000404     D DSUL06        E DS                  EXTNAME(TRUL06DS)
013800000404     D  LIN                    1     90  0
013900000404     D                                     DIM(30)
014000970224     D KPJBA         E DS
014100970224     D CNCR80        E DS
014200970401     D DS05          E DS
014300090930     D Dvpodeco      E DS
014400970401     D DS17          E DS
014500990409     D DS1Y          E DS
014600970603     D OG148         E DS
014700000626     D OG143         E DS
014800090605     d ds5e          e ds
014900090930     D* DS PER TIBS02R - GESTIONE TNTBE00F
015000090930     D DSBS02        E DS                  EXTNAME(TIBS02DS)
015100060131     d
015200970224     D WLBDA8          DS
015300970224     D  G08DAT                 1      8  0
015400970224     D  G08INV                 9     16  0
015500970224     D  G08ERR                17     17
015600970224     D  G08TGI                18     22  0
015700970527     D WGIDAT          DS
015800970527     D  GI8DAT                 1      8  0
015900970527     D  GI8INV                 9     16  0
016000990409     D  WAA                    9     12  0
016100990409     D  WMM                   13     14  0
016200990409     D  WGG                   15     16  0
016300970527     D  GI8TGI                17     21  0
016400970527     D                 DS
016500970527     D  PRTDAT                 1      4  0 INZ
016600970527     D  PRTGG                  1      2  0
016700970527     D  PRTMM                  3      4  0
016800970225     D PARAM           DS
016900970225     D  V1CDST                 1      1
017000970225     D  PARCAR                 2      4  0
017100970225     D  V1CLIN                 5      7  0
017200970225     D  PARDEL                 8     15  0
017300970225     D  V1CDEL                16     23  0
017400990427     D  V1CIND                24     24
017500970407     D  V1CTSP                25     25
017600970526     D  V1CDAL                26     33  0
017700970526     D  PARDAL                34     41  0
017800970527     D  PARTGI                42     46  0
017900990427     D  V1CINA                47     47
018000990427     D  V1CINN                48     48
018100990427     D  V1CNTW                49     49
018200990427     D  V1CCRP                50     50
018300091001     d
018400091001     d $v              s              2  0
018500091001     d
018600990503     D COS1            C                   CONST('        di cui BAR  -
018700990503     D                                      ')
018800990503     D COS2            C                   CONST('        di cui S.D.I-
018900990503     D                                     .')
019000990503     D COS3            C                   CONST('AREA                -
019100990430     D                                      ')
019200990503     D COS4            C                   CONST('DISTRETTO           -
019300990430     D                                      ')
019400970228      *---------------------------------------------------------------*
019500970228      * INDICATORI USATI
019600970228      *---------------------------------------------------------------*
019700970228      * 01 = RICHIESTO UN P.O. SPECIFICO
019800970228      * 02 = RICHIESTO UN COD.AREA SPECIFICO
019900970228      * 03 = RICHIESTO UN COD.DISTRETTO SPECIFICO
020000970526      * 05 = RICHIESTO UN PERIODO DAL/AL
020100970527      * 06 = NON STAMPA COD+DECOD. DEL P.O.
020200990430      * 11 = ON NON PERVENUTI DATI DEL PARTITO
020300990430      * 12 = ON NON PERVENUTI DATI DELL' A TERRA
020400990430      * 13 = ON NON PERVENUTI DATI DEL BOLLETTATO
020500990430      * 14 = ON NON PERVENUTI DATI DEL CONSEGNATO
020600050831      * 15 = ON NON PERVENUTO DATO DEI RITIRI NON EFFETTUATI - eliminato
020700990430      * 16 = ON NON PERVENUTI DATI DELLA STATISTICA ARRIVI
020800990430      * 17 = ON NON PERVENUTO DATO ESECUZIONE IDD
020900990430      * 18 = NON FA MAI L'IDD
021000970228      * 30 = DI COMODO
021100970228      * 31 = DI COMODO
021200970228      * 40 = ON STAMPA RIGA DI TOTALE PER PUNTO OPERATIVO
021300990503      * 41 = CONDIZIONAMENTO E HI DICITURE IN STAMPA
021400970228      *---------------------------------------------------------------*
021500970227     C*
021600970526     C* RICHIESTO UN PERIODO DAL/AL      --> 05 ON
021700970527     C     PARDAL        COMP      PARDEL                             05
021800970526     C*
021900970225     C* RICHIESTO UN P.O. SPECIFICO      --> 01 ON
022000970225     C     V1CLIN        COMP      0                                  01
022100970226     C   01              MOVEL(P)  V1CLIN        PRTLIN
022200970226     C  N01              MOVEL     'TUTTI'       PRTLIN
022300970225     C* RICHIESTO UN COD.AREA SPECIFICO  --> 02 ON
022400970225     C     PARCAR        COMP      0                                  02
022500970226     C   02              MOVEL(P)  PARCAR        PRTCA1
022600970226     C  N02              MOVEL     'TUTTI'       PRTCA1
022700970225     C* RICHIESTO UN DISTRETTO SPECIFICO --> 03 ON
022800970227     C     V1CDST        COMP      *BLANKS                            0303
022900970226     C   03              MOVEL(P)  V1CDST        PRTDST
023000970226     C  N03              MOVEL     'TUTTI'       PRTDST
023100970227     C* STAMPO INTESTAZIONE
023200970227     C                   WRITE     LV66T1
023300970227     C                   Z-ADD     7             $RIGHE            2 0
023400970225     C*
023500970225     C   01V1CLIN        CHAIN     AZORG01L                           30
023600970314     C  N01
023700970314     CANN02V1CDST        SETLL     AZORG02L                           30
023800970226     C  N01
023900970226     CAN 02KORG2         SETLL     AZORG02L                           30
024000970225     C  N01
024100970225     CANN02
024200970225     CANN03              READ      AZORG02L                               30
024300970314     C  N01
024400970314     CANN02
024500970314     CAN 03V1CDST        READE     AZORG02L                               30
024600970225     C  N01
024700970225     CAN 02KORG2         READE     AZORG02L                               30
024800970227    1C     *IN30         DOWEQ     *OFF
024900980407     C* ESCLUDO RECORD ANNULLATI E DISTRETTO SEDE
025000970311    2C     ORGFAG        IFEQ      'F'
025100970311     C     ORGFVA        ANDEQ     *BLANKS
025200970311     C     ORGFL3        ANDNE     '9'
025300970311     C     ORGFAG        OREQ      'A'
025400970311     C     ORGFVA        ANDEQ     *BLANKS
025500970227     C     ORGFL3        ANDNE     '9'
025600970603     C                   MOVEL     ORGDE8        OG148
025700000626     C                   MOVEL     ORGDE3        OG143
025800970603   2AC     §OGLFV        IFEQ      *BLANKS
025900970603     C     *IN01         OREQ      *ON
026000000626     C* ESCLUDO I P.O. POSTE
026100000626   2BC     §OGPT         IFNE      'S'
026200970526     C*
026300990503     C* VERIFICO ROTTURA DI DISTRETTO E AREA
026400970227    3C     ORGFL3        IFNE      PRTFL3
026500970226     C                   MOVE      '1'           WBRK
026600990623     C                   MOVE      '1'           WBRKPE
026700990623     C                   MOVE      '1'           WBRKPA
026800990623     C                   MOVE      '1'           WBRKPD
026900970401     C                   MOVE      '1'           WBRKD
027000990428     C                   MOVE      '1'           WBRKN
027100990428     C                   MOVE      ' '           WPRTN
027200970227    3C                   END
027300970227    3C     ORGCAR        IFNE      PRTCAR
027400970226     C                   MOVE      '1'           WBRK
027500990623     C                   MOVE      '1'           WBRKPE
027600990623     C                   MOVE      '1'           WBRKPA
027700990623     C                   MOVE      '1'           WBRKPD
027800970401     C                   MOVE      '1'           WBRKA
027900990428     C                   MOVE      '1'           WBRKN
028000990428     C                   MOVE      ' '           WPRTN
028100970227    3C                   END
028200990429     C*
028300990429     C                   MOVEL     ORGDIT        COMDIT
028400990428     C* MEMORIZZO SE P.O. RELATIVO AL NETWORK RICHIESTO
028500990428    3C     V1CINN        IFEQ      'S'
028600990428    4C     COMDIT        IFNE      V1CNTW
028700990428     C                   MOVE      ' '           WBRKN1            1
028800990428     C                   ELSE
028900990428     C                   MOVE      '1'           WBRKN1
029000990428    4C                   END
029100990428    3C                   END
029200990429     C*
029300990429     C* ROTTURA DI DISTRETTO O AREA:
029400970227    3C     WBRK          IFEQ      '1'
029500990429     C* - STAMPO I TOTALI
029600990503    4C     WPRT          IFEQ      '1'
029700990429     C                   EXSR      ELATAR
029800990503    5C     WBRKD         IFEQ      '1'
029900990429     C                   EXSR      ELATDS
030000990503    5C                   END
030100990503     C* Stampo 2 righe vuote
030200990503    5C     $RIGHE        IFLE      63
030300990503     C                   WRITE     LV66T2
030400990503     C                   WRITE     LV66T2
030500990503     C                   ADD       2             $RIGHE
030600990503    5C                   END
030700990503    4C                   END
030800990503     C*
030900990429     C                   CLEAR                   WPRT
031000990503     C*
031100990503     C     WBRKD         IFEQ      '1'
031200990503     C* Decodifico distretto
031300990503     C                   EXSR      DECDST
031400990503     C                   END
031500990503     C     WBRKA         IFEQ      '1'
031600990503     C* Decodifico area
031700990503     C                   EXSR      DECCAR
031800990503     C                   END
031900990429     C* - STAMPO DECODIFICHE DISTRETTO E AREA
032000970227     C                   ADD       3             $RIGHE
032100970227     C* Salto pagina se ho superato il n° massimo di righe
032200970227    4C     $RIGHE        IFGT      60
032300970226     C                   WRITE     LV66T1
032400970227     C                   Z-ADD     10            $RIGHE
032500970227    4C                   END
032600970227     C*
032700970227     C                   WRITE     LV66T3
032800970226     C                   WRITE     LV66T4
032900970227     C                   WRITE     LV66T3
033000970401     C* SE NON C'E' STATA LA ROTTURA DEL COD.DISTRETTO STAMPO DECOD.
033100970401     C* ANCHE IN SPOOL PER IL CAPO DISTRETTO
033200990428     C     V1CIND        IFEQ      'S'
033300970401     C     WBRKD         ANDEQ     *BLANKS
033400970401     C                   ADD       3             $RIGHD
033500970401     C* Salto pagina se ho superato il n° massimo di righe
033600970401    4C     $RIGHD        IFGT      60
033700970401     C                   WRITE     LV66T1D
033800970401     C                   Z-ADD     10            $RIGHD
033900970401    4C                   END
034000970401     C*
034100970401     C                   WRITE     LV66T3D
034200970401     C                   WRITE     LV66T4D
034300970401     C                   WRITE     LV66T3D
034400970401     C                   END
034500970227    3C                   END
034600970401     C* SE RICHIESTO INVIO STAMPE RICHIAMO ROUTINE DI PREPARAZIONE
034700970401     C* SPOOL DA INVIARE
034800990428     C     V1CIND        IFEQ      'S'
034900990428     C     WBRKD         ANDEQ     '1'
035000970401     C                   EXSR      OVPRTD
035100970401     C                   END
035200990428     C     V1CINA        IFEQ      'S'
035300990428     C     WBRKA         ANDEQ     '1'
035400970401     C                   EXSR      OVPRTA
035500970401     C                   END
035600990428     C     V1CINN        IFEQ      'S'
035700990428     C     WBRKN         ANDEQ     '1'
035800990428     C     WBRKN1        ANDEQ     '1'
035900990428     C                   EXSR      OVPRTN
036000990428     C                   END
036100970226     C*
036200970306     C     ORGDE5        IFNE      *BLANKS
036300970306     C                   MOVEL     ORGDE5        ORGDEP
036400970306     C                   ELSE
036500970306     C                   MOVEL     ORGDES        ORGDEP
036600970306     C                   END
036700970527     C* CICLO DI ELABORAZIONE DEL P.O. PER IL PERIODO DAL/AL RICHIESTO
036800970527     C                   Z-ADD     PARDEL        WDATEL
036900970527     C                   MOVE      PARTGI        WTGI
037000970527     C                   SETOFF                                       06
037100000208     C                   CLEAR                   SAVTFP
037200000208     C                   CLEAR                   SAVTFA
037300970527     C     WDATEL        DOWLE     PARDAL
037400990503     c* Se vengono creati i dati li devo anche cancellare prima
037500990503     C     V1CCRP        IFEQ      'S'
037600990503     C     *IN31         DOUEQ     '1'
037700010124     C     KRCQ          DELETE    WFRCQ11L                           31
037800990503     C                   ENDDO
037900990503     C                   END
038000970226     C* Pulisco campi e schiere di calcolo
038100970226     C                   CLEAR                   BOL
038200970226     C                   CLEAR                   BSS
038300970226     C                   CLEAR                   SDA
038400970226     C                   CLEAR                   FAF
038500970226     C                   CLEAR                   SPA
038600970226     C                   CLEAR                   NOC
038700970226     C                   CLEAR                   CSB
038800010124     C                   CLEAR                   DSC
038900010124     C                   CLEAR                   DSR
039000970226     C                   CLEAR                   TP88B
039100970226     C                   CLEAR                   TP88A
039200970226     C                   CLEAR                   TOTPAR
039300970226     C                   CLEAR                   TOTRIT
039400970226     C                   CLEAR                   TOTBAM
039500970226     C                   CLEAR                   TOTDIS
039600040405     C                   CLEAR                   TOTDISP
039700970226     C                   CLEAR                   TOTDAR
039800970606     C                   CLEAR                   TOTNAR
039900970606     C                   CLEAR                   TOTARR
040000000208     C* DETERMINO TERMINAL DI PARTENZA E DI ARRIVO
040100000208     C                   SETOFF                                       07
040200000208     C                   CLEAR                   DSLV55
040300000208     C                   MOVE      ORGFIL        D55LIN
040400000208     C                   Z-ADD     WDATEL        D55DRF
040500000208     C                   CALL      'FNLV55R'
040600000208     C                   PARM                    DSLV55
040700000208     C                   MOVE      D55TFP        PRTTFP
040800000208     C                   MOVE      D55TFA        PRTTFA
040900000208     C* VEDO SE I TERMINAL SONO CAMBIATI (SERVE SE RICHIESTO UN PERIODO
041000000208     C* DAL AL)
041100000208     C     PRTTFP        IFNE      SAVTFP
041200000208     C                   SETON                                        07
041300000208     C                   Z-ADD     PRTTFP        SAVTFP
041400000208     C                   ENDIF
041500000208     C     PRTTFA        IFNE      SAVTFA
041600000208     C                   SETON                                        07
041700000208     C                   Z-ADD     PRTTFA        SAVTFA
041800000208     C                   ENDIF
041900970226     C* CALCOLO RITIRI NON ESEGUITI,DISGUIDI,DISGUIDI IN AREA,MANCA BAM
042000970225     C                   EXSR      ELAANT
042100970226     C* CALCOLO BOLLETTATO IN SOSPENSIONE
042200970225     C                   EXSR      ELASBO
042300990504     C***
042400990504     C* I DATI RELATIVI ALLE PARTENZE LI CALCOLO SOLO SE IL P.O. LETTO
042500060131     C* E' UN TERMINAL DI PARTENZA o un p.o. estero dalla data decorrenza
042600990504     C***
042700060131     c     orgfil        lookup    est                                    32
042800990504     C     ORGFIL        IFEQ      PRTTFP
042900060131     c     *in32         oreq      *on
043000070510     c***  wdatel        andge     §vpoqua
043100060131     c
043200990504     C* CALCOLO COLLI PARTITI CON PISTOLA 88
043300970225     C                   EXSR      ELASPA
043400970306     C* CALCOLO SPEDIZIONI A TERRA FUORI AFFIDABILITA' E SPAREGGIATE
043500970225     C                   EXSR      ELASAT
043600990506     C                   SETOFF                                       42
043700990505     C                   ELSE
043800990505     C* Se per il p.o. letto non devo calcolare i dati relativi alle
043900060131     C* partenze imposto flag='2' per non stampare "dati non pervenuti"
044000060131     C                   MOVE      '2'           WTSPA
044100060131     C                   MOVE      '2'           WTSAT
044200990506     C                   SETON                                        42
044300990504     C                   END
044400970606     C* CALCOLO COLLI PARTITI NON ARRIVATI AL TERMINAL
044500990504     C* (SOLO SE IL P.O. LETTO E' TERMINAL DI ARRIVO)
044600990504     C                   EXSR      CHKTFA
044700990504     C     WTARR         IFEQ      '1'
044800970606     C                   EXSR      ELASAR
044900990506     C                   SETOFF                                       43
045000990506     C                   ELSE
045100990506     C                   MOVE      '1'           WTSAR
045200990506     C                   SETON                                        43
045300990504     C                   END
045400970226     C* CALCOLO SPEDIZIONI NON CONSEGNATE FUORI AFFIDABILITA'
045500970225     C                   EXSR      ELASCO
045600990409     C* CALCOLO SE ESEGUITO IDD O MENO
045700990409     C                   EXSR      ELAIDD
045800970226     C* STAMPO PER PUNTO OPERATIVO
045900970225     C                   EXSR      PRTLNP
046000970527     C**
046100970527     C* INCREMENTO LA DATA (WDATEL) DI UN GIORNO
046200970527     C                   CLEAR                   WGIDAT
046300970527     C                   ADD       1             WTGI
046400970527     C                   Z-ADD     WTGI          GI8TGI
046500970527     C                   CALL      'XSRGI8'
046600970527     C                   PARM                    WGIDAT
046700970527     C                   MOVE      GI8INV        WDATEL
046800970527     C*
046900970527     C                   MOVE      *BLANKS       WBRK              1
047000990623     C                   MOVE      *BLANKS       WBRKPE            1
047100990623     C                   MOVE      *BLANKS       WBRKPA            1
047200990623     C                   MOVE      *BLANKS       WBRKPD            1
047300970527     C                   MOVE      *BLANKS       WBRKA             1
047400970527     C                   MOVE      *BLANKS       WBRKD             1
047500990428     C     WBRKN1        IFEQ      '1'
047600990428     C                   MOVE      *BLANKS       WBRKN             1
047700990428     C                   END
047800970527     C**
047900970527     C                   ENDDO
048000970527     C****
048100970527     C* STAMPO TOTALE PUNTO OPERATIVO SE RICHIESTO UN PERIODO DAL/AL
048200970527     C     *IN05         IFEQ      *ON
048300970527     C                   EXSR      PRTTOF
048400970527     C                   END
048500970527     C****
048600970225     C*
048700000626   2BC                   END
048800000626   2AC                   END
048900970227    2C                   END
049000970225     C   01              SETON                                        30
049100970225     C  N01
049200970225     CANN02
049300970225     CANN03              READ      AZORG02L                               30
049400970314     C  N01
049500970314     CANN02
049600970314     CAN 03V1CDST        READE     AZORG02L                               30
049700970225     C  N01
049800970225     CAN 02KORG2         READE     AZORG02L                               30
049900970227    1C                   ENDDO
050000990505     C* - STAMPO I TOTALI (SE NON RICHIESTO UN P.O. SPECIFICO)
050100990429     C     WPRT          IFEQ      '1'
050200990505     C     *IN01         ANDEQ     *OFF
050300990429     C                   EXSR      ELATAR
050400990505     C* STAMPO TOT.DISTR.SOLO SE NON E' STATA ELABOR. UN'AREA SPECIFICA
050500990505     C  N02              EXSR      ELATDS
050600990429     C                   END
050700970226     C* STAMPO RIGA "FINE STAMPA"
050800970227     C                   ADD       3             $RIGHE
050900970227    1C     $RIGHE        IFGT      65
051000970227     C                   WRITE     LV66T1
051100970227     C                   WRITE     LV66T3
051200970227    1C                   END
051300970226     C                   WRITE     LV66E2
051400970401     C* STAMPO RIGA "FINE STAMPA" IN SPOOL PER CAPO AREA E CAPO DISTR.
051500990428    1C     V1CINA        IFEQ      'S'
051600970401     C                   ADD       3             $RIGHA
051700970401    2C     $RIGHA        IFGT      65
051800970401     C                   WRITE     LV66T1A
051900970401     C                   WRITE     LV66T3A
052000970401    2C                   END
052100970401     C                   WRITE     LV66E2A
052200990428    1C                   END
052300990428    1C     V1CIND        IFEQ      'S'
052400970401     C                   ADD       3             $RIGHD
052500970401    2C     $RIGHD        IFGT      65
052600970401     C                   WRITE     LV66T1D
052700970401     C                   WRITE     LV66T3D
052800970401    2C                   END
052900970401     C                   WRITE     LV66E2D
053000970401    1C                   END
053100990428    1C     V1CINN        IFEQ      'S'
053200990428     C     WPRTN         ANDEQ     '1'
053300990428     C                   ADD       3             $RIGHN
053400990428    2C     $RIGHN        IFGT      65
053500990428     C                   WRITE     LV66T1N
053600990428     C                   WRITE     LV66T3N
053700990428    2C                   END
053800990428     C                   WRITE     LV66E2N
053900990428    1C                   END
054000971125     C*
054100971125     C                   MOVEL     'C'           D55TLA
054200971125     C                   CALL      'FNLV55R'
054300971125     C                   PARM                    DSLV55
054400970227     C*
054500970224     C                   SETON                                        LR
054600990504     C*****************************************************************
054700990504     C* VERIFICA SE P.O. LETTO E' TERMINAL DI ARRIVO
054800990504     C*****************************************************************
054900990504     C     CHKTFA        BEGSR
055000990504     C                   MOVEL     'A'           KEPA
055100990504     C                   MOVE      *BLANKS       WTARR             1
055200990504     C     KCAE5         SETLL     AZCAE05L
055300990504     C     KCAE5         READE     AZCAE05L                               31
055400990504     C     *IN31         DOWEQ     *OFF
055500990504     C     WTARR         ANDEQ     *BLANKS
055600990504     C     CAEATB        IFEQ      *BLANKS
055700990504     C     WDATEL        ANDGE     CAEDDE
055800990504     C     WDATEL        ANDLE     CAEDSC
055900990504     C                   MOVEL     '1'           WTARR
056000990504     C                   END
056100990504     C     KCAE5         READE     AZCAE05L                               31
056200990504     C                   ENDDO
056300990504     C                   ENDSR
056400970226     C*****************************************************************
056500970226     C* DETERMINAZIONE DISGUIDI,MANCA BAM,RITIRI NON EFFETTUATI
056600970226     C*****************************************************************
056700970226     C     ELAANT        BEGSR
056800050831     C* RITIRI NON ESEGUITI : non esiste più tolto 31/8/05    EDPES
056900000404     C* Se p.o. in £6 (gestito da altri) non segnalo dati non pervenuti
057000050831     C**                 Z-ADD     300           WCAA
057100050831     C**                 CLEAR                   WT300
057200050831     C**   ORGFIL        LOOKUP    NOG                                    31
057300050831    1C**   *IN31         IFEQ      *OFF
057400050831     C**   KANT          CHAIN     FNANT01L                           31
057500050831    2C**   *IN31         IFEQ      *OFF
057600050831     C**                 Z-ADD     ANTTOT        TOTRIT
057700050831     C**                 MOVE      '1'           WT300             1
057800050831    2C**                 END
057900050831   X1C**                 ELSE
058000050831     C**                 CLEAR                   TOTRIT
058100050831     C**                 MOVE      '1'           WT300             1
058200050831    1C**                 ENDIF
058300970305     C* MANCA DDT
058400970226     C                   Z-ADD     71            WCAA
058500970226     C     KANT          SETLL     FNANT01L
058600970226     C     KANT          READE     FNANT01L                               31
058700970226     C     *IN31         DOWEQ     *OFF
058800970226     C                   ADD       ANTTOT        TOTBAM
058900970226     C     KANT          READE     FNANT01L                               31
059000970226     C                   ENDDO
059100970226     C* DISGUIDI
059200990419     C     WDATEL        IFLE      19990418
059300970226     C                   Z-ADD     55            WCAA
059400990419     C                   ELSE
059500990419     C                   Z-ADD     56            WCAA
059600990419     C                   ENDIF
059700990419     C*
059800990504     C     KANT          SETLL     FNANT04L
059900990504     C     KANT          READE     FNANT04L                               31
060000970226     C     *IN31         DOWEQ     *OFF
060100970226     C                   ADD       ANTTOT        TOTDIS
060200990504     C     KANT          READE     FNANT04L                               31
060300970226     C                   ENDDO
060400040405     C* DISGUIDI non spuntati in partenza
060500040405     C                   Z-ADD     200           WCAA
060600040405     C*
060700040421     C     KANT          SETLL     FNANT02L
060800040421     C     KANT          READE     FNANT02L                               31
060900040405     C     *IN31         DOWEQ     *OFF
061000040405     C                   ADD       ANTTOT        TOTDISP
061100040421     C     KANT          READE     FNANT02L                               31
061200040405     C                   ENDDO
061300970226     C* DISGUIDI IN AREA
061400970226     C                   Z-ADD     145           WCAA
061500970226     C                   DO        2
061600970226     C     KANT          SETLL     FNANT02L
061700970226     C     KANT          READE     FNANT02L                               31
061800970226     C     *IN31         DOWEQ     *OFF
061900970226     C                   ADD       ANTTOT        TOTDAR
062000970226     C     KANT          READE     FNANT02L                               31
062100970226     C                   ENDDO
062200970226     C                   Z-ADD     146           WCAA
062300970226     C                   ENDDO
062400970226     C                   ENDSR
062500970225     C*****************************************************************
062600970225     C* DETERMINAZIONE BOLLETTATO IN SOSPENSIONE
062700970225     C*****************************************************************
062800970225     C     ELASBO        BEGSR
062900970225     C                   MOVE      'S'           WSOS
063000090911     C     KSBO          SETLL     FiSBO02L
063100090911     C     KSBO          READE     FiSBO02L                               31
063200970225     C     *IN31         DOWEQ     *OFF
063300090930     C* H1030
063400090930     C                   ADD       SBOShA        BSS(1)
063500090930     C                   ADD       SBOShF        BSS(1)
063600090930     C* ESPR
063700090930     C                   ADD       SBOSEA        BSS(2)
063800090930     C                   ADD       SBOSEF        BSS(2)
063900970225     C* CORR
064000090930     C                   ADD       SBOSNA        BSS(3)
064100090930     C                   ADD       SBOSNF        BSS(3)
064200970305     C* DIST
064300090930     C                   ADD       SBOSSA        BSS(4)
064400090930     C                   ADD       SBOSSF        BSS(4)
064500970225     C*
064600090911     C     KSBO          READE     FiSBO02L                               31
064700970225     C                   ENDDO
064800970225     C* CALCOLO ANCHE TOTALE BOLLETTATO NEL GIORNO
064900970225     C                   MOVE      ' '           WSOS
065000970226     C                   CLEAR                   WTSBO
065100090911     C     KSBO          SETLL     FiSBO02L
065200090911     C     KSBO          READE     Fisbo02L                               31
065300970225     C     *IN31         DOWEQ     *OFF
065400970226     C                   MOVE      '1'           WTSBO             1
065500090930     C* H1030
065600090930     C                   ADD       SBOShA        BOL(1)
065700090930     C                   ADD       SBOShF        BOL(1)
065800090930     C* ESPR
065900090930     C                   ADD       SBOSEA        BOL(2)
066000090930     C                   ADD       SBOSEF        BOL(2)
066100970225     C* CORR
066200090930     C                   ADD       SBOSNA        BOL(3)
066300090930     C                   ADD       SBOSNF        BOL(3)
066400970305     C* DIST
066500090930     C                   ADD       SBOSSA        BOL(4)
066600090930     C                   ADD       SBOSSF        BOL(4)
066700970225     C*
066800090911     C     KSBO          READE     FiSBO02L                               31
066900970225     C                   ENDDO
067000970225     C                   ENDSR
067100970226     C*****************************************************************
067200970306     C* DETERMINAZIONE COLLI PARTITI CON PISTOLA 88
067300970226     C*****************************************************************
067400970226     C     ELASPA        BEGSR
067500970226     C                   CLEAR                   WTSPA
067600070510     c                   if        orgfil=prttfp
067700090915     C     KSPA          SETLL     FISPA02L
067800090915     C     KSPA          READE     FISPA02L                               31
067900060131     c                   else
068000060131     c* Per  l'estero divido i dati per p.o.
068100090915     C     KSPAfil       SETLL     FISPA01L
068200090915     C     KSPAfil       READE     FISPA01L                               31
068300060131     c                   endif
068400060131     c
068500000626    1C     *IN31         DOWEQ     *OFF
068600970305     C* ESCLUDO I TRANSITI
068700000626    2C     SPATRE        IFNE      'T'
068800000626     C* ESCLUDO IL P.O. PARTENZA POSTE
068900000626     C     SPALNP        LOOKUP    PT                                     32
069000060131     c
069100060131     c* Se da elaborare a parte escludo i p.o. esteri
069200070510     C  n32              if        orgfil=prttfp
069300060131     C     SPALNP        LOOKUP    EST                                    32
069400060131     c                   endif
069500060131     c
069600000626    3C     *IN32         IFEQ      *OFF
069700970226     C                   MOVE      '1'           WTSPA             1
069800990504     C*  PISTOLA 88
069900970226     C                   ADD       SPA88B        TP88B
070000970226     C                   ADD       SPA88A        TP88A
070100970226     C                   ADD       SPACPC        TOTPAR
070200970226     C                   ADD       SPACPE        TOTPAR
070300970226     C                   ADD       SPACPD        TOTPAR
070400090915     C                   ADD       SPACPh        TOTPAR
070500000626    3C                   END
070600000626    2C                   END
070700070510     c                   if        orgfil=prttfp
070800090915     C     KSPA          READE     FISPA02L                               31
070900060131     c                   else
071000090915     C     KSPAfil       READE     FISPA01L                               31
071100060131     c                   endif
071200060131     c
071300000626    1C                   ENDDO
071400970226     C                   ENDSR
071500970226     C*****************************************************************
071600970226     C* DETERMINAZIONE SPEDIZIONI A TERRA SPAREGGIATE
071700970226     C*****************************************************************
071800970226     C     ELASAT        BEGSR
071900970226     C                   CLEAR                   WTSAT
072000070510     c****               if        orgfil=prttfp or wdatel<§vpoqua
072100070510     c                   if        orgfil=prttfp
072200090916     C     KSAT          SETLL     fisat03L
072300090916     C     KSAT          READE     fisat03L                               31
072400060131     c                   else
072500090916     C     KSATfil       SETLL     fisat02L
072600090916     C     KSATfil       READE     fisat02L                               31
072700060131     c                   endif
072800000626    1C     *IN31         DOWEQ     *OFF
072900970226     C                   MOVE      '1'           WTSAT             1
073000000626     C* ESCLUDO I P.O. PARTENZA POSTE
073100000626     C     SATLNP        LOOKUP    PT                                     32
073200060131     c
073300060131     c* Se da elaborare a parte escludo i p.o. esteri
073400070510     c**n32              if        orgfil=prttfp  and wdatel>=§vpoqua
073500070510     c  n32              if        orgfil=prttfp
073600060131     C     SatLNP        LOOKUP    EST                                    32
073700060131     c                   endif
073800060131     c
073900000626    2C     *IN32         IFEQ      *OFF
074000090930     C* SPAREGGIATE H1030
074100091001     C     SATSDh        ADD       SATSnh        WTERR
074200090930     C     WTERR         SUB       SATSch        WSPAR
074300970226     C                   ADD       WSPAR         SPA(1)
074400090930     C* SPAREGGIATE ESPRESSO
074500090930     C     SATSDE        ADD       SATSNE        WTERR
074600090930     C     WTERR         SUB       SATSCE        WSPAR
074700090930     C                   ADD       WSPAR         SPA(2)
074800970305     C* SPAREGGIATE CORRIERE
074900970305     C     SATSDC        ADD       SATSNC        WTERR
075000970305     C     WTERR         SUB       SATSCC        WSPAR
075100090930     C                   ADD       WSPAR         SPA(3)
075200970226     C* SPAREGGIATE DISTRIBUZIONE
075300970226     C     SATSDD        ADD       SATSND        WTERR
075400970226     C     WTERR         SUB       SATSCD        WSPAR
075500090930     C                   ADD       WSPAR         SPA(4)
075600970306     C*
075700970306     C* SPEDIZIONI FUORI AFFIDABILITA'
075800090930     C                   ADD       SATSFH        FAF(1)
075900090930     C                   ADD       SATSFE        FAF(2)
076000090930     C                   ADD       SATSFC        FAF(3)
076100090930     C                   ADD       SATSFD        FAF(4)
076200970306     C* TOTALE SPEDIZIONI DA PARTIRE
076300090930     C                   ADD       SATSPh        SDA(1)
076400090930     C                   ADD       SATSPE        SDA(2)
076500090930     C                   ADD       SATSPC        SDA(3)
076600090930     C                   ADD       SATSPD        SDA(4)
076700000626     C                   ENDIF
076800970306     C*
076900070510     c****               if        orgfil=prttfp or wdatel<§vpoqua
077000070510     c                   if        orgfil=prttfp
077100090916     C     KSAT          READE     fisat03L                               31
077200060131     c                   else
077300090916     C     KSATfil       READE     fisat02L                               31
077400060131     c                   endif
077500970226     C                   ENDDO
077600970311     C* SE NON TROVATO RECORD CERCO QUELLO FESTIVO
077700990504     C***        WTSAT     IFEQ *BLANKS
077800990504     C***                  MOVEL'F'       WTRE
077900090916     C***        KSAT2     CHAINfisat03L             31
078000990504     C**N31                MOVE '1'       WTSAT
078100990504     C***                  END
078200970226     C                   ENDSR
078300970606     C*****************************************************************
078400970606     C* DETERMINAZIONE COLLI PARTITI NON ARRIVATI AL TERMINAL
078500970606     C*****************************************************************
078600970606     C     ELASAR        BEGSR
078700970606     C                   CLEAR                   WTSAR
078800990504     C******
078900090930     C* N.B.: QUESTA ROUTINE PRIMA LEGGEVA FISAR05L ORDINATO
079000090930     C* PER LNA+TRE+DRE. ORA LEGGE FISAR06L ORDINATO PER TFA+TRE+DRE
079100990504     C* E' COMUNQUE STATA MANTENUTA LA STRUTTURA ORIGINARIA NEL
079200990504     C* CASO SI VOLESSE RITORNARE AD ATTRIBUIRE COLLI PARTITI NON
079300990504     C* SPUNTATI IN ARRIVO AL RELATIVO P.O. ANZICHE' AL TERMINAL DI ARR
079400990504     C* ANCHE I COMMENTI SOTTO RIPORTATI SI RIFERISCONO ALLA STRUTTURA
079500990504     C* ORIGINARIA DELLA ROUTINE
079600990504     C******
079700970606     C* * * * * * * * * * * * *
079800970606     C* PRIMO CICLO DI LETTURA:
079900970606     C* Leggo records di tipo 'R' cioè del terminal-arr se su altro AS
080000970606     C* Di questi records non mi interessa la colonna dei colli non
080100970606     C* spuntati in partenza e nemmeno la colonna dei colli arrivati
080200970606     C* (Faccio una read anzichè chain perchè se ci fossero delle ecce-
080300970606     C* zioni terminal di arrivo potrei avere più di un record)
080400970606     C* * * * * * * * * * * * *
080500100701     c* ES 1/07/10 --> non si dovrebbero più creare
080600100701     C                   MOVEL     'R'           WTRE
080700100701     C     KSAR          SETLL     FISAR06L
080800100701     C     KSAR          READE     FISAR06L                               31
080900100701     C     *IN31         DOWEQ     *OFF
081000100701     C                   MOVE      '1'           WTSAR             1
081100970606     C                   ADD       SARPAR        TOTNAR
081200090930     C     KSAR          READE     FISAR06L                               31
081300970606     C                   ENDDO
081400970606     C* SECONDO CICLO DI LETTURA:
081500970606     C* Leggo records di tipo ' '
081600970606     C* Sommo i colli partiti non spuntati in arrivo e sottraggo i
081700970606     C* colli non spuntati in partenza e non arrivati
081800970606     C* (Se la lna avesse il terminal su altro As i colli partiti e
081900970606     C* non spuntati in arrivo sarà probabilmente = 0 su questo tipo
082000970606     C* record ma potrebbe anche essere > 0 se ci fossero eccezioni
082100970606     C* al terminal di arrivo)
082200970606     C* * * * * * * * * * * * *
082300970606     C                   MOVEL     ' '           WTRE
082400100701     c                   clear                   wlna              3 0
082500090930     C     KSAR          SETLL     FISAR06L
082600090930     C     KSAR          READE     FISAR06L                               31
082700970606     C     *IN31         DOWEQ     *OFF
082800100701     c
082900100701     c* Escludo i record "doppi"  avvero scritti sia dal terminal di arrivo
083000100701     c*  che di partenza, quando la fil ha i 2 terminal diversi o il giorno
083100100701     c*  che la fil cambia terminal di arrivo
083200100701     c                   if        wlna=0 or wlna<>sarlna
083300970606     C                   MOVE      '1'           WTSAR             1
083400970606     C                   ADD       SARPAR        TOTNAR
083500970606     C                   SUB       SARP88        TOTNAR
083600970606     C                   ADD       SARCLL        TOTARR
083700100701     c                   eval      wlna=sarlna
083800100701     c                   endif
083900100701     c
084000090930     C     KSAR          READE     FISAR06L                               31
084100970606     C                   ENDDO
084200970606     C* TERZO CICLO DI LETTURA:
084300970606     C* Leggo records di tipo 'T' cioè relativi ai colli in TRANSITO
084400970606     C* Sommo i colli partiti non spuntati in arrivo e sottraggo i
084500970606     C* colli non spuntati in partenza e non arrivati
084600970606     C* (Questo ciclo lo faccio solo se la filiale che sto elaborando
084700970606     C* è un primo livello)
084800970606     C* * * * * * * * * * * * *
084900990504     C     ORGFIL        IFEQ      PRTTFP
085000970606     C                   MOVEL     'T'           WTRE
085100090930     C     KSAR          SETLL     FISAR06L
085200090930     C     KSAR          READE     FISAR06L                               31
085300970606     C     *IN31         DOWEQ     *OFF
085400970606     C                   MOVE      '1'           WTSAR             1
085500970606     C                   ADD       SARPAR        TOTNAR
085600970606     C                   SUB       SARP88        TOTNAR
085700970606     C                   ADD       SARCLL        TOTARR
085800090930     C     KSAR          READE     FiSAR06L                               31
085900970606     C                   ENDDO
086000990504     C                   END
086100970610     C*
086200970610     C     TOTNAR        IFLT      *ZEROS
086300970610     C                   MOVE      *ZEROS        TOTNAR
086400970610     C                   END
086500970606     C                   ENDSR
086600970226     C*****************************************************************
086700970226     C* DETERMINAZIONE SPEDIZIONI NON CONSEGNATE FUORI AFFIDABILITA'
086800970226     C*****************************************************************
086900970226     C     ELASCO        BEGSR
087000970226     C* 1° CICLO: LETTURA "NON CONSEGNATE"
087100970226     C                   MOVEL     'N'           WSCO
087200970226    1C                   DO        4             II                2 0
087300970226     C* Giacenze in ritardo           GBO=G,ORE>0
087400970226     C* lasciati avviso in ritardo    GBO=L,ORE>0
087500970226     c* chiuso per turno in ritardo   GBO=C,ORE>0
087600970226     c* non fatte in ritardo+conformi GBO=N,ORE>=0
087700970226     C                   MOVEL     GBO(II)       WGBO
087800000208     C     KSCO          SETLL     FNSCO03L
087900000208     C     KSCO          READE     FNSCO03L                               31
088000970226    2C     *IN31         DOWEQ     *OFF
088100970226     C* escludo se olt = 'A' o 'T'
088200970226    3C     SCOOLT        IFNE      'A'
088300970226     C     SCOOLT        ANDNE     'T'
088400970226     c*
088500970226    4C     SCOORE        IFGT      *ZEROS
088600970226     C     SCOORE        OREQ      *ZEROS
088700970226     C     GBO(II)       ANDEQ     'N'
088800090930     C                   ADD       SCOSS4        NOC(1)
088900090930     C                   ADD       SCOSS1        NOC(2)
089000090930     C                   ADD       SCOSS2        NOC(3)
089100090930     C                   ADD       SCOSS3        NOC(4)
089200970226    4C                   END
089300970226    3C                   END
089400000208     C     KSCO          READE     FNSCO03L                               31
089500970226    2C                   ENDDO
089600970226    1C                   ENDDO
089700970226     C*
089800970226     C* 2° CICLO: LETTURA "IN DISTINTA"
089900970226     C                   MOVEL     'D'           WSCO
090000970226     C                   MOVE      *BLANKS       WGBO
090100000208     C     KSCO          SETLL     FNSCO03L
090200000208     C     KSCO          READE     FNSCO03L                               31
090300970226    1C     *IN31         DOWEQ     *OFF
090400970226     C* escludo se olt = 'A' o 'T'
090500970226    2C     SCOOLT        IFNE      'A'
090600970226     C     SCOOLT        ANDNE     'T'
090700010124     C     SCOORE        ANDGE     *ZEROS
090800090930     C                   ADD       SCOSS4        NOC(1)
090900090930     C                   ADD       SCOSS1        NOC(2)
091000090930     C                   ADD       SCOSS2        NOC(3)
091100090930     C                   ADD       SCOSS3        NOC(4)
091200010124     C* Memorizzo anche il" Di cui in distinta conforme"
091300010124     C* e il "Di cui in distinta in ritardo"
091400010124     C     SCOORE        IFEQ      *ZEROS
091500090930     C                   ADD       SCOSS4        DSC(1)
091600090930     C                   ADD       SCOSS1        DSC(2)
091700090930     C                   ADD       SCOSS2        DSC(3)
091800090930     C                   ADD       SCOSS3        DSC(4)
091900010124     C                   ELSE
092000090930     C                   ADD       SCOSS4        DSR(1)
092100090930     C                   ADD       SCOSS1        DSR(2)
092200090930     C                   ADD       SCOSS2        DSR(3)
092300090930     C                   ADD       SCOSS3        DSR(4)
092400010124     C                   END
092500970226    2C                   END
092600000208     C     KSCO          READE     FNSCO03L                               31
092700970226    1C                   ENDDO
092800970226     C* 3° CICLO: LETTURA "NON IN CONSEGNA"
092900970226     C                   MOVEL     'O'           WSCO
093000970226     C                   MOVE      *BLANKS       WGBO
093100000208     C     KSCO          SETLL     FNSCO03L
093200000208     C     KSCO          READE     FNSCO03L                               31
093300970226    1C     *IN31         DOWEQ     *OFF
093400970226     C* escludo se olt = 'A' o 'T'
093500970226    2C     SCOOLT        IFNE      'A'
093600970226     C     SCOOLT        ANDNE     'T'
093700970226     C     SCOORE        ANDGE     *ZEROS
093800090930     C                   ADD       SCOSS4        NOC(1)
093900090930     C                   ADD       SCOSS1        NOC(2)
094000090930     C                   ADD       SCOSS2        NOC(3)
094100090930     C                   ADD       SCOSS3        NOC(4)
094200970226    2C                   END
094300000208     C     KSCO          READE     FNSCO03L                               31
094400970226    1C                   ENDDO
094500970226     C* 4° CICLO: LETTURA "CONSEGNABILI"
094600970226     C                   CLEAR                   WTSCO
094700970226     C                   MOVEL     'B'           WSCO
094800970226     C                   MOVE      *BLANKS       WGBO
094900000208     C     KSCO          SETLL     FNSCO03L
095000000208     C     KSCO          READE     FNSCO03L                               31
095100970226    1C     *IN31         DOWEQ     *OFF
095200970226     C* seleziono olt = 'T'
095300970226     C     SCOOLT        IFEQ      'T'
095400970226     C                   MOVE      '1'           WTSCO             1
095500090930     C                   ADD       SCOSS4        CSB(1)
095600090930     C                   ADD       SCOSS1        CSB(2)
095700090930     C                   ADD       SCOSS2        CSB(3)
095800090930     C                   ADD       SCOSS3        CSB(4)
095900970226    2C                   END
096000000208     C     KSCO          READE     FNSCO03L                               31
096100970226    1C                   ENDDO
096200970226     C*
096300970226     C     WTSCO         IFEQ      *BLANKS
096400970226     C                   MOVE      'F'           WSCO
096500970226     C                   MOVE      *BLANKS       WGBO
096600000208     C     KSCO          CHAIN     FNSCO03L                           31
096700970226     C     *IN31         IFEQ      *OFF
096800970226     C                   MOVE      '1'           WTSCO
096900970226     C                   END
097000970226     C                   END
097100970226     C*
097200970226     C                   ENDSR
097300990409     C*****************************************************************
097400990409     C* RICERCA DATA IDD-1 X I P.O. CHE LO FANNO IL GG DOPO
097500990409     C*****************************************************************
097600990409     C     RICDAT        BEGSR
097700990409     C* UTILIZZO IL CALENDARIO
097800990409     C                   MOVE      WDATEL        G08INV
097900990409     C                   MOVEL     '3'           G08ERR
098000990409     C                   CALL      'XSRDA8'
098100990409     C                   PARM                    WLBDA8
098200990409     C*
098300990409     C                   MOVEL     'S'           WFEST             1
098400990409     C                   CLEAR                   KTFP
098500990409     C* CERCO IL PRIMO GIORNO LAVORATIVO PRECEDENTE
098600990409     C     WFEST         DOWEQ     'S'
098700990409     C                   SUB       1             G08TGI
098800990409     C                   Z-ADD     G08TGI        GI8TGI
098900990409     C                   CALL      'XSRGI8'
099000990409     C                   PARM                    WGIDAT
099100990409     C                   Z-ADD     GI8INV        WDATAM
099200990409     C* IMPOSTO ANNO E MESE
099300990409     C                   Z-ADD     WAA           KANN
099400990409     C                   Z-ADD     WMM           KMES
099500990409     C*
099600990409     C                   CLEAR                   WFEST
099700990409     C     KCLN          CHAIN     AZCLN01L                           31
099800990409    1C     *IN31         IFEQ      *OFF
099900990511     C                   MOVEA     CLNPOM        POM
100000990511   1AC     POM(WGG)      IFNE      *BLANKS
100100990409     C                   MOVEL     '1Y'          KCOD
100200990409     C                   MOVEL(P)  POM(WGG)      KKEY
100300990409     C     KTAB          CHAIN     TABEL                              31
100400990409    2C     *IN31         IFEQ      *OFF
100500990409     C                   MOVEL     TBLUNI        DS1Y
100600990409    3C     §1YFEI        IFEQ      'S'
100700990409     C                   MOVEL     'S'           WFEST
100800990409    3C                   ENDIF
100900990409    2C                   ENDIF
101000990511   1AC                   ENDIF
101100990511    1C                   ENDIF
101200990409     C                   ENDDO
101300990409     C                   ENDSR
101400990409     C*****************************************************************
101500990409     C* DETERMINAZIONE SE ELABORATO IDD O MENO
101600990409     C*****************************************************************
101700990409     C     ELAIDD        BEGSR
101800990409     C                   SETOFF                                       18
101900990409     C* SE NON FA L'IDD LASCIO IN BIANCO
102000050831    1C     §OGNID        IFEQ      ' '
102100050831     c
102200050906     c* Escludo i p.o. esteri e la 101
102300050831    2c                   if        §ogntw<>'EEX' and §ogntw<>'EUP' and
102400050906     c                             §ogntw<>'DPD' and §ogntw<>'FED' and
102500050906     c                             orgfil<>101
102600050831     c
102700990409     C                   Z-ADD     95            KCAA
102800990409     C* PER I P.O. CHE FANNO L'IDD IL GIORNO DOPO
102900990409     C*  CERCO LA DATA PRECEDENTE
103000990409     C                   EXSR      RICDAT
103100990409     C*
103200990409     C                   MOVEL     ORGFIL        KLNA
103300990409     C* IMPOSTO LA DATA
103400990409     C     §OGEID        IFEQ      ' '
103500990409     C                   Z-ADD     WDATEL        KDAO
103600990409     C                   ELSE
103700990409     C                   Z-ADD     WDATAM        KDAO
103800990409     C                   ENDIF
103900050831     c
104000050831     c* Escludo i p.o. in £6
104100050831     C                   MOVEL     '6'           D55TPT
104200050831     C                   MOVEL     ORGFIL        D55LIN
104300050831     C                   Z-ADD     kdao          D55DRF
104400050831     C                   CALL      'FNLV55R'
104500050831     C                   PARM                    DSLV55
104600050831     c
104700050831     c* P.o. gestito--> accendo il 18 come p.o. che non fa mai l'idd
104800050831     c                   if        d55tfa<>orgfil
104900050831     C                   SETON                                        18
105000050831     c                   else
105100990409     C*
105200990520     C     KANM4         SETLL     FNANM04L                               31
105300990409     C* NON C'E' ANOMALIA
105400990409    6C     *IN31         IFEQ      *OFF
105500990409     C                   SETON                                        17
105600990409     C                   ELSE
105700990409     C                   SETOFF                                       17
105800990409     C                   ENDIF
105900050831     C                   ENDIF
106000050831     c
106100050831   x2C                   ELSE
106200050831     C                   SETON                                        18
106300050831    2C                   ENDIF
106400990409     C*
106500050831   x1C                   ELSE
106600990409     C                   SETON                                        18
106700050831    1C                   ENDIF
106800990409     C                   ENDSR
106900970225     C*****************************************************************
107000970225     C* STAMPA DATI DEL PUNTO OPERATIVO
107100970225     C*****************************************************************
107200970225     C     PRTLNP        BEGSR
107300970225      * ------------------ PRIMA RIGA, TOTALI DI FILIALE --------------
107400970225     C*
107500970225     C                   SETON                                        40
107600970527     C*
107700970527     C     *IN05         IFEQ      *ON
107800990517     C                   MOVE      WDATEL        PRTGG
107900990517     C                   MOVE      WDATEL        W0040             4 0
108000970527     C                   MOVEL     W0040         PRTMM
108100970527     C                   END
108200970226     C* VERIFICO SE DATI NON PERVENUTI
108300970226     C     WTSPA         COMP      *BLANKS                                11
108400970226     C     WTSAT         COMP      *BLANKS                                12
108500970226     C     WTSBO         COMP      *BLANKS                                13
108600970226     C     WTSCO         COMP      *BLANKS                                14
108700050831     C**   WT300         COMP      *BLANKS                                15
108800970606     C     WTSAR         COMP      *BLANKS                                16
108900050831     c                   setoff                                       15
109000970226     C*
109100990505     C* PULISCO I CAMPI DI STAMPA CHE POTREBBERO RIMANERE SPORCHI
109200990505     C                   MOVE      *ZEROS        PRT88B
109300990505     C                   MOVE      *ZEROS        PRT88P
109400990505     C                   MOVE      *ZEROS        PRT88A
109500990505     C                   MOVE      *ZEROS        PRT88C
109600990505     C                   MOVE      *ZEROS        PRTFAF
109700990505     C                   MOVE      *ZEROS        PRTFAP
109800990505     C                   MOVE      *ZEROS        PRTSSP
109900010123     C**                   MOVE *ZEROS    PRTNAR
110000010123     C**                   MOVE *ZEROS    PRTNRP
110100990505     C                   MOVE      *ZEROS        SDA123
110200990505     C*
110300060131     c* Stampo per terminal e p.o. esteri, se <> da '2'
110400060131    1C***  ORGFIL        IFEQ      PRTTFP
110500060131    1C**   *IN11         ANDEQ     *OFF
110600060131     c                   if        wtspa<>'2'
110700970226     C* SPUNTE PISTOLA 88
110800970226     C* Disk B
110900970226     C                   Z-ADD     TP88B         PRT88B
111000970226     C                   MOVE      *ZEROS        PRT88P
111100970226    2C     TOTPAR        IFGT      *ZEROS
111200970226     C     PRT88B        DIV(H)    TOTPAR        $XX               7 4
111300970226     C     $XX           MULT      100           PRT88P
111400970226    2C                   END
111500970226     C* Altri
111600970226     C                   Z-ADD     TP88A         PRT88A
111700970226     C                   MOVE      *ZEROS        PRT88C
111800970226    2C     TOTPAR        IFGT      *ZEROS
111900970226     C     PRT88A        DIV(H)    TOTPAR        $XX               7 4
112000970226     C     $XX           MULT      100           PRT88C
112100970226    2C                   END
112200970226    1C                   END
112300970225     C* BOLLETTATO IN SOSPENSIONE
112400970227    1C     *IN13         IFEQ      *OFF
112500970225     C                   XFOOT     BOL           TOTBOL
112600970225     C                   XFOOT     BSS           PRTBSS
112700970226     C                   MOVE      *ZEROS        PRTBSP
112800970227    2C     TOTBOL        IFGT      *ZEROS
112900970225     C     PRTBSS        DIV(H)    TOTBOL        $XX               7 4
113000970226     C     $XX           MULT      100           PRTBSP
113100970227    2C                   END
113200970227    1C                   END
113300970306     C* SPEDIZIONI A TERRA
113400060131     c* Stampo per terminal e p.o. esteri, se <> da '2'
113500060131    1C**   ORGFIL        IFEQ      PRTTFP
113600060131    1C**   *IN12         ANDEQ     *OFF
113700060131     c                   if        wtsat<>'2'
113800970306     C* ... FUORI AFFIDABILITA'
113900970306     C                   XFOOT     SDA           SDA123
114000970306     C                   XFOOT     FAF           PRTFAF
114100970306     C                   MOVE      *ZEROS        PRTFAP
114200970306    2C     SDA123        IFGT      *ZEROS
114300970306     C     PRTFAF        DIV(H)    SDA123        $XX               7 4
114400970306     C     $XX           MULT      100           PRTFAP
114500970306    2C                   END
114600970306     C* ... SPAREGGIATE
114700970306     C                   XFOOT     SPA           PRTSSP
114800970306    1C                   END
114900970606     C*
115000970606     C* COLLI PARTITI E NON SPUNTATI DAL TERMINAL DI ARRIVO
115100990504    1C     WTARR         IFEQ      '1'
115200990504    1C     *IN16         ANDEQ     *OFF
115300010123     C**                   Z-ADDTOTNAR    PRTNAR
115400010123     C**                   MOVE *ZEROS    PRTNRP
115500010123    2C**         TOTARR    IFGT *ZEROS
115600010123     C**         PRTNAR    DIV  TOTARR    $XX     74H
115700010123     C**         $XX       MULT 100       PRTNRP
115800010123    2C**                   END
115900970606    1C                   END
116000970606     C*
116100010124     C* SPEDIZIONI NON CONSEGNATE fuori affidabilita'
116200970227    1C     *IN14         IFEQ      *OFF
116300970226     C                   XFOOT     CSB           CSB123
116400970226     C                   XFOOT     NOC           PRTNCN
116500970226     C                   MOVE      *ZEROS        PRTNCP
116600970227    2C     CSB123        IFGT      *ZEROS
116700970226     C     PRTNCN        DIV(H)    CSB123        $XX               7 4
116800970226     C     $XX           MULT      100           PRTNCP
116900970227    2C                   END
117000010124     C                   XFOOT     DSC           PRTDSC
117100010124     C                   XFOOT     DSR           PRTDSR
117200970227    1C                   END
117300970226     C* RITIRI NON ESEGUITI
117400050831     C**N15              Z-ADD     TOTRIT        PRTRIT
117500970226     C* MANCA BAM
117600970226     C                   Z-ADD     TOTBAM        PRTBAM
117700970226     C* DISGUIDI
117800970226     C                   Z-ADD     TOTDIS        PRTDIS
117900040405     C                   Z-ADD     TOTDISP       PRTDISP
118000970226     C* DISGUIDI IN AREA
118100970226     C                   Z-ADD     TOTDAR        PRTDIA
118200990503     C*
118300990503     C* SE RICHIESTO SCARICO DATI SU FILE DI WORK
118400990503     C     V1CCRP        IFEQ      'S'
118500990503     C                   EXSR      RIERCQ
118600990503     C                   END
118700990503     C*
118800970225     C*
118900990623     C     WBRKPE        IFEQ      '1'
119000970528     C     *IN05         OREQ      *ON
119100970528     C     V1CTSP        ANDEQ     'N'
119200970227     C                   ADD       1             $RIGHE
119300990623     C                   ADD       1             $RIGHA
119400990623     C                   ADD       1             $RIGHD
119500970227     C                   ELSE
119600970227     C                   ADD       2             $RIGHE
119700990623     C                   ADD       2             $RIGHA
119800990623     C                   ADD       2             $RIGHD
119900970227     C                   END
120000970401     C*
120100970401     C*
120200990428     C*
120300990428     C     WBRKN         IFEQ      '1'
120400990428     C     WBRKN1        ANDEQ     '1'
120500990428     C     *IN05         OREQ      *ON
120600990428     C     V1CTSP        ANDEQ     'N'
120700990428     C     WBRKN1        ANDEQ     '1'
120800990428     C                   ADD       1             $RIGHN
120900990428     C                   ELSE
121000990507     C     WBRKN1        IFEQ      '1'
121100990428     C                   ADD       2             $RIGHN
121200990428     C                   END
121300990507     C                   END
121400970227     C*
121500990621     C     $RIGHE        IFGT      60
121600970602     C     *IN05         ANDEQ     *OFF
121700990621     C     $RIGHE        ORGT      60
121800970602     C     *IN05         ANDEQ     *ON
121900970602     C     V1CTSP        ANDEQ     'N'
122000970602     C     $RIGHE        ORGT      58
122100970602     C     *IN05         ANDEQ     *ON
122200970602     C     V1CTSP        ANDEQ     'S'
122300970227     C                   WRITE     LV66T1
122400970227     C                   WRITE     LV66T3
122500970227     C                   WRITE     LV66T4
122600970227     C                   WRITE     LV66T3
122700970227     C                   Z-ADD     11            $RIGHE
122800990623     C                   MOVE      '1'           WBRKPE
122900970227    1C                   END
123000970227     C* Stampo la riga vuota solo se non ho appena stampato la riga
123100970528     C* Distretto/area
123200970528     C* Se la stampa non è dettagliata per tipo servizio ed è stato
123300970528     C* richiesto un periodo dal/al non stampo MAI la riga vuota
123400990623     C     WBRKPE        IFEQ      *BLANKS
123500970528     C     *IN05         ANDEQ     *OFF
123600990623     C     WBRKPE        OREQ      *BLANKS
123700970528     C     *IN05         ANDEQ     *ON
123800970528     C     V1CTSP        ANDEQ     'S'
123900970227     C                   WRITE     LV66T2
124000970227     C                   END
124100970227     C*
124200970227     C                   WRITE     LV66D1
124300990429     c* memorizzo che ho stampato almeno una riga per l'area
124400990429     C                   MOVE      '1'           WPRT              1
124500970527     C*
124600970401     C* STAMPO ANCHE PER CAPO AREA E CAPO DISTRETTO SE RICHIESTO
124700990428    1C     V1CINA        IFEQ      'S'
124800970401     C* PER CAPO AREA
124900990621    2C     $RIGHA        IFGT      60
125000970602     C     *IN05         ANDEQ     *OFF
125100990621     C     $RIGHA        ORGT      60
125200970602     C     *IN05         ANDEQ     *ON
125300970602     C     V1CTSP        ANDEQ     'N'
125400970602     C     $RIGHA        ORGT      58
125500970602     C     *IN05         ANDEQ     *ON
125600970602     C     V1CTSP        ANDEQ     'S'
125700970401     C                   WRITE     LV66T1A
125800970401     C                   WRITE     LV66T3A
125900970401     C                   WRITE     LV66T4A
126000970401     C                   WRITE     LV66T3A
126100970401     C                   Z-ADD     11            $RIGHA
126200990623     C                   MOVE      '1'           WBRKPA
126300970527    2C                   END
126400970401     C* Stampo la riga vuota solo se non ho appena stampato la riga
126500970401     C* Distretto/area
126600970528     C* Se la stampa non è dettagliata per tipo servizio ed è stato
126700970528     C* richiesto un periodo dal/al non stampo MAI la riga vuota
126800990623    2C     WBRKPA        IFEQ      *BLANKS
126900970528     C     *IN05         ANDEQ     *OFF
127000990623     C     WBRKPA        OREQ      *BLANKS
127100970528     C     *IN05         ANDEQ     *ON
127200970528     C     V1CTSP        ANDEQ     'S'
127300970401     C                   WRITE     LV66T2A
127400970527    2C                   END
127500970401     C                   WRITE     LV66D1A
127600990428    1C                   END
127700970401     C* PER CAPO DISTRETTO
127800990428    1C     V1CIND        IFEQ      'S'
127900990621    2C     $RIGHD        IFGT      60
128000970602     C     *IN05         ANDEQ     *OFF
128100990621    2C     $RIGHD        ORGT      60
128200970602     C     *IN05         ANDEQ     *ON
128300970602     C     V1CTSP        ANDEQ     'N'
128400970602     C     $RIGHD        ORGT      58
128500970602     C     *IN05         ANDEQ     *ON
128600970602     C     V1CTSP        ANDEQ     'S'
128700970401     C                   WRITE     LV66T1D
128800970401     C                   WRITE     LV66T3D
128900970401     C                   WRITE     LV66T4D
129000970401     C                   WRITE     LV66T3D
129100970401     C                   Z-ADD     11            $RIGHD
129200990623     C                   MOVE      '1'           WBRKPD
129300970527    2C                   END
129400970401     C* Stampo la riga vuota solo se non ho appena stampato la riga
129500970401     C* Distretto/area
129600970528     C* Se la stampa non è dettagliata per tipo servizio ed è stato
129700970528     C* richiesto un periodo dal/al non stampo MAI la riga vuota
129800990623    2C     WBRKPD        IFEQ      *BLANKS
129900970528     C     *IN05         ANDEQ     *OFF
130000990623     C     WBRKPD        OREQ      *BLANKS
130100970528     C     *IN05         ANDEQ     *ON
130200970528     C     V1CTSP        ANDEQ     'S'
130300970401     C                   WRITE     LV66T2D
130400970527    2C                   END
130500970401     C                   WRITE     LV66D1D
130600970527    1C                   END
130700990428     C* STAMPO ANCHE PER NETWORK SE RICHIESTO
130800990428    1C     V1CINN        IFEQ      'S'
130900990428     C     WBRKN1        ANDEQ     '1'
131000990428     C*
131100990621    2C     $RIGHN        IFGT      60
131200990428     C     *IN05         ANDEQ     *OFF
131300990621     C     $RIGHN        ORGT      60
131400990428     C     *IN05         ANDEQ     *ON
131500990428     C     V1CTSP        ANDEQ     'N'
131600990428     C     $RIGHN        ORGT      58
131700990428     C     *IN05         ANDEQ     *ON
131800990428     C     V1CTSP        ANDEQ     'S'
131900990428     C                   WRITE     LV66T1N
132000990428     C                   WRITE     LV66T3N
132100990428     C                   WRITE     LV66T4N
132200990428     C                   WRITE     LV66T3N
132300990428     C                   Z-ADD     11            $RIGHN
132400990428     C                   MOVE      '1'           WBRKN
132500990428    2C                   END
132600990428     C* Stampo la riga vuota solo se non ho appena stampato la riga
132700990428     C* Distretto/area
132800990428     C* Se la stampa non è dettagliata per tipo servizio ed è stato
132900990428     C* richiesto un periodo dal/al non stampo MAI la riga vuota
133000990428    2C     WBRKN         IFEQ      *BLANKS
133100990428     C     *IN05         ANDEQ     *OFF
133200990428     C     WBRKN         OREQ      *BLANKS
133300990428     C     *IN05         ANDEQ     *ON
133400990428     C     V1CTSP        ANDEQ     'S'
133500990428     C                   WRITE     LV66T2N
133600990428    2C                   END
133700990428     C                   WRITE     LV66D1N
133800990428     C* memorizzo che ho stampato almeno una riga per il network
133900990428     C                   MOVE      '1'           WPRTN             1
134000990428    1C                   END
134100970527     C*
134200970527     C* MEMORIZZO I TOTALI STAMPATI IN CAMPI PER STAMPA TOTALI DI
134300970527     C* FILIALE
134400970527     C     *IN05         IFEQ      *ON
134500970527     C* ACCENDO *IN06 PER NON STAMPARE COD.+DECOD. DEL PUNTO OPERATIVO
134600970527     C* NELLE RIGHE SUCCESSIVE
134700970527     C                   SETON                                        06
134800050831     C**N15              ADD       TOTRIT        TOFRIT
134900970527     C  N13              ADD       PRTBSS        TOFBSS
135000970527     C  N13              ADD       TOTBOL        TOFBOL            8 0
135100970527     C  N11              ADD       TP88B         TOF88B
135200970527     C  N11              ADD       TOTPAR        TOFPAR            8 0
135300970527     C  N11              ADD       TP88A         TOF88A
135400970527     C  N12              ADD       PRTFAF        TOFFAF
135500970527     C  N12              ADD       SDA123        TOFSDA            8 0
135600970527     C  N12              ADD       PRTSSP        TOFSSP
135700970527     C                   ADD       TOTDIS        TOFDIS
135800040405     C                   ADD       TOTDISP       TOFDISP
135900970527     C                   ADD       TOTBAM        TOFBAM
136000970527     C  N14              ADD       PRTNCN        TOFNCN
136100970527     C  N14              ADD       CSB123        TOFCSB           10 0
136200010124     C  N14              ADD       PRTDSC        TOFDSC
136300010124     C  N14              ADD       PRTDSR        TOFDSR
136400970527     C                   ADD       TOTDAR        TOFDIA
136500010123     C**N16                ADD  TOTARR    TOFARR  80
136600010123     C**N16                ADD  TOTNAR    TOFNAR
136700970527     C                   END
136800990429     C* MEMORIZZO I TOTALI STAMPATI IN CAMPI PER STAMPA TOTALI DI
136900990505     C* AREA (SE NON E' STATA RICHIESTO UN P.O. SPECIFICO)
137000990505     C     *IN01         IFEQ      *OFF
137100990429     C                   SELECT
137200990429     C* NETWORK BARTOLINI
137300990429     C     COMDIT        WHENEQ    'B'
137400990430     C                   Z-ADD     2             NT
137500990430     C                   Z-ADD     1             NA                1 0
137600990430     C     COMDIT        WHENEQ    'S'
137700990430     C                   Z-ADD     3             NT
137800990430     C                   Z-ADD     1             NA
137900990430     C                   ENDSL
138000990430     C*
138100990505     C*
138200050831     C**N15              ADD       TOTRIT        RIT(NT)
138300990430     C  N13              ADD       PRTBSS        SOS(NT)
138400990505     C  N13              ADD       TOTBOL        TBL(NT)
138500990430     C  N11              ADD       TP88B         B88(NT)
138600990505     C  N11              ADD       TOTPAR        PAR(NT)
138700990430     C  N11              ADD       TP88A         A88(NT)
138800990430     C  N12              ADD       PRTFAF        SFA(NT)
138900990505     C  N12              ADD       SDA123        TDA(NT)
139000990430     C  N12              ADD       PRTSSP        SSP(NT)
139100990430     C                   ADD       TOTDIS        DIS(NT)
139200040405     C                   ADD       TOTDISP       DISP(NT)
139300990430     C                   ADD       TOTBAM        DDT(NT)
139400990430     C  N14              ADD       PRTNCN        NCN(NT)
139500990505     C  N14              ADD       CSB123        SCS(NT)
139600010124     C  N14              ADD       PRTDSC        DCN(NT)
139700010124     C  N14              ADD       PRTDSR        DRT(NT)
139800990430     C                   ADD       TOTDAR        DIA(NT)
139900010123     C**N16                ADD  TOTARR    ARR,NT
140000010123     C**N16                ADD  TOTNAR    NAR,NT
140100990429     C* MEMORIZZO I TOTALI PER LA STAMPA DEI TOTALI GENERALI
140200050831     C**N15              ADD       TOTRIT        RIT(NA)
140300990430     C  N13              ADD       PRTBSS        SOS(NA)
140400990505     C  N13              ADD       TOTBOL        TBL(NA)
140500990430     C  N11              ADD       TP88B         B88(NA)
140600990505     C  N11              ADD       TOTPAR        PAR(NA)
140700990430     C  N11              ADD       TP88A         A88(NA)
140800990430     C  N12              ADD       PRTFAF        SFA(NA)
140900990505     C  N12              ADD       SDA123        TDA(NA)
141000990430     C  N12              ADD       PRTSSP        SSP(NA)
141100990430     C                   ADD       TOTDIS        DIS(NA)
141200040405     C                   ADD       TOTDISP       DISP(NA)
141300990430     C                   ADD       TOTBAM        DDT(NA)
141400990430     C  N14              ADD       PRTNCN        NCN(NA)
141500990505     C  N14              ADD       CSB123        SCS(NA)
141600010124     C  N14              ADD       PRTDSC        DCN(NA)
141700010124     C  N14              ADD       PRTDSR        DRT(NA)
141800990430     C                   ADD       TOTDAR        DIA(NA)
141900010123     C**N16                ADD  TOTARR    ARR,NA
142000010123     C**N16                ADD  TOTNAR    NAR,NA
142100990505     C                   END
142200970527     C*
142300970225      *
142400090930      * ------------------ ESPR./DIST./CORR./H1030 --------------------
142500970225      *
142600970407     C* Il dettaglio per tipo servizio lo stampo solo se richiesto
142700970407   1AC     V1CTSP        IFEQ      'S'
142800970225     C                   SETOFF                                       40
142900091001     c  n10              z-add     2             $v
143000091001     c   10              z-add     1             $v
143100091001     c
143200091001    1C     $v            DOwle     4
143300970225     C                   MOVE      ENS($V)       TIPOP
143400970225     C* BOLLETTATO IN SOSPENSIONE
143500970227    2C     *IN13         IFEQ      *OFF
143600970225     C                   Z-ADD     BSS($V)       PRTBSS
143700970226     C                   MOVE      *ZEROS        PRTBSP
143800970227    3C     BOL($V)       IFGT      *ZEROS
143900970225     C     BSS($V)       DIV(H)    BOL($V)       $XX               7 4
144000970226     C     $XX           MULT      100           PRTBSP
144100970227    3C                   END
144200970227    2C                   END
144300970306     C* SPEDIZIONI A TERRA
144400970306     C* ... FUORI AFFIDABILITA'
144500970306    2C     *IN12         IFEQ      *OFF
144600970226     C                   Z-ADD     FAF($V)       PRTFAF
144700970226     C                   MOVE      *ZEROS        PRTFAP
144800970227    3C     SDA($V)       IFGT      *ZEROS
144900970226     C     FAF($V)       DIV(H)    SDA($V)       $XX               7 4
145000970226     C     $XX           MULT      100           PRTFAP
145100970227    3C                   END
145200970306     C* ... SPAREGGIATE
145300970306     C                   Z-ADD     SPA($V)       PRTSSP
145400970306    2C                   END
145500970226     C* SPEDIZIONI NON CONSEGNATE
145600970227    2C     *IN14         IFEQ      *OFF
145700970226     C                   Z-ADD     NOC($V)       PRTNCN
145800970226     C                   MOVE      *ZEROS        PRTNCP
145900970227    3C     CSB($V)       IFGT      *ZEROS
146000970226     C     PRTNCN        DIV(H)    CSB($V)       $XX               7 4
146100970226     C     $XX           MULT      100           PRTNCP
146200970227    3C                   END
146300010124     C                   Z-ADD     DSC($V)       PRTDSC
146400010124     C                   Z-ADD     DSR($V)       PRTDSR
146500970227    2C                   END
146600970225     C*
146700970227     C                   ADD       1             $RIGHE
146800970226     C                   WRITE     LV66D1
146900970401     C*
147000970401     C* STAMPO ANCHE PER CAPO DISTRETTO E CAPO AREA SE RICHIESTO
147100990428     C     V1CINA        IFEQ      'S'
147200970401     C                   ADD       1             $RIGHA
147300970401     C                   WRITE     LV66D1A
147400990428     C                   END
147500990428     C     V1CIND        IFEQ      'S'
147600970401     C                   ADD       1             $RIGHD
147700970401     C                   WRITE     LV66D1D
147800970401     C                   END
147900990428     C*
148000990428     C* STAMPO ANCHE PER NETWORK
148100990428     C     V1CINN        IFEQ      'S'
148200990428     C     WBRKN1        ANDEQ     '1'
148300990428     C                   ADD       1             $RIGHN
148400990428     C                   WRITE     LV66D1N
148500990428     C                   END
148600970401     C*
148700091001     c                   eval      $v=$v+1
148800970227    1C                   ENDDO
148900970407   1AC                   END
149000970226     C*
149100970226     C                   ENDSR
149200990503     C*****************************************************************
149300990503     C* CREAZIONE DATI REPORT SU FILE DI DATA BASE
149400990503     C*****************************************************************
149500990503     C     RIERCQ        BEGSR
149600990504     C                   CLEAR                   WFRCQ000
149700990504     C                   MOVE      PRTFL3        RCQDIV
149800990503     C                   Z-ADD     PRTCAR        RCQARE
149900990503     C                   Z-ADD     ORGFIL        RCQFIL
150000990503     C                   MOVEL     ORGDIT        RCQNTW
150100990503     C                   Z-ADD     WDATEL        RCQDAT
150200990504     C  N17
150300990504     CANN18              Z-ADD     1             RCQIDD
150400990504     C   17
150500990504     CANN18              Z-ADD     0             RCQIDD
150600010124     C   17
150700010124     CANN18              MOVEL     'N'           RCQIDN
150800990504     C   18              Z-ADD     9             RCQIDD
150900050831     C**N15              Z-ADD     PRTRIT        RCQRIT
151000050831     C** 15              MOVEL     'N'           RCQRIN
151100050831     C                   MOVEL     'N'           RCQRIN
151200990504     C  N13              Z-ADD     PRTBSS        RCQBSS
151300990504     C  N13              Z-ADD     TOTBOL        RCQBOL
151400010124     C   13              MOVEL     'N'           RCQBON
151500990504     C  N11              Z-ADD     PRT88B        RCQ88B
151600990504     C  N11              Z-ADD     PRT88A        RCQ88A
151700990504     C  N11              Z-ADD     TOTPAR        RCQPAR
151800010124     C   11              MOVEL     'N'           RCQPAN
151900990504     C  N12              Z-ADD     PRTFAF        RCQFAF
152000990504     C  N12              Z-ADD     SDA123        RCQSDA
152100990504     C  N12              Z-ADD     PRTSSP        RCQSSP
152200010124     C   12              MOVEL     'N'           RCQSTN
152300990503     C                   Z-ADD     PRTDIS        RCQDIS
152400040405     C                   Z-ADD     PRTDISP       RCQDISP
152500990503     C                   Z-ADD     PRTBAM        RCQDDT
152600010123     C  N16              Z-ADD     TOTNAR        RCQNAR
152700990504     C  N16              Z-ADD     TOTARR        RCQARR
152800010124     C   16              MOVEL     'N'           RCQARN
152900990504     C  N14              Z-ADD     PRTNCN        RCQNCN
153000990504     C  N14              Z-ADD     CSB123        RCQCSB
153100010124     C   14              MOVEL     'N'           RCQCSN
153200990503     C                   Z-ADD     PRTDIA        RCQDIA
153300010124     C* DEVO LEGGERE IL FILE DELLE STATISTICHE CONSEGNE PER VALORIZZARE
153400010124     C* I CAMPI RESTANTI
153500010124     C                   EXSR      RIESCO
153600010124     C*
153700990503     C                   WRITE     WFRCQ000
153800990503     C                   ENDSR
153900010124     C*****************************************************************
154000010124     C* RIEMPIMENTO CAMPI DEL WORK FILE RELATIVI ALLA STAT.CONSEGNE
154100010124     C*****************************************************************
154200010124     C     RIESCO        BEGSR
154300010124     C     KSCO4         SETLL     FNSCO04L
154400010124     C     KSCO4         READE     FNSCO04L                               31
154500010124    1C     *IN31         DOWEQ     *OFF
154600010124     C* ESCLUDO TOTALI E FESTIVI
154700010124    2C     SCOOLT        IFNE      'T'
154800010124     C     SCOOLT        ANDNE     'F'
154900010124     C* VALORE ASSOLUTO DELLE ORE DI RITARDO
155000010124    3C     SCOORE        IFLT      *ZEROS
155100010124     C                   Z-SUB     SCOORE        WORRIT
155200010124    3C                   ENDIF
155300010124     C* GLI SCAGLIONI DI RITARDO/ANTICIPO DEL REPORT CONTROLLO QUALITÀ
155400010124     C* PER IL MOMENTO SONO GLI STESSI DELLA STATISTICA CONSEGNE MA IN
155500010124     C* FUTURO POTREBBERO CAMBIARE DAL MOMENTO CHE QUELLI DELLA STATIS-
155600010124     C* TICA CONSEGNE SONO TABELLATI. DEVO QUINDI DETERMINARE LO
155700010124     C* SCAGLIONE DI RITARDO/ANTICIPO
155800010124     C                   Z-ADD     1             YY                1 0
155900010124    3C     SCOORE        IFLT      *ZEROS
156000010124     C     WORRIT        LOOKUP    SCA(YY)                                32
156100010124    4C     *IN32         IFEQ      '0'
156200010124     C                   Z-ADD     1             YY
156300010124    4C                   ENDIF
156400010124    4C     SCOOLT        IFEQ      'A'
156500010124     C     YY            ANDGT     1
156600010124     C                   SUB       1             YY
156700010124    4C                   ENDIF
156800010124   X3C                   ELSE
156900010124    4C     SCOORE        IFGT      *ZEROS
157000010124     C     SCOORE        LOOKUP    SCR(YY)                                32
157100010124    5C     *IN32         IFEQ      '0'
157200010124     C                   Z-ADD     5             YY
157300010124    5C                   ENDIF
157400010124    5C     SCOOLT        IFEQ      'O'
157500010124     C     YY            ANDLT     5
157600010124     C                   ADD       1             YY
157700010124    5C                   ENDIF
157800010124    4C                   ENDIF
157900010124    3C                   ENDIF
158000010124     C*
158100010124    3C                   SELECT
158200010124     C***********************
158300010124     C* SPEDIZIONI CONSEGNATE
158400010124     C***********************
158500010124     C     SCOSCO        WHENEQ    'C'
158600010124    4C     SCOGBO        IFEQ      *BLANKS
158700010124     C* Anticipo
158800010124    5C     SCOORE        IFLT      *ZEROS
158900010124    6C                   SELECT
159000010124     C* Oltre 24 ore
159100010124     C     YY            WHENEQ    1
159200091002     C                   Z-ADD     SCOSS4        RCQCOh
159300091002     C                   Z-ADD     SCOSS1        RCQCOE
159400010124     C                   Z-ADD     SCOSS2        RCQCOC
159500010124     C                   ADD       SCOSS3        RCQCOC
159600010124     c* 24 ore
159700010124     C     YY            WHENEQ    2
159800091002     C                   Z-ADD     SCOSS4        RCQCAh
159900091002     C                   Z-ADD     SCOSS1        RCQCAE
160000010124     C                   Z-ADD     SCOSS2        RCQCAC
160100010124     C                   ADD       SCOSS3        RCQCAC
160200010124     C                   OTHER
160300010124     c* 12 ore
160400091002     C                   Z-ADD     SCOSS4        RCQCBh
160500091002     C                   Z-ADD     SCOSS1        RCQCBE
160600010124     C                   Z-ADD     SCOSS2        RCQCBC
160700010124     C                   ADD       SCOSS3        RCQCBC
160800010124    6C                   ENDSL
160900010124   X5C                   ELSE
161000010124     c* Conforme
161100010124    6C     SCOORE        IFEQ      *ZEROS
161200091002     C                   Z-ADD     SCOSS4        RCQC0h
161300091002     C                   Z-ADD     SCOSS1        RCQC0E
161400010124     C                   Z-ADD     SCOSS2        RCQC0C
161500010124     C                   ADD       SCOSS3        RCQC0C
161600010124   X6C                   ELSE
161700010124     c* Ritardo
161800010124    7C                   SELECT
161900010124     c* 12 ore
162000010124     C     YY            WHENEQ    1
162100091002     C                   Z-ADD     SCOSS4        RCQC1h
162200091002     C                   Z-ADD     SCOSS1        RCQC1E
162300010124     C                   Z-ADD     SCOSS2        RCQC1C
162400010124     C                   ADD       SCOSS3        RCQC1C
162500010124     c* 24 ore
162600010124     C     YY            WHENEQ    2
162700091002     C                   Z-ADD     SCOSS4        RCQC2h
162800091002     C                   Z-ADD     SCOSS1        RCQC2E
162900010124     C                   Z-ADD     SCOSS2        RCQC2C
163000010124     C                   ADD       SCOSS3        RCQC2C
163100010124     c* 36 ore
163200010124     C     YY            WHENEQ    3
163300091002     C                   Z-ADD     SCOSS4        RCQC3h
163400091002     C                   Z-ADD     SCOSS1        RCQC3E
163500010124     C                   Z-ADD     SCOSS2        RCQC3C
163600010124     C                   ADD       SCOSS3        RCQC3C
163700010124     c* 48 ore
163800010124     C     YY            WHENEQ    4
163900091002     C                   Z-ADD     SCOSS4        RCQC4h
164000091002     C                   Z-ADD     SCOSS1        RCQC4E
164100010124     C                   Z-ADD     SCOSS2        RCQC4C
164200010124     C                   ADD       SCOSS3        RCQC4C
164300010124     C                   OTHER
164400010124     c* oltre 48 ore
164500091002     C                   Z-ADD     SCOSS4        RCQCTh
164600091002     C                   Z-ADD     SCOSS1        RCQCTE
164700010124     C                   Z-ADD     SCOSS2        RCQCTC
164800010124     C                   ADD       SCOSS3        RCQCTC
164900010124    7C                   ENDSL
165000010124    6C                   ENDIF
165100010124    5C                   ENDIF
165200010124   X4C                   ELSE
165300010124     C* ...........DI CUI CONSEGNE PARZIALI
165400010124    5C     SCOGBO        IFEQ      'P'
165500091002     C                   Z-ADD     SCOSS4        RCQCPh
165600091002     C                   Z-ADD     SCOSS1        RCQCPE
165700010124     C                   Z-ADD     SCOSS2        RCQCPC
165800010124     C                   ADD       SCOSS3        RCQCPC
165900010124   X5C                   ELSE
166000010124     C* ...........DI CUI RISERVA DI CONSEGNA
166100010124    6C     SCOGBO        IFEQ      'R'
166200091002     C                   Z-ADD     SCOSS4        RCQCRh
166300091002     C                   Z-ADD     SCOSS1        RCQCRE
166400010124     C                   Z-ADD     SCOSS2        RCQCRC
166500010124     C                   ADD       SCOSS3        RCQCRC
166600010124    6C                   ENDIF
166700010124    5C                   ENDIF
166800010124    4C                   ENDIF
166900010124     C***************************
167000010124     C* SPEDIZIONI NON CONSEGNATE
167100010124     C***************************
167200010124     C     SCOSCO        WHENEQ    'N'
167300010124    4C                   SELECT
167400010124     C*............DI CUI GIACENZE APERTE
167500010124     C     SCOGBO        WHENEQ    'G'
167600010124     C* Anticipo
167700010124    5C     SCOORE        IFLT      *ZEROS
167800010124    6C                   SELECT
167900010124     C* Oltre 24 ore
168000010124     C     YY            WHENEQ    1
168100091002     C                   Z-ADD     SCOSS4        RCQGOh
168200091002     C                   Z-ADD     SCOSS1        RCQGOE
168300010124     C                   Z-ADD     SCOSS2        RCQGOC
168400010124     C                   ADD       SCOSS3        RCQGOC
168500010124     c* 24 ore
168600010124     C     YY            WHENEQ    2
168700091002     C                   Z-ADD     SCOSS4        RCQGAh
168800091002     C                   Z-ADD     SCOSS1        RCQGAE
168900010124     C                   Z-ADD     SCOSS2        RCQGAC
169000010124     C                   ADD       SCOSS3        RCQGAC
169100010124     C                   OTHER
169200010124     c* 12 ore
169300091002     C                   Z-ADD     SCOSS4        RCQGBh
169400091002     C                   Z-ADD     SCOSS1        RCQGBE
169500010124     C                   Z-ADD     SCOSS2        RCQGBC
169600010124     C                   ADD       SCOSS3        RCQGBC
169700010124    6C                   ENDSL
169800010124   X5C                   ELSE
169900010124     c* Conforme
170000010124    6C     SCOORE        IFEQ      *ZEROS
170100091002     C                   Z-ADD     SCOSS4        RCQG0h
170200091002     C                   Z-ADD     SCOSS1        RCQG0E
170300010124     C                   Z-ADD     SCOSS2        RCQG0C
170400010124     C                   ADD       SCOSS3        RCQG0C
170500010124   X6C                   ELSE
170600010124     c* Ritardo
170700010124    7C                   SELECT
170800010124     c* 12 ore
170900010124     C     YY            WHENEQ    1
171000091002     C                   Z-ADD     SCOSS4        RCQG1h
171100091002     C                   Z-ADD     SCOSS1        RCQG1E
171200010124     C                   Z-ADD     SCOSS2        RCQG1C
171300010124     C                   ADD       SCOSS3        RCQG1C
171400010124     c* 24 ore
171500010124     C     YY            WHENEQ    2
171600091002     C                   Z-ADD     SCOSS4        RCQG2h
171700091002     C                   Z-ADD     SCOSS1        RCQG2E
171800010124     C                   Z-ADD     SCOSS2        RCQG2C
171900010124     C                   ADD       SCOSS3        RCQG2C
172000010124     c* 36 ore
172100010124     C     YY            WHENEQ    3
172200091002     C                   Z-ADD     SCOSS4        RCQG3h
172300091002     C                   Z-ADD     SCOSS1        RCQG3E
172400010124     C                   Z-ADD     SCOSS2        RCQG3C
172500010124     C                   ADD       SCOSS3        RCQG3C
172600010124     c* 48 ore
172700010124     C     YY            WHENEQ    4
172800091002     C                   Z-ADD     SCOSS4        RCQG4h
172900091002     C                   Z-ADD     SCOSS1        RCQG4E
173000010124     C                   Z-ADD     SCOSS2        RCQG4C
173100010124     C                   ADD       SCOSS3        RCQG4C
173200010124     C                   OTHER
173300010124     c* oltre 48 ore
173400091002     C                   Z-ADD     SCOSS4        RCQGTh
173500091002     C                   Z-ADD     SCOSS1        RCQGTE
173600010124     C                   Z-ADD     SCOSS2        RCQGTC
173700010124     C                   ADD       SCOSS3        RCQGTC
173800010124    7C                   ENDSL
173900010124    6C                   ENDIF
174000010124    5C                   ENDIF
174100010124     C*............DI CUI LASCIATI AVVISO
174200010124     C     SCOGBO        WHENEQ    'L'
174300010124     C* Anticipo
174400010124    5C     SCOORE        IFLT      *ZEROS
174500010124    6C                   SELECT
174600010124     C* Oltre 24 ore
174700010124     C     YY            WHENEQ    1
174800091002     C                   Z-ADD     SCOSS4        RCQLOh
174900091002     C                   Z-ADD     SCOSS1        RCQLOE
175000010124     C                   Z-ADD     SCOSS2        RCQLOC
175100010124     C                   ADD       SCOSS3        RCQLOC
175200010124     c* 24 ore
175300010124     C     YY            WHENEQ    2
175400091002     C                   Z-ADD     SCOSS4        RCQLAh
175500091002     C                   Z-ADD     SCOSS1        RCQLAE
175600010124     C                   Z-ADD     SCOSS2        RCQLAC
175700010124     C                   ADD       SCOSS3        RCQLAC
175800010124     C                   OTHER
175900010124     c* 12 ore
176000091002     C                   Z-ADD     SCOSS4        RCQLBh
176100091002     C                   Z-ADD     SCOSS1        RCQLBE
176200010124     C                   Z-ADD     SCOSS2        RCQLBC
176300010124     C                   ADD       SCOSS3        RCQLBC
176400010124    6C                   ENDSL
176500010124   X5C                   ELSE
176600010124     c* Conforme
176700010124    6C     SCOORE        IFEQ      *ZEROS
176800091002     C                   Z-ADD     SCOSS4        RCQL0h
176900091002     C                   Z-ADD     SCOSS1        RCQL0E
177000010124     C                   Z-ADD     SCOSS2        RCQL0C
177100010124     C                   ADD       SCOSS3        RCQL0C
177200010124   X6C                   ELSE
177300010124     c* Ritardo
177400010124    7C                   SELECT
177500010124     c* 12 ore
177600010124     C     YY            WHENEQ    1
177700091002     C                   Z-ADD     SCOSS4        RCQL1h
177800091002     C                   Z-ADD     SCOSS1        RCQL1E
177900010124     C                   Z-ADD     SCOSS2        RCQL1C
178000010124     C                   ADD       SCOSS3        RCQL1C
178100010124     c* 24 ore
178200010124     C     YY            WHENEQ    2
178300091002     C                   Z-ADD     SCOSS4        RCQL2h
178400091002     C                   Z-ADD     SCOSS1        RCQL2E
178500010124     C                   Z-ADD     SCOSS2        RCQL2C
178600010124     C                   ADD       SCOSS3        RCQL2C
178700010124     c* 36 ore
178800010124     C     YY            WHENEQ    3
178900091002     C                   Z-ADD     SCOSS4        RCQL3h
179000091002     C                   Z-ADD     SCOSS1        RCQL3E
179100010124     C                   Z-ADD     SCOSS2        RCQL3C
179200010124     C                   ADD       SCOSS3        RCQL3C
179300010124     c* 48 ore
179400010124     C     YY            WHENEQ    4
179500091002     C                   Z-ADD     SCOSS4        RCQL4h
179600091002     C                   Z-ADD     SCOSS1        RCQL4E
179700010124     C                   Z-ADD     SCOSS2        RCQL4C
179800010124     C                   ADD       SCOSS3        RCQL4C
179900010124     C                   OTHER
180000010124     c* oltre 48 ore
180100091002     C                   Z-ADD     SCOSS4        RCQLTh
180200091002     C                   Z-ADD     SCOSS1        RCQLTE
180300010124     C                   Z-ADD     SCOSS2        RCQLTC
180400010124     C                   ADD       SCOSS3        RCQLTC
180500010124    7C                   ENDSL
180600010124    6C                   ENDIF
180700010124    5C                   ENDIF
180800010124     C*............DI CUI CHIUSO PER TURNO
180900010124     C     SCOGBO        WHENEQ    'C'
181000010124     C* Anticipo
181100010124    5C     SCOORE        IFLT      *ZEROS
181200010124    6C                   SELECT
181300010124     C* Oltre 24 ore
181400010124     C     YY            WHENEQ    1
181500091002     C                   Z-ADD     SCOSS4        RCQHOh
181600091002     C                   Z-ADD     SCOSS1        RCQHOE
181700010124     C                   Z-ADD     SCOSS2        RCQHOC
181800010124     C                   ADD       SCOSS3        RCQHOC
181900010124     c* 24 ore
182000010124     C     YY            WHENEQ    2
182100091002     C                   Z-ADD     SCOSS4        RCQHAh
182200091002     C                   Z-ADD     SCOSS1        RCQHAE
182300010124     C                   Z-ADD     SCOSS2        RCQHAC
182400010124     C                   ADD       SCOSS3        RCQHAC
182500010124     C                   OTHER
182600010124     c* 12 ore
182700091002     C                   Z-ADD     SCOSS4        RCQHBh
182800091002     C                   Z-ADD     SCOSS1        RCQHBE
182900010124     C                   Z-ADD     SCOSS2        RCQHBC
183000010124     C                   ADD       SCOSS3        RCQHBC
183100010124    6C                   ENDSL
183200010124   X5C                   ELSE
183300010124     c* Conforme
183400010124    6C     SCOORE        IFEQ      *ZEROS
183500091002     C                   Z-ADD     SCOSS4        RCQH0h
183600091002     C                   Z-ADD     SCOSS1        RCQH0E
183700010124     C                   Z-ADD     SCOSS2        RCQH0C
183800010124     C                   ADD       SCOSS3        RCQH0C
183900010124   X6C                   ELSE
184000010124     c* Ritardo
184100010124    7C                   SELECT
184200010124     c* 12 ore
184300010124     C     YY            WHENEQ    1
184400091002     C                   Z-ADD     SCOSS4        RCQH1h
184500091002     C                   Z-ADD     SCOSS1        RCQH1E
184600010124     C                   Z-ADD     SCOSS2        RCQH1C
184700010124     C                   ADD       SCOSS3        RCQH1C
184800010124     c* 24 ore
184900010124     C     YY            WHENEQ    2
185000091002     C                   Z-ADD     SCOSS4        RCQH2h
185100091002     C                   Z-ADD     SCOSS1        RCQH2E
185200010124     C                   Z-ADD     SCOSS2        RCQH2C
185300010124     C                   ADD       SCOSS3        RCQH2C
185400010124     c* 36 ore
185500010124     C     YY            WHENEQ    3
185600091002     C                   Z-ADD     SCOSS4        RCQH3h
185700091002     C                   Z-ADD     SCOSS1        RCQH3E
185800010124     C                   Z-ADD     SCOSS2        RCQH3C
185900010124     C                   ADD       SCOSS3        RCQH3C
186000010124     c* 48 ore
186100010124     C     YY            WHENEQ    4
186200091002     C                   Z-ADD     SCOSS4        RCQH4h
186300091002     C                   Z-ADD     SCOSS1        RCQH4E
186400010124     C                   Z-ADD     SCOSS2        RCQH4C
186500010124     C                   ADD       SCOSS3        RCQH4C
186600010124     C                   OTHER
186700010124     c* oltre 48 ore
186800091002     C                   Z-ADD     SCOSS4        RCQHth
186900091002     C                   Z-ADD     SCOSS1        RCQHTE
187000010124     C                   Z-ADD     SCOSS2        RCQHTC
187100010124     C                   ADD       SCOSS3        RCQHTC
187200010124    7C                   ENDSL
187300010124    6C                   ENDIF
187400010124    5C                   ENDIF
187500010124     C*............DI CUI NON FATTE
187600010124     C     SCOGBO        WHENEQ    'N'
187700010124     C* Anticipo
187800010124    5C     SCOORE        IFLT      *ZEROS
187900010124    6C                   SELECT
188000010124     C* Oltre 24 ore
188100010124     C     YY            WHENEQ    1
188200091002     C                   Z-ADD     SCOSS4        RCQFOh
188300091002     C                   Z-ADD     SCOSS1        RCQFOE
188400010124     C                   Z-ADD     SCOSS2        RCQFOC
188500010124     C                   ADD       SCOSS3        RCQFOC
188600010124     c* 24 ore
188700010124     C     YY            WHENEQ    2
188800091002     C                   Z-ADD     SCOSS4        RCQFAh
188900091002     C                   Z-ADD     SCOSS1        RCQFAE
189000010124     C                   Z-ADD     SCOSS2        RCQFAC
189100010124     C                   ADD       SCOSS3        RCQFAC
189200010124     C                   OTHER
189300010124     c* 12 ore
189400091002     C                   Z-ADD     SCOSS4        RCQFBh
189500091002     C                   Z-ADD     SCOSS1        RCQFBE
189600010124     C                   Z-ADD     SCOSS2        RCQFBC
189700010124     C                   ADD       SCOSS3        RCQFBC
189800010124    6C                   ENDSL
189900010124   X5C                   ELSE
190000010124     c* Conforme
190100010124    6C     SCOORE        IFEQ      *ZEROS
190200091002     C                   Z-ADD     SCOSS4        RCQF0h
190300091002     C                   Z-ADD     SCOSS1        RCQF0E
190400010124     C                   Z-ADD     SCOSS2        RCQF0C
190500010124     C                   ADD       SCOSS3        RCQF0C
190600010124   X6C                   ELSE
190700010124     c* Ritardo
190800010124    7C                   SELECT
190900010124     c* 12 ore
191000010124     C     YY            WHENEQ    1
191100091002     C                   Z-ADD     SCOSS4        RCQF1h
191200091002     C                   Z-ADD     SCOSS1        RCQF1E
191300010124     C                   Z-ADD     SCOSS2        RCQF1C
191400010124     C                   ADD       SCOSS3        RCQF1C
191500010124     c* 24 ore
191600010124     C     YY            WHENEQ    2
191700091002     C                   Z-ADD     SCOSS4        RCQF2h
191800091002     C                   Z-ADD     SCOSS1        RCQF2E
191900010124     C                   Z-ADD     SCOSS2        RCQF2C
192000010124     C                   ADD       SCOSS3        RCQF2C
192100010124     c* 36 ore
192200010124     C     YY            WHENEQ    3
192300091002     C                   Z-ADD     SCOSS4        RCQF3h
192400091002     C                   Z-ADD     SCOSS1        RCQF3E
192500010124     C                   Z-ADD     SCOSS2        RCQF3C
192600010124     C                   ADD       SCOSS3        RCQF3C
192700010124     c* 48 ore
192800010124     C     YY            WHENEQ    4
192900091002     C                   Z-ADD     SCOSS4        RCQF4h
193000091002     C                   Z-ADD     SCOSS1        RCQF4E
193100010124     C                   Z-ADD     SCOSS2        RCQF4C
193200010124     C                   ADD       SCOSS3        RCQF4C
193300010124     C                   OTHER
193400010124     c* oltre 48 ore
193500091002     C                   Z-ADD     SCOSS4        RCQFTh
193600091002     C                   Z-ADD     SCOSS1        RCQFTE
193700010124     C                   Z-ADD     SCOSS2        RCQFTC
193800010124     C                   ADD       SCOSS3        RCQFTC
193900010124    7C                   ENDSL
194000010124    6C                   ENDIF
194100010124    5C                   ENDIF
194200010124    4C                   ENDSL
194300010124     C*******************************
194400010124     C* SPEDIZIONI ANCORA IN DISTINTA
194500010124     C*******************************
194600010124     C     SCOSCO        WHENEQ    'D'
194700010124     C* Anticipo
194800010124    5C     SCOORE        IFLT      *ZEROS
194900010124    6C                   SELECT
195000010124     C* Oltre 24 ore
195100010124     C     YY            WHENEQ    1
195200091002     C                   Z-ADD     SCOSS4        RCQDOh
195300091002     C                   Z-ADD     SCOSS1        RCQDOE
195400010124     C                   Z-ADD     SCOSS2        RCQDOC
195500010124     C                   ADD       SCOSS3        RCQDOC
195600010124     c* 24 ore
195700010124     C     YY            WHENEQ    2
195800091002     C                   Z-ADD     SCOSS4        RCQDAh
195900091002     C                   Z-ADD     SCOSS1        RCQDAE
196000010124     C                   Z-ADD     SCOSS2        RCQDAC
196100010124     C                   ADD       SCOSS3        RCQDAC
196200010124     C                   OTHER
196300010124     c* 12 ore
196400091002     C                   Z-ADD     SCOSS4        RCQDBh
196500091002     C                   Z-ADD     SCOSS1        RCQDBE
196600010124     C                   Z-ADD     SCOSS2        RCQDBC
196700010124     C                   ADD       SCOSS3        RCQDBC
196800010124    6C                   ENDSL
196900010124   X5C                   ELSE
197000010124     c* Conforme
197100010124    6C     SCOORE        IFEQ      *ZEROS
197200091002     C                   Z-ADD     SCOSS4        RCQD0h
197300091002     C                   Z-ADD     SCOSS1        RCQD0E
197400010124     C                   Z-ADD     SCOSS2        RCQD0C
197500010124     C                   ADD       SCOSS3        RCQD0C
197600010124   X6C                   ELSE
197700010124     c* Ritardo
197800010124    7C                   SELECT
197900010124     c* 12 ore
198000010124     C     YY            WHENEQ    1
198100091002     C                   Z-ADD     SCOSS4        RCQD1h
198200091002     C                   Z-ADD     SCOSS1        RCQD1E
198300010124     C                   Z-ADD     SCOSS2        RCQD1C
198400010124     C                   ADD       SCOSS3        RCQD1C
198500010124     c* 24 ore
198600010124     C     YY            WHENEQ    2
198700091002     C                   Z-ADD     SCOSS4        RCQD2h
198800091002     C                   Z-ADD     SCOSS1        RCQD2E
198900010124     C                   Z-ADD     SCOSS2        RCQD2C
199000010124     C                   ADD       SCOSS3        RCQD2C
199100010124     c* 36 ore
199200010124     C     YY            WHENEQ    3
199300091002     C                   Z-ADD     SCOSS4        RCQD3h
199400091002     C                   Z-ADD     SCOSS1        RCQD3E
199500010124     C                   Z-ADD     SCOSS2        RCQD3C
199600010124     C                   ADD       SCOSS3        RCQD3C
199700010124     c* 48 ore
199800010124     C     YY            WHENEQ    4
199900091002     C                   Z-ADD     SCOSS4        RCQD4h
200000091002     C                   Z-ADD     SCOSS1        RCQD4E
200100010124     C                   Z-ADD     SCOSS2        RCQD4C
200200010124     C                   ADD       SCOSS3        RCQD4C
200300010124     C                   OTHER
200400010124     c* oltre 48 ore
200500091002     C                   Z-ADD     SCOSS4        RCQDTh
200600091002     C                   Z-ADD     SCOSS1        RCQDTE
200700010124     C                   Z-ADD     SCOSS2        RCQDTC
200800010124     C                   ADD       SCOSS3        RCQDTC
200900010124    7C                   ENDSL
201000010124    6C                   ENDIF
201100010124    5C                   ENDIF
201200010124     C***************************
201300010124     C* SPEDIZIONI IN CONSEGNA
201400010124     C***************************
201500010124     C     SCOSCO        WHENEQ    'I'
201600010124     C* Anticipo
201700010124    5C     SCOORE        IFLT      *ZEROS
201800010124    6C                   SELECT
201900010124     C* Oltre 24 ore
202000010124     C     YY            WHENEQ    1
202100091002     C                   Z-ADD     SCOSS4        RCQIOh
202200091002     C                   Z-ADD     SCOSS1        RCQIOE
202300010124     C                   Z-ADD     SCOSS2        RCQIOC
202400010124     C                   ADD       SCOSS3        RCQIOC
202500010124     c* 24 ore
202600010124     C     YY            WHENEQ    2
202700091002     C                   Z-ADD     SCOSS4        RCQIAh
202800091002     C                   Z-ADD     SCOSS1        RCQIAE
202900010124     C                   Z-ADD     SCOSS2        RCQIAC
203000010124     C                   ADD       SCOSS3        RCQIAC
203100010124     C                   OTHER
203200010124     c* 12 ore
203300091002     C                   Z-ADD     SCOSS4        RCQIBh
203400091002     C                   Z-ADD     SCOSS1        RCQIBE
203500010124     C                   Z-ADD     SCOSS2        RCQIBC
203600010124     C                   ADD       SCOSS3        RCQIBC
203700010124    6C                   ENDSL
203800010124   X5C                   ELSE
203900010124     c* Conforme
204000010124    6C     SCOORE        IFEQ      *ZEROS
204100091002     C                   Z-ADD     SCOSS4        RCQI0h
204200091002     C                   Z-ADD     SCOSS1        RCQI0E
204300010124     C                   Z-ADD     SCOSS2        RCQI0C
204400010124     C                   ADD       SCOSS3        RCQI0C
204500010124   X6C                   ELSE
204600010124     c* Ritardo
204700010124    7C                   SELECT
204800010124     c* 12 ore
204900010124     C     YY            WHENEQ    1
205000091002     C                   Z-ADD     SCOSS4        RCQI1h
205100091002     C                   Z-ADD     SCOSS1        RCQI1E
205200010124     C                   Z-ADD     SCOSS2        RCQI1C
205300010124     C                   ADD       SCOSS3        RCQI1C
205400010124     c* 24 ore
205500010124     C     YY            WHENEQ    2
205600091002     C                   Z-ADD     SCOSS4        RCQI2h
205700091002     C                   Z-ADD     SCOSS1        RCQI2E
205800010124     C                   Z-ADD     SCOSS2        RCQI2C
205900010124     C                   ADD       SCOSS3        RCQI2C
206000010124     c* 36 ore
206100010124     C     YY            WHENEQ    3
206200091002     C                   Z-ADD     SCOSS4        RCQI3h
206300091002     C                   Z-ADD     SCOSS1        RCQI3E
206400010124     C                   Z-ADD     SCOSS2        RCQI3C
206500010124     C                   ADD       SCOSS3        RCQI3C
206600010124     c* 48 ore
206700010124     C     YY            WHENEQ    4
206800091002     C                   Z-ADD     SCOSS4        RCQI4h
206900091002     C                   Z-ADD     SCOSS1        RCQI4E
207000010124     C                   Z-ADD     SCOSS2        RCQI4C
207100010124     C                   ADD       SCOSS3        RCQI4C
207200010124     C                   OTHER
207300010124     c* oltre 48 ore
207400091002     C                   Z-ADD     SCOSS4        RCQITh
207500091002     C                   Z-ADD     SCOSS1        RCQITE
207600010124     C                   Z-ADD     SCOSS2        RCQITC
207700010124     C                   ADD       SCOSS3        RCQITC
207800010124    7C                   ENDSL
207900010124    6C                   ENDIF
208000010124    5C                   ENDIF
208100010124     C***************************
208200010124     C* SPEDIZIONI NON IN CONSEGNA
208300010124     C***************************
208400010124     C     SCOSCO        WHENEQ    'O'
208500010124     C* Anticipo
208600010124    5C     SCOORE        IFLT      *ZEROS
208700010124    6C                   SELECT
208800010124     C* Oltre 24 ore
208900010124     C     YY            WHENEQ    1
209000091002     C                   Z-ADD     SCOSS4        RCQOOh
209100091002     C                   Z-ADD     SCOSS1        RCQOOE
209200010124     C                   Z-ADD     SCOSS2        RCQOOC
209300010124     C                   ADD       SCOSS3        RCQOOC
209400010124     c* 24 ore
209500091002     C     YY            WHENEQ    2
209600091002     C                   Z-ADD     SCOSS4        RCQOAh
209700091002     C                   Z-ADD     SCOSS1        RCQOAE
209800010124     C                   Z-ADD     SCOSS2        RCQOAC
209900010124     C                   ADD       SCOSS3        RCQOAC
210000010124     C                   OTHER
210100010124     c* 12 ore
210200091002     C                   Z-ADD     SCOSS4        RCQOBh
210300091002     C                   Z-ADD     SCOSS1        RCQOBE
210400010124     C                   Z-ADD     SCOSS2        RCQOBC
210500010124     C                   ADD       SCOSS3        RCQOBC
210600010124    6C                   ENDSL
210700010124   X5C                   ELSE
210800010124     c* Conforme
210900010124    6C     SCOORE        IFEQ      *ZEROS
211000091002     C                   Z-ADD     SCOSS4        RCQO0h
211100091002     C                   Z-ADD     SCOSS1        RCQO0E
211200010124     C                   Z-ADD     SCOSS2        RCQO0C
211300010124     C                   ADD       SCOSS3        RCQO0C
211400010124   X6C                   ELSE
211500010124     c* Ritardo
211600010124    7C                   SELECT
211700010124     c* 12 ore
211800010124     C     YY            WHENEQ    1
211900091002     C                   Z-ADD     SCOSS4        RCQO1h
212000091002     C                   Z-ADD     SCOSS1        RCQO1E
212100010124     C                   Z-ADD     SCOSS2        RCQO1C
212200010124     C                   ADD       SCOSS3        RCQO1C
212300010124     c* 24 ore
212400010124     C     YY            WHENEQ    2
212500091002     C                   Z-ADD     SCOSS4        RCQO2h
212600091002     C                   Z-ADD     SCOSS1        RCQO2E
212700010124     C                   Z-ADD     SCOSS2        RCQO2C
212800010124     C                   ADD       SCOSS3        RCQO2C
212900010124     c* 36 ore
213000010124     C     YY            WHENEQ    3
213100091002     C                   Z-ADD     SCOSS4        RCQO3h
213200091002     C                   Z-ADD     SCOSS1        RCQO3E
213300010124     C                   Z-ADD     SCOSS2        RCQO3C
213400010124     C                   ADD       SCOSS3        RCQO3C
213500010124     c* 48 ore
213600010124     C     YY            WHENEQ    4
213700091002     C                   Z-ADD     SCOSS4        RCQO4h
213800091002     C                   Z-ADD     SCOSS1        RCQO4E
213900010124     C                   Z-ADD     SCOSS2        RCQO4C
214000010124     C                   ADD       SCOSS3        RCQO4C
214100010124     C                   OTHER
214200010124     c* oltre 48 ore
214300091002     C                   Z-ADD     SCOSS4        RCQOTh
214400091002     C                   Z-ADD     SCOSS1        RCQOTE
214500010124     C                   Z-ADD     SCOSS2        RCQOTC
214600010124     C                   ADD       SCOSS3        RCQOTC
214700010124    7C                   ENDSL
214800010124    6C                   ENDIF
214900010124    5C                   ENDIF
215000010124     C***************************
215100010124     C* SPEDIZIONI CONSEGNABILI
215200010124     C***************************
215300010124     C     SCOSCO        WHENEQ    'B'
215400010124    4C                   SELECT
215500010124     C* Anticipo
215600010124     C     SCOORE        WHENLT    0
215700091002     C                   Z-ADD     SCOSS4        RCQBAh
215800091002     C                   Z-ADD     SCOSS1        RCQBAE
215900010124     C                   Z-ADD     SCOSS2        RCQBAC
216000010124     C                   ADD       SCOSS3        RCQBAC
216100010124     c* Conformi
216200010124     C     SCOORE        WHENEQ    0
216300091002     C                   Z-ADD     SCOSS4        RCQB0h
216400091002     C                   Z-ADD     SCOSS1        RCQB0E
216500010124     C                   Z-ADD     SCOSS2        RCQB0C
216600010124     C                   ADD       SCOSS3        RCQB0C
216700010124     C                   OTHER
216800010124     c* Ritardo
216900091002     C                   Z-ADD     SCOSS4        RCQB1h
217000091002     C                   Z-ADD     SCOSS1        RCQB1E
217100010124     C                   Z-ADD     SCOSS2        RCQB1C
217200010124     C                   ADD       SCOSS3        RCQB1C
217300010124    4C                   ENDSL
217400010124    3C                   ENDSL
217500010124    2C                   ENDIF
217600010124     C     KSCO4         READE     FNSCO04L                               31
217700010124    1C                   ENDDO
217800010124     C                   ENDSR
217900970527     C*****************************************************************
218000970527     C* STAMPA RIGA DI TOTALE P.O. SE RICHIESTO DAL/AL
218100970527     C*****************************************************************
218200970527     C     PRTTOF        BEGSR
218300970527     C* SPUNTE PISTOLA 88
218400970527     C* Disk B
218500970527     C                   MOVE      *ZEROS        TOF88P
218600970623     C     TOFPAR        IFGT      *ZEROS
218700970527     C     TOF88B        DIV(H)    TOFPAR        $XX               7 4
218800970527     C     $XX           MULT      100           TOF88P
218900970527     C                   END
219000970527     C* Altri
219100970527     C                   MOVE      *ZEROS        TOF88C
219200970623     C     TOFPAR        IFGT      *ZEROS
219300970527     C     TOF88A        DIV(H)    TOFPAR        $XX               7 4
219400970527     C     $XX           MULT      100           TOF88C
219500970527     C                   END
219600970527     C* BOLLETTATO IN SOSPENSIONE
219700970527     C                   MOVE      *ZEROS        TOFBSP
219800970623     C     TOFBOL        IFGT      *ZEROS
219900970527     C     TOFBSS        DIV(H)    TOFBOL        $XX               7 4
220000970527     C     $XX           MULT      100           TOFBSP
220100970527     C                   END
220200970527     C* SPEDIZIONI A TERRA FUORI AFFIDABILITA'
220300970527     C                   MOVE      *ZEROS        TOFFAP
220400970623     C     TOFSDA        IFGT      *ZEROS
220500970527     C     TOFFAF        DIV(H)    TOFSDA        $XX               7 4
220600970527     C     $XX           MULT      100           TOFFAP
220700970527     C                   END
220800970606     C* COLLI PARTITI NON SPUNTATI IN ARRIVO
220900010123     C**                   MOVE *ZEROS    TOFNRP
221000010123     C**         TOFARR    IFGT *ZEROS
221100010123     C**         TOFNAR    DIV  TOFARR    $XX     74H
221200010123     C**         $XX       MULT 100       TOFNRP
221300010123     C**                   END
221400970527     C* SPEDIZIONI NON CONSEGNATE
221500970527     C                   MOVE      *ZEROS        TOFNCP
221600970623     C     TOFCSB        IFGT      *ZEROS
221700970527     C     TOFNCN        DIV(H)    TOFCSB        $XX               7 4
221800970527     C     $XX           MULT      100           TOFNCP
221900970527     C                   END
222000970527     C*
222100970527     C                   ADD       3             $RIGHE
222200970527     C                   ADD       3             $RIGHA
222300970527     C                   ADD       3             $RIGHD
222400990428     C     WPRTN         IFEQ      '1'
222500990428     C                   ADD       3             $RIGHN
222600990428     C                   END
222700970527     C                   WRITE     LV66TS
222800970527     C                   WRITE     LV66TF
222900970527     C                   WRITE     LV66TS
223000970527     C*
223100970527     C* STAMPO ANCHE PER CAPO AREA E CAPO DISTRETTO SE RICHIESTO
223200990428    1C     V1CINA        IFEQ      'S'
223300970527     C* PER CAPO AREA
223400970527     C                   WRITE     LV66TSA
223500970527     C                   WRITE     LV66TFA
223600970527     C                   WRITE     LV66TSA
223700990428    1C                   END
223800990428     C*
223900990428    1C     V1CIND        IFEQ      'S'
224000970527     C* PER CAPO DISTRETTO
224100970527     C                   WRITE     LV66TSD
224200970527     C                   WRITE     LV66TFD
224300970527     C                   WRITE     LV66TSD
224400970527    1C                   END
224500990428     C*
224600990428    1C     V1CINN        IFEQ      'S'
224700990428     C     WPRTN         ANDEQ     '1'
224800990507     C     WBRKN1        ANDEQ     '1'
224900990428     C* PER NETWORK
225000990428     C                   WRITE     LV66TSN
225100990428     C                   WRITE     LV66TFN
225200990428     C                   WRITE     LV66TSN
225300990428    1C                   END
225400970527     C*
225500970527     C* AZZERAMENTO CAMPI DI TOTALE
225600050831     C***                CLEAR                   TOFRIT
225700970527     C                   CLEAR                   TOFBSS
225800970527     C                   CLEAR                   TOFBOL
225900970527     C                   CLEAR                   TOF88B
226000970527     C                   CLEAR                   TOFPAR
226100970527     C                   CLEAR                   TOF88A
226200970527     C                   CLEAR                   TOFFAF
226300970527     C                   CLEAR                   TOFSDA
226400970527     C                   CLEAR                   TOFSSP
226500970527     C                   CLEAR                   TOFDIS
226600040405     C                   CLEAR                   TOFDISP
226700970527     C                   CLEAR                   TOFBAM
226800970527     C                   CLEAR                   TOFNCN
226900970527     C                   CLEAR                   TOFCSB
227000010124     C                   CLEAR                   TOFDSC
227100010124     C                   CLEAR                   TOFDSR
227200970527     C                   CLEAR                   TOFDIA
227300010123     C**                   CLEARTOFARR
227400010123     C**                   CLEARTOFNAR
227500970527     C                   ENDSR
227600990429     C*****************************************************************
227700990429     C* STAMPA TOTALI AREA
227800990429     C*****************************************************************
227900990429     C     ELATAR        BEGSR
228000990430     C                   SETON                                        41
228100990503     C                   ADD       5             $RIGHE
228200990429     C*   Salto pagina se ho superato il n° massimo di righe
228300990503     C     $RIGHE        IFGT      60
228400990429     C                   WRITE     LV66T1
228500990503     C                   Z-ADD     12            $RIGHE
228600990429    4C                   END
228700990430     C                   WRITE     LV66TST
228800990430     C*    Totali AREA   (BAR+SDI)
228900990430     C*    (imposto indice schiera totali AREA)
229000990430     C                   Z-ADD     1             NT                1 0
229100990503     C                   CLEAR                   W21
229200990503     C                   MOVEA     COS3          W21
229300990503     C                   MOVEA     PRTDAR        W21(6)
229400990503     C                   MOVEA(P)  W21           TOAVAR
229500990430     C                   EXSR      RIETAR
229600990429     C                   WRITE     LV66TA
229700990430     C                   SETOFF                                       41
229800990430     C*    Totali BARTOLINI
229900990430     C*    (imposto indice schiera totali network BAR)
230000990430     C                   Z-ADD     2             NT                1 0
230100990430     C                   MOVEL     COS1          TOAVAR
230200990430     C                   EXSR      RIETAR
230300990429     C                   WRITE     LV66TA
230400990430     C*    TOTALI SDI
230500990430     C*    (imposto indice schiera totali network SDI)
230600990430     C                   Z-ADD     3             NT                1 0
230700990503     C                   MOVEL     COS2          TOAVAR
230800990430     C                   EXSR      RIETAR
230900990429     C                   WRITE     LV66TA
231000990430     C                   WRITE     LV66TST
231100990430     C* MEMORIZZO SCHIERE TOTALI DISTRETTO
231200990430     C                   DO        3             NT
231300990430     C                   EXSR      RIEDIS
231400990430     C                   ENDDO
231500990429     C* AZZERO CAMPI DI TOTALE
231600990429     C                   EXSR      AZZTAR
231700990429     C                   ENDSR
231800990429     C*****************************************************************
231900990430     C* IMPOSTO CAMPI STAMPA DEI TOTALI AREA
232000990429     C*****************************************************************
232100990430     C     RIETAR        BEGSR
232200050831     C**                 Z-ADD     RIT(NT)       TOARIT
232300990429     C* BOLLETTATO IN SOSPENSIONE
232400990430     C                   Z-ADD     SOS(NT)       TOABSS
232500990429     C                   MOVE      *ZEROS        TOABSP
232600990430     C     TBL(NT)       IFGT      *ZEROS
232700990430     C     TOABSS        DIV(H)    TBL(NT)       $XX               7 4
232800990429     C     $XX           MULT      100           TOABSP
232900990429     C                   END
233000990429     C* SPUNTE PISTOLA 88
233100990429     C* Disk B
233200990429     C                   MOVE      *ZEROS        TOA88P
233300990430     C                   Z-ADD     B88(NT)       TOA88B
233400990430     C     PAR(NT)       IFGT      *ZEROS
233500990430     C     TOA88B        DIV(H)    PAR(NT)       $XX               7 4
233600990429     C     $XX           MULT      100           TOA88P
233700990429     C                   END
233800990429     C* Altri
233900990429     C                   MOVE      *ZEROS        TOA88C
234000990430     C                   Z-ADD     A88(NT)       TOA88A
234100990430     C     PAR(NT)       IFGT      *ZEROS
234200990430     C     TOA88A        DIV(H)    PAR(NT)       $XX               7 4
234300990429     C     $XX           MULT      100           TOA88C
234400990429     C                   END
234500990429     C* SPEDIZIONI A TERRA FUORI AFFIDABILITA'
234600990430     C                   Z-ADD     SFA(NT)       TOAFAF
234700990429     C                   MOVE      *ZEROS        TOAFAP
234800990430     C     TDA(NT)       IFGT      *ZEROS
234900990430     C     TOAFAF        DIV(H)    TDA(NT)       $XX               7 4
235000990429     C     $XX           MULT      100           TOAFAP
235100990429     C                   END
235200990429     C* SPEDIZIONI SPAREGGIATE
235300990430     C                   Z-ADD     SSP(NT)       TOASSP
235400990429     C* COLLI PARTITI IN DISGUIDO
235500990430     C                   Z-ADD     DIS(NT)       TOADIS
235600040405     C* disguidi non spuntati in partenza
235700040405     C                   Z-ADD     DISP(NT)      TOADISP
235800990429     C* MANCA DDT
235900990430     C                   Z-ADD     DDT(NT)       TOABAM
236000990429     C* COLLI PARTITI NON SPUNTATI IN ARRIVO
236100010123     C**                   MOVE *ZEROS    TOANRP
236200010123     C**                   Z-ADDNAR,NT    TOANAR
236300010123     C**         ARR,NT    IFGT *ZEROS
236400010123     C**         TOANAR    DIV  ARR,NT    $XX     74H
236500010123     C**         $XX       MULT 100       TOANRP
236600010123     C**                   END
236700990429     C* SPEDIZIONI NON CONSEGNATE
236800990429     C                   MOVE      *ZEROS        TOANCP
236900990430     C                   Z-ADD     NCN(NT)       TOANCN
237000990430     C     SCS(NT)       IFGT      *ZEROS
237100990430     C     TOANCN        DIV(H)    SCS(NT)       $XX               7 4
237200990429     C     $XX           MULT      100           TOANCP
237300990429     C                   END
237400010124     C                   Z-ADD     DCN(NT)       TOADSC
237500010124     C                   Z-ADD     DRT(NT)       TOADSR
237600990429     C* DISGUIDI IN AREA
237700990430     C                   Z-ADD     DIA(NT)       TOADIA
237800990429     C                   ENDSR
237900990430     C*****************************************************************
238000990430     C* IMPOSTO SCHIERE TOTALI DISTRETTO
238100990430     C*****************************************************************
238200990430     C     RIEDIS        BEGSR
238300050831     C**                 ADD       RIT(NT)       RID(NT)
238400990430     C                   ADD       SOS(NT)       SOD(NT)
238500990430     C                   ADD       TBL(NT)       TBD(NT)
238600990430     C                   ADD       B88(NT)       B8D(NT)
238700990430     C                   ADD       PAR(NT)       PAD(NT)
238800990430     C                   ADD       A88(NT)       A8D(NT)
238900990430     C                   ADD       SFA(NT)       SFD(NT)
239000990430     C                   ADD       TDA(NT)       TDD(NT)
239100990430     C                   ADD       SSP(NT)       SSD(NT)
239200990430     C                   ADD       DIS(NT)       DID(NT)
239300040405     C                   ADD       DISP(NT)      DISD(NT)
239400990430     C                   ADD       DDT(NT)       DDD(NT)
239500990430     C                   ADD       NCN(NT)       NCD(NT)
239600990430     C                   ADD       SCS(NT)       SCD(NT)
239700010124     C                   ADD       DCN(NT)       DCD(NT)
239800010124     C                   ADD       DRT(NT)       DRD(NT)
239900990430     C                   ADD       DIA(NT)       DSD(NT)
240000010123     C**                   ADD  ARR,NT    ARD,NT
240100010123     C**                   ADD  NAR,NT    NAD,NT
240200990430     C                   ENDSR
240300990430     C*****************************************************************
240400990430     C* STAMPA TOTALI DISTRETTO
240500990430     C*****************************************************************
240600990430     C     ELATDS        BEGSR
240700990503     C                   SETON                                        41
240800990430     C                   ADD       4             $RIGHE
240900990430     C*   Salto pagina se ho superato il n° massimo di righe
241000990430     C     $RIGHE        IFGT      60
241100990430     C                   WRITE     LV66T1
241200990430     C                   Z-ADD     11            $RIGHE
241300990430    4C                   END
241400990430     C*    Totali DISTRETTO (BAR+SDI)
241500990430     C*    (imposto indice schiera totali network Bar)
241600990430     C                   Z-ADD     1             NT                1 0
241700990503     C                   CLEAR                   W21
241800990503     C                   MOVEA     COS4          W21
241900990506     C                   MOVEA     PRTDED        W21(11)
242000990503     C                   MOVEA(P)  W21           TOAVAR
242100990430     C                   EXSR      RIETDS
242200990430     C                   WRITE     LV66TA
242300990503     C                   SETOFF                                       41
242400990430     C*    Totali BARTOLINI
242500990430     C*    (imposto indice schiera totali network BAR)
242600990430     C                   Z-ADD     2             NT                1 0
242700990430     C                   MOVEL     COS1          TOAVAR
242800990430     C                   EXSR      RIETDS
242900990430     C                   WRITE     LV66TA
243000990430     C*    TOTALI SDI
243100990430     C                   Z-ADD     3             NT                1 0
243200990430     C                   MOVEL     COS2          TOAVAR
243300990430     C                   EXSR      RIETDS
243400990430     C                   WRITE     LV66TA
243500990430     C                   WRITE     LV66TST
243600990430     C* AZZERO CAMPI DI TOTALE
243700990430     C                   EXSR      AZZTDS
243800990430     C                   ENDSR
243900990430     C*****************************************************************
244000990430     C* IMPOSTO CAMPI STAMPA DEI TOTALI DISTRETTO
244100990430     C*****************************************************************
244200990430     C     RIETDS        BEGSR
244300050831     C***                Z-ADD     RID(NT)       TOARIT
244400990430     C* BOLLETTATO IN SOSPENSIONE
244500990430     C                   Z-ADD     SOD(NT)       TOABSS
244600990430     C                   MOVE      *ZEROS        TOABSP
244700990430     C     TBD(NT)       IFGT      *ZEROS
244800990430     C     TOABSS        DIV(H)    TBD(NT)       $XX               7 4
244900990430     C     $XX           MULT      100           TOABSP
245000990430     C                   END
245100990430     C* SPUNTE PISTOLA 88
245200990430     C* Disk B
245300990430     C                   MOVE      *ZEROS        TOA88P
245400990430     C                   Z-ADD     B8D(NT)       TOA88B
245500990430     C     PAD(NT)       IFGT      *ZEROS
245600990430     C     TOA88B        DIV(H)    PAD(NT)       $XX               7 4
245700990430     C     $XX           MULT      100           TOA88P
245800990430     C                   END
245900990430     C* Altri
246000990430     C                   MOVE      *ZEROS        TOA88C
246100990430     C                   Z-ADD     A8D(NT)       TOA88A
246200990430     C     PAD(NT)       IFGT      *ZEROS
246300990430     C     TOA88A        DIV(H)    PAD(NT)       $XX               7 4
246400990430     C     $XX           MULT      100           TOA88C
246500990430     C                   END
246600990430     C* SPEDIZIONI A TERRA FUORI AFFIDABILITA'
246700990430     C                   Z-ADD     SFD(NT)       TOAFAF
246800990430     C                   MOVE      *ZEROS        TOAFAP
246900990430     C     TDD(NT)       IFGT      *ZEROS
247000990430     C     TOAFAF        DIV(H)    TDD(NT)       $XX               7 4
247100990430     C     $XX           MULT      100           TOAFAP
247200990430     C                   END
247300990430     C* SPEDIZIONI SPAREGGIATE
247400990430     C                   Z-ADD     SSD(NT)       TOASSP
247500990430     C* COLLI PARTITI IN DISGUIDO
247600990430     C                   Z-ADD     DID(NT)       TOADIS
247700040405     c* disguidi non spuntati in partenza
247800040405     C                   Z-ADD     DISD(NT)      TOADISP
247900990430     C* MANCA DDT
248000990430     C                   Z-ADD     DDD(NT)       TOABAM
248100990430     C* COLLI PARTITI NON SPUNTATI IN ARRIVO
248200010123     C**                   MOVE *ZEROS    TOANRP
248300010123     C**                   Z-ADDNAD,NT    TOANAR
248400010123     C**         ARD,NT    IFGT *ZEROS
248500010123     C**         TOANAR    DIV  ARD,NT    $XX     74H
248600010123     C**         $XX       MULT 100       TOANRP
248700010123     C**                   END
248800990430     C* SPEDIZIONI NON CONSEGNATE
248900990430     C                   MOVE      *ZEROS        TOANCP
249000990430     C                   Z-ADD     NCD(NT)       TOANCN
249100990430     C     SCD(NT)       IFGT      *ZEROS
249200990430     C     TOANCN        DIV(H)    SCD(NT)       $XX               7 4
249300990430     C     $XX           MULT      100           TOANCP
249400990430     C                   END
249500010124     C                   Z-ADD     DCD(NT)       TOADSC
249600010124     C                   Z-ADD     DRD(NT)       TOADSR
249700990430     C* DISGUIDI IN AREA
249800990430     C                   Z-ADD     DSD(NT)       TOADIA
249900990430     C                   ENDSR
250000990429     C*****************************************************************
250100990429     C* AZZERO CAMPI DI TOTALE A LIVELLO DI AREA
250200990429     C*****************************************************************
250300990429     C     AZZTAR        BEGSR
250400990430     C                   CLEAR                   RIT
250500990430     C                   CLEAR                   SOS
250600990430     C                   CLEAR                   TBL
250700990430     C                   CLEAR                   B88
250800990430     C                   CLEAR                   PAR
250900990430     C                   CLEAR                   A88
251000990430     C                   CLEAR                   SFA
251100990430     C                   CLEAR                   TDA
251200990430     C                   CLEAR                   SSP
251300990430     C                   CLEAR                   DIS
251400040405     C                   CLEAR                   DISP
251500990430     C                   CLEAR                   DDT
251600010123     C**                   CLEARARR
251700010123     C**                   CLEARNAR
251800990430     C                   CLEAR                   NCN
251900990430     C                   CLEAR                   SCS
252000010124     C                   CLEAR                   DCN
252100010124     C                   CLEAR                   DRT
252200990430     C                   CLEAR                   DIA
252300990429     C                   ENDSR
252400990430     C*****************************************************************
252500990430     C* AZZERO CAMPI DI TOTALE A LIVELLO DI DISTRETTO
252600990430     C*****************************************************************
252700990430     C     AZZTDS        BEGSR
252800990430     C                   CLEAR                   RID
252900990430     C                   CLEAR                   SOD
253000990430     C                   CLEAR                   TBD
253100990430     C                   CLEAR                   B8D
253200990430     C                   CLEAR                   PAD
253300990430     C                   CLEAR                   A8D
253400990430     C                   CLEAR                   SFD
253500990430     C                   CLEAR                   TDD
253600990430     C                   CLEAR                   SSD
253700990430     C                   CLEAR                   DID
253800040405     C                   CLEAR                   DISD
253900990430     C                   CLEAR                   DDD
254000010123     C**                   CLEARARD
254100010123     C**                   CLEARNAD
254200990430     C                   CLEAR                   NCD
254300990430     C                   CLEAR                   SCD
254400010124     C                   CLEAR                   DCD
254500010124     C                   CLEAR                   DRD
254600990430     C                   CLEAR                   DSD
254700990430     C                   ENDSR
254800970225     C*****************************************************************
254900970225     C* DECODIFICA CODICE DISTRETTO
255000970225     C*****************************************************************
255100970225     C     DECDST        BEGSR
255200970225     C* Chaino tabella 17 per decodifica
255300970225     C                   MOVEL     '17'          KCOD
255400970225     C                   MOVEL(P)  ORGFL3        KKEY
255500970225     C     KTAB          CHAIN     TABEL00F                           30
255600970225    2C     *IN30         IFEQ      *OFF
255700970225     C     TBLFLG        ANDEQ     *BLANKS
255800970401     C                   MOVEL     TBLUNI        DS17
255900970401     C                   MOVEL     §17DES        PRTDED
256000970227     C                   ELSE
256100970227     C                   MOVE      *BLANKS       PRTDED
256200970225    2C                   END
256300990503     C                   MOVE      ORGFL3        PRTFL3
256400970225     C                   ENDSR
256500970225     C*****************************************************************
256600970225     C* DECODIFICA CODICE AREA
256700970225     C*****************************************************************
256800970225     C     DECCAR        BEGSR
256900970225     C* Chaino tabella 05 per decodifica
257000970225     C                   MOVEL     '05'          KCOD
257100970225     C                   MOVEL(P)  ORGCAR        KKEY
257200970225     C     KTAB          CHAIN     TABEL00F                           30
257300970225    2C     *IN30         IFEQ      *OFF
257400970225     C     TBLFLG        ANDEQ     *BLANKS
257500970401     C                   MOVEL     TBLUNI        DS05
257600970401     C                   MOVEL     §05DES        PRTDAR
257700970227     C                   ELSE
257800970227     C                   MOVE      *BLANKS       PRTDAR
257900970225    2C                   END
258000990503     C                   MOVE      ORGCAR        PRTCAR
258100970225     C                   ENDSR
258200970401     C*****************************************************************
258300970401     C* OVRPRTF FILE DI STAMPA PER INVIO A CAPO AREA
258400970401     C*****************************************************************
258500970401     C     OVPRTA        BEGSR
258600970401     C*
258700970401     C     OPNA          IFEQ      '1'
258800970401     C* STAMPO RIGA "FINE STAMPA"
258900970401     C                   ADD       3             $RIGHA
259000970401     C     $RIGHA        IFGT      65
259100970401     C                   WRITE     LV66T1A
259200970401     C                   WRITE     LV66T3A
259300970401     C                   END
259400970401     C                   WRITE     LV66E2A
259500970401     C* CHIUDO FILE PRINTER SE ERA APERTO
259600970401     C                   CLOSE     FNLV66PA
259700970401     C                   END
259800970401     C* ESEGUO OVRPRTF
259900970401     C                   Z-ADD     65            LUNG
260000970401     C                   MOVEA     CMD(1)        W65
260100970401     C     §05OTQ        IFNE      *BLANKS
260200970401     C                   MOVEA     §05OTQ        W65(31)
260300970401     C                   ELSE
260400970402     C                   MOVEA     '*JOB'        W65(31)
260500970401     C                   END
260600970401     C                   MOVEA     W65           COMMAN
260700970401     C                   CALL      'QCMDEXC'
260800970401     C                   PARM                    COMMAN           80
260900970401     C                   PARM                    LUNG             15 5
261000970401     C                   OPEN      FNLV66PA
261100970401     C*
261200970401     C                   MOVE      '1'           OPNA              1
261300970401     C* STAMPO INTESTAZIONE
261400970401     C                   WRITE     LV66T1A
261500970401     C                   Z-ADD     7             $RIGHA            2 0
261600970401     C                   ADD       3             $RIGHA
261700970401     C                   WRITE     LV66T3A
261800970401     C                   WRITE     LV66T4A
261900970401     C                   WRITE     LV66T3A
262000970401     C*
262100970401     C                   ENDSR
262200970401     C*****************************************************************
262300970401     C* OVRPRTF FILE DI STAMPA PER INVIO A CAPO DISTRETTO
262400970401     C*****************************************************************
262500970401     C     OVPRTD        BEGSR
262600970401     C*
262700970401     C     OPND          IFEQ      '1'
262800970401     C* STAMPO RIGA "FINE STAMPA"
262900970401     C                   ADD       3             $RIGHD
263000970401     C     $RIGHD        IFGT      65
263100970401     C                   WRITE     LV66T1D
263200970401     C                   WRITE     LV66T3D
263300970401     C                   END
263400970401     C                   WRITE     LV66E2D
263500970401     C* CHIUDO FILE PRINTER SE ERA APERTO
263600970401     C                   CLOSE     FNLV66PD
263700970401     C                   END
263800970401     C* ESEGUO OVRPRTF
263900970401     C                   Z-ADD     65            LUNG
264000970401     C                   MOVEA     CMD(2)        W65
264100970401     C     §17OTQ        IFNE      *BLANKS
264200970401     C                   MOVEA     §17OTQ        W65(31)
264300970401     C                   ELSE
264400970402     C                   MOVEA     '*JOB'        W65(31)
264500970401     C                   END
264600970401     C                   MOVEA     W65           COMMAN
264700970401     C                   CALL      'QCMDEXC'
264800970401     C                   PARM                    COMMAN           80
264900970401     C                   PARM                    LUNG             15 5
265000970401     C                   OPEN      FNLV66PD
265100970401     C*
265200970401     C                   MOVE      '1'           OPND              1
265300970401     C* STAMPO INTESTAZIONE
265400970401     C                   WRITE     LV66T1D
265500970401     C                   Z-ADD     7             $RIGHD            2 0
265600970401     C                   ADD       3             $RIGHD
265700970401     C                   WRITE     LV66T3D
265800970401     C                   WRITE     LV66T4D
265900970401     C                   WRITE     LV66T3D
266000970401     C*
266100970401     C                   ENDSR
266200990428     C*****************************************************************
266300990428     C* OVRPRTF FILE DI STAMPA PER INVIO A NETWORK
266400990428     C*****************************************************************
266500990428     C     OVPRTN        BEGSR
266600990428     C*
266700990428     C     OPNN          IFEQ      '1'
266800990428     C* STAMPO RIGA "FINE STAMPA"
266900990428     C                   ADD       3             $RIGHN
267000990428     C     $RIGHN        IFGT      65
267100990428     C                   WRITE     LV66T1N
267200990428     C                   WRITE     LV66T3N
267300990428     C                   END
267400990428     C                   WRITE     LV66E2N
267500990428     C* CHIUDO FILE PRINTER SE ERA APERTO
267600990428     C                   CLOSE     FNLV66PN
267700990428     C                   END
267800990428     C* ESEGUO OVRPRTF
267900990428     C                   Z-ADD     65            LUNG
268000990428     C                   MOVEA     CMD(3)        W65
268100990428     C                   SELECT
268200990428     C     V1CNTW        WHENEQ    'B'
268300990428     C     §05OTQ        ANDNE     *BLANKS
268400990428     C                   MOVEA     §05OTQ        W65(31)
268500990428     C     V1CNTW        WHENEQ    'S'
268600990428     C     §05OTS        ANDNE     *BLANKS
268700990428     C                   MOVEA     §05OTS        W65(31)
268800990428     C                   OTHER
268900990428     C                   MOVEA     '*JOB'        W65(31)
269000990428     C                   ENDSL
269100990428     C*
269200990428     C                   MOVEA     W65           COMMAN
269300990428     C                   CALL      'QCMDEXC'
269400990428     C                   PARM                    COMMAN           80
269500990428     C                   PARM                    LUNG             15 5
269600990428     C                   OPEN      FNLV66PN
269700990428     C*
269800990428     C                   MOVE      '1'           OPNN              1
269900990428     C* STAMPO INTESTAZIONE
270000990428     C                   WRITE     LV66T1N
270100990428     C                   Z-ADD     7             $RIGHN            2 0
270200990428     C                   ADD       3             $RIGHN
270300990428     C                   WRITE     LV66T3N
270400990428     C                   WRITE     LV66T4N
270500990428     C                   WRITE     LV66T3N
270600990428     C*
270700990428     C                   ENDSR
270800970225     C*****************************************************************
270900970224     C* OPERAZIONI INIZIALI
271000970224     C*****************************************************************
271100970224     C     *INZSR        BEGSR
271200970224     C*
271300970224     C     *ENTRY        PLIST
271400970224     C                   PARM                    KPJBA
271500970225     C                   MOVEL     KPJBU         PARAM
271600970224     C*
271700970224     C                   Z-ADD     1             CODUT
271800970224     C                   CALL      'X§PARUT'
271900970224     C                   PARM                    UT§DSE
272000970224     C                   MOVEL     REC80         CNCR80
272100970224     C                   MOVEL     RAGUT         RSUT
272200970226     C*
272300970527     C     PARDAL        IFEQ      *ZEROS
272400970527     C                   Z-ADD     PARDEL        PARDAL
272500970527     C                   END
272600970224     C*
272700970224     C                   TIME                    WTIME            14 0
272800970224     C                   MOVE      WTIME         WDATE             8 0
272900970311     C                   MOVEL     WTIME         UTIME             6 0
273000970224     C                   CLEAR                   WLBDA8
273100970224     C                   MOVE      WDATE         G08DAT
273200970224     C                   CALL      'XSRDA8'
273300970224     C                   PARM                    WLBDA8
273400970224     C                   MOVE      G08INV        WDATEU            8 0
273500990428     C*
273600990428     C     V1CINN        IFEQ      'N'
273700990428     C                   CLEAR                   V1CNTW
273800990428     C                   END
273900990503     C*
274000990503     C     V1CCRP        IFEQ      'S'
274100010124     C                   OPEN      WFRCQ11L
274200990503     C                   END
274300990504     C* Inizializzo schiere totali area visto che le somme sulle
274400990504     C* stesse sono condizionate dagli indicatori di "dati non pervenu-
274500990504     C* ti mentre
274600990504     C*                    EXSR AZZTAR
274700000626     C*
274800000626     C* LEGGO AZORG E CARICO I TOTALI
274900000626     C                   CLEAR                   CC
275000060131     C                   CLEAR                   dd                4 0
275100000626     C     *LOVAL        SETLL     AZORG01L
275200000626     C                   READ      AZORG01L                               31
275300000626     C     *IN31         DOWEQ     *OFF
275400000626     C     ORGFAG        IFEQ      'F'
275500000626     C     ORGFAG        OREQ      'A'
275600000626     C     ORGFVA        IFEQ      ' '
275700000626     C                   MOVEL     ORGDE3        OG143
275800000626     C     §OGPT         IFEQ      'S'
275900000626     C                   ADD       1             CC                3 0
276000000626     C                   Z-ADD     ORGFIL        PT(CC)
276100000626     C                   ENDIF
276200060131     c* Schiera dei p.o. esteri
276300060131     c                   if        §ogntw='EEX' or §ogntw=
276400060131     c                             'DPD' or §ogntw='FED'
276500060131     C                   ADD       1             dd
276600060131     C                   Z-ADD     ORGFIL        EST(DD)
276700060131     c                   endif
276800060131     c
276900000626     C                   ENDIF
277000000626     C                   ENDIF
277100000626     C                   READ      AZORG01L                               31
277200000626     C                   ENDDO
277300060131     c*
277400060131     c* Reperisco data decorrenza separazione dati partenza italia/estero
277500090930     C                   CLEAR                   dsbs02
277600090930     c                   clear                   dvpodeco
277700090930     C                   MOVEL     'L'           T02tla
277800090930     C                   MOVEL     'C'           T02MOD
277900090930     C                   MOVEL     KNSIF         T02SIF
278000090930     C                   MOVEL     'VPO'         T02COD
278100090930     c                   movel(P)  'DECO'        t02ke1
278200090930     C                   CALL      'TIBS02R'
278300090930     C                   PARM                    KPJBA
278400090930     C                   PARM                    dsbs02
278500090930    2C                   IF        T02ERR = *BLANKS
278600090930     C                   MOVEL     T02uni        dvpodeco
278700090930    2C                   ENDIF
278800091001     c
278900091001     c* Se tipo servizio H attivo, stampo lo colonna
279000091001     c                   if        pardal>=§vpotsph
279100091001     c                   seton                                        10
279200091001     c                   endif
279300060131     c
279400090605      * carico le descrizioni fisse dei tipi servizio
279500090605      * chiodo l'indice di schiera per ogni tipo servizio
279600090930      * 1=H - 2=E - 3=C  4=D
279700090605     c                   clear                   ds5e
279800090605     c                   clear                   ens
279900090605     c                   eval      kcod = '5E'
280000090605     c     ktab2         setll     tabel00f
280100090605     c                   do        *hival
280200090605     c     ktab2         reade(n)  tabel00f
280300090605     c                   if        %eof(tabel00f)
280400090605     c                   leave
280500090605     c                   endif
280600090605     c                   eval      ds5e = tbluni
280700090605     c                   if        tblkey = 'C'
280800090930     c                   eval      ens(03) = §5ed05
280900090605     c                   endif
281000090605     c                   if        tblkey = 'D'
281100090930     c                   eval      ens(04) = §5ed05
281200090605     c                   endif
281300090605     c                   if        tblkey = 'E'
281400090930     c                   eval      ens(02) = §5ed05
281500090605     c                   endif
281600090930     c                   if        tblkey = 'H'
281700090930     c                   eval      ens(01) = §5ed05
281800090930     c                   endif
281900090605     c                   enddo
282000060131     c
282100000626     C**
282200970224     C*
282300970224     C     KTAB          KLIST
282400970224     C                   KFLD                    CODUT
282500970224     C                   KFLD                    KCOD
282600970224     C                   KFLD                    KKEY
282700090605
282800090605     C     KTAB2         KLIST
282900090605     C                   KFLD                    CODUT
283000090605     C                   KFLD                    KCOD
283100970226     C*
283200970226     C     KORG2         KLIST
283300970226     C                   KFLD                    V1CDST
283400970226     C                   KFLD                    PARCAR
283500970224     C*
283600970226     C     KSBO          KLIST
283700970226     C                   KFLD                    WSOS
283800970226     C                   KFLD                    ORGFIL
283900990517     C                   KFLD                    WDATEL
284000970226     C*
284100970226     C     KSPA          KLIST
284200971125     C                   KFLD                    PRTTFP
284300970527     C                   KFLD                    WDATEL
284400060131     C     KSPAfil       KLIST
284500060131     C                   KFLD                    PRTTFP
284600060131     C                   KFLD                    orgfil
284700060131     C                   KFLD                    WDATEL
284800970226     C*
284900970226     C     KSAT          KLIST
285000971125     C                   KFLD                    PRTTFP
285100970226     C                   KFLD                    WTFA
285200970527     C                   KFLD                    WDATEL
285300060131     C     KSATfil       KLIST
285400060131     C                   KFLD                    PRTTFP
285500060131     C                   KFLD                    WTFA
285600060131     C                   KFLD                    orgfil
285700060131     C                   KFLD                    WDATEL
285800970311     C*
285900990504     C***        KSAT2     KLIST
286000990504     C***                  KFLD           PRTTFP
286100990504     C***                  KFLD           WTFA
286200990504     C***                  KFLD           WDATEL
286300990504     C***                  KFLD           WTRE
286400990409     C     KANM4         KLIST
286500990409     C                   KFLD                    KCAA
286600990409     C                   KFLD                    KDAO
286700990409     C                   KFLD                    KLNA
286800970311     C*
286900970226     C                   CLEAR                   WTFA
287000970226     C*
287100970226     C     KSCO          KLIST
287200970226     C                   KFLD                    ORGFIL
287300970226     C                   KFLD                    WSCO
287400970226     C                   KFLD                    WGBO
287500970527     C                   KFLD                    WDATEL
287600010124     C*
287700010124     C     KSCO4         KLIST
287800010124     C                   KFLD                    RCQFIL
287900010124     C                   KFLD                    RCQDAT
288000970226     C*
288100970226     C     KANT          KLIST
288200970226     C                   KFLD                    ORGFIL
288300970527     C                   KFLD                    WDATEL
288400970226     C                   KFLD                    WCAA
288500970606     C*
288600970606     C     KSAR          KLIST
288700970606     C                   KFLD                    ORGFIL
288800970606     C                   KFLD                    WTRE
288900990517     C                   KFLD                    WDATEL
289000990409     C     KCLN          KLIST
289100990409     C                   KFLD                    KTFP
289200990409     C                   KFLD                    KLNA
289300990409     C                   KFLD                    KANN
289400990409     C                   KFLD                    KMES
289500990504     C*
289600990504     C     KCAE5         KLIST
289700990504     C                   KFLD                    KEPA
289800990504     C                   KFLD                    ORGFIL
289900990503     C*
290000990503     C     KRCQ          KLIST
290100990503     C                   KFLD                    WDATEL
290200990503     C                   KFLD                    ORGFIL
290300970226     C*
290400990409     C     *LIKE         DEFINE    ANMCAA        KCAA
290500990409     C     *LIKE         DEFINE    ANMDAO        KDAO
290600970527     C     *LIKE         DEFINE    PARDEL        WDATEL
290700970527     C     *LIKE         DEFINE    PARTGI        WTGI
290800000208     C     *LIKE         DEFINE    PRTTFP        SAVTFP
290900000208     C     *LIKE         DEFINE    PRTTFA        SAVTFA
291000990409     C     *LIKE         DEFINE    CLNTFP        KTFP
291100990409     C     *LIKE         DEFINE    CLNANN        KANN
291200990409     C     *LIKE         DEFINE    CLNMES        KMES
291300990409     C     *LIKE         DEFINE    ANMLNA        KLNA
291400990409     C     *LIKE         DEFINE    PARDEL        WDATAM
291500970527     C*
291600970224     C     *LIKE         DEFINE    TBLCOD        KCOD
291700970224     C     *LIKE         DEFINE    TBLKEY        KKEY
291800970226     C*
291900970226     C     *LIKE         DEFINE    SBOSOS        WSOS
292000970226     C     *LIKE         DEFINE    SBOSEA        TOTBOL
292100970226     C*
292200970226     C     *LIKE         DEFINE    SPASDE        SDA123
292300970226     C     *LIKE         DEFINE    SPACPC        TOTPAR
292400970226     C     *LIKE         DEFINE    SPA88A        TP88A
292500970226     C     *LIKE         DEFINE    SPA88B        TP88B
292600970226     C*
292700970226     C     *LIKE         DEFINE    SATTFA        WTFA
292800970311     C     *LIKE         DEFINE    SATTRE        WTRE
292900970226     C     *LIKE         DEFINE    SATSDE        WTERR
293000970226     C     *LIKE         DEFINE    SATSDE        WSPAR
293100970225     C*
293200970226     C     *LIKE         DEFINE    SCOSCO        WSCO
293300970226     C     *LIKE         DEFINE    SCOGBO        WGBO
293400970226     C     *LIKE         DEFINE    SCOSS1        CSB123
293500970226     C*
293600970226     C     *LIKE         DEFINE    ANTCAA        WCAA
293700970226     C     *LIKE         DEFINE    ANTTOT        TOTBAM
293800970226     C     *LIKE         DEFINE    ANTTOT        TOTDAR
293900970226     C     *LIKE         DEFINE    ANTTOT        TOTDIS
294000040405     C     *LIKE         DEFINE    ANTTOT        TOTDISP
294100970226     C     *LIKE         DEFINE    ANTTOT        TOTRIT
294200970606     C*
294300970606     C     *LIKE         DEFINE    SARPAR        TOTNAR
294400970606     C     *LIKE         DEFINE    SARCLL        TOTARR
294500990428     C*
294600990428     C     *LIKE         DEFINE    V1CNTW        COMDIT
294700990504     C*
294800990504     C     *LIKE         DEFINE    CAEEPA        KEPA
294900010124     C     *LIKE         DEFINE    SCOORE        WORRIT
295000970226     C*
295100970224     C                   ENDSR
295200970401** GBO - Valori per impostare gbo in calcolo spedizioni non consegnate
295300970226GLCN
295400970401** CMD - Comandi per qcmdexc
295500970401OVRPRTF FNLV66PA FNLV66P OUTQ(          ) FORMTYPE('RPQ_AREA')
295600970401OVRPRTF FNLV66PD FNLV66P OUTQ(          ) FORMTYPE('RPQ_DIST')
295700990428OVRPRTF FNLV66PN FNLV66P OUTQ(          ) FORMTYPE('RPQ_NETW')
295800010124** SCA
295900010124999024012
296000010124** SCR
296100010124012024036048999
