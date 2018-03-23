000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200960208     H* FNLS49R *----------------------------------------------------*
000300960208     H*              ELENCO TRAZIONI IN RITARDO           - BATCH -  *
000400000000     H*--------------------------------------------------------------*
000500960216     FTNTLT01L  IF   E           K DISK
000600960216     FTNTLR01L  IF   E           K DISK
000700960216     F                                     RENAME(TNTLR000:TNTLR001)
000800960216     FTNTLR02L  IF   E           K DISK
000900960216     FFNFGV03L  IF   E           K DISK
001000000427     FFNFGW01L  IF   E           K DISK
001100960212     FFNFVA05L  IF   E           K DISK
001200000427     FFNFWA01L  IF   E           K DISK
001300960208     FFNFV201L  IF   E           K DISK
001400960208     FFNFV203L  IF   E           K DISK
001500960208     F                                     RENAME(FNFV2000:FNFV2003)
001600960208     FFNFV401L  IF   E           K DISK
001700960208     FAZORG01L  IF   E           K DISK
001800960208     FTABEL00F  IF   E           K DISK
001900960212     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
002000960216     F                                     FORMLEN(066)
002100960216     F                                     FORMOFL(063)
002200950720     D*
002300960208     D* DEFINIZIONE SCHIERE
002400960219     D*
002500000427     D PLP             S              3    DIM(300)
002600960216     D SK              S              3    DIM(100)
002700960219     D K14             S              1    DIM(14)
002800960216     D*
002900960216     D TTC             S              1    DIM(40)
003000960216     D DTC             S             35    DIM(40)
003100960216     D ITC             S              1    DIM(40)
003200960216     D GFV             S              1    DIM(20) CTDATA PERRCD(20)
003300960220     D TS              S             66    DIM(27) CTDATA PERRCD(1)
003400960216     D**
003500950406     D* PARAMETRI PASSATI DAL FILTRO
003600000000     D PARAM           DS
003700960215     D* CODICE FILIALE
003800960215     D  PARFIL                 1      3  0
003900960215     D* DATA FOGLIO VIAGGIO PARTENZA  DAL / AL   (AAAA/MM/GG)
004000960215     D  PARDFD                 4     11  0
004100960215     D  PARDFA                12     19  0
004200960215     D*
004300960215     D* DATA FOGLIO ARRIVI            DAL / AL   (AAAA/MM/GG)
004400960215     D  PARDAD                20     27  0
004500960215     D  PARDAA                28     35  0
004600960215     D* TIPO ELENCO RICHIESTO
004700960215     D  PARTTB                36     36
004800960216     D**
004900960216     D WLBDAT          DS
005000960216     D  G02DAT                 1      8  0
005100960216     D  G02INV                 9     16  0
005200960216     D  G02ERR                17     17
005300960216     D  G02TGI                18     22  0
005400960216     D WGIDAT          DS
005500960216     D  GI8DAT                 1      8  0
005600960216     D  GI8INV                 9     16  0
005700960216     D  GI8TGI                17     21  0
005800960219     D                 DS
005900960219     D  FGVLNA                 1      3  0
006000960219     D  FGVFFV                 4    243
006100960219     D  FGVFF2               244    450
006200000427     D  FGWFF3               451    690
006300000427     D  FGWFF4               691    900
006400000427     D  VUOTO                901    903
006500000427     D  FFV                    1    903
006600000427     D                                     DIM(301)
006700960219     D                 DS
006800960219     D  WLNA2                  1      3  0
006900960219     D  FGVFLP                 4    243
007000960219     D  FGVFL2               244    450
007100000427     D  FGWFL3               451    690
007200000427     D  FGWFL4               691    900
007300000427     D  VUOTO2               901    903
007400000427     D  FLP                    1    903
007500000427     D                                     DIM(301)
007600960219     D                 DS
007700960219     D  FVALNA                 1      3  0
007800960219     D  FVAFFV                 4    243
007900960219     D  FVAFF2               244    450
008000000427     D  FWAFF3               451    690
008100000427     D  FWAFF4               691    900
008200000427     D  VUOTO3               901    903
008300000427     D  FFA                    1    903
008400000427     D                                     DIM(301)
008500960219     D                 DS
008600960219     D  WLNA3                  1      3  0
008700960219     D  FVAFLP                 4    243
008800960219     D  FVAFL2               244    450
008900000427     D  FVAFL3               451    690
009000000427     D  FWAFL4               691    900
009100000427     D  VUOTO4               901    903
009200000427     D  FLA                    1    903
009300000427     D                                     DIM(301)
009400960216     D*
009500940719     D                 DS
009600940719     D  AA                     1      4  0
009700940719     D  MM                     5      6  0
009800940719     D  GG                     7      8  0
009900960208     D  WDATA                  1      8  0
010000960216     D DSTC          E DS
010100960216     D KPJBA         E DS
010200900406     D CNCR80        E DS
010300971230     D DSLV55        E DS                  EXTNAME(FNLV55DS)
010400900406     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
010500960216     D* DS PER PGM TNTL35R - CALCOLO MINUTI RITARDO ORE TEORICA/REALE
010600960216     D DSTL35        E DS                  EXTNAME(TNTL35DS)
010700960216     D*
010800960216     D* DEFINIZIONE COSTANTI
010900960216     D*
011000960216     D CTESP           C                   CONST('P A R T E N Z A')
011100960216     D CTESA           C                   CONST('  A R R I V O  ')
011200960216     D CDEST           C                   CONST('DESTINAZIONI  ')
011300960926     D CCARIC          C                   CONST('P.O. DI CARICO')
011400960219     D CPARTE          C                   CONST('P   A   R   T   E   -
011500960219     D                                     N   Z   A ')
011600960220     D CARRIV          C                   CONST('    A   R   R   I   -
011700960220     D                                      V   O    ')
011800960219     D CPRSAR          C                   CONST(' PRESENTE RITARDO IN-
011900960219     D                                      ARRIVO ')
012000960219     D CPRSPA          C                   CONST('PRESENTE RITARDO IN -
012100960219     D                                     PARTENZA')
012200960216     D CLNA            C                   CONST('LNA  ARRIVO REALE ')
012300960216     D CLNP            C                   CONST('LNP PARTENZA REALE')
012400960220     D CTRANS          C                   CONST('XX trans.da XX')
012500921012     C*****************************************************************
012600921012     C* RIEPILOGO INDICATORI
012700921012     C*****************************************************************
012800960216     C* 01    - ELENCO TRAZIONI IN PARTENZA: RITARDI DI PARTENZA
012900960220     C* 02    - ELENCO TRAZIONI IN PARTENZA: RITARDI DI ARRIVO
013000960216     C* 03    - ELENCO TRAZIONI IN ARRIVO  : RITARDI DI PARTENZA
013100960216     C* 04    - ELENCO TRAZIONI IN ARRIVO  : RITARDI DI ARRIVO
013200960216     C* 07      IMMESSA DATA FOGLIO VIAGGIO AL
013300960216     C* 08    - IMMESSA DATA FOGLIO ARRIVI  AL
013400960216     C* 09    - STAMPO CATEGORIA TRAINO : BIS
013500960216     C* 10    - DEVO STAMPARE DELLE FILIALI DI CARICO/SCARICO IN 2 RIGA
013600960220     C* 11    - STAMPA DALLA 2 VOLTA IN POI I SEPARATORI DI COLONNA
013700960220     C* 12    - STAMPA MINUTI CAMPO LUNGO 4
013800960220     C* 13    - NON STAMPA LA 1 VOLTA FV4NOT
013900960220     C* 15    - STAMPA MINUTI
014000960220     C* 16    - NON STAMPO LA PRIMA VOLTA I PIEDI
014100960216     C* 20    - STAMPO DELLA 2 RIGA DI DETTAGLIO FV4NOT
014200960216     C* 21    - STAMPO DELLA 2 RIGA DI LA LINEA IN RITARDO
014300960216     C* 22    - TROVATO ALMENO UN RITARDO
014400960216     C* 30/32 - DI COMODO
014500960219     C* 98    -
014600960219     C* 99    -
014700921012     C*****************************************************************
014800000000     C     *ENTRY        PLIST
014900000000     C                   PARM                    KPJBA
015000921013     C                   MOVEL     KPJBU         PARAM
015100960208     C*
015200960216     C* IMPOSTO LE DATE E IL TIPO TABULATO
015300960216     C                   EXSR      CTRSEL
015400960216     C*
015500960216     C* TRAZIONI IN PARTENZA: RITARDI DI PARTENZA
015600960216     C     WTAB1         IFEQ      'S'
015700960216     C                   SETON                                        01
015800090710     C                   SETOff                                       22
015900960216     C                   EXSR      ELAB
016000960216     C                   ENDIF
016100960216     C* TRAZIONI IN PARTENZA: RITARDI DI ARRIVO
016200960216     C     WTAB2         IFEQ      'S'
016300960216     C                   SETOFF                                       01
016400090710     C                   SETOff                                       22
016500960216     C                   SETON                                        02
016600960216     C                   EXSR      ELAB
016700960216     C                   ENDIF
016800960216     C* TRAZIONI IN ARRIVO  : RITARDI DI PARTENZA
016900960216     C     WTAB3         IFEQ      'S'
017000960216     C                   SETOFF                                       0102
017100090710     C                   SETOff                                       22
017200960216     C                   SETON                                        03
017300960216     C                   EXSR      ELAB
017400960216     C                   ENDIF
017500960216     C* TRAZIONI IN ARRIVO  : RITARDI DI PARTENZA
017600960216     C     WTAB4         IFEQ      'S'
017700960216     C                   SETOFF                                       010203
017800090710     C                   SETOff                                       22
017900960216     C                   SETON                                        04
018000960216     C                   EXSR      ELAB
018100960216     C                   ENDIF
018200971230     C*
018300971230     C                   MOVEL     'C'           D55TLA
018400971230     C                   CALL      'FNLV55R'
018500971230     C                   PARM                    DSLV55
018600960216     C*
018700900406     C                   SETON                                        LR
018800960216     C*
018900960216     C*--- IMPOSTO LE DATE RICHIESTE --------------------------------*
019000960216     C     CTRSEL        BEGSR
019100960216     C**
019200960216     C* DECODIFICA FILIALE RICHIESTA
019300960216     C     PARFIL        CHAIN     AZORG01L                           30
019400960216     C  N30              MOVEL     ORGDES        DESFIL           20
019500960216     C   30              CLEAR                   DESFIL
019600960209     C*
019700960216     C* GIRO DATA FOGLIO VIAGGIO DAL
019800960209     C                   Z-ADD     PARDFD        WDATA
019900960209     C                   Z-ADD     MM            GGMM              4 0
020000960209     C                   MOVEL     GG            GGMM
020100960216     C                   Z-ADD     AA            PRTDFD
020200960216     C                   MOVEL     GGMM          PRTDFD
020300960216     C* GIRO DATA FOGLIO VIAGGIO AL
020400960209     C     PARDFA        IFGT      0
020500960209     C                   Z-ADD     PARDFA        WDATA
020600960209     C                   Z-ADD     MM            GGMM
020700960209     C                   MOVEL     GG            GGMM
020800960216     C                   Z-ADD     AA            PRTDFA
020900960216     C                   MOVEL     GGMM          PRTDFA
021000960209     C                   ENDIF
021100960209     C*
021200960209     C* SE DATA AL = 0 METTO DATA DAL IN DATA AL
021300960209     C     PARDFA        IFEQ      0
021400960209     C                   Z-ADD     PARDFD        PARDFA
021500960216     C                   Z-ADD     PRTDFD        PRTDFA
021600960209     C                   ENDIF
021700960209     C*
021800960209     C* 07 ON  - IMMESSA DATA AL
021900960209     C     PARDFA        IFGT      0
022000960209     C     PARDFA        ANDNE     PARDFD
022100960209     C                   SETON                                        07
022200960209     C                   ENDIF
022300960216     C* GIRO DATA FOGLIO ARRIVO DAL
022400960216     C                   Z-ADD     PARDAD        WDATA
022500960216     C                   Z-ADD     MM            GGMM              4 0
022600960216     C                   MOVEL     GG            GGMM
022700960216     C                   Z-ADD     AA            PRTDAD
022800960216     C                   MOVEL     GGMM          PRTDAD
022900960216     C* GIRO DATA FOGLIO  ARRIVO AL
023000960216     C     PARDAA        IFGT      0
023100960216     C                   Z-ADD     PARDAA        WDATA
023200960216     C                   Z-ADD     MM            GGMM
023300960216     C                   MOVEL     GG            GGMM
023400960216     C                   Z-ADD     AA            PRTDAA
023500960216     C                   MOVEL     GGMM          PRTDAA
023600960216     C                   ENDIF
023700960216     C*
023800960216     C* SE DATA AL = 0 METTO DATA DAL IN DATA AL
023900960216     C     PARDAA        IFEQ      0
024000960216     C                   Z-ADD     PARDAD        PARDAA
024100960216     C                   Z-ADD     PRTDAD        PRTDAA
024200960216     C                   ENDIF
024300960216     C*
024400960216     C* 07 ON  - IMMESSA DATA AL
024500960216     C     PARDAA        IFGT      0
024600960216     C     PARDAA        ANDNE     PARDAD
024700960216     C                   SETON                                        08
024800960216     C                   ENDIF
024900960209     C*
025000960216     C* CONTROLLO LA RICHIESTA DEL TIPO ELENCO
025100960216     C     PARTTB        IFEQ      'P'
025200960216     C     PARTTB        OREQ      '1'
025300960216     C     PARTTB        OREQ      ' '
025400960216     C                   MOVEL     'S'           WTAB1             1
025500960216     C                   ENDIF
025600960216     C     PARTTB        IFEQ      'P'
025700960216     C     PARTTB        OREQ      '2'
025800960216     C     PARTTB        OREQ      ' '
025900960216     C                   MOVEL     'S'           WTAB2             1
026000960216     C                   ENDIF
026100960216     C     PARTTB        IFEQ      'A'
026200960216     C     PARTTB        OREQ      '3'
026300960216     C     PARTTB        OREQ      ' '
026400960216     C                   MOVEL     'S'           WTAB3             1
026500960216     C                   ENDIF
026600960216     C     PARTTB        IFEQ      'A'
026700960216     C     PARTTB        OREQ      '4'
026800960216     C     PARTTB        OREQ      ' '
026900960216     C                   MOVEL     'S'           WTAB4             1
027000960216     C                   ENDIF
027100960209     C                   ENDSR
027200921012     C*
027300950720     C*--- INZSR INIZIALE --------------------------------------------*
027400940719     C     *INZSR        BEGSR
027500960219     C*
027600960219     C                   Z-ADD     1             CODUT
027700960219     C                   CALL      'X§PARUT'
027800960219     C                   PARM                    UT§DSE
027900960219     C                   MOVEL     RAGUT         RSUT             20
028000960219     C                   MOVEL     REC80         CNCR80
028100960209     C****
028200960209     C** KLIST
028300960209     C****
028400960208     C** ACCESSO TABEL00F
028500960209     C     KTAB2         KLIST
028600960209     C                   KFLD                    CODUT
028700960209     C                   KFLD                    COD
028800960216     C** ACCESSO FNFGV03L
028900960208     C     KFGV          KLIST
029000960208     C                   KFLD                    PARFIL
029100960208     C                   KFLD                    PARDFD
029200960216     C** ACCESSO FNFVA05L
029300960216     C     KFVA          KLIST
029400960216     C                   KFLD                    PARFIL
029500960216     C                   KFLD                    PARDAD
029600960208     C** ACCESSO FNFV203L
029700960208     C     KFV203        KLIST
029800960208     C                   KFLD                    FGVLNP
029900960208     C                   KFLD                    FGVNFV
030000960208     C                   KFLD                    WEPA
030100960208     C                   KFLD                    WTDH
030200960216     C     KFV233        KLIST
030300960216     C                   KFLD                    K2LNP
030400960216     C                   KFLD                    K2NFV
030500960216     C                   KFLD                    K2EPA
030600960216     C                   KFLD                    K2TDH
030700960208     C** ACCESSO FNFV201L
030800960209     C     KFV200        KLIST
030900960209     C                   KFLD                    FGVLNP
031000960209     C                   KFLD                    FGVNFV
031100960209     C                   KFLD                    WEPA
031200960209     C                   KFLD                    WLAI
031300960209     C                   KFLD                    WTDH
031400960216     C     KFV211        KLIST
031500960216     C                   KFLD                    K2LNP
031600960216     C                   KFLD                    K2NFV
031700960216     C                   KFLD                    K2EPA
031800960216     C                   KFLD                    K2LAI
031900960216     C                   KFLD                    K2TDH
032000960208     C** ACCESSO FNFV401L
032100960208     C     KFV4          KLIST
032200960216     C                   KFLD                    K4LNP
032300960216     C                   KFLD                    K4NFV
032400960216     C                   KFLD                    K4EPA
032500960216     C                   KFLD                    K4LAI
032600960216     C                   KFLD                    K4TRC
032700000427     C     KFWA          KLIST
032800000427     C                   KFLD                    FVALNP
032900000427     C                   KFLD                    FVANFV
033000000427     C                   KFLD                    FVALAI
033100960216     C* LETTURA PER IL FILE TNTLR02L
033200960216     C     KTLR          KLIST
033300960216     C                   KFLD                    FGVTRN
033400960216     C                   KFLD                    FGVFRQ
033500960216     C                   KFLD                    FGVLNP
033600960216     C* LETTURA PER IL FILE TNTLM01L
033700960216     C     KTLR1         KLIST
033800960216     C                   KFLD                    TLTTRN
033900960216     C                   KFLD                    TLTDDE
034000020813     C** ACCESSO FNFGW01L
034100020813     C     K02FGW01      klist
034200020813     C                   kfld                    FGVnfv
034300020813     C                   kfld                    FGVlnp
034400960209     C****
034500960209     C** DEFINIZIONE CAMPI
034600960209     C****
034700960216     C     *LIKE         DEFINE    TLTDDE        WDTN
034800960216     C     *LIKE         DEFINE    TLTDDE        SAVDTN
034900960216     C     *LIKE         DEFINE    TLTTRN        SAVTRN
035000960216     C     *LIKE         DEFINE    FGVDFV        SAVDFV
035100960216     C*
035200960216     C     *LIKE         DEFINE    TBLCOD        COD
035300960208     C     *LIKE         DEFINE    FV2TDH        WTDH
035400960208     C     *LIKE         DEFINE    FV2EPA        WEPA
035500960216     C     *LIKE         DEFINE    FV2LAI        WLAI
035600960216     C     *LIKE         DEFINE    FV2EPA        K4EPA
035700960216     C     *LIKE         DEFINE    FV4TRC        K4TRC
035800960216     C     *LIKE         DEFINE    FV2LAI        K4LAI
035900960209     C*
036000960216     C     *LIKE         DEFINE    PARDFD        PRTDFD
036100960216     C     *LIKE         DEFINE    PARDFA        PRTDFA
036200960216     C     *LIKE         DEFINE    PARDFD        PRTDAD
036300960216     C     *LIKE         DEFINE    PARDFA        PRTDAA
036400960216     C     *LIKE         DEFINE    PARDFD        PRTDAL
036500960216     C     *LIKE         DEFINE    PARDFA        PRTAL
036600960216     C*
036700960216     C     *LIKE         DEFINE    FV2LAI        PR2FIL
036800960216     C     *LIKE         DEFINE    PR1DTE        PR1DRE
036900960216     C     *LIKE         DEFINE    FV2HPA        PR1HRE
037000960216     C     *LIKE         DEFINE    PR1DTE        PR2DRE
037100960216     C     *LIKE         DEFINE    FV2HPA        PR2HRE
037200960216     C     *LIKE         DEFINE    FV2HPA        PR1HTE
037300960216     C     *LIKE         DEFINE    FV2RTC        PR2RTC
037400960216     C     *LIKE         DEFINE    FV2RTC        PR1RTC
037500960219     C     *LIKE         DEFINE    §TCDES        PD1RTC
037600960216     C     *LIKE         DEFINE    FGVTRN        PR1TRN
037700960216     C     *LIKE         DEFINE    FGVTTR        PR1TTR
037800960216     C     *LIKE         DEFINE    FGVPDR        PR1PDR
037900960216     C     *LIKE         DEFINE    FGVLNP        PR1SEG
038000960216     C     *LIKE         DEFINE    TLTTFP        PR1TFP
038100960216     C     *LIKE         DEFINE    TLTTFA        PR1TFA
038200960216     C*
038300960216     C     *LIKE         DEFINE    FGVLNP        K4LNP
038400960216     C     *LIKE         DEFINE    FGVNFV        K4NFV
038500960216     C*
038600960216     C     *LIKE         DEFINE    FGVLNP        K2LNP
038700960216     C     *LIKE         DEFINE    FGVNFV        K2NFV
038800960216     C     *LIKE         DEFINE    FV2TDH        K2TDH
038900960216     C     *LIKE         DEFINE    FV2LAI        K2LAI
039000960216     C     *LIKE         DEFINE    FV2EPA        K2EPA
039100960216     C*
039200960216     C     *LIKE         DEFINE    FGVLNP        PR1LNP
039300960216     C     *LIKE         DEFINE    FGVNFV        PR1NFV
039400960208     C*
039500960209     C****
039600960209     C** PRENDO DATA E ORA CORRENTE
039700960209     C****
039800950406     C                   TIME                    W0140            14 0
039900950406     C                   MOVE      W0140         UDATE8            8 0
040000960208     C                   MOVEL     W0140         UTIME             6 0
040100960209     C****
040200960209     C** CARICO TABELLA DELLE CAUSALI RITARDO
040300960209     C****
040400960209     C                   MOVEL     'TC'          COD
040500960209     C                   Z-ADD     1             X                 3 0
040600960209     C     KTAB2         SETLL     TABEL
040700960209     C     KTAB2         READE     TABEL                                  30
040800960209    1C     *IN30         DOWEQ     *OFF
040900960209    2C     TBLFLG        IFEQ      ' '
041000960209     C                   MOVEL     TBLKEY        TTC(X)
041100960216     C                   MOVEL     TBLUNI        DSTC
041200960216     C                   MOVEL     §TCDES        DTC(X)
041300960216     C                   MOVEL     §TCIMP        ITC(X)
041400960209     C                   ADD       1             X
041500960209    2C                   ENDIF
041600960209     C     KTAB2         READE     TABEL                                  30
041700960209    1C                   ENDDO
041800960209     C*
041900960209     C* RITARDO
042000960216     C                   MOVEL     'R'           K4TRC
042100960216     C                   ENDSR
042200960216     C**
042300960216     C* PREPARA CAMPI TESTATA ----------------------------------------
042400960216     C     PRETES        BEGSR
042500960216     C* TRAZIONI IN PARTENZA
042600960216     C     *IN01         IFEQ      *ON
042700960216     C     *IN02         OREQ      *ON
042800960220     C   01              Z-ADD     1             PRTNUM            1 0
042900960220     C   02              Z-ADD     2             PRTNUM            1 0
043000960216     C                   MOVEL     CDEST         PRTDFL           14
043100960216     C* STAMPO LE DATE FOGLIO VIAGGIO
043200960216     C                   Z-ADD     PRTDFD        PRTDAL
043300960216     C                   Z-ADD     PRTDFA        PRTAL
043400960216     C* RIGHE TESTATA
043500960216     C                   MOVEL     TS(14)        PRTRG1
043600960216     C                   MOVEL     TS(16)        PRTRG2
043700960216     C                   ENDIF
043800960216     C* TRAZIONI IN ARRIVO
043900960216     C     *IN03         IFEQ      *ON
044000960216     C     *IN04         OREQ      *ON
044100960220     C   03              Z-ADD     3             PRTNUM
044200960220     C   04              Z-ADD     4             PRTNUM
044300960216     C                   MOVEL     CCARIC        PRTDFL
044400960216     C* STAMPO LE DATE FOGLIO ARRIVI
044500960216     C                   Z-ADD     PRTDAD        PRTDAL
044600960216     C                   Z-ADD     PRTDAA        PRTAL
044700960216     C* RIGHE TESTATA
044800960216     C                   MOVEL     TS(15)        PRTRG1           66
044900960216     C                   MOVEL     TS(17)        PRTRG2           66
045000960216     C                   ENDIF
045100960216     C*
045200960216     C* STAMPA DEL RITARDO PARTENZA, CON AFFIANCO IL RITARDO ARRIVO
045300960216     C     *IN01         IFEQ      *ON
045400960216     C     *IN03         OREQ      *ON
045500960219     C                   MOVEL     CTESP         PRTPA            15
045600960216     C                   MOVEL     CPARTE        PRTTPA           30
045700960216     C                   MOVEL     CPRSAR        PRTPRS           28
045800960216     C                   MOVEL     CLNA          PRTLIN           18
045900960216     C                   ENDIF
046000960216     C* STAMPA DEL RITARDO ARRIVO  , CON AFFIANCO IL RITARDO PARTENZA
046100960216     C     *IN02         IFEQ      *ON
046200960216     C     *IN04         OREQ      *ON
046300960216     C                   MOVEL     CTESA         PRTPA
046400960216     C                   MOVEL     CARRIV        PRTTPA
046500960216     C                   MOVEL     CPRSPA        PRTPRS
046600960216     C                   MOVEL     CLNP          PRTLIN
046700960216     C                   ENDIF
046800960216     C                   ENDSR
046900960216     C*
047000960216     C*--- ELABORAZIONE ----------------------------------------------*
047100960216     C     ELAB          BEGSR
047200960216     C                   CLEAR                   SAVTRN
047300960216     C                   CLEAR                   SAVDTN
047400960216     C                   CLEAR                   SAVDFV
047500960216     C                   CLEAR                   PR1TFP
047600960216     C                   CLEAR                   PR1TFA
047700960216     C                   CLEAR                   SK
047800960220     C                   SETOFF                                       1016
047900960219     C                   SETON                                        OF
048000960216     C**
048100960216     C* PREPARO I CAMPI DELLA TESTATA
048200960216     C                   EXSR      PRETES
048300960216     C******
048400960216     C**  TRAZIONI IN PARTENZA DALLA PROPRIA FILIALE
048500960216     C******
048600960216    0C     *IN01         IFEQ      *ON
048700960216     C     *IN02         OREQ      *ON
048800960216     C                   MOVEL     'FGV'         WLETT             3
048900960216     C   01              MOVEL     'P'           K4EPA
049000960216     C   01              CLEAR                   K4LAI
049100960216     C   02              MOVEL     'A'           K4EPA
049200960216     C*
049300960216     C     KFGV          SETLL     FNFGV03L
049400960216     C  N07KFGV          READE     FNFGV03L                               30
049500960216     C   07PARFIL        READE     FNFGV03L                               30
049600960216     C*
049700960216    1C     *IN30         DOWEQ     *OFF
049800960216     C* CONTROLLO FINE LETTURA SE IMMESSA DATA AL
049900960216    2C   07FGVDFV        IFGT      PARDFA
050000960216     C                   SETON                                        30
050100960216    2C                   ENDIF
050200960216     C*
050300960216    2C     *IN30         IFEQ      *OFF
050400960216     C                   MOVEL     FGVNFV        K4NFV
050500960216     C                   MOVEL     FGVLNP        K4LNP
050600960219     C                   Z-ADD     FGVLNA        WLNA2
050700000427     C*
050800020813     ***C     FGVNFV        CHAIN     FNFGW01L                           39
050900020813     C     K02FGW01      chain     FNFGW01L                           39
051000000427     C     *IN39         IFEQ      *ON
051100000427     C     FGWATB        ORNE      ' '
051200000427     C                   CLEAR                   FGWFF3
051300000427     C                   CLEAR                   FGWFF4
051400000427     C                   CLEAR                   FGWFL3
051500000427     C                   CLEAR                   FGWFL4
051600000427     C                   ENDIF
051700960216     C******
051800960216     C**  RITARDI DI  P A R T E N Z A
051900960216     C******
052000960216    3C     *IN01         IFEQ      *ON
052100960216     C*
052200960216     C                   MOVEL     'P'           WEPA
052300960216     C                   CLEAR                   WLAI
052400960216     C                   MOVEL     'R'           WTDH
052500960216     C     KFV200        CHAIN     FNFV201L                           31
052600960216     C*
052700960216     C* ESCLUDO RECORD ANNULLATI
052800960216    4C  N31FV2ATB        IFEQ      ' '
052900960216     C* CONTROLLO CHE ESISTA LA CAUSALE RITARDO
053000960216     C     FV2RTC        ANDNE     ' '
053100960216     C                   EXSR      STAMPA
053200960216    4C                   ENDIF
053300960216     C*
053400960216    3C                   ENDIF
053500960216     C*
053600960216     C******
053700960216     C**  RITARDI DI  A R R I V O
053800960216     C******
053900960216    3C     *IN02         IFEQ      *ON
054000960216     C*
054100960216     C                   MOVEL     'A'           WEPA
054200960216     C                   MOVEL     'R'           WTDH
054300960216     C     KFV203        SETLL     FNFV203L
054400960216     C     KFV203        READE     FNFV203L                               31
054500960216     C*
054600960216    4C     *IN31         DOWEQ     *OFF
054700960216     C* ESCLUDO RECORD ANNULLATI
054800960216    5C     FV2ATB        IFEQ      ' '
054900960216     C* CONTROLLO CHE ESISTA LA CAUSALE RITARDO
055000960216     C     FV2RTC        ANDNE     ' '
055100960216     C                   MOVEL     FV2LAI        K4LAI
055200960216     C                   EXSR      STAMPA
055300960216    5C                   ENDIF
055400960216     C*
055500960220     C                   MOVEL     'A'           WEPA
055600960220     C                   MOVEL     'R'           WTDH
055700960216     C     KFV203        READE     FNFV203L                               31
055800960216    4C                   ENDDO
055900960216     C*
056000960216    3C                   ENDIF
056100960216     C*
056200960216     C  N07KFGV          READE     FNFGV03L                               30
056300960216     C   07PARFIL        READE     FNFGV03L                               30
056400960216    2C                   ENDIF
056500960216    1C                   ENDDO
056600960216     C*
056700960216    0C                   ENDIF
056800960216     C*
056900960216     C******
057000960216     C**  TRAZIONI IN ARRIVO ALLA PROPRIA FILIALE
057100960216     C******
057200960216    0C     *IN03         IFEQ      *ON
057300960216     C     *IN04         OREQ      *ON
057400960216     C                   MOVEL     'FVA'         WLETT             3
057500960216     C   03              MOVEL     'P'           K4EPA
057600960216     C   03              CLEAR                   K4LAI
057700960216     C   04              MOVEL     'A'           K4EPA
057800960216     C   04              MOVEL     PARFIL        K4LAI
057900960216     C*
058000960216     C     KFVA          SETLL     FNFVA05L
058100960219     C  N08KFVA          READE     FNFVA05L                               30
058200960219     C   08PARFIL        READE     FNFVA05L                               30
058300960216     C*
058400960219    1C     *IN30         DOWEQ     *OFF
058500960216     C* CONTROLLO FINE LETTURA SE IMMESSA DATA AL
058600960216    2C   08FVADFA        IFGT      PARDAA
058700960219     C                   SETON                                        30
058800960216    2C                   ENDIF
058900960220     C*
059000960219     C*
059100960219     C* ESCLUDO I FOGLI ILLUMINATI DEI 2 LIVELLO IN ARRIVO
059200960219     C                   SETOFF                                       33
059300000427     C*
059400000427     C     KFWA          CHAIN     FNFWA01L                           39
059500000427     C   39              CLEAR                   FWAFF3
059600000427     C   39              CLEAR                   FWAFF4
059700000427     C   39              CLEAR                   FWAFL3
059800000427     C   39              CLEAR                   FWAFL4
059900000427     C**
060000960220    2C  N30FVAATR        IFEQ      'T'
060100960219     C* SE LA FILIALE DI SCARICO E' IL TERMINAL DI ARRIVO DELLA LNA
060200960219     C*  E NON E' A LNA STESSA, E' FOGLIO PER I 2 LIVELLO IN ARR
060300960219     C                   Z-ADD     1             I
060400960219     C                   Z-ADD     FVALNA        WLNA3
060500960219     C                   MOVEL     FVALAI        W003A
060600960219     C     W003A         LOOKUP    FFA(I)                                 32
060700960219     C   32              MOVEL     FLA(I)        W0030             3 0
060800960219     C  N32              MOVEL     FVALAI        W0030
060900971230     C*
061000971230     C                   CLEAR                   DSLV55
061100971230     C                   MOVEL     'A'           D55TPT
061200971230     C                   MOVE      FVALAI        D55LIN
061300971230     C                   Z-ADD     FVADFV        D55DRF
061400971230     C                   CALL      'FNLV55R'
061500971230     C                   PARM                    DSLV55
061600960220     C*
061700960220    3C     FVALAI        IFNE      W0030
061800971230     C     D55TFA        ANDEQ     W0030
061900960219     C                   SETON                                        33
062000960219    3C                   ENDIF
062100960219     C*
062200960219     C* SE E' TRANSITO MEMORIZZO NELLE FILIALI DI CARICO
062300960219    3C     *IN33         IFEQ      *OFF
062400971230     C     FVALAI        CHAIN     AZORG01L                           32
062500960219     C                   CLEAR                   PR1DES
062600960219     C                   MOVEA     CTRANS        K14
062700960220     C                   MOVEA     ORGPRO        K14(1)
062800960220     C     W0030         CHAIN     AZORG01L                           32
062900960220     C                   MOVEA     ORGPRO        K14(13)
063000960219     C                   MOVEA     K14           PR1DES
063100960219    3C                   ENDIF
063200960219    2C                   ENDIF
063300960216     C*
063400960219    2C  N30FVAATR        IFEQ      ' '
063500960219     C     *IN33         OREQ      *OFF
063600960216     C                   MOVEL     FVANFV        K4NFV
063700960216     C                   MOVEL     FVALNP        K4LNP
063800960216     C******
063900960216     C**  RITARDI DI  P A R T E N Z A
064000960216     C******
064100960216    3C     *IN03         IFEQ      *ON
064200960216     C     FVARTP        ANDNE     ' '
064300960219     C                   EXSR      STAMPA
064400960216    3C                   ENDIF
064500960216     C******
064600960216     C**  RITARDI DI  A R R I V O
064700960216     C******
064800960216    3C     *IN04         IFEQ      *ON
064900960216     C     FVARTA        ANDNE     ' '
065000960219     C                   EXSR      STAMPA
065100960216    3C                   ENDIF
065200960216    2C                   ENDIF
065300960216     C*
065400960220     C  N30
065500960220     CANN08KFVA          READE     FNFVA05L                               30
065600960220     C  N30
065700960220     CAN 08PARFIL        READE     FNFVA05L                               30
065800960216    1C                   ENDDO
065900960216     C*
066000960216    0C                   ENDIF
066100960216     C*
066200960216     C* SE NON C'E' NIENTE DA STAMPARE STAMPO LA TESTATA
066300960220     C     *IN22         IFEQ      *OFF
066400960220     C                   EXCEPT    TESTA2
066500960220     C                   DO        10
066600960220     C                   EXCEPT    DETT3
066700960220     C                   ENDDO
066800960220     C                   ENDIF
066900000427     C* E NE HO STAMPATO ALMENO UNO
067000000427     C     *IN16         IFEQ      *ON
067100960220     C                   EXCEPT    FINEST
067200960220     C                   EXCEPT    PIEDI
067300000427     C                   ENDIF
067400960216     C                   ENDSR
067500960216     C*
067600960216     C*--- STAMPA RITARDO --------------------------------------------*
067700960216     C     STAMPA        BEGSR
067800960216     C* PULIZIA CAMPI NON SEMPRE PIENI
067900960219     C                   SETOFF                                       091020
068000960220     C                   SETOFF                                       111213
068100960220     C                   CLEAR                   PD1RTC
068200960220     C                   CLEAR                   PR1DTE
068300960216     C                   CLEAR                   PR1HTE
068400960220     C                   CLEAR                   PR1MN3
068500960220     C                   CLEAR                   PR1MN4
068600960219     C                   CLEAR                   WDET2
068700960219     C                   MOVEL     1             WNUSTA
068800960216     C*******
068900960216     C* CAMPI FOGLIO VIAGGIO   PARTENZA
069000960216     C*******
069100960216    1C     WLETT         IFEQ      'FGV'
069200960216     C**
069300960216     C                   MOVEL     FGVTRN        PR1TRN
069400960216     C*
069500960216     C* SALVO DATA FOGLIO E NUMERO TRAINO:SE CAMBIANO RICERCO IL TRAINO
069600960216    2C     FGVTRN        IFNE      SAVTRN
069700960216     C     FGVDFV        ORNE      SAVDFV
069800960216     C                   CLEAR                   PR1TFP
069900960216     C                   CLEAR                   PR1TFA
070000960216     C                   CLEAR                   WDTN
070100960216     C**
070200960216    3C     PR1TRN        IFGT      0
070300960216     C* CALCOLO DATA TRAINO PER LEGGERE L'ANAGRAFICA TRAINI
070400960216     C                   EXSR      CALDTN
070500960216     C* SE TROVATA DATA TRAINO CHIN SULLA TESTATA
070600960216    4C     WDTN          IFGT      0
070700960216     C                   EXSR      REDTLT
070800960216    4C                   ENDIF
070900960216     C*
071000960219     C* SE NON HO TROVATO LA DECORRENZA GIUSTA, COME SE NON CI FOSSE
071100960216    4C     PR1TFP        IFEQ      0
071200960216     C                   CLEAR                   WDTN
071300960216     C                   CLEAR                   PR1TRN
071400960216    4C                   ENDIF
071500960216    3C                   ENDIF
071600960216     C*
071700960216     C                   MOVEL     FGVDFV        SAVDFV
071800960216     C                   MOVEL     FGVTRN        SAVTRN
071900960220     C                   ELSE
072000960216    2C                   ENDIF
072100960216     C*
072200960216    2C     FGVSNP        IFEQ      'B'
072300960216     C                   SETON                                        09
072400960216    2C                   ENDIF
072500960216     C*
072600960216     C                   MOVEL     FGVTTR        PR1TTR
072700960216     C                   MOVEL     FGVLNP        PR1LNP
072800960216     C                   MOVEL     FGVNFV        PR1NFV
072900960216     C*
073000960216     C                   MOVEL     FGVDFV        WDATA
073100960216     C                   MOVEL     GG            PR1DFV
073200960216     C                   MOVE      MM            PR1DFV            4 0
073300960216     C                   MOVEL     FGVPDR        PR1PDR
073400960216     C                   MOVEL     FGVDPD        PR1DPD           11
073500960216     C                   MOVEL     FGVDCN        PR1DCN           10
073600960216     C*
073700960216     C   01              MOVEL     FGVLNP        PR1SEG
073800960216     C   02              MOVEL     FV2LAI        PR1SEG
073900960219     C**
074000960219     C   01              MOVEL     'P'           WPAAR             1
074100960219     C   02              MOVEL     'A'           WPAAR             1
074200960219     C                   EXSR      CALFGV
074300960216     C*
074400960216     C* CARICO LE DESTINAZIONI
074500960219     C* SE E' DATA DI LINEA TRANSITATA SCRIVO SOLO CHE E' TRANSITO
074600960219     C   02              MOVEL     FV2LAI        W003A
074700960219     C   02W003A         LOOKUP    FLP                                    32
074800960219     C     *IN02         IFEQ      *ON
074900960219    2C     *IN32         ANDEQ     *ON
075000960219     C     *IN01         OREQ      *ON
075100960219     C                   EXSR      CARDES
075200960219   X2C                   ELSE
075300960219     C*
075400960219     C                   Z-ADD     1             I
075500960219     C                   MOVEL     FV2LAI        W003A
075600960219     C     W003A         LOOKUP    FFV(I)                                 32
075700960219     C                   MOVEA     CTRANS        K14
075800960220     C     FV2LAI        CHAIN     AZORG01L                           31
075900960220     C                   MOVEA     ORGPRO        K14(1)
076000960220     C     *IN32         IFEQ      *ON
076100960220     C                   MOVEL     FLP(I)        W0030
076200960220     C     W0030         CHAIN     AZORG01L                           31
076300960220     C                   ENDIF
076400960220     C                   MOVEA     ORGPRO        K14(13)
076500960219     C                   MOVEA     K14           PR1DES
076600960219     C                   SETOFF                                       10
076700960219     C*
076800960219    2C                   ENDIF
076900960219     C*
077000960216    1C                   ENDIF
077100960216     C*******
077200960216     C* CAMPI FOGLIO VIAGGIO   PARTENZA  IN ARRIVO
077300960216     C*******
077400960216    1C     WLETT         IFEQ      'FVA'
077500960216     C**
077600960216     C                   MOVEL     FVATTR        PR1TTR
077700960216     C                   MOVEL     FVALNP        PR1LNP
077800960216     C                   MOVEL     FVANFV        PR1NFV
077900960216     C                   MOVEL     FVATRN        PR1TRN
078000960216     C*
078100960216     C* SE CAMBIATO NUMERO O DATA TRAINO
078200960216    2C     FVATRN        IFNE      SAVTRN
078300960216     C     FVADTN        ORNE      SAVDTN
078400960216     C                   CLEAR                   PR1TFP
078500960216     C                   CLEAR                   PR1TFA
078600960216     C*
078700960216     C* CHAIN SULLA TESTATA TRAINI
078800960219    3C     FVATRN        IFGT      0
078900960216     C                   Z-ADD     FVADTN        WDTN
079000960216     C                   EXSR      REDTLT
079100960216     C*
079200960219    4C     PR1TFP        IFEQ      0
079300960216     C                   CLEAR                   PR1TRN
079400960219    4C                   ENDIF
079500960219    3C                   ENDIF
079600960216     C*
079700960216     C                   MOVEL     PR1TRN        SAVTRN
079800960216     C                   MOVEL     FVADTN        SAVDTN
079900960216    2C                   ENDIF
080000960216     C* SE NON E' ABBINATO A TRAINO, METTO COME FIL CARICO SOLO LNP
080100960219    2C     PR1TRN        IFEQ      0
080200960219     C     FVAATR        ANDEQ     ' '
080300960216     C                   CLEAR                   PR1DES
080400960216     C     FVALNP        CHAIN     AZORG01L                           31
080500960220     C                   MOVEL     ORGPRO        W003A
080600960220     C                   MOVE      '/'           W003A
080700960220     C                   MOVEL     W003A         PR1DES
080800960219    2C                   ENDIF
080900960216     C*
081000960216     C                   MOVEL     FVADFV        WDATA
081100960216     C                   MOVEL     GG            PR1DFV
081200960216     C                   MOVE      MM            PR1DFV
081300960216     C                   MOVEL     FVAPDR        PR1PDR
081400960216     C                   MOVEL     FVADPD        PR1DPD           11
081500960216     C                   MOVEL     FVADCN        PR1DCN           10
081600960216     C*
081700960216     C   03              MOVEL     FVALNP        PR1SEG
081800960216     C   04              MOVEL     FVALAI        PR1SEG
081900960219     C*
082000960219     C   03              MOVEL     'P'           WPAAR
082100960220     C   04              MOVEL     'A'           WPAAR
082200960219     C                   EXSR      CALFVA
082300960216    1C                   ENDIF
082400960216     ******
082500960216     C** RITARDO
082600960216     ******
082700960216     C                   Z-ADD     1             X
082800960216     C     PR1RTC        LOOKUP    TTC(X)                                 35
082900960219     C   35              MOVEL     ITC(X)        PR1IMP            1
083000960219     C  N35              MOVEL     '?'           PR1IMP
083100960216     C**
083200960220     C     PR1RTC        IFNE      'G'
083300960219     C   35              MOVEL     DTC(X)        PD1RTC
083400960219     C  N35              MOVEL     *ALL'?'       PD1RTC
083500960216     C                   ENDIF
083600960220     C*****
083700960220     C** VERIFICO LE DESTINAZIONI
083800960220     C*****
083900960220     C                   Z-ADD     6             DS
084000960220     C     SK(DS)        IFNE      ' '
084100960220     C                   SETON                                        10
084200960220     C                   ENDIF
084300960216     ******
084400960216     C* STAMPO TESTATA
084500960216     ******
084600960216     C     *INOF         IFEQ      *ON
084700960220     C   16              EXCEPT    PIEDI
084800960220     C                   EXCEPT    TESTA2
084900960216     C                   SETOFF                                       OF
085000960220     C                   SETON                                        16
085100960216     C                   ENDIF
085200960216     ******
085300960216     C* STAMPO DETTAGLIO
085400960216     ******
085500960216     C                   EXCEPT    DETT
085600960216     C**
085700960216     C* STAMPO LA SECONDA PARTE DEL DETTAGLIO
085800960216     C                   MOVEL     PR1LNP        K2LNP
085900960216     C                   MOVEL     PR1NFV        K2NFV
086000960219     C                   SETOFF                                       98
086100960216     C* POSIZIONAMENTO SU FNFV4 SE C'E'
086200960220     C                   SETON                                        21
086300960216     C                   CLEAR                   FV4NOT
086400960220     C     KFV4          SETLL     FNFV401L                               20
086500960220     C*
086600960220     C* SE LA CAUSALE RITARDO NON E' "G" LEGGO FV4 DALLA  2 VOLTA
086700960220     C     PR1RTC        IFEQ      'G'
086800960220     C                   SETON                                        13
086900960220     C                   ENDIF
087000960216     C**
087100960216     C* LETTURA DEL FILE FNFV201L
087200960216    1C     *IN01         IFEQ      *ON
087300960216     C                   MOVEL     'R'           K2TDH
087400960216     C                   MOVEL     'A'           K2EPA
087500960216     C*
087600960216     C     KFV233        SETLL     FNFV203L
087700960219     C     KFV233        READE     FNFV203L                               98
087800960219    2C     *IN98         DOWEQ     *OFF
087900960216     C* SE NON E' ANNULLATO ED HA RITARDO
088000960216    3C     FV2ATB        IFEQ      ' '
088100960216     C     FV2RTC        ANDNE     ' '
088200960216     C* E NON E' UNA LINEA IN TRANSITO
088300960216     C                   MOVEL     FV2LAI        W003A             3
088400960216     C     W003A         LOOKUP    FLP                                    32
088500960216    4C     *IN32         IFEQ      *ON
088600960219     C* STAMPA 2 PARTE
088700960216     C                   EXSR      STAMP2
088800960216    4C                   ENDIF
088900960216    3C                   ENDIF
089000960216     C*
089100960219     C     KFV233        READE     FNFV203L                               98
089200960216    2C                   ENDDO
089300960216     C**
089400960216    1C                   ENDIF
089500960216     C*
089600960216     C* LETTURA DEL FILE FNFV201L
089700960216    1C     *IN02         IFEQ      *ON
089800960216     C                   MOVEL     'R'           K2TDH
089900960219     C                   MOVEL     'P'           K2EPA
090000960219     C                   CLEAR                   K2LAI
090100960216     C*
090200960219     C     KFV211        CHAIN     FNFV201L                           98
090300960219    2C     *IN98         IFEQ      *OFF
090400960216     C     FV2ATB        ANDEQ     ' '
090500960216     C     FV2RTC        ANDNE     ' '
090600960216     C                   EXSR      STAMP2
090700960216    2C                   ENDIF
090800960216    1C                   ENDIF
090900960216     C*
091000960216    1C     *IN03         IFEQ      *ON
091100960219     C* VEDO SE C'E' RITARDO DI ARRIVO
091200960219     C     FVARTA        ANDNE     ' '
091300960219     C                   EXSR      STAMP2
091400960219    1C                   ENDIF
091500960219     C**
091600960219    1C     *IN04         IFEQ      *ON
091700960219     C* VEDO SE C'E' RITARDO DI PARTENZA
091800960219     C     FVARTP        ANDNE     ' '
091900960219     C                   EXSR      STAMP2
092000960219    1C                   ENDIF
092100960219     C**
092200960219     C* FINISCO DI STAMPARE FV4 SE CE N'E'
092300960219     C                   SETON                                        98
092400960219     C                   SETOFF                                       21
092500960219    2C     *IN20         DOWEQ     *ON
092600960219     C                   EXSR      REDFV4
092700960220     C   10
092800960220     CAN 11              EXSR      REDDES
092900960220     C   10
093000960220     CAN 11
093100960220     COR 20              EXSR      STAMP2
093200960219    2C                   ENDDO
093300960220     C*
093400960219     C* FINISCO DI STAMPARE LE DESTINAZIONI
093500960219    2C     *IN10         DOWEQ     *ON
093600960220     C* SE NON HO ANCORA STAMPATO UNA RIGA FACCIO EXCPTDETT3
093700960220     C  N11              EXCEPT    DETT3
093800960220     C                   SETON                                        11
093900960219     C                   EXSR      REDDES
094000960219     C   10              EXSR      STAMP2
094100960219    2C                   ENDDO
094200960216     C*
094300960216     C* 22 ON - HO STAMPATO QUALCOSA
094400960216     C                   SETON                                        22
094500960219     C*
094600960219     C     WDET2         IFEQ      ' '
094700960219     C                   EXCEPT    DETT3
094800960219     C                   ENDIF
094900960216     C                   ENDSR
095000960219     C*
095100960219     C* CALCOLA LE DATE ED I MINUTI DI RITARDO------------------------*
095200960219     C     CALFGV        BEGSR
095300960220     C                   SETOFF                                       15
095400960219     C* DATA / ORA REALE
095500960219     C                   Z-ADD     FV2DPA        WDATA
095600960219     C                   Z-ADD     MM            GGMM
095700960219     C                   MOVEL     GG            GGMM
095800960219    1C     WNUSTA        IFEQ      1
095900960219     C                   Z-ADD     AA            PR1DRE
096000960219     C                   MOVEL     GGMM          PR1DRE
096100960219     C*
096200960219     C                   MOVEL     FV2HPA        PR1HRE
096300960219     C                   MOVEL     FV2RTC        PR1RTC
096400960219   X1C                   ELSE
096500960219     C                   Z-ADD     AA            PR2DRE
096600960219     C                   MOVEL     GGMM          PR2DRE
096700960219     C*
096800960219     C                   MOVEL     FV2HPA        PR2HRE
096900960219     C                   MOVEL     FV2RTC        PR2RTC
097000960219    1C                   ENDIF
097100960219     C*
097200960219     C* IMCOMINCIO AD IMPOSTARE I CAMPI DELLA DS PER PRENDERE I MINUTI
097300960219     C*  DI RITARDO
097400960219     C                   CLEAR                   DSTL35
097500960219     C                   MOVEL     'C'           D35TUP
097600960219     C                   MOVEL     WPAAR         D35EPA
097700960219     C                   MOVEL     FV2DPA        D35DRE
097800960219     C                   MOVEL     FV2HPA        D35HRE
097900960219     C*
098000960219     C* AGGANCIO FNFV201L PER REPERIRE DATA E ORA TEORICA
098100960219     C                   MOVEL     WPAAR         WEPA
098200960219     C                   MOVEL     'T'           WTDH
098300960219    1C     WPAAR         IFEQ      'P'
098400960219     C                   CLEAR                   WLAI
098500960219     C                   ELSE
098600960219     C                   MOVEL     FV2LAI        WLAI
098700960219    1C                   ENDIF
098800960219     C*
098900960219     C     KFV200        CHAIN     FNFV201L                           32
099000960219    1C     *IN32         IFEQ      *OFF
099100960219     C     FV2ATB        ANDEQ     ' '
099200960219     C     FV2DPA        ANDGT     0
099300960219     C* GIRO DATA PARTENZA TEORICA
099400960219    2C     WNUSTA        IFEQ      1
099500960219     C                   Z-ADD     FV2DPA        WDATA
099600960219     C                   Z-ADD     MM            GGMM
099700960219     C                   MOVEL     GG            GGMM
099800960219     C                   Z-ADD     AA            PR1DTE            6 0
099900960219     C                   MOVEL     GGMM          PR1DTE
100000960219     C                   MOVEL     FV2HPA        PR1HTE
100100960219    2C                   ENDIF
100200960219     C*
100300960219     C* SE TROVATA LA TEORICA CALL AL PGM PER IL CALCOLO DEI MINUTI
100400960219     C*  DI RITARDO
100500960219     C                   MOVEL     FV2DPA        D35DTE
100600960219     C                   MOVEL     FV2HPA        D35HTE
100700960220     C                   EXSR      CATL35
100800960219    1C                   ENDIF
100900960219     C                   ENDSR
101000960220     C**
101100960220     C* CALL AL PGM TLTL35R-------------------------------------------*
101200960220     C     CATL35        BEGSR
101300960220     C*
101400960220     C                   MOVEL     DSTL35        KPJBU
101500960220     C                   CALL      'TNTL35R'
101600960220     C                   PARM                    KPJBA
101700960220     C                   MOVEL     KPJBU         DSTL35
101800960220     C                   SETON                                        15
101900960220     C*
102000960220    1C     D35MIN        IFGT      9999
102100960220     C                   Z-ADD     9999          D35MIN
102200960220    1C                   ENDIF
102300960220     C*
102400960220    1C     WNUSTA        IFEQ      1
102500960220    2C     D35MIN        IFLE      999
102600960220     C                   Z-ADD     D35MIN        PR1MN3            3 0
102700960220     C                   ELSE
102800960220     C                   Z-ADD     D35MIN        PR1MN4            4 0
102900960220     C                   SETON                                        12
103000960220    2C                   ENDIF
103100960220     C*
103200960220   X1C                   ELSE
103300960220     C                   Z-ADD     D35MIN        PR2MN4            4 0
103400960220    1C                   ENDIF
103500960220     C                   ENDSR
103600960219     C**
103700960219     C* CALCOLO LE DATE PER FVA E MINUTI RITARDO---------------------*
103800960219     C     CALFVA        BEGSR
103900960219     C* IMCOMINCIO AD IMPOSTARE I CAMPI DELLA DS PER PRENDERE I MINUTI
104000960219     C*  DI RITARDO
104100960220     C                   SETOFF                                       15
104200960219     C                   CLEAR                   DSTL35
104300960219     C                   MOVEL     'C'           D35TUP
104400960219     C* DATA / ORA REALE
104500960219    2C     WPAAR         IFEQ      'P'
104600960219     C                   Z-ADD     FVADRP        WDATA
104700960219     C     WNUSTA        IFEQ      1
104800960219     C                   MOVEL     FVAHRP        PR1HRE
104900960219     C                   MOVEL     FVAHTP        PR1HTE
105000960219     C                   MOVEL     FVARTP        PR1RTC
105100960219     C                   ELSE
105200960219     C                   MOVEL     FVAHRP        PR2HRE
105300960219     C                   MOVEL     FVARTP        PR2RTC
105400960219     C                   ENDIF
105500960219     C                   MOVEL     'P'           D35EPA
105600960219     C                   MOVEL     FVADRP        D35DRE
105700960219     C                   MOVEL     FVAHRP        D35HRE
105800960219     C                   MOVEL     FVADTP        D35DTE
105900960219     C                   MOVEL     FVAHTP        D35HTE
106000960219    2C                   ENDIF
106100960219     C*
106200960219    2C     WPAAR         IFEQ      'A'
106300960219     C                   Z-ADD     FVAAED        WDATA
106400960219     C     WNUSTA        IFEQ      1
106500960219     C                   MOVEL     FVAAEH        PR1HRE
106600960219     C                   MOVEL     FVAHTA        PR1HTE
106700960219     C                   MOVEL     FVARTA        PR1RTC
106800960219     C                   ELSE
106900960219     C                   MOVEL     FVAAEH        PR2HRE
107000960219     C                   MOVEL     FVARTA        PR2RTC
107100960219     C                   ENDIF
107200960219     C                   MOVEL     'A'           D35EPA
107300960219     C                   MOVEL     FVAAED        D35DRE
107400960219     C                   MOVEL     FVAAEH        D35HRE
107500960219     C                   MOVEL     FVADTA        D35DTE
107600960219     C                   MOVEL     FVAHTA        D35HTE
107700960219    2C                   ENDIF
107800960219     C*
107900960219     C                   Z-ADD     MM            GGMM
108000960219     C                   MOVEL     GG            GGMM
108100960219     C     WNUSTA        IFEQ      1
108200960219     C                   Z-ADD     AA            PR1DRE
108300960219     C                   MOVEL     GGMM          PR1DRE
108400960219     C                   ELSE
108500960219     C                   Z-ADD     AA            PR2DRE
108600960219     C                   MOVEL     GGMM          PR2DRE
108700960219     C                   ENDIF
108800960219     C*
108900960219     C* SE LA DATA TEORICA C'E' LA GIRO E CHIAMO PGM TNTL35R
109000960219     C     WPAAR         IFEQ      'P'
109100960219     C                   Z-ADD     FVADTP        WDATA
109200960219     C                   ELSE
109300960219     C                   Z-ADD     FVADTA        WDATA
109400960219     C                   ENDIF
109500960220    1C     WDATA         IFGT      0
109600960219     C*
109700960220    2C     WNUSTA        IFEQ      1
109800960219     C                   Z-ADD     MM            GGMM
109900960219     C                   MOVEL     GG            GGMM
110000960219     C                   Z-ADD     AA            PR1DTE
110100960219     C                   MOVEL     GGMM          PR1DTE
110200960220    2C                   ENDIF
110300960219     C**
110400960220     C                   EXSR      CATL35
110500960220    1C                   ENDIF
110600960219     C                   ENDSR
110700960216     C**
110800960216     C* STAMPA 2 PARTE -----------------------------------------------*
110900960216     C     STAMP2        BEGSR
111000960219     C     *IN98         IFEQ      *OFF
111100960219     C                   MOVEL     2             WNUSTA            1 0
111200960219     C*
111300960219     C     *IN01         IFEQ      *ON
111400960219     C     *IN02         OREQ      *ON
111500960219     C   01              MOVEL     'A'           WPAAR
111600960219     C   02              MOVEL     'P'           WPAAR
111700960219     C                   EXSR      CALFGV
111800960219     C*
111900960219     C   01              MOVEL     FV2LAI        PR2FIL
112000960219     C   02              MOVEL     FGVLNP        PR2FIL
112100960219    1C                   ENDIF
112200960219     C**
112300960219    2C     *IN03         IFEQ      *ON
112400960219     C     *IN04         OREQ      *ON
112500960219     C   03              MOVEL     'A'           WPAAR
112600960219     C   04              MOVEL     'P'           WPAAR
112700960219     C                   EXSR      CALFVA
112800960219     C*
112900960219     C   03              MOVEL     FVALAI        PR2FIL
113000960219     C   04              MOVEL     FVALNP        PR2FIL
113100960219    2C                   ENDIF
113200960219     C*
113300960219     C* VERIFICO SE C'E' FV4 DA STAMPARE
113400960219     C   20              EXSR      REDFV4
113500960219     C*
113600960219     C* VERIFICO SE CI SONO ANCORA DELLE DESTINAZIONI DA STAMPARE
113700960220     C   10
113800960220     CAN 11              EXSR      REDDES
113900960219     C                   ENDIF
114000960219     C*
114100960219     C     *INOF         IFEQ      *ON
114200960220     C   16              EXCEPT    PIEDI
114300960220     C                   EXCEPT    TESTA2
114400960219     C                   EXCEPT    DETT
114500960219     C                   SETOFF                                       OF
114600960219     C                   ENDIF
114700960219     C** 2 RIGA DI DETTAGLIO
114800960219     C                   EXCEPT    DETT2
114900960220     C                   SETON                                        1113
115000960219     C                   MOVEL     '1'           WDET2             1
115100960216     C                   ENDSR
115200960219     C**
115300960219     C* LETTURA DELLE DESTINAZIONI ANCORA DA STAMPARE-----------------*
115400960219     C     REDDES        BEGSR
115500960220     C     SK(DS)        IFNE      '  '
115600960219     C                   CLEAR                   PR2DES
115700960220     C                   MOVEA     SK(DS)        PR2DES           14
115800960220     C                   ADD       5             DS
115900960219     C                   ELSE
116000960219     C                   SETOFF                                       10
116100960219     C                   ENDIF
116200960219     C*
116300960219     C                   ENDSR
116400960216     C**
116500960216     C* CALCOLO LA DATA TRAINO PER LEGGERE L'ANAGRAFICA TRAINI--------*
116600960216     C     CALDTN        BEGSR
116700960216     C                   SETOFF                                       99
116800960216     C     KTLR          SETLL     TNTLR02L
116900960216     C     KTLR          READE     TNTLR02L                               32
117000960216    1C     *IN32         DOWEQ     *OFF
117100960216    2C     TLRATB        IFEQ      ' '
117200960216     C     TLRCAR        ANDEQ     'S'
117300960216     C*  ESCO DALLA LETTURA
117400960216     C                   SETON                                        3299
117500960216   X2C                   ELSE
117600960216     C  N32KTLR          READE     TNTLR02L                               32
117700960216    2C                   ENDIF
117800960216    1C                   ENDDO
117900960216     C*
118000960216    1C     *IN99         IFEQ      *ON
118100960216    2C     TLRGFV        IFEQ      'A'
118200960216     C                   Z-ADD     FGVDFV        WDTN
118300960216   X2C                   ELSE
118400960216     C* GIORNO<>A ->SOTTRAGGO ALLA DATA FOGLIO I GIORNI DI APERTURA FV
118500960216     C                   Z-ADD     1             C                 3 0
118600960216     C     TLRGFV        LOOKUP    GFV(C)                                 31
118700960216     C                   MOVE      FGVDFV        G02INV
118800960216     C                   MOVE      *ZEROS        G02DAT
118900960216     C                   MOVEL     '3'           G02ERR
119000960216     C                   CALL      'XSRDA8'
119100960216     C                   PARM                    WLBDAT
119200960216     C     G02TGI        SUB       C             GI8TGI
119300960216     C                   CALL      'XSRGI8'
119400960216     C                   PARM                    WGIDAT
119500960216     C                   Z-ADD     GI8INV        WDTN
119600960216    2C                   ENDIF
119700960216    1C                   ENDIF
119800960216     C                   ENDSR
119900960216     C**
120000960216     C* LETTURA DELLA TESTATA TRAINI ---------------------------------*
120100960216     C     REDTLT        BEGSR
120200960216     C     PR1TRN        SETLL     TNTLT01L
120300960216     C     PR1TRN        READE     TNTLT01L                               32
120400960216     C*
120500960216    1C     *IN32         DOWEQ     *OFF
120600960216    2C     TLTATB        IFEQ      ' '
120700960216     C     WDTN          ANDGE     TLTDDE
120800960216     C     WDTN          ANDLE     TLTDSC
120900960216     C                   Z-ADD     TLTTFP        PR1TFP
121000960216     C                   Z-ADD     TLTTFA        PR1TFA
121100960216     C                   SETON                                        32
121200960216     C* CERCO NEI METRI CUBI LE FILIALI DI CARICO PER TABUL 3 O 4
121300960216    3C     *IN03         IFEQ      *ON
121400960216     C     *IN04         OREQ      *ON
121500960219     C* SOLO SE FVA NON E' UN FOGLIO ILLUMINATO
121600960219     C     FVAATR        IFEQ      ' '
121700960216     C                   EXSR      REDTLR
121800960216    3C                   ENDIF
121900960219    3C                   ENDIF
122000960216     C*
122100960216   X2C                   ELSE
122200960216     C     PR1TRN        READE     TNTLT01L                               32
122300960216    2C                   ENDIF
122400960216    1C                   ENDDO
122500960216     C*
122600960216     C                   ENDSR
122700960216     C**
122800960216     C* LETTURA FILE METRI CUBI -------------------------------------*
122900960216     C     REDTLR        BEGSR
123000960216     C                   SETOFF                                       10
123100960216     C                   CLEAR                   SK
123200960220     C                   Z-ADD     0             DS                3 0
123300960216     C     KTLR1         SETLL     TNTLR01L
123400960216     C     KTLR1         READE     TNTLR01L                               32
123500960216     C*
123600960216     C     *IN32         DOWEQ     *OFF
123700960216     C*
123800960216     C     TLRATB        IFEQ      ' '
123900960216     C     TLRCAR        ANDEQ     'S'
124000960216     C     TLRTFM        ANDEQ     FVATFM
124100960216     C*
124200960216     C     TLRFIL        CHAIN     AZORG01L                           31
124300960220     C                   ADD       1             DS
124400960220     C                   MOVEL     ORGPRO        SK(DS)
124500960220     C                   MOVE      '/'           SK(DS)
124600960216     C                   ENDIF
124700960216     C*
124800960216     C     KTLR1         READE     TNTLR01L                               32
124900960216     C                   ENDDO
125000960216     C*
125100960216     C                   MOVEA     SK            PR1DES           14
125200960216     C*
125300960216     C                   ENDSR
125400960216     C**
125500960216     C* LETTURA FILE FNFV401L ---------------------------------------*
125600960216     C     REDFV4        BEGSR
125700960220     C     *IN13         IFEQ      *ON
125800960216     C     KFV4          READE     FNFV401L                               33
125900960216     C     *IN33         DOWEQ     *OFF
126000960220     C     FV4ATB        ANDNE     ' '
126100960216     C     KFV4          READE     FNFV401L                               33
126200960216     C                   ENDDO
126300960216     C*
126400960219     C  N33              SETON                                        20
126500960219     C   33              SETOFF                                       20
126600960220     C                   ENDIF
126700960216     C                   ENDSR
126800960219     C**
126900960219     C* LETTURA PER CARICO DESTINAZIONI FV --------------------------*
127000960219     C     CARDES        BEGSR
127100960219     C                   Z-ADD     1             I                 3 0
127200960220     C                   Z-ADD     0             DS                3 0
127300960305     C                   CLEAR                   SK
127400960305     C                   CLEAR                   PLP
127500960219     C     FLP(I)        DOWNE     *BLANKS
127600960219     C     FLP(I)        LOOKUP    PLP                                    32
127700960219     C     *IN32         IFEQ      *OFF
127800960219     C                   MOVEL     FLP(I)        W0030
127900960219     C     W0030         CHAIN     AZORG01L                           32
128000960220     C                   ADD       1             DS
128100960220     C                   MOVEL     ORGPRO        SK(DS)
128200960220     C                   MOVE      '/'           SK(DS)
128300960220     C                   MOVEL     FLP(I)        PLP(DS)
128400960219     C                   ENDIF
128500960219     C*
128600960219     C                   ADD       1             I
128700960219     C                   ENDDO
128800960219     C*
128900960219     C                   MOVEA     SK            PR1DES
129000960219     C* SE HO LETTO PIU' DI 5 ELEMENTI ACCENDO L'INDICATORE 10
129100960220     C     DS            IFGT      5
129200960220     C                   Z-ADD     6             DS
129300960219     C                   SETON                                        10
129400960219     C                   ENDIF
129500960219     C*
129600960219     C                   ENDSR
129700940719     C*
129800900406     O*--------------------------------------------------------------*
129900900406     O*     TESTATA                                                  *
130000900406     O*--------------------------------------------------------------*
130100960216     OPRTF198   E            TESTA2           02
130200960216     O                       TS(1)               66
130300960216     O                       TS(2)              132
130400960219     O                       TS(3)              198
130500960208     O                       RSUT                20
130600960208     O                       SIMFEL              24
130700960219     O                       PRTPA              133
130800960212     O                       UDATE8             180 '  /  /    '
130900960212     O                       PAGE          Z    198
131000960216     O          E            TESTA2           03
131100960212     O                       KNSIF               10
131200960212     O                       KNMUS               24
131300960212     O                       UTIME              178 '  :  :  '
131400960216     O          E            TESTA2           05
131500960216     O                       PRTRG1              66
131600960216     O                       PRTRG2             132
131700960220     O                       PRTNUM               3
131800960926     O                       PARFIL              42
131900960926     O                       DESFIL              65
132000960220     O                       PRTDAL             106 '  /  /    '
132100960220     O                       PRTAL              124 '  /  /    '
132200960220     O          E            TESTA2      0  0
132300960220     O                       PRTNUM               3
132400960220     O                       PRTRG1              66
132500960926     O                       PARFIL              42
132600960926     O                       DESFIL              65
132700960212     O*
132800960208     O          E            TESTA2           07
132900960212     O                       TS(5)               66
133000960212     O                       TS(8)              132
133100960212     O                       TS(11)             198
133200960216     O                       PRTDFL              53
133300960216     O                       PRTTPA             122
133400960216     O                       PRTPRS             198
133500960219     O          E            TESTA2           08
133600960212     O                       TS(6)               66
133700960212     O                       TS(9)              132
133800960212     O                       TS(12)             198
133900960220     O          E            TESTA2           09
134000960220     O                       TS(27)              66
134100960219     O                       TS(21)             132
134200960219     O                       TS(22)             198
134300960220     O                       PRTLIN             188
134400960220     O          E            TESTA2           10
134500960220     O                       TS(23)              66
134600960220     O                       TS(23)             132
134700960220     O                       TS(23)             198
134800960208     O*
134900900406     O*--------------------------------------------------------------*
135000900406     O*     DETTAGLIO                                                *
135100900406     O*--------------------------------------------------------------*
135200960219     O          E            DETT        1
135300960219     O                       TS(18)              66
135400960219     O                       TS(19)             132
135500960219     O                       TS(20)             198
135600960212     O* NUMERO TRAINO
135700960216     O                       PR1TRN        Z      7
135800960212     O* PARTENZA/ARRIVO  TRAINO
135900960216     O                       PR1TFP        Z     11
136000960212     O                                           12 '/'
136100960216     O                       PR1TFA        Z     15
136200960212     O* TIPO TRAINO
136300960220     O                     09                    19 'BIS'
136400960220     O                       PR1TTR              21
136500960212     O* LNP / NUMERO / DATA  FOGLIO VIAGGIO
136600960220     O                       PR1LNP        Z     25
136700960216     O                       PR1NFV        Z     31
136800960216     O                       PR1DFV              37 '  /  '
136900960212     O* DESTINAZIONI
137000960219     O                       PR1DES              53
137100960212     O* TRAZIONISTA
137200960216     O                       PR1PDR        Z     62
137300960216     O                                           63 '-'
137400960216     O                       PR1DPD              74
137500960212     O* CONDUCENTE
137600960220     O                       PR1DCN              86
137700960212     O* FILIALE CHE SEGNALA
137800960216     O                       PR1SEG        Z     90
137900960212     O* PARTENZA/ARRIVO TEORICO  (01-02 DA FV2    03-04 DA FVA)
138000960216     O                       PR1DTE             100 '  /  /  '
138100960216     O                       PR1HTE             106 '  :  '
138200960212     O* PARTENZA/ARRIVO REALE    (01-02 DA FV2    03-04 DA FVA)
138300960216     O                       PR1DRE             115 '  /  /  '
138400960216     O                       PR1HRE             121 '  :  '
138500960212     O* MINUTI RITARDO
138600960220     O              N12 15   PR1MN3        3    126
138700960220     O               12 15   PR1MN4        3    126
138800960212     O* CAUSALE RITARDO          (01-02 DA FV2    03-04 DA FVA)
138900960216     O                       PR1RTC             129
139000960212     O                                          130 '-'
139100960216     O                       PD1RTC             165
139200960212     O* IMPUTAZIONE
139300960216     O                       PR1IMP             168
139400960208     O*--------------------------------------------------------------*
139500960208     O*     DETTAGLIO   DI   FNFV4                                   *
139600960208     O*--------------------------------------------------------------*
139700960216     O          E            DETT2       0  1
139800960220     O* STAMPATO DALLA 2 VOLTA IN POI
139900960220     O                  11   TS(18)              66
140000960220     O                  11   TS(19)             132
140100960220     O                  11   TS(20)             198
140200960219     O* DESTINAZIONI
140300960220     O               10 11   PR2DES              53
140400960220     O               20 13   FV4NOT             165
140500960216     O* LINEA ARRIVO
140600960220     O                  21   PR2FIL        Z    173
140700960216     O* PARTENZA/ARRIVO REALE    (01-02 DA FV2    03-04 DA FVA)
140800960216     O                  21   PR2DRE             182 '  /  /  '
140900960216     O                  21   PR2HRE             188 '  :  '
141000960216     O* MINUTI RITARDO
141100960220     O               15 21   PR2MN4        3    193
141200960216     O* CAUSALE RITARDO          (01-02 DA FV2    03-04 DA FVA)
141300960216     O                  21   PR2RTC             197
141400960219     O*
141500960219     O* FITTIZIO, PER IL SALTO GIUSTO DELLE RIGHE
141600960219     O          E            DETT3       0  1
141700960220     O                                              ' '
141800960220     O**
141900960220     O          E            FINEST      2
142000960220     O                       TS(26)             132
142100960220     O**
142200960220     O          E            PIEDI            65
142300960220     O                       TS(24)              66
142400960220     O                       TS(25)             132
142500960208     O*--------------------------------------------------------------*
142600960208     O*
142700960220** SCHIERA GFV
142800960216BCDEFGHILMNOPQRSTUVZ
142900960216** SCHIERA TS
143000960216                    /XXX                                    **  EL   1
143100960216ENCO  TRAZIONI  IN  RITARDO :  R I T A R D I   DI   XXXXXXXXXXXXXX   2
143200960219X  **              FNLS49R                               PAG. XXXX   3
143300960216                                                                     4
143400960220  T R A I N O  |    F.V I A G G I O    XXXXXXXXXXXXXX    TRAZIONIS   5
143500960220NUMERO  LNP/LNA|   TP LNP NUMERO DATA                                6
143600960220XXXXXXX XXX/XXX XXX X XXX XXXXX XX/XX  XX/XX/XX/XX/XX  XXXXXXX-XXX   7
143700960926TA        CONDUCENTE|P.O.|XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX|        R   8
143800960220                    |SEGN| T E O R I C O    R E A L E   |MIN|        9
143900960220XXXXXXXX  XXXXXXXXXX XXX  XX/XX/XX XX:XX XX/XX/XX XX:XX XXXX  X-XX  10
144000960220   I   T   A   R   D   O         IMPU|XXXXXXXXXXXXXXXXXXXXXXXXXXXX  11
144100960220       C A U S A L E             |TAZ|                   MIN        12
144200960212XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  X  XXX XX/XX/XX XX:XX XXXX   X   13
144300960926-   -  TRAZIONI IN PARTENZA DAL P.O. : XXX - XXXXXXXXXXXXXXXXXXXX
144400960926-   -  TRAZIONI IN ARRIVO  AL  P.O.  : XXX - XXXXXXXXXXXXXXXXXXXX
144500960220     DATA FOGLIO VIAGGIO: DAL XX/XX/XXXX     AL XX/XX/XXXX
144600960220     DATA FOGLIO ARRIVI : DAL XX/XX/XXXX     AL XX/XX/XXXX
144700960220               |                                                    18
144800960220                    |    |                              |   |       19
144900960220                                 |   |                              20
145000960220                    |ALAZ|                              |UTI|       21
145100960220                                 |   |XXXXXXXXXXXXXXXXXX UTI  CAUS  22
145200960220------------------------------------------------------------------  23
145300960926IMPUTAZIONE:   A=AUTISTA    F=FORZA MAGGIORE    P=P.O.PARTENZA
145400960926T=P.O.TRANSITO    S=SEDE
145500960220           * * *     F I N E      S T A M P A    * * *              26
145600960220               |   TR                                               18
