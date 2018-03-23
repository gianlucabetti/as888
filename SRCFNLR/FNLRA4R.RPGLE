000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200950201     H* FNLRA4R *-----------------------------------------------------*
000300930609     H*                   QUADRATURA DEFLUENZA        - BATCH -       *
000400930609     H*---------------------------------------------------------------*
000500920907     FAZORG01L  IF   E           K DISK
000600070111     FFNBRV02L  IF   E           K DISK
000700140618     FFNBRV05L  IF   E           K DISK
000800140618     F                                     RENAME(FNBRV000:FNBRV005)
000900020809     F                                     prefix(BR3:3)
001000070111     FFNBRVE1L  IF A E           K DISK
001100070111     F                                     RENAME(FNBRV000:FNBRV00E)
001200950201     FFNFGV01L  IF   E           K DISK
001300000503     FFNFGW01L  IF   E           K DISK
001400950201     FFNFVV01L  IF   E           K DISK
001500910321     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001600950201     D*
001700930330     D STA             S             66    DIM(24) CTDATA PERRCD(1)             SKIERE STAMPA
001800980611     D LNA             S              3  0 DIM(199)                             LNA FOGLI SEL
001900920907     D INL             S              7  0 DIM(199)                             COLLI INOLTRAT
002000920923     D INA             S              7  0 DIM(199)                             PART. DA AREA
002100920923     D INF             S              7  0 DIM(199)                             ARR. DA FILIAL
002200920907     D ARR             S              7  0 DIM(199)                             COLLI ARRIVATI
002300920907     D NIN             S              7  0 DIM(199)                             COLLI NON INOL
002400920907     D PAR             S              1    DIM(116)                             STAMPA FV PART
002500920915     D L1              S              3  0 DIM(30)                              FIL GESTITE
002600020809     D*
002700950201     D PARAM1          DS
002800960712     D  NFV                    1    246P 0
002900960712     D                                     DIM(82)                              FOGLI SELEZ
003000960712     D  PA1STA               249    249
003100960712     D  PA1FGS               250    251P 0
003200950201     D  PA1NFV               252    254P 0
003300950201     D  PA1SN                255    255
003400950201     D  PA1NPG               256    256  0
003500960318     D*
003600960318     D* PASSAGGIO DATI ALLA TRASMISSIONE FV IN SEDE  - FNLSA4R -
003700960318     D PARAM4          DS
003800960318     D  PA4MBD                 1     10
003900021016     D  PA4EMME                1      1    INZ('M')
004000021016     D  PA4DA                  2      4
004100021016     D  PA4A                   5      7
004200960318     D  PA4FEL                11     13  0
004300960318     D* IL CAMPO PA4ALL E' UTILIZZATO NEL PGM FNLSA3R
004400960318     D  PA4ALL                14     14    INZ
004500960318     D* TIPO LANCIO PROGRAMMA ("C" / "L" / " ")
004600960318     D  PA4TLA                15     15
004700960318     D* PA4FLG = "M" ---> RICHIAMATO DALLA GESTIONE F.VIAGGIO
004800960318     D  PA4FLG                16     16    INZ('M')
004900971205     D* IL CAMPO PA4LNP DEVE ESSERE PASSATO SOLO SE PGM
005000960318     D*   RICHIAMATO DALLA GESTIONE F.VIAGGIO
005100960318     D  PA4LNP                17     19  0
005200960318     D*
005300960318     D* CHIUSURA FOGLI PARTENZE
005400920916     D PARAM2          DS
005500940511     D* I SEGUENTI 4  CAMPI LI PASSO VUOTI PERCHE' SERVONO SOLO
005600940511     D* QUANDO DEVO CREARE LE SPUNTE IN AUTOMATICO NELLA CHIUSURA FV
005700940511     D*  (SOLO SE CHIUSURA DA IMP)
005800940511     D  NPG1                   1      1  0 INZ
005900940511     D  NFV1                   2      4P 0 INZ
006000950201     D  FVVDFV                 5      9P 0 INZ
006100960209     D  PA2AGS                10     10
006200920916     D  PARNFV                11     15  0
006300920916     D  PARNUM                17     17  0
006400960129     D* PA2FLG = "M" ---> RICHIAMATO DALLA GESTIONE F.VIAGGIO
006500960129     D  PA2FLG                19     19    INZ
006600920916     D  PARSCE                77     77
006700950207     D  PL1                   78    137P 0
006800950207     D                                     DIM(30)                              FIL GESTITE
006900950207     D  PARSUT               138    157
007000930308     D  PARFEL               158    160  0
007100950201     D* FILIALE GESTIONE FOGLIO I.M.P.
007200950201     D  PARFGS               164    166  0
007300920907     D                 DS
007400000503     D  FVV                    1    903
007500000503     D                                     DIM(301)                             LINEE FOGLIO
007600920907     D  FGVLNA                 1      3  0
007700920907     D  FGVFFV                 4    243
007800920907     D  FGVFF2               244    450
007900000503     D  FGWFF3               451    690
008000000503     D  FGWFF4               691    900
008100000503     D  VUOTO                901    903
008200920522     D WLBDAT          DS
008300950201     D  G02DAT                 1      8  0
008400950201     D  G02INV                 9     16  0
008500950201     D  G02ERR                17     17
008600020809     D  G02TGI                18     22  0
008700020809     D*
008800920427     D KPJBA         E DS
008900020809     D UT§DSE0F      E DS
009000020809     D CNCR80        E DS
009100950201     D* DS PER TRUL06R - CARICAMENTO £X
009200020809     D TRUL06DS      E DS
009300950201     D  LIN                    1     90  0
009400950201     D                                     DIM(30)                              FIL. COMODO
009500020809     D DS3A          E DS
009600020809     D*
009700020809     D COMNPG          S                   LIKE(BRVNPG)                         CATEGORIA FOGLIO
009800020809     D CO1NPG          S                   LIKE(BRVNPG)                         CATEGORIA FOGLIO
009900020809     D CO1FGS          S                   LIKE(BRVFGS)                         FIL.GESTIONE
010000020809     D SAVLNA          S                   LIKE(BRVLNA)                         LNA
010100020809     D CO1NFV          S                   LIKE(FGVNFV)                         NR.FOGLIO
010200020809     D CO2NFV          S                   LIKE(BRVNFV)                         NR.FOGLIO
010300020809     D WNFV            S                   LIKE(BRVNFV)                         NR.FOGLIO
010400020809     D STADFV          S                   LIKE(FVVDFV)                         DATA FOGLIO
010500020809     D XA              S              3  0 INZ
010600020809     D XB              S              3  0 INZ
010700020809     D XC              S              3  0 INZ
010800020809     D XD              S              3  0 INZ
010900020809     D WTIME           S             14  0 INZ                                  ORA & DATA
011000020809     D WDATE           S              8  0 INZ                                  GG/MM/AAAA
011100020809     D UTIME           S              6  0 INZ                                  HH:MM:SS
011200020809     D NUM3            S              3  0 INZ
011300020809     D W0050           S              5  0 INZ
011400020809     D W004A           S              4    INZ
011500020809     D ALF3            S              3    INZ
011600020809     D ALF5            S              5    INZ
011700020809     D ALF8            S              8    INZ
011800020809     C*---------------------------------------------------------------*
011900020809     C*                  INDICATORI                                   *
012000020809     C*---------------------------------------------------------------*
012100920914     C* 20    - OFF  STAMPO SOLO LA TESTATA
012200920422     C* 30/35 - COMODO
012300911018     C*---------------------------------------------------------------*
012400900622     C     *ENTRY        PLIST
012500900622     C                   PARM                    KPJBA
012600950201     C                   MOVEL     KPJBU         PARAM1
012700950201     C*
012800000000     C                   Z-ADD     1             CODUT
012900900621     C                   CALL      'X§PARUT'
013000020809     C                   PARM                    UT§DSE0F
013100000000     C                   MOVEL     RAGUT         RSUT             20
013200920427     C                   MOVEL     REC80         CNCR80
013300930308     C*
013400900828     C*-----------------------------------------------------
013500920417     C*
013600010612     C     KFVV          KLIST
013700950201     C                   KFLD                    COMNPG
013800950201     C                   KFLD                    PA1NFV
013900950201     C                   KFLD                    PA1FGS
014000920907     C     KBRV          KLIST
014100950201     C                   KFLD                    COMNPG
014200000203     C                   KFLD                    WNFV
014300950201     C                   KFLD                    PA1FGS
014400950201     C                   KFLD                    SAVLNA
014500920907     C     KBRV2         KLIST
014600950201     C                   KFLD                    CO1NPG
014700920907     C                   KFLD                    BRVLNP
014800920907     C                   KFLD                    BRVLNA
014900920907     C                   KFLD                    BRVNRS
015000920907     C                   KFLD                    BRVNSC
015100950201     C                   KFLD                    CO1FGS
015200950201     C     KBRV3         KLIST
015300950201     C                   KFLD                    CO1NPG
015400000203     C                   KFLD                    CO2NFV
015500950201     C                   KFLD                    CO1FGS
015600010612     C     KBRVE         KLIST
015700010612     C                   KFLD                    BRVNPG
015800010612     C                   KFLD                    BRVNFV
015900010612     C                   KFLD                    BRVFGS
016000010612     C                   KFLD                    BRVLNP
016100010612     C                   KFLD                    BRVLNA
016200010612     C                   KFLD                    BRVNRS
016300010612     C                   KFLD                    BRVNSC
016400020809     C     K02FGV01      klist
016500020809     C                   kfld                    CO1NFV
016600020809     C                   kfld                    CO1FGS
016700020809     ***C*
016800020809     ***C     *LIKE         DEFINE    BRVNPG        COMNPG                         CATEGORIA FOGLIO
016900020809     ***C     *LIKE         DEFINE    BRVNPG        CO1NPG                         CATEGORIA FOGLIO
017000020809     ***C     *LIKE         DEFINE    BRVFGS        CO1FGS                         FIL.GESTIONE
017100020809     ***C     *LIKE         DEFINE    BRVLNA        SAVLNA                         LNA
017200020809     ***C     *LIKE         DEFINE    FGVNFV        CO1NFV                         NR.FOGLIO
017300020809     ***C     *LIKE         DEFINE    BRVNFV        CO2NFV                         NR.FOGLIO
017400020809     ***C     *LIKE         DEFINE    BRVNFV        WNFV                           NR.FOGLIO
017500020809     ***C     *LIKE         DEFINE    FVVDFV        STADFV                         DATA FOGLIO
017600950407     C*-----------------------------------------------------
017700950407     C                   Z-ADD     1             CO1NPG                         1=PARTENZE
017800950407     C                   Z-ADD     SIMFEL        CO1FGS                         DELL'AREA
017900900828     C*-----------------------------------------------------
018000920421     C*
018100920915     C* CARICO TABELLA FILIALI GESTITE £1
018200920915     C                   EXSR      CARTAB
018300920915     C*
018400950201     C                   Z-ADD     PA1NPG        COMNPG
018500020809     C                   Z-ADD     1             XA
018600020809     C                   Z-ADD     1             XC
018700920907     C*
018800020809     C                   TIME                    WTIME                          ORA & DATA
018900020809     C                   MOVE      WTIME         WDATE                          GG/MM/AAAA
019000020809     C                   MOVEL     WTIME         UTIME                          HH:MM:SS
019100920914     C                   SETON                                        OF
019200920907     C*
019300950201     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
019400020809     C     NFV(XA)       DOWNE     0
019500020809     C                   Z-ADD     NFV(XA)       CO1NFV
019600020809     C                   Z-ADD     NFV(XA)       CO2NFV
019700020809     ***C     CO1NFV        CHAIN     FNFGV01L                           30
019800020809     C     K02FGV01      CHAIN     FNFGV01L                           30
019900920907     C*
020000920907     C  N30              DO
020100020809     ***C     CO1NFV        CHAIN     FNFGW01L                           30
020200020809     C     K02FGV01      CHAIN     FNFGW01L                           30
020300000503     C     *IN30         IFEQ      *OFF
020400000503     C     FGWATB        ORNE      ' '
020500000503     C                   CLEAR                   FGWFF3
020600000503     C                   CLEAR                   FGWFF3
020700000503     C                   ENDIF
020800000503     C*
020900920907     C* CARICO LE LINEE DEL FOGLIO
021000020809     C                   Z-ADD     1             XB
021100020809     C     FVV(XB)       DOWGT     *ZEROS
021200020809     C                   MOVEL     FVV(XB)       NUM3
021300920907     C     NUM3          LOOKUP    LNA                                    30
021400020809     C  N30              Z-ADD     NUM3          LNA(XC)
021500020809     C  N30              ADD       1             XC
021600020809     C                   ADD       1             XB
021700000503     C                   ENDDO
021800920907     C*
021900920907     C* LETTURA E MEMORIZZAZIONE PER LINEA ARRIVO
022000070111     C     KBRV3         SETLL     FNBRV02L
022100070111     C     KBRV3         READE     FNBRV02L                               30
022200920907     C*
022300980727    1C     *IN30         DOWEQ     '0'
022400021113     c* non elaboro linee non previste dal foglio (come invece faceva
022500021113     c*  prima se non avevano la E)
022600021113    3C     SAVLNA        IFNE      BRVLNA
022700021113     C                   Z-ADD     1             XD
022800021113     C     BRVLNA        LOOKUP    LNA(XD)                                31
022900021113     C                   MOVEL     BRVLNA        SAVLNA
023000021113    3C                   END
023100021113     c*
023200980727     C* SE LA SPUNTA HA ANOMALIA "E" LA SCIROV NEL FILE DEGLI ERRORI
023300980727    2C     BRVCAN        IFEQ      'E'
023400070111     C     KBRVE         SETLL     FNBRVE1L                               99
023500070111     C  N99              WRITE     FNBRV00E
023600980727   X2C                   ELSE
023700021113     c**
023800021113    3c     *in31         ifeq      *on
023900920907     C*
024000920907     C* MODIFICA LINEA DI ARRIVO LA CERCO
024100021113    3C**   SAVLNA        IFNE      BRVLNA
024200021113     C**                 Z-ADD     1             XD
024300021113     C**   BRVLNA        LOOKUP    LNA(XD)                                31
024400021113     C**N31              Z-ADD     BRVLNA        LNA(XC)
024500021113     C**N31              Z-ADD     XC            XD
024600021113     C**N31              ADD       1             XC
024700021113     C**                 MOVEL     BRVLNA        SAVLNA
024800021113    3C**                 END
024900920923     C* COLLO INOLTRATO
025000020809     C                   ADD       1             INL(XD)
025100920923     C*
025200950201     C     BRVLNP        LOOKUP    L1                                     36
025300921023     C* 36 ON - COLLO INOLTRATO MA IN PARTENZA DALLA
025400920923     C*         PROPRIA AREA
025500020809     C   36              ADD       1             INA(XD)
025600921023     C* 36 OFF- COLLO INOLTRATO MA IN ARRIVO DA ALTRE FIL.
025700020809     C  N36              ADD       1             INF(XD)
025800021113    3C                   END
025900021113    2C                   END
026000921023     C**
026100920907     C*
026200070111     C     KBRV3         READE     FNBRV02L                               30
026300920907     C                   END
026400920907     C*
026500920911     C                   END
026600920911     C*
026700020809     C                   ADD       1             XA
026800920907     C                   END
026900950201     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
027000960712     C* VOGLIO LA STAMPA DEL CONTROLLO DEFLUENZA
027100960712    1C     PA1STA        IFEQ      'S'
027200960712     C*
027300920911     C* DECODIFICHE PER TESTATA
027400920911     C                   EXSR      DECODI
027500920907     C**
027600920907     C** VERIFICO SE COLLI ARRIVATI NON INOLTRATI
027700920907     C**
027800020809     C                   Z-ADD     1             XA
027900000203     C                   Z-ADD     PA1NFV        WNFV
028000020809     C     LNA(XA)       DOWNE     0
028100020809     C                   Z-ADD     LNA(XA)       SAVLNA
028200070111     C     KBRV          SETLL     FNBRV02L
028300070111     C     KBRV          READE     FNBRV02L                               30
028400920907     C*
028500920907     C     *IN30         DOWEQ     '0'
028600920914     C* COLLO ARRIVATO
028700020809     C                   ADD       1             ARR(XA)
028800950407     C                   SETOFF                                       32
028900920914     C*
029000140618     C     KBRV2         CHAIN     FNBRV05L                           31
029100950407     C     *IN31         DOWEQ     *OFF
029200950407     C*
029300020809     C                   Z-ADD     BR3NFV        W0050
029400000203     C     W0050         LOOKUP    NFV                                    32
029500950407     C   32              SETON                                        31
029600950407     C*
029700140618     C  N31KBRV2         READE     FNBRV05L                               31
029800950407     C                   ENDDO
029900920907     C*
030000950407     C* 32 OFF - COLLO ARRIVATO NON INOLTRATO
030100020809     C  N32              ADD       1             NIN(XA)
030200950407     C  N32              EXSR      STAMPA
030300920907     C*
030400070111     C     KBRV          READE     FNBRV02L                               30
030500950407     C                   ENDDO
030600920907     C*
030700020809     C                   ADD       1             XA
030800950407     C                   ENDDO
030900911204     C*
031000920914     C* STAMPO TOTALE COLLI PER LINEA DI ARRIVO
031100920914     C   20              EXSR      TOTALE
031200930331     C*
031300930331     C* 20 OFF - STAMPO TESTATA
031400930331     C     *IN20         IFEQ      '0'
031500930331     C                   EXCEPT    TES
031600930331     C*
031700930331     C* STAMPO FOGLI VIAGGIO PARTENZA
031800930331     C     STA2          IFNE      *BLANKS
031900930331     C                   MOVEL     STA2          STAPAR          116
032000930331     C                   EXCEPT    TES2
032100930331     C                   END
032200930331     C     STA3          IFNE      *BLANKS
032300930331     C                   MOVEL     STA3          STAPAR
032400930331     C                   EXCEPT    TES2
032500930331     C                   END
032600930331     C     STA4          IFNE      *BLANKS
032700930331     C                   MOVEL     STA4          STAPAR
032800930331     C                   EXCEPT    TES2
032900930331     C                   END
033000930331     C     STA5          IFNE      *BLANKS
033100930331     C                   MOVEL     STA5          STAPAR
033200930331     C                   EXCEPT    TES2
033300930331     C                   END
033400930331     C*
033500930330     C* SE NON CI SONO ANOMALIE STAMPO MESSAGGIO
033600930331     C                   EXCEPT    OK
033700930331     C                   END
033800960712     C                   ENDIF
033900920916     C*
034000920916     C* LANCIO CHIUSURE FOGLI VIAGGIO PARTENZE SE ELABORAZIONE DEFINIT
034100950201     C     PA1SN         IFEQ      'S'
034200020809     C                   Z-ADD     1             XA
034300020809     C     NFV(XA)       DOWNE     0
034400950207     C                   CLEAR                   PARAM2
034500950207     C                   MOVEA     L1            PL1
034600950207     C                   MOVEL     RSUT          PARSUT
034700020809     C                   Z-ADD     NFV(XA)       PARNFV
034800940511     C                   Z-ADD     3             NPG1
034900940511     C                   Z-ADD     0             NFV1
035000940511     C                   Z-ADD     0             FVVDFV
035100950207     C                   MOVEL     SIMFEL        PARFEL
035200920916     C                   MOVEL     PARAM2        KPJBU
035300130718     C                   MOVEL     'LSU2'        KCOAZ
035400920916     C                   CALL      'BCH10'
035500920916     C                   PARM                    KPJBA
035600020809     C                   ADD       1             XA
035700920916     C                   END
035800920916     C                   END
035900960318     C*
036000960318     C* SOTTOMETTO IL PGM DI TRASMISSIONE FV A SEDE
036100960318     C                   RESET                   PARAM4
036200021016     C                   MOVEL     SIMFEL        PA4DA
036300021016     C                   MOVE      046           PA4A
036400960318     C                   MOVEL     SIMFEL        PA4FEL
036500960318     C                   MOVEL     PARAM4        KPJBU
036600960318     C                   MOVEL     'LSA4'        KCOAZ
036700960318     C                   CALL      'BCH10'
036800960318     C                   PARM                    KPJBA
036900920424     C*
037000000000     C                   SETON                                        LR
037100950201     C**************************************************************************
037200950201     C*    CARICO TABELLA FILIALI GESTITE £1
037300950201     C**************************************************************************
037400920915     C     CARTAB        BEGSR
037500950201     C*
037600020809     C                   CLEAR                   TRUL06DS
037700950201     C                   MOVE      '£1'          D06COD
037800950201     C                   MOVEL     SIMFEL        D06KEY
037900020809     C                   MOVEL     TRUL06DS      KPJBU
038000950201     C*
038100950201     C                   CALL      'TRUL06R'
038200950201     C                   PARM                    KPJBA
038300020809     C                   MOVEL     KPJBU         TRUL06DS
038400950201     C                   MOVEA     LIN           L1
038500920916     C*
038600920916     C                   MOVEL     *ZEROS        PARNUM
038700920916     C                   MOVEL     'N'           PARSCE
038800950201     C*
038900920915     C                   ENDSR
039000950201     C**************************************************************************
039100950201     C* DECODIFICHE PER TESTATA
039200950201     C**************************************************************************
039300920907     C     DECODI        BEGSR
039400950201     C*
039500950201     C     KFVV          CHAIN     FNFVV01L                           30
039600950201     C*
039700950201     C  N30              DO
039800920907     C* DATA FOGLIO
039900950201     C                   Z-ADD     FVVDFV        G02INV
040000950201     C                   Z-ADD     *ZEROS        G02DAT
040100920907     C                   MOVEL     3             G02ERR
040200950201     C                   CALL      'XSRDA8'
040300920907     C                   PARM                    WLBDAT
040400950201     C                   Z-ADD     G02DAT        STADFV
040500920907     C* FILIALE EMITTENTE
040600950201     C     FVVFGS        CHAIN     AZORG                              30
040700920907     C  N30              MOVEL     ORGDES        DESCDU           15
040800920907     C                   END
040900920907     C*
041000920907     C* FOGLI PARTENZE
041100020809     C                   Z-ADD     1             XA
041200020809     C                   Z-ADD     1             XC
041300020809     C     NFV(XA)       DOWNE     0
041400020809     C                   MOVEL     NFV(XA)       ALF5
041500020809     C                   MOVEA     ALF5          PAR(XC)
041600020809     C                   ADD       6             XC
041700020809     C                   ADD       1             XA
041800920907     C*
041900020809     C     XC            IFEQ      115
042000020809     C     XA            IFEQ      20
042100920907     C                   MOVEA     PAR           STA1            116
042200920907     C                   END
042300020809     C     XA            IFEQ      39
042400920907     C                   MOVEA     PAR           STA2            116
042500920907     C                   END
042600020809     C     XA            IFEQ      58
042700920907     C                   MOVEA     PAR           STA3            116
042800920907     C                   END
042900020809     C     XA            IFEQ      77
043000920907     C                   MOVEA     PAR           STA4            116
043100920907     C                   END
043200920907     C*
043300920907     C                   MOVEA     *BLANKS       PAR
043400020809     C                   Z-ADD     1             XC
043500920907     C                   END
043600920907     C                   END
043700920907     C*
043800020809     C     XA            IFLE      20
043900920907     C                   MOVEA     PAR           STA1            116
044000920907     C                   ELSE
044100020809     C     XA            IFLE      39
044200920907     C                   MOVEA     PAR           STA2            116
044300920907     C                   ELSE
044400020809     C     XA            IFLE      58
044500920907     C                   MOVEA     PAR           STA3            116
044600920907     C                   ELSE
044700020809     C     XA            IFLE      77
044800920907     C                   MOVEA     PAR           STA4            116
044900920907     C                   ELSE
045000920907     C                   MOVEA     PAR           STA5            116
045100920907     C                   END
045200920907     C                   END
045300920907     C                   END
045400920907     C                   END
045500920907     C*
045600920907     C* LINEE DI ARRIVO
045700020809     C                   Z-ADD     1             XA
045800020809     C                   Z-ADD     1             XC
045900920907     C                   MOVEA     *BLANKS       PAR
046000920907     C*
046100020809     C     LNA(XA)       DOWNE     0
046200020809     C     LNA(XA)       CHAIN     AZORG                              30
046300020809     C  N30              MOVEL     ORGDES        ALF8
046400920907     C   30              MOVEL     *BLANKS       ALF8
046500020809     C                   MOVEL     LNA(XA)       ALF3
046600020809     C                   MOVEA     ALF3          PAR(XC)
046700020809     C                   ADD       3             XC
046800020809     C                   MOVEA     '-'           PAR(XC)
046900020809     C                   ADD       1             XC
047000020809     C                   MOVEA     ALF8          PAR(XC)
047100920907     C*
047200020809     C                   ADD       9             XC
047300020809     C                   ADD       1             XA
047400920907     C*
047500020809     C     XC            IFEQ      118
047600020809     C     XA            IFEQ      10
047700920907     C                   MOVEA     PAR           STAL1           116
047800920907     C                   END
047900020809     C     XA            IFEQ      19
048000920907     C                   MOVEA     PAR           STAL2           116
048100920907     C                   END
048200020809     C     XA            IFEQ      28
048300920907     C                   MOVEA     PAR           STAL3           116
048400920907     C                   END
048500020809     C     XA            IFEQ      37
048600920907     C                   MOVEA     PAR           STAL4           116
048700920907     C                   END
048800020809     C     XA            IFEQ      46
048900920907     C                   MOVEA     PAR           STAL5           116
049000020809     C                   Z-ADD     199           XA
049100920907     C                   END
049200920907     C*
049300020809     C                   Z-ADD     1             XC
049400920907     C                   MOVEA     *BLANKS       PAR
049500920907     C                   END
049600920907     C                   END
049700920907     C*
049800020809     C     XA            IFLE      10
049900920907     C                   MOVEA     PAR           STAL1           116
050000920907     C                   ELSE
050100020809     C     XA            IFLE      19
050200920907     C                   MOVEA     PAR           STAL2           116
050300920907     C                   ELSE
050400020809     C     XA            IFLE      28
050500920907     C                   MOVEA     PAR           STAL3           116
050600920907     C                   ELSE
050700020809     C     XA            IFLE      37
050800920907     C                   MOVEA     PAR           STAL4           116
050900920907     C                   ELSE
051000020809     C     XA            IFLE      46
051100920907     C                   MOVEA     PAR           STAL5           116
051200930331     C                   END
051300920907     C                   END
051400920907     C                   END
051500920907     C                   END
051600920907     C                   END
051700920907     C*
051800920907     C                   ENDSR
051900950201     C**************************************************************************
052000950201     C* STAMPO ERRORE
052100950201     C**************************************************************************
052200900621     C     STAMPA        BEGSR
052300920421     C*
052400920424     C                   SETON                                        20
052500920424     C*
052600911015     C* STAMPA TESTATA
052700920907     C   OF              DO
052800920907     C                   EXCEPT    TES
052900920907     C     STA2          IFNE      *BLANKS
053000920907     C                   MOVEL     STA2          STAPAR          116
053100920907     C                   EXCEPT    TES2
053200920907     C                   END
053300920907     C     STA3          IFNE      *BLANKS
053400920907     C                   MOVEL     STA3          STAPAR
053500920907     C                   EXCEPT    TES2
053600920907     C                   END
053700920907     C     STA4          IFNE      *BLANKS
053800920907     C                   MOVEL     STA4          STAPAR
053900920907     C                   EXCEPT    TES2
054000920907     C                   END
054100920907     C     STA5          IFNE      *BLANKS
054200920907     C                   MOVEL     STA5          STAPAR
054300920907     C                   EXCEPT    TES2
054400920907     C                   END
054500920907     C*
054600920907     C                   EXCEPT    TES3
054700920907     C     STAL2         IFNE      *BLANKS
054800920907     C                   MOVEL     STAL2         STAPAR
054900920907     C                   EXCEPT    TES2
055000920907     C                   END
055100920907     C     STAL3         IFNE      *BLANKS
055200920907     C                   MOVEL     STAL3         STAPAR
055300920907     C                   EXCEPT    TES2
055400920907     C                   END
055500920907     C     STAL4         IFNE      *BLANKS
055600920907     C                   MOVEL     STAL4         STAPAR
055700920907     C                   EXCEPT    TES2
055800920907     C                   END
055900920907     C     STAL5         IFNE      *BLANKS
056000920907     C                   MOVEL     STAL5         STAPAR
056100920907     C                   EXCEPT    TES2
056200920907     C                   END
056300920907     C*
056400920907     C                   EXCEPT    TES4
056500920907     C*
056600920907     C                   SETOFF                                       OF
056700920907     C                   END
056800911018     C*
056900900622     C* STAMPO DETTAGLIO
057000920915     C     BRVLNP        CHAIN     AZORG                              31
057100920907     C  N31              MOVEL     ORGDES        DESLNP           20
057200920907     C   31              MOVEL     *BLANKS       DESLNP
057300920907     C*
057400920907     C     BRVLNA        CHAIN     AZORG                              31
057500920907     C  N31              MOVEL     ORGDES        DESLNA           20
057600920907     C   31              MOVEL     *BLANKS       DESLNA
057700920907     C*
057800911018     C                   EXCEPT    DET
057900900622     C*
058000900622     C                   ENDSR
058100950201     C**************************************************************************
058200950201     C* STAMPO TOTALE COLLI X LNA
058300950201     C**************************************************************************
058400920914     C     TOTALE        BEGSR
058500920914     C*
058600920914     C                   SETON                                        OF
058700920914     C* STAMPO TESTATA
058800920915     C   OF              DO
058900920915     C                   EXCEPT    TES
059000930401     C*
059100930401     C* STAMPO FOGLI VIAGGIO PARTENZA
059200930401     C     STA2          IFNE      *BLANKS
059300930401     C                   MOVEL     STA2          STAPAR          116
059400930401     C                   EXCEPT    TES2
059500930401     C                   END
059600930401     C     STA3          IFNE      *BLANKS
059700930401     C                   MOVEL     STA3          STAPAR
059800930401     C                   EXCEPT    TES2
059900930401     C                   END
060000930401     C     STA4          IFNE      *BLANKS
060100930401     C                   MOVEL     STA4          STAPAR
060200930401     C                   EXCEPT    TES2
060300930401     C                   END
060400930401     C     STA5          IFNE      *BLANKS
060500930401     C                   MOVEL     STA5          STAPAR
060600930401     C                   EXCEPT    TES2
060700930401     C                   END
060800930401     C*
060900930401     C                   EXCEPT    TES5
061000920915     C                   SETOFF                                       OF
061100920915     C                   END
061200920914     C*
061300920914     C* TOTALE COLLI ARRIVATI
061400920914     C                   XFOOT     ARR           TOTARR           10 0
061500920914     C* TOTALE COLLI INOLTRATI
061600920914     C                   XFOOT     INL           TOTINL           10 0
061700920923     C* TOTALE COLLI INOLTRATI MA IN PARTENZA DALLA PROPRIA AREA
061800920923     C                   XFOOT     INA           TOTINA           10 0
061900920923     C* TOTALE COLLI INOLTRATI MA IN ARRIVO DA ALTRE FILIALI
062000920923     C                   XFOOT     INF           TOTINF           10 0
062100920914     C* TOTALE COLLI NON INOLTRATI
062200920914     C                   XFOOT     NIN           TOTNIN           10 0
062300920914     C*
062400020809     C                   Z-ADD     1             XA
062500020809     C     LNA(XA)       DOWNE     0
062600920914     C* DECODIFICO LNA
062700020809     C     LNA(XA)       CHAIN     AZORG                              30
062800920914     C  N30              MOVEL     ORGDES        DESCR            20
062900920914     C   30              MOVEL     *BLANKS       DESCR
063000920914     C*
063100920915     C   OF              DO
063200920915     C                   EXCEPT    TES
063300930401     C*
063400930401     C* STAMPO FOGLI VIAGGIO PARTENZA
063500930401     C     STA2          IFNE      *BLANKS
063600930401     C                   MOVEL     STA2          STAPAR          116
063700930401     C                   EXCEPT    TES2
063800930401     C                   END
063900930401     C     STA3          IFNE      *BLANKS
064000930401     C                   MOVEL     STA3          STAPAR
064100930401     C                   EXCEPT    TES2
064200930401     C                   END
064300930401     C     STA4          IFNE      *BLANKS
064400930401     C                   MOVEL     STA4          STAPAR
064500930401     C                   EXCEPT    TES2
064600930401     C                   END
064700930401     C     STA5          IFNE      *BLANKS
064800930401     C                   MOVEL     STA5          STAPAR
064900930401     C                   EXCEPT    TES2
065000930401     C                   END
065100930401     C*
065200930401     C                   EXCEPT    TES5
065300920915     C                   SETOFF                                       OF
065400920915     C                   END
065500920915     C*
065600920915     C                   EXCEPT    TOT2
065700920914     C*
065800020809     C                   ADD       1             XA
065900920914     C                   END
066000920914     C*
066100920914     C* STAMPO TOTALE GENERALE
066200920915     C                   EXCEPT    TOTGEN
066300920914     C*
066400920914     C                   ENDSR
066500950201     C**************************************************************************
066600911018     OQSYSPRT   E            TES              02
066700911015     O                       STA(1)              66
066800911015     O                       STA(2)             132
066900900626     O                       RSUT                20
067000920427     O                                           21 '/'
067100920427     O                       SIMFEL              24
067200950201     O                       WDATE              116 '  /  /    '
067300901115     O                       PAGE          Z    132
067400920907     O          E            TES         1
067500950201     O                       KNSIF               10
067600950201     O                       KNMUS               21
067700950201     O                       UTIME              115 '  :  :  '
067800920907     O          E            TES         2
067900920907     O                       STA(3)              66
068000950201     O                       FVVFGS              22
068100920907     O                       DESCDU              38
068200950201     O                       PA1NPG              48
068300930916     O                                           49 '-'
068400950201     O                       PA1NFV        Z     54
068500950201     O                       STADFV              71 '  /  /    '
068600911015     O          E            TES         2
068700920907     O                       STA(4)              66
068800920907     O                       STA1               132
068900950201     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
069000920907     O          E            TES2        1
069100920907     O                       STAPAR             132
069200950201     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
069300920907     O          E            TES3        2
069400920907     O                       STA(6)              66
069500920907     O                       STAL1              132
069600950201     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
069700920907     O          E            TES4        2
069800920907     O                       STA(8)              66
069900920915     O                       STA(11)            132
070000920915     O          E            TES4        1
070100920915     O                       STA(9)              66
070200920915     O                       STA(12)            132
070300950201     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
070400911015     O          E            DET         1
070500920915     O                       BRVLNP               3
070600920907     O                                            4 '-'
070700920907     O                       DESLNP              24
070800920907     O                       BRVLNA              29
070900920907     O                                           30 '-'
071000920907     O                       DESLNA              50
071100920915     O                       BRVNRS        Z     54
071200920915     O                       BRVNSC        2     69
071300950201     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
071400930401     O          E            TES5        2
071500920923     O                       STA(17)            132
071600930401     O          E            TES5        1
071700920923     O                       STA(14)             66
071800920923     O                       STA(18)            132
071900930401     O          E            TES5        1
072000920915     O                       STA(15)             66
072100920923     O                       STA(19)            132
072200930401     O          E            TES5        1
072300920914     O                                          132 ' '
072400950201     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
072500920915     O          E            TOT2        2
072600020809     O                       LNA(XA)             10
072700920914     O                                         +  1 '-'
072800920914     O                       DESCR             +  1
072900020809     O                       ARR(XA)       2     51
073000020809     O                       NIN(XA)       2     68
073100020809     O                       INF(XA)       2     88
073200020809     O                       INA(XA)       2    108
073300020809     O                       INL(XA)       2    130
073400950201     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
073500920915     O          E            TOTGEN      2
073600920923     O                       STA(21)             66
073700920923     O                       STA(21)            132
073800920915     O          E            TOTGEN      2
073900920923     O                       STA(22)             66
074000920923     O                       TOTARR        2     51
074100920923     O                       TOTNIN        2     68
074200920923     O                       TOTINF        2     88
074300920923     O                       TOTINA        2    108
074400920923     O                       TOTINL        2    130
074500920915     O          E            TOTGEN      2
074600920923     O                       STA(21)             66
074700920923     O                       STA(21)            132
074800950201     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
074900930330     O          E            OK               15
075000930330     O                       STA(23)             66
075100930330     O                       STA(24)            132
075200950201     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
075300930330**
075400920907XXXXXXXXXXXXXXXXXXXX                 ***   QUADRATURA   DEFLUENZA     1
075500950201  ***                     FNLRA4R       XX/XX/XXXX       PAG. XXXX    2
075600950201FOGLIO ARRIVI : DI XXX-XXXXXXXXXXXXXXX   N.RO  X-XXXXX  DEL  XX/XX    3
075700920907FOGLI PARTENZE: XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XX    4
075800920907XXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX       5
075900920916LINEE CONTROL.: XXX-XXXXXXXX XXX-XXXXXXXX XXX-XXXXXXXX XXX-XXXXXXX    6
076000920907X XXX-XXXXXXXX XXX-XXXXXXXX XXX-XXXXXXXX XXX-XXXXXXXX XXX-XXXXXXXX    7
076100920915LINEA DI PARTENZA         LINEA DI ARRIVO          SERIE    SEGNAC    8
076200920915                                                           NON INO    9
076300920915XXX-XXXXXXXXXXXXXXXXXXXX  XXX-XXXXXXXXXXXXXXXXXXXX  XX      X.XXX.   10
076400920915OLLO                                                                 11
076500920915LTRATO                                                               12
076600920915XXX                                                                  13
076700920923       LINEE CONTROLLATE                    COLLI          COLLI N   14
076800920923                                           ARRIVATI        INOLTRA   15
076900920923       XXX - XXXXXXXXXXXXXXXXXXXX         X.XXX.XXX        X.XXX.X   16
077000920923             C  O  L  L  I        I  N  O  L  T  R  A  T  I          17
077100920923ON        COLLI IN  ARRIVO   COLLI IN  PARTENZA         TOTALE       18
077200960926TI          DA ALTRI P.O.    DALLA PROPRIA AREA        INOLTRATI     19
077300920923XX           X.XXX.XXX           X.XXX.XXX             X.XXX.XXX     20
077400920923------------------------------------------------------------------   21
077500920923T O T A L E   G E N E R A L E                                        22
077600930330                                        TUTTO  O.K.  :  NESSUNA      23
077700930330ANOMALIA  RISCONTRATA  !!                                            24
