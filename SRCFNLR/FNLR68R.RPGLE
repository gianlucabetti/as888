000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200940721     H* FNLR68R *----------------------------------------------------*
000300940721     H*           INTERROGAZIONE BOLLE TRANSITI
000400000000     H*--------------------------------------------------------------*
000500940721     FFNLR68D   CF   E             WORKSTN
000600900517     F                                     INFDS(CA02DS)
000700940721     F                                     SFILE(LR68S02:NRR2)
000800940722     FAZORG01L  IF   E           K DISK
000900091027     Ffnfgv01l  IF   E           K DISK
001000940722     FFNBTP01L  IF   E           K DISK
001100160517     FFNarb01L  IF   E           K DISK
001200020821     FFNBTP11L  IF   E           K DISK
001300020821     F                                     RENAME(FNBTP000:FNBTP011)
001400940722     FFNBTP26L  IF   E           K DISK
001500940722     F                                     RENAME(FNBTP000:FNBTP026)
001600940722     FFNBTP31L  IF   E           K DISK
001700940722     F                                     RENAME(FNBTP000:FNBTP031)
001800940722     FFNBTT01L  IF   E           K DISK
001900920413     FTABEL00F  IF   E           K DISK
002000040531     Ffiar501l  IF   E           K DISK
002100940721      *-------------------------------------------------------------------------
002200900515     D C1              S              1    DIM(25)                              COMODO
002300920206     D SEG             S              1    DIM(15)                              COMODO X SEGN.
002400920413     D SEQ             S             15    DIM(1) CTDATA PERRCD(1)              NON SEQUENZIALSUR
002500940721      *-------------------------------------------------------------------------
002600930111     D                 DS
002700940722     D  DATA                   1      8  0
002800940722     D  AA                     1      4  0
002900940722     D  MM                     5      6  0
003000940722     D  GG                     7      8  0
003100940722     D  MMGG                   5      8  0
003200920206     D WLBDAT          DS
003300940721     D  G02DAT                 1      8  0
003400940721     D  G02INV                 9     16  0
003500940721     D  G02ERR                17     17
003600940721     D  G02TGI                18     22  0
003601161103     D* RICHIAMO PGM INTERROGAZ.BOLLE unica               - FNLRU6R -
003602161103     D PARAMu6ds       DS
003603161103     d* CAMPI DI OUTPUT:
003604161103     d* . Key spedizione
003605161103     D  pa1AAS                14     17  0
003606161103     D  pa1LNP                18     20  0
003607161103     D  pa1NRS                21     22  0
003608161103     D  pa1NSP                23     29  0
003609161103     d* . Flag '1'=Premuto F3=Fine
003610161103     D  PA1F03                30     30
003611161103     d* CAMPI DI INPUT:
003612161103     d* . Modalità di richiamo
003613161103     D  PA1FLG                31     31
003614161103     D* . Flag '1'= RICH DA PGM GIAC. (Serve per disabilitare tasto funzionale F14=Giac.
003615161103     d*                                in interrogazione della bolla ed evitare così
003616161103     d*                                errore di chiamata ricorsiva
003617161103     D  PA1GIA               144    144
003618161103     d* . Solo dal chiamante FNLRU6R1 per il Perfect Order Wurth:
003619161103     d  pa1rma               149    163
003620161103     d  pa1xco               164    164
003621161103     d* non utilizzato
003622161103     d  pa1flo               165    256
003700920423     D* RICHIAMO PGM DI INTERROGAZIONE BOLLE ARRIVI
003800161103     D PARAM2          DS
003900161103     D  PA2FLG                31     31
004000161103     D  PA2BOL                62     62
004100930115     D* RICHIAMO PGM INTERROGAZIONE ANOMALIE
004200960523     D DSLR44        E DS                  EXTNAME(FNLR44DS)
004300940722     D* RICHIAMO PGM INTERROGAZIONE DETTAGLIO COLLI
004400940727     D PARAA8          DS
004500940727     D  PA8AAS                14     17  0
004600940727     D  PA8LNP                18     20  0
004700940727     D  PA8NRS                21     22  0
004800940727     D  PA8NSP                23     29  0
004900940727     D  PA8DSP                30     37  0
005000940727     D  PA8LNA                38     40  0
005100940727     D  PD8LNA                41     60
005200940727     D  PA8RSD                61     95
005300940727     D  PA8RSM                96    130
005400940727     D  PA8DSA               131    151
005500940727     D  PA8CML               152    152
005600020821     D  PA8POT               153    155  0
005700020821     D  PD8POT               156    170
005800940923     D PARA04          DS
005900940923     D  P04NFV                 1      5  0
006000940923     D  P04LNP                 6      8  0
006100940923     D  P04LNA                 9     11  0
006200940923     D  P04NRS                12     13  0
006300940923     D  P04NCD                14     20  0
006400940923     D  P04NCA                21     27  0
006500940923     D  P04NPS                28     29  0
006600940923     D  P04NPG                30     30  0
006700940923     D  P04FLG                31     31
006800940923     D  P04PGM                32     38
006900950112     D  P04FGS                39     41  0
007000101202     d* bolla
007100101202     d  p04bolp               42     44  0
007200101202     d  p04bors               45     46  0
007300101202     d  p04bosp               47     53  0
007400101202     d  p04boan               54     57  0
007500101202     D  P04ERR                58     58
007600901116     D                 DS
007700940721     D  V1CLNA                 1      3  0
007800940721     D  V1CLP1                 4      6  0
007900940722     D  V1CDSD                 7     14  0
008000940722     D  V1CDSA                15     22  0
008100020822     D  SEL1                   1     22  0
008200020822      *
008300020822     D                 DS
008400020822     D  V1CLP2                 1      3  0
008500020822     D  V1CNRS                 4      5  0
008600020822     D  V1CNSP                 6     12  0
008700020822     D  SEL2                   1     12  0
008800020821      *
008900940721     D* DS PER COMPORRE LA DATA SPEDIZIONE
009000900515     D                 DS
009100940721     D  BTPAAS                 1      4  0
009200940721     D  BTPMGS                 5      8  0
009300940721     D  BTPDSP                 1      8  0
009400900517     D CA02DS          DS
009500900517     D  PRIMA                378    379B 0
009600090604     d ds5e          e ds
009700920413     D DS3A          E DS
009800920413     D CNCR80        E DS
009900900514     D KPJBA         E DS
010000040531     D Dar5gen       E DS
010100920224     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
010200040531     D ktrd            s                   like(ar5trd) inz('GEN')
010300900518     C****************************************************************
010400900518     C*  RIEPILOGO INDICATORI
010500900518     C***************************************************************
010600920423     C* 01-16 - INDOCANO QUALI CAMPI HO SCELTO A VIDEO
010700920413     C* 03    - ESPEDIZIONE IN FRANCO
010800920423     C* 05    - NON ABILITO COMANDI PER ANDARE IN ALTRE INTERROGAZIONI
010900900518     C* 17-19 - INDICANO IL TIPO DI ARCHIVIO(IN PART.;PART.;ENTRAM)SCEL
011000920203     C* 20    - DISABILITO CMD7 INTERROGAZIONE BOLLE ARRIVI
011100921021     C* 21    - RICHIESTA SIA LINEA DI PARTENZA CHE LINEA DI ARRIVO
011200930115     C* 22    - SIMFEL HA IL TRISROT EMETTO VOLUME DA CML
011300920413     C* 30    - FINE LETTURA FILE
011400900518     C* 31    - COMODO
011500900518     C* 32    - BOLLA LETTA DA SCARTARE
011600020801     C* 33    - IND. DEL LOOP DI LETTURA PER P.O DI TRANSITO
011700901123     C* 34    - LETTURA SUBFILE PER RECORD SCELTI
011800920413     C* 35-37 - PER SUBFILE
011900901123     C* 40-60 - ERRORI
012000900518     C* 90    - INDICA LA PRESENZA DI UN ERRORE
012100900518     C* 99    - ERRORE
012200900518     C*****************************************************************
012300000000     C     *ENTRY        PLIST
012400000000     C                   PARM                    KPJBA
012500920423     C                   MOVEL     KPJBU         PARAM2
012600920203     C*
012700920224     C                   CALL      'X§PARUT'
012800920224     C                   PARM                    UT§DSE
012900940722     C                   MOVEL     RAGUT         V1CRSU
013000920224     C                   MOVEL     REC80         CNCR80
013100940721     C*---------------------------------------------------------------*
013200920423     C* VEDO SE PGM RICHIAMAMTO NON ABILITO INTERROGAZIONI
013300940722     C     PA2BOL        IFEQ      'T'
013400920423     C                   SETON                                        05
013500920423     C                   END
013600900516     C* PULIZIA CAMPI CHE RIMANGONO INVARIATI IL SECONDO GIRO
013700021114     C                   move      simfel        V1CPOT
013800940721     C                   MOVEL     ' '           V1CFTA
013900940721     C                   MOVEL     *ZEROS        V1CLNA
014000940721     C                   MOVEL     *ZEROS        V1CLP1
014100020822     C                   MOVEL     *ZEROS        V1CLP2
014200940721     C                   MOVEL     *ZEROS        V1CDSD
014300940721     C                   MOVEL     *ZEROS        V1CDSA
014400940722     C                   Z-ADD     UYEAR         V1CAAS
014500940722     C     V1CAAS        IFGT      60
014600940722     C                   MOVEL     19            V1CAAS                         XX SECOLO
014700940722     C                   ELSE
014800940722     C                   MOVEL     20            V1CAAS                         XXI SECOLO
014900940722     C                   ENDIF
015000940722     C*
015100900509     C     INIZIO        TAG
015200900514     C                   EXSR      PUL01
015300940722     C                   MOVEL     'S'           WRIEM                          SI RIEMETTO TESTATA
015400900509     C     FOR01         TAG
015500940722     C     WRIEM         IFEQ      'S'
015600940722     C                   WRITE     LR68T01                                      INTESTAZIONE VIDEO
015700940722     C                   MOVEL     *BLANKS       WRIEM             1            NO RIEMETTO TESTATA
015800940722     C                   ENDIF
015900940722     C                   EXFMT     LR68D01
016000920203     C** CMD3 - FINE LAVORO
016100900509     C   KC              GOTO      FINE
016200161103     C** CMD7  - INTERROGAZIONE BOLLE
016300920203     C   KG              DO
016400161103     C***                MOVEL     '4'           PA2FLG
016500161103     C***                MOVEL     *BLANKS       KPJBU
016600161103     C***                MOVEL     PARAM2        KPJBU
016700161103     C***                CALL      'FNLR36R'
016800161103     C***                PARM                    KPJBA
016801161103     c                   clear                   paramu6ds
016802161103     c                   movel     paramu6ds     kpjbu
016803161103     c                   call      'FNLRU6R'
016804161103     c                   parm                    kpjba
016900940722     C                   MOVEL     'S'           WRIEM                          SI RIEMETTO TESTATA
017000920203     C                   GOTO      FOR01
017100920203     C                   END
017200920414     C*
017300920414     C** CMD14 - INTERROGAZIONE BOLLE PARTENZE
017400161103     C*  KN              DO
017500161103     C*                  MOVEL     'A'           PA2FLG
017600161103     C*                  MOVEL     *BLANKS       KPJBU
017700161103     C*                  MOVEL     PARAM2        KPJBU
017800161103     C*                  CALL      'FNLS04R'
017900161103     C*                  PARM                    KPJBA
018000161103     C*                  MOVEL     'S'           WRIEM                          SI RIEMETTO TESTATA
018100161103     C*                  GOTO      FOR01
018200161103     C*                  END
018300900509     C* CONTROLLI FORMATO1
018400900509     C                   EXSR      CONTR
018500900509     C   90              GOTO      FOR01
018600900511     C* ELABORAZIONE
018700900515     C                   EXSR      ELAB
018800901115     C*
018900901115     C* SE NUM SPE SECCO DOPO VISUALIZZAZIONE TORNO A VID. INIZIALE
019000901115     C     SEL2          IFNE      0
019100021115      * reimposto fra le prima selezioni simfel
019200021115     C                   move      simfel        V1CPOT
019300901115     C                   GOTO      INIZIO
019400901115     C                   END
019500901115     C* SE NON CI SONO BOLLE SEGNALO ERRORE
019600940721     C     NRR2          IFEQ      0
019700900516     C                   SETON                                        58
019800900516     C                   GOTO      FOR01
019900900516     C                   END
020000900515     C** CARICAMENTO SUBFILE
020100900518     C     FORCT2        TAG
020200020822      *
020300940722     C                   WRITE     LR68T01                                      INTESTAZIONE VIDEO
020400940722     C                   WRITE     LR68Z02                                      TASTI FUNZIONALI
020500900515     C** COMANDI SUBFILE
020600900515     C     FORCT         TAG
020700940721     C                   EXFMT     LR68C02
020800920203     C* CMD 3 - FINE
020900900515     C   KC              GOTO      FINE
021000900515     C* CMD12 - RITORNO
021100900517     C   KL              GOTO      INIZIO
021200900515     C* ROLLUP
021300940505     C     *IN37         IFEQ      *ON
021400940505     C   30              SETON                                        59
021500021111     C  n30              EXSR      CARSF
021600021111     C
021700940721     C                   Z-ADD     COMOD         REC2
021800940505     C                   GOTO      FORCT
021900940505     C                   ENDIF
022000900515     C*
022100900516     C** CONTROLLO SE EFFETTUATA UNA SCELTA
022200940721     C                   Z-ADD     PRIMA         REC2
022300940721     C                   READC     LR68S02                                34
022400900516     C   34              SETON                                        99
022500900516     C   34              GOTO      FORCT
022600900516     C* VISUALIZZO BOLLA X BOLLA
022700900516     C                   DO        *HIVAL
022800941109     C     VSCSCE        IFEQ      '5'
022900021114      * Linea di partenza
023000940721     C   01              MOVEL     VSCLIN        PARLNP
023100020822     C   02              MOVEL     V1CLP1        PARLNP
023200021113     C   12              MOVEL     V1CLPA        PARLNP
023300920413     C                   EXSR      BOLLA                                         CARICO BOLL
023400900516     C                   END
023500940721     C                   MOVEL     *BLANKS       VSCSCE
023600940721     C                   UPDATE    LR68S02
023700920203     C* SE FLAG 2 TORNO AL SUBFILE
023800920413     C     FLAG          IFEQ      '2'
023900900530     C                   SETON                                        34
024000920413     C                   ELSE
024100940721     C                   READC     LR68S02                                34
024200920413     C                   END
024300900530     C  N34              END
024400900518     C                   GOTO      FORCT2
024500940722     C*
024600000000     C     FINE          TAG
024700000000     C                   SETON                                        LR
024800940722     C**************************************************************************
024900940722     C*    PULIZIA CAMPI FORMATO 1
025000940722     C**************************************************************************
025100900514     C     PUL01         BEGSR
025200940722     C*
025300021113     C                   MOVEL     *ZEROS        V1CNRS
025400021113     C                   MOVEL     *ZEROS        V1CLP2
025500940721     C                   MOVEL     *ZEROS        V1CNSP
025600940721     C                   MOVEL     *BLANKS       V1CRSM
025700940721     C                   MOVEL     *BLANKS       V1CTSP
025800940721     C                   MOVEL     *ZEROS        V1CPSD
025900940721     C                   MOVEL     99            V1CPSA
026000940721     C                   MOVEL     *ZEROS        V1CSCA
026100940721     C                   MOVEL     *ZEROS        V1CSCD
026200940721     C                   Z-ADD     0             NRR2
026300900516     C                   EXSR      PULSF
026400940722     C*
026500900514     C                   ENDSR
026600940722     C**************************************************************************
026700940722     C*    CONTROLLI FORMATO1
026800940722     C**************************************************************************
026900900509     C     CONTR         BEGSR
027000940722     C*
027100900515     C                   SETOFF                                       900102
027200020822     C                   SETOFF                                       040612
027300920413     C                   SETOFF                                       070811
027400921021     C                   SETOFF                                       151621
027500021108     C     V1CSCA        COMP      0                                  08
027600021108     C     V1CSCD        COMP      0                                  07
027700940722     C*
027800940722     C* SE L'ANNO E' DI DUE CIFRE, LO METTO A QUATTRO
027900000107     C     V1CAAS        IFLT      100
028000940722     C     V1CAAS        IFGT      60
028100940722     C                   MOVEL     19            V1CAAS                         XX SECOLO
028200940722     C                   ELSE
028300940722     C                   MOVEL     20            V1CAAS                         XXI SECOLO
028400940722     C                   ENDIF
028500000107     C                   ENDIF
028600940722     C*
028700940721     C                   MOVEL     *BLANKS       VCCLIN
028800901115     C* SELEZIONO PER DATA SPEDIZIONE O PER NUMERO SPEDIZIONE
028900901115     C     SEL1          IFEQ      0
029000921021     C     SEL2          ANDEQ     0
029100901115     C                   SETON                                        4390
029200901115     C                   GOTO      ENDCTR
029300901115     C                   END
029400901115     C     SEL1          IFNE      0
029500921021     C     SEL2          ANDNE     0
029600901115     C                   SETON                                        4390
029700901115     C                   GOTO      ENDCTR
029800901115     C                   END
029900901115     C* DATA SPEDIZIONE
030000901115     C     SEL1          IFNE      0
030100020801     C* P.O DI TRANSITO
030200020801     C
030300020801     C                   MOVEL     *blanks       V1DPOT
030400020801     C     V1CPOT        IFNE      0
030500020801     C     V1CPOT        CHAIN     AZORG01L                           30
030600020801     C                   IF        *in30 =*on or Orgfag<>'F'
030700020801     C                             and OrgFag <> 'A' or OrgFva <> ' '
030800020801     C                   SETON                                        4690
030900020801     C                   GOTO      ENDCTR
031000020801     C                   ELSE
031100020801     C                   MOVEL     ORGDES        V1DPOT
031200020801     C                   SETON                                        06
031300020801     C                   ENDIF
031400020801     C                   ENDIF
031500901115     C* CHIEDO O LA LINEA PARTENZA O LA LINEA ARRIVO
031600940721     C     V1CLNA        IFEQ      0
031700940721     C     V1CLP1        IFEQ      0
031800901115     C                   SETON                                        4090
031900901115     C                   GOTO      ENDCTR
032000901115     C                   END
032100901115     C                   END
032200901115     C* DATA SPEDIZIONE DAL OBBLIGATORIA
032300940721     C     V1CDSD        IFEQ      0
032400901115     C                   SETON                                        5190
032500901115     C                   GOTO      ENDCTR
032600901115     C                   END
032700900514     C* LINEA ARRIVO
032800940721     C     V1CLNA        IFNE      0
032900940721     C     V1CLNA        CHAIN     AZORG01L                           30
033000900514     C   30              SETON                                        4190
033100900509     C   30              GOTO      ENDCTR
033200900514     C                   SETON                                        01
033300020822     C   01              MOVEL     ORGDES        VCCLIN
033400901115     C                   END
033500901115     C* LINEA PARTENZA
033600940721     C     V1CLP1        IFNE      0
033700940721     C     V1CLP1        CHAIN     AZORG01L                           30
033800920416     C   30              SETON                                        4290
033900901115     C   30              GOTO      ENDCTR
034000901115     C                   SETON                                        02
034100020822     C   02              MOVEL     ORGDES        VCCLIN
034200901115     C                   END
034300901115     C* DATA SPEDIZIONE DAL
034400940721     C                   MOVE      V1CDSD        G02DAT
034500920206     C                   MOVEL     *BLANK        G02ERR
034600940722     C                   CALL      'XSRDA8'
034700920206     C                   PARM                    WLBDAT
034800920206     C     G02ERR        IFEQ      '1'
034900900515     C                   SETON                                        5190
035000900515     C                   GOTO      ENDCTR
035100900515     C                   ELSE
035200940722     C                   MOVEL     G02DAT        V1CDSD
035300940722     C                   MOVE      G02INV        COMDSD            8 0
035400940722     C                   MOVEL     G02INV        COMAAD            4 0
035500920206     C                   MOVE      G02INV        COMMGD            4 0
035600900515     C                   SETON                                        16
035700900515     C                   END
035800900515     C* DATA SPEDIZIONE AL
035900940721     C     V1CDSA        IFNE      0
036000940721     C                   MOVE      V1CDSA        G02DAT
036100920206     C                   MOVEL     *BLANK        G02ERR
036200940722     C                   CALL      'XSRDA8'
036300920206     C                   PARM                    WLBDAT
036400920206     C     G02ERR        IFEQ      '1'
036500900515     C                   SETON                                        5290
036600900515     C                   GOTO      ENDCTR
036700900515     C                   ELSE
036800940722     C                   MOVEL     G02DAT        V1CDSA
036900940722     C                   MOVE      G02INV        COMDSA            8 0
037000900515     C                   END
037100900515     C* DATA SPEDIZIONE AL MAGGIORE DATA SPEDIZIONE AL
037200900515     C     COMDSA        IFLT      COMDSD
037300900515     C                   SETON                                        5490
037400900515     C                   GOTO      ENDCTR
037500900515     C                   END
037600900515     C                   SETON                                        15
037700900515     C                   END
037800901115     C***
037900901115     C* TIPO SPEDIZIONE ESPRESSO
038000940722     C                   MOVEL     *BLANKS       V1DTSP                         DECODIFICA
038100940721     C     V1CTSP        IFNE      ' '
038200901115     C                   SETON                                        04
038300940722     C     V1CTSP        IFEQ      '?'
038400940722     C                   MOVEL     CODUT         §KUT
038500940722     C                   MOVEL     '5E'          KCOD
038600940722     C                   CALL      'X§TABER'
038700940722     C                   PARM                    §KUT              1 0
038800940722     C                   PARM                    KCOD
038900940722     C                   PARM                    KKEY
039000940722     C                   PARM                    §DES             30
039100940722     C                   MOVEL     KKEY          V1CTSP
039200940722     C                   SETON                                        90
039300940722     C                   MOVEL     'S'           WRIEM                          RIEMETTO TESTATA
039400940722     C                   ENDIF
039500940722     C*
039600090604     c                   clear                   ds5e
039700940722     C                   MOVEL     '5E'          KCOD
039800940722     C                   MOVEL     *BLANKS       KKEY
039900940722     C                   MOVEL     V1CTSP        KKEY
040000940722     C     KTAB1         CHAIN     TABEL00F                           47
040100090604     C  N47              MOVEL     TBLUNI        ds5e                           DECODIFICA
040200090604     C  N47              MOVEL     §5ed08        V1DTSP                         DECODIFICA
040300940722     C   47              SETON                                        90
040400940722     C   47              GOTO      ENDCTR
040500901115     C                   END
040600901115     C* NUMERO SERIE AL DEVE ESSERE > NUMERO SERIE DAL
040700901115     C*  SE IMMESSO
040800940721     C     V1CPSD        IFGT      V1CPSA
040900901115     C                   SETON                                        4890
041000901115     C                   GOTO      ENDCTR
041100901115     C                   END
041200901115     C* NUMERO SEGNACOLLO AL DEVE ESSERE > NUMERO SEGNACOLLO DAL
041300940721     C     V1CSCA        IFGT      0
041400940721     C     V1CSCD        IFGT      V1CSCA
041500901115     C                   SETON                                        4990
041600901115     C                   GOTO      ENDCTR
041700901115     C                   END
041800901115     C                   END
041900901115     C* SE IMMESSO AL IMMETTERE ANCHE DAL
042000940721     C     V1CSCA        IFGT      0
042100940721     C     V1CSCD        IFEQ      0
042200901115     C                   SETON                                        5790
042300901115     C                   GOTO      ENDCTR
042400901115     C                   END
042500901115     C                   END
042600901115     C* SE IMMESSA RAGIONE SOCIALE CERCO PRIMO BLANKS
042700940721     C     V1CRSM        IFNE      *BLANKS
042800901115     C                   SETON                                        11
042900940721     C                   MOVEA     V1CRSM        C1
043000901116     C                   Z-ADD     25            X                 2 0
043100901116     C     C1(X)         DOWEQ     *BLANKS
043200901116     C                   SUB       1             X
043300901116     C                   END
043400901116     C     X             ADD       1             XX                2 0
043500901116     C                   END
043600920413     C                   END
043700020821     C*
043800020822     C     SEL2          IFNE      0
043900020821     C     V1CPOT        IFNE      0
044000020821      *
044100020821     C** NUMERO SPEDIZIONE
044200020821     C** LINEA PARTENZA
044300020821     C** LINEA DI TRANSITO
044400020821     C** Aggancio direttamente la testata file transiti
044500020821     C     V1CLP2        CHAIN     AZORG01L                           30
044600020821     C   30              SETON                                        4590
044700020821     C   30              GOTO      ENDCTR
044800020821     C* NUMERO SPEDIZIONE OBBLIGATOIRO SE IMMESSA LNP
044900020821     C                   SETOFF                                         61
045000020821     C     KLNP4         CHAIN     FNBTP11L                           61
045100020821     C   61              SETON                                        9044
045200020821     C   61              GOTO      ENDCTR
045300020822      * segnalo che posso emettere direttamente la bolla
045400020823     C                   z-add     1             $Bolla
045500020821     C*
045600020821     C                   ELSE
045700020822     C                   SETON                                        12
045800020821     C*
045900020821     C** NUMERO SPEDIZIONE
046000020821     C** LINEA PARTENZA
046100020821     C     V1CLP2        CHAIN     AZORG01L                           30
046200020821     C   30              SETON                                        4590
046300020821     C   30              GOTO      ENDCTR
046400020822     C   12              MOVEL     ORGDES        VCCLIN
046500020821     C* NUMERO SPEDIZIONE OBBLIGATOIRO SE IMMESSA LNP
046600020821     C                   SETOFF                                         61
046700020823     C                   z-add     0             $Bolla            2 0
046800020821     C     KLNP          SETLL     FNBTP01L
046900020821     C     KLNP          READE     FNBTP01L                               61
047000020821     C     *in61         DOWEQ     *off
047100020822      * vedo se c'è piu' di una bolla con questa chiave
047200020821     C                   add       1             $Bolla
047300020821     C     KLNP          READE     FNBTP01L                               61
047400020821     C                   ENDDO
047500020822     C                   IF        $Bolla = 0
047600020822     C                   SETON                                        9044
047700020822     C                   GOTO      ENDCTR
047800020822     C                   ENDIF
047900020821     C*
048000020821     C                   END
048100020821     C                   END
048200921021     C* 21 ON RICHIESTA SIA LNA  CHE LNP
048300921021     C   01
048400921021     CAN 02              DO
048500921021     C                   SETOFF                                       02
048600921021     C                   SETON                                        21
048700921021     C                   END
048800900514     C*
048900900514     C     ENDCTR        ENDSR
049000940722     C**************************************************************************
049100940722     C*    ELABORA BOLLE DA VISUALIZZARE
049200940722     C**************************************************************************
049300900515     C     ELAB          BEGSR
049400940722     C*
049500920416     C                   Z-ADD     0             COMOD
049600901115     C* NUMERO DI SPEDIZIONE
049700020822     C     SEL2          IFNE      0
049800020821     C     V1CPOT        IFNE      0
049900020821     C     $Bolla        oreq      1
050000940722     C                   MOVEL     BTPLNP        PARLNP
050100940722     C                   MOVEL     BTPNRS        VSCNRS
050200940722     C                   MOVEL     BTPNSP        VSCNSP
050300940722     C                   MOVEL     BTPAAS        VSCAAS
050400020823     C                   MOVEL     BTPFLP        VHCPOT
050500920413     C*
050600920413     C                   EXSR      BOLLA
050700901115     C*
050800901115     C                   END
050900020821     C                   END
051000901115     C*
051100901115     C* DATA SPEDIZIONE
051200020822     C* OPPURE PER SPEDIZIONE MA PRESENTI PIU' BOLLE
051300901115     C     SEL1          IFNE      0
051400020821     C     $Bolla        orgt      1
051500940721     C                   Z-ADD     1             REC2
051600900515     C*** FILE STORICO
051700940721     C     V1CFTA        COMP      '1'                                    17
051800940721     C     V1CFTA        COMP      '2'                                    18
051900940721     C     V1CFTA        COMP      ' '                                    19
052000900517     C*
052100900515     C* TIPO SPEDIZIONE "N" ==> BLANK
052200901116     C                   MOVEL     *BLANKS       TSP
052300940721     C   04V1CTSP        IFEQ      'N'
052400900515     C                   MOVEL     *BLANKS       TSP               1
052500900515     C                   ELSE
052600940721     C                   MOVEL     V1CTSP        TSP               1
052700900515     C                   END
052800020801     C*
052900020801     C* ESEGUO UN LOOP ESTERNO PER POSIZIONARMI SUL CORRETTO P.O TRANSITO
053000020801     C                   IF        V1CPOT = *zeros
053100020801     C   01*loval        setll     FNBTP26L
053200020801     C   01              read      FNBTP26L                               33
053300020801     C   02*loval        setll     FNBTP31L
053400020801     C   02              read      FNBTP31L                               33
053500021111     C  n33              z-add     BtpFlp        KeyFlp            3 0
053600020822      *
053700020822     C   12KLNP          setll     FNBTP01L
053800020801     C                   ELSE
053900021111     c                   eval      keyflp=v1cpot
054000020801     C                   ENDIF
054100940722     C*
054200900515     C* LETTURA PER LNA DATA SPEDIZ. TIPO SPEDIZIONE
054300901116     C   01              DO
054400940722     C   16KLNA1         SETLL     FNBTP26L
054500900515     C* LETTURA SOLO PER LNA
054600020801     C  N16KLNA2         SETLL     FNBTP26L
054700901116     C                   END
054800940722     C*
054900901116     C* LETTURA PER LNP DATA SPEDIZ. SERIE
055000940722     C   02KLNP2         SETLL     FNBTP31L
055100920416     C*
055200920416     C                   EXSR      CARSF
055300920416     C                   END
055400920416     C*
055500901116     C                   ENDSR
055600940722     C**************************************************************************
055700940722     C*    CARICO SUBFILE IN AVANTI
055800940722     C**************************************************************************
055900900515     C     CARSF         BEGSR
056000900517     C*
056100940721     C                   Z-ADD     COMOD         NRR2
056200021111     C                   Z-ADD     1             B                 2 0
056300021111    1C     B             DOWLE     19
056400021111     c     *in30         ANDEQ     *OFF
056500021111     C   01KLna2         READE     FNBTP26L                               33
056600021111     C   02Klnp3         READE     FNBTP31L                               33
056700021111     C   12KLNP          reade     FNBTP01L                               33
056800021111     c
056900021111    2c                   if        not *in33
057000940722     C*
057100021111     c                   setoff                                       32
057200021111     C  N12              EXSR      CONFR
057300021111     C  N32              EXSR      EMISS
057400021111   x2c                   else
057500021111     C
057600021111    3c                   if        NOT *IN12 AND V1CPOT=0
057700021111     C   01KeyFlp        SETGT     FNBTP26L
057800021111     C   01              read      FNBTP26L                               30
057900021111     C   02KeyFlp        SETGT     FNBTP31L
058000021111     C   02              read      FNBTP31L                               30
058100021111     C
058200021111     C* SE TROVATO NUOVO P.O. transito, mi riposizione prima di leggere
058300021111    4C                   IF        NOT *IN30
058400021111     C                   z-add     BtpFlp        KeyFlp            3 0
058500021111     C* LETTURA PER LNA DATA SPEDIZ. TIPO SPEDIZIONE
058600021111    5C   01              DO
058700021111     C   16KLNA1         SETLL     FNBTP26L
058800021111     C* LETTURA SOLO PER LNA
058900021111     C  N16KLNA2         SETLL     FNBTP26L
059000021111    5C                   END
059100021111     C*
059200021111     C* LETTURA PER LNP DATA SPEDIZ. SERIE
059300021111     C   02KLNP2         SETLL     FNBTP31L
059400021111    4C                   ENDIF
059500021111     c
059600021111   x3c                   else
059700021111     c                   seton                                        30
059800021111    3C                   ENDIF
059900021111    2C                   ENDIF
060000020822      *
060100021111    1C                   ENDDO
060200021111     c
060300940721     C                   Z-ADD     NRR2          COMOD             4 0
060400021114      * imposto linea di arrivo del control se sono nel caso di
060500021114      * selezioni per num. spedizione bolla
060600021113     C   12              move      v1clp2        v1clpa
060700021113     C  N12              clear                   v1clpa
060800021113      *
060900020823     C                   move      *zeros        SEL2
061000900516     C*
061100900515     C                   ENDSR
061200940722     C**************************************************************************
061300940722     C*    CONFRONTO PER CERCARE BOLLE DA EMETTERE
061400940722     C**************************************************************************
061500900515     C     CONFR         BEGSR
061600940722     C*
061700900518     C                   SETOFF                                       32
061800900515     C** CONFRONTI SOLO PER LINEA ARRIVO
061900900515     C*  DATA SPEDIZIONE > DATA AL
062000940721     C   16
062100940721     CAN 15BTPDSP        IFGT      COMDSA
062200021113     C                   SETON                                          3032
062300900515     C                   GOTO      ENDCON
062400900515     C                   END
062500940721     C   16
062600940721     CANN15BTPDSP        IFGT      COMDSD
062700021113     C                   SETON                                          3032
062800900518     C                   GOTO      ENDCON
062900900518     C                   END
063000920414     C*
063100920414     C* CONFRONTO TIPO SELEZIONE
063200920414     C* SOLO USCITE DA TRANSITO
063300940722     C   17BTPDUT        IFEQ      0
063400920414     C                   SETON                                        32
063500920414     C                   GOTO      ENDCON
063600920414     C                   END
063700920414     C* SOLO ANCORA DA USCIRE DAL TRANSITO
063800940722     C   18BTPDUT        IFNE      0
063900920414     C                   SETON                                        32
064000920414     C                   GOTO      ENDCON
064100920414     C                   END
064200921021     C* LINEA DI PARTENZA
064300940722     C   21BTPLNP        IFNE      V1CLP1
064400921021     C                   SETON                                        32
064500921021     C                   GOTO      ENDCON
064600921021     C                   END
064700900515     C* TIPO SPEDIZIONE
064800940722     C   04BTPTSP        IFNE      TSP
064900900515     C                   SETON                                            32
065000900515     C                   GOTO      ENDCON
065100900515     C                   END
065200900518     C* SERIE DAL + AL
065300940722     C     BTPNRS        IFLT      V1CPSD
065400900518     C                   SETON                                            32
065500900518     C                   GOTO      ENDCON
065600900518     C                   END
065700940722     C     BTPNRS        IFGT      V1CPSA
065800900515     C                   SETON                                            32
065900900515     C                   GOTO      ENDCON
066000900515     C                   END
066100900518     C* SEGNACOLLO DAL SOLO
066200940722     C   07
066300940722     CANN08KBTP2         SETLL     FNBTT01L                               31
066400910827     C   07
066500910827     CANN08
066600910827     CANN31              SETON                                        32
066700910827     C   07
066800910827     CANN08
066900910827     CANN31              GOTO      ENDCON
067000900515     C* SEGNACOLLO DAL + AL
067100910827     C   08              DO
067200910827     C                   SETON                                        32
067300940722     C     KBTP          SETLL     FNBTT01L
067400940722     C     KBTP          READE     FNBTT01L                               31
067500910827     C     *IN31         DOWEQ     '0'
067600940722     C     BTTNSC        IFGE      V1CSCD
067700940722     C     BTTNSC        ANDLE     V1CSCA
067800910827     C                   SETON                                        31
067900910827     C                   SETOFF                                       32
068000910827     C                   END
068100910827     C*
068200940722     C  N31KBTP          READE     FNBTT01L                               31
068300910827     C                   END
068400910827     C*
068500910827     C   32              GOTO      ENDCON
068600910827     C                   END
068700900515     C* RAGIONE SOCIALE MITTENTE
068800900515     C   11              DO
068900940722     C                   MOVEA     BTPRSM        C1
069000900515     C*
069100900515     C     XX            DO        25            X
069200900515     C                   MOVE      *BLANKS       C1(X)
069300900515     C                   END
069400900515     C                   MOVEA     C1            COMRAG           25
069500900515     C*
069600940721     C     COMRAG        IFNE      V1CRSM
069700900515     C                   SETON                                            32
069800900515     C                   GOTO      ENDCON
069900900515     C                   END
070000900515     C                   END
070100940722     C*
070200900515     C     ENDCON        ENDSR
070300940722     C**************************************************************************
070400940722     C* EMISSIONE SUBFILE
070500940722     C**************************************************************************
070600900515     C     EMISS         BEGSR
070700940722     C*
070800940721     C                   MOVEL     *BLANKS       VSCSCE
070900940722     C   01              MOVEL     BTPLNP        VSCLIN
071000940722     C   02              MOVEL     BTPLNA        VSCLIN
071100020823     C   12              MOVEL     BTPLNA        VSCLIN
071200940722     C                   MOVE      BTPAAS        VSCAAS
071300020823     C                   MOVEL     BTPFLP        VHCPOT
071400920206     C*
071500940722     C                   MOVEL     BTPMGS        MMGG
071600940721     C                   MOVEL     GG            VSCDSP                          DATA SPEDIZ
071700940721     C                   MOVE      MM            VSCDSP
071800920206     C*
071900941219     C                   MOVE      BTPDET        G02INV                          AAAA/MM/GG
072000920413     C                   MOVE      *ZEROS        G02DAT
072100920413     C                   MOVEL     3             G02ERR
072200940722     C                   CALL      'XSRDA8'
072300920413     C                   PARM                    WLBDAT
072400941219     C                   MOVEL     G02DAT        VSCDET                          GG/MM/AA (6)
072500941219     C                   MOVE      G02DAT        DUE               2 0           ANNO
072600941219     C                   MOVE      DUE           VSCDET
072700920413     C*
072800941219     C                   MOVE      BTPDUT        G02INV                          AAAA/MM/GG
072900920413     C                   MOVE      *ZEROS        G02DAT
073000920413     C                   MOVEL     3             G02ERR
073100940722     C                   CALL      'XSRDA8'
073200920413     C                   PARM                    WLBDAT
073300941219     C                   MOVEL     G02DAT        VSCDUT                          GG/MM/AA (6)
073400941219     C                   MOVE      G02DAT        DUE                             ANNO
073500941219     C                   MOVE      DUE           VSCDUT
073600900516     C*
073700940722     C                   MOVEL     BTPNSP        VSCNSP
073800940722     C                   MOVEL     BTPRSD        VSCRSD
073900940722     C                   MOVEL     BTPRSM        VSCMIT
074000940722     C                   MOVEL     BTPNRS        VSCNRS
074100940722     C     BTPTSP        IFNE      'N'                                          'N'=NORMALE
074200940722     C                   MOVEL     BTPTSP        VSCTSP                         TIPO SERVIZIO BOLLE
074300940722     C                   ELSE
074400940722     C                   MOVEL     *BLANKS       VSCTSP
074500940722     C                   ENDIF
074600941219     C                   MOVEL     BTPFLS        VSCFLS                         FIL.SEGNACOLLO
074700920206     C* SEGNACOLLI
074800920206     C                   MOVEA     *BLANKS       SEG
074900940722     C                   MOVEL     BTPNCD        COMSCN            7
075000920206     C                   MOVEA     COMSCN        SEG(1)
075100940722     C     BTPNCA        IFNE      0
075200940722     C                   MOVEL     BTPNCA        COMSCN
075300920206     C                   MOVEA     COMSCN        SEG(9)
075400920206     C                   END
075500920206     C                   MOVEA     '/'           SEG(8)
075600920206     C* ELIMINO ZERO NON SIGNIFICATIVI
075700920206     C                   Z-ADD     1             X
075800920206     C     SEG(X)        DOWEQ     '0'
075900920206     C                   MOVE      ' '           SEG(X)
076000920206     C                   ADD       1             X
076100920206     C                   END
076200920206     C*
076300940722     C     BTPNCA        IFNE      0
076400920206     C                   Z-ADD     9             X
076500920206     C     SEG(X)        DOWEQ     '0'
076600920206     C                   MOVE      ' '           SEG(X)
076700920206     C                   ADD       1             X
076800920206     C                   END
076900920206     C                   END
077000940721     C                   MOVEA     SEG           VSCSEG
077100900522     C*
077200900518     C                   ADD       1             B
077300940721     C                   ADD       1             NRR2              4 0
077400940721     C                   WRITE     LR68S02
077500940722     C*
077600900515     C                   ENDSR
077700940722     C**************************************************************************
077800940722     C* PULIZIA SUBFILE
077900940722     C**************************************************************************
078000900515     C     PULSF         BEGSR
078100940722     C*
078200900515     C                   SETON                                        35
078300940721     C                   WRITE     LR68C02
078400900515     C                   SETOFF                                       35
078500940722     C*
078600900515     C                   ENDSR
078700940722     C**************************************************************************
078800940722     C* CARICO DATI BOLLA
078900940722     C**************************************************************************
079000920413     C     BOLLA         BEGSR
079100940722     C*
079200020823      * Aggancio la singola bolla usando la chiave con P.O di transito
079300020823     C     KBTP4         CHAIN     FNBTP11L                           31
079400020823     C***  KBTP3         CHAIN     FNBTP01L                           31
079500920413     C* CODICE BOLLA
079600940722     C                   MOVEL     '3A'          KCOD
079700940722     C                   MOVEL     *BLANKS       KKEY
079800940722     C                   MOVEL     BTPCBO        KKEY
079900940722     C     KTAB1         CHAIN     TABEL00F                           31
080000920413     C                   MOVEL     TBLUNI        DS3A
080100920413     C                   MOVEL     §3ATB1        FLGTB1            1
080200940721     C                   MOVEL     §3ADES        V3DCBO
080300920413     C     FLGTB1        IFEQ      'F'
080400920413     C                   SETON                                        03
080500920413     C                   END
080600930115     C*
080700920413     C* DECODIFICA LINEA DI ARRIVO
080800940721     C     BTPLNA        CHAIN     AZORG01L                           35
080900940727     C  N35              MOVEL     ORGDES        PD8LNA
081000940727     C   35              MOVEL     *BLANKS       PD8LNA
081100940727     C                   MOVEL     PD8LNA        V3DLNA
081200021108     C* DECODIFICA LINEA DI PARTENZA
081300940721     C     BTPLNP        CHAIN     AZORG01L                           35
081400940721     C  N35              MOVEL     ORGDES        V3DLNP
081500940721     C   35              MOVEL     *BLANKS       V3DLNP
081600940722     C*
081700920413     C* NON SEQUENZIALI
081800940721     C     BTPFNS        IFEQ      'S'
081900940721     C                   MOVEL     SEQ(1)        V3DFNS
082000920413     C                   ELSE
082100940721     C                   MOVEL     *BLANKS       V3DFNS
082200920413     C                   END
082300920413     C*
082400940721     C* DECODIFICA TIPO SERVIZIO BOLLE (ESPRESSO, NORMALE, ...)
082500940721     C                   MOVEL     *BLANKS       V3DTSP
082600940721     C                   MOVEL     '5E'          KCOD
082700940721     C                   MOVEL     *BLANKS       KKEY
082800940721     C                   MOVEL     BTPTSP        KKEY
082900940722     C     KTAB1         CHAIN     TABEL00F                           31
083000090604     C  N31              MOVEL     TBLUNI        ds5e                           DECOD.TIPO SERVIZIO
083100090604     C  N31              MOVEL     §5ed08        V3DTSP                         DECOD.TIPO SERVIZIO
083200920414     C*
083300940721     C                   MOVEL     BTPNRS        V3CNRS                         NUMERO SERIE
083400941219     C                   MOVEL     BTPNRS        V3CSER                         NUMERO SERIE
083500940721     C                   MOVEL     BTPNSP        V3CNSP                         NUMERO SPEDIZIONE
083600091027     c
083700940721     C                   MOVEL     BTPNFV        V3CNFV                         NUMERO FOGLIO VIAGGI
083800091027     C                   MOVEL     BTPfle        V3Cfle                         lnp partenza NFV
083900091027     c* Prendo la data del foglio da fnfgv
084000091027     c     kfgv          chain     fnfgv01l
084100091027     c                   if        %found(fnfgv01l)
084200091027     C                   MOVEL     fgvdfv        G02INV
084300091027     C                   Z-ADD     *ZEROS        G02DAT
084400091027     C                   MOVEL     '3'           G02ERR
084500091027     C                   CALL      'XSRDA8'
084600091027     C                   PARM                    WLBDAT
084700091027     C                   MOVEL     G02DAT        V3Cdfv
084800091027     c                   else
084900091027     c                   clear                   v3cdfv
085000091027     c                   endif
085100091027     c
085200940721     C                   MOVEL     BTPLNP        V3CLNP                         LINEA PARTENZA
085300941109     C                   MOVEL     BTPLNP        VIDLNP                         LINEA PARTENZA
085400940721     C                   MOVEL     BTPLNA        V3CLNA                         LINEA ARRIVO
085500940721     C                   MOVEL     BTPCBO        V3CCBO                         CODICE BOLLA
085600940721     C                   MOVEL     BTPZNC        V3CZNC                         ZONA
085700940721     C                   MOVEL     BTPRSM        V3CRSM                         RAG.SOC.MITTENTE
085800940727     C                   MOVEL     BTPRSM        PA8RSM                         RAG.SOC.MITTENTE
085900940721     C                   MOVEL     BTPRSD        V3CRSD                         RAG.SOC.DESTINATARIO
086000940727     C                   MOVEL     BTPRSD        PA8RSD                         RAG.SOC.DESTINATARIO
086100940721     C                   MOVEL     BTPNCL        V3CNCL                         NUMERO COLLI
086200940721     C                   MOVEL     BTPPKB        V3CPKB                         PESO BOLLETTATO
086300990907     C                   MOVEL     BTPPKC        V3CPKC                         PESO CML
086400940721     C                   MOVEL     BTPFVB        V3CFVB                         FLAG VOLUME BOLLETTA
086500940721     C                   MOVEL     BTPVLB        V3CVLB                         VOLUME BOLLETTATO
086600940721     C                   MOVEL     BTPVLC        V3CVLC                         VOLUME CML
086700950120     C     BTPNCR        IFNE      99999
086800950120     C                   MOVEL     BTPNCR        V3CNCR                         COLLI RILEVATI DA CM
086900950120     C                   ELSE
087000950120     C                   CLEAR                   V3CNCR
087100950120     C                   ENDIF
087200990907     C     BTPNCP        IFNE      99999
087300990907     C                   MOVEL     BTPNCP        V3CNCP                         COLLI RILEVATI DA CM
087400990907     C                   ELSE
087500990907     C                   CLEAR                   V3CNCP
087600990907     C                   ENDIF
087700941219     C                   MOVEL     BTPFLS        V3CFLS                         FIL.SEGNACOLLO
087800940721     C                   MOVEL     BTPNCD        V3CNCD                         NR.SEGNACOLLO DA
087900940721     C                   MOVEL     BTPNCA        V3CNCA                         NR.SEGNACOLLO A
088000920413     C* GIRO DATA SPEDIZIONE
088100940721     C                   MOVEL     BTPDSP        G02INV
088200940721     C                   Z-ADD     *ZEROS        G02DAT
088300940721     C                   MOVEL     '3'           G02ERR
088400940721     C                   CALL      'XSRDA8'
088500940721     C                   PARM                    WLBDAT
088600940721     C                   MOVEL     G02DAT        V3CDSP
088700920413     C* DATA BORDERO'
088800940721     C                   MOVEL     BTPDBR        G02INV
088900940721     C                   Z-ADD     *ZEROS        G02DAT
089000940721     C                   MOVEL     '3'           G02ERR
089100940721     C                   CALL      'XSRDA8'
089200940721     C                   PARM                    WLBDAT
089300940721     C                   MOVEL     G02DAT        V3CDBR
089400940721     C* DATA PARTENZA ULTIMO COLLO
089500940721     C                   MOVEL     BTPDPC        G02INV
089600940721     C                   Z-ADD     *ZEROS        G02DAT
089700940721     C                   MOVEL     '3'           G02ERR
089800940721     C                   CALL      'XSRDA8'
089900940721     C                   PARM                    WLBDAT
090000940721     C                   MOVEL     G02DAT        V3CDPC
090100940721     C* DATA PARTENZA ULTIMO COLLO
090200940721     C                   MOVEL     BTPDUC        G02INV
090300940721     C                   Z-ADD     *ZEROS        G02DAT
090400940721     C                   MOVEL     '3'           G02ERR
090500940721     C                   CALL      'XSRDA8'
090600940721     C                   PARM                    WLBDAT
090700940721     C                   MOVEL     G02DAT        V3CDUC
090800920413     C* DATA ENTRATA AL TRANSITO
090900940721     C                   MOVEL     BTPDET        G02INV
091000940721     C                   Z-ADD     *ZEROS        G02DAT
091100940721     C                   MOVEL     '3'           G02ERR
091200940721     C                   CALL      'XSRDA8'
091300940721     C                   PARM                    WLBDAT
091400940721     C                   MOVEL     G02DAT        V3CDET
091500920413     C* DATA USCITA DAL TRANSITO
091600940721     C                   MOVEL     BTPDUT        G02INV
091700940721     C                   Z-ADD     *ZEROS        G02DAT
091800940721     C                   MOVEL     '3'           G02ERR
091900940721     C                   CALL      'XSRDA8'
092000940721     C                   PARM                    WLBDAT
092100940721     C                   MOVEL     G02DAT        V3CDUT
092200940721     C* DATA ARRIVO MERCE A FILIALE ARRIVO
092300160517     c                   if        btpdam=0
092400160517     c     kbtp          chain     fnarb01l
092500160517     c                   if        %found(fnarb01l)
092600160517     c                   eval      btpdam=arbdam
092700160517     c                   endif
092800160517     c                   endif
092900160517     c
093000940721     C                   MOVEL     BTPDAM        G02INV
093100940721     C                   Z-ADD     *ZEROS        G02DAT
093200940721     C                   MOVEL     '3'           G02ERR
093300940721     C                   CALL      'XSRDA8'
093400940721     C                   PARM                    WLBDAT
093500940721     C                   MOVEL     G02DAT        V3CDAM
093600940721     C*
093700920413     C                   MOVEL     ' '           FLAG
093800020821     C* P.O TRANSITO PIU' DECODIFICA
093900020821     C                   MOVEL     BTPFLP        VIDPOT
094000020821     C     BTPFLP        CHAIN     AZORG01L                           35
094100020821     C  N35              MOVEL     ORGDES        PD8POT
094200020821     C   35              MOVEL     *BLANKS       PD8POT
094300020821     C                   MOVEL     PD8POT        V3DPOT
094400040531     c* Verifico se collo di valore
094500040531     c     kar5          chain     fiar501l
094600040531     c                   if        %found(fiar501l)
094700040531     c                   movel     ar5uni        dar5gen
094800040531     c                   else
094900040531     c                   clear                   dar5gen
095000040531     c                   endif
095100040531     c                   eval      v3cbva=§ar5bva
095200040531     C*c
095300940721     C*  EMETTO IL FORMATO DI DETTAGLIO DELLA BOLLA TRANSITO
095400940721     C                   MOVEL     'S'           WRIEM                          SI RIEMETTO TESTATA
095500920414     C     FOR03         TAG
095600940721     C     WRIEM         IFEQ      'S'
095700940721     C                   WRITE     LR68T01                                      INTESTAZIONE VIDEO
095800940721     C                   MOVEL     *BLANKS       WRIEM             1            NO RIEMETTO TESTATA
095900940721     C                   ENDIF
096000940722     C                   EXFMT     LR68D03
096100940721     C*
096200940721     C* F3 = TORNO A SUBFILE
096300920413     C   KC              MOVEL     '2'           FLAG              1
096400920413     C   KC              GOTO      ENDBOL
096500940721     C* F12 = LEGGO ALTRA BOLLA
096600920413     C   KL              MOVEL     ' '           FLAG
096700920413     C   KL              GOTO      ENDBOL
096800940923     C* F19 = INTERROGAZIONME SPUNTE
096900940923     C   KT              DO
097000950112     C                   CLEAR                   PARA04
097100950116     C                   Z-ADD     V3CFLS        P04LNP
097200940923     C                   Z-ADD     V3CLNA        P04LNA
097300940923     C                   Z-ADD     V3CNRS        P04NRS
097400950116     C                   Z-ADD     V3CNCD        P04NCD
097500950116     C                   Z-ADD     V3CNCA        P04NCA
097600101202     C                   MOVE      V3CDSP        p04boan
097700101202     C                   MOVE      V3CLNP        p04bolp
097800101202     C                   MOVE      V3CNRS        p04bors
097900101202     C                   MOVE      V3CNSP        p04bosp
098000950116     C* LEGGO DAI SEGNACOLLI PERCHE' NON SEQUENZIALI
098100950116     C     V3CNCD        IFEQ      0
098200950116     C     V3CNRS        ANDGT     0
098300950116     C     KBTP          CHAIN     FNBTT01L                           30
098400950116     C*
098500950116     C     *IN30         IFEQ      *OFF
098600950116     C* SEGNACOLLO DAL
098700950116     C                   MOVEL     BTTNSC        P04NCD
098800950116     C* SEGNACOLLO AL
098900950116     C     KBTP          SETGT     FNBTT01L                           30
099000950116     C     KBTP          READPE    FNBTT01L                               30
099100950116     C  N30BTTNSC        IFNE      P04NCD
099200950116     C                   MOVEL     BTTNSC        P04NCA
099300950116     C                   ENDIF
099400950116     C                   ENDIF
099500950116     C                   ENDIF
099600940923     C                   Z-ADD     99            P04NPS
099700950112     C                   Z-ADD     9             P04NPG
099800940923     C                   MOVEL     '1'           P04FLG
099900950112     C                   MOVEL     'FNLR68R'     P04PGM
100000940923     C                   MOVEL     PARA04        KPJBU
100100940923     C                   CALL      'FNLR04R'
100200940923     C                   PARM                    KPJBA
100300940923     C                   MOVEL     'S'           WRIEM                          SI RIEMETTO TESTATA
100400940923     C                   GOTO      FOR03
100500940923     C                   ENDDO
100600940721     C* F21 = DETTAGLIO COLLI
100700940721     C   KV              DO
100800020821     C                   MOVE      VIDPOT        PA8POT
100900020821     C                   MOVE      V3CDSP        PA8AAS
101000940727     C                   MOVEL     V3CLNP        PA8LNP
101100940727     C                   MOVEL     V3CNRS        PA8NRS
101200940727     C                   MOVEL     V3CNSP        PA8NSP
101300940727     C                   MOVEL     V3CDSP        PA8DSP
101400940727     C                   MOVEL     V3CLNA        PA8LNA
101500940727     C                   MOVEL     V1CRSU        PA8DSA
101600940722     C                   MOVEL     *BLANKS       KPJBU
101700940727     C                   MOVEL     PARAA8        KPJBU
101800940722     C                   CALL      'FNLRA8R'
101900920416     C                   PARM                    KPJBA
102000940721     C                   MOVEL     'S'           WRIEM                          SI RIEMETTO TESTATA
102100940721     C                   GOTO      FOR03
102200940721     C                   ENDDO
102300940721     C* F7 = ANOMALIE
102400920423     C   KG              DO
102500960523     C                   CLEAR                   DSLR44
102600960523     C                   MOVE      V3CDSP        D44AAS
102700960523     C                   MOVE      V3CLNP        D44FIL
102800960523     C                   MOVE      V3CNRS        D44NRS
102900960523     C                   MOVE      V3CNSP        D44NSP
103000960523     C                   MOVEL     'T'           D44TBO
103100960523     C                   MOVE      'I'           D44MOD
103200960523     C                   MOVEL     DSLR44        KPJBU
103300960523     C                   CALL      'FNLR44R'
103400960523     C                   PARM                    KPJBA
103500940721     C                   MOVEL     'S'           WRIEM                          SI RIEMETTO TESTATA
103600920423     C                   GOTO      FOR03
103700940721     C                   ENDDO
103800920413     C*
103900920413     C     ENDBOL        ENDSR
104000940721     C**************************************************************************
104100940721     C*    R O U T I N E    I N I Z I A L E
104200940721     C**************************************************************************
104300940721     C     *INZSR        BEGSR
104400940721     C*
104500940721     C     KLNA1         KLIST
104600020801     C                   KFLD                    KeyFlp
104700940721     C                   KFLD                    V1CLNA
104800940721     C                   KFLD                    COMAAD
104900940721     C                   KFLD                    COMMGD
105000940721     C                   KFLD                    TSP               1
105100020801     C     KLNA2         KLIST
105200020801     C                   KFLD                    KeyFlp
105300020801     C                   KFLD                    V1CLNA
105400940721     C     KLNP          KLIST
105500940721     C                   KFLD                    V1CAAS
105600940721     C                   KFLD                    V1CLP2
105700940721     C                   KFLD                    V1CNRS
105800940721     C                   KFLD                    V1CNSP
105900020821     C     KLNP4         KLIST
106000020821     C                   KFLD                    V1CPOT
106100020821     C                   KFLD                    V1CAAS
106200020821     C                   KFLD                    V1CLP2
106300020821     C                   KFLD                    V1CNRS
106400020821     C                   KFLD                    V1CNSP
106500940722     C     KBTP          KLIST
106600940722     C                   KFLD                    BTPAAS
106700940722     C                   KFLD                    BTPLNP
106800940722     C                   KFLD                    BTPNRS
106900940722     C                   KFLD                    BTPNSP
107000940722     C     KBTP2         KLIST
107100940722     C                   KFLD                    BTPAAS
107200940722     C                   KFLD                    BTPLNP
107300940722     C                   KFLD                    BTPNRS
107400940722     C                   KFLD                    BTPNSP
107500940721     C                   KFLD                    V1CSCD
107600020823     C**   KBTP3         KLIST
107700020823     C**                 KFLD                    VSCAAS
107800020823     C**                 KFLD                    PARLNP            3 0
107900020823     C**                 KFLD                    VSCNRS
108000020823     C**                 KFLD                    VSCNSP
108100020822     C     KBTP4         KLIST
108200020823     C                   KFLD                    VHCPOT
108300020822     C                   KFLD                    VSCAAS
108400020823     C                   KFLD                    PARLNP            3 0
108500020822     C                   KFLD                    VSCNRS
108600020822     C                   KFLD                    VSCNSP
108700940721     C     KLNP2         KLIST
108800020801     C                   KFLD                    KeyFlp
108900940721     C                   KFLD                    V1CLP1
109000940721     C                   KFLD                    COMAAD
109100940721     C                   KFLD                    COMMGD
109200940721     C                   KFLD                    V1CPSD
109300020801     C     KLNP3         KLIST
109400020801     C                   KFLD                    KeyFlp
109500020801     C                   KFLD                    V1CLP1
109600940721     C     KTAB1         KLIST                                                  TABEL00F
109700940722     C                   KFLD                    CODUT                          ..CODICE UTENTE
109800940721     C                   KFLD                    KCOD                           ..CODICE TABELLA
109900940721     C                   KFLD                    KKEY                           ..CODICE ELEMENTO
110000040531     C     Kar5          KLIST
110100040531     C                   KFLD                    BTPAAS
110200040531     C                   KFLD                    BTPLNP
110300040531     C                   KFLD                    BTPNRS
110400040531     C                   KFLD                    BTPNSP
110500040531     C                   KFLD                    ktrd
110600091027     C     KFGV          KLIST
110700091027     C                   KFLD                    BTPnfv
110800091027     C                   KFLD                    BTPfle
110900940721     C*
111000940721     C     *LIKE         DEFINE    TBLCOD        KCOD                           CODICE TABELLA
111100940721     C     *LIKE         DEFINE    TBLKEY        KKEY                           CODICE ELEMENTO
111200940721     C*
111300940722     C                   Z-ADD     1             CODUT                          CODICE UTENTE
111400940721     C*
111500940721     C                   ENDSR
111600940721     C**************************************************************************
111700920413**   SEQ
111800920413NON SEQUENZIALI
