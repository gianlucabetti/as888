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
001100020821     FFNBTP11L  IF   E           K DISK
001200020821     F                                     RENAME(FNBTP000:FNBTP011)
001300940722     FFNBTP26L  IF   E           K DISK
001400940722     F                                     RENAME(FNBTP000:FNBTP026)
001500940722     FFNBTP31L  IF   E           K DISK
001600940722     F                                     RENAME(FNBTP000:FNBTP031)
001700940722     FFNBTT01L  IF   E           K DISK
001800920413     FTABEL00F  IF   E           K DISK
001900040531     Ffiar501l  IF   E           K DISK
002000940721      *-------------------------------------------------------------------------
002100900515     D C1              S              1    DIM(25)                              COMODO
002200920206     D SEG             S              1    DIM(15)                              COMODO X SEGN.
002300920413     D SEQ             S             15    DIM(1) CTDATA PERRCD(1)              NON SEQUENZIALSUR
002400940721      *-------------------------------------------------------------------------
002500930111     D                 DS
002600940722     D  DATA                   1      8  0
002700940722     D  AA                     1      4  0
002800940722     D  MM                     5      6  0
002900940722     D  GG                     7      8  0
003000940722     D  MMGG                   5      8  0
003100920206     D WLBDAT          DS
003200940721     D  G02DAT                 1      8  0
003300940721     D  G02INV                 9     16  0
003400940721     D  G02ERR                17     17
003500940721     D  G02TGI                18     22  0
003600920423     D* RICHIAMO PGM DI INTERROGAZIONE BOLLE ARRIVI
003700920423     D PARAM2          DS
003800940722     D  PA2FLG                31     31
003900940722     D  PA2BOL                62     62
004000930115     D* RICHIAMO PGM INTERROGAZIONE ANOMALIE
004100960523     D DSLR44        E DS                  EXTNAME(FNLR44DS)
004200940722     D* RICHIAMO PGM INTERROGAZIONE DETTAGLIO COLLI
004300940727     D PARAA8          DS
004400940727     D  PA8AAS                14     17  0
004500940727     D  PA8LNP                18     20  0
004600940727     D  PA8NRS                21     22  0
004700940727     D  PA8NSP                23     29  0
004800940727     D  PA8DSP                30     37  0
004900940727     D  PA8LNA                38     40  0
005000940727     D  PD8LNA                41     60
005100940727     D  PA8RSD                61     95
005200940727     D  PA8RSM                96    130
005300940727     D  PA8DSA               131    151
005400940727     D  PA8CML               152    152
005500020821     D  PA8POT               153    155  0
005600020821     D  PD8POT               156    170
005700940923     D PARA04          DS
005800940923     D  P04NFV                 1      5  0
005900940923     D  P04LNP                 6      8  0
006000940923     D  P04LNA                 9     11  0
006100940923     D  P04NRS                12     13  0
006200940923     D  P04NCD                14     20  0
006300940923     D  P04NCA                21     27  0
006400940923     D  P04NPS                28     29  0
006500940923     D  P04NPG                30     30  0
006600940923     D  P04FLG                31     31
006700940923     D  P04PGM                32     38
006800950112     D  P04FGS                39     41  0
006900101202     d* bolla
007000101202     d  p04bolp               42     44  0
007100101202     d  p04bors               45     46  0
007200101202     d  p04bosp               47     53  0
007300101202     d  p04boan               54     57  0
007400101202     D  P04ERR                58     58
007500901116     D                 DS
007600940721     D  V1CLNA                 1      3  0
007700940721     D  V1CLP1                 4      6  0
007800940722     D  V1CDSD                 7     14  0
007900940722     D  V1CDSA                15     22  0
008000020822     D  SEL1                   1     22  0
008100020822      *
008200020822     D                 DS
008300020822     D  V1CLP2                 1      3  0
008400020822     D  V1CNRS                 4      5  0
008500020822     D  V1CNSP                 6     12  0
008600020822     D  SEL2                   1     12  0
008700020821      *
008800940721     D* DS PER COMPORRE LA DATA SPEDIZIONE
008900900515     D                 DS
009000940721     D  BTPAAS                 1      4  0
009100940721     D  BTPMGS                 5      8  0
009200940721     D  BTPDSP                 1      8  0
009300900517     D CA02DS          DS
009400900517     D  PRIMA                378    379B 0
009500090604     d ds5e          e ds
009600920413     D DS3A          E DS
009700920413     D CNCR80        E DS
009800900514     D KPJBA         E DS
009900040531     D Dar5gen       E DS
010000920224     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
010100040531     D ktrd            s                   like(ar5trd) inz('GEN')
010200900518     C****************************************************************
010300900518     C*  RIEPILOGO INDICATORI
010400900518     C***************************************************************
010500920423     C* 01-16 - INDOCANO QUALI CAMPI HO SCELTO A VIDEO
010600920413     C* 03    - ESPEDIZIONE IN FRANCO
010700920423     C* 05    - NON ABILITO COMANDI PER ANDARE IN ALTRE INTERROGAZIONI
010800900518     C* 17-19 - INDICANO IL TIPO DI ARCHIVIO(IN PART.;PART.;ENTRAM)SCEL
010900920203     C* 20    - DISABILITO CMD7 INTERROGAZIONE BOLLE ARRIVI
011000921021     C* 21    - RICHIESTA SIA LINEA DI PARTENZA CHE LINEA DI ARRIVO
011100930115     C* 22    - SIMFEL HA IL TRISROT EMETTO VOLUME DA CML
011200920413     C* 30    - FINE LETTURA FILE
011300900518     C* 31    - COMODO
011400900518     C* 32    - BOLLA LETTA DA SCARTARE
011500020801     C* 33    - IND. DEL LOOP DI LETTURA PER P.O DI TRANSITO
011600901123     C* 34    - LETTURA SUBFILE PER RECORD SCELTI
011700920413     C* 35-37 - PER SUBFILE
011800901123     C* 40-60 - ERRORI
011900900518     C* 90    - INDICA LA PRESENZA DI UN ERRORE
012000900518     C* 99    - ERRORE
012100900518     C*****************************************************************
012200000000     C     *ENTRY        PLIST
012300000000     C                   PARM                    KPJBA
012400920423     C                   MOVEL     KPJBU         PARAM2
012500920203     C*
012600920224     C                   CALL      'X§PARUT'
012700920224     C                   PARM                    UT§DSE
012800940722     C                   MOVEL     RAGUT         V1CRSU
012900920224     C                   MOVEL     REC80         CNCR80
013000940721     C*---------------------------------------------------------------*
013100920423     C* VEDO SE PGM RICHIAMAMTO NON ABILITO INTERROGAZIONI
013200940722     C     PA2BOL        IFEQ      'T'
013300920423     C                   SETON                                        05
013400920423     C                   END
013500900516     C* PULIZIA CAMPI CHE RIMANGONO INVARIATI IL SECONDO GIRO
013600021114     C                   move      simfel        V1CPOT
013700940721     C                   MOVEL     ' '           V1CFTA
013800940721     C                   MOVEL     *ZEROS        V1CLNA
013900940721     C                   MOVEL     *ZEROS        V1CLP1
014000020822     C                   MOVEL     *ZEROS        V1CLP2
014100940721     C                   MOVEL     *ZEROS        V1CDSD
014200940721     C                   MOVEL     *ZEROS        V1CDSA
014300940722     C                   Z-ADD     UYEAR         V1CAAS
014400940722     C     V1CAAS        IFGT      60
014500940722     C                   MOVEL     19            V1CAAS                         XX SECOLO
014600940722     C                   ELSE
014700940722     C                   MOVEL     20            V1CAAS                         XXI SECOLO
014800940722     C                   ENDIF
014900940722     C*
015000900509     C     INIZIO        TAG
015100900514     C                   EXSR      PUL01
015200940722     C                   MOVEL     'S'           WRIEM                          SI RIEMETTO TESTATA
015300900509     C     FOR01         TAG
015400940722     C     WRIEM         IFEQ      'S'
015500940722     C                   WRITE     LR68T01                                      INTESTAZIONE VIDEO
015600940722     C                   MOVEL     *BLANKS       WRIEM             1            NO RIEMETTO TESTATA
015700940722     C                   ENDIF
015800940722     C                   EXFMT     LR68D01
015900920203     C** CMD3 - FINE LAVORO
016000900509     C   KC              GOTO      FINE
016100920203     C** CMD7  - INTERROGAZIONE BOLLE ARRIVI
016200920203     C   KG              DO
016300940722     C                   MOVEL     '4'           PA2FLG
016400940722     C                   MOVEL     *BLANKS       KPJBU
016500920203     C                   MOVEL     PARAM2        KPJBU
016600940722     C                   CALL      'FNLR36R'
016700920203     C                   PARM                    KPJBA
016800940722     C                   MOVEL     'S'           WRIEM                          SI RIEMETTO TESTATA
016900920203     C                   GOTO      FOR01
017000920203     C                   END
017100920414     C*
017200920414     C** CMD14 - INTERROGAZIONE BOLLE PARTENZE
017300920414     C   KN              DO
017400940722     C                   MOVEL     'A'           PA2FLG
017500940722     C                   MOVEL     *BLANKS       KPJBU
017600920414     C                   MOVEL     PARAM2        KPJBU
017700940722     C                   CALL      'FNLS04R'
017800920414     C                   PARM                    KPJBA
017900940722     C                   MOVEL     'S'           WRIEM                          SI RIEMETTO TESTATA
018000920414     C                   GOTO      FOR01
018100920414     C                   END
018200900509     C* CONTROLLI FORMATO1
018300900509     C                   EXSR      CONTR
018400900509     C   90              GOTO      FOR01
018500900511     C* ELABORAZIONE
018600900515     C                   EXSR      ELAB
018700901115     C*
018800901115     C* SE NUM SPE SECCO DOPO VISUALIZZAZIONE TORNO A VID. INIZIALE
018900901115     C     SEL2          IFNE      0
019000021115      * reimposto fra le prima selezioni simfel
019100021115     C                   move      simfel        V1CPOT
019200901115     C                   GOTO      INIZIO
019300901115     C                   END
019400901115     C* SE NON CI SONO BOLLE SEGNALO ERRORE
019500940721     C     NRR2          IFEQ      0
019600900516     C                   SETON                                        58
019700900516     C                   GOTO      FOR01
019800900516     C                   END
019900900515     C** CARICAMENTO SUBFILE
020000900518     C     FORCT2        TAG
020100020822      *
020200940722     C                   WRITE     LR68T01                                      INTESTAZIONE VIDEO
020300940722     C                   WRITE     LR68Z02                                      TASTI FUNZIONALI
020400900515     C** COMANDI SUBFILE
020500900515     C     FORCT         TAG
020600940721     C                   EXFMT     LR68C02
020700920203     C* CMD 3 - FINE
020800900515     C   KC              GOTO      FINE
020900900515     C* CMD12 - RITORNO
021000900517     C   KL              GOTO      INIZIO
021100900515     C* ROLLUP
021200940505     C     *IN37         IFEQ      *ON
021300940505     C   30              SETON                                        59
021400021111     C  n30              EXSR      CARSF
021500021111     C
021600940721     C                   Z-ADD     COMOD         REC2
021700940505     C                   GOTO      FORCT
021800940505     C                   ENDIF
021900900515     C*
022000900516     C** CONTROLLO SE EFFETTUATA UNA SCELTA
022100940721     C                   Z-ADD     PRIMA         REC2
022200940721     C                   READC     LR68S02                                34
022300900516     C   34              SETON                                        99
022400900516     C   34              GOTO      FORCT
022500900516     C* VISUALIZZO BOLLA X BOLLA
022600900516     C                   DO        *HIVAL
022700941109     C     VSCSCE        IFEQ      '5'
022800021114      * Linea di partenza
022900940721     C   01              MOVEL     VSCLIN        PARLNP
023000020822     C   02              MOVEL     V1CLP1        PARLNP
023100021113     C   12              MOVEL     V1CLPA        PARLNP
023200920413     C                   EXSR      BOLLA                                         CARICO BOLL
023300900516     C                   END
023400940721     C                   MOVEL     *BLANKS       VSCSCE
023500940721     C                   UPDATE    LR68S02
023600920203     C* SE FLAG 2 TORNO AL SUBFILE
023700920413     C     FLAG          IFEQ      '2'
023800900530     C                   SETON                                        34
023900920413     C                   ELSE
024000940721     C                   READC     LR68S02                                34
024100920413     C                   END
024200900530     C  N34              END
024300900518     C                   GOTO      FORCT2
024400940722     C*
024500000000     C     FINE          TAG
024600000000     C                   SETON                                        LR
024700940722     C**************************************************************************
024800940722     C*    PULIZIA CAMPI FORMATO 1
024900940722     C**************************************************************************
025000900514     C     PUL01         BEGSR
025100940722     C*
025200021113     C                   MOVEL     *ZEROS        V1CNRS
025300021113     C                   MOVEL     *ZEROS        V1CLP2
025400940721     C                   MOVEL     *ZEROS        V1CNSP
025500940721     C                   MOVEL     *BLANKS       V1CRSM
025600940721     C                   MOVEL     *BLANKS       V1CTSP
025700940721     C                   MOVEL     *ZEROS        V1CPSD
025800940721     C                   MOVEL     99            V1CPSA
025900940721     C                   MOVEL     *ZEROS        V1CSCA
026000940721     C                   MOVEL     *ZEROS        V1CSCD
026100940721     C                   Z-ADD     0             NRR2
026200900516     C                   EXSR      PULSF
026300940722     C*
026400900514     C                   ENDSR
026500940722     C**************************************************************************
026600940722     C*    CONTROLLI FORMATO1
026700940722     C**************************************************************************
026800900509     C     CONTR         BEGSR
026900940722     C*
027000900515     C                   SETOFF                                       900102
027100020822     C                   SETOFF                                       040612
027200920413     C                   SETOFF                                       070811
027300921021     C                   SETOFF                                       151621
027400021108     C     V1CSCA        COMP      0                                  08
027500021108     C     V1CSCD        COMP      0                                  07
027600940722     C*
027700940722     C* SE L'ANNO E' DI DUE CIFRE, LO METTO A QUATTRO
027800000107     C     V1CAAS        IFLT      100
027900940722     C     V1CAAS        IFGT      60
028000940722     C                   MOVEL     19            V1CAAS                         XX SECOLO
028100940722     C                   ELSE
028200940722     C                   MOVEL     20            V1CAAS                         XXI SECOLO
028300940722     C                   ENDIF
028400000107     C                   ENDIF
028500940722     C*
028600940721     C                   MOVEL     *BLANKS       VCCLIN
028700901115     C* SELEZIONO PER DATA SPEDIZIONE O PER NUMERO SPEDIZIONE
028800901115     C     SEL1          IFEQ      0
028900921021     C     SEL2          ANDEQ     0
029000901115     C                   SETON                                        4390
029100901115     C                   GOTO      ENDCTR
029200901115     C                   END
029300901115     C     SEL1          IFNE      0
029400921021     C     SEL2          ANDNE     0
029500901115     C                   SETON                                        4390
029600901115     C                   GOTO      ENDCTR
029700901115     C                   END
029800901115     C* DATA SPEDIZIONE
029900901115     C     SEL1          IFNE      0
030000020801     C* P.O DI TRANSITO
030100020801     C
030200020801     C                   MOVEL     *blanks       V1DPOT
030300020801     C     V1CPOT        IFNE      0
030400020801     C     V1CPOT        CHAIN     AZORG01L                           30
030500020801     C                   IF        *in30 =*on or Orgfag<>'F'
030600020801     C                             and OrgFag <> 'A' or OrgFva <> ' '
030700020801     C                   SETON                                        4690
030800020801     C                   GOTO      ENDCTR
030900020801     C                   ELSE
031000020801     C                   MOVEL     ORGDES        V1DPOT
031100020801     C                   SETON                                        06
031200020801     C                   ENDIF
031300020801     C                   ENDIF
031400901115     C* CHIEDO O LA LINEA PARTENZA O LA LINEA ARRIVO
031500940721     C     V1CLNA        IFEQ      0
031600940721     C     V1CLP1        IFEQ      0
031700901115     C                   SETON                                        4090
031800901115     C                   GOTO      ENDCTR
031900901115     C                   END
032000901115     C                   END
032100901115     C* DATA SPEDIZIONE DAL OBBLIGATORIA
032200940721     C     V1CDSD        IFEQ      0
032300901115     C                   SETON                                        5190
032400901115     C                   GOTO      ENDCTR
032500901115     C                   END
032600900514     C* LINEA ARRIVO
032700940721     C     V1CLNA        IFNE      0
032800940721     C     V1CLNA        CHAIN     AZORG01L                           30
032900900514     C   30              SETON                                        4190
033000900509     C   30              GOTO      ENDCTR
033100900514     C                   SETON                                        01
033200020822     C   01              MOVEL     ORGDES        VCCLIN
033300901115     C                   END
033400901115     C* LINEA PARTENZA
033500940721     C     V1CLP1        IFNE      0
033600940721     C     V1CLP1        CHAIN     AZORG01L                           30
033700920416     C   30              SETON                                        4290
033800901115     C   30              GOTO      ENDCTR
033900901115     C                   SETON                                        02
034000020822     C   02              MOVEL     ORGDES        VCCLIN
034100901115     C                   END
034200901115     C* DATA SPEDIZIONE DAL
034300940721     C                   MOVE      V1CDSD        G02DAT
034400920206     C                   MOVEL     *BLANK        G02ERR
034500940722     C                   CALL      'XSRDA8'
034600920206     C                   PARM                    WLBDAT
034700920206     C     G02ERR        IFEQ      '1'
034800900515     C                   SETON                                        5190
034900900515     C                   GOTO      ENDCTR
035000900515     C                   ELSE
035100940722     C                   MOVEL     G02DAT        V1CDSD
035200940722     C                   MOVE      G02INV        COMDSD            8 0
035300940722     C                   MOVEL     G02INV        COMAAD            4 0
035400920206     C                   MOVE      G02INV        COMMGD            4 0
035500900515     C                   SETON                                        16
035600900515     C                   END
035700900515     C* DATA SPEDIZIONE AL
035800940721     C     V1CDSA        IFNE      0
035900940721     C                   MOVE      V1CDSA        G02DAT
036000920206     C                   MOVEL     *BLANK        G02ERR
036100940722     C                   CALL      'XSRDA8'
036200920206     C                   PARM                    WLBDAT
036300920206     C     G02ERR        IFEQ      '1'
036400900515     C                   SETON                                        5290
036500900515     C                   GOTO      ENDCTR
036600900515     C                   ELSE
036700940722     C                   MOVEL     G02DAT        V1CDSA
036800940722     C                   MOVE      G02INV        COMDSA            8 0
036900900515     C                   END
037000900515     C* DATA SPEDIZIONE AL MAGGIORE DATA SPEDIZIONE AL
037100900515     C     COMDSA        IFLT      COMDSD
037200900515     C                   SETON                                        5490
037300900515     C                   GOTO      ENDCTR
037400900515     C                   END
037500900515     C                   SETON                                        15
037600900515     C                   END
037700901115     C***
037800901115     C* TIPO SPEDIZIONE ESPRESSO
037900940722     C                   MOVEL     *BLANKS       V1DTSP                         DECODIFICA
038000940721     C     V1CTSP        IFNE      ' '
038100901115     C                   SETON                                        04
038200940722     C     V1CTSP        IFEQ      '?'
038300940722     C                   MOVEL     CODUT         §KUT
038400940722     C                   MOVEL     '5E'          KCOD
038500940722     C                   CALL      'X§TABER'
038600940722     C                   PARM                    §KUT              1 0
038700940722     C                   PARM                    KCOD
038800940722     C                   PARM                    KKEY
038900940722     C                   PARM                    §DES             30
039000940722     C                   MOVEL     KKEY          V1CTSP
039100940722     C                   SETON                                        90
039200940722     C                   MOVEL     'S'           WRIEM                          RIEMETTO TESTATA
039300940722     C                   ENDIF
039400940722     C*
039500090604     c                   clear                   ds5e
039600940722     C                   MOVEL     '5E'          KCOD
039700940722     C                   MOVEL     *BLANKS       KKEY
039800940722     C                   MOVEL     V1CTSP        KKEY
039900940722     C     KTAB1         CHAIN     TABEL00F                           47
040000090604     C  N47              MOVEL     TBLUNI        ds5e                           DECODIFICA
040100090604     C  N47              MOVEL     §5ed08        V1DTSP                         DECODIFICA
040200940722     C   47              SETON                                        90
040300940722     C   47              GOTO      ENDCTR
040400901115     C                   END
040500901115     C* NUMERO SERIE AL DEVE ESSERE > NUMERO SERIE DAL
040600901115     C*  SE IMMESSO
040700940721     C     V1CPSD        IFGT      V1CPSA
040800901115     C                   SETON                                        4890
040900901115     C                   GOTO      ENDCTR
041000901115     C                   END
041100901115     C* NUMERO SEGNACOLLO AL DEVE ESSERE > NUMERO SEGNACOLLO DAL
041200940721     C     V1CSCA        IFGT      0
041300940721     C     V1CSCD        IFGT      V1CSCA
041400901115     C                   SETON                                        4990
041500901115     C                   GOTO      ENDCTR
041600901115     C                   END
041700901115     C                   END
041800901115     C* SE IMMESSO AL IMMETTERE ANCHE DAL
041900940721     C     V1CSCA        IFGT      0
042000940721     C     V1CSCD        IFEQ      0
042100901115     C                   SETON                                        5790
042200901115     C                   GOTO      ENDCTR
042300901115     C                   END
042400901115     C                   END
042500901115     C* SE IMMESSA RAGIONE SOCIALE CERCO PRIMO BLANKS
042600940721     C     V1CRSM        IFNE      *BLANKS
042700901115     C                   SETON                                        11
042800940721     C                   MOVEA     V1CRSM        C1
042900901116     C                   Z-ADD     25            X                 2 0
043000901116     C     C1(X)         DOWEQ     *BLANKS
043100901116     C                   SUB       1             X
043200901116     C                   END
043300901116     C     X             ADD       1             XX                2 0
043400901116     C                   END
043500920413     C                   END
043600020821     C*
043700020822     C     SEL2          IFNE      0
043800020821     C     V1CPOT        IFNE      0
043900020821      *
044000020821     C** NUMERO SPEDIZIONE
044100020821     C** LINEA PARTENZA
044200020821     C** LINEA DI TRANSITO
044300020821     C** Aggancio direttamente la testata file transiti
044400020821     C     V1CLP2        CHAIN     AZORG01L                           30
044500020821     C   30              SETON                                        4590
044600020821     C   30              GOTO      ENDCTR
044700020821     C* NUMERO SPEDIZIONE OBBLIGATOIRO SE IMMESSA LNP
044800020821     C                   SETOFF                                         61
044900020821     C     KLNP4         CHAIN     FNBTP11L                           61
045000020821     C   61              SETON                                        9044
045100020821     C   61              GOTO      ENDCTR
045200020822      * segnalo che posso emettere direttamente la bolla
045300020823     C                   z-add     1             $Bolla
045400020821     C*
045500020821     C                   ELSE
045600020822     C                   SETON                                        12
045700020821     C*
045800020821     C** NUMERO SPEDIZIONE
045900020821     C** LINEA PARTENZA
046000020821     C     V1CLP2        CHAIN     AZORG01L                           30
046100020821     C   30              SETON                                        4590
046200020821     C   30              GOTO      ENDCTR
046300020822     C   12              MOVEL     ORGDES        VCCLIN
046400020821     C* NUMERO SPEDIZIONE OBBLIGATOIRO SE IMMESSA LNP
046500020821     C                   SETOFF                                         61
046600020823     C                   z-add     0             $Bolla            2 0
046700020821     C     KLNP          SETLL     FNBTP01L
046800020821     C     KLNP          READE     FNBTP01L                               61
046900020821     C     *in61         DOWEQ     *off
047000020822      * vedo se c'è piu' di una bolla con questa chiave
047100020821     C                   add       1             $Bolla
047200020821     C     KLNP          READE     FNBTP01L                               61
047300020821     C                   ENDDO
047400020822     C                   IF        $Bolla = 0
047500020822     C                   SETON                                        9044
047600020822     C                   GOTO      ENDCTR
047700020822     C                   ENDIF
047800020821     C*
047900020821     C                   END
048000020821     C                   END
048100921021     C* 21 ON RICHIESTA SIA LNA  CHE LNP
048200921021     C   01
048300921021     CAN 02              DO
048400921021     C                   SETOFF                                       02
048500921021     C                   SETON                                        21
048600921021     C                   END
048700900514     C*
048800900514     C     ENDCTR        ENDSR
048900940722     C**************************************************************************
049000940722     C*    ELABORA BOLLE DA VISUALIZZARE
049100940722     C**************************************************************************
049200900515     C     ELAB          BEGSR
049300940722     C*
049400920416     C                   Z-ADD     0             COMOD
049500901115     C* NUMERO DI SPEDIZIONE
049600020822     C     SEL2          IFNE      0
049700020821     C     V1CPOT        IFNE      0
049800020821     C     $Bolla        oreq      1
049900940722     C                   MOVEL     BTPLNP        PARLNP
050000940722     C                   MOVEL     BTPNRS        VSCNRS
050100940722     C                   MOVEL     BTPNSP        VSCNSP
050200940722     C                   MOVEL     BTPAAS        VSCAAS
050300020823     C                   MOVEL     BTPFLP        VHCPOT
050400920413     C*
050500920413     C                   EXSR      BOLLA
050600901115     C*
050700901115     C                   END
050800020821     C                   END
050900901115     C*
051000901115     C* DATA SPEDIZIONE
051100020822     C* OPPURE PER SPEDIZIONE MA PRESENTI PIU' BOLLE
051200901115     C     SEL1          IFNE      0
051300020821     C     $Bolla        orgt      1
051400940721     C                   Z-ADD     1             REC2
051500900515     C*** FILE STORICO
051600940721     C     V1CFTA        COMP      '1'                                    17
051700940721     C     V1CFTA        COMP      '2'                                    18
051800940721     C     V1CFTA        COMP      ' '                                    19
051900900517     C*
052000900515     C* TIPO SPEDIZIONE "N" ==> BLANK
052100901116     C                   MOVEL     *BLANKS       TSP
052200940721     C   04V1CTSP        IFEQ      'N'
052300900515     C                   MOVEL     *BLANKS       TSP               1
052400900515     C                   ELSE
052500940721     C                   MOVEL     V1CTSP        TSP               1
052600900515     C                   END
052700020801     C*
052800020801     C* ESEGUO UN LOOP ESTERNO PER POSIZIONARMI SUL CORRETTO P.O TRANSITO
052900020801     C                   IF        V1CPOT = *zeros
053000020801     C   01*loval        setll     FNBTP26L
053100020801     C   01              read      FNBTP26L                               33
053200020801     C   02*loval        setll     FNBTP31L
053300020801     C   02              read      FNBTP31L                               33
053400021111     C  n33              z-add     BtpFlp        KeyFlp            3 0
053500020822      *
053600020822     C   12KLNP          setll     FNBTP01L
053700020801     C                   ELSE
053800021111     c                   eval      keyflp=v1cpot
053900020801     C                   ENDIF
054000940722     C*
054100900515     C* LETTURA PER LNA DATA SPEDIZ. TIPO SPEDIZIONE
054200901116     C   01              DO
054300940722     C   16KLNA1         SETLL     FNBTP26L
054400900515     C* LETTURA SOLO PER LNA
054500020801     C  N16KLNA2         SETLL     FNBTP26L
054600901116     C                   END
054700940722     C*
054800901116     C* LETTURA PER LNP DATA SPEDIZ. SERIE
054900940722     C   02KLNP2         SETLL     FNBTP31L
055000920416     C*
055100920416     C                   EXSR      CARSF
055200920416     C                   END
055300920416     C*
055400901116     C                   ENDSR
055500940722     C**************************************************************************
055600940722     C*    CARICO SUBFILE IN AVANTI
055700940722     C**************************************************************************
055800900515     C     CARSF         BEGSR
055900900517     C*
056000940721     C                   Z-ADD     COMOD         NRR2
056100021111     C                   Z-ADD     1             B                 2 0
056200021111    1C     B             DOWLE     19
056300021111     c     *in30         ANDEQ     *OFF
056400021111     C   01KLna2         READE     FNBTP26L                               33
056500021111     C   02Klnp3         READE     FNBTP31L                               33
056600021111     C   12KLNP          reade     FNBTP01L                               33
056700021111     c
056800021111    2c                   if        not *in33
056900940722     C*
057000021111     c                   setoff                                       32
057100021111     C  N12              EXSR      CONFR
057200021111     C  N32              EXSR      EMISS
057300021111   x2c                   else
057400021111     C
057500021111    3c                   if        NOT *IN12 AND V1CPOT=0
057600021111     C   01KeyFlp        SETGT     FNBTP26L
057700021111     C   01              read      FNBTP26L                               30
057800021111     C   02KeyFlp        SETGT     FNBTP31L
057900021111     C   02              read      FNBTP31L                               30
058000021111     C
058100021111     C* SE TROVATO NUOVO P.O. transito, mi riposizione prima di leggere
058200021111    4C                   IF        NOT *IN30
058300021111     C                   z-add     BtpFlp        KeyFlp            3 0
058400021111     C* LETTURA PER LNA DATA SPEDIZ. TIPO SPEDIZIONE
058500021111    5C   01              DO
058600021111     C   16KLNA1         SETLL     FNBTP26L
058700021111     C* LETTURA SOLO PER LNA
058800021111     C  N16KLNA2         SETLL     FNBTP26L
058900021111    5C                   END
059000021111     C*
059100021111     C* LETTURA PER LNP DATA SPEDIZ. SERIE
059200021111     C   02KLNP2         SETLL     FNBTP31L
059300021111    4C                   ENDIF
059400021111     c
059500021111   x3c                   else
059600021111     c                   seton                                        30
059700021111    3C                   ENDIF
059800021111    2C                   ENDIF
059900020822      *
060000021111    1C                   ENDDO
060100021111     c
060200940721     C                   Z-ADD     NRR2          COMOD             4 0
060300021114      * imposto linea di arrivo del control se sono nel caso di
060400021114      * selezioni per num. spedizione bolla
060500021113     C   12              move      v1clp2        v1clpa
060600021113     C  N12              clear                   v1clpa
060700021113      *
060800020823     C                   move      *zeros        SEL2
060900900516     C*
061000900515     C                   ENDSR
061100940722     C**************************************************************************
061200940722     C*    CONFRONTO PER CERCARE BOLLE DA EMETTERE
061300940722     C**************************************************************************
061400900515     C     CONFR         BEGSR
061500940722     C*
061600900518     C                   SETOFF                                       32
061700900515     C** CONFRONTI SOLO PER LINEA ARRIVO
061800900515     C*  DATA SPEDIZIONE > DATA AL
061900940721     C   16
062000940721     CAN 15BTPDSP        IFGT      COMDSA
062100021113     C                   SETON                                          3032
062200900515     C                   GOTO      ENDCON
062300900515     C                   END
062400940721     C   16
062500940721     CANN15BTPDSP        IFGT      COMDSD
062600021113     C                   SETON                                          3032
062700900518     C                   GOTO      ENDCON
062800900518     C                   END
062900920414     C*
063000920414     C* CONFRONTO TIPO SELEZIONE
063100920414     C* SOLO USCITE DA TRANSITO
063200940722     C   17BTPDUT        IFEQ      0
063300920414     C                   SETON                                        32
063400920414     C                   GOTO      ENDCON
063500920414     C                   END
063600920414     C* SOLO ANCORA DA USCIRE DAL TRANSITO
063700940722     C   18BTPDUT        IFNE      0
063800920414     C                   SETON                                        32
063900920414     C                   GOTO      ENDCON
064000920414     C                   END
064100921021     C* LINEA DI PARTENZA
064200940722     C   21BTPLNP        IFNE      V1CLP1
064300921021     C                   SETON                                        32
064400921021     C                   GOTO      ENDCON
064500921021     C                   END
064600900515     C* TIPO SPEDIZIONE
064700940722     C   04BTPTSP        IFNE      TSP
064800900515     C                   SETON                                            32
064900900515     C                   GOTO      ENDCON
065000900515     C                   END
065100900518     C* SERIE DAL + AL
065200940722     C     BTPNRS        IFLT      V1CPSD
065300900518     C                   SETON                                            32
065400900518     C                   GOTO      ENDCON
065500900518     C                   END
065600940722     C     BTPNRS        IFGT      V1CPSA
065700900515     C                   SETON                                            32
065800900515     C                   GOTO      ENDCON
065900900515     C                   END
066000900518     C* SEGNACOLLO DAL SOLO
066100940722     C   07
066200940722     CANN08KBTP2         SETLL     FNBTT01L                               31
066300910827     C   07
066400910827     CANN08
066500910827     CANN31              SETON                                        32
066600910827     C   07
066700910827     CANN08
066800910827     CANN31              GOTO      ENDCON
066900900515     C* SEGNACOLLO DAL + AL
067000910827     C   08              DO
067100910827     C                   SETON                                        32
067200940722     C     KBTP          SETLL     FNBTT01L
067300940722     C     KBTP          READE     FNBTT01L                               31
067400910827     C     *IN31         DOWEQ     '0'
067500940722     C     BTTNSC        IFGE      V1CSCD
067600940722     C     BTTNSC        ANDLE     V1CSCA
067700910827     C                   SETON                                        31
067800910827     C                   SETOFF                                       32
067900910827     C                   END
068000910827     C*
068100940722     C  N31KBTP          READE     FNBTT01L                               31
068200910827     C                   END
068300910827     C*
068400910827     C   32              GOTO      ENDCON
068500910827     C                   END
068600900515     C* RAGIONE SOCIALE MITTENTE
068700900515     C   11              DO
068800940722     C                   MOVEA     BTPRSM        C1
068900900515     C*
069000900515     C     XX            DO        25            X
069100900515     C                   MOVE      *BLANKS       C1(X)
069200900515     C                   END
069300900515     C                   MOVEA     C1            COMRAG           25
069400900515     C*
069500940721     C     COMRAG        IFNE      V1CRSM
069600900515     C                   SETON                                            32
069700900515     C                   GOTO      ENDCON
069800900515     C                   END
069900900515     C                   END
070000940722     C*
070100900515     C     ENDCON        ENDSR
070200940722     C**************************************************************************
070300940722     C* EMISSIONE SUBFILE
070400940722     C**************************************************************************
070500900515     C     EMISS         BEGSR
070600940722     C*
070700940721     C                   MOVEL     *BLANKS       VSCSCE
070800940722     C   01              MOVEL     BTPLNP        VSCLIN
070900940722     C   02              MOVEL     BTPLNA        VSCLIN
071000020823     C   12              MOVEL     BTPLNA        VSCLIN
071100940722     C                   MOVE      BTPAAS        VSCAAS
071200020823     C                   MOVEL     BTPFLP        VHCPOT
071300920206     C*
071400940722     C                   MOVEL     BTPMGS        MMGG
071500940721     C                   MOVEL     GG            VSCDSP                          DATA SPEDIZ
071600940721     C                   MOVE      MM            VSCDSP
071700920206     C*
071800941219     C                   MOVE      BTPDET        G02INV                          AAAA/MM/GG
071900920413     C                   MOVE      *ZEROS        G02DAT
072000920413     C                   MOVEL     3             G02ERR
072100940722     C                   CALL      'XSRDA8'
072200920413     C                   PARM                    WLBDAT
072300941219     C                   MOVEL     G02DAT        VSCDET                          GG/MM/AA (6)
072400941219     C                   MOVE      G02DAT        DUE               2 0           ANNO
072500941219     C                   MOVE      DUE           VSCDET
072600920413     C*
072700941219     C                   MOVE      BTPDUT        G02INV                          AAAA/MM/GG
072800920413     C                   MOVE      *ZEROS        G02DAT
072900920413     C                   MOVEL     3             G02ERR
073000940722     C                   CALL      'XSRDA8'
073100920413     C                   PARM                    WLBDAT
073200941219     C                   MOVEL     G02DAT        VSCDUT                          GG/MM/AA (6)
073300941219     C                   MOVE      G02DAT        DUE                             ANNO
073400941219     C                   MOVE      DUE           VSCDUT
073500900516     C*
073600940722     C                   MOVEL     BTPNSP        VSCNSP
073700940722     C                   MOVEL     BTPRSD        VSCRSD
073800940722     C                   MOVEL     BTPRSM        VSCMIT
073900940722     C                   MOVEL     BTPNRS        VSCNRS
074000940722     C     BTPTSP        IFNE      'N'                                          'N'=NORMALE
074100940722     C                   MOVEL     BTPTSP        VSCTSP                         TIPO SERVIZIO BOLLE
074200940722     C                   ELSE
074300940722     C                   MOVEL     *BLANKS       VSCTSP
074400940722     C                   ENDIF
074500941219     C                   MOVEL     BTPFLS        VSCFLS                         FIL.SEGNACOLLO
074600920206     C* SEGNACOLLI
074700920206     C                   MOVEA     *BLANKS       SEG
074800940722     C                   MOVEL     BTPNCD        COMSCN            7
074900920206     C                   MOVEA     COMSCN        SEG(1)
075000940722     C     BTPNCA        IFNE      0
075100940722     C                   MOVEL     BTPNCA        COMSCN
075200920206     C                   MOVEA     COMSCN        SEG(9)
075300920206     C                   END
075400920206     C                   MOVEA     '/'           SEG(8)
075500920206     C* ELIMINO ZERO NON SIGNIFICATIVI
075600920206     C                   Z-ADD     1             X
075700920206     C     SEG(X)        DOWEQ     '0'
075800920206     C                   MOVE      ' '           SEG(X)
075900920206     C                   ADD       1             X
076000920206     C                   END
076100920206     C*
076200940722     C     BTPNCA        IFNE      0
076300920206     C                   Z-ADD     9             X
076400920206     C     SEG(X)        DOWEQ     '0'
076500920206     C                   MOVE      ' '           SEG(X)
076600920206     C                   ADD       1             X
076700920206     C                   END
076800920206     C                   END
076900940721     C                   MOVEA     SEG           VSCSEG
077000900522     C*
077100900518     C                   ADD       1             B
077200940721     C                   ADD       1             NRR2              4 0
077300940721     C                   WRITE     LR68S02
077400940722     C*
077500900515     C                   ENDSR
077600940722     C**************************************************************************
077700940722     C* PULIZIA SUBFILE
077800940722     C**************************************************************************
077900900515     C     PULSF         BEGSR
078000940722     C*
078100900515     C                   SETON                                        35
078200940721     C                   WRITE     LR68C02
078300900515     C                   SETOFF                                       35
078400940722     C*
078500900515     C                   ENDSR
078600940722     C**************************************************************************
078700940722     C* CARICO DATI BOLLA
078800940722     C**************************************************************************
078900920413     C     BOLLA         BEGSR
079000940722     C*
079100020823      * Aggancio la singola bolla usando la chiave con P.O di transito
079200020823     C     KBTP4         CHAIN     FNBTP11L                           31
079300020823     C***  KBTP3         CHAIN     FNBTP01L                           31
079400920413     C* CODICE BOLLA
079500940722     C                   MOVEL     '3A'          KCOD
079600940722     C                   MOVEL     *BLANKS       KKEY
079700940722     C                   MOVEL     BTPCBO        KKEY
079800940722     C     KTAB1         CHAIN     TABEL00F                           31
079900920413     C                   MOVEL     TBLUNI        DS3A
080000920413     C                   MOVEL     §3ATB1        FLGTB1            1
080100940721     C                   MOVEL     §3ADES        V3DCBO
080200920413     C     FLGTB1        IFEQ      'F'
080300920413     C                   SETON                                        03
080400920413     C                   END
080500930115     C*
080600920413     C* DECODIFICA LINEA DI ARRIVO
080700940721     C     BTPLNA        CHAIN     AZORG01L                           35
080800940727     C  N35              MOVEL     ORGDES        PD8LNA
080900940727     C   35              MOVEL     *BLANKS       PD8LNA
081000940727     C                   MOVEL     PD8LNA        V3DLNA
081100021108     C* DECODIFICA LINEA DI PARTENZA
081200940721     C     BTPLNP        CHAIN     AZORG01L                           35
081300940721     C  N35              MOVEL     ORGDES        V3DLNP
081400940721     C   35              MOVEL     *BLANKS       V3DLNP
081500940722     C*
081600920413     C* NON SEQUENZIALI
081700940721     C     BTPFNS        IFEQ      'S'
081800940721     C                   MOVEL     SEQ(1)        V3DFNS
081900920413     C                   ELSE
082000940721     C                   MOVEL     *BLANKS       V3DFNS
082100920413     C                   END
082200920413     C*
082300940721     C* DECODIFICA TIPO SERVIZIO BOLLE (ESPRESSO, NORMALE, ...)
082400940721     C                   MOVEL     *BLANKS       V3DTSP
082500940721     C                   MOVEL     '5E'          KCOD
082600940721     C                   MOVEL     *BLANKS       KKEY
082700940721     C                   MOVEL     BTPTSP        KKEY
082800940722     C     KTAB1         CHAIN     TABEL00F                           31
082900090604     C  N31              MOVEL     TBLUNI        ds5e                           DECOD.TIPO SERVIZIO
083000090604     C  N31              MOVEL     §5ed08        V3DTSP                         DECOD.TIPO SERVIZIO
083100920414     C*
083200940721     C                   MOVEL     BTPNRS        V3CNRS                         NUMERO SERIE
083300941219     C                   MOVEL     BTPNRS        V3CSER                         NUMERO SERIE
083400940721     C                   MOVEL     BTPNSP        V3CNSP                         NUMERO SPEDIZIONE
083500091027     c
083600940721     C                   MOVEL     BTPNFV        V3CNFV                         NUMERO FOGLIO VIAGGI
083700091027     C                   MOVEL     BTPfle        V3Cfle                         lnp partenza NFV
083800091027     c* Prendo la data del foglio da fnfgv
083900091027     c     kfgv          chain     fnfgv01l
084000091027     c                   if        %found(fnfgv01l)
084100091027     C                   MOVEL     fgvdfv        G02INV
084200091027     C                   Z-ADD     *ZEROS        G02DAT
084300091027     C                   MOVEL     '3'           G02ERR
084400091027     C                   CALL      'XSRDA8'
084500091027     C                   PARM                    WLBDAT
084600091027     C                   MOVEL     G02DAT        V3Cdfv
084700091027     c                   else
084800091027     c                   clear                   v3cdfv
084900091027     c                   endif
085000091027     c
085100940721     C                   MOVEL     BTPLNP        V3CLNP                         LINEA PARTENZA
085200941109     C                   MOVEL     BTPLNP        VIDLNP                         LINEA PARTENZA
085300940721     C                   MOVEL     BTPLNA        V3CLNA                         LINEA ARRIVO
085400940721     C                   MOVEL     BTPCBO        V3CCBO                         CODICE BOLLA
085500940721     C                   MOVEL     BTPZNC        V3CZNC                         ZONA
085600940721     C                   MOVEL     BTPRSM        V3CRSM                         RAG.SOC.MITTENTE
085700940727     C                   MOVEL     BTPRSM        PA8RSM                         RAG.SOC.MITTENTE
085800940721     C                   MOVEL     BTPRSD        V3CRSD                         RAG.SOC.DESTINATARIO
085900940727     C                   MOVEL     BTPRSD        PA8RSD                         RAG.SOC.DESTINATARIO
086000940721     C                   MOVEL     BTPNCL        V3CNCL                         NUMERO COLLI
086100940721     C                   MOVEL     BTPPKB        V3CPKB                         PESO BOLLETTATO
086200990907     C                   MOVEL     BTPPKC        V3CPKC                         PESO CML
086300940721     C                   MOVEL     BTPFVB        V3CFVB                         FLAG VOLUME BOLLETTA
086400940721     C                   MOVEL     BTPVLB        V3CVLB                         VOLUME BOLLETTATO
086500940721     C                   MOVEL     BTPVLC        V3CVLC                         VOLUME CML
086600950120     C     BTPNCR        IFNE      99999
086700950120     C                   MOVEL     BTPNCR        V3CNCR                         COLLI RILEVATI DA CM
086800950120     C                   ELSE
086900950120     C                   CLEAR                   V3CNCR
087000950120     C                   ENDIF
087100990907     C     BTPNCP        IFNE      99999
087200990907     C                   MOVEL     BTPNCP        V3CNCP                         COLLI RILEVATI DA CM
087300990907     C                   ELSE
087400990907     C                   CLEAR                   V3CNCP
087500990907     C                   ENDIF
087600941219     C                   MOVEL     BTPFLS        V3CFLS                         FIL.SEGNACOLLO
087700940721     C                   MOVEL     BTPNCD        V3CNCD                         NR.SEGNACOLLO DA
087800940721     C                   MOVEL     BTPNCA        V3CNCA                         NR.SEGNACOLLO A
087900920413     C* GIRO DATA SPEDIZIONE
088000940721     C                   MOVEL     BTPDSP        G02INV
088100940721     C                   Z-ADD     *ZEROS        G02DAT
088200940721     C                   MOVEL     '3'           G02ERR
088300940721     C                   CALL      'XSRDA8'
088400940721     C                   PARM                    WLBDAT
088500940721     C                   MOVEL     G02DAT        V3CDSP
088600920413     C* DATA BORDERO'
088700940721     C                   MOVEL     BTPDBR        G02INV
088800940721     C                   Z-ADD     *ZEROS        G02DAT
088900940721     C                   MOVEL     '3'           G02ERR
089000940721     C                   CALL      'XSRDA8'
089100940721     C                   PARM                    WLBDAT
089200940721     C                   MOVEL     G02DAT        V3CDBR
089300940721     C* DATA PARTENZA ULTIMO COLLO
089400940721     C                   MOVEL     BTPDPC        G02INV
089500940721     C                   Z-ADD     *ZEROS        G02DAT
089600940721     C                   MOVEL     '3'           G02ERR
089700940721     C                   CALL      'XSRDA8'
089800940721     C                   PARM                    WLBDAT
089900940721     C                   MOVEL     G02DAT        V3CDPC
090000940721     C* DATA PARTENZA ULTIMO COLLO
090100940721     C                   MOVEL     BTPDUC        G02INV
090200940721     C                   Z-ADD     *ZEROS        G02DAT
090300940721     C                   MOVEL     '3'           G02ERR
090400940721     C                   CALL      'XSRDA8'
090500940721     C                   PARM                    WLBDAT
090600940721     C                   MOVEL     G02DAT        V3CDUC
090700920413     C* DATA ENTRATA AL TRANSITO
090800940721     C                   MOVEL     BTPDET        G02INV
090900940721     C                   Z-ADD     *ZEROS        G02DAT
091000940721     C                   MOVEL     '3'           G02ERR
091100940721     C                   CALL      'XSRDA8'
091200940721     C                   PARM                    WLBDAT
091300940721     C                   MOVEL     G02DAT        V3CDET
091400920413     C* DATA USCITA DAL TRANSITO
091500940721     C                   MOVEL     BTPDUT        G02INV
091600940721     C                   Z-ADD     *ZEROS        G02DAT
091700940721     C                   MOVEL     '3'           G02ERR
091800940721     C                   CALL      'XSRDA8'
091900940721     C                   PARM                    WLBDAT
092000940721     C                   MOVEL     G02DAT        V3CDUT
092100940721     C* DATA ARRIVO MERCE A FILIALE ARRIVO
092200940721     C                   MOVEL     BTPDAM        G02INV
092300940721     C                   Z-ADD     *ZEROS        G02DAT
092400940721     C                   MOVEL     '3'           G02ERR
092500940721     C                   CALL      'XSRDA8'
092600940721     C                   PARM                    WLBDAT
092700940721     C                   MOVEL     G02DAT        V3CDAM
092800940721     C*
092900920413     C                   MOVEL     ' '           FLAG
093000020821     C* P.O TRANSITO PIU' DECODIFICA
093100020821     C                   MOVEL     BTPFLP        VIDPOT
093200020821     C     BTPFLP        CHAIN     AZORG01L                           35
093300020821     C  N35              MOVEL     ORGDES        PD8POT
093400020821     C   35              MOVEL     *BLANKS       PD8POT
093500020821     C                   MOVEL     PD8POT        V3DPOT
093600040531     c* Verifico se collo di valore
093700040531     c     kar5          chain     fiar501l
093800040531     c                   if        %found(fiar501l)
093900040531     c                   movel     ar5uni        dar5gen
094000040531     c                   else
094100040531     c                   clear                   dar5gen
094200040531     c                   endif
094300040531     c                   eval      v3cbva=§ar5bva
094400040531     C*c
094500940721     C*  EMETTO IL FORMATO DI DETTAGLIO DELLA BOLLA TRANSITO
094600940721     C                   MOVEL     'S'           WRIEM                          SI RIEMETTO TESTATA
094700920414     C     FOR03         TAG
094800940721     C     WRIEM         IFEQ      'S'
094900940721     C                   WRITE     LR68T01                                      INTESTAZIONE VIDEO
095000940721     C                   MOVEL     *BLANKS       WRIEM             1            NO RIEMETTO TESTATA
095100940721     C                   ENDIF
095200940722     C                   EXFMT     LR68D03
095300940721     C*
095400940721     C* F3 = TORNO A SUBFILE
095500920413     C   KC              MOVEL     '2'           FLAG              1
095600920413     C   KC              GOTO      ENDBOL
095700940721     C* F12 = LEGGO ALTRA BOLLA
095800920413     C   KL              MOVEL     ' '           FLAG
095900920413     C   KL              GOTO      ENDBOL
096000940923     C* F19 = INTERROGAZIONME SPUNTE
096100940923     C   KT              DO
096200950112     C                   CLEAR                   PARA04
096300950116     C                   Z-ADD     V3CFLS        P04LNP
096400940923     C                   Z-ADD     V3CLNA        P04LNA
096500940923     C                   Z-ADD     V3CNRS        P04NRS
096600950116     C                   Z-ADD     V3CNCD        P04NCD
096700950116     C                   Z-ADD     V3CNCA        P04NCA
096800101202     C                   MOVE      V3CDSP        p04boan
096900101202     C                   MOVE      V3CLNP        p04bolp
097000101202     C                   MOVE      V3CNRS        p04bors
097100101202     C                   MOVE      V3CNSP        p04bosp
097200950116     C* LEGGO DAI SEGNACOLLI PERCHE' NON SEQUENZIALI
097300950116     C     V3CNCD        IFEQ      0
097400950116     C     V3CNRS        ANDGT     0
097500950116     C     KBTP          CHAIN     FNBTT01L                           30
097600950116     C*
097700950116     C     *IN30         IFEQ      *OFF
097800950116     C* SEGNACOLLO DAL
097900950116     C                   MOVEL     BTTNSC        P04NCD
098000950116     C* SEGNACOLLO AL
098100950116     C     KBTP          SETGT     FNBTT01L                           30
098200950116     C     KBTP          READPE    FNBTT01L                               30
098300950116     C  N30BTTNSC        IFNE      P04NCD
098400950116     C                   MOVEL     BTTNSC        P04NCA
098500950116     C                   ENDIF
098600950116     C                   ENDIF
098700950116     C                   ENDIF
098800940923     C                   Z-ADD     99            P04NPS
098900950112     C                   Z-ADD     9             P04NPG
099000940923     C                   MOVEL     '1'           P04FLG
099100950112     C                   MOVEL     'FNLR68R'     P04PGM
099200940923     C                   MOVEL     PARA04        KPJBU
099300940923     C                   CALL      'FNLR04R'
099400940923     C                   PARM                    KPJBA
099500940923     C                   MOVEL     'S'           WRIEM                          SI RIEMETTO TESTATA
099600940923     C                   GOTO      FOR03
099700940923     C                   ENDDO
099800940721     C* F21 = DETTAGLIO COLLI
099900940721     C   KV              DO
100000020821     C                   MOVE      VIDPOT        PA8POT
100100020821     C                   MOVE      V3CDSP        PA8AAS
100200940727     C                   MOVEL     V3CLNP        PA8LNP
100300940727     C                   MOVEL     V3CNRS        PA8NRS
100400940727     C                   MOVEL     V3CNSP        PA8NSP
100500940727     C                   MOVEL     V3CDSP        PA8DSP
100600940727     C                   MOVEL     V3CLNA        PA8LNA
100700940727     C                   MOVEL     V1CRSU        PA8DSA
100800940722     C                   MOVEL     *BLANKS       KPJBU
100900940727     C                   MOVEL     PARAA8        KPJBU
101000940722     C                   CALL      'FNLRA8R'
101100920416     C                   PARM                    KPJBA
101200940721     C                   MOVEL     'S'           WRIEM                          SI RIEMETTO TESTATA
101300940721     C                   GOTO      FOR03
101400940721     C                   ENDDO
101500940721     C* F7 = ANOMALIE
101600920423     C   KG              DO
101700960523     C                   CLEAR                   DSLR44
101800960523     C                   MOVE      V3CDSP        D44AAS
101900960523     C                   MOVE      V3CLNP        D44FIL
102000960523     C                   MOVE      V3CNRS        D44NRS
102100960523     C                   MOVE      V3CNSP        D44NSP
102200960523     C                   MOVEL     'T'           D44TBO
102300960523     C                   MOVE      'I'           D44MOD
102400960523     C                   MOVEL     DSLR44        KPJBU
102500960523     C                   CALL      'FNLR44R'
102600960523     C                   PARM                    KPJBA
102700940721     C                   MOVEL     'S'           WRIEM                          SI RIEMETTO TESTATA
102800920423     C                   GOTO      FOR03
102900940721     C                   ENDDO
103000920413     C*
103100920413     C     ENDBOL        ENDSR
103200940721     C**************************************************************************
103300940721     C*    R O U T I N E    I N I Z I A L E
103400940721     C**************************************************************************
103500940721     C     *INZSR        BEGSR
103600940721     C*
103700940721     C     KLNA1         KLIST
103800020801     C                   KFLD                    KeyFlp
103900940721     C                   KFLD                    V1CLNA
104000940721     C                   KFLD                    COMAAD
104100940721     C                   KFLD                    COMMGD
104200940721     C                   KFLD                    TSP               1
104300020801     C     KLNA2         KLIST
104400020801     C                   KFLD                    KeyFlp
104500020801     C                   KFLD                    V1CLNA
104600940721     C     KLNP          KLIST
104700940721     C                   KFLD                    V1CAAS
104800940721     C                   KFLD                    V1CLP2
104900940721     C                   KFLD                    V1CNRS
105000940721     C                   KFLD                    V1CNSP
105100020821     C     KLNP4         KLIST
105200020821     C                   KFLD                    V1CPOT
105300020821     C                   KFLD                    V1CAAS
105400020821     C                   KFLD                    V1CLP2
105500020821     C                   KFLD                    V1CNRS
105600020821     C                   KFLD                    V1CNSP
105700940722     C     KBTP          KLIST
105800940722     C                   KFLD                    BTPAAS
105900940722     C                   KFLD                    BTPLNP
106000940722     C                   KFLD                    BTPNRS
106100940722     C                   KFLD                    BTPNSP
106200940722     C     KBTP2         KLIST
106300940722     C                   KFLD                    BTPAAS
106400940722     C                   KFLD                    BTPLNP
106500940722     C                   KFLD                    BTPNRS
106600940722     C                   KFLD                    BTPNSP
106700940721     C                   KFLD                    V1CSCD
106800020823     C**   KBTP3         KLIST
106900020823     C**                 KFLD                    VSCAAS
107000020823     C**                 KFLD                    PARLNP            3 0
107100020823     C**                 KFLD                    VSCNRS
107200020823     C**                 KFLD                    VSCNSP
107300020822     C     KBTP4         KLIST
107400020823     C                   KFLD                    VHCPOT
107500020822     C                   KFLD                    VSCAAS
107600020823     C                   KFLD                    PARLNP            3 0
107700020822     C                   KFLD                    VSCNRS
107800020822     C                   KFLD                    VSCNSP
107900940721     C     KLNP2         KLIST
108000020801     C                   KFLD                    KeyFlp
108100940721     C                   KFLD                    V1CLP1
108200940721     C                   KFLD                    COMAAD
108300940721     C                   KFLD                    COMMGD
108400940721     C                   KFLD                    V1CPSD
108500020801     C     KLNP3         KLIST
108600020801     C                   KFLD                    KeyFlp
108700020801     C                   KFLD                    V1CLP1
108800940721     C     KTAB1         KLIST                                                  TABEL00F
108900940722     C                   KFLD                    CODUT                          ..CODICE UTENTE
109000940721     C                   KFLD                    KCOD                           ..CODICE TABELLA
109100940721     C                   KFLD                    KKEY                           ..CODICE ELEMENTO
109200040531     C     Kar5          KLIST
109300040531     C                   KFLD                    BTPAAS
109400040531     C                   KFLD                    BTPLNP
109500040531     C                   KFLD                    BTPNRS
109600040531     C                   KFLD                    BTPNSP
109700040531     C                   KFLD                    ktrd
109800091027     C     KFGV          KLIST
109900091027     C                   KFLD                    BTPnfv
110000091027     C                   KFLD                    BTPfle
110100940721     C*
110200940721     C     *LIKE         DEFINE    TBLCOD        KCOD                           CODICE TABELLA
110300940721     C     *LIKE         DEFINE    TBLKEY        KKEY                           CODICE ELEMENTO
110400940721     C*
110500940722     C                   Z-ADD     1             CODUT                          CODICE UTENTE
110600940721     C*
110700940721     C                   ENDSR
110800940721     C**************************************************************************
110900920413**   SEQ
111000920413NON SEQUENZIALI
