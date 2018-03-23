000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200940715     H* TNTA17R *----------------------------------------------------*
000300000000     H*-----------------*                                            *
000400940715     H*              LISTA SINTETICA TARIFFE          FILTRO         *
000500000000     H*--------------------------------------------------------------*
000600940715     FTNTA17D   CF   E             WORKSTN
000700000000     FTABEL00F  IF   E           K DISK
000800900423     FAZORG01L  IF   E           K DISK
000900940715     D*
001000950718     D* SCHIERE PER CARICAMENTO L1
001100950718     D L1              S              3  0 DIM(30)
001200950801     D* SCHIERA PER CONTROLLARE QUALE FILIALE PUO' GESTIRE LE TARIFFE
001300950718     D FGT             S              1    DIM(30)
001400950718     D* SCHIERA CONTENENTE LE FILIALI GESTITE DALLA FILIALE COLLEGATA
001500950718     D*
001600950718     D MSG             S             79    DIM(10) CTDATA PERRCD(1)
001700940715     D*
001800000000     D PARAM           DS
001900940715     D* CODICE FILIALE
002000940715     D  V1CFIL                 1      3  0
002100940715     D* DATA  DAL / AL   (GG/MM/AAAA)
002200940715     D  V1CDAL                 4     11  0
002300940715     D  V1CAL                 12     19  0
002400940715     D* CODICE COMMERCIALE
002500940715     D  V1CAGE                20     26  0
002600940715     D* DATA  DAL / AL   (AAAA/MM/GG)
002700940715     D  COMDAL                27     34  0
002800940715     D  COMAL                 35     42  0
002900940715     D* RICHIESTA STAMPA: S=IN SCADENZA / V=VARIATE / D=CON DECORRENZA
003000940715     D  WSTA                  43     43
003100950406     D* SALTO PAGINA A ROTTURA CODICE COMMERCIALE (S/ )
003200950406     D  V1CRCC                44     44
003300950718     D* SCHIERA FIG  ---> FILIALI GESTITE DALLA FILIALE COLLEGATA
003400950718     D  FIG                   45    104P 0
003500950718     D                                     DIM(30)
003600960621     D* COPIA FIRMATA OFFERTA: S=FIRMATE / N=NON FIRMATE / ' '=TUTTE
003700960621     D  V1CCFO               105    105
003800970407     D* SCELTA CLIENTI_: BLOCCATI NON BLOCCATI TUTTI
003900970407     D  V1CTCL               106    106
004000950718     D*
004100950718     D* DS PER TRUL06R - CARICAMENTO £1
004200950718     D DSUL06        E DS                  EXTNAME(TRUL06DS)
004300950718     D  LIN                    1     90  0
004400950718     D                                     DIM(30)
004500940715     D*
004600921012     D WLBDAT          DS
004700940715     D  G02DAT                 1      8  0
004800940715     D  G02INV                 9     16  0
004900940715     D  G02ERR                17     17
005000940715     D  G02TGI                18     22  0
005100000000     D KPJBA         E DS
005200950718     D  REM                    1      3
005300950718     D  REMFIL                 4      6
005400950718     D OG147         E DS
005500921012     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005600950718     D CNCR80        E DS
005700000000     I/SPACE 3
005800940715     C*****************************************************************
005900940715     C* RIEPILOGO INDICATORI
006000940715     C*****************************************************************
006100950718     C* 08    - QUALSIASI TASTO VENGA PREMUTO --> ESCO DAL PROGRAMMA
006200940715     C* 28    - INDICATORE DI EMISSIONE MESSAGGIO DI ERRORE
006300940715     C* 30    - DI COMODO
006400950718     C* 35    - USATO PER LOKUP
006500940715     C* 40/43 - ERRORI
006600940715     C*****************************************************************
006700940719     C*
006800940715     C                   CLEAR                   V1CAGE
006900940715     C                   CLEAR                   V1DAGE
007000940715     C                   CLEAR                   V1CAL
007100940719     C                   Z-ADD     01011901      V1CDAL
007200940715     C                   MOVEL     'S'           V1CSTA
007300960621     C                   MOVEL     'N'           V1CCFO
007400950406     C                   MOVEL     'S'           V1CRCC
007500970407     C                   MOVEL     'N'           V1CTCL
007600921012     C*
007700940715     C     FOR01         TAG
007800940715     C                   EXFMT     TA17D01
007900940715     C*
008000940715     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
008100940715     C                   CLEAR                   V1CMSG
008200940715     C* AZZERO GLI INDICATORI RELATIVI AI MESSAGGI DI ERRORE
008300940715     C                   SETOFF                                       404142
008400940715     C                   SETOFF                                       4328
008500940715     C*
008600940715     C* CMD3 - FINE LAVORO
008700950718     C   KC
008800950718     C** 08 ON  - QUALSIASI TASTO VENGA PREMUTO, ESCO DAL PROGRAMMA
008900950718     COR 08              GOTO      FINE
009000950718     C*
009100950718     C** SOLO IN FILIALE: SE LA FILIALE COLLEGATA NON PUO' GESTIRE LE
009200950718     C**   TARIFFE, QUALSIASI TASTO VENGA PREMUTO ESCO DAL PGM
009300950718    1C     SIMFEL        IFGT      0
009400950718     C                   Z-ADD     1             C
009500950718     C     WFIL          LOOKUP    L1(C)                                  35
009600950718    2C   35FGT(C)        IFEQ      ' '
009700950718     C                   MOVEL     MSG(6)        V1CMSG
009800950718     C                   SETON                                        2808
009900950718     C                   GOTO      FOR01
010000950718    2C                   ENDIF
010100950718    1C                   ENDIF
010200921012     C*
010300921012     C* CONTROLLI FORMATO
010400940715     C                   EXSR      CTRD01
010500940715     C   28
010600940715     CORNKF              GOTO      FOR01
010700900406     C*
010800940715     C* CMD6 - CONFERMA ELABORAZIONE
010900921013     C* SE RICHIESTA STAMPA = *BLANKS  LANCIO TUTTE LE STAMPE
011000940715    1C     V1CSTA        IFEQ      ' '
011100940715     C                   MOVEL     'S'           WSTA
011200921013     C                   MOVEL     PARAM         KPJBU
011300940720     C                   MOVEL     'TA18'        KCOAZ
011400921013     C                   CALL      'BCH10'
011500921013     C                   PARM                    KPJBA
011600921013     C*
011700940715     C                   MOVEL     'V'           WSTA
011800921013     C                   MOVEL     PARAM         KPJBU
011900940720     C                   MOVEL     'TA18'        KCOAZ
012000921013     C                   CALL      'BCH10'
012100921013     C                   PARM                    KPJBA
012200921013     C*
012300940715     C                   MOVEL     'D'           WSTA
012400921013     C                   MOVEL     PARAM         KPJBU
012500940720     C                   MOVEL     'TA18'        KCOAZ
012600921013     C                   CALL      'BCH10'
012700921013     C                   PARM                    KPJBA
012800921013     C*
012900940715   X1C                   ELSE
013000921013     C* LANCIO SOLO LA STAMPA RICHIESTA
013100940715     C                   MOVEL     V1CSTA        WSTA
013200921012     C                   MOVEL     PARAM         KPJBU
013300940720     C                   MOVEL     'TA18'        KCOAZ
013400921012     C                   CALL      'BCH10'
013500000000     C                   PARM                    KPJBA
013600940715    1C                   ENDIF
013700921012     C*
013800000000     C     FINE          TAG
013900000000     C                   SETON                                        LR
014000921012     C*
014100921012     C*--- CONTROLLI FORMATO -----------------------------------------*
014200940715     C     CTRD01        BEGSR
014300930301     C                   Z-ADD     0             COMDAL
014400930301     C                   Z-ADD     0             COMAL
014500921012     C*
014600940715     C****  CODICE FILIALE  ****
014700921012     C* SE IMMESSA CONTROLLO VALIDITA' FILIALE
014800940715    1C     V1CFIL        IFGT      0
014900950718     C*
015000950718     C* CONTROLLO SE LA FILIALE COLLEGATA PUO' GESTIRE LE TARIFFE DELLA
015100950718     C*   FILIALE RICHIESTA A VIDEO
015200950718    2C     SIMFEL        IFGT      0
015300950802     C     V1CFIL        LOOKUP    FIG                                    35
015400950718     C*
015500950802    3C     *IN35         IFEQ      *OFF
015600950718     C                   MOVEL     MSG(7)        V1CMSG
015700950718     C                   SETON                                        4028
015800950718     C                   GOTO      ENDCTR
015900950718    3C                   ENDIF
016000950718    2C                   ENDIF
016100950718     C*
016200940715     C     V1CFIL        CHAIN     AZORG                              30
016300940715     C     *IN30         IFEQ      *ON
016400940715     C                   MOVEL     MSG(1)        V1CMSG
016500940715     C                   SETON                                        4028
016600940715     C                   GOTO      ENDCTR
016700940715     C                   ENDIF
016800940715    1C                   ENDIF
016900921012     C*
017000940715     C****  CODICE COMMERCIALE  ****
017100940715     C* SE IMMESSO CONTROLLO CODICE COMMERCIALE
017200940715    1C     V1CAGE        IFNE      0
017300950718     C                   MOVEL     V1CAGE        FILAGE            3 0
017400950718     C*
017500950718     C* CONTROLLO SE LA FILIALE COLLEGATA PUO' GESTIRE LE TARIFFE DEL
017600950718     C*   CODICE COMMERCIALE DIGITATO
017700950718    2C     SIMFEL        IFGT      0
017800950802     C     FILAGE        LOOKUP    FIG                                    35
017900950718     C*
018000950802    3C     *IN35         IFEQ      *OFF
018100950904     C                   MOVEL     MSG(8)        V1CMSG
018200950718     C                   SETON                                        4128
018300950718     C                   GOTO      ENDCTR
018400950718    3C                   ENDIF
018500950718    2C                   ENDIF
018600950718     C*
018700950718     C* CONTROLLO VALIDITA' CODICE COMMERCIALE
018800921012     C                   MOVE      '01'          COD
018900940715     C                   MOVEL     *BLANKS       KEY
019000940715     C                   MOVEL     V1CAGE        KEY
019100921012     C     KTAB          CHAIN     TABEL                              30
019200940715     C  N30TBLFLG        IFNE      ' '
019300940715     C                   SETON                                        30
019400940715     C                   ENDIF
019500940715     C*
019600940715     C     *IN30         IFEQ      *ON
019700940715     C                   MOVEL     MSG(2)        V1CMSG
019800940715     C                   SETON                                        4128
019900940715     C                   GOTO      ENDCTR
020000940715     C                   ENDIF
020100940715     C*
020200940715     C* DECODIFICA CODICE COMMERCIALE
020300940715     C                   MOVEL     TBLUNI        V1DAGE
020400940715     C*
020500940715     C* CONTROLLO CONGRUENZA CON FILIALE SE IMMESSA
020600940715    2C     V1CFIL        IFGT      0
020700940715     C     V1CFIL        IFNE      FILAGE
020800940715     C                   MOVEL     MSG(3)        V1CMSG
020900940715     C                   SETON                                        4028
021000940715     C                   GOTO      ENDCTR
021100940715     C                   ENDIF
021200940715    2C                   ENDIF
021300921012     C*
021400940715   X1C                   ELSE
021500950718     C* CODICE COMMERCIALE = 0
021600940715     C                   CLEAR                   V1DAGE
021700940715    1C                   ENDIF
021800921012     C*
021900940715     C****  CONTROLLO DATA  DAL  ****
022000921012     C* DATA DAL OBBLIGATORIA
022100940715     C                   Z-ADD     V1CDAL        G02DAT
022200921012     C                   MOVEL     *BLANKS       G02ERR
022300940715     C                   CALL      'XSRDA8'
022400930301     C                   PARM                    WLBDAT
022500930301     C     G02ERR        IFEQ      '1'
022600940715     C                   MOVEL     MSG(4)        V1CMSG
022700940715     C                   SETON                                        4228
022800940715     C                   GOTO      ENDCTR
022900940715     C                   ENDIF
023000940715     C                   Z-ADD     G02INV        COMDAL            8 0
023100940715     C*
023200940715     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
023300940715     C                   Z-ADD     G02DAT        V1CDAL
023400930301     C*
023500940715     C     V1CSTA        IFEQ      'S'
023600940719     C     V1CDAL        ANDNE     01011901
023700940715     C                   MOVEL     MSG(5)        V1CMSG
023800940715     C                   SETON                                        4228
023900940715     C                   GOTO      ENDCTR
024000940715     C                   ENDIF
024100921012     C*
024200940715     C****  CONTROLLO DATA  AL   ****
024300921012     C* DATA AL OBBLIGATORIA
024400940715     C                   Z-ADD     V1CAL         G02DAT
024500921012     C                   MOVEL     *BLANKS       G02ERR
024600940715     C                   CALL      'XSRDA8'
024700921012     C                   PARM                    WLBDAT
024800921013     C     G02ERR        IFEQ      '1'
024900940715     C                   MOVEL     MSG(4)        V1CMSG
025000940715     C                   SETON                                        4328
025100940715     C                   GOTO      ENDCTR
025200940715     C                   ENDIF
025300940715     C                   Z-ADD     G02INV        COMAL             8 0
025400940715     C*
025500940715     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
025600940715     C                   Z-ADD     G02DAT        V1CAL
025700921012     C*
025800940715     C     ENDCTR        ENDSR
025900921012     C*
026000950720     C*--- INZSR INIZIALE --------------------------------------------*
026100940719     C     *INZSR        BEGSR
026200950720     C*
026300950720     C     *ENTRY        PLIST
026400950720     C                   PARM                    KPJBA
026500950720     C                   Z-ADD     1             CODUT
026600950720     C                   CALL      'X§PARUT'
026700950720     C                   PARM                    UT§DSE
026800950720     C                   MOVEL     RAGUT         V1CRSU
026900950720     C                   MOVEL     REC80         CNCR80
027000950721     C                   CLEAR                   PARAM
027100950718     C*
027200950718     C* SE SONO UN "REMXXX" IMPOSTO LA FILIALE A VIDEO
027300950718    1C     REM           IFEQ      'REM'
027400950718     C     REMFIL        ANDGT     *ZEROS
027500950718     C                   MOVEL     REMFIL        V1CFIL
027600950718     C                   MOVEL     REMFIL        WFIL
027700950718   X1C                   ELSE
027800950721     C                   CLEAR                   V1CFIL
027900950718     C                   MOVEL     SIMFEL        WFIL
028000950718    1C                   ENDIF
028100950718     C*
028200950718     C***
028300950718     C* CARICO TABELLA FILIALI GESTITE £1  ****
028400950718     C***
028500950718    1C     SIMFEL        IFGT      0
028600950718     C*
028700950718     C                   CLEAR                   DSUL06
028800950718     C                   MOVE      '£1'          D06COD
028900950718     C                   MOVEL     SIMFEL        D06KEY
029000950718     C                   MOVEL     DSUL06        KPJBU
029100950718     C                   CALL      'TRUL06R'
029200950718     C                   PARM                    KPJBA
029300950718     C                   MOVEL     KPJBU         DSUL06
029400950718     C                   MOVEA     LIN           L1
029500950718     C***
029600950718     C* CARICO TUTTE LE FILIALI DELL'AREA CON IL RELATIVO FLAG GESTIONE
029700950801     C*   TARIFFE E CARICO LE FILIALI CHE PUO' GESTIRE LA FIL.COLLEGATA
029800950718     C***
029900950718     C                   Z-ADD     1             C                 3 0
030000950801     C                   Z-ADD     1             A                 3 0
030100950801     C                   CLEAR                   FIG
030200950718    2C     L1(C)         DOWGT     0
030300950718     C*
030400950718     C     L1(C)         CHAIN     AZORG                              30
030500950718     C*
030600950718    3C     *IN30         IFEQ      *OFF
030700950718     C     ORGFVA        ANDEQ     ' '
030800950718     C                   MOVEL     ORGDE7        OG147
030900950718     C                   MOVEL     §OGFGT        FGT(C)
031000950718     C     §OGFIT        IFNE      *BLANKS
031100950801     C                   MOVEL     §OGFIT        WFIT              3 0
031200950718     C                   ELSE
031300950801     C                   Z-ADD     0             WFIT
031400950718     C                   ENDIF
031500950801     C*
031600950801    4C     §OGFGT        IFEQ      'E'
031700950801     C     WFIL          ANDEQ     WFIT
031800950801     C*
031900950801     C     §OGFGT        OREQ      'E'
032000950801     C     WFIL          ANDEQ     SIMFEL
032100950801     C*
032200950801     C     §OGFGT        ORNE      'E'
032300950801     C     WFIL          ANDEQ     WFIT
032400950801     C                   MOVEL     L1(C)         FIG(A)
032500950801     C                   ADD       1             A
032600950801    4C                   ENDIF
032700950718    3C                   ENDIF
032800950718     C*
032900950718     C                   ADD       1             C
033000950718    2C                   ENDDO
033100950718    1C                   ENDIF
033200950718     C*
033300940719     C***
033400940719     C* CHIAVI
033500940719     C***
033600940719     C* ACCESSO TABEL00F
033700940719     C     KTAB          KLIST
033800940719     C                   KFLD                    CODUT
033900940719     C                   KFLD                    COD
034000940719     C                   KFLD                    KEY
034100940719     C***
034200940719     C* DEFINIZIONE CAMPI
034300940719     C***
034400940719     C     *LIKE         DEFINE    TBLCOD        COD
034500940719     C     *LIKE         DEFINE    TBLKEY        KEY
034600950718     C     *LIKE         DEFINE    SIMFEL        WFIL
034700940719     C                   ENDSR
034800940719     C*
034900940719     C*---------------------------------------------------------------*
035000950904**  MSG
035100940715Filiale Inesistente                                                            1
035200940715Codice Commerciale non codificato                                              2
035300940715Filiale Incongruente con Codice Commerciale                                    3
035400940715Data Errata                                                                    4
035500940719Per le tariffe in scadenza la data DAL deve essere 01/01/1901                  5
035600950803LA  GESTIONE  DELLE  TARIFFE  E'  AFFIDATA  AD  ALTRA  FILIALE  !!!            6
035700950904Filiale in gestione ad altra filiale !!                                        7
035800950904Codice Commerciale in gestione ad altra filiale !!                             8
