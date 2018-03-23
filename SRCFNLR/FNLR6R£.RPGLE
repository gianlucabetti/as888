000100950524     H DECEDIT('0,') DATEDIT(*DMY.)
000200950524      *---------*----------------------------------------------------*
000300950524      * FNLR60R *                                                    *
000400950524      *---------*                                                    *
000500950524      *         - PREPARAZIONE DISTINTA DI CONSEGNA -                *
000600950524      *             --        DA SPUNTE        --                    *
000700950524      *--------------------------------------------------------------*
000800950524     FFNFVV02L  UF   E           K DISK
000900950524     F*---------
001000950524     FFNFVV01L  UF   E           K DISK
001100950524     F                                     RENAME(FNFVV000:FNFVV001)
001200950524     F*---------
001300000114     FFIBRV01L  IF   E           K DISK
001400950524     F*--------
001500950525     FFNARB01L  UF   E           K DISK
001600951211     F*--------
001700990921     FFIAR601L  IF   E           K DISK
001800950526     F*--------
001900990519     FFNANM01L  IF   E           K DISK
002000990519     F                                     INFDS(FNANM1)
002100950524     F*--------
002200950524     FFNART27L  IF   E           K DISK
002300950524     F*--------
002400950524     FFNLBL02L  IF   E           K DISK
002500950525     F*--------
002600990713     FFIGCA01L  IF   E           K DISK
002700950525     F*--------
002800950525     FAZCLN01L  IF   E           K DISK    USROPN
002900950525     F*--------
003000950525     FFNDST01L  IF   E           K DISK
003100950524     F*--------
003200950525     FFNAPD01L  IF   E           K DISK
003300950525     F*--------
003400950525     FFNARBY1L  UF A E           K DISK
003500950526     F*--------
003600950526     FFNARBZ0F  UF A E           K DISK
003700950526     F*--------
003800950526     FTABEL00F  IF   E           K DISK    USROPN
003900950524     F*--------
004000950612     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
004100950612     F                                     USROPN
004200950524      *--------------------------------------------------------------*
004300950524      * Schiere
004400950524      *--------------------------------------------------------------*
004500950524     D CDA             S              2    DIM(10)
004600950525     D CDF             S              2    DIM(10)
004700950525     D MES             S              1    DIM(31)
004800951211     D C3A             S              2    DIM(100)                             CODICE TABELLA 3A
004900951211     D D3A             S             73    DIM(100)                             FLAG. TBLUNI  3A
005000020506     D £6              S              3  0 DIM(30)                              LINEE ARRIVI
005100020506     D*UNI             S              3    DIM(29)                              LINEE DA TABEL
005200950524      *--------------------------------------------------------------*
005300950524      *   DS
005400950524      *--------------------------------------------------------------*
005500950525     D WLBDA8          DS                  INZ
005600950525     D  G02DAT                 1      8  0
005700950525     D  G02INV                 9     16  0
005800950525     D  G02ERR                17     17
005900950525     D  G02TGI                18     22  0
006000950524     D KPJBA         E DS
006100000516     D DSTPD           DS
006200000516     D  TIPOP                  1      2
006300000516     D  LIBERO                 3     20
006400000516     D TIBS02        E DS                  EXTNAME(TIBS02DS)
006500000516     D DTPD          E DS
006600950525     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
006700950525     D CNCR80        E DS
006701020506     d trul06ds      e ds
006702020506     d  lin                    1     90  0 dim(30)
006800950524     D FNLR59        E DS                  EXTNAME(FNLR59DS)
006900950524     D FNLR55        E DS                  EXTNAME(FNLR55DS)
007000951211     D DS3A          E DS
007100960415     D DSLR33        E DS                  EXTNAME(FNLR33DS)
007200990519     D FNANM1          DS
007300960415     D  ANMNR1               397    400B 0
007400950524      *--------------------------------------------------------------*
007500950524      * Ridenomino i campi dei codici anomalie in schiere
007600950524      *--------------------------------------------------------------*
007700950524     IFNARBY00
007800950525     I              ARBA01                      CDF(1)
007900950525     I              ARBA02                      CDF(2)
008000950525     I              ARBA03                      CDF(3)
008100950525     I              ARBA04                      CDF(4)
008200950525     I              ARBA05                      CDF(5)
008300950525     I              ARBA06                      CDF(6)
008400950525     I              ARBA07                      CDF(7)
008500950525     I              ARBA08                      CDF(8)
008600950525     I              ARBA09                      CDF(9)
008700950525     I              ARBA10                      CDF(10)
008800950524     C*--------------------------------------------------------------*
008900950524     C*  Ciclo principale
009000950524     C*--------------------------------------------------------------*
009100950613     C*  Richiamo pgm x eventuale sblocco distinte
009200950613     C                   MOVEL     FNLR59        KPJBU
009300950613     C                   CALL      'FNLR60R1'
009400950613     C                   PARM                    KPJBA
009500950613     C                   MOVEL     KPJBU         FNLR59
009600950613     C                   SETON                                        40
009700950613     C*  Imposto la chiave per il posizionamento
009800950524     C                   Z-ADD     4             KNPG
009900950524     C                   Z-ADD     D59FGS        KFGS
010000950524      *  Se ho selezionato una sola distinta mi posiziono direttamente
010100950524      *  su quella
010200950613     C     D59NDD        IFNE      *ZEROS                                       ---- 01 ----
010300950524     C     D59NDD        ANDEQ     D59NDA
010400950613     C                   SETOFF                                       40
010500000114     C                   Z-ADD     D59NDD        KNFV
010600000114     C                   Z-ADD     D59NDD        KNFV1
010700950628     C     KFVV1         CHAIN     FNFVV01L                           31
010800950605     C                   MOVEL     'S'           WNWDOC            1
010900950628     C  N31              EXSR      GESDIS
011000950613     C                   ELSE                                                   ---- 01 ----
011100950524      *  Altrimenti leggo in FNFVV tutti le distinte comprese entro i
011200950524      *  limiti scelti
011300950524     C                   Z-ADD     D59DDT        KDFV
011400950628     C     KFVV2         SETLL     FNFVV02L
011500950628     C     KFVV2         READE     FNFVV02L                               31
011600950613     C     *IN31         DOWEQ     '0'                                          ---- 02 ----
011700950524      * Controllo se distinta rientra nei limiti indicati
011800950613     C     FVVNFV        IFGE      D59NDD                                       ---- 03 ----
011900950613     C     FVVNFV        ANDLE     D59NDA                                       NR distinte +
012000950524     C     FVVFCF        ANDNE     'S'                                          F.V.chiuso +
012100950602     C     FVVATB        ANDNE     '*'                                          F.V.chiuso +
012200950613     C     D59FPD        IFEQ      *BLANKS                                      ---- 04 ----
012300950613     C     D59FPD        OREQ      FVVFPP                                       Tipo prest.
012400950605     C                   MOVEL     'S'           WNWDOC            1
012500950524     C                   Z-ADD     FVVNFV        KNFV
012600000114     C                   Z-ADD     FVVNFV        KNFV1
012700950524     C                   EXSR      GESDIS
012800950606     C                   MOVEL     'N'           D59NWP
012900950613     C     D59STP        IFNE      'A'                                          ---- 05 ----
013000950612     C     D59LIN        ORGT      60
013100950612     C     D59LIN        ORGT      54
013200950612     C     D59STP        ANDEQ     'A'
013300950606     C                   MOVEL     'S'           D59NWP
013400950613     C                   END                                                    ---- 05 ----
013500950613     C                   END                                                    ---- 04 ----
013600950613     C                   END                                                    ---- 03 ----
013700950628     C     KFVV2         READE     FNFVV02L                               31
013800950613     C                   END                                                    ---- 02 ----
013900950524     C*
014000950613     C                   END                                                    ---- 01 ----
014100950911     C*
014200950911     C*  Richiamo pgm. per l'ultima volta x farli chiudere in LR
014300950911     C                   EXSR      CLOFIL
014400950525     C*
014500950524     C                   SETON                                        LR
014600950524     C*--------------------------------------------------------------*
014700950524     C*  Gestione singola distinta
014800950524     C*--------------------------------------------------------------*
014900950524     C     GESDIS        BEGSR
015000950524     C*
015100950530     C*  Decodifico Padroncino
015200950605     C                   MOVEL     'N'           D59ANM
015300950530     C                   Z-ADD     0             STCPDR
015400950530     C                   MOVEL     *BLANKS       STDPDR
015500950530     C                   Z-ADD     FVVNFV        STCNDC
015600950530     C                   MOVEL     FVVFPP        TPPRE
015700950530     C                   Z-ADD     FVVDFV        G02INV
015800950530     C                   MOVEL     '3'           G02ERR
015900950530     C                   CALL      'XSRDA8'
016000950530     C                   PARM                    WLBDA8
016100950530     C                   Z-ADD     G02DAT        STCDDC            8 0
016200950612     C                   Z-ADD     0             DSTNBL
016300950530     C     KDST          CHAIN     FNDST01L                           34
016400950602     C                   MOVEL     ' '           APDFCM
016500950612     C                   Z-ADD     DSTNBL        D59NBL
016600950530     C  N34DSTPDR        CHAIN     FNAPD01L                           34
016700950530     C     *IN34         IFEQ      '0'
016800000516     C     D59TPD        IFNE      'T'
016900000628     C     *IN40         ANDEQ     '1'
017000000516     C                   EXSR      PADRO
017100000516     C     D59TPD        IFNE      §TPDTI
017200000516     C     T02ERR        ORNE      *BLANK
017300000628     C*  Sblocco le distinte
017400000628     C                   MOVEL     ' '           FVVFCF
017500000628     C  N40              EXCEPT    AGGFL1
017600000628     C   40              EXCEPT    AGGFL2
017700000516     C                   GOTO      FINDIS
017800000516     C                   END
017900000516     C                   END
018000950530     C                   Z-ADD     DSTPDR        STCPDR
018100950530     C                   MOVEL     APDRSC        STDPDR
018200950530     C                   END
018300950613     C*  Controllo se FNFVV è bloccato da una preparazione da spunte
018400950613     C     FVVFCF        IFEQ      '§'
018500950613     C                   Z-ADD     FVVNFV        D59NDT
018600000114     C     KBRV          CHAIN     FIBRV01L                           33
018700950524     C     *IN33         IFEQ      '0'
018800950613     C*  Richiamo pgm x eventuale calcolo stop
018900950613     C                   MOVEL     FNLR59        KPJBU
019000950613     C                   CALL      'FNLR60R3'
019100950613     C                   PARM                    KPJBA
019200950613     C                   MOVEL     KPJBU         FNLR59
019300950524     C*  Loop per esame dettaglio spunta
019400950524     C     *IN33         DOWEQ     '0'
019500950524     C                   EXSR      DETBRV
019600000114     C     KBRV          READE     FIBRV01L                               33
019700950524     C                   END
019800950524     C*  A fine spunta richiamo pgm x stampa anomalie/agg.FNARB
019900950525     C                   MOVE      APDPDR        D59PDR
020000950525     C                   MOVE      FVVFPP        D59FPP
020100950525     C                   MOVEL     APDRSC        D59DSF
020200950525     C                   MOVEL     FNLR59        KPJBU
020300950524     C                   CALL      'FNLR60R2'
020400950524     C                   PARM                    KPJBA
020500950605     C                   MOVEL     KPJBU         FNLR59
020600950612     C     D59NWP        IFEQ      ' '
020700950612     C                   MOVEL     ' '           WPRIMA
020800950612     C                   END
020900950613     C*  Sblocco le distinte
021000950613     C                   MOVEL     ' '           FVVFCF
021100950613     C  N40              EXCEPT    AGGFL1
021200950613     C   40              EXCEPT    AGGFL2
021300950524     C*  Se richiesta stampa distinta richiamo pgm stampa
021400950524     C     D59STP        IFEQ      ' '
021500010216     C     D59SOR        OREQ      'S'
021600010604     C                   CLEAR                   W55STP
021700950524     C                   EXSR      CALL55
021800950524     C                   END
021900950613     C*
022000950613     C     D59STP        IFNE      'A'
022100950613     C                   MOVEL     'S'           D59NWP
022200950613     C                   END
022300950613     C*
022400010613     C                   ELSE                                                   *IN33 = '0'
022500950613     C*  Sblocco le distinte
022600950613     C                   MOVEL     ' '           FVVFCF
022700950613     C  N40              EXCEPT    AGGFL1
022800950613     C   40              EXCEPT    AGGFL2
022900950613     C                   END                                                    Trovate spunte
023000010604     C*
023100010613     C                   ELSE                                                   FVVFCF = '§'
023200010604      *  Non ha spunte ma stampo se è stata richiesta
023300010604      *  la stampa delle distinte ORM
023400010604     C     D59SOR        IFEQ      'S'
023500010604     C     D59STP        ANDEQ     ' '
023600010604     C                   MOVEL     'O'           W55STP
023700010604     C                   Z-ADD     FVVNFV        D59NDT
023800010604     C                   EXSR      CALL55
023900010604     C                   END
024000950613     C*
024100950613     C                   END                                                    FVVFCF = '§'
024200950524     C*
024300950524     C     FINDIS        ENDSR
024400000516     C*--------------------------------------------------------------*
024500000516     C*  CONTROLLO PADRONCINO SE RICHIESTO A VIDEO
024600000516     C*--------------------------------------------------------------*
024700000516     C     PADRO         BEGSR
024800000516      * DECODIFICO IL TIPO PADRONCINO
024900000516     C                   MOVEL     APDZNP        TIPOP
025000000516     C                   CLEAR                   DTPD
025100000516     C                   CLEAR                   TIBS02
025200000516     C                   MOVEL     'C'           T02MOD
025300000516     C                   MOVEL     KNSIF         T02SIF
025400000516     C                   MOVEL     'TPD'         T02COD
025500000516     C                   MOVEL     TIPOP         T02KE1
025600000516      *
025700000516     C                   CALL      'TIBS02R'
025800000516     C                   PARM                    KPJBA
025900000516     C                   PARM                    TIBS02
026000000516      *
026100000516     C     T02ERR        IFEQ      *BLANKS
026200000516     C                   MOVEL     T02UNI        DTPD
026300000516     C                   ENDIF
026400000516     C                   ENDSR
026500950524     C*--------------------------------------------------------------*
026600950524     C*  Dettaglio singola spunta
026700950524     C*--------------------------------------------------------------*
026800950524     C     DETBRV        BEGSR
026900950524     C*
027000950530     C                   Z-ADD     0             X                 3 0
027100950524     C                   CLEAR                   FNARBY00
027200950530     C                   MOVEA     *BLANKS       CDA
027300950524     C*-----------------------------------------------------*
027400950524     C*  E R R O R I      B L O C C A N T I  ! ! ! ! !
027500950524     C*-----------------------------------------------------*
027600950609     C*  CONTROLLO SE SPARATO COLLO DI ALTRA FILIALE
027700950609     C     BRVLNA        LOOKUP    £6                                     39
027800950609     C     *IN39         IFEQ      '0'
027900950609     C                   SETON                                        36
028000950609     C                   MOVEL     'S'           D59ANM
028100950612     C     D59LIN        IFGT      60
028200950609     C     WNWDOC        OREQ      'S'
028300950612     C     D59LIN        ANDGT     54
028400950613     C     WNWDOC        OREQ      'S'
028500950613     C     D59STP        ANDNE     'A'
028600950612     C     WPRIMA        OREQ      'S'
028700950612     C                   MOVEL     ' '           WPRIMA
028800950609     C                   ADD       1             D59PAG
028900950609     C                   EXCEPT    TESTA
029000950609     C                   EXCEPT    TESTA1
029100950612     C                   Z-ADD     8             D59LIN
029200950609     C                   MOVEL     'N'           WNWDOC
029300950609     C                   END
029400950609     C     WNWDOC        IFEQ      'S'
029500950622     C                   EXCEPT    TESTA1
029600950622     C                   ADD       6             D59LIN
029700950609     C                   MOVEL     'N'           WNWDOC
029800950609     C                   END
029900950609     C                   EXCEPT    ERRFIL
030000950612     C                   ADD       2             D59LIN
030100950609     C                   MOVEL     ' '           D59NWP
030200950609     C                   ELSE
030300950524     C*  Mi posiziono dalle spunte sul dettaglio segnacolli
030400950524     C                   Z-ADD     BRVLNP        KFLS
030500950524     C                   Z-ADD     BRVLNA        KLNA
030600950524     C                   Z-ADD     BRVNRS        KNRS
030700950524     C                   Z-ADD     BRVNSC        KNSC
030800950524     C     KART          CHAIN     FNART27L                           34
030900950524     C     *IN34         IFEQ      '1'
031000950609     C                   SETON                                        36
031100950605     C                   MOVEL     'S'           D59ANM
031200950612     C     D59LIN        IFGT      60
031300950613     C     WNWDOC        OREQ      'S'
031400950613     C     D59STP        ANDNE     'A'
031500950605     C     WNWDOC        OREQ      'S'
031600950612     C     D59LIN        ANDGT     54
031700950612     C     WPRIMA        OREQ      'S'
031800950612     C                   MOVEL     ' '           WPRIMA
031900950605     C                   ADD       1             D59PAG
032000950605     C                   EXCEPT    TESTA
032100950605     C                   EXCEPT    TESTA1
032200950612     C                   Z-ADD     8             D59LIN
032300950605     C                   MOVEL     'N'           WNWDOC
032400950605     C                   END
032500950605     C     WNWDOC        IFEQ      'S'
032600950622     C                   EXCEPT    TESTA1
032700950622     C                   ADD       6             D59LIN
032800950605     C                   MOVEL     'N'           WNWDOC
032900950605     C                   END
033000950524     C                   EXCEPT    ERRSGN
033100950612     C                   ADD       2             D59LIN
033200950529     C                   MOVEL     ' '           D59NWP
033300950529     C                   ELSE
033400950524     C*  Se trovato controllo spedizione
033500950529     C     WNSP          IFNE      ARTNSP
033600950529     C     WNRS          ORNE      ARTNRS
033700950529     C     WLNP          ORNE      ARTLNP
033800950529     C     WAAS          ORNE      ARTAAS
033900950529     C                   Z-ADD     ARTAAS        WAAS
034000950529     C                   Z-ADD     ARTLNP        WLNP
034100950529     C                   Z-ADD     ARTNRS        WNRS
034200950529     C                   Z-ADD     ARTNSP        WNSP
034300950529     C                   Z-ADD     ARTAAS        KAAS
034400950529     C                   Z-ADD     ARTLNP        KLNP
034500950529     C                   Z-ADD     ARTNRS        KNRS
034600950529     C                   Z-ADD     ARTNSP        KNSP
034700950525     C     KARB          CHAIN     FNARB01L                           36
034800950530     C                   MOVEL     ARBFBC        OLDFBC
034900950524     C*  Manca bolla
035000950525     C     *IN36         IFEQ      '1'
035100950525     C                   Z-ADD     ARTAAS        ARBAAS
035200950525     C                   Z-ADD     ARTLNP        ARBLNP
035300950525     C                   Z-ADD     ARTNRS        ARBNRS
035400950525     C                   Z-ADD     ARTNSP        ARBNSP
035500950524     C                   ADD       1             X
035600950524     C                   MOVEL     '01'          CDA(X)
035700950524     C                   GOTO      FINBRV
035800950524     C                   END
035900950524     C*  Bolla già consegnata
036000950524     C     ARBDCM        IFNE      0
036100950524     C                   ADD       1             X
036200950524     C                   MOVEL     '02'          CDA(X)
036300950524     C     X             IFEQ      10
036400950524     C                   GOTO      FINBRV
036500950524     C                   END
036600950524     C                   END
036700950524     C*  Controllo se spedizione mamma
036800950524     C                   Z-ADD     ARTAAS        KAAP
036900950524     C                   Z-ADD     ARTLNP        KLPP
037000950524     C                   Z-ADD     ARTNRS        KNRP
037100950529     C                   Z-ADD     ARTNSP        KNSP1
037200950524     C                   MOVEL     'N'           WTRBOL            1
037300950529     C                   MOVEL     'N'           WBOLOK            1
037400950524     C     KLBL          SETGT     FNLBL02L
037500950524     C     KLBL          READPE    FNLBL02L                               34
037600950524     C*  Se lo è cerco l'ultima bolla figlia sbloccata da consegnare
037700950524     C     *IN34         IFEQ      '0'
037800950524     C                   EXSR      CHKFGL
037900950524     C                   END
038000950524     C*  Se esistono figlie ma sono tutte bloccate emetto msg. errore
038100950524     C     WTRBOL        IFEQ      'S'
038200950529     C     WBOLOK        ANDEQ     'N'
038300950615     C                   MOVEL     *BLANKS       OLDFBC
038400950524     C                   ADD       1             X
038500950615     C*  SE NON HO TROVATO FIGLIE MI RIPOSIZIONO SU MAMMA
038600950615     C     *IN36         IFEQ      '1'
038700950615     C                   MOVEL     '14'          CDA(X)
038800950615     C                   Z-ADD     ARTAAS        KAAS
038900950615     C                   Z-ADD     ARTLNP        KLNP
039000950615     C                   Z-ADD     ARTNRS        KNRS
039100950615     C                   Z-ADD     ARTNSP        KNSP
039200950615     C     KARB          CHAIN     FNARB01L                           36
039300950615     C                   ELSE
039400950615     C                   MOVEL     '04'          CDA(X)
039500950615     C                   END
039600950524     C     X             IFEQ      10
039700950524     C                   GOTO      FINBRV
039800950524     C                   END
039900950524     C                   END
040000950524     C*  Se bolla non legata controllo se bloccata
040100950524     C     WTRBOL        IFEQ      'N'
040200950605     C     ARBFBC        ANDNE     *BLANKS
040300950525     C                   MOVEL     'N'           WFIGL             1            bolla figlia
040400950524     C                   EXSR      SBLOCC
040500970318     C                   EXSR      CHI140
040600950524     C     ARBFBC        IFNE      *BLANKS
040700950530     C                   MOVEL     *BLANKS       OLDFBC
040800950530     C                   ADD       1             X
040900950524     C                   MOVEL     '04'          CDA(X)
041000950524     C     X             IFEQ      10
041100950524     C                   GOTO      FINBRV
041200950524     C                   END
041300950524     C                   END
041400950524     C                   END
041500950529     C*  Bolla in consegna su altra distinta
041600950529     C     ARBNDC        IFNE      0
041700950531     C     ARBNDC        ANDNE     FVVNFV
041800950530     C     ARBDCM        ANDEQ     0
041900950529     C                   ADD       1             X
042000950529     C                   MOVEL     '03'          CDA(X)
042100950529     C     X             IFEQ      10
042200950529     C                   GOTO      FINBRV
042300950529     C                   END
042400950529     C                   END
042500950524     C*  Controllo se porto assegnato non tassato
042600950524     C     ARBACA        IFNE      'S'
042700950524     C                   ADD       1             X
042800950524     C                   MOVEL     '05'          CDA(X)
042900950524     C     X             IFEQ      10
043000950524     C                   GOTO      FINBRV
043100950524     C                   END
043200950524     C                   END
043300950526     C*-----------------------------------------------------*
043400950526     C*  Se non ci sono errori bloccanti procedo eventuale
043500950526     C*  calcolo degli stop.
043600950526     C                   EXSR      CTRSTP
043700950524     C*-----------------------------------------------------*
043800950524     C*  E R R O R I    N O N    B L O C C A N T I  ! ! ! ! !
043900950524     C*-----------------------------------------------------*
044000950525     C     OLDFBC        IFNE      *BLANK
044100950524     C     ARBFBC        ANDEQ     *BLANK
044200950524     C                   ADD       1             X
044300950524     C                   MOVEL     '06'          CDA(X)
044400950524     C     X             IFEQ      10
044500950524     C                   GOTO      FINBRV
044600950524     C                   END
044700950524     C                   END
044800950524     C*  Spedizione da abilitare all'incasso C/ass.
044900950524     C     ARBACC        IFNE      'S'
045000950524     C                   MOVEL     'S'           ARBACC
045100950524     C                   ADD       1             X
045200950524     C                   MOVEL     '07'          CDA(X)
045300950524     C     X             IFEQ      10
045400950524     C                   GOTO      FINBRV
045500950524     C                   END
045600950524     C                   END
045700950524     C*  Spedizione da abilitare alla consegna merce
045800950524     C     ARBAMA        IFEQ      0
045900950524     C                   Z-ADD     3             ARBAMA
046000950703     C***                  ADD  1         X
046100950703     C****                 MOVEL'08'      CDA,X
046200950703     C***        X         IFEQ 10
046300950703     C***                  GOTO FINBRV
046400950703     C***                  END
046500950524     C                   END
046600950524     C*  Consegnata sped. fermo deposito con padroncino no 9999
046700950524     C     ARBFFD        IFEQ      'S'
046800950602     C     APDFCM        IFNE      'S'
046900950524     C                   ADD       1             X
047000950524     C                   MOVEL     '09'          CDA(X)
047100950524     C     X             IFEQ      10
047200950524     C                   GOTO      FINBRV
047300950524     C                   END
047400950524     C                   END
047500950524     C                   END
047600950524     C*  Controllo se spedizione consegnata in anticipo
047700950524     C     ARBDCR        IFGE      D59DDT
047800950524     C     ARBTCR        ANDEQ     'D'
047900950524     C     ARBDCR        ORGT      D59DDT
048000950524     C     ARBTCR        ANDEQ     ' '
048100950524     C                   ADD       1             X
048200950524     C                   MOVEL     '10'          CDA(X)
048300950524     C     X             IFEQ      10
048400950524     C                   GOTO      FINBRV
048500950524     C                   END
048600950524     C                   END
048700950524     C*  Calcolo turno chiusura
048800950524     C                   EXSR      TURNO
048900950524     C*
049000950525     C*  Aggiorno FNARB x sblocco + abiltazione C/A-merce
049100950525     C     FINBRV        TAG
049200950525     C  N36              EXCEPT    AGGARB
049300950525     C     KARB          CHAIN     FNARBY1L                           33
049400950525     C                   MOVEL     CDA           CDF
049500950525     C   33              WRITE     FNARBY00
049600950525     C  N33              UPDATE    FNARBY00
049700950529     C                   END
049800950525     C*
049900950529     C                   END
050000950609     C*
050100950609     C                   END
050200950529     C*
050300950529     C                   ENDSR
050400950526     C*-----------------------------------------------------*
050500950526     C* Gestione eventuale attribuzione Stop                *
050600950526     C*-----------------------------------------------------*
050700950526     C     CTRSTP        BEGSR
050800951121     C                   MOVEL     *BLANKS       KRAG
050900951211     C                   Z-ADD     0             KKSC              7 0
051000951211     C                   Z-ADD     0             ARBKCS
051100951211     C*  Controllo se devo attribuire lo Stop
051200951211     C                   Z-ADD     1             Y                 2 0
051300951211     C                   MOVEL     *BLANKS       WTBOL1            1
051400951211     C     ARBCBO        LOOKUP    C3A(Y)                                 30
051500951211     C     *IN30         IFEQ      '1'
051600951211     C                   MOVEL     D3A(Y)        DS3A
051700951211     C                   MOVEL     §3ATB1        WTBOL1
051800951211     C     WTBOL1        IFEQ      'A'
051900951211     C                   Z-ADD     ARBKSC        KKSC
052000951211     C                   END
052100951211     C                   END
052200951211     C*
052300951211     C     *IN30         IFEQ      '1'
052400990921     C     WTBOL1        IFEQ      'A'
052500990921     C     §3ATB2        ORNE      *BLANKS
052600990921     C     KARB          CHAIN     FIAR601L                           30
052700990921     C  N30              Z-ADD     AR6KSC        KKSC
052800951211     C                   END
052900951211     C                   END
053000951211     C*  Controllo se cliente no generico e se si attribuisco stop
053100951211     C                   Z-ADD     KKSC          APPOG             4 0
053200951211     C     KKSC          IFNE      0
053300951211     C     APPOG         ANDNE     9999
053400951211     C     APPOG         ANDNE     8888
053500951211     C                   Z-ADD     KKSC          ARBKCS
053600951211     C                   MOVEL     KKSC          KRAG
053700951211     C                   ELSE
053800951211     C*  Se no assegnato codificato lavoro con rag.sociale/indirizzo
053900951121     C                   MOVEL     ARBRSD        KRAG
054000951121     C                   MOVE      ARBIND        KRAG
054100951211     C                   END
054200951211     C*  Attribuisco lo Stop
054300951121     C     KRAG          CHAIN     FNARBZ0F                           30
054400951121     C*  Controllo se cliente esiste già
054500950526     C     *IN30         IFEQ      '1'
054600950526     C                   ADD       1             D59STO
054700950526     C                   Z-ADD     D59STO        ARBSTP
054800951121     C                   MOVEL     KRAG          ARBRAG
054900950526     C                   WRITE     FNARBZ00
055000950526     C                   ELSE
055100950526     C                   EXCEPT    LIBSTP
055200950526     C                   END
055300950526     C*
055400950526     C                   ENDSR
055500950525     C*--------------------------------------------------------------*
055600950525     C*  Cerco figlia
055700950525     C*--------------------------------------------------------------*
055800950525     C     CHKFGL        BEGSR
055900950524     C*  Se bolla con C/assegno controllo se da abilitare
056000950529     C     WBOLOK        DOWEQ     'N'
056100950530     C     *IN34         ANDEQ     '0'
056200950615     C     *IN36         ANDEQ     '0'
056300950524     C     LBLLAP        IFEQ      LBLLAN
056400950524     C                   MOVEL     'S'           WTRBOL
056500950524     C                   Z-ADD     LBLAAN        KAAS
056600950524     C                   Z-ADD     LBLLPN        KLNP
056700950524     C                   Z-ADD     LBLNRN        KNRS
056800950524     C                   Z-ADD     LBLNSN        KNSP
056900950609     C     KARB          CHAIN     FNARB01L                           36
057000950609     C     *IN36         IFEQ      '0'
057100950530     C     ARBNDC        ANDNE     9999999
057200950531     C                   MOVEL     ARBFBC        OLDFBC
057300950524     C*  Se bolla bloccata controllo se sbloccabile
057400950525     C     ARBFBC        IFNE      *BLANKS
057500950525     C                   MOVEL     'S'           WFIGL                          bolla figlia
057600950525     C                   EXSR      SBLOCC
057700950530     C                   END
057800950524     C*  Se sbloccata metto in consegna figlia
057900970318     C                   EXSR      CHI140
058000950524     C     ARBFBC        IFEQ      ' '
058100950524     C                   MOVEL     'S'           WBOLOK
058200950524     C                   END
058300950530     C*
058400950530     C                   END
058500950530     C                   END
058600950524     C*  Se non trovata bolla loop su successiva
058700950524     C     WBOLOK        IFEQ      'N'
058800950615     C     KLBL          READPE    FNLBL02L                               34
058900950524     C                   END
059000950525     C*
059100950525     C                   END
059200950525     C*
059300950525     C                   ENDSR
059400950525     C*--------------------------------------------------------------*
059500950525     C*  Sblocco spedizione se possibile
059600950525     C*--------------------------------------------------------------*
059700950525     C     SBLOCC        BEGSR
059800950529     C*
059900950529     C     ARBFBC        IFEQ      'A'
060000950525     C     ARBFBC        OREQ      'B'
060100950525     C                   MOVEL     *BLANKS       ARBFBC
060200950712     C                   MOVEL     *BLANKS       ARBCMC
060300950525     C                   ELSE
060400950525     C     ARBFBC        IFEQ      'G'
060500950525     C*  Verifico se posso sbloccare la spedizione
060600950525     C     WFIGL         IFEQ      'S'
060700990713     C     KGCA2         CHAIN     FIGCA01L                           35
060800950525     C     *IN35         IFEQ      '0'
060900950525     C     GCPFAS        ANDNE     39
061000950525     C                   GOTO      FINSBL
061100950525     C                   END
061200950525     C                   ELSE
061300990713     C     KGCA1         CHAIN     FIGCA01L                           35
061400950525     C     *IN35         IFEQ      '0'
061500950525     C     GCPFAS        ANDLT     35
061600950525     C                   GOTO      FINSBL
061700950525     C                   END
061800950525     C                   END
061900950525     C                   MOVEL     *BLANKS       ARBFBC
062000950712     C                   MOVEL     *BLANKS       ARBCMC
062100950525     C                   END
062200950525     C*
062300950525     C                   END
062400970318     C*
062500970318     C     FINSBL        ENDSR
062600970318     C*--------------------------------------------------------------*
062700970318     C*  Call a pgm Chiusura anomalia 140
062800970318     C*--------------------------------------------------------------*
062900970318     C     CHI140        BEGSR
063000970318     C*
063100950918     C*
063200950918     C*  Se ho trovato i segnacolli vado a chiudere le anomalie
063300960415     C                   CLEAR                   DSLR33
063400960415     C                   MOVE      '4'           D33FSC
063500960415     C                   Z-ADD     D59DDT        D33DCH
063600960415     C                   MOVEL     'DI'          D33CCH
063700960415     C*
063800950918     C                   Z-ADD     ARTLNP        KLNP2
063900950918     C                   Z-ADD     ARTNRS        KNRS2
064000950918     C                   Z-ADD     ARTNSP        KNSP2
064100950918     C                   Z-ADD     ARTAAS        KAAS2
064200950918     C                   Z-ADD     140           KCAA
064300990519     C     KANM          CHAIN     FNANM01L                           37
064400950918     C     *IN37         DOWEQ     '0'
064500960415     C                   Z-ADD     ANMNR1        D33NRR
064600960415     C                   CALL      'FNLR33R'
064700960415     C                   PARM                    DSLR33
064800990519     C     KANM          READE     FNANM01L                               37
064900950918     C                   END
065000950525     C*
065100970318     C                   ENDSR
065200950525     C*--------------------------------------------------------------*
065300950525     C*  Call a pgm stampa distinta
065400950525     C*--------------------------------------------------------------*
065500950525     C     CALL55        BEGSR
065600950525     C*
065700950525     C                   CLEAR                   FNLR55
065800950529     C                   MOVEL     KPJBU         WKPJBU          256
065900950605     C                   MOVEL     D59ANM        D55ANM
066000950525     C                   Z-ADD     D59DDT        D55DAT
066100950525     C                   Z-ADD     D59NDT        D55NRD
066200950525     C                   Z-ADD     D59NDT        D55NRA
066300950525     C                   MOVEL     'A'           D55TIP
066400950525     C                   MOVEL     D59FGS        D55FIL
066500950911     C                   Z-ADD     DSTNBN        D55NBN
066600950911     C                   MOVEL     DSTNOT        D55NOT
066700010216      *--
066800010604     C     W55STP        IFEQ      *BLANKS
066900010216     C     D59STP        IFEQ      'A'
067000010216     C     D59SOR        ANDEQ     'S'
067100010216     C                   MOVEL     'O'           D55STP
067200010216     C                   ENDIF
067300010216     C     D59STP        IFEQ      ' '
067400010216     C     D59SOR        ANDEQ     'S'
067500010216     C                   MOVEL     'E'           D55STP
067600010216     C                   ENDIF
067700010216     C     D59STP        IFEQ      ' '
067800010216     C     D59SOR        ANDEQ     'N'
067900010216     C                   MOVEL     'C'           D55STP
068000010216     C                   ENDIF
068100010604     C                   ELSE
068200010604     C                   MOVEL     W55STP        D55STP
068300010604     C                   ENDIF
068400010216      *--
068500950525     C                   MOVEL     FNLR55        KPJBU
068600950525     C                   CALL      'FNLR55R'
068700950525     C                   PARM                    KPJBA
068800950529     C                   MOVEL     WKPJBU        KPJBU
068900950525     C*
069000950525     C                   ENDSR
069100950525     C*--------------------------------------------------------------*
069200950525     C*  Gestisco turno di chiusura
069300950525     C*--------------------------------------------------------------*
069400950525     C     TURNO         BEGSR
069500950525     C*
069600950525     C     ARBGC1        IFNE      *BLANKS
069700950525     C                   MOVEL     ARBGC1        WGIOR             1
069800950525     C     WGIOR         IFEQ      WGG
069900950525     C                   ADD       1             X
070000950525     C                   MOVEL     '11'          CDA(X)
070100950531     C                   GOTO      FINTUR
070200950525     C                   END
070300950531     C                   END
070400950531     C*
070500950525     C     ARBGC2        IFNE      *BLANKS
070600950531     C                   MOVEL     ARBGC2        WGIOR             1
070700950525     C     WGIOR         IFEQ      WGG
070800950525     C                   ADD       1             X
070900950525     C                   MOVEL     '11'          CDA(X)
071000950525     C                   END
071100950525     C                   END
071200950525     C*
071300950531     C     FINTUR        ENDSR
071400950911     C*--------------------------------------------------------------*
071500950911     C*  Richiamo pgm x ultima volta
071600950911     C*--------------------------------------------------------------*
071700950911     C     CLOFIL        BEGSR
071800950911     C*
071900950911     C                   MOVEL     'U'           D59TPR
072000950911     C                   MOVEL     'U'           D55TPR
072100950911     C                   MOVEL     FNLR59        KPJBU
072200950911     C                   CALL      'FNLR60R1'
072300950911     C                   PARM                    KPJBA
072400950911     C                   CALL      'FNLR60R3'
072500950911     C                   PARM                    KPJBA
072600950911     C                   CALL      'FNLR60R2'
072700950911     C                   PARM                    KPJBA
072800950911     C                   MOVEL     FNLR55        KPJBU
072900950911     C                   CALL      'FNLR55R'
073000950911     C                   PARM                    KPJBA
073100960415     C*
073200960415     C                   MOVEL     'C'           D33TLA
073300960415     C                   CALL      'FNLR33R'
073400960415     C                   PARM                    DSLR33
073500950911     C*
073600950911     C                   ENDSR
073700950525     C*--------------------------------------------------------------*
073800950525     C*  Operazioni iniziali
073900950525     C*--------------------------------------------------------------*
074000950525     C     *INZSR        BEGSR
074100950525     C*
074200950525     C     *ENTRY        PLIST
074300950525     C                   PARM                    KPJBA
074400950525     C                   MOVEL     KPJBU         FNLR59
074500950525     C*------------------------------------------------------*
074600950525     C*  Richiamo XPARUT
074700950525     C                   Z-ADD     1             CODUT
074800950525     C                   CALL      'X§PARUT'
074900950525     C                   PARM                    UT§DSE
075000950525     C                   MOVEL     REC80         CNCR80
075100950525     C                   MOVEL     RAGUT         RSUT             20
075200950605     C                   Z-ADD     0             D59PAG
075300950525     C*------------------------------------------------------*
075400950525     C*  Definisco chiavi di accesso
075500950525     C     KFVV1         KLIST
075600950525     C                   KFLD                    KNPG
075700950525     C                   KFLD                    KNFV
075800950525     C                   KFLD                    KFGS
075900950525     C     KFVV2         KLIST
076000950525     C                   KFLD                    KNPG
076100950525     C                   KFLD                    KDFV
076200950525     C                   KFLD                    KFGS
076300950525     C     KDST          KLIST
076400950525     C                   KFLD                    KNFV
076500950525     C                   KFLD                    KFGS
076600950525     C     KBRV          KLIST
076700950525     C                   KFLD                    KNPG
076800000114     C                   KFLD                    KNFV1
076900950525     C                   KFLD                    KFGS
077000950525     C     KART          KLIST
077100950525     C                   KFLD                    KFLS
077200950525     C                   KFLD                    KLNA
077300950525     C                   KFLD                    KNRS
077400950525     C                   KFLD                    KNSC
077500950918     C     KANM          KLIST
077600950918     C                   KFLD                    KAAS2
077700950918     C                   KFLD                    KLNP2
077800950918     C                   KFLD                    KNRS2
077900950918     C                   KFLD                    KNSP2
078000960529     C                   KFLD                    KCAA
078100950525     C     KARB          KLIST
078200950525     C                   KFLD                    KAAS
078300950525     C                   KFLD                    KLNP
078400950525     C                   KFLD                    KNRS
078500950525     C                   KFLD                    KNSP
078600950525     C     KLBL          KLIST
078700950525     C                   KFLD                    KAAP
078800950525     C                   KFLD                    KLPP
078900950525     C                   KFLD                    KNRP
079000950529     C                   KFLD                    KNSP1
079100950525     C     KGCA1         KLIST
079200950525     C                   KFLD                    KAAS
079300950525     C                   KFLD                    KLNP
079400950525     C                   KFLD                    KNRS
079500950525     C                   KFLD                    KNSP
079600950525     C     KGCA2         KLIST
079700950525     C                   KFLD                    KAAP
079800950525     C                   KFLD                    KLPP
079900950525     C                   KFLD                    KNRP
080000950529     C                   KFLD                    KNSP1
080100950525     C     KCLN          KLIST
080200950525     C                   KFLD                    KTFP
080300950525     C                   KFLD                    KTFA
080400950525     C                   KFLD                    KANN
080500950525     C                   KFLD                    KMES
080600950526     C     KTAB          KLIST
080700950526     C                   KFLD                    KKUT
080800950526     C                   KFLD                    KCOD
080900950609     C     KTAB2         KLIST
081000950609     C                   KFLD                    KKUT
081100950609     C                   KFLD                    KCOD
081200950609     C                   KFLD                    KKEY
081300950525     C*------------------------------------------------------*
081400950525     C*  Definisco variabili
081500950525     C     *LIKE         DEFINE    FVVNPG        KNPG
081600950525     C     *LIKE         DEFINE    FVVNFV        KNFV
081700000114     C     *LIKE         DEFINE    BRVNFV        KNFV1
081800950525     C     *LIKE         DEFINE    FVVFGS        KFGS
081900950525     C     *LIKE         DEFINE    FVVDFV        KDFV
082000950525     C     *LIKE         DEFINE    ARTFLS        KFLS
082100950525     C     *LIKE         DEFINE    ARTLNA        KLNA
082200950525     C     *LIKE         DEFINE    ARTNRS        KNRS
082300950525     C     *LIKE         DEFINE    ARTNSC        KNSC
082400950525     C     *LIKE         DEFINE    ARTAAS        KAAS
082500950525     C     *LIKE         DEFINE    ARTLNP        KLNP
082600950525     C     *LIKE         DEFINE    ARTNSP        KNSP
082700950529     C     *LIKE         DEFINE    ARTNSP        KNSP1
082800950525     C     *LIKE         DEFINE    LBLAAP        KAAP
082900950525     C     *LIKE         DEFINE    LBLLPP        KLPP
083000950525     C     *LIKE         DEFINE    LBLNRP        KNRP
083100950525     C     *LIKE         DEFINE    CLNTFP        KTFP
083200950525     C     *LIKE         DEFINE    CLNTFA        KTFA
083300950525     C     *LIKE         DEFINE    CLNANN        KANN
083400950525     C     *LIKE         DEFINE    CLNMES        KMES
083500950526     C     *LIKE         DEFINE    TBLKUT        KKUT
083600950526     C     *LIKE         DEFINE    TBLCOD        KCOD
083700950609     C     *LIKE         DEFINE    TBLKEY        KKEY
083800951121     C     *LIKE         DEFINE    ARBRAG        KRAG
083900950525     C     *LIKE         DEFINE    FVVNFV        STCNDC
084000950525     C     *LIKE         DEFINE    FVVFPP        TPPRE
084100950525     C     *LIKE         DEFINE    DSTPDR        STCPDR
084200950525     C     *LIKE         DEFINE    APDRSC        STDPDR
084300950525     C     *LIKE         DEFINE    ARBFBC        OLDFBC
084400950529     C     *LIKE         DEFINE    ARBAAS        WAAS
084500950529     C     *LIKE         DEFINE    ARBLNP        WLNP
084600950529     C     *LIKE         DEFINE    ARBNRS        WNRS
084700950529     C     *LIKE         DEFINE    ARBNSP        WNSP
084800950918     C     *LIKE         DEFINE    ANMAAS        KAAS2
084900950918     C     *LIKE         DEFINE    ANMLNP        KLNP2
085000950918     C     *LIKE         DEFINE    ANMNRS        KNRS2
085100950918     C     *LIKE         DEFINE    ANMNSP        KNSP2
085200950918     C     *LIKE         DEFINE    ANMCAA        KCAA
085300010604     C     *LIKE         DEFINE    D55STP        W55STP
085400950525     C*------------------------------------------------------*
085500950525     C*  Controllo sul calendario il tipo di giorno che è la
085600950525     C*  data di consegna
085700950525     C                   Z-ADD     D59DDT        WMMGG             4 0
085800950525     C                   Z-ADD     D59DDT        WG                2 0
085900950525     C                   MOVEL     D59DDT        KANN
086000950525     C                   Z-ADD     0             KTFP
086100950525     C                   Z-ADD     0             KTFA
086200950525     C                   MOVEL     WMMGG         KMES
086300950525     C                   OPEN      AZCLN01L
086400950525     C     KCLN          CHAIN     AZCLN01L                           31
086500950525     C                   MOVEA     CLNMAT        MES
086600950525     C                   MOVE      MES(WG)       WGG               1
086700950525     C                   CLOSE     AZCLN01L
086800950525     C*------------------------------------------------------*
086900950525     C*  Inizializzo variabili
087000950530     C                   MOVEL     *ALL'-'       CMP132          132
087100950605     C                   MOVEL     *ALL'*'       CM1132          132
087200950525     C                   TIME                    TIMES             6 0
087300960927     C                   TIME                    WHDAT            12 0
087400960927     C                   MOVE      WHDAT         WOGGI             6 0
087500951211     C*  Carico dati tabella 3A
087600951211     C                   OPEN      TABEL00F
087700951211     C                   Z-ADD     1             KKUT
087800951211     C                   MOVEL     '3A'          KCOD
087900951211     C                   Z-ADD     0             X
088000951211     C     KTAB          CHAIN     TABEL00F                           31
088100951211     C     *IN31         DOWEQ     '0'
088200951211     C     X             ANDLT     100
088300951211     C                   ADD       1             X
088400951211     C                   MOVEL     TBLKEY        C3A(X)
088500951211     C                   MOVEL     TBLUNI        D3A(X)
088600951211     C     KTAB          READE     TABEL00F                               31
088700951211     C                   END
088800950609     C*  CARICO£6
088801020506     c                   clear                   trul06ds
088802020506     c                   eval      d06cod = '£6'
088803020506     c                   movel     d59fgs        d06key
088804020506     c                   movel     trul06ds      kpjbu
088805020506     c                   call      'TRUL06R'
088806020506     c                   parm                    kpjba
088807020506     c                   movel     kpjbu         trul06ds
088808020506     c                   movea     lin           £6
088900020506     C*                  MOVEL     *BLANKS       KKEY
089000020506     C*                  MOVEL     D59FGS        KKEY
089100020506     C*                  MOVEL     '£6'          KCOD
089200020506     C*                  Z-ADD     0             X
089300020506     C*    KTAB2         CHAIN     TABEL00F                           31
089400020506     C*    *IN31         IFEQ      '0'
089500020506     C*    TBLFLG        ANDEQ     ' '
089600020506     C*                  MOVEA     TBLUNI        UNI
089700020506     C*                  MOVE      UNI           £6
089800020506     C*                  ELSE
089900020506     C*                  MOVEA     *ZEROS        £6
090000020506     C*                  MOVE      D59FGS        £6(1)
090100020506     C*                  END
090200950526     C                   CLOSE     TABEL00F
090300950531     C*
090400950531     C                   OPEN      QSYSPRT
090500950606     C                   MOVEL     'S'           D59NWP
090600950612     C                   MOVEL     'S'           WPRIMA            1
090700950612     C                   Z-ADD     0             D59LIN
090800950525     C*
090900950525     C                   ENDSR
091000950525     O*------------------------------------------------------*
091100950525     OQSYSPRT   E            TESTA          0 01
091200950705     O***                      RSUT      20
091300950525     O                                           21 '/'
091400950525     O                       SIMFEL              24
091500950525     O                                           53 '***  STAMPA ANOMALIE PRE'
091600950525     O                                              'PARAZIONE DISTINTA DI CO'
091700950525     O                                              'NSEGNA DALLE SPUNTE  ***'
091800960927     O                                          111 'FNLR60R'
091900960927     O                       WOGGI         Y    121
092000950525     O                                          127 'Pag.'
092100950605     O                       D59PAG        Z    132
092200950525     O          E            TESTA          0 02
092300950525     O                       KNSIF               10
092400950525     O                       KNMUS               21
092500960927     O                       TIMES              121 '  .  .  '
092600950605     O          E            TESTA1      2  1
092700950605     O                       CM1132             132
092800950605     O          E            TESTA1         1
092900960607     O                                              'AUTOTRASPORTATORE:'
093000950525     O                       STCPDR        Z   +  1
093100950525     O                       STDPDR            +  1
093200950525     O                                         +  5 'N.'
093300950525     O                       STCNDC        Z   +  1
093400950525     O                                         +  2 'DEL'
093500950525     O                       STCDDC        8   +  1
093600950525     O                                         +  2 'TIPO PRESTAZIONE:'
093700950525     O                       TPPRE             +  1
093800950605     O          E            TESTA1         1
093900950525     O                                            7 'TP.ANO'
094000950525     O                                           12 'ANNO'
094100950525     O                                           16 'LNP'
094200950525     O                                           27 'SPEDIZIONE'
094300950525     O                                           55 'A N O M A L I A'
094400950525     O                                          116 'INSERIMENTO'
094500950605     O          E            TESTA1         1
094600950525     O                       CMP132             132
094700950525     O          E            ERRSGN         2
094800950526     O                                            4 'B'
094900950525     O                                           27 '++++ +++ ++ +++++++'
095000951031     O                                           53 'MANCA RECORD BOLLA DEL C'
095100950525     O                                              'OLLO NR.'
095200950525     O                       BRVLNP        Z   +  1
095300950525     O                                              '/'
095400950525     O                       BRVLNA        Z
095500950525     O                       BRVNRS        Z   +  1
095600950525     O                                              '-'
095700950525     O                       BRVNSC        Z
095800950525     O                                         +  1 'PISTOLA NR.'
095900950525     O                       BRVNPS            +  1
096000950525     O                                          114 'DA SPUNTE'
096100950609     O          E            ERRFIL         2
096200950609     O                                            4 'B'
096300950609     O                                           27 '++++ +++ ++ +++++++'
096400960927     O                                           53 'SPUNTATO COLLO DI ALTRO '
096500960927     O                                              'P.O.    '
096600950609     O                       BRVLNP        Z   +  1
096700950609     O                                              '/'
096800950609     O                       BRVLNA        Z
096900950609     O                       BRVNRS        Z   +  1
097000950609     O                                              '-'
097100950609     O                       BRVNSC        Z
097200950609     O                                         +  1 'PISTOLA NR.'
097300950609     O                       BRVNPS            +  1
097400950609     O                                          114 'DA SPUNTE'
097500950525     OFNARB000  E            AGGARB
097600950525     O                       ARBACC
097700950525     O                       ARBAMA
097800950525     O                       ARBFBC
097900950712     O                       ARBCMC
098000950613     OFNFVV001  E            AGGFL1
098100950613     O                       FVVFCF
098200950613     OFNFVV000  E            AGGFL2
098300950918     O                       FVVFCF
098400950918     OFNARBZ00  E            LIBSTP
