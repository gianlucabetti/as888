000100950915     H*------------------------------------------------------------------------*
000200950915     H* CREAZIONE GRIGLIA                                              NO      *
000300950915     H*------------------------------------------------------------------------*
000400950915     H DECEDIT('0,') DATEDIT(*DMY.)
000500950915     F*------------------------------------------------------------------------*
000600950915     FTNTLT01L  IF   E           K DISK    USROPN
000700950915     FTNTLT02L  IF   E           K DISK    USROPN
000800950915     F                                     RENAME(TNTLT000:TNTLT2)
000900950920     FTNTLR01L  IF   E           K DISK
001000951124     FTNTLR03L  IF   E           K DISK
001100951124     F                                     RENAME(TNTLR000:TNTLR3)
001200950920     FTNTLM01L  IF   E           K DISK
001300951123     F*TNTLL01LIF  E           K        DISK
001400950921     FTNTST01L  IF A E           K DISK
001500950921     FTNTSR01L  IF A E           K DISK
001600950921     FTNTSM01L  IF A E           K DISK
001700951030     FTABEL00F  IF   E           K DISK
001800951205     FAZCLN01L  IF   E           K DISK
001900950920     D CAR             S              3  0 DIM(200)                             FILIALI CARICO
002000950920     D NFP             S              3  0 DIM(200)                             N.FERMATE PART.
002100951218     D DTT             S              8  0 DIM(200)                             DATE F.V.TEOR.
002200960301     D CAS             S              3  0 DIM(200)                             FILIALI CARICO
002300960301     D NFS             S              3  0 DIM(200)                             N.FERMATE PART.
002400960301     D DTS             S              8  0 DIM(200)                             DATE F.V.TEOR.
002500950921     D ALF             S              1    DIM(20) CTDATA PERRCD(20)            *ALFABETO (1÷20)
002600951002     D DAT             S              8  0 DIM(31)                              DATE DA ELAB.
002700951002     D NGG             S              5  0 DIM(31)                              DATE IN GIORNI
002800951002     D GGS             S              1    DIM(31)                              GG.SETTIMANA
002900960115     D S1Y             S              1    DIM(10)                              SIMBOLI CALEND
003000960115     D FET             S              1    DIM(10)                              FLAG.FESTIVO
003100960301     D SCA             S              4    DIM(200)                             FIL.SCAR.ANAGRAF
003200960304     D NFA             S              3  0 DIM(200)                             N.FERM.ARRIVO
003300950915     D WLBDA8          DS
003400950915     D  G08DAT                 1      8  0
003500950915     D  G08INV                 9     16  0
003600950915     D  G08ERR                17     17
003700950915     D  G08TGI                18     22  0
003800950921     D WGIDAT          DS
003900950921     D  GI8DAT                 1      8  0
004000950921     D  GI8INV                 9     16  0
004100950921     D  GI8TGI                17     21  0
004200951206     D                 DS
004300951206     D  CLNPOM                 1     31
004400951206     D  POM                    1     31
004500951205     D                                     DIM(31)                              FESTIV.POMER.
004600950922     D KPJBA         E DS
004700951030     D DSTV          E DS
004800960117     D DS1Y          E DS
004900010725      * ds per divisa
005000010725     d dged          e ds
005100010725     d tibs02ds      e ds
005200010725
005300951030     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005400960118     D* DS PER PASSAGGIO PARAMETRI A PGM TNTL35R - CALCOLO DATA TEORICA
005500960118     D DSTL35        E DS                  EXTNAME(TNTL35DS)
005600960118     D*
005700950915     D PARAM           DS
005800950915     D* DATA TRAINO DAL
005900950915     D  PARDTD                 1      8  0
006000950915     D* DATA TRAINO AL
006100950915     D  PARDTA                 9     16  0
006200950915     D* NUMERO TRAINO
006300950915     D  PARTRN                17     23  0
006400950915     D* TERMINAL DI PARTENZA
006500950915     D  PARTFP                24     26  0
006600951002     D* DATA DECORRENZA TRAINO
006700951002     D  PARDDE                27     34  0
006800951205     D* CREAZIONE SE GIORNO FESTIVO (' '=No controllo festivo
006900951205     D*                             ('S'=Controllo festivo e crea solo
007000951205     D*                                  se non festivo)
007100951205     D  PARFES                35     35
007200950915     I*
007300950922     C****************************************************************
007400950922     C*  RIEPILOGO INDICATORI
007500950922     C***************************************************************
007600950922     C* 01    - CREAZIONE GRIGLIA PER NUMERO TRAINO
007700950922     C* 02    - CREAZIONE GRIGLIA PER DATA
007800950922     C* 03    - CREAZIONE GRIGLIA PER TERMINAL DI PARTENZA/DATA
007900950922     C* 29    - GESTIONE LETTURA TNTLT01L
008000950922     C* 30    - COMODO
008100950922     C***************************************************************
008200951002     C* Se ho ricevuto numero traino/data decorrenza ed una sola data
008300960122     C* traino controllo solo (se richiesto) se quest'ultima è festiva
008400960122     C* (gli altri controlli sono già stati fatti dal chiamante)
008500951002     C                   SELECT
008600951002     C     PARDDE        WHENNE    *ZEROS
008700951002     C     PARTRN        ANDNE     *ZEROS
008800951002     C     PARDTD        ANDEQ     PARDTA
008900951002     C     KTLT1A        CHAIN     TNTLT01L                           30
009000951002     C     *IN30         IFEQ      *OFF
009100951115     C                   MOVE      PARDTD        WDTN
009200951115     C                   MOVE      WTGID         WTGI
009300960122    3C     PARFES        IFEQ      'S'
009400960122     C                   MOVEL     WDTN          KANN
009500960122     C                   MOVE      WDTN          W0040             4 0
009600960122     C                   MOVEL     W0040         KMES
009700960122     C                   MOVE      W0040         GG                2 0
009800960122     C                   EXSR      CTRFES
009900960122     C* Creo griglia solo se non festivo
010000960122    4C     WFES          CASEQ     ' '           CREAG1
010100960122    4C                   END
010200960122     C                   ELSE
010300960122     C                   EXSR      CREAG1
010400960122    3C                   END
010500951002     C                   END
010600951002     C                   OTHER
010700960314     C                   CLEAR                   WBIS
010800960314     C     RIPETI        TAG
010900950918     C* Posizionamento per numero traino/data decorrenza
011000950922     C   01KTLT1         SETLL     TNTLT01L                               29
011100950918     C* Posizionamento per " "/terminal partenza/data scadenza
011200950922     C   03KTLT2         SETLL     TNTLT02L
011300950918     C*
011400960314     C   02WBIS          SETLL     TNTLT02L
011500950918     C*
011600950922     C   01
011700950922     CAN 29KTLT1         READE     TNTLT01L                               30
011800950922     C   01
011900950922     CANN29PARTRN        READPE    TNTLT01L                               30
012000950922     C   03KTLT2B        READE     TNTLT02L                               30
012100960314     C   02WBIS          READE     TNTLT02L                               30
012200950915     C     *IN30         DOWEQ     *OFF
012300950922     C* ESCLUDO ANNULLATI
012400960220     C     TLTATB        CABNE     *BLANKS       LEGGI
012500960424     C* SE GIRO PER BIS ESCLUDO QUELLI < 9000000
012600960424     C     WBIS          IFEQ      'B'
012700960424     C     TLTTRN        ANDLT     9000000
012800960424     C                   GOTO      LEGGI
012900960424     C                   END
013000950915     C*
013100950915     C     *IN01         IFEQ      *ON
013200950922     C     *IN29         ANDEQ     *OFF
013300950918     C* VERIFICO SE SCADUTO
013400950919     C     TLTDSC        ANDLT     PARDTD
013500950915     C                   GOTO      LEGGI
013600950915     C                   END
013700950922     C   29              SETOFF                                       29
013800950918     C     *IN02         IFEQ      *ON
013900950922     C     TLTDSC        ANDLT     PARDTD
014000950919     C     KTLT2C        SETLL     TNTLT02L
014100950919     C                   GOTO      LEGGI
014200950919     C                   END
014300950920     C* VERIFICO CHE SIA DECORRENTE
014400950922     C     *IN02         IFEQ      *ON
014500950922     C     *IN03         OREQ      *ON
014600950919     C     TLTDDE        IFGT      PARDTA
014700950919     C                   GOTO      LEGGI
014800950919     C                   END
014900950919     C                   END
015000951030     C* VERIFICO CHE PER IL TIPO TRAINO LA GRIGLIA SI CREABILE
015100951030     C                   MOVEL     'TV'          KCOD
015200951030     C                   MOVEL(P)  TLTTTR        KKEY
015300951122     C                   MOVEL     'S'           §TVUTC
015400951030     C     KTAB          CHAIN     TABEL00F                           30
015500951030     C     *IN30         IFEQ      *OFF
015600951030     C     TBLFLG        ANDEQ     *BLANKS
015700951030     C                   MOVEL     TBLUNI        DSTV
015800951030     C                   END
015900951030     C     §TVUTC        CABEQ     *BLANKS       LEGGI
016000950921     C* SOLO LA PRIMA VOLTA RICHIAMO ROUTINE DI CARICAMENTO DATE
016100951002     C     PARDTD        IFNE      PARDTA
016200951002     C     VOLTA         ANDEQ     ' '
016300950921     C                   EXSR      CARDAT
016400950921     C                   MOVE      '1'           VOLTA             1
016500950921     C                   END
016600950915     C                   EXSR      ELAB
016700950915     C*
016800950915     C     LEGGI         TAG
016900950919     C*
017000950918     C   01PARTRN        READE     TNTLT01L                               30
017100950919     C   01
017200950919     CANN30TLTDDE        COMP      PARDTA                             30
017300950919     C*
017400950922     C   03KTLT2B        READE     TNTLT02L                               30
017500960314     C   02WBIS          READE     TNTLT02L                               30
017600950915     C                   ENDDO
017700960314     C* SE CREAZIONE GRIGLIA PER DATA (NON DI UN TRAINO SPECIFICO)
017800960424     C* RIPETO IL GIRO PER CREARE LA GRIGLIA ANCHE PER I TRAINI DI
017900960424     C* FILIALE BIS (TRAINI BIS >= 9000000)
018000960314     C     *IN02         IFEQ      *ON
018100960314     C     *IN03         OREQ      *ON
018200960314     C     WBIS          IFEQ      *BLANKS
018300960424     C                   MOVE      'B'           WBIS
018400960314     C                   GOTO      RIPETI
018500960314     C                   END
018600960314     C                   END
018700951002     C                   ENDSL
018800950915     C*
018900950915     C                   SETON                                        LR
019000960111     C************  ELABORAZIONE TESTATA TRAINO  **********************
019100950919     C     ELAB          BEGSR
019200950919     C* Verifico se griglia creabile per la DATA DA richiesta
019300951115    1C     TLTDDE        IFLE      PARDTD
019400951115     C     WGIOSD        SCAN      TLTGSE                                 30
019500950920    2C     *IN30         IFEQ      '1'
019600951115     C                   MOVE      PARDTD        WDTN
019700951115     C                   MOVE      WTGID         WTGI
019800951205     C* Se richiesto, controllo che non sia un giorno festivo
019900951205    3C     PARFES        IFEQ      'S'
020000960115     C                   MOVEL     WDTN          KANN
020100960115     C                   MOVE      WDTN          W0040             4 0
020200960115     C                   MOVEL     W0040         KMES
020300960115     C                   MOVE      W0040         GG                2 0
020400951205     C                   EXSR      CTRFES
020500951205     C* Creo griglia solo se non festivo
020600951205    4C     WFES          CASEQ     ' '           CREAG1
020700951205    4C                   END
020800951205     C                   ELSE
020900951205     C                   EXSR      CREAG1
021000951205    3C                   END
021100950920    2C                   END
021200950920    1C                   END
021300950919     C* Se è stata richiesta più di una data ripeto il giro per tutte
021400950919     C* le date richieste
021500950919    1C     PARDTD        IFNE      PARDTA
021600951002     C                   Z-ADD     2             XX
021700951002    2C     XX            DOWLE     31
021800950928     C     DAT(XX)       ANDNE     *ZEROS
021900950920     C* Verifico se griglia creabile
022000960315     C     TLTDDE        IFLE      DAT(XX)
022100960315     C     TLTDSC        ANDGE     DAT(XX)
022200950921     C     GGS(XX)       SCAN      TLTGSE                                 30
022300950918     C* RICHIAMO CREAZIONE GRIGLIA
022400950920    4C     *IN30         IFEQ      '1'
022500950927     C                   Z-ADD     DAT(XX)       WDTN                           DATA TRAINO
022600950927     C                   Z-ADD     NGG(XX)       WTGI                           DATA IN GG
022700951205     C* Se richiesto, controllo che non sia un giorno festivo
022800951205    5C     PARFES        IFEQ      'S'
022900960115     C                   MOVEL     WDTN          KANN
023000960115     C                   MOVE      WDTN          W0040             4 0
023100960115     C                   MOVEL     W0040         KMES
023200960115     C                   MOVE      W0040         GG                2 0
023300951205     C                   EXSR      CTRFES
023400951205     C* Creo griglia solo se non festivo
023500951205    6C     WFES          CASEQ     ' '           CREAG1
023600951205    6C                   END
023700951205   X5C                   ELSE
023800951205     C                   EXSR      CREAG1
023900951205    5C                   END
024000950920    4C                   END
024100950920    3C                   END
024200950921     C                   ADD       1             XX
024300950919    2C                   ENDDO
024400950919    1C                   END
024500950919     C                   ENDSR
024600950920     C****************  CREAZIONE GRIGLIA 1  **************************
024700950920     C     CREAG1        BEGSR
024800951006     C**** CREO LA GRIGLIA SOLO SE NON ESISTE LA TESTATA STORICO TRAINI
024900950920     C     KTST          SETLL     TNTST01L                               30
025000951006    1C     *IN30         IFEQ      *OFF
025100950921     C**** STORICO FERMATE
025200960301     C*
025300960312     C                   CLEAR                   WTFM
025400950920     C     KTLR          SETLL     TNTLR01L
025500950920     C     KTLR          READE     TNTLR01L                               30
025600951006    2C     *IN30         DOWEQ     *OFF
025700950921     C* Escludo record annullati
025800960220    3C     TLRATB        IFEQ      *BLANKS
025900951006    4C                   SELECT
026000960304     C* E' una FERMATA DI SOLO CARICO:
026100950920     C     TLRSCA        WHENEQ    ' '
026200960312     C* Pulizie a rottura di tipo fermata
026300960312     C     TLRTFM        IFNE      WTFM
026400960312     C                   Z-ADD     1             II                3 0
026500960312     C                   MOVE      *ZEROS        IS                3 0
026600960312     C                   MOVEA     *ZEROS        CAR
026700960312     C                   MOVEA     *ZEROS        NFP
026800960312     C                   MOVEA     *ZEROS        DTT
026900960312     C                   MOVEA     *ZEROS        CAS
027000960312     C                   MOVEA     *ZEROS        NFS
027100960312     C                   MOVEA     *ZEROS        DTS
027200960312     C                   CLEAR                   SCA
027300960312     C                   CLEAR                   NFA
027400960312     C                   MOVE      *ZEROS        SS                3 0
027500960312     C                   MOVE      TLRTFM        WTFM
027600960312     C                   END
027700960115     C     TLRGFV        IFEQ      'A'
027800960117     C* ... Se giorno apertura FV = A (cioè = data traino):
027900960117     C* ... memorizzo il carico, il n. fermata e la data FV teorica
028000960115     C                   MOVE      WDTN          DTT(II)
028100960115     C                   MOVE      TLRFIL        CAR(II)
028200960115     C                   MOVE      TLRNFM        NFP(II)
028300960115     C                   ADD       1             II
028400960115     C                   ELSE
028500960117     C* ... Se giorno apertura FV > A (cioè > di data traino):
028600960117     C* ... calcolo la data FV teorica
028700960112     C                   MOVE      TLRGFV        WALF
028800960112     C                   EXSR      SRDAT
028900960117     C* ... Se la data FVT calcolata non è festiva memorizzo il carico,
029000960117     C* ... il n. fermata e la data FV teorica
029100960115     C     WFES          IFEQ      *BLANKS
029200960117     C                   MOVE      WGFV          DTT(II)
029300960115     C                   MOVE      TLRFIL        CAR(II)
029400960115     C                   MOVE      TLRNFM        NFP(II)
029500960115     C                   ADD       1             II
029600960112     C                   END
029700960115     C                   END
029800950920     C*
029900960304     C* E' una FERMATA DI SCARICO:
030000950920     C     TLRSCA        WHENEQ    'S'
030100960301     C*
030200950920     C                   Z-ADD     1             II
030300960304     C                   MOVE      *BLANKS       WTROV             1
030400951006    5C     II            DOWLE     200
030500950928     C     CAR(II)       ANDNE     *ZEROS
030600960117     C* ... verifico nei mc se occorre scrivere griglia per i carichi
030700960117     C* ... memorizzati
030800960328     C     KTLM          SETLL     TNTLM01L
030900960328     C     KTLM          READE     TNTLM01L                               30
031000960328     C     *IN30         DOWEQ     *OFF
031100960328     C     TLMATB        ANDNE     *BLANKS
031200960328     C     KTLM          READE     TNTLM01L                               30
031300960328     C                   ENDDO
031400960328    6C     *IN30         IFEQ      *OFF
031500960328     C     TLMATB        ANDEQ     *BLANKS
031600960328     C*      Memorizzo che ho trovato almeno un carico con Mc
031700960328     C                   MOVE      '1'           WTROV
031800950920     C                   EXSR      CREAG2
031900960301     C                   ELSE
032000960301     C* ... mc non trovati: salvo il carico che è sicuramente relativo
032100960301     C* ... ad una successiva fermata di scarico
032200960304     C     TLRCAR        IFEQ      'S'
032300960301     C                   ADD       1             IS                3 0
032400960301     C                   MOVE      DTT(II)       DTS(IS)
032500960301     C                   MOVE      CAR(II)       CAS(IS)
032600960301     C                   MOVE      NFP(II)       NFS(IS)
032700960304     C                   END
032800951006    6C                   END
032900950920     C                   ADD       1             II
033000960304    5C                   ENDDO
033100960304     C* Se non ho trovato nemmeno un carico con Mc memorizzo la filiale
033200960304     C* di scarico
033300960304     C     WTROV         IFEQ      *BLANKS
033400960304     C                   ADD       1             SS
033500960304     C                   MOVEL     TLRFIL        SCA(SS)
033600960304     C                   MOVE      TLRTFM        SCA(SS)
033700960304     C                   MOVE      TLRNFM        NFA(SS)
033800960304     C                   END
033900960304     C*
034000951006    5C     TLRCAR        IFEQ      'S'
034100950920     C* CARICA ANCHE: memorizzo il nuovo carico dopo aver cancellato
034200960301     C*               i carichi in memoria e ricaricato quelli senza mc
034300960115    6C     TLRGFV        IFEQ      'A'
034400960301     C* Pulizie
034500960301     C                   EXSR      PULCAM
034600960301     C                   ADD       1             II
034700950920     C                   MOVE      TLRFIL        CAR(II)
034800950920     C                   MOVE      TLRNFM        NFP(II)
034900951023     C                   MOVE      WDTN          DTT(II)
035000960115     C                   ADD       1             II
035100960115   X6C                   ELSE
035200951019     C                   MOVE      TLRGFV        WALF
035300951019     C                   EXSR      SRDAT
035400960115    7C     WFES          IFEQ      *BLANKS
035500960301     C* Pulizie
035600960301     C                   EXSR      PULCAM
035700960301     C                   ADD       1             II
035800960115     C                   MOVE      TLRFIL        CAR(II)
035900960115     C                   MOVE      TLRNFM        NFP(II)
036000960117     C                   MOVE      WGFV          DTT(II)
036100960115     C                   ADD       1             II
036200960115    7C                   END
036300960115    6C                   END
036400960115    5C                   END
036500951006    4C                   ENDSL
036600951006    3C                   END
036700950920     C     KTLR          READE     TNTLR01L                               30
036800951006    2C                   ENDDO
036900960304     C* VERIFICO SE CI SONO FERMATE DI SCARICO ANCORA DA SCRIVERE
037000960304    2C     SS            IFGT      *ZEROS
037100960304     C                   Z-ADD     1             SS
037200960304    3C     SCA(SS)       DOWNE     *BLANKS
037300960304     C     SS            ANDLE     200
037400960304     C* Cerco una fermata di carico con Mc per questo scarico
037500960304     C                   MOVE      SCA(SS)       KTFM
037600960304     C                   MOVEL     SCA(SS)       KSCA
037700960304     C                   CLEAR                   WTROV
037800960304     C     KTLR3B        SETLL     TNTLR03L
037900960304     C     KTLR3B        READE     TNTLR03L                               30
038000960304    4C     *IN30         DOWEQ     *OFF
038100960304    5C     TLRCAR        IFEQ      'S'
038200960304     C* Verifico se ci sono Mc
038300960328     C     KTLMB         SETLL     TNTLM01L
038400960328     C     KTLMB         READE     TNTLM01L                               30
038500960328     C     *IN30         DOWEQ     *OFF
038600960328     C     TLMATB        ANDNE     *BLANKS
038700960328     C     KTLMB         READE     TNTLM01L                               30
038800960328     C                   ENDDO
038900960304    6C     *IN30         IFEQ      *OFF
039000960304     C     TLMATB        ANDEQ     *BLANKS
039100960304     C* Imposto campi per poter eseguire creag2
039200960304     C                   Z-ADD     1             II
039300960304     C                   MOVE      TLRNFM        NFP(II)
039400960304     C                   MOVE      NFA(SS)       TLRNFM
039500960304     C                   MOVE      TLRFIL        CAR(II)
039600960304     C                   MOVE      KSCA          TLRFIL
039700960304     C* . . . . . . . . Determino data fv teorico
039800960304    7C     TLRGFV        IFEQ      'A'
039900960304     C                   MOVE      WDTN          DTT(II)
040000960304     C                   EXSR      CREAG2
040100960304     C                   MOVE      '1'           WTROV
040200960304   X7C                   ELSE
040300960304     C                   MOVE      TLRGFV        WALF
040400960304     C                   EXSR      SRDAT
040500960304    8C     WFES          IFEQ      *BLANKS
040600960304     C                   MOVE      WGFV          DTT(II)
040700960304     C                   EXSR      CREAG2
040800960304     C                   MOVE      '1'           WTROV
040900960304    8C                   END
041000960304    7C                   END
041100960304    6C                   END
041200960304    5C                   END
041300960304    5C     WTROV         IFEQ      *BLANKS
041400960304     C     KTLR3B        READE     TNTLR03L                               30
041500960304     C                   ELSE
041600960304     C                   SETON                                        30
041700960304    5C                   END
041800960304    4C                   ENDDO
041900960304     C                   ADD       1             SS
042000960304    3C                   ENDDO
042100960304    2C                   END
042200950921     C**** STORICO METRI CUBI
042300950921     C     KTLM1         SETLL     TNTLM01L
042400950921     C     KTLM1         READE     TNTLM01L                               30
042500951006    2C     *IN30         DOWEQ     *OFF
042600950921     C* Escludo annullati
042700960220    3C     TLMATB        IFEQ      *BLANKS
042800950921     C     KTSM          SETLL     TNTSM01L                               30
042900951006    4C     *IN30         IFEQ      *OFF
043000950921     C                   CLEAR                   TNTSM000
043100950921     C                   MOVE      TLMTRN        TSMTRN
043200950927     C                   MOVE      WDTN          TSMDTN
043300950921     C                   MOVE      TLMTFP        TSMTFP
043400950921     C                   MOVE      TLMTFA        TSMTFA
043500950921     C                   MOVE      TLMTFF        TSMTFF
043600950921     C                   MOVE      TLMMCA        TSMMCA
043700960321     C                   MOVE      TLMMCT        TSMMCT
043800960424     C                   MOVE      TLMMCB        TSMMCB
043900950921     C                   WRITE     TNTSM000
044000951006    4C                   END
044100951006    3C                   END
044200950921     C     KTLM1         READE     TNTLM01L                               30
044300951006    2C                   ENDDO
044400951006     C**** STORICO TRAINI
044500951006     C                   CLEAR                   TNTST000
044600951006     C                   MOVE      TLTTRN        TSTTRN
044700951006     C                   MOVE      WDTN          TSTDTN
044800951006     C                   MOVE      TLTTFP        TSTTFP
044900951006     C                   MOVE      TLTTFA        TSTTFA
045000951006     C                   MOVEL     TLTTTR        TSTTTR
045100951006     C                   MOVEL     TLTBIS        TSTBIS
045200951026     C                   MOVE      TLTFES        TSTFES
045300951124     C* Determino numero fermata della filiale che espone importo sul
045400951124     C* f.v.: se fes è presente sia in andata che in ritorno prendo
045500951124     C*       il numero fermata di andata
045600951124     C                   MOVE      *ZEROS        TSTNFE
045700951124     C                   MOVEL     'R'           KTFM
045800951124     C     KTLR3         CHAIN     TNTLR03L                           30
045900951124     C  N30              MOVE      TLRNFM        TSTNFE
046000951124     C                   MOVEL     'A'           KTFM
046100951124     C     KTLR3         CHAIN     TNTLR03L                           30
046200951124     C  N30              MOVE      TLRNFM        TSTNFE
046300951123     C*          *IN30     IFEQ *OFF
046400951123     C*                    MOVELTLLTMZ    TSTTMZ
046500951123     C*                    MOVE TLLPDR    TSTPDR
046600951123     C*                    MOVE TLLILI    TSTILI
046700951123     C*                    MOVE TLLIES    TSTIES
046800951123     C*                    MOVELTLLCAU    TSTCAU
046900951123     C*                    END
047000010725      * Divisa
047100010725     C                   CLEAR                   tibs02ds
047200010725
047300010725     C                   MOVEL     'L'           T02TLA
047400010725     C                   MOVEL     'C'           T02MOD
047500010725     C                   MOVEL     KNSIF         T02SIF
047600010725     C                   MOVEL     'GED'         T02COD
047700010725     C                   MOVEL     '1'           T02KE1
047800010725
047900010725     C                   CALL      'TIBS02R'
048000010725     C                   PARM                    KPJBA
048100010725     C                   PARM                    tibs02ds
048200010725
048300010725     c                   movel     T02UNI        DGED
048400010725     c                   if        wdtn < §gedd2
048500010725     c                   eval      tstdiv = §gedt1
048600010725     c                   else
048700010725     c                   eval      tstdiv = §gedt2
048800010725     c                   endif
048900010725
049000951006     C                   WRITE     TNTST000
049100951006    1C                   END
049200950920     C                   ENDSR
049300950921     C***********  CREAZIONE GRIGLIA 2  (PER STORICO FERMATE)  ********
049400950920     C     CREAG2        BEGSR
049500950921     C     KTSR          SETLL     TNTSR01L                               30
049600950922    1C     *IN30         IFEQ      *OFF
049700950920     C* Scrittura storico fermate
049800950921     C                   CLEAR                   TNTSR000
049900950921     C                   MOVE      TLRTRN        TSRTRN
050000950927     C                   MOVE      WDTN          TSRDTN
050100951031     C                   MOVE      DTT(II)       TSRDTT
050200950921     C                   MOVE      NFP(II)       TSRNFP
050300950921     C                   MOVE      TLRNFM        TSRNFA
050400950921     C                   MOVE      CAR(II)       TSRFFP
050500950921     C                   MOVEL     TLRFIL        TSRFFA
050600960301     C                   MOVEL     TLRTFM        TSRTFM
050700960118     C* Ricavo data/ora PARTENZA
050800960118     C                   CLEAR                   DSTL35
050900960118     C                   MOVEL     'D'           D35TUP
051000960118     C                   MOVEL     'P'           D35EPA
051100960118     C                   MOVE      TLRTRN        D35TRN
051200960118     C                   MOVE      TLRDDE        D35DDE
051300960118     C                   MOVE      TSRDTN        D35DTN
051400960118     C                   MOVE      TSRNFP        D35NFM
051500960118     C                   MOVEL     DSTL35        KPJBU
051600960118     C                   CALL      'TNTL35R'
051700960118     C                   PARM                    KPJBA
051800960118     C                   MOVEL     KPJBU         DSTL35
051900960118     C     D35ERR        IFEQ      'E'
052000960118     C                   CLEAR                   TSRDTP
052100960118     C                   CLEAR                   TSRHTP
052200960118     C                   ELSE
052300960118     C                   MOVE      D35DTE        TSRDTP
052400960118     C                   MOVE      D35HTE        TSRHTP
052500960118     C                   END
052600960118     C* Ricavo data/ora ARRIVO
052700960118     C                   CLEAR                   DSTL35
052800960118     C                   MOVEL     'D'           D35TUP
052900960118     C                   MOVEL     'A'           D35EPA
053000960118     C                   MOVE      TLRTRN        D35TRN
053100960118     C                   MOVE      TLRDDE        D35DDE
053200960118     C                   MOVE      TSRDTN        D35DTN
053300960118     C                   MOVE      TSRNFA        D35NFM
053400960118     C                   MOVEL     DSTL35        KPJBU
053500960118     C                   CALL      'TNTL35R'
053600960118     C                   PARM                    KPJBA
053700960118     C                   MOVEL     KPJBU         DSTL35
053800960118     C     D35ERR        IFEQ      'E'
053900960118     C                   CLEAR                   TSRDTA
054000960118     C                   CLEAR                   TSRHTA
054100960118     C                   ELSE
054200960118     C                   MOVE      D35DTE        TSRDTA
054300960118     C                   MOVE      D35HTE        TSRHTA
054400960118     C                   END
054500960118     C*
054600950921     C                   WRITE     TNTSR000
054700950922    1C                   END
054800950920     C                   ENDSR
054900960301     C*************  PULIZIA CAMPI DI CALCOLO  ************************
055000960301     C     PULCAM        BEGSR
055100960301     C                   MOVEA     *ZEROS        CAR
055200960301     C                   MOVEA     *ZEROS        NFP
055300960301     C                   MOVEA     *ZEROS        DTT
055400960301     C                   MOVEA     CAS           CAR
055500960301     C                   MOVEA     NFS           NFP
055600960301     C                   MOVEA     DTS           DTT
055700960301     C                   MOVEA     *ZEROS        CAS
055800960301     C                   MOVEA     *ZEROS        NFS
055900960301     C                   MOVEA     *ZEROS        DTS
056000960301     C                   Z-ADD     IS            II
056100960301     C                   MOVE      *ZEROS        IS
056200960301     C                   ENDSR
056300950921     C*************  CARICAMENTO DATE DA ELABORARE  *******************
056400950921     C     CARDAT        BEGSR
056500950921     C                   MOVEA     *ZEROS        DAT                            DATE DA ELAB
056600950921     C                   MOVEA     *ZEROS        NGG                            DATE IN GG.
056700950921     C                   MOVEA     *BLANKS       GGS                            DAY OF WEEK
056800951115     C                   MOVE      WTGID         WTGI
056900950921     C*
057000951002     C                   Z-ADD     1             XX
057100951002    2C     DAT(XX)       DOWLT     PARDTA
057200950921     C                   CLEAR                   WGIDAT
057300951114     C     WTGI          ADD       XX            GI8TGI
057400951116     C                   Z-ADD     GI8TGI        COMTGI
057500950921     C                   CALL      'XSRGI8'
057600950921     C                   PARM                    WGIDAT
057700950921     C                   MOVEL     GI8DAT        W006A             6
057800950922     C                   MOVE      GI8DAT        W002A             2
057900950922     C                   MOVE      W002A         W006A
058000950921     C                   CALL      'XGIOSE1'
058100950921     C                   PARM                    W006A
058200950921     C                   PARM                    GIOSET
058300951002     C                   ADD       1             XX
058400950921     C                   MOVE      GI8INV        DAT(XX)
058500951116     C                   MOVE      COMTGI        NGG(XX)
058600950922     C                   MOVE      GIOSET        GGS(XX)
058700950921    2C                   ENDDO
058800950921     C                   ENDSR
058900960117     C***************  CALCOLO DATA FOGLIO VIAGGIO TEORICA  (1) *******
059000951019     C     SRDAT         BEGSR
059100960117     C                   CLEAR                   WFES
059200951019     C                   CLEAR                   WGIDAT
059300951019     C                   Z-ADD     1             XX                2 0
059400951019     C     WALF          LOOKUP    ALF(XX)                                30
059500960117    1C     *IN30         IFEQ      *ON
059600951114     C     XX            ADD       WTGI          GI8TGI
059700951019     C                   CALL      'XSRGI8'
059800951019     C                   PARM                    WGIDAT
059900960115     C                   MOVE      GI8INV        WGFV
060000960117     C* Verifico se data FV teorica calcolata è festiva
060100960115     C                   MOVEL     WGFV          KANN
060200960115     C                   MOVE      WGFV          W0040             4 0
060300960112     C                   MOVEL     W0040         KMES
060400960112     C                   MOVE      W0040         GG                2 0
060500960304     C                   EXSR      CTRFES
060600960304     C*§         KCLN      CHAINAZCLN01L             30
060700960304    2C*§         *IN30     IFEQ *OFF
060800960117     C* Data non festiva: se gfv > 'B' verifico se devo slittare
060900960304    3C*§         POM,GG    IFEQ ' '
061000960304     C     WFES          IFEQ      *BLANKS
061100960304    4C     WALF          IFGT      'B'
061200960117     C                   EXSR      SRDAT2
061300960117    4C                   END
061400960117   X3C                   ELSE
061500960112     C* DATA FESTIVA: NON FACCIO NULLA
061600960117     C                   MOVE      POM(GG)       WFES              1
061700960117    3C                   END
061800960304    2C*§                   END
061900960117    1C                   END
062000951019     C                   ENDSR
062100960117     C***********  CALCOLA DATA FOGLIO VIAGGIO TEORICA  (2) ***********
062200960117     C* SE PER UNA FERMATA DI CARICO E' PREVISTA GIORNO APERTURA FV > B
062300960117     C* QUESTA ROUTINE SERVE PER FAR SLITTARE, SE OCCORRE, LA DATA FV
062400960117     C* TEORICA DI N GIORNI QUANTI SONO I GIORNI FESTIVI CON BLOCCO
062500960117     C* TRAFFICO PRESENTI FRA LA DATA TRAINO E LA DATA FVT PRESUNTA.
062600960117     C     SRDAT2        BEGSR
062700960115     C                   MOVE      WGFV          WDTAA                          DATA A
062800960115     C                   MOVE      *ZEROS        WDAT
062900960115     C                   MOVE      *ZEROS        Y                 2 0
063000960115     C                   Z-ADD     1             X                 2 0
063100960117     C* CALCOLO DATA TRAINO+1
063200960115     C                   CLEAR                   WGIDAT
063300960115     C     WTGI          ADD       1             GI8TGI
063400960115     C                   CALL      'XSRGI8'
063500960115     C                   PARM                    WGIDAT
063600960115     C                   MOVE      GI8INV        WDAT
063700960115     C*
063800960117     C     CICLO         TAG
063900960117     C*
064000960117    1C     WDAT          DOWLT     WDTAA
064100960115     C* VERIFICO SE DATA FESTIVA
064200960115     C                   MOVEL     WDAT          KANN
064300960115     C                   MOVE      WDAT          W0040             4 0
064400960115     C                   MOVEL     W0040         KMES
064500960115     C                   MOVE      W0040         GG                2 0
064600960115     C                   EXSR      CTRFES
064700960117    2C     WFES          IFEQ      'B'
064800960117     C                   ADD       1             Y                              fest. blocco
064900960117    2C                   END
065000960115     C* CALCOLO DATA TRAINO+X
065100960115     C                   ADD       1             X
065200960117     C                   CLEAR                   WGIDAT
065300960115     C     WTGI          ADD       X             GI8TGI
065400960117     C                   MOVE      GI8TGI        COMTGI
065500960115     C                   CALL      'XSRGI8'
065600960115     C                   PARM                    WGIDAT
065700960115     C                   MOVE      GI8INV        WDAT
065800960117    1C                   ENDDO
065900960117     C* TROVATI GIORNI FESTIVI CON BLOCCO TRAFFICO FRA LE DUE DATE:
066000960117    1C     Y             IFGT      0
066100960117     C* CALCOLO DATA FVT NUOVA AGGIUNGENDO IL NUMERO DI FESTIVI
066200960117     C                   CLEAR                   WGIDAT
066300960117     C     COMTGI        ADD       Y             GI8TGI
066400960117     C                   CALL      'XSRGI8'
066500960117     C                   PARM                    WGIDAT
066600960117     C                   MOVEL     GI8INV        KANN
066700960117     C                   MOVE      GI8INV        W0040             4 0
066800960117     C                   MOVEL     W0040         KMES
066900960117     C                   MOVE      W0040         GG                2 0
067000960117     C                   EXSR      CTRFES
067100960117     C* DATA FVT FESTIVA: NON POSSO APRIRE IL FV IN DATA FESTIVA
067200960117     C* DATA FVT NON FESTIVA: RIPETO IL CONTROLLO PRECEDENTE CON
067300960117     C*          DATA DAL = DATA FVT VECCHIO E DATA AL = DATA FVT NUOVO
067400960117     C*          (IL CONTROLLO NON OCCORRE RIPETERLO SE HO SLITTATO DI
067500960117     C*          UN SOLO GIORNO)
067600960117    2C     WFES          IFEQ      *BLANKS
067700960117    3C     Y             IFEQ      1
067800960117     C                   MOVE      GI8INV        WGFV
067900960117   X3C                   ELSE
068000960117     C                   MOVE      GI8INV        WDTAA
068100960117     C*
068200960117     C                   MOVE      *ZEROS        Y                 2 0
068300960117     C                   ADD       1             X                 2 0
068400960117     C                   CLEAR                   WGIDAT
068500960117     C     WTGI          ADD       X             GI8TGI
068600960117     C                   CALL      'XSRGI8'
068700960117     C                   PARM                    WGIDAT
068800960117     C                   MOVE      GI8INV        WDAT
068900960117     C                   GOTO      CICLO
069000960117    3C                   END
069100960117    2C                   END
069200960117   X1C                   ELSE
069300960117     C* NON TROVATI GIORNI FESTIVI CON BLOCCO TRAFFICO FRA LE DUE DATE
069400960117     C                   CLEAR                   WFES
069500960117     C                   MOVE      WDTAA         WGFV
069600960117    1C                   END
069700960115     C                   ENDSR
069800951205     C*************  CONTROLLO SE GIORNO FESTIVO  *********************
069900951205     C     CTRFES        BEGSR
070000951205     C                   CLEAR                   WFES
070100951205     C     KCLN          CHAIN     AZCLN01L                           30
070200960115    1C     *IN30         IFEQ      *OFF
070300960122     C     POM(GG)       ANDNE     *BLANKS
070400960115     C                   Z-ADD     1             XY                2 0
070500960115     C     POM(GG)       LOOKUP    S1Y(XY)                                30
070600960117     C   30              MOVE      FET(XY)       WFES              1
070700960115    1C                   END
070800951205     C                   ENDSR
070900951205     C******************  ROUTINE INIZIALE  ***************************
071000950915     C     *INZSR        BEGSR
071100950915     C     *ENTRY        PLIST
071200950922     C                   PARM                    KPJBA
071300950922     C                   MOVEL     KPJBU         PARAM
071400951030     C*
071500951030     C                   Z-ADD     1             CODUT
071600951030     C                   CALL      'X§PARUT'
071700951030     C                   PARM                    UT§DSE
071800950915     C*
071900951002     C     KTLT1A        KLIST
072000951002     C                   KFLD                    PARTRN
072100951002     C                   KFLD                    PARDDE
072200950915     C     KTLT1         KLIST
072300950915     C                   KFLD                    PARTRN
072400950915     C                   KFLD                    PARDTD
072500950915     C*
072600950915     C     KTLT2         KLIST
072700960314     C                   KFLD                    WBIS
072800950918     C                   KFLD                    PARTFP
072900950918     C                   KFLD                    PARDTD
073000950918     C*
073100950918     C     KTLT2B        KLIST
073200960314     C                   KFLD                    WBIS
073300950918     C                   KFLD                    PARTFP
073400950919     C     KTLT2C        KLIST
073500960314     C                   KFLD                    WBIS
073600950919     C                   KFLD                    TLTTFP
073700950919     C                   KFLD                    PARDTD
073800950915     C*
073900950920     C     KTST          KLIST
074000950920     C                   KFLD                    TLTTRN
074100950927     C                   KFLD                    WDTN
074200950920     C*
074300950920     C     KTLR          KLIST
074400950920     C                   KFLD                    TLTTRN
074500950920     C                   KFLD                    TLTDDE
074600951124     C     KTLR3         KLIST
074700951124     C                   KFLD                    TLTTRN
074800951124     C                   KFLD                    TLTDDE
074900951124     C                   KFLD                    KTFM
075000951124     C                   KFLD                    TLTFES
075100960304     C     KTLR3B        KLIST
075200960304     C                   KFLD                    TLTTRN
075300960304     C                   KFLD                    TLTDDE
075400960304     C                   KFLD                    KTFM
075500950920     C*
075600950920     C     KTSR          KLIST
075700950921     C                   KFLD                    TLRTRN
075800950927     C                   KFLD                    WDTN
075900950921     C                   KFLD                    NFP(II)
076000950921     C                   KFLD                    TLRNFM
076100950921     C                   KFLD                    CAR(II)
076200950921     C                   KFLD                    TLRFIL
076300950920     C*
076400950920     C     KTLM          KLIST
076500950921     C                   KFLD                    TLRTRN
076600950921     C                   KFLD                    TLRDDE
076700950920     C                   KFLD                    CAR(II)
076800950920     C                   KFLD                    TLRFIL
076900950921     C*
077000950921     C     KTLM1         KLIST
077100950921     C                   KFLD                    TLTTRN
077200950921     C                   KFLD                    TLTDDE
077300960304     C*
077400960304     C     KTLMB         KLIST
077500960304     C                   KFLD                    TLRTRN
077600960304     C                   KFLD                    TLRDDE
077700960304     C                   KFLD                    TLRFIL
077800960304     C                   KFLD                    KSCA
077900950921     C*
078000950921     C     KTSM          KLIST
078100950921     C                   KFLD                    TLMTRN
078200950927     C                   KFLD                    WDTN
078300950921     C                   KFLD                    TLMTFP
078400950921     C                   KFLD                    TLMTFA
078500950921     C                   KFLD                    TLMTFF
078600950920     C*
078700951123     C*          KTLL      KLIST
078800951123     C*                    KFLD           TLTTRN
078900951123     C*                    KFLD           TLTDDE
079000951123     C*                    KFLD           TLTTMZ
079100951030     C*
079200960115     C     KTAB0         KLIST
079300960115     C                   KFLD                    CODUT
079400960115     C                   KFLD                    KCOD
079500951030     C     KTAB          KLIST
079600951030     C                   KFLD                    CODUT
079700951030     C                   KFLD                    KCOD
079800951030     C                   KFLD                    KKEY
079900951205     C     KCLN          KLIST
080000951205     C                   KFLD                    KTFP
080100951205     C                   KFLD                    KTFA
080200951205     C                   KFLD                    KANN
080300951205     C                   KFLD                    KMES
080400950920     C*
080500960314     C     *LIKE         DEFINE    TLTBIS        WBIS
080600951124     C     *LIKE         DEFINE    TLRTFM        KTFM
080700960304     C     *LIKE         DEFINE    TLRFIL        KSCA
080800950927     C     *LIKE         DEFINE    TSTDTN        WDTN
080900950927     C     *LIKE         DEFINE    G08TGI        WTGI
081000951115     C     *LIKE         DEFINE    G08TGI        WTGID
081100951116     C     *LIKE         DEFINE    GI8TGI        COMTGI
081200951115     C     *LIKE         DEFINE    GIOSET        WGIOSD
081300960118     C     *LIKE         DEFINE    TLRGFV        WALF
081400951030     C     *LIKE         DEFINE    TBLCOD        KCOD
081500951030     C     *LIKE         DEFINE    TBLKEY        KKEY
081600951205     C     *LIKE         DEFINE    CLNANN        KANN
081700951205     C     *LIKE         DEFINE    CLNMES        KMES
081800951205     C     *LIKE         DEFINE    CLNTFP        KTFP
081900951205     C     *LIKE         DEFINE    CLNTFA        KTFA
082000960115     C     *LIKE         DEFINE    WDTN          WDTAA
082100960115     C     *LIKE         DEFINE    WDTN          WDAT
082200960115     C     *LIKE         DEFINE    WDTN          WGFV
082300960312     C     *LIKE         DEFINE    TLRTFM        WTFM
082400951205     C*
082500960112     C                   Z-ADD     68            KTFP
082600951205     C                   CLEAR                   KTFA
082700950915     C*
082800950915     C     PARTRN        IFNE      *ZEROS
082900950915     C                   OPEN      TNTLT01L
083000950915     C                   SETON                                        01
083100950915     C                   ELSE
083200950915     C                   OPEN      TNTLT02L
083300950918     C     PARTFP        COMP      0                                  0303
083400950922     C  N03              SETON                                        02
083500950915     C                   END
083600950918     C*
083700950918     C     PARDTA        IFEQ      *ZEROS
083800950918     C                   Z-ADD     PARDTD        PARDTA
083900950918     C                   END
084000951002     C* MEMORIZZAZIONI "DATA DA"
084100951002     C* Inverto data dal ricevuta DA AAAA/MM/GG A GG/MM/AAAA
084200951002     C                   MOVE      *ZEROS        G08DAT
084300951002     C                   Z-ADD     PARDTD        G08INV
084400951002     C                   MOVE      '3'           G08ERR
084500951002     C                   CALL      'XSRDA8'
084600951002     C                   PARM                    WLBDA8
084700951115     C                   Z-ADD     G08TGI        WTGID
084800951002     C* Determino giorno della settimana
084900951002     C                   MOVEL     G08DAT        W006A             6
085000951002     C                   MOVE      G08DAT        W002A             2
085100951002     C                   MOVE      W002A         W006A
085200951002     C                   CALL      'XGIOSE1'
085300951002     C                   PARM                    W006A
085400951002     C                   PARM                    GIOSET            1
085500951115     C                   MOVE      GIOSET        WGIOSD
085600960118     C* CARICAMENTO TABELLA 1Y - SIMBOLI CALENDARIO BARTOLINI
085700960118     C* (CARICAMENTO DEI SIMBOLI FESTIVI PER PROCDEURA TRAINI)
085800960118     C                   MOVE      *ZEROS        II
085900960118     C                   MOVEL     '1Y'          KCOD
086000960118     C     KTAB0         SETLL     TABEL00F
086100960118     C     KTAB0         READE     TABEL00F                               30
086200960118     C     *IN30         DOWEQ     *OFF
086300960118     C     TBLFLG        IFEQ      *BLANKS
086400960118     C                   MOVEL     TBLUNI        DS1Y
086500960118     C     §1YFET        IFNE      *BLANKS
086600960118     C                   ADD       1             II
086700960118     C                   MOVEL     TBLKEY        S1Y(II)
086800960118     C                   MOVE      §1YFET        FET(II)
086900960118     C                   END
087000960118     C                   END
087100960118     C     KTAB0         READE     TABEL00F                               30
087200960118     C                   ENDDO
087300950915     C                   ENDSR
087400950921**  ALF
087500950921BCDEFGHILMNOPQRSTUVZ
