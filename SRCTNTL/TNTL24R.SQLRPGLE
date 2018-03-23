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
001900101209     FAZORG01L  IF   E           K DISK
002000950920     D CAR             S              3  0 DIM(200)                             FILIALI CARICO
002100950920     D NFP             S              3  0 DIM(200)                             N.FERMATE PART.
002200951218     D DTT             S              8  0 DIM(200)                             DATE F.V.TEOR.
002300960301     D CAS             S              3  0 DIM(200)                             FILIALI CARICO
002400960301     D NFS             S              3  0 DIM(200)                             N.FERMATE PART.
002500960301     D DTS             S              8  0 DIM(200)                             DATE F.V.TEOR.
002600950921     D ALF             S              1    DIM(20) CTDATA PERRCD(20)            *ALFABETO (1÷20)
002700951002     D DAT             S              8  0 DIM(31)                              DATE DA ELAB.
002800951002     D NGG             S              5  0 DIM(31)                              DATE IN GIORNI
002900951002     D GGS             S              1    DIM(31)                              GG.SETTIMANA
003000960115     D S1Y             S              1    DIM(10)                              SIMBOLI CALEND
003100960115     D FET             S              1    DIM(10)                              FLAG.FESTIVO
003200960301     D SCA             S              4    DIM(200)                             FIL.SCAR.ANAGRAF
003300960304     D NFA             S              3  0 DIM(200)                             N.FERM.ARRIVO
003400101209     D est             S              3  0 DIM(200)
003500950915     D WLBDA8          DS
003600950915     D  G08DAT                 1      8  0
003700950915     D  G08INV                 9     16  0
003800950915     D  G08ERR                17     17
003900950915     D  G08TGI                18     22  0
004000950921     D WGIDAT          DS
004100950921     D  GI8DAT                 1      8  0
004200950921     D  GI8INV                 9     16  0
004300950921     D  GI8TGI                17     21  0
004400951206     D                 DS
004500951206     D  CLNPOM                 1     31
004600951206     D  POM                    1     31
004700951205     D                                     DIM(31)                              FESTIV.POMER.
004800950922     D KPJBA         E DS
004900951030     D DSTV          E DS
005000960117     D DS1Y          E DS
005100101209     D OG143         E DS
005200010725      * ds per divisa
005300010725     d dged          e ds
005400010725     d tibs02ds      e ds
005500010725
005600951030     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005700960118     D* DS PER PASSAGGIO PARAMETRI A PGM TNTL35R - CALCOLO DATA TEORICA
005800960118     D DSTL35        E DS                  EXTNAME(TNTL35DS)
005900960118     D*
006000950915     D PARAM           DS
006100950915     D* DATA TRAINO DAL
006200950915     D  PARDTD                 1      8  0
006300950915     D* DATA TRAINO AL
006400950915     D  PARDTA                 9     16  0
006500950915     D* NUMERO TRAINO
006600950915     D  PARTRN                17     23  0
006700950915     D* TERMINAL DI PARTENZA
006800950915     D  PARTFP                24     26  0
006900951002     D* DATA DECORRENZA TRAINO
007000951002     D  PARDDE                27     34  0
007100951205     D* CREAZIONE SE GIORNO FESTIVO (' '=No controllo festivo
007200951205     D*                             ('S'=Controllo festivo e crea solo
007300951205     D*                                  se non festivo)
007400951205     D  PARFES                35     35
007500160204
007600160204     d dw              s              1s 0
007700160204     d dataiso         s               d   datfmt(*iso)
007800160204     d wdata           s              8s 0
007900950915     I*
008000950922     C****************************************************************
008100950922     C*  RIEPILOGO INDICATORI
008200950922     C***************************************************************
008300950922     C* 01    - CREAZIONE GRIGLIA PER NUMERO TRAINO
008400950922     C* 02    - CREAZIONE GRIGLIA PER DATA
008500950922     C* 03    - CREAZIONE GRIGLIA PER TERMINAL DI PARTENZA/DATA
008600950922     C* 29    - GESTIONE LETTURA TNTLT01L
008700950922     C* 30    - COMODO
008800950922     C***************************************************************
008900160204
009000160204      /free
009100160204         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
009200160204      /end-free
009300160204
009400951002     C* Se ho ricevuto numero traino/data decorrenza ed una sola data
009500960122     C* traino controllo solo (se richiesto) se quest'ultima è festiva
009600960122     C* (gli altri controlli sono già stati fatti dal chiamante)
009700951002     C                   SELECT
009800951002     C     PARDDE        WHENNE    *ZEROS
009900951002     C     PARTRN        ANDNE     *ZEROS
010000951002     C     PARDTD        ANDEQ     PARDTA
010100951002     C     KTLT1A        CHAIN     TNTLT01L                           30
010200951002     C     *IN30         IFEQ      *OFF
010300951115     C                   MOVE      PARDTD        WDTN
010400951115     C                   MOVE      WTGID         WTGI
010500960122    3C     PARFES        IFEQ      'S'
010600960122     C                   MOVEL     WDTN          KANN
010700960122     C                   MOVE      WDTN          W0040             4 0
010800960122     C                   MOVEL     W0040         KMES
010900960122     C                   MOVE      W0040         GG                2 0
011000160204     c                   z-add     wdtn          wdata
011100960122     C                   EXSR      CTRFES
011200960122     C* Creo griglia solo se non festivo
011300960122    4C     WFES          CASEQ     ' '           CREAG1
011400960122    4C                   END
011500960122     C                   ELSE
011600960122     C                   EXSR      CREAG1
011700960122    3C                   END
011800951002     C                   END
011900951002     C                   OTHER
012000960314     C                   CLEAR                   WBIS
012100960314     C     RIPETI        TAG
012200950918     C* Posizionamento per numero traino/data decorrenza
012300950922     C   01KTLT1         SETLL     TNTLT01L                               29
012400950918     C* Posizionamento per " "/terminal partenza/data scadenza
012500950922     C   03KTLT2         SETLL     TNTLT02L
012600950918     C*
012700960314     C   02WBIS          SETLL     TNTLT02L
012800950918     C*
012900950922     C   01
013000950922     CAN 29KTLT1         READE     TNTLT01L                               30
013100950922     C   01
013200950922     CANN29PARTRN        READPE    TNTLT01L                               30
013300950922     C   03KTLT2B        READE     TNTLT02L                               30
013400960314     C   02WBIS          READE     TNTLT02L                               30
013500950915     C     *IN30         DOWEQ     *OFF
013600950922     C* ESCLUDO ANNULLATI
013700960220     C     TLTATB        CABNE     *BLANKS       LEGGI
013800101209     c*
013900101209     C                   MOVEL     'TV'          KCOD
014000101209     C                   MOVEL(P)  TLTTTR        KKEY
014100101209     C                   MOVEL     'S'           §TVUTC
014200101209     C     KTAB          CHAIN     TABEL00F                           30
014300101209     C     *IN30         IFEQ      *OFF
014400101209     C     TBLFLG        ANDEQ     *BLANKS
014500101209     C                   MOVEL     TBLUNI        DSTV
014600101209     C                   END
014700101209     c*
014800960424     C* SE GIRO PER BIS ESCLUDO QUELLI < 9000000
014900960424     C     WBIS          IFEQ      'B'
015000960424     C     TLTTRN        ANDLT     9000000
015100101209     c* se defluenza per estero non escludo perchè in questi casi il traino
015200101209     c* è stato creato come bis solo per esigenze tecniche di filiale.
015300101209     c                   if        %lookup(tlttfa:est)=0 or §tvdef<>'S'
015400960424     C                   GOTO      LEGGI
015500101209     c                   endif
015600960424     C                   END
015700950915     C*
015800950915     C     *IN01         IFEQ      *ON
015900950922     C     *IN29         ANDEQ     *OFF
016000950918     C* VERIFICO SE SCADUTO
016100950919     C     TLTDSC        ANDLT     PARDTD
016200950915     C                   GOTO      LEGGI
016300950915     C                   END
016400950922     C   29              SETOFF                                       29
016500950918     C     *IN02         IFEQ      *ON
016600950922     C     TLTDSC        ANDLT     PARDTD
016700950919     C     KTLT2C        SETLL     TNTLT02L
016800950919     C                   GOTO      LEGGI
016900950919     C                   END
017000950920     C* VERIFICO CHE SIA DECORRENTE
017100950922     C     *IN02         IFEQ      *ON
017200950922     C     *IN03         OREQ      *ON
017300950919     C     TLTDDE        IFGT      PARDTA
017400950919     C                   GOTO      LEGGI
017500950919     C                   END
017600950919     C                   END
017700951030     C* VERIFICO CHE PER IL TIPO TRAINO LA GRIGLIA SI CREABILE
017800101209     C*                  MOVEL     'TV'          KCOD
017900101209     C*                  MOVEL(P)  TLTTTR        KKEY
018000101209     C*                  MOVEL     'S'           §TVUTC
018100101209     C*    KTAB          CHAIN     TABEL00F                           30
018200101209     C*    *IN30         IFEQ      *OFF
018300101209     C*    TBLFLG        ANDEQ     *BLANKS
018400101209     C*                  MOVEL     TBLUNI        DSTV
018500101209     C*                  END
018600951030     C     §TVUTC        CABEQ     *BLANKS       LEGGI
018700950921     C* SOLO LA PRIMA VOLTA RICHIAMO ROUTINE DI CARICAMENTO DATE
018800951002     C     PARDTD        IFNE      PARDTA
018900951002     C     VOLTA         ANDEQ     ' '
019000950921     C                   EXSR      CARDAT
019100950921     C                   MOVE      '1'           VOLTA             1
019200950921     C                   END
019300950915     C                   EXSR      ELAB
019400950915     C*
019500950915     C     LEGGI         TAG
019600950919     C*
019700950918     C   01PARTRN        READE     TNTLT01L                               30
019800950919     C   01
019900950919     CANN30TLTDDE        COMP      PARDTA                             30
020000950919     C*
020100950922     C   03KTLT2B        READE     TNTLT02L                               30
020200960314     C   02WBIS          READE     TNTLT02L                               30
020300950915     C                   ENDDO
020400960314     C* SE CREAZIONE GRIGLIA PER DATA (NON DI UN TRAINO SPECIFICO)
020500960424     C* RIPETO IL GIRO PER CREARE LA GRIGLIA ANCHE PER I TRAINI DI
020600960424     C* FILIALE BIS (TRAINI BIS >= 9000000)
020700960314     C     *IN02         IFEQ      *ON
020800960314     C     *IN03         OREQ      *ON
020900960314     C     WBIS          IFEQ      *BLANKS
021000960424     C                   MOVE      'B'           WBIS
021100960314     C                   GOTO      RIPETI
021200960314     C                   END
021300960314     C                   END
021400951002     C                   ENDSL
021500950915     C*
021600950915     C                   SETON                                        LR
021700960111     C************  ELABORAZIONE TESTATA TRAINO  **********************
021800950919     C     ELAB          BEGSR
021900950919     C* Verifico se griglia creabile per la DATA DA richiesta
022000951115    1C     TLTDDE        IFLE      PARDTD
022100951115     C     WGIOSD        SCAN      TLTGSE                                 30
022200950920    2C     *IN30         IFEQ      '1'
022300951115     C                   MOVE      PARDTD        WDTN
022400951115     C                   MOVE      WTGID         WTGI
022500951205     C* Se richiesto, controllo che non sia un giorno festivo
022600951205    3C     PARFES        IFEQ      'S'
022700960115     C                   MOVEL     WDTN          KANN
022800960115     C                   MOVE      WDTN          W0040             4 0
022900960115     C                   MOVEL     W0040         KMES
023000960115     C                   MOVE      W0040         GG                2 0
023100160204     c                   z-add     wdtn          wdata
023200951205     C                   EXSR      CTRFES
023300951205     C* Creo griglia solo se non festivo
023400951205    4C     WFES          CASEQ     ' '           CREAG1
023500951205    4C                   END
023600951205     C                   ELSE
023700951205     C                   EXSR      CREAG1
023800951205    3C                   END
023900950920    2C                   END
024000950920    1C                   END
024100950919     C* Se è stata richiesta più di una data ripeto il giro per tutte
024200950919     C* le date richieste
024300950919    1C     PARDTD        IFNE      PARDTA
024400951002     C                   Z-ADD     2             XX
024500951002    2C     XX            DOWLE     31
024600950928     C     DAT(XX)       ANDNE     *ZEROS
024700950920     C* Verifico se griglia creabile
024800960315     C     TLTDDE        IFLE      DAT(XX)
024900960315     C     TLTDSC        ANDGE     DAT(XX)
025000950921     C     GGS(XX)       SCAN      TLTGSE                                 30
025100950918     C* RICHIAMO CREAZIONE GRIGLIA
025200950920    4C     *IN30         IFEQ      '1'
025300950927     C                   Z-ADD     DAT(XX)       WDTN                           DATA TRAINO
025400950927     C                   Z-ADD     NGG(XX)       WTGI                           DATA IN GG
025500951205     C* Se richiesto, controllo che non sia un giorno festivo
025600951205    5C     PARFES        IFEQ      'S'
025700960115     C                   MOVEL     WDTN          KANN
025800960115     C                   MOVE      WDTN          W0040             4 0
025900960115     C                   MOVEL     W0040         KMES
026000960115     C                   MOVE      W0040         GG                2 0
026100160204     c                   z-add     wdtn          wdata
026200951205     C                   EXSR      CTRFES
026300951205     C* Creo griglia solo se non festivo
026400951205    6C     WFES          CASEQ     ' '           CREAG1
026500951205    6C                   END
026600951205   X5C                   ELSE
026700951205     C                   EXSR      CREAG1
026800951205    5C                   END
026900950920    4C                   END
027000950920    3C                   END
027100950921     C                   ADD       1             XX
027200950919    2C                   ENDDO
027300950919    1C                   END
027400950919     C                   ENDSR
027500950920     C****************  CREAZIONE GRIGLIA 1  **************************
027600950920     C     CREAG1        BEGSR
027700951006     C**** CREO LA GRIGLIA SOLO SE NON ESISTE LA TESTATA STORICO TRAINI
027800950920     C     KTST          SETLL     TNTST01L                               30
027900951006    1C     *IN30         IFEQ      *OFF
028000950921     C**** STORICO FERMATE
028100960301     C*
028200960312     C                   CLEAR                   WTFM
028300950920     C     KTLR          SETLL     TNTLR01L
028400950920     C     KTLR          READE     TNTLR01L                               30
028500951006    2C     *IN30         DOWEQ     *OFF
028600950921     C* Escludo record annullati
028700960220    3C     TLRATB        IFEQ      *BLANKS
028800951006    4C                   SELECT
028900960304     C* E' una FERMATA DI SOLO CARICO:
029000950920     C     TLRSCA        WHENEQ    ' '
029100960312     C* Pulizie a rottura di tipo fermata
029200960312     C     TLRTFM        IFNE      WTFM
029300960312     C                   Z-ADD     1             II                3 0
029400960312     C                   MOVE      *ZEROS        IS                3 0
029500960312     C                   MOVEA     *ZEROS        CAR
029600960312     C                   MOVEA     *ZEROS        NFP
029700960312     C                   MOVEA     *ZEROS        DTT
029800960312     C                   MOVEA     *ZEROS        CAS
029900960312     C                   MOVEA     *ZEROS        NFS
030000960312     C                   MOVEA     *ZEROS        DTS
030100960312     C                   CLEAR                   SCA
030200960312     C                   CLEAR                   NFA
030300960312     C                   MOVE      *ZEROS        SS                3 0
030400960312     C                   MOVE      TLRTFM        WTFM
030500960312     C                   END
030600960115     C     TLRGFV        IFEQ      'A'
030700960117     C* ... Se giorno apertura FV = A (cioè = data traino):
030800960117     C* ... memorizzo il carico, il n. fermata e la data FV teorica
030900960115     C                   MOVE      WDTN          DTT(II)
031000960115     C                   MOVE      TLRFIL        CAR(II)
031100960115     C                   MOVE      TLRNFM        NFP(II)
031200960115     C                   ADD       1             II
031300960115     C                   ELSE
031400960117     C* ... Se giorno apertura FV > A (cioè > di data traino):
031500960117     C* ... calcolo la data FV teorica
031600960112     C                   MOVE      TLRGFV        WALF
031700960112     C                   EXSR      SRDAT
031800960117     C* ... Se la data FVT calcolata non è festiva memorizzo il carico,
031900960117     C* ... il n. fermata e la data FV teorica
032000960115     C     WFES          IFEQ      *BLANKS
032100960117     C                   MOVE      WGFV          DTT(II)
032200960115     C                   MOVE      TLRFIL        CAR(II)
032300960115     C                   MOVE      TLRNFM        NFP(II)
032400960115     C                   ADD       1             II
032500960112     C                   END
032600960115     C                   END
032700950920     C*
032800960304     C* E' una FERMATA DI SCARICO:
032900950920     C     TLRSCA        WHENEQ    'S'
033000960301     C*
033100950920     C                   Z-ADD     1             II
033200960304     C                   MOVE      *BLANKS       WTROV             1
033300951006    5C     II            DOWLE     200
033400950928     C     CAR(II)       ANDNE     *ZEROS
033500960117     C* ... verifico nei mc se occorre scrivere griglia per i carichi
033600960117     C* ... memorizzati
033700960328     C     KTLM          SETLL     TNTLM01L
033800960328     C     KTLM          READE     TNTLM01L                               30
033900960328     C     *IN30         DOWEQ     *OFF
034000960328     C     TLMATB        ANDNE     *BLANKS
034100960328     C     KTLM          READE     TNTLM01L                               30
034200960328     C                   ENDDO
034300960328    6C     *IN30         IFEQ      *OFF
034400960328     C     TLMATB        ANDEQ     *BLANKS
034500960328     C*      Memorizzo che ho trovato almeno un carico con Mc
034600960328     C                   MOVE      '1'           WTROV
034700950920     C                   EXSR      CREAG2
034800960301     C                   ELSE
034900960301     C* ... mc non trovati: salvo il carico che è sicuramente relativo
035000960301     C* ... ad una successiva fermata di scarico
035100960304     C     TLRCAR        IFEQ      'S'
035200960301     C                   ADD       1             IS                3 0
035300960301     C                   MOVE      DTT(II)       DTS(IS)
035400960301     C                   MOVE      CAR(II)       CAS(IS)
035500960301     C                   MOVE      NFP(II)       NFS(IS)
035600960304     C                   END
035700951006    6C                   END
035800950920     C                   ADD       1             II
035900960304    5C                   ENDDO
036000960304     C* Se non ho trovato nemmeno un carico con Mc memorizzo la filiale
036100960304     C* di scarico
036200960304     C     WTROV         IFEQ      *BLANKS
036300960304     C                   ADD       1             SS
036400960304     C                   MOVEL     TLRFIL        SCA(SS)
036500960304     C                   MOVE      TLRTFM        SCA(SS)
036600960304     C                   MOVE      TLRNFM        NFA(SS)
036700960304     C                   END
036800960304     C*
036900951006    5C     TLRCAR        IFEQ      'S'
037000950920     C* CARICA ANCHE: memorizzo il nuovo carico dopo aver cancellato
037100960301     C*               i carichi in memoria e ricaricato quelli senza mc
037200960115    6C     TLRGFV        IFEQ      'A'
037300960301     C* Pulizie
037400960301     C                   EXSR      PULCAM
037500960301     C                   ADD       1             II
037600950920     C                   MOVE      TLRFIL        CAR(II)
037700950920     C                   MOVE      TLRNFM        NFP(II)
037800951023     C                   MOVE      WDTN          DTT(II)
037900960115     C                   ADD       1             II
038000960115   X6C                   ELSE
038100951019     C                   MOVE      TLRGFV        WALF
038200951019     C                   EXSR      SRDAT
038300960115    7C     WFES          IFEQ      *BLANKS
038400960301     C* Pulizie
038500960301     C                   EXSR      PULCAM
038600960301     C                   ADD       1             II
038700960115     C                   MOVE      TLRFIL        CAR(II)
038800960115     C                   MOVE      TLRNFM        NFP(II)
038900960117     C                   MOVE      WGFV          DTT(II)
039000960115     C                   ADD       1             II
039100960115    7C                   END
039200960115    6C                   END
039300960115    5C                   END
039400951006    4C                   ENDSL
039500951006    3C                   END
039600950920     C     KTLR          READE     TNTLR01L                               30
039700951006    2C                   ENDDO
039800960304     C* VERIFICO SE CI SONO FERMATE DI SCARICO ANCORA DA SCRIVERE
039900960304    2C     SS            IFGT      *ZEROS
040000960304     C                   Z-ADD     1             SS
040100960304    3C     SCA(SS)       DOWNE     *BLANKS
040200960304     C     SS            ANDLE     200
040300960304     C* Cerco una fermata di carico con Mc per questo scarico
040400960304     C                   MOVE      SCA(SS)       KTFM
040500960304     C                   MOVEL     SCA(SS)       KSCA
040600960304     C                   CLEAR                   WTROV
040700960304     C     KTLR3B        SETLL     TNTLR03L
040800960304     C     KTLR3B        READE     TNTLR03L                               30
040900960304    4C     *IN30         DOWEQ     *OFF
041000960304    5C     TLRCAR        IFEQ      'S'
041100960304     C* Verifico se ci sono Mc
041200960328     C     KTLMB         SETLL     TNTLM01L
041300960328     C     KTLMB         READE     TNTLM01L                               30
041400960328     C     *IN30         DOWEQ     *OFF
041500960328     C     TLMATB        ANDNE     *BLANKS
041600960328     C     KTLMB         READE     TNTLM01L                               30
041700960328     C                   ENDDO
041800960304    6C     *IN30         IFEQ      *OFF
041900960304     C     TLMATB        ANDEQ     *BLANKS
042000960304     C* Imposto campi per poter eseguire creag2
042100960304     C                   Z-ADD     1             II
042200960304     C                   MOVE      TLRNFM        NFP(II)
042300960304     C                   MOVE      NFA(SS)       TLRNFM
042400960304     C                   MOVE      TLRFIL        CAR(II)
042500960304     C                   MOVE      KSCA          TLRFIL
042600960304     C* . . . . . . . . Determino data fv teorico
042700960304    7C     TLRGFV        IFEQ      'A'
042800960304     C                   MOVE      WDTN          DTT(II)
042900960304     C                   EXSR      CREAG2
043000960304     C                   MOVE      '1'           WTROV
043100960304   X7C                   ELSE
043200960304     C                   MOVE      TLRGFV        WALF
043300960304     C                   EXSR      SRDAT
043400960304    8C     WFES          IFEQ      *BLANKS
043500960304     C                   MOVE      WGFV          DTT(II)
043600960304     C                   EXSR      CREAG2
043700960304     C                   MOVE      '1'           WTROV
043800960304    8C                   END
043900960304    7C                   END
044000960304    6C                   END
044100960304    5C                   END
044200960304    5C     WTROV         IFEQ      *BLANKS
044300960304     C     KTLR3B        READE     TNTLR03L                               30
044400960304     C                   ELSE
044500960304     C                   SETON                                        30
044600960304    5C                   END
044700960304    4C                   ENDDO
044800960304     C                   ADD       1             SS
044900960304    3C                   ENDDO
045000960304    2C                   END
045100950921     C**** STORICO METRI CUBI
045200950921     C     KTLM1         SETLL     TNTLM01L
045300950921     C     KTLM1         READE     TNTLM01L                               30
045400951006    2C     *IN30         DOWEQ     *OFF
045500950921     C* Escludo annullati
045600960220    3C     TLMATB        IFEQ      *BLANKS
045700950921     C     KTSM          SETLL     TNTSM01L                               30
045800951006    4C     *IN30         IFEQ      *OFF
045900950921     C                   CLEAR                   TNTSM000
046000950921     C                   MOVE      TLMTRN        TSMTRN
046100950927     C                   MOVE      WDTN          TSMDTN
046200950921     C                   MOVE      TLMTFP        TSMTFP
046300950921     C                   MOVE      TLMTFA        TSMTFA
046400950921     C                   MOVE      TLMTFF        TSMTFF
046500950921     C                   MOVE      TLMMCA        TSMMCA
046600960321     C                   MOVE      TLMMCT        TSMMCT
046700960424     C                   MOVE      TLMMCB        TSMMCB
046800950921     C                   WRITE     TNTSM000
046900951006    4C                   END
047000951006    3C                   END
047100950921     C     KTLM1         READE     TNTLM01L                               30
047200951006    2C                   ENDDO
047300951006     C**** STORICO TRAINI
047400951006     C                   CLEAR                   TNTST000
047500951006     C                   MOVE      TLTTRN        TSTTRN
047600951006     C                   MOVE      WDTN          TSTDTN
047700951006     C                   MOVE      TLTTFP        TSTTFP
047800951006     C                   MOVE      TLTTFA        TSTTFA
047900951006     C                   MOVEL     TLTTTR        TSTTTR
048000951006     C                   MOVEL     TLTBIS        TSTBIS
048100951026     C                   MOVE      TLTFES        TSTFES
048200951124     C* Determino numero fermata della filiale che espone importo sul
048300951124     C* f.v.: se fes è presente sia in andata che in ritorno prendo
048400951124     C*       il numero fermata di andata
048500951124     C                   MOVE      *ZEROS        TSTNFE
048600951124     C                   MOVEL     'R'           KTFM
048700951124     C     KTLR3         CHAIN     TNTLR03L                           30
048800951124     C  N30              MOVE      TLRNFM        TSTNFE
048900951124     C                   MOVEL     'A'           KTFM
049000951124     C     KTLR3         CHAIN     TNTLR03L                           30
049100951124     C  N30              MOVE      TLRNFM        TSTNFE
049200951123     C*          *IN30     IFEQ *OFF
049300951123     C*                    MOVELTLLTMZ    TSTTMZ
049400951123     C*                    MOVE TLLPDR    TSTPDR
049500951123     C*                    MOVE TLLILI    TSTILI
049600951123     C*                    MOVE TLLIES    TSTIES
049700951123     C*                    MOVELTLLCAU    TSTCAU
049800951123     C*                    END
049900010725      * Divisa
050000010725     C                   CLEAR                   tibs02ds
050100010725
050200010725     C                   MOVEL     'L'           T02TLA
050300010725     C                   MOVEL     'C'           T02MOD
050400010725     C                   MOVEL     KNSIF         T02SIF
050500010725     C                   MOVEL     'GED'         T02COD
050600010725     C                   MOVEL     '1'           T02KE1
050700010725
050800010725     C                   CALL      'TIBS02R'
050900010725     C                   PARM                    KPJBA
051000010725     C                   PARM                    tibs02ds
051100010725
051200010725     c                   movel     T02UNI        DGED
051300010725     c                   if        wdtn < §gedd2
051400010725     c                   eval      tstdiv = §gedt1
051500010725     c                   else
051600010725     c                   eval      tstdiv = §gedt2
051700010725     c                   endif
051800010725
051900951006     C                   WRITE     TNTST000
052000951006    1C                   END
052100950920     C                   ENDSR
052200950921     C***********  CREAZIONE GRIGLIA 2  (PER STORICO FERMATE)  ********
052300950920     C     CREAG2        BEGSR
052400950921     C     KTSR          SETLL     TNTSR01L                               30
052500950922    1C     *IN30         IFEQ      *OFF
052600950920     C* Scrittura storico fermate
052700950921     C                   CLEAR                   TNTSR000
052800950921     C                   MOVE      TLRTRN        TSRTRN
052900950927     C                   MOVE      WDTN          TSRDTN
053000951031     C                   MOVE      DTT(II)       TSRDTT
053100950921     C                   MOVE      NFP(II)       TSRNFP
053200950921     C                   MOVE      TLRNFM        TSRNFA
053300950921     C                   MOVE      CAR(II)       TSRFFP
053400950921     C                   MOVEL     TLRFIL        TSRFFA
053500960301     C                   MOVEL     TLRTFM        TSRTFM
053600960118     C* Ricavo data/ora PARTENZA
053700960118     C                   CLEAR                   DSTL35
053800960118     C                   MOVEL     'D'           D35TUP
053900960118     C                   MOVEL     'P'           D35EPA
054000960118     C                   MOVE      TLRTRN        D35TRN
054100960118     C                   MOVE      TLRDDE        D35DDE
054200960118     C                   MOVE      TSRDTN        D35DTN
054300960118     C                   MOVE      TSRNFP        D35NFM
054400960118     C                   MOVEL     DSTL35        KPJBU
054500960118     C                   CALL      'TNTL35R'
054600960118     C                   PARM                    KPJBA
054700960118     C                   MOVEL     KPJBU         DSTL35
054800960118     C     D35ERR        IFEQ      'E'
054900960118     C                   CLEAR                   TSRDTP
055000960118     C                   CLEAR                   TSRHTP
055100960118     C                   ELSE
055200960118     C                   MOVE      D35DTE        TSRDTP
055300960118     C                   MOVE      D35HTE        TSRHTP
055400960118     C                   END
055500960118     C* Ricavo data/ora ARRIVO
055600960118     C                   CLEAR                   DSTL35
055700960118     C                   MOVEL     'D'           D35TUP
055800960118     C                   MOVEL     'A'           D35EPA
055900960118     C                   MOVE      TLRTRN        D35TRN
056000960118     C                   MOVE      TLRDDE        D35DDE
056100960118     C                   MOVE      TSRDTN        D35DTN
056200960118     C                   MOVE      TSRNFA        D35NFM
056300960118     C                   MOVEL     DSTL35        KPJBU
056400960118     C                   CALL      'TNTL35R'
056500960118     C                   PARM                    KPJBA
056600960118     C                   MOVEL     KPJBU         DSTL35
056700960118     C     D35ERR        IFEQ      'E'
056800960118     C                   CLEAR                   TSRDTA
056900960118     C                   CLEAR                   TSRHTA
057000960118     C                   ELSE
057100960118     C                   MOVE      D35DTE        TSRDTA
057200960118     C                   MOVE      D35HTE        TSRHTA
057300960118     C                   END
057400960118     C*
057500950921     C                   WRITE     TNTSR000
057600950922    1C                   END
057700950920     C                   ENDSR
057800960301     C*************  PULIZIA CAMPI DI CALCOLO  ************************
057900960301     C     PULCAM        BEGSR
058000960301     C                   MOVEA     *ZEROS        CAR
058100960301     C                   MOVEA     *ZEROS        NFP
058200960301     C                   MOVEA     *ZEROS        DTT
058300960301     C                   MOVEA     CAS           CAR
058400960301     C                   MOVEA     NFS           NFP
058500960301     C                   MOVEA     DTS           DTT
058600960301     C                   MOVEA     *ZEROS        CAS
058700960301     C                   MOVEA     *ZEROS        NFS
058800960301     C                   MOVEA     *ZEROS        DTS
058900960301     C                   Z-ADD     IS            II
059000960301     C                   MOVE      *ZEROS        IS
059100960301     C                   ENDSR
059200950921     C*************  CARICAMENTO DATE DA ELABORARE  *******************
059300950921     C     CARDAT        BEGSR
059400950921     C                   MOVEA     *ZEROS        DAT                            DATE DA ELAB
059500950921     C                   MOVEA     *ZEROS        NGG                            DATE IN GG.
059600950921     C                   MOVEA     *BLANKS       GGS                            DAY OF WEEK
059700951115     C                   MOVE      WTGID         WTGI
059800950921     C*
059900951002     C                   Z-ADD     1             XX
060000951002    2C     DAT(XX)       DOWLT     PARDTA
060100950921     C                   CLEAR                   WGIDAT
060200951114     C     WTGI          ADD       XX            GI8TGI
060300951116     C                   Z-ADD     GI8TGI        COMTGI
060400950921     C                   CALL      'XSRGI8'
060500950921     C                   PARM                    WGIDAT
060600950921     C                   MOVEL     GI8DAT        W006A             6
060700950922     C                   MOVE      GI8DAT        W002A             2
060800950922     C                   MOVE      W002A         W006A
060900950921     C                   CALL      'XGIOSE1'
061000950921     C                   PARM                    W006A
061100950921     C                   PARM                    GIOSET
061200951002     C                   ADD       1             XX
061300950921     C                   MOVE      GI8INV        DAT(XX)
061400951116     C                   MOVE      COMTGI        NGG(XX)
061500950922     C                   MOVE      GIOSET        GGS(XX)
061600950921    2C                   ENDDO
061700950921     C                   ENDSR
061800960117     C***************  CALCOLO DATA FOGLIO VIAGGIO TEORICA  (1) *******
061900951019     C     SRDAT         BEGSR
062000960117     C                   CLEAR                   WFES
062100951019     C                   CLEAR                   WGIDAT
062200951019     C                   Z-ADD     1             XX                2 0
062300951019     C     WALF          LOOKUP    ALF(XX)                                30
062400960117    1C     *IN30         IFEQ      *ON
062500951114     C     XX            ADD       WTGI          GI8TGI
062600951019     C                   CALL      'XSRGI8'
062700951019     C                   PARM                    WGIDAT
062800960115     C                   MOVE      GI8INV        WGFV
062900960117     C* Verifico se data FV teorica calcolata è festiva
063000960115     C                   MOVEL     WGFV          KANN
063100960115     C                   MOVE      WGFV          W0040             4 0
063200960112     C                   MOVEL     W0040         KMES
063300960112     C                   MOVE      W0040         GG                2 0
063400160204     c                   z-add     wgfv          wdata
063500960304     C                   EXSR      CTRFES
063600960304     C*§         KCLN      CHAINAZCLN01L             30
063700960304    2C*§         *IN30     IFEQ *OFF
063800960117     C* Data non festiva: se gfv > 'B' verifico se devo slittare
063900960304    3C*§         POM,GG    IFEQ ' '
064000960304     C     WFES          IFEQ      *BLANKS
064100960304    4C     WALF          IFGT      'B'
064200960117     C                   EXSR      SRDAT2
064300960117    4C                   END
064400960117   X3C                   ELSE
064500960112     C* DATA FESTIVA: NON FACCIO NULLA
064600960117     C                   MOVE      POM(GG)       WFES              1
064700960117    3C                   END
064800960304    2C*§                   END
064900960117    1C                   END
065000951019     C                   ENDSR
065100960117     C***********  CALCOLA DATA FOGLIO VIAGGIO TEORICA  (2) ***********
065200960117     C* SE PER UNA FERMATA DI CARICO E' PREVISTA GIORNO APERTURA FV > B
065300960117     C* QUESTA ROUTINE SERVE PER FAR SLITTARE, SE OCCORRE, LA DATA FV
065400960117     C* TEORICA DI N GIORNI QUANTI SONO I GIORNI FESTIVI CON BLOCCO
065500960117     C* TRAFFICO PRESENTI FRA LA DATA TRAINO E LA DATA FVT PRESUNTA.
065600960117     C     SRDAT2        BEGSR
065700960115     C                   MOVE      WGFV          WDTAA                          DATA A
065800960115     C                   MOVE      *ZEROS        WDAT
065900960115     C                   MOVE      *ZEROS        Y                 2 0
066000960115     C                   Z-ADD     1             X                 2 0
066100960117     C* CALCOLO DATA TRAINO+1
066200960115     C                   CLEAR                   WGIDAT
066300960115     C     WTGI          ADD       1             GI8TGI
066400960115     C                   CALL      'XSRGI8'
066500960115     C                   PARM                    WGIDAT
066600960115     C                   MOVE      GI8INV        WDAT
066700960115     C*
066800960117     C     CICLO         TAG
066900960117     C*
067000960117    1C     WDAT          DOWLT     WDTAA
067100960115     C* VERIFICO SE DATA FESTIVA
067200960115     C                   MOVEL     WDAT          KANN
067300960115     C                   MOVE      WDAT          W0040             4 0
067400960115     C                   MOVEL     W0040         KMES
067500960115     C                   MOVE      W0040         GG                2 0
067600160204     c                   z-add     wdat          wdata
067700960115     C                   EXSR      CTRFES
067800960117    2C     WFES          IFEQ      'B'
067900960117     C                   ADD       1             Y                              fest. blocco
068000960117    2C                   END
068100960115     C* CALCOLO DATA TRAINO+X
068200960115     C                   ADD       1             X
068300960117     C                   CLEAR                   WGIDAT
068400960115     C     WTGI          ADD       X             GI8TGI
068500960117     C                   MOVE      GI8TGI        COMTGI
068600960115     C                   CALL      'XSRGI8'
068700960115     C                   PARM                    WGIDAT
068800960115     C                   MOVE      GI8INV        WDAT
068900960117    1C                   ENDDO
069000960117     C* TROVATI GIORNI FESTIVI CON BLOCCO TRAFFICO FRA LE DUE DATE:
069100960117    1C     Y             IFGT      0
069200960117     C* CALCOLO DATA FVT NUOVA AGGIUNGENDO IL NUMERO DI FESTIVI
069300960117     C                   CLEAR                   WGIDAT
069400960117     C     COMTGI        ADD       Y             GI8TGI
069500960117     C                   CALL      'XSRGI8'
069600960117     C                   PARM                    WGIDAT
069700960117     C                   MOVEL     GI8INV        KANN
069800960117     C                   MOVE      GI8INV        W0040             4 0
069900960117     C                   MOVEL     W0040         KMES
070000960117     C                   MOVE      W0040         GG                2 0
070100160204     c                   z-add     gi8inv        wdata
070200960117     C                   EXSR      CTRFES
070300960117     C* DATA FVT FESTIVA: NON POSSO APRIRE IL FV IN DATA FESTIVA
070400960117     C* DATA FVT NON FESTIVA: RIPETO IL CONTROLLO PRECEDENTE CON
070500960117     C*          DATA DAL = DATA FVT VECCHIO E DATA AL = DATA FVT NUOVO
070600960117     C*          (IL CONTROLLO NON OCCORRE RIPETERLO SE HO SLITTATO DI
070700960117     C*          UN SOLO GIORNO)
070800960117    2C     WFES          IFEQ      *BLANKS
070900960117    3C     Y             IFEQ      1
071000960117     C                   MOVE      GI8INV        WGFV
071100960117   X3C                   ELSE
071200960117     C                   MOVE      GI8INV        WDTAA
071300960117     C*
071400960117     C                   MOVE      *ZEROS        Y                 2 0
071500960117     C                   ADD       1             X                 2 0
071600960117     C                   CLEAR                   WGIDAT
071700960117     C     WTGI          ADD       X             GI8TGI
071800960117     C                   CALL      'XSRGI8'
071900960117     C                   PARM                    WGIDAT
072000960117     C                   MOVE      GI8INV        WDAT
072100960117     C                   GOTO      CICLO
072200960117    3C                   END
072300960117    2C                   END
072400960117   X1C                   ELSE
072500960117     C* NON TROVATI GIORNI FESTIVI CON BLOCCO TRAFFICO FRA LE DUE DATE
072600960117     C                   CLEAR                   WFES
072700960117     C                   MOVE      WDTAA         WGFV
072800960117    1C                   END
072900960115     C                   ENDSR
073000951205     C*************  CONTROLLO SE GIORNO FESTIVO  *********************
073100951205     C     CTRFES        BEGSR
073200951205     C                   CLEAR                   WFES
073300951205     C     KCLN          CHAIN     AZCLN01L                           30
073400960115    1C     *IN30         IFEQ      *OFF
073500960122     C     POM(GG)       ANDNE     *BLANKS
073600960115     C                   Z-ADD     1             XY                2 0
073700960115     C     POM(GG)       LOOKUP    S1Y(XY)                                30
073800960117     C   30              MOVE      FET(XY)       WFES              1
073900160204     c* se festivo ma si tratta di domenica lo considero lavorativo
074000160204     c                   if        wfes<>*blanks
074100160204      /free
074200160204       //       Determino il giorno della settimana della data calcolata
074300160204               dataiso=%date(wdata:*iso);
074400160204               EXEC SQL SET :dw = DAYOFWEEK_ISO(:dataiso);
074500160204      /end-free
074600160204     c                   if        dw=7
074700160204     c                   clear                   wfes
074800160204     c                   endif
074900160204     c                   endif
075000960115    1C                   END
075100951205     C                   ENDSR
075200951205     C******************  ROUTINE INIZIALE  ***************************
075300950915     C     *INZSR        BEGSR
075400950915     C     *ENTRY        PLIST
075500950922     C                   PARM                    KPJBA
075600950922     C                   MOVEL     KPJBU         PARAM
075700951030     C*
075800951030     C                   Z-ADD     1             CODUT
075900951030     C                   CALL      'X§PARUT'
076000951030     C                   PARM                    UT§DSE
076100950915     C*
076200951002     C     KTLT1A        KLIST
076300951002     C                   KFLD                    PARTRN
076400951002     C                   KFLD                    PARDDE
076500950915     C     KTLT1         KLIST
076600950915     C                   KFLD                    PARTRN
076700950915     C                   KFLD                    PARDTD
076800950915     C*
076900950915     C     KTLT2         KLIST
077000960314     C                   KFLD                    WBIS
077100950918     C                   KFLD                    PARTFP
077200950918     C                   KFLD                    PARDTD
077300950918     C*
077400950918     C     KTLT2B        KLIST
077500960314     C                   KFLD                    WBIS
077600950918     C                   KFLD                    PARTFP
077700950919     C     KTLT2C        KLIST
077800960314     C                   KFLD                    WBIS
077900950919     C                   KFLD                    TLTTFP
078000950919     C                   KFLD                    PARDTD
078100950915     C*
078200950920     C     KTST          KLIST
078300950920     C                   KFLD                    TLTTRN
078400950927     C                   KFLD                    WDTN
078500950920     C*
078600950920     C     KTLR          KLIST
078700950920     C                   KFLD                    TLTTRN
078800950920     C                   KFLD                    TLTDDE
078900951124     C     KTLR3         KLIST
079000951124     C                   KFLD                    TLTTRN
079100951124     C                   KFLD                    TLTDDE
079200951124     C                   KFLD                    KTFM
079300951124     C                   KFLD                    TLTFES
079400960304     C     KTLR3B        KLIST
079500960304     C                   KFLD                    TLTTRN
079600960304     C                   KFLD                    TLTDDE
079700960304     C                   KFLD                    KTFM
079800950920     C*
079900950920     C     KTSR          KLIST
080000950921     C                   KFLD                    TLRTRN
080100950927     C                   KFLD                    WDTN
080200950921     C                   KFLD                    NFP(II)
080300950921     C                   KFLD                    TLRNFM
080400950921     C                   KFLD                    CAR(II)
080500950921     C                   KFLD                    TLRFIL
080600950920     C*
080700950920     C     KTLM          KLIST
080800950921     C                   KFLD                    TLRTRN
080900950921     C                   KFLD                    TLRDDE
081000950920     C                   KFLD                    CAR(II)
081100950920     C                   KFLD                    TLRFIL
081200950921     C*
081300950921     C     KTLM1         KLIST
081400950921     C                   KFLD                    TLTTRN
081500950921     C                   KFLD                    TLTDDE
081600960304     C*
081700960304     C     KTLMB         KLIST
081800960304     C                   KFLD                    TLRTRN
081900960304     C                   KFLD                    TLRDDE
082000960304     C                   KFLD                    TLRFIL
082100960304     C                   KFLD                    KSCA
082200950921     C*
082300950921     C     KTSM          KLIST
082400950921     C                   KFLD                    TLMTRN
082500950927     C                   KFLD                    WDTN
082600950921     C                   KFLD                    TLMTFP
082700950921     C                   KFLD                    TLMTFA
082800950921     C                   KFLD                    TLMTFF
082900950920     C*
083000951123     C*          KTLL      KLIST
083100951123     C*                    KFLD           TLTTRN
083200951123     C*                    KFLD           TLTDDE
083300951123     C*                    KFLD           TLTTMZ
083400951030     C*
083500960115     C     KTAB0         KLIST
083600960115     C                   KFLD                    CODUT
083700960115     C                   KFLD                    KCOD
083800951030     C     KTAB          KLIST
083900951030     C                   KFLD                    CODUT
084000951030     C                   KFLD                    KCOD
084100951030     C                   KFLD                    KKEY
084200951205     C     KCLN          KLIST
084300951205     C                   KFLD                    KTFP
084400951205     C                   KFLD                    KTFA
084500951205     C                   KFLD                    KANN
084600951205     C                   KFLD                    KMES
084700950920     C*
084800960314     C     *LIKE         DEFINE    TLTBIS        WBIS
084900951124     C     *LIKE         DEFINE    TLRTFM        KTFM
085000960304     C     *LIKE         DEFINE    TLRFIL        KSCA
085100950927     C     *LIKE         DEFINE    TSTDTN        WDTN
085200950927     C     *LIKE         DEFINE    G08TGI        WTGI
085300951115     C     *LIKE         DEFINE    G08TGI        WTGID
085400951116     C     *LIKE         DEFINE    GI8TGI        COMTGI
085500951115     C     *LIKE         DEFINE    GIOSET        WGIOSD
085600960118     C     *LIKE         DEFINE    TLRGFV        WALF
085700951030     C     *LIKE         DEFINE    TBLCOD        KCOD
085800951030     C     *LIKE         DEFINE    TBLKEY        KKEY
085900951205     C     *LIKE         DEFINE    CLNANN        KANN
086000951205     C     *LIKE         DEFINE    CLNMES        KMES
086100951205     C     *LIKE         DEFINE    CLNTFP        KTFP
086200951205     C     *LIKE         DEFINE    CLNTFA        KTFA
086300960115     C     *LIKE         DEFINE    WDTN          WDTAA
086400960115     C     *LIKE         DEFINE    WDTN          WDAT
086500960115     C     *LIKE         DEFINE    WDTN          WGFV
086600960312     C     *LIKE         DEFINE    TLRTFM        WTFM
086700951205     C*
086800960112     C                   Z-ADD     68            KTFP
086900951205     C                   CLEAR                   KTFA
087000950915     C*
087100950915     C     PARTRN        IFNE      *ZEROS
087200950915     C                   OPEN      TNTLT01L
087300950915     C                   SETON                                        01
087400950915     C                   ELSE
087500950915     C                   OPEN      TNTLT02L
087600950918     C     PARTFP        COMP      0                                  0303
087700950922     C  N03              SETON                                        02
087800950915     C                   END
087900950918     C*
088000950918     C     PARDTA        IFEQ      *ZEROS
088100950918     C                   Z-ADD     PARDTD        PARDTA
088200950918     C                   END
088300951002     C* MEMORIZZAZIONI "DATA DA"
088400951002     C* Inverto data dal ricevuta DA AAAA/MM/GG A GG/MM/AAAA
088500951002     C                   MOVE      *ZEROS        G08DAT
088600951002     C                   Z-ADD     PARDTD        G08INV
088700951002     C                   MOVE      '3'           G08ERR
088800951002     C                   CALL      'XSRDA8'
088900951002     C                   PARM                    WLBDA8
089000951115     C                   Z-ADD     G08TGI        WTGID
089100951002     C* Determino giorno della settimana
089200951002     C                   MOVEL     G08DAT        W006A             6
089300951002     C                   MOVE      G08DAT        W002A             2
089400951002     C                   MOVE      W002A         W006A
089500951002     C                   CALL      'XGIOSE1'
089600951002     C                   PARM                    W006A
089700951002     C                   PARM                    GIOSET            1
089800951115     C                   MOVE      GIOSET        WGIOSD
089900960118     C* CARICAMENTO TABELLA 1Y - SIMBOLI CALENDARIO BARTOLINI
090000960118     C* (CARICAMENTO DEI SIMBOLI FESTIVI PER PROCDEURA TRAINI)
090100960118     C                   MOVE      *ZEROS        II
090200960118     C                   MOVEL     '1Y'          KCOD
090300960118     C     KTAB0         SETLL     TABEL00F
090400960118     C     KTAB0         READE     TABEL00F                               30
090500960118     C     *IN30         DOWEQ     *OFF
090600960118     C     TBLFLG        IFEQ      *BLANKS
090700960118     C                   MOVEL     TBLUNI        DS1Y
090800960118     C     §1YFET        IFNE      *BLANKS
090900960118     C                   ADD       1             II
091000960118     C                   MOVEL     TBLKEY        S1Y(II)
091100960118     C                   MOVE      §1YFET        FET(II)
091200960118     C                   END
091300960118     C                   END
091400960118     C     KTAB0         READE     TABEL00F                               30
091500960118     C                   ENDDO
091600101209     C* CARICO SCHIERA filiali estere
091700101209     C                   CLEAR                   est
091800101209     C                   Z-ADD     1             IN                3 0
091900101209     C     *LOVAL        SETLL     AZORG01L
092000101209     C                   READ      AZORG01L                               31
092100101209     C     *IN31         DOWEQ     *OFF
092200101209     C     ORGFVA        IFEQ      *BLANKS
092300101209     C     ORGFAG        ANDEQ     'F'
092400101209     C     ORGFVA        OREQ      *BLANKS
092500101209     C     ORGFAG        ANDEQ     'A'
092600101209     C                   MOVEL     ORGDE3        OG143
092700101209     C                   IF        §OGntw = 'DPD'  or §ogntw='EEX'
092800101209     c                             or §ogntw='FED'
092900101209     C                   Z-ADD     ORGFIL        est(IN)
093000101209     C                   ADD       1             IN
093100101209     C                   ENDIF
093200101209     C                   ENDIF
093300101209     C                   READ      AZORG01L                               31
093400101209     C                   ENDDO
093500950915     C                   ENDSR
093600950921**  ALF
093700950921BCDEFGHILMNOPQRSTUVZ
