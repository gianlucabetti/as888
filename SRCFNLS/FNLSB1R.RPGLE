000100950130     H* FNLSB1R *-----------------------------------------------------*
000200930112     H*-----------------*                                             *
000300930113     H*     PROGRAMMA DI STAMPA QUADRATURA SPUNTE I.M.P. (FILTRO)     *
000400930112     H*---------------------------------------------------------------*
000500930112     H DECEDIT('0,') DATEDIT(*DMY.)
000600020918     FFNFVV01L  IF   E           K DISK
000700950130     FFNLSB1D   CF   E             WORKSTN
000800930112     D*
000900930112     D KPJBA         E DS
001000930112     D* RICERCA RAGIONE SOCIALE
001100020813     D UT§DSE0F      E DS
001200020813     D CNCR80        E DS
001300930112     D*
001400930112     D* RICERCA N° FOGLIO IMP PER '?'
001500950130     D DSLR01        E DS                  INZ
001600930113     D* CALL PROGRAMMA STAMPA QUADRATURA
001700930112     D PARAM           DS
001800930115     D* NUMERO FOGLIO
001900930112     D  VIDNFI                 1      5
002000950130     D* DATA FOGLIO GG/MM/AAAA
002100950130     D  PARDF2                 6     13  0
002200930114     D* DATA PRESENZA COLLI (G/M/A)
002300950130     D  PARDPC                14     21  0
002400930115     D* RAGIONE SOCIALE
002500950130     D  RSUT                  22     41
002600930115     D* FILIALE ELABORATORE
002700950130     D  PARFEL                42     44  0
002800950320     D* FILIALE GESTIONE FOGLIO
002900950320     D  PARFGS                45     47  0
003000950320     D* DATA FOGLIO AAMMGG
003100950320     D  PARDFV                48     55  0
003200930112     D* ROVESCIAMENTO DATA
003300930112     D WLBDAT          DS
003400950130     D  G02DAT                 1      8  0
003500950130     D  G02INV                 9     16  0
003600950130     D  G02ERR                17     17
003700950130     D  G02TGI                18     22  0
003800930112     D* TRASFORMAZIONE GIORNI IN DATA
003900930112     D WGIDAT          DS
004000950130     D  GIODAT                 1      8  0
004100950130     D  GIOINV                 9     16  0
004200950130     D  GIOTGI                17     21  0
004300930112     C*---------------------------------------------------------------*
004400930112     C* INDICATORI USATI                                              *
004500930112     C*---------------------------------------------------------------*
004600930112     C* 01 - N° FOGLIO IMP OBBLIGATORIO
004700930112     C* 02 - DATA PRESENZA COLLI OBBLIGATORIA
004800930112     C* 03 - N° FOGLIO INESISTENTE
004900930112     C* 04 - N° FOGLIO IMP ERRATO
005000930112     C* 05 - N° FOGLIO IMP ANNULLATO
005100930112     C* 06 - DATA PRESENZA COLLI > O = ALLA DATA FOGLIO IMP
005200930112     C* 07 - INSERIMENTO SOLO DI FOGLI IMP
005300930112     C* 08 - DATA PRESENZA COLLI FORMALMENTE NON CORRETTA
005400930112     C* 30 - RICERCA '?' IN VIDNFI
005500930112     C* 31 - TESTN SUL VIDNFI
005600950130     C* 32 - CHAIN SU FNFVV
005700930112     C* 90 - GENERICO DI ERRORE
005800930112     C*---------------------------------------------------------------*
005900930112     C*                    MAIN                                       *
006000930118     C* PROGRAMMA PRINCIPALE LANCIO STAMPA QUADRATURA SPUNTE          *
006100930112     C*---------------------------------------------------------------*
006200930112     C*
006300930112     C* DEFINIZIONI VARIABILI, KEYLIST E PLIST
006400930112     C                   EXSR      DEFVAR
006500020813     C*
006600020813     C* Imposto il P.O. di Gestione
006700020918     C                   eval      VIDfgs = SIMfel
006800020918     C                   clear                   DESfgs
006900930112     C*
007000930112     C* IMPOSTO STAMPA SPUNTA A 'N'
007100930112     C                   MOVEL     'N'           VIDSSI
007200930112     C*
007300930112     C* EMISSIONE FORMATO VIDEO - RICHIESTA N° FOGLIO VIAGGIO E DATA
007400930112     C     FMTD1         TAG
007500950130     C                   EXFMT     LSB1D1
007600930112     C*
007700930112     C* CMD3 - FINE LAVORO
007800930112     C   KC              GOTO      FINE
007900930112     C*
008000930112     C* CONTROLLI FORMATO VIDEO
008100930112     C                   EXSR      CTLD1
008200930112     C*
008300930112     C* SE ERRORI RIEMETTO IL FORMATO
008400930112     C   90              GOTO      FMTD1
008500930112     C*
008600950130     C* CMD6 - LANCIO PROGRAMMA
008700950130     C   KF              EXSR      LANCIO
008800930112     C*
008900930112     C* ENTER - RIMANE NELLA VIDEATA
009000930118IF  1C     *INKC         IFEQ      '0'
009100950130     C     *INKF         ANDEQ     '0'
009200930118     C*
009300930112     C                   GOTO      FMTD1
009400930118     C*
009500930118E   1C                   ENDIF
009600930112     C*
009700930112     C     FINE          TAG
009800930112     C                   SETON                                        LR
009900930112     C*---------------------------------------------------------------*
010000930112     C*                    CTLD1                                      *
010100930112     C* ROUTINE DI CONTROLLO DATI FORMATO VIDEO                       *
010200930112     C*---------------------------------------------------------------*
010300930112     C     CTLD1         BEGSR
010400930112     C*
010500930112     C                   SETOFF                                       903031
010600930112     C*
010700930112     C* N° FOGLIO IMP OBBLIGATORIO
010800930112IF  1C     VIDNFI        IFEQ      *BLANKS
010900930112     C     VIDNFI        OREQ      *ZEROS
011000930112     C                   SETON                                        9001
011100930112     C                   GOTO      ENDC1
011200930112E   1C                   ENDIF
011300930112     C*
011400930112     C* RICERCA
011500950320     C     '?'           SCAN      VIDNFI                                 30
011600930112DO  1C   30              DO
011700950130     C                   CLEAR                   DSLR01
011800950130     C                   MOVEL     'S'           DLRRIC
011900950130     C                   Z-ADD     3             DLRNPG
012000950130     C                   Z-ADD     DATAIN        DLRGDA
012100950130     C                   Z-ADD     31129999      DLRADA
012200950130     C                   MOVEL     'FNLSB1R'     DLRPGM
012300950130     C                   MOVEL     SIMFEL        DLRFGS
012400950130     C                   MOVEL     DSLR01        KPJBU
012500950130     C                   CALL      'FNLR02R'
012600930112     C                   PARM                    KPJBA
012700950130     C                   MOVEL     KPJBU         DSLR01
012800950130     C                   MOVEL     DLRNFV        VIDNFI
012900930112     C                   SETON                                        90
013000930112     C                   GOTO      ENDC1
013100930118E   1C                   ENDDO
013200930112     C*
013300930112     C* SOLO VALORE NUMERICO
013400930112     C                   TESTN                   VIDNFI               31
013500930112     C  N31              SETON                                        9004
013600930112     C  N31              GOTO      ENDC1
013700930112     C*
013800930112     C* ESISTENZA FOGLIO IMP
013900950130     C* LEGGO TUTTI I FOGLI FVV CON NPG+NFV
014000950130     C                   SETOFF                                       33
014100950131     C                   MOVEL     3             KNPG
014200950130     C                   MOVEL     VIDNFI        KNFV
014300020918     C                   MOVEL     Vidfgs        KFgs
014400020918     C     KFVV          CHAIN     FNFVV01L                           32
014500930112     C*
014600950320IF  1C     *IN32         IFEQ      *ON
014700930112     C                   SETON                                        9003
014800930112     C                   GOTO      ENDC1
014900930112X   1C                   ELSE
015000930112     C*
015100930112     C* SOLO FOGLIO IMP
015200930112IF  2C     FVVSPG        IFNE      'P'
015300930112     C                   SETON                                        9007
015400930112     C                   GOTO      ENDC1
015500930112E   2C                   ENDIF
015600930112     C*
015700930112     C* FOGLIO ANNULLATO
015800930112IF  2C     FVVATB        IFNE      *BLANKS
015900930112     C                   SETON                                        9005
016000930112     C                   GOTO      ENDC1
016100930112E   2C                   ENDIF
016200930112E   1C                   ENDIF
016300930112     C*
016400930112     C* GIRO DATA FOGLIO DA PORTARE A VIDEO
016500950130     C                   Z-ADD     FVVDFV        G02INV
016600950130     C                   Z-ADD     *ZERO         G02DAT
016700950130     C                   MOVEL     '3'           G02ERR
016800950130     C                   CALL      'XSRDA8'
016900950130     C                   PARM                    WLBDAT
017000950130     C                   Z-ADD     G02DAT        VIDDFI
017100930112     C*
017200930112     C* DATA PRESENZA COLLI OBBLIGATORIA
017300930112IF  1C     VIDDPC        IFEQ      *ZEROS
017400930112     C                   SETON                                        9002
017500930112     C                   GOTO      ENDC1
017600930112E   1C                   ENDIF
017700930112     C*
017800930112     C* CONTROLLO FORMALE DATA
017900930112     C                   MOVE      VIDDPC        G02DAT
018000930112     C                   MOVEL     *BLANK        G02ERR
018100950130     C                   CALL      'XSRDA8'
018200930112     C                   PARM                    WLBDAT
018300930112     C* DATA NON CORRETTA
018400930112IF  1C     G02ERR        IFEQ      '1'
018500930112     C                   SETON                                        9008
018600930112     C                   GOTO      ENDC1
018700930112X   1C                   ELSE
018800930112     C*
018900930112     C* DATA CORRETTA: CONFRONTO DATA FOGLIO IMP
019000950130     C                   Z-ADD     G02DAT        VIDDPC
019100950130     C                   Z-ADD     G02INV        ROVDA             8 0
019200930112     C*
019300930112IF  2C     ROVDA         IFLE      FVVDFV
019400930112     C                   SETON                                        9006
019500930112     C                   GOTO      ENDC1
019600930112E   2C                   ENDIF
019700930112E   1C                   ENDIF
019800930112     C*
019900930112     C     ENDC1         TAG
020000930112     C*
020100930112     C                   ENDSR
020200930112     C*---------------------------------------------------------------*
020300930112     C*                    LANCIO                                     *
020400930112     C* ROUTINE DI LANCIO PROGRAMMI BATCH                             *
020500930112     C*---------------------------------------------------------------*
020600930112     C     LANCIO        BEGSR
020700930112     C*
020800930113     C* STAMPA QUADRATURA SPUNTE
020900950320     C                   Z-ADD     FVVFGS        PARFGS
021000930118     C                   MOVEL     VIDDFI        PARDF2
021100950320     C                   MOVEL     FVVDFV        PARDFV
021200930118     C                   MOVEL     VIDDPC        PARDPC
021300020813     C                   MOVEL     SIMFEL        PARFEL
021400930118     C*
021500950130     C                   CLEAR                   KPJBU
021600930112     C                   MOVEL     PARAM         KPJBU
021700950130     C                   MOVEL     'LSB2'        KCOAZ
021800930112     C                   CALL      'BCH10'
021900930112     C                   PARM                    KPJBA
022000930112     C*
022100930113     C* STAMPA SPUNTE
022200930112IF  1C     VIDSSI        IFEQ      'S'
022300950130     C                   CLEAR                   DSLR01
022400950130     C                   Z-ADD     3             DLRNPG
022500950130     C                   MOVEL     VIDNFI        DLRNFV
022600950131     C                   MOVE      *ALL'9'       DLRNPA
022700950131     C                   MOVE      *ALL'9'       DLRSRA
022800950131     C                   MOVEL     'S'           DLRFS1
022900950131     C                   MOVEL     SIMFEL        DLRFGS
023000950130     C                   CLEAR                   KPJBU
023100950130     C                   MOVEL     DSLR01        KPJBU
023200950130     C                   MOVEL     'LR06'        KCOAZ
023300930112     C                   CALL      'BCH10'
023400930112     C                   PARM                    KPJBA
023500930112E   1C                   ENDIF
023600930112     C*
023700930112     C                   ENDSR
023800930112     C*---------------------------------------------------------------*
023900930112     C*                    DEFVAR                                     *
024000930112     C* ROUTINE DI DEFINIZIONE KLIST, VARIABILI E PLIST               *
024100930112     C*---------------------------------------------------------------*
024200930112     C     DEFVAR        BEGSR
024300930112     C*
024400930112     C     *ENTRY        PLIST
024500930112     C                   PARM                    KPJBA
024600930112     C*
024700020813     ***C                   Z-ADD     1             CODUT             1 0
024800930112     C                   CALL      'X§PARUT'
024900020813     C                   PARM                    UT§DSE0F
025000950130     C                   MOVEL     RAGUT         RSUT
025100930115     C                   MOVEL     REC80         CNCR80
025200950130     C*
025300950130     C                   TIME                    WTIME            14 0
025400950130     C                   MOVE      WTIME         WDATE             8 0
025500930112     C*
025600930112     C* GIRO DATA
025700950130     C                   Z-ADD     WDATE         G02DAT
025800930112     C                   MOVEL     *BLANK        G02ERR
025900950130     C                   CALL      'XSRDA8'
026000930112     C                   PARM                    WLBDAT
026100930112     C* SOTTRAGGO 3gg
026200930112     C     G02TGI        SUB       3             GIOTGI
026300950130     C                   CALL      'XSRGI8'
026400930112     C                   PARM                    WGIDAT
026500930112     C*
026600930112     C* DATA INIZIALE = DATA CORRENTE - 3gg
026700950130     C                   Z-ADD     GIODAT        DATAIN            8 0
026800930112     C*
026900950130     C* LETTURA FNFVV01L
027000930112     C     KFVV          KLIST
027100950130     C                   KFLD                    KNPG
027200950130     C                   KFLD                    KNFV
027300020918     C                   KFLD                    KFgs
027400930112     C*
027500950130     C     *LIKE         DEFINE    FVVNPG        KNPG
027600950130     C     *LIKE         DEFINE    FVVNFV        KNFV
027700020918     C     *LIKE         DEFINE    FVVFgs        KFgs
027800950130     C*
027900930112     C                   ENDSR
