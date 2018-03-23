000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941230     H* FNLR54R *----------------------------------------------------*
000300000000     H*-----------------*                                            *
000400911127     H*         - FILTRO LISTA DISTINTE DI CONSEGNA
000500940221      *--------------------------------------------------------------*
000600940221      * 04 - CAMBIO FILIALE                                          *
000700940221      * 05 - PRIMO LIVELLO (OFF)/SECONDO LIVELLO (ON)                *
000800940221      * 11 - ERRMSG: DATA ERRATA                                     *
000900940221      * 12 - ERRMSG: NUMERO DAL MAGGIORE NUMERO AL                   *
001000940221      * 30 - INDICATORE DI COMODO                                    *
001100940221      * 31 - INDICATORE DI COMODO                                    *
001200940221      * 32 - INDICATORE DI COMODO                                    *
001300940221      * 49 - ERRMSG: FILIALE IN GESTIOEN AL ALTRA FILIALE            *
001400940221      * 50 - ERRMSG: FILIALE DA GESTIRE ERRATA                       *
001500940221      * 90 - INDICATORE DI COMODO                                    *
001600940221      *--------------------------------------------------------------*
001700941230     FFNLR54D   CF   E             WORKSTN
001800920909     FTABEL00F  IF   E           K DISK
001900941230      *--------------------------------------------------------------*
002000920909     D L1              S              3  0 DIM(30)                              FIL GESTITE
002200920909     D L6S             S              3  0 DIM(30)                              FIL GEST SIMFE
002400941230      *--------------------------------------------------------------*
002500941230     D FNLR55        E DS                  EXTNAME(FNLR55DS)
002600941230      * DS per richiamo pgm FNLR02R
002700941230     D DSLR01        E DS
002800941230     D WLBDA8          DS
002900941230     D  G02DAT                 1      8  0
003000941230     D  G02INV                 9     16  0
003100941230     D  G02ERR                17     17
003200941230     D  G02TGI                18     22  0
003300920909     D CNCR80        E DS
003400020503     d trul06ds      e ds
003500020503     d  lin                    1     90  0 dim(30)
003600920909     D KPJBA         E DS
003700900511     D  LIBSYS                92    100
003800920909     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003900000000     I/SPACE 3
004000900702     C*****************************************************************
004100000000     C     *ENTRY        PLIST
004200000000     C                   PARM                    KPJBA
004300000000     C                   Z-ADD     1             CODUT
004400920909     C                   CALL      'X§PARUT'
004500920909     C                   PARM                    UT§DSE
004600000000     C                   MOVEL     RAGUT         RSUT             20
004700920909     C                   MOVEL     REC80         CNCR80
004800950929     C                   CLEAR                   FNLR55
004801050204     c* per profili EDP lascio la possibilità di lanciare anche le
004802050204     c* distinte di uscita
004803050204     c                   if        %subst(knmus: 1: 3)='EDP'
004804050204     c                   seton                                        60
004805050204     c                   end
004900900511     C*---------------------------------------------------------------*
005000920909     C     KTAB2         KLIST
005100920909     C                   KFLD                    CODUT
005200920909     C                   KFLD                    COD               2
005300920909     C                   KFLD                    KEY               8
005400900517     C*
005500920909     C                   EXSR      OPERAZ
005600920909     C*
005700941230     C                   MOVEL     'FNLR54R'     VIDPGM
005800911127     C                   MOVEL     'A'           VIDTIP
005900911128     C                   MOVE      'SI'          VIDSIN
006000050204     C                   MOVE      'O'           VIDSTP
006100911016     C*
006200900509     C     FOR01         TAG
006300911127     C                   SETOFF                                       90
006400950102     C                   EXFMT     LR54D01
006500910320     C** CMD3 - FINE LAVORO
006600900509     C   KC              GOTO      FINE
006700920909     C** CMD18- CAMBIO FILIALE
006800920909     C   KS              SETON                                        04
006900920909     C   KS              GOTO      FOR01
007000920909     C*
007100920909     C   04              DO
007200920909     C     VIDFIL        LOOKUP    L1                                     30
007300920909     C  N30              SETON                                        50
007400920909     C  N30              GOTO      FOR01
007500920909     C*
007600920909     C* NON POSSO CAMBIARE SE E' IN MIA L6
007700920909     C     VIDFIL        IFNE      SIMFEL
007800920909     C     VIDFIL        LOOKUP    L6S                                    49
007900920909     C   49              GOTO      FOR01
008000920909     C                   END
008100920909     C*
008300920909     C                   END
008400911127     C* CONTROLLO LA DATA IMMESSA
008500911127     C*
008600911127     C                   MOVE      VIDDTD        G02DAT
008700911127     C                   MOVEL     *BLANK        G02ERR
008800950102     C                   CALL      'XSRDA8'
008900950102     C                   PARM                    WLBDA8
009000911127     C     G02ERR        IFEQ      '1'
009100911127     C                   SETON                                        11
009200911127     C                   GOTO      FOR01
009300911127     C                   END
009400950102     C                   Z-ADD     G02INV        D55DAT
009500950102     C                   MOVE      G02DAT        VIDDTD
009600911104     C*
009700911127     C*
009800911127     C* NUMERO FOGLIO VIAGGIO
009900911127     C  N90VIDNDA        IFNE      *BLANKS
010000911127     C     VIDNDA        ANDNE     *ZEROS
010100911127     C*
010200911127     C* RICERCA
010300950102     C     '?'           SCAN      VIDNDA                                 31
010400911127     C   31              DO
010500950102     C                   CLEAR                   DSLR01
010600950102     C                   Z-ADD     D55DAT        DLRADA
010700950102     C                   Z-ADD     D55DAT        DLRAAL
010800950102     C                   Z-ADD     VIDFIL        DLRFGS
010900950102     C                   MOVEL     'FNLR54R'     DLRPGM
011000950102     C                   MOVEL     'S'           DLRRIC
011100950131     C     VIDTIP        IFEQ      'A'
011200950102     C                   MOVEL     '2'           DLRTFV
011300950131     C                   END
011400950131     C     VIDTIP        IFEQ      'C'
011500950131     C                   MOVEL     '1'           DLRTFV
011600950131     C                   END
011700950131     C     VIDTIP        IFEQ      'T'
011800950131     C                   MOVEL     ' '           DLRTFV
011900950131     C                   END
012000950102     C                   Z-ADD     4             DLRNPG
012100950102     C                   MOVEL     DSLR01        KPJBU
012200950102     C                   CALL      'FNLR02R'
012300911127     C                   PARM                    KPJBA
012400950102     C                   MOVEL     KPJBU         DSLR01
012500911127     C                   MOVE      *ZEROS        VIDNDA
012600950102     C                   MOVE      DLRNFV        VIDNDA
012700911127     C                   GOTO      FOR01
012800911127     C                   END
012900911127     C* CONTROLLI
013000911127     C                   TESTN                   VIDNDA               31
013100911127     C  N31              SETON                                        32
013200911127     C*
013300950102     C  N32              MOVE      VIDNDA        D55NRD
013400911127     C                   ELSE
013500950102     C                   Z-ADD     *ZEROS        D55NRD
013600911127     C                   END
013700911127     C*
013800911127     C* NUMERO FOGLIO VIAGGIO
013900911127     C  N90VIDNAL        IFNE      *BLANKS
014000911127     C     VIDNAL        ANDNE     *ZEROS
014100911127     C* RICERCA
014200950102     C     '?'           SCAN      VIDNAL                                 31
014300911127     C   31              DO
014400950102     C                   CLEAR                   DSLR01
014500950102     C                   Z-ADD     D55DAT        DLRADA
014600950102     C                   Z-ADD     D55DAT        DLRAAL
014700950102     C                   Z-ADD     VIDFIL        DLRFGS
014800950102     C                   MOVEL     'FNLR54R'     DLRPGM
014900950102     C                   MOVEL     'S'           DLRRIC
015000950131     C     VIDTIP        IFEQ      'A'
015100950131     C                   MOVEL     '2'           DLRTFV
015200950131     C                   END
015300950131     C     VIDTIP        IFEQ      'C'
015400950131     C                   MOVEL     '1'           DLRTFV
015500950131     C                   END
015600950131     C     VIDTIP        IFEQ      'T'
015700950131     C                   MOVEL     ' '           DLRTFV
015800950131     C                   END
015900950102     C                   Z-ADD     4             DLRNPG
016000950102     C                   MOVEL     DSLR01        KPJBU
016100950102     C                   CALL      'FNLR02R'
016200950102     C                   PARM                    KPJBA
016300950102     C                   MOVEL     KPJBU         DSLR01
016400950102     C                   MOVE      *ZEROS        VIDNAL
016500950102     C                   MOVE      DLRNFV        VIDNAL
016600950102     C                   GOTO      FOR01
016700911127     C                   END
016800911127     C* CONTROLLI
016900911127     C                   TESTN                   VIDNAL               31
017000911127     C  N31              SETON                                        32
017100911127     C*
017200950102     C  N32              MOVE      VIDNAL        D55NRA
017300911127     C                   ELSE
017400950102     C                   Z-ADD     *ZEROS        D55NRA
017500911127     C                   END
017600911127     C* CONTROLLO I NUMERI IMPOSTATI A VIDEO
017700950102     C  N90D55NRA        IFEQ      *ZEROS
017800950102     C                   Z-ADD     D55NRD        D55NRA
017900911127     C                   ELSE
018000950102     C     D55NRD        IFGT      D55NRA
018100911127     C                   SETON                                        12
018200911127     C                   GOTO      FOR01
018300911127     C                   END
018400911104     C                   END
018500950102     C     D55NRA        IFEQ      *ZEROS
018600950102     C     D55NRD        ANDEQ     *ZEROS
018700950102     C                   Z-ADD     9999999       D55NRA
018800911128     C                   END
018900920909     C*
019000950102     C  NKF              GOTO      FOR01
019100950102     C                   MOVE      VIDFIL        D55FIL
019200950102     C                   MOVE      VIDTIP        D55TIP
019300010215      *--
019400010215     C                   MOVE      VIDSTP        D55STP
019500010215      *--
019600911104     C*
019700950102     C* CMD6 - ELABORAZIONE
019800030625     C                   MOVEL(p)  FNLR55        KPJBU
019900050427     C                   MOVEL     'LR5B'        KCOAZ
020000010216     C     KNMUS         IFEQ      *ALL'1'
020100050427     C                   CALL      'FNLR55C'
020200900702     C                   PARM                    KPJBA
020300010216     C                   ELSE
020400010216     C                   CALL      'BCH10'
020500010216     C                   PARM                    KPJBA
020600010216     C                   END
020700911127     C                   MOVEL     *ZEROS        VIDDTD
020800911127     C                   MOVE      *BLANKS       VIDNDA
020900911127     C                   MOVE      *BLANKS       VIDNAL
021000911127     C                   MOVE      'A'           VIDTIP
021100911128     C                   MOVE      'SI'          VIDSIN
021200050204     C                   MOVE      'O'           VIDSTP
021300920909     C                   SETOFF                                       04
021400911127     C                   GOTO      FOR01
021500910320     C*
021600000000     C     FINE          TAG
021700000000     C                   SETON                                        LR
021800920909     C*
021900920909     C* OPERAZIONI INIZIALI -------------------------------------------
022000920909     C     OPERAZ        BEGSR
022100920909     C*
022200920909     C* VEDO SE SONO SIMFEL O REMOTO
022300920909     C*
022400020730     C                   MOVEL     SIMPOU        VIDFIL
022500920909     C                   EXSR      CARL6
022700020730     C*
022800020730     C     SimTpP        IFEQ      '2'
022900020730     C     SimTpP        OREQ      *BLANK
023000920909     C                   SETON                                        05
023100920909     C                   END
023200920909     C*
023300920909     C* CARICO TABELLA FILIALI GESTITE £1
023400020503     c                   clear                   trul06ds
023500020503     c                   eval      d06cod = '£1'
023600020730     c                   movel     simPOU        d06key
023700020503     c                   movel     trul06ds      kpjbu
023800020503     c                   call      'TRUL06R'
023900020503     c                   parm                    kpjba
024000020503     c                   movel     kpjbu         trul06ds
024100020503     c                   movea     lin           L1
026200920909     C                   ENDSR
026300920909     C* CARICO L6 ------------------------------------------***********
026400920909     C     CARL6         BEGSR
026500020503     c                   clear                   trul06ds
026600020503     c                   eval      d06cod = '£6'
026700020503     c                   movel     vidfil        d06key
026800020503     c                   movel     trul06ds      kpjbu
026900020503     c                   call      'TRUL06R'
027000020503     c                   parm                    kpjba
027100020503     c                   movel     kpjbu         trul06ds
027200020916     c                   movea     lin           L6s
029400920909     C                   ENDSR
