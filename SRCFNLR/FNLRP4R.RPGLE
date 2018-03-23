000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200040305     H* FNLRP4R *----------------------------------------------------*
000300000000     H*-----------------*                                            *
000400040305     H*         - FILTRO elenco spedizioni di valore in distinta     *
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
001700040305     FFNLRP4D   CF   E             WORKSTN
001800920909     FTABEL00F  IF   E           K DISK
001900941230      *--------------------------------------------------------------*
002000920909     D L1              S              3  0 DIM(30)                              FIL GESTITE
002100920909     D L6S             S              3  0 DIM(30)                              FIL GEST SIMFE
002200941230      *--------------------------------------------------------------*
002300040305     D FNLRP5DS      E DS
002400941230      * DS per richiamo pgm FNLR02R
002500941230     D DSLR01        E DS
002600941230     D WLBDA8          DS
002700941230     D  G02DAT                 1      8  0
002800941230     D  G02INV                 9     16  0
002900941230     D  G02ERR                17     17
003000941230     D  G02TGI                18     22  0
003100920909     D CNCR80        E DS
003200020503     d trul06ds      e ds
003300020503     d  lin                    1     90  0 dim(30)
003400920909     D KPJBA         E DS
003500900511     D  LIBSYS                92    100
003600920909     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003700000000     I/SPACE 3
003800900702     C*****************************************************************
003900000000     C     *ENTRY        PLIST
004000000000     C                   PARM                    KPJBA
004100000000     C                   Z-ADD     1             CODUT
004200920909     C                   CALL      'X§PARUT'
004300920909     C                   PARM                    UT§DSE
004400000000     C                   MOVEL     RAGUT         RSUT             20
004500920909     C                   MOVEL     REC80         CNCR80
004600040305     C                   CLEAR                   FNLRP5DS
004700900511     C*---------------------------------------------------------------*
004800920909     C     KTAB2         KLIST
004900920909     C                   KFLD                    CODUT
005000920909     C                   KFLD                    COD               2
005100920909     C                   KFLD                    KEY               8
005200900517     C*
005300920909     C                   EXSR      OPERAZ
005400920909     C*
005500040305     C                   MOVEL     'FNLRP4R'     VIDPGM
005600911016     C*
005700900509     C     FOR01         TAG
005800911127     C                   SETOFF                                       90
005900040305     C                   EXFMT     LRP4D01
006000910320     C** CMD3 - FINE LAVORO
006100900509     C   KC              GOTO      FINE
006200920909     C** CMD18- CAMBIO FILIALE
006300920909     C   KS              SETON                                        04
006400920909     C   KS              GOTO      FOR01
006500920909     C*
006600920909     C   04              DO
006700920909     C     VIDFIL        LOOKUP    L1                                     30
006800920909     C  N30              SETON                                        50
006900920909     C  N30              GOTO      FOR01
007000920909     C*
007100920909     C* NON POSSO CAMBIARE SE E' IN MIA L6
007200920909     C     VIDFIL        IFNE      SIMFEL
007300920909     C     VIDFIL        LOOKUP    L6S                                    49
007400920909     C   49              GOTO      FOR01
007500920909     C                   END
007600920909     C*
007700920909     C                   END
007800911127     C* CONTROLLO LA DATA IMMESSA
007900911127     C*
008000911127     C                   MOVE      VIDDTD        G02DAT
008100911127     C                   MOVEL     *BLANK        G02ERR
008200950102     C                   CALL      'XSRDA8'
008300950102     C                   PARM                    WLBDA8
008400911127     C     G02ERR        IFEQ      '1'
008500911127     C                   SETON                                        11
008600911127     C                   GOTO      FOR01
008700911127     C                   END
008800040305     C                   Z-ADD     G02INV        ip5DAT
008900950102     C                   MOVE      G02DAT        VIDDTD
009000911104     C*
009100911127     C*
009200911127     C* NUMERO FOGLIO VIAGGIO
009300911127     C  N90VIDNDA        IFNE      *BLANKS
009400911127     C     VIDNDA        ANDNE     *ZEROS
009500911127     C*
009600911127     C* RICERCA
009700950102     C     '?'           SCAN      VIDNDA                                 31
009800911127     C   31              DO
009900950102     C                   CLEAR                   DSLR01
010000040305     C                   Z-ADD     ip5DAT        DLRADA
010100040305     C                   Z-ADD     ip5DAT        DLRAAL
010200040308     c                   z-add     viddtd        dlrgda
010300040308     c                   z-add     viddtd        dlrgal
010400950102     C                   Z-ADD     VIDFIL        DLRFGS
010500040308     C                   MOVEL     'FNLRP4R'     DLRPGM
010600950102     C                   MOVEL     'S'           DLRRIC
010700040305     C                   MOVEL     ' '           DLRTFV
010800950102     C                   Z-ADD     4             DLRNPG
010900950102     C                   MOVEL     DSLR01        KPJBU
011000950102     C                   CALL      'FNLR02R'
011100911127     C                   PARM                    KPJBA
011200950102     C                   MOVEL     KPJBU         DSLR01
011300911127     C                   MOVE      *ZEROS        VIDNDA
011400950102     C                   MOVE      DLRNFV        VIDNDA
011500911127     C                   GOTO      FOR01
011600911127     C                   END
011700911127     C* CONTROLLI
011800911127     C                   TESTN                   VIDNDA               31
011900911127     C  N31              SETON                                        32
012000911127     C*
012100040305     C  N32              MOVE      VIDNDA        ip5NRD
012200911127     C                   ELSE
012300040305     C                   Z-ADD     *ZEROS        ip5NRD
012400911127     C                   END
012500911127     C*
012600911127     C* NUMERO FOGLIO VIAGGIO
012700911127     C  N90VIDNAL        IFNE      *BLANKS
012800911127     C     VIDNAL        ANDNE     *ZEROS
012900911127     C* RICERCA
013000950102     C     '?'           SCAN      VIDNAL                                 31
013100911127     C   31              DO
013200950102     C                   CLEAR                   DSLR01
013300040305     C                   Z-ADD     ip5DAT        DLRADA
013400040305     C                   Z-ADD     ip5DAT        DLRAAL
013500040308     c                   z-add     viddtd        dlrgda
013600040308     c                   z-add     viddtd        dlrgal
013700950102     C                   Z-ADD     VIDFIL        DLRFGS
013800950102     C                   MOVEL     'FNLR54R'     DLRPGM
013900950102     C                   MOVEL     'S'           DLRRIC
014000950131     C                   MOVEL     ' '           DLRTFV
014100950102     C                   Z-ADD     4             DLRNPG
014200950102     C                   MOVEL     DSLR01        KPJBU
014300950102     C                   CALL      'FNLR02R'
014400950102     C                   PARM                    KPJBA
014500950102     C                   MOVEL     KPJBU         DSLR01
014600950102     C                   MOVE      *ZEROS        VIDNAL
014700950102     C                   MOVE      DLRNFV        VIDNAL
014800950102     C                   GOTO      FOR01
014900911127     C                   END
015000911127     C* CONTROLLI
015100911127     C                   TESTN                   VIDNAL               31
015200911127     C  N31              SETON                                        32
015300911127     C*
015400040305     C  N32              MOVE      VIDNAL        ip5NRA
015500911127     C                   ELSE
015600040305     C                   Z-ADD     *ZEROS        ip5NRA
015700911127     C                   END
015800911127     C* CONTROLLO I NUMERI IMPOSTATI A VIDEO
015900040305     C  N90ip5NRA        IFEQ      *ZEROS
016000040305     C                   Z-ADD     ip5NRD        ip5NRA
016100911127     C                   ELSE
016200040305     C     ip5NRD        IFGT      ip5NRA
016300911127     C                   SETON                                        12
016400911127     C                   GOTO      FOR01
016500911127     C                   END
016600911104     C                   END
016700040305     C     ip5NRA        IFEQ      *ZEROS
016800040305     C     ip5NRD        ANDEQ     *ZEROS
016900040305     C                   Z-ADD     9999999       ip5NRA
017000911128     C                   END
017100920909     C*
017200950102     C  NKF              GOTO      FOR01
017300040305     C                   MOVE      VIDFIL        ip5FIL
017400010215      *--
017500010215      *--
017600911104     C*
017700950102     C* CMD6 - ELABORAZIONE
017800040305     C                   MOVEL(p)  FNLRP5DS      KPJBU
017900040305     C                   MOVEL     'LRP5'        KCOAZ
018000010216     C     KNMUS         IFEQ      *ALL'1'
018100040305     C                   CALL      'FNLRP5R'
018200900702     C                   PARM                    KPJBA
018300010216     C                   ELSE
018400010216     C                   CALL      'BCH10'
018500010216     C                   PARM                    KPJBA
018600010216     C                   END
018700911127     C                   MOVEL     *ZEROS        VIDDTD
018800911127     C                   MOVE      *BLANKS       VIDNDA
018900911127     C                   MOVE      *BLANKS       VIDNAL
019000920909     C                   SETOFF                                       04
019100911127     C                   GOTO      FOR01
019200910320     C*
019300000000     C     FINE          TAG
019400000000     C                   SETON                                        LR
019500920909     C*
019600920909     C* OPERAZIONI INIZIALI -------------------------------------------
019700920909     C     OPERAZ        BEGSR
019800920909     C*
019900920909     C* VEDO SE SONO SIMFEL O REMOTO
020000920909     C*
020100020730     C                   MOVEL     SIMPOU        VIDFIL
020200920909     C                   EXSR      CARL6
020300020730     C*
020400020730     C     SimTpP        IFEQ      '2'
020500020730     C     SimTpP        OREQ      *BLANK
020600920909     C                   SETON                                        05
020700920909     C                   END
020800920909     C*
020900920909     C* CARICO TABELLA FILIALI GESTITE £1
021000020503     c                   clear                   trul06ds
021100020503     c                   eval      d06cod = '£1'
021200020730     c                   movel     simPOU        d06key
021300020503     c                   movel     trul06ds      kpjbu
021400020503     c                   call      'TRUL06R'
021500020503     c                   parm                    kpjba
021600020503     c                   movel     kpjbu         trul06ds
021700020503     c                   movea     lin           L1
021800920909     C                   ENDSR
021900920909     C* CARICO L6 ------------------------------------------***********
022000920909     C     CARL6         BEGSR
022100020503     c                   clear                   trul06ds
022200020503     c                   eval      d06cod = '£6'
022300020503     c                   movel     vidfil        d06key
022400020503     c                   movel     trul06ds      kpjbu
022500020503     c                   call      'TRUL06R'
022600020503     c                   parm                    kpjba
022700020503     c                   movel     kpjbu         trul06ds
022800020916     c                   movea     lin           L6s
022900920909     C                   ENDSR
