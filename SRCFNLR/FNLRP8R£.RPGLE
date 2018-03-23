000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200050512     H* FNLRP6R *----------------------------------------------------*
000300050512     H*         - FILTRO elenco spedizioni in distinta
000400050512     H*         -        DI DIVERSO TIPO ZONA CONSEGNA
000500940221      *--------------------------------------------------------------*
000600940221      * 11 - ERRMSG: DATA ERRATA                                     *
000700940221      * 12 - ERRMSG: NUMERO DAL MAGGIORE NUMERO AL                   *
000800940221      * 30 - INDICATORE DI COMODO                                    *
000900940221      * 31 - INDICATORE DI COMODO                                    *
001000940221      * 32 - INDICATORE DI COMODO                                    *
001100940221      * 90 - INDICATORE DI COMODO                                    *
001200940221      *--------------------------------------------------------------*
001300050512     FFNLRP6D   CF   E             WORKSTN
001400050512      *--------------------------------------------------------------*
001500050512     D*  DS PER PGM BATCH . FNLRP7R
001600050512     D FNLRP7DS      E DS
001700941230      * DS per richiamo pgm FNLR02R
001800941230     D DSLR01        E DS
001900941230     D WLBDA8          DS
002000941230     D  G02DAT                 1      8  0
002100941230     D  G02INV                 9     16  0
002200941230     D  G02ERR                17     17
002300941230     D  G02TGI                18     22  0
002400920909     D CNCR80        E DS
002500920909     D KPJBA         E DS
002600920909     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002700050513     d savtpm          s                   like(vidtpm)
002800900702     C*****************************************************************
002900000000     C     *ENTRY        PLIST
003000000000     C                   PARM                    KPJBA
003100000000     C                   Z-ADD     1             CODUT
003200920909     C                   CALL      'X§PARUT'
003300920909     C                   PARM                    UT§DSE
003400000000     C                   MOVEL     RAGUT         RSUT             20
003500920909     C                   MOVEL     REC80         CNCR80
003600050512     C                   CLEAR                   FNLRP7DS
003700050512     C
003800050512     C                   MOVEL     SIMPOU        VIDFIL
003900050512     C
004000050512     C                   MOVEL     'FNLRP6R'     VIDPGM
004100050512     C*---------------------------------------------------------------*
004200911016     C*
004300050513     c                   eval      vidtpm='M'
004400050513     c                   eval      savtpm='M'
004500050513     c                   eval      alttpm='P A R C E L'
004600900509     C     FOR01         TAG
004700911127     C                   SETOFF                                       90
004800050512     C                   EXFMT     LRP6D01
004900050512     C** F3 - FINE LAVORO
005000900509     C   KC              GOTO      FINE
005100050513     c* se imposto 'P'  scrivo messaggerie nell'altro tipo zona consegna
005200050513     c                   if        vidtpm<>savtpm
005300050513     c                   if        vidtpm='P'
005400050513     c                   eval      alttpm='MESSAGGERIE'
005500050513     c                   endif
005600050513     c                   if        vidtpm='M'
005700050513     c                   eval      alttpm='P A R C E L'
005800050513     c                   endif
005900050513     c
006000050513     c                   eval      savtpm=vidtpm
006100050513     c* riemetto videata
006200050513     c                   goto      for01
006300050513     c                   endif
006400920909     C*
006500911127     C* CONTROLLO LA DATA IMMESSA
006600911127     C*
006700911127     C                   MOVE      VIDDTD        G02DAT
006800911127     C                   MOVEL     *BLANK        G02ERR
006900950102     C                   CALL      'XSRDA8'
007000950102     C                   PARM                    WLBDA8
007100911127     C     G02ERR        IFEQ      '1'
007200911127     C                   SETON                                        11
007300911127     C                   GOTO      FOR01
007400911127     C                   END
007500050512     C                   Z-ADD     G02INV        ip7DAT
007600950102     C                   MOVE      G02DAT        VIDDTD
007700911104     C*
007800050512     C* NUMERO FOGLIO
007900911127     C  N90VIDNDA        IFNE      *BLANKS
008000911127     C     VIDNDA        ANDNE     *ZEROS
008100911127     C*
008200911127     C* RICERCA
008300950102     C     '?'           SCAN      VIDNDA                                 31
008400911127     C   31              DO
008500950102     C                   CLEAR                   DSLR01
008600050512     C                   Z-ADD     ip7DAT        DLRADA
008700050512     C                   Z-ADD     ip7DAT        DLRAAL
008800040308     c                   z-add     viddtd        dlrgda
008900040308     c                   z-add     viddtd        dlrgal
009000950102     C                   Z-ADD     VIDFIL        DLRFGS
009100050512     C                   MOVEL     'FNLRP6R'     DLRPGM
009200950102     C                   MOVEL     'S'           DLRRIC
009300040305     C                   MOVEL     ' '           DLRTFV
009400950102     C                   Z-ADD     4             DLRNPG
009500950102     C                   MOVEL     DSLR01        KPJBU
009600950102     C                   CALL      'FNLR02R'
009700911127     C                   PARM                    KPJBA
009800950102     C                   MOVEL     KPJBU         DSLR01
009900911127     C                   MOVE      *ZEROS        VIDNDA
010000950102     C                   MOVE      DLRNFV        VIDNDA
010100911127     C                   GOTO      FOR01
010200911127     C                   END
010300911127     C* CONTROLLI
010400911127     C                   TESTN                   VIDNDA               31
010500911127     C  N31              SETON                                        32
010600911127     C*
010700050512     C  N32              MOVE      VIDNDA        ip7NRD
010800911127     C                   ELSE
010900050512     C                   Z-ADD     *ZEROS        ip7NRD
011000911127     C                   END
011100911127     C*
011200911127     C* NUMERO FOGLIO VIAGGIO
011300911127     C  N90VIDNAL        IFNE      *BLANKS
011400911127     C     VIDNAL        ANDNE     *ZEROS
011500911127     C* RICERCA
011600950102     C     '?'           SCAN      VIDNAL                                 31
011700911127     C   31              DO
011800950102     C                   CLEAR                   DSLR01
011900050512     C                   Z-ADD     ip7DAT        DLRADA
012000050512     C                   Z-ADD     ip7DAT        DLRAAL
012100040308     c                   z-add     viddtd        dlrgda
012200040308     c                   z-add     viddtd        dlrgal
012300950102     C                   Z-ADD     VIDFIL        DLRFGS
012400050512     C                   MOVEL     'FNLRP6R'     DLRPGM
012500950102     C                   MOVEL     'S'           DLRRIC
012600950131     C                   MOVEL     ' '           DLRTFV
012700950102     C                   Z-ADD     4             DLRNPG
012800950102     C                   MOVEL     DSLR01        KPJBU
012900950102     C                   CALL      'FNLR02R'
013000950102     C                   PARM                    KPJBA
013100950102     C                   MOVEL     KPJBU         DSLR01
013200950102     C                   MOVE      *ZEROS        VIDNAL
013300950102     C                   MOVE      DLRNFV        VIDNAL
013400950102     C                   GOTO      FOR01
013500911127     C                   END
013600911127     C* CONTROLLI
013700911127     C                   TESTN                   VIDNAL               31
013800911127     C  N31              SETON                                        32
013900911127     C*
014000050512     C  N32              MOVE      VIDNAL        ip7NRA
014100911127     C                   ELSE
014200050512     C                   Z-ADD     *ZEROS        ip7NRA
014300911127     C                   END
014400911127     C* CONTROLLO I NUMERI IMPOSTATI A VIDEO
014500050512     C  N90ip7NRA        IFEQ      *ZEROS
014600050512     C                   Z-ADD     ip7NRD        ip7NRA
014700911127     C                   ELSE
014800050512     C     ip7NRD        IFGT      ip7NRA
014900911127     C                   SETON                                        12
015000911127     C                   GOTO      FOR01
015100911127     C                   END
015200911104     C                   END
015300050512     C     ip7NRA        IFEQ      *ZEROS
015400050512     C     ip7NRD        ANDEQ     *ZEROS
015500050512     C                   Z-ADD     9999999       ip7NRA
015600911128     C                   END
015700920909     C*
015800950102     C  NKF              GOTO      FOR01
015900050512     C                   MOVE      VIDFIL        ip7FIL
016000050516     C                   MOVE      VIDtpm        ip7tpm
016100010215      *--
016200050512     C* F6 - ELABORAZIONE
016300050512     C                   MOVEL(p)  FNLRP7DS      KPJBU
016400050512     C                   MOVEL     'LRP7'        KCOAZ
016500010216     C                   CALL      'BCH10'
016600010216     C                   PARM                    KPJBA
016700911127     C                   MOVEL     *ZEROS        VIDDTD
016800911127     C                   MOVE      *BLANKS       VIDNDA
016900911127     C                   MOVE      *BLANKS       VIDNAL
017000910320     C*
017100000000     C     FINE          TAG
017200000000     C                   SETON                                        LR
