000100911024     H DECEDIT('0,') DATEDIT(*DMY.)
000200941201     H* FNLR57R *----------------------------------------------------*
000300911024     H*-----------------*                                            *
000400920619     H*    LANCIO STAMPA LISTA BOLLE DI RESO                         *
000500911024     H*--------------------------------------------------------------*
000600941201     FFNLR57D   CF   E             WORKSTN
000700911024     FTABEL00F  IF   E           K DISK
000800020506     D*UNI             S              3    DIM(29)
000900920622     D LMN             S              3  0 DIM(29)                              FILIALI
001000911024     D PARAM           DS
001100920619     D  LIN                    1     87  0
001200911104     D                                     DIM(29) DESCEND
001300920619     D  VIDLNA                88     90  0
001400941201     D  DATE                  91     98  0
001500911024     D KPJBA         E DS
001600911024     D  LIBSYS                92    100
001700911024     D  MSGERR               453    502
001800911024     D CNCR80        E DS
001900020506     d trul06ds      e ds
002000020506     d  lint                   1     87  0 dim(29)
002100911024     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002200911024     D  TCU                  398    697
002300911024     D                                     DIM(50)                              TAB FLAG CNT
002400911024     D  KCU                  698    847P 0
002500911024     D                                     DIM(50)                              TAB CAPO CONTO
002600911024     D                                     PACKEVEN
002700911024     D  DTU                  848    895P 0
002800911024     D                                     DIM(12)                              DATE PARAM
002900911024     D                                     PACKEVEN
003000911115      *
003100941201     D WLBDA8          DS
003200941201     D  G08DAT                 1      8  0
003300941201     D  G08INV                 9     16  0
003400941201     D  G08ERR                17     17
003500941201     D  G08TGI                18     22  0
003600911115      *
003700941201     D                SDS
003800941201     D  §PGM                   1     10
003900911024     C*---------------------------------------------------------------*
004000911024     C     *ENTRY        PLIST
004100911024     C                   PARM                    KPJBA
004200911024     C                   Z-ADD     1             CODUT
004300911024     C                   CALL      'X§PARUT'
004400911024     C                   PARM                    UT§DSE
004500911024     C     CODUT         CABEQ     -1            FINE
004600911024     C                   MOVEL     REC80         CNCR80
004700911024     C                   MOVEL     RAGUT         RSUT             20
004800911024     C*
004900911024     C                   CALL      'XKCDUS'
005000911024     C                   PARM                    KNMUS
005100911024     C                   PARM                    KCDUS             3 0
005200911024     C* RECUPERO LE LINEE DI ARRIVO
005300941202     C*§§
005400941202     C                   TIME                    WHHUDT           14 0
005500941202     C                   MOVEL     WHHUDT        ORA               6 0
005600941202     C                   MOVE      WHHUDT        GMAUD8            8 0
005700911024     C*
005800911024     C* SE PROFILO UGUALE A REM PRENDO LE FILIALI A LORO ASSOCIATE
005900911024     C*
006000020730     C                   Z-ADD     SIMPOU        FILIA             3 0
006100020730     C     SimTpP        IFEQ      '2'
006200911025     C                   SETOFF                                       21
006300911024     C                   ELSE
006400911025     C                   SETON                                        21
006500911024     C                   END
006600911024     C* AGGANCIO LA £6 E RECUPERO LE LINEE A LORO ASSOCIATE
006700911024     C*
006800020506     c                   clear                   trul06ds
006900020506     c                   eval      d06cod = '£6'
007000020506     c                   movel     filia         d06key
007100020506     c                   movel     trul06ds      kpjbu
007200020506     c                   call      'TRUL06R'
007300020506     c                   parm                    kpjba
007400020506     c                   movel     kpjbu         trul06ds
007500020506     c                   movea     lint          Lin
007600020506     C*                  MOVEL     '£6'          TBLCOD
007700020506     C*                  MOVE      *BLANKS       TBLKEY
007800020506     C*                  MOVEL     FILIA         TBLKEY
007900911024     C*
008000020506     C*    KTAB          CHAIN     TABEL00F                           51
008100020506     C* N51              MOVEA     TBLUNI        UNI
008200020506     C* N51              MOVE      UNI           LIN
008300020506     C*  51              MOVE      *BLANKS       LIN
008400020506     C*  51              MOVE      FILIA         LIN(1)
008500911024     C*
008600911024     C*---------------------------------------------------------------*
008700911024     C*
008800911024     C     KTAB          KLIST
008900911024     C                   KFLD                    CODUT
009000911024     C                   KFLD                    TBLCOD
009100911024     C                   KFLD                    TBLKEY
009200911024     C*
009300911024     C     INIZI1        TAG
009400911025     C                   SETON                                        20
009500920619     C                   Z-ADD     FILIA         VIDLNA
009600941202     C                   Z-ADD     GMAUD8        VIDDAM
009700920619     C*
009800911024     C     INIZIO        TAG
009900911115     C                   SETOFF                                       90
010000941201     C                   EXFMT     LR57D01
010100911024     C** CMD03 - FINE LAVORO
010200911024     C   KC              GOTO      FINE
010300911025     C*
010400911025     C   21
010500911025     CAN KS              SETOFF                                       20
010600911025     C   21
010700911025     CAN KS              SETON                                        90
010800920622      *
010900920622      *
011000920622     C*Per CMD18 eseguo controlli e carico £6
011100920622     C   21              DO                                                     *
011200020506     c                   clear                   trul06ds
011300020506     c                   eval      d06cod = '£1'
011400020506     c                   movel     simfel        d06key
011500020506     c                   movel     trul06ds      kpjbu
011600020506     c                   call      'TRUL06R'
011700020506     c                   parm                    kpjba
011800020506     c                   movel     kpjbu         trul06ds
011900020506     c                   movea     lint          Lmn
012000020506     C*                  MOVEL     '£1'          TBLCOD
012100020506     C*                  MOVEL     *BLANKS       TBLKEY
012200020506     C*                  MOVEL     SIMFEL        TBLKEY
012300020506     C*    KTAB          CHAIN     TABEL00F                           51
012400020506     C*  51              SETON                                        1790
012500020506     C* N51              MOVEA     TBLUNI        UNI
012600020506     C* N51              MOVE      UNI           LMN
012700920622     C*
012800920622     C* CONTROLLO SE ESISTE LA FILIALE DA ME DIGITATA
012900920622     C*
013000920622     C     VIDLNA        LOOKUP    LMN                                    70
013100920622     C  N70              SETON                                        1790
013200920622     C*
013300920622     C   70              DO
013400920622     C* RECUPERO LE LINEE DI ARRIVO DALLA TABELLA £6
013500920622     C*
013600920622     C                   MOVE      VIDLNA        FILIA
013700920622     C* AGGANCIO LA £6 E RECUPERO LE LINEE A LORO ASSOCIATE
013800920622     C*
013900020506     c                   clear                   trul06ds
014000020506     c                   eval      d06cod = '£6'
014100020506     c                   movel     filia         d06key
014200020506     c                   movel     trul06ds      kpjbu
014300020506     c                   call      'TRUL06R'
014400020506     c                   parm                    kpjba
014500020506     c                   movel     kpjbu         trul06ds
014600020506     c                   movea     lint          Lin
014700020506     C*                  MOVEL     '£6'          TBLCOD
014800020506     C*                  MOVEL     *BLANKS       TBLKEY
014900020506     C*                  MOVEL     FILIA         TBLKEY
015000920622     C*
015100020506     C*    KTAB          CHAIN     TABEL00F                           51
015200020506     C* N51              MOVEA     TBLUNI        UNI
015300020506     C* N51              MOVE      UNI           LIN
015400020506     C*  51              MOVE      *BLANKS       LIN
015500020506     C*  51              MOVE      FILIA         LIN(1)
015600020506     C                   END
015700920622     C                   END
015800920622      *
015900911025     C  N90              EXSR      CONTR
016000920622      *
016100941201     C  NKF
016200911024     COR 90              GOTO      INIZIO
016300911024     C*
016400911024     C                   SORTA     LIN
016500911024     C                   MOVEL     PARAM         KPJBU
016600941201     C                   MOVEL     'LR58'        KCOAZ
016700911024     C                   CALL      'BCH10'
016800911024     C                   PARM                    KPJBA
016900920622     C*
017000911024     C     FINE          TAG
017100911024     C                   SETON                                        LR
017200920622      *
017300920622      *
017400920622      *
017500911024     C*--- CONTROLLI FORMATO1 ----------------------------------------*
017600911024     C     CONTR         BEGSR
017700911024     C                   SETOFF                                       90
017800911024     C* CONTROLLO CHE ESISTA
017900911024     C     VIDLNA        LOOKUP    LIN                                    70
018000911024     C* PER ERRORE
018100911024     C  N70              SETON                                        9021
018200911024     C   70              MOVE      *ZEROS        LIN
018300911024     C   70              MOVE      VIDLNA        LIN(1)
018400920619      *
018500920619     C* Routine di controllo data
018600941201     C                   MOVE      VIDDAM        G08DAT
018700941202     C                   MOVE      *ZEROS        G08INV
018800941201     C                   MOVEL     *BLANK        G08ERR
018900941201     C                   CALL      'XSRDA8'
019000941201     C                   PARM                    WLBDA8
019100941201     C     G08ERR        COMP      '1'                                    10
019200911115     C   10              SETON                                        90
019300941202     C  N10              Z-ADD     G08DAT        VIDDAM
019400941202     C  N10              Z-ADD     G08INV        DATE
019500911024     C*
019600911024     C     ENDCTR        ENDSR
019700911024     C*
