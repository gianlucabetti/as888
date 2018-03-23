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
004900911024     C* RECUPERO LE LINEE DI ARRIVO
005000941202     C*§§
005100941202     C                   TIME                    WHHUDT           14 0
005200941202     C                   MOVEL     WHHUDT        ORA               6 0
005300941202     C                   MOVE      WHHUDT        GMAUD8            8 0
005400911024     C*
005500911024     C* SE PROFILO UGUALE A REM PRENDO LE FILIALI A LORO ASSOCIATE
005600911024     C*
005700020730     C                   Z-ADD     SIMPOU        FILIA             3 0
005800020730     C     SimTpP        IFEQ      '2'
005900911025     C                   SETOFF                                       21
006000911024     C                   ELSE
006100911025     C                   SETON                                        21
006200911024     C                   END
006300911024     C* AGGANCIO LA £6 E RECUPERO LE LINEE A LORO ASSOCIATE
006400911024     C*
006500020506     c                   clear                   trul06ds
006600020506     c                   eval      d06cod = '£6'
006700020506     c                   movel     filia         d06key
006800020506     c                   movel     trul06ds      kpjbu
006900020506     c                   call      'TRUL06R'
007000020506     c                   parm                    kpjba
007100020506     c                   movel     kpjbu         trul06ds
007200020506     c                   movea     lint          Lin
007300020506     C*                  MOVEL     '£6'          TBLCOD
007400020506     C*                  MOVE      *BLANKS       TBLKEY
007500020506     C*                  MOVEL     FILIA         TBLKEY
007600911024     C*
007700020506     C*    KTAB          CHAIN     TABEL00F                           51
007800020506     C* N51              MOVEA     TBLUNI        UNI
007900020506     C* N51              MOVE      UNI           LIN
008000020506     C*  51              MOVE      *BLANKS       LIN
008100020506     C*  51              MOVE      FILIA         LIN(1)
008200911024     C*
008300911024     C*---------------------------------------------------------------*
008400911024     C*
008500911024     C     KTAB          KLIST
008600911024     C                   KFLD                    CODUT
008700911024     C                   KFLD                    TBLCOD
008800911024     C                   KFLD                    TBLKEY
008900911024     C*
009000911024     C     INIZI1        TAG
009100911025     C                   SETON                                        20
009200920619     C                   Z-ADD     FILIA         VIDLNA
009300941202     C                   Z-ADD     GMAUD8        VIDDAM
009400920619     C*
009500911024     C     INIZIO        TAG
009600911115     C                   SETOFF                                       90
009700941201     C                   EXFMT     LR57D01
009800911024     C** CMD03 - FINE LAVORO
009900911024     C   KC              GOTO      FINE
010000911025     C*
010100911025     C   21
010200911025     CAN KS              SETOFF                                       20
010300911025     C   21
010400911025     CAN KS              SETON                                        90
010500920622      *
010600920622      *
010700920622     C*Per CMD18 eseguo controlli e carico £6
010800920622     C   21              DO                                                     *
010900020506     c                   clear                   trul06ds
011000020506     c                   eval      d06cod = '£1'
011100020506     c                   movel     simfel        d06key
011200020506     c                   movel     trul06ds      kpjbu
011300020506     c                   call      'TRUL06R'
011400020506     c                   parm                    kpjba
011500020506     c                   movel     kpjbu         trul06ds
011600020506     c                   movea     lint          Lmn
011700020506     C*                  MOVEL     '£1'          TBLCOD
011800020506     C*                  MOVEL     *BLANKS       TBLKEY
011900020506     C*                  MOVEL     SIMFEL        TBLKEY
012000020506     C*    KTAB          CHAIN     TABEL00F                           51
012100020506     C*  51              SETON                                        1790
012200020506     C* N51              MOVEA     TBLUNI        UNI
012300020506     C* N51              MOVE      UNI           LMN
012400920622     C*
012500920622     C* CONTROLLO SE ESISTE LA FILIALE DA ME DIGITATA
012600920622     C*
012700920622     C     VIDLNA        LOOKUP    LMN                                    70
012800920622     C  N70              SETON                                        1790
012900920622     C*
013000920622     C   70              DO
013100920622     C* RECUPERO LE LINEE DI ARRIVO DALLA TABELLA £6
013200920622     C*
013300920622     C                   MOVE      VIDLNA        FILIA
013400920622     C* AGGANCIO LA £6 E RECUPERO LE LINEE A LORO ASSOCIATE
013500920622     C*
013600020506     c                   clear                   trul06ds
013700020506     c                   eval      d06cod = '£6'
013800020506     c                   movel     filia         d06key
013900020506     c                   movel     trul06ds      kpjbu
014000020506     c                   call      'TRUL06R'
014100020506     c                   parm                    kpjba
014200020506     c                   movel     kpjbu         trul06ds
014300020506     c                   movea     lint          Lin
014400020506     C*                  MOVEL     '£6'          TBLCOD
014500020506     C*                  MOVEL     *BLANKS       TBLKEY
014600020506     C*                  MOVEL     FILIA         TBLKEY
014700920622     C*
014800020506     C*    KTAB          CHAIN     TABEL00F                           51
014900020506     C* N51              MOVEA     TBLUNI        UNI
015000020506     C* N51              MOVE      UNI           LIN
015100020506     C*  51              MOVE      *BLANKS       LIN
015200020506     C*  51              MOVE      FILIA         LIN(1)
015300020506     C                   END
015400920622     C                   END
015500920622      *
015600911025     C  N90              EXSR      CONTR
015700920622      *
015800941201     C  NKF
015900911024     COR 90              GOTO      INIZIO
016000911024     C*
016100911024     C                   SORTA     LIN
016200911024     C                   MOVEL     PARAM         KPJBU
016300941201     C                   MOVEL     'LR58'        KCOAZ
016400911024     C                   CALL      'BCH10'
016500911024     C                   PARM                    KPJBA
016600920622     C*
016700911024     C     FINE          TAG
016800911024     C                   SETON                                        LR
016900920622      *
017000920622      *
017100920622      *
017200911024     C*--- CONTROLLI FORMATO1 ----------------------------------------*
017300911024     C     CONTR         BEGSR
017400911024     C                   SETOFF                                       90
017500911024     C* CONTROLLO CHE ESISTA
017600911024     C     VIDLNA        LOOKUP    LIN                                    70
017700911024     C* PER ERRORE
017800911024     C  N70              SETON                                        9021
017900911024     C   70              MOVE      *ZEROS        LIN
018000911024     C   70              MOVE      VIDLNA        LIN(1)
018100920619      *
018200920619     C* Routine di controllo data
018300941201     C                   MOVE      VIDDAM        G08DAT
018400941202     C                   MOVE      *ZEROS        G08INV
018500941201     C                   MOVEL     *BLANK        G08ERR
018600941201     C                   CALL      'XSRDA8'
018700941201     C                   PARM                    WLBDA8
018800941201     C     G08ERR        COMP      '1'                                    10
018900911115     C   10              SETON                                        90
019000941202     C  N10              Z-ADD     G08DAT        VIDDAM
019100941202     C  N10              Z-ADD     G08INV        DATE
019200911024     C*
019300911024     C     ENDCTR        ENDSR
019400911024     C*
