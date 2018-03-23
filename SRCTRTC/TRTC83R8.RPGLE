000100000405     H DECEDIT('0,') DATEDIT(*DMY.)
000200020228      * TRTC83R8  *----------------------------------------------------*
000300020227      *     Per trasmissione Bolle singole a Fedex
000400000405      *--------------------------------------------------------------*
000500020227     FTRTC83R8D CF   E             WORKSTN
000600000405      *---------
000700020227     FFNARB01L  IF   E           K DISK
000800020227     FazORG01L  IF   E           K DISK
000900000405      *--------------------------------------------------------------*
001000000405      *  SCHIERE                                                     *
001100000405      *--------------------------------------------------------------*
001200000405      *
001300000405      *--------------------------------------------------------------*
001400000405      *  DS                                                          *
001500000405      *--------------------------------------------------------------*
001600020227     D Og143         E DS
001700020227     D UT§DSE0F      E DS
001800020227     D  TCU                  398    697    DIM(50)                              PARAM FLG7/8,TIPC
001900020227     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    PARAM CAPI CONTO
002000020227     D TCUDS           DS
002100020227     D  F3                     3      3
002200020227     D  F4                     4      4
002300020227     D  F56                    5      6
002400020227     D CNCR80        E DS
002500000405      * PASSAGGIO DATI ALL'ELABORAZIONE BATCH             - TRTC83C -
002600020227     D TRTC831       E DS                  EXTNAME(TRTC83DS1)
002700000405      *
002800900511     D KPJBA         E DS
002900960701     D WLBDA8          DS
003000960701     D  G02DAT                 1      8  0
003100960701     D  G02INV                 9     16  0
003200960701     D  G02ERR                17     17
003300960701     D  G02TGI                18     22  0
003400000405
003500000405      *-------------------------------------------------------------*
003600000405      *  CICLO PRINCIPALE                                           *
003700000405      *-------------------------------------------------------------*
003800000405      *  Inizializzazione parametri a video
003900960529     C                   EXSR      INZ01
004000000405      *  Loop gestione prima videata
004100960529     C     WFINE         DOWNE     'S'
004200960530     C     WTPVID        CASEQ     '1'           GESD01
004300960529     C                   END
004400960530     C                   END
004500000405      *
004600960529     C     FINE          TAG
004700000405     C                   eval       *inlr = *on
004800960529     C***************************************************************
004900960529     C*  Inizializzazione prima videata                             *
005000960529     C***************************************************************
005100960529     C     INZ01         BEGSR
005200960529     C*
005300020227     C                   CLEAR                   d1aas
005400020227     C                   CLEAR                   d1lnp
005500020227     C                   CLEAR                   d1nrs
005600020227     C                   CLEAR                   d1nsp
005700020228     C                   CLEAR                   d1dfv
005800020227     C                   MOVEL     '1'           WTPVID            1
005900960529     C*
006000960529     C                   ENDSR
006100960529     C***************************************************************
006200960529     C*  Gestione prima videata                                     *
006300960529     C***************************************************************
006400960529     C     GESD01        BEGSR
006500970108     C*
006600020227     c                   setoff                                       99
006700020227     C                   EXFMT     TC83D01
006800960529     C** Fine Lavoro
006900960529     C     *INKC         IFEQ      '1'
007000960529     C                   MOVEL     'S'           WFINE             1
007100960529     C                   GOTO      FINVD1
007200960529     C                   END
007300960529     C* Controlli formato
007400960529     C                   EXSR      CONTR1
007500020227     C   99              GOTO      FINVD1
007600020227     C*
007700020227     c   KF              EXSR      ESEGUE
007800960529     C*
007900960529     C     FINVD1        ENDSR
008000960529     C***************************************************************
008100960530     C*  Controlli prima videata                                    *
008200960529     C***************************************************************
008300960529     C     CONTR1        BEGSR
008400960529     C*
008500020227     c     karb          chain     FNarb01l                           21
008600020227     C* Per non trovata bolla --> errore
008700020227     c   21              seton                                        99
008800020227     c   21              goto      endctr
008900960530     C*
009000020227     C*  Se trovata deve essere una Bolla per la Fedex
009100020227     c     arblna        chain     AzORG01l                           22
009200020227     c   22              seton                                        99
009300020227     c   22              goto      endctr
009400020227     c                   movel     orgde3        og143
009500020227     c                   if        §ogntw <> 'FED'
009600020227     c                   seton                                        9922
009700020227     c   22              goto      endctr
009800020227     c                   end
009900020227     C*
010000020228     C*  Bolla già trasmessa
010100020228     c                   if        arbft3='T' or arbdt3 >0
010200020228     c                   seton                                        9924
010300020228     c                   goto      endctr
010400020228     c                   end
010500020228     C*
010600020228     C*  Data Trasmissione (data foglio viaggio errata o mancante)
010700020228     C*
010800020228     C                   z-add     d1dfv         G02DAT
010900020228     C                   movel     *BLANK        G02ERR
011000020228     C                   CALL      'XSRDA8'
011100020228     C                   PARM                    WLBDA8
011200020228     C                   z-add     g02dat        d1dfv
011300020228     C                   z-add     g02inv        h1dfv             8 0
011400020228     C* ERRATA
011500020228    1C     G02ERR        IFEQ      '1'
011600020228     C                   SETON                                        2599
011700020228     c                   goto      endctr
011800020228     C                   end
011900020228     C*
012000960530     C     ENDCTR        ENDSR
012100020227     C***************************************************************
012200020227     C*  Esegue                                                     *
012300020227     C***************************************************************
012400020227     C     ESEGUE        BEGSR
012500020227     C*
012600020227     c                   clear                   TRTC831
012700020227     c                   eval      f83aas = d1aas
012800020227     c                   eval      f83lnp = d1lnp
012900020227     c                   eval      f83nrs = d1nrs
013000020227     c                   eval      f83nsp = d1nsp
013100020228     c                   eval      f83dfv = h1dfv
013200020227     c                   movel     TRTC831       KpJBU
013300020227     c                   call      'TRTC83R21'
013400020227     c                   parm                    KpJBA
013500020227     C*
013600020228     C* per chiuderlo il LR deve richiamarlo in quanto il programma
013700020228     C*  chiamato è chiuso in RT o LR
013800020227     c                   eval      f83aas = 0
013900020227     c                   eval      f83lnp = 0
014000020227     c                   eval      f83nrs = 0
014100020227     c                   eval      f83nsp = 0
014200020228     c                   eval      f83dfv = 0
014300020227     c                   movel     TRTC831       KpJBU
014400020227     c                   call      'TRTC83R21'
014500020227     c                   parm                    KpJBA
014600020227     C*
014700020227     C                   CLEAR                   d1aas
014800020227     C                   CLEAR                   d1lnp
014900020227     C                   CLEAR                   d1nrs
015000020227     C                   CLEAR                   d1nsp
015100020228     C                   CLEAR                   d1dfv
015200020227     C*
015300020227     c                   commit
015400020227     C*
015500020227     C                   ENDSR
015600000405      *---------------------------------------------------------------*
015700000405      * *INZSR: Operazioni iniziali                                  -*
015800000405      *---------------------------------------------------------------*
015900960529     C     *INZSR        BEGSR
016000960529     C*
016100960529     C     *ENTRY        PLIST
016200960529     C                   PARM                    KPJBA
016300960529     C* Richiamo XPARUT
016400960529     C                   Z-ADD     1             CODUT
016500960529     C                   CALL      'X§PARUT'
016600000405     C                   PARM                    UT§DSE0F
016700960529     C                   MOVEL     REC80         CNCR80
016800020227     C                   MOVEL     RAGUT         RSUT
016900020227     C*
017000961129     C*--- RICERCA CAPOCONTI------------------------------------------*
017100020227     C                   DO        50            X                 3 0
017200961129     C                   MOVE      TCU(X)        TCUDS
017300961129     C     F56           IFEQ      'CG'
017400961129     C     F4            ANDEQ     '1'
017500961129     C     F3            ANDEQ     '0'
017600961129     C                   Z-ADD     KCU(X)        KCI               4 0
017700961129     C                   END
017800961129     C                   END
017900960529     C*
018000020227     C     Karb          KLIST
018100020227     C                   KFLD                    D1aas
018200020227     C                   KFLD                    D1lnp
018300020227     C                   KFLD                    D1nrs
018400020227     C                   KFLD                    D1nsp
018500961121     C*
018600961121     C* REPERISCO DATA DEL SISTEMA
018700961121     C                   TIME                    WTIME            14 0          ORA E DATA
018800961121     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
018900010611     C                   MOVE      Wdate         anno              4 0          AAAA
019000010611     C     anno          sub       1             daanno            4 0          AAAA
019100010611     C     anno          add       1             adanno            4 0          AAAA
019200961121     C                   Z-ADD     WDATE         G02DAT
019300961121     C                   Z-ADD     *ZERO         G02INV
019400961121     C                   MOVEL     *BLANKS       G02ERR
019500961121     C                   CALL      'XSRDA8'
019600961121     C                   PARM                    WLBDA8
019700961121     C                   Z-ADD     G02INV        DATEU             8 0          AAAA/MM/GG
019800960529     C*
019900960529     C                   ENDSR
