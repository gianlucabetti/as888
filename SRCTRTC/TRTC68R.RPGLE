000100961219     H DECEDIT('0,') DATEDIT(*DMY.)
000200930128      ***********************************************************************
000300961219      *                      -  Pgm. TRTC68R -                              *
000400961219      *---------------------------------------------------------------------*
000500060213      *   Cancella i record dei file EDRE000F, EDSUM00F, fiar400F (TRC='E') *
000600961219      *   se la bolla è già stata confermata e cancellata                   *
000700961219     F*---------------------------------------------------------------------*
000800961219     F*  DATA BASE                                                          *
000900961219     F*---------------------------------------------------------------------*
001000950116     FFNBLP01L  IF   E           K DISK
001100961219     F*----------
001200970108     FFNARB01L  IF   E           K DISK
001300970108     F*----------
001400020920     FEDivab2L  IF   E           K DISK
001500961219     F*----------
001600020920     FFiVAB04L  IF   E           K DISK
001700961219     F*----------
001800961219     FEDRDE01L  UF   E           K DISK
001900961219     F*----------
002000961220     FEDSUM03L  UF   E           K DISK
002100961219     F*----------
002200060213     Ffiar401l  UF   E           K DISK
002300961219      *---------------------------------------------------------------------*
002400961219      *  DS
002500961219      *---------------------------------------------------------------------*
002600020920     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002700020920     D  TCU                  398    697
002800020920     D                                     DIM(50)                              Flg 8 tp.conto
002900020920     D  KCU                  698    847P 0
003000020920     D                                     DIM(50)                              Capiconto
003100020920     D                                     PACKEVEN
003200020920     D CNCR80        E DS
003300961219      *---------------------------------------------------------------------*
003400961220     C* Posizionamento su prima spedizione
003500961220     C                   EXSR      SETFIL
003600930128     C*
003700961219     C* Ciclo fino a fine file
003800961220     C     WFINE         DOWEQ     'N'                                          --- 1 -->
003900961219     C* Controlla se la bolla è già stata confermata e cancellata
004000931006     C                   EXSR      CHKREC
004100961219     C* Se si esegue pulizia archivi EDI
004200931006     C                   EXSR      CANCEL
004300930128     C                   END                                                    <-- 1 ---
004400060213     C* Richiamo pgm x cancellare eventuali ar4 rimasti
004500961220     C                   CALL      'TRTC68R1'
004600961219     C* Fine pgm
004700000000     C                   SETON                                        LR
004800961220     C*----------------------------------------------------------------
004900961220     C*? SETFIL - Agganciamento successiva spedizione
005000961220     C*----------------------------------------------------------------
005100961220     C     SETFIL        BEGSR
005200961220     C*
005300961220     C* Setgt x agganciare successiva spedizione
005400961220     C     *LOVAL        SETLL     EDRDE01L
005500961220     C                   READ      EDRDE01L                               30
005600961220     C     *IN30         IFEQ      '1'
005700961220     C                   MOVEL     'S'           WFINE
005800931006     C                   END
005900930208     C*
006000930208     C                   ENDSR
006100931006     C*----------------------------------------------------------------
006200961220     C*? CHKREC - Controlla se il record è da cancellare
006300931006     C*----------------------------------------------------------------
006400931006     C     CHKREC        BEGSR
006500931006     C*
006600961220     C                   MOVEL     'N'           WCANC             1
006700961220     C* Controllo se record su BLP
006800961220     C                   Z-ADD     RDEAAS        KAAS
006900961220     C                   Z-ADD     RDELNP        KLNP
007000961220     C                   Z-ADD     RDENRS        KNRS
007100961220     C                   Z-ADD     RDENSP        KNSP
007200961220     C     KBLP          CHAIN     FNBLP01L                           31
007300961220     C* ... se la spedizione non è presente su BLP verifico se
007400961220     C*     è ancora da confermare.
007500020920     C   31KVAB          CHAIN     FiVAB04L                           31
007600020920     C   31KVAB          CHAIN     EDivab2L                           31
007700970108     C   31KARB          CHAIN     FNARB01L                           31
007800961220     C* se la bolla è gia stata confermata cancello archivi EDI
007900961220     C     *IN31         IFEQ      '1'
008000931006     C                   MOVEL     'S'           WCANC             1
008100931006     C                   END
008200931006     C*
008300931006     C                   ENDSR
008400930212     C*----------------------------------------------------------------
008500961220     C*? CANCEL - CANCELLA IL RECORD SU EDRDE00F
008600930212     C*----------------------------------------------------------------
008700931006     C     CANCEL        BEGSR
008800930222     C*
008900961220     C* Cancello archivi EDI
009000931006     C     WCANC         IFEQ      'S'
009100961220     C* Imposto nr. sepdizione in campi di appoggio
009200961220     C                   Z-ADD     RDEAAS        WSVAAS
009300961220     C                   Z-ADD     RDELNP        WSVLNP
009400961220     C                   Z-ADD     RDENRS        WSVNRS
009500961220     C                   Z-ADD     RDENSP        WSVNSP
009600961220     C* EDRDE: loop di delete fino a nuovo nr. spedizione
009700961220     C     *IN30         DOWEQ     '0'
009800961220     C     WSVNSP        ANDEQ     RDENSP
009900961220     C     WSVNRS        ANDEQ     RDENRS
010000961220     C     WSVLNP        ANDEQ     RDELNP
010100961220     C     WSVAAS        ANDEQ     RDEAAS
010200961220     C     *IN30         ANDEQ     '0'
010300970115     C                   DELETE    EDRDE000
010400961220     C                   READ      EDRDE01L                               30
010500931006     C                   END
010600060213     C* FNar4: cancello nr.spedizione partner
010700961220     C                   MOVEL     'E'           KTRC
010800060213     C     Kar4          CHAIN     fiar401l                           31
010900060213     C  N31              DELETE    fiar4000
011000961220     C* EDSUM: cancello errori qudratura arrivo
011100961220     C     KSUM          CHAIN     EDSUM03L                           31
011200970115     C  N31              DELETE    EDSUM000
011300961220     C* Se il record non è da cancellare leggo successivo
011400961220     C                   ELSE
011500961220     C                   READ      EDRDE01L                               30
011600961220     C                   END
011700961220     C* Controllo se sono a fine file
011800961220     C     *IN30         IFEQ      '1'
011900961220     C                   MOVEL     'S'           WFINE
012000961220     C                   END
012100930212     C*
012200930212     C                   ENDSR
012300000000     C*----------------------------------------------------------------
012400961219     C*? *INZSR: OPERAZIONI INIZIALI
012500000000     C*----------------------------------------------------------------
012600961219     C     *INZSR        BEGSR
012700930129     C*
012800020920     C*  Richiamo XPARUT
012900020920     C                   Z-ADD     1             CODUT
013000020920     C                   CALL      'X§PARUT'
013100020920     C                   PARM                    UT§DSE
013200020920     C                   MOVEL     REC80         CNCR80
013300020920      *
013400961219     C* Definisco chiavi di accesso
013500961219     C     KBLP          KLIST
013600961219     C                   KFLD                    KAAS
013700961219     C                   KFLD                    KLNP
013800961219     C                   KFLD                    KNRS
013900961219     C                   KFLD                    KNSP
014000970108     C     KARB          KLIST
014100970108     C                   KFLD                    KAAS
014200970108     C                   KFLD                    KLNP
014300970108     C                   KFLD                    KNRS
014400970108     C                   KFLD                    KNSP
014500961219     C     KVAB          KLIST
014600961219     C                   KFLD                    KAAS
014700961219     C                   KFLD                    KLNP
014800961219     C                   KFLD                    KNRS
014900961219     C                   KFLD                    KNSP
015000060213     C     Kar4          KLIST
015100961220     C                   KFLD                    KAAS
015200961220     C                   KFLD                    KLNP
015300961220     C                   KFLD                    KNRS
015400961220     C                   KFLD                    KNSP
015500961220     C                   KFLD                    KTRC
015600961220     C     KSUM          KLIST
015700961220     C                   KFLD                    KAAS
015800961220     C                   KFLD                    KLNP
015900961220     C                   KFLD                    KNRS
016000961220     C                   KFLD                    KNSP
016100961219     C*
016200961219     C* Definisco variabili
016300961219     C     *LIKE         DEFINE    BLPAAS        KAAS
016400961219     C     *LIKE         DEFINE    BLPLNP        KLNP
016500961219     C     *LIKE         DEFINE    BLPNRS        KNRS
016600961219     C     *LIKE         DEFINE    BLPNSP        KNSP
016700060213     C     *LIKE         DEFINE    ar4TRC        KTRC
016800961220     C     *LIKE         DEFINE    RDEAAS        WSVAAS
016900961220     C     *LIKE         DEFINE    RDELNP        WSVLNP
017000961220     C     *LIKE         DEFINE    RDENRS        WSVNRS
017100961220     C     *LIKE         DEFINE    RDENSP        WSVNSP
017200930208     C*
017300961219     C* Imposto a zero il fine pgm
017400961219     C                   MOVEL     'N'           WFINE             1
017500000000     C*
017600000000     C                   ENDSR
