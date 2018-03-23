000100961219     H DECEDIT('0,') DATEDIT(*DMY.)
000200930128      ***********************************************************************
000300961220      *                      -  Pgm. TRTC68R1-                              *
000400961219      *---------------------------------------------------------------------*
000500060213      *   Cancella i record dei file fiar400F (TRC='E')                     *
000600961219     F*---------------------------------------------------------------------*
000700961219     F*  DATA BASE                                                          *
000800961219     F*---------------------------------------------------------------------*
000900950116     FFNBLP01L  IF   E           K DISK
001000961219     F*----------
001100020920     FEDivab2L  IF   E           K DISK
001200961219     F*----------
001300020920     FFiVAB04L  IF   E           K DISK
001400961219     F*----------
001500060213     Ffiar403L  UF   E           K DISK
001600961219      *---------------------------------------------------------------------*
001700961219      *  DS
001800961219      *---------------------------------------------------------------------*
001900020920     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002000020920     D  TCU                  398    697
002100020920     D                                     DIM(50)                              Flg 8 tp.conto
002200020920     D  KCU                  698    847P 0
002300020920     D                                     DIM(50)                              Capiconto
002400020920     D                                     PACKEVEN
002500020920     D CNCR80        E DS
002600961219      *---------------------------------------------------------------------*
002700961220     C* Posizionamento su prima spedizione
002800961220     C                   EXSR      SETFIL
002900930128     C*
003000961219     C* Ciclo fino a fine file
003100961220     C     WFINE         DOWEQ     'N'                                          --- 1 -->
003200961219     C* Controlla se la bolla è già stata confermata e cancellata
003300931006     C                   EXSR      CHKREC
003400961219     C* Se si esegue pulizia archivi EDI
003500961219     C     WCANC         IFEQ      'S'
003600931006     C                   EXSR      CANCEL
003700961219     C                   END
003800961220     C* Lettura successiva
003900961220     C                   EXSR      REDFIL
004000930128     C                   END                                                    <-- 1 ---
004100961219     C* Fine pgm
004200000000     C                   SETON                                        LR
004300961220     C*----------------------------------------------------------------
004400961220     C*? SETFIL - Agganciamento successiva spedizione
004500961220     C*----------------------------------------------------------------
004600961220     C     SETFIL        BEGSR
004700961220     C*
004800961220     C* Setgt x agganciare successiva spedizione
004900961220     C                   MOVEL     'E'           KTRC
005000060213     C     KTRC          CHAIN     fiar403L                           30
005100961220     C     *IN30         IFEQ      '1'
005200961220     C                   MOVEL     'S'           WFINE
005300931006     C                   END
005400930208     C*
005500930208     C                   ENDSR
005600961220     C*----------------------------------------------------------------
005700961220     C*? REDFIL - Agganciamento successiva spedizione
005800961220     C*----------------------------------------------------------------
005900961220     C     REDFIL        BEGSR
006000961220     C*
006100961220     C* LETTURA SUCCESSIVA
006200961220     C                   MOVEL     'E'           KTRC
006300060213     C     KTRC          READE     fiar403L                               30
006400961220     C     *IN30         IFEQ      '1'
006500961220     C                   MOVEL     'S'           WFINE
006600961220     C                   END
006700961220     C*
006800961220     C                   ENDSR
006900931006     C*----------------------------------------------------------------
007000961220     C*? CHKREC - Controlla se il record è da cancellare
007100931006     C*----------------------------------------------------------------
007200931006     C     CHKREC        BEGSR
007300931006     C*
007400961220     C                   MOVEL     'N'           WCANC             1
007500961220     C* Controllo se record su BLP
007600060213     C                   Z-ADD     ar4AAS        KAAS
007700060213     C                   Z-ADD     ar4LNP        KLNP
007800060213     C                   Z-ADD     ar4NRS        KNRS
007900060213     C                   Z-ADD     ar4NSP        KNSP
008000961220     C     KBLP          CHAIN     FNBLP01L                           31
008100961220     C* ... se la spedizione non è presente su BLP verifico se
008200961220     C*     è ancora da confermare.
008300020920     C   31KVAB          CHAIN     FiVAB04L                           31
008400020920     C   31KVAB          CHAIN     EDivab2L                           31
008500961220     C* se la bolla è gia stata confermata cancello archivi EDI
008600961220     C     *IN31         IFEQ      '1'
008700931006     C                   MOVEL     'S'           WCANC             1
008800931006     C                   END
008900931006     C*
009000931006     C                   ENDSR
009100930212     C*----------------------------------------------------------------
009200060213     C*? CANCEL - CANCELLA IL RECORD SU fiar403L
009300930212     C*----------------------------------------------------------------
009400931006     C     CANCEL        BEGSR
009500930222     C*
009600060213     C* Cancello FIAR4
009700931006     C     WCANC         IFEQ      'S'
009800060213     C                   DELETE    fiar4000
009900961220     C                   END
010000930212     C*
010100930212     C                   ENDSR
010200000000     C*----------------------------------------------------------------
010300961219     C*? *INZSR: OPERAZIONI INIZIALI
010400000000     C*----------------------------------------------------------------
010500961219     C     *INZSR        BEGSR
010600930129     C*
010700020920     C*  Richiamo XPARUT
010800020920     C                   Z-ADD     1             CODUT
010900020920     C                   CALL      'X§PARUT'
011000020920     C                   PARM                    UT§DSE
011100020920     C                   MOVEL     REC80         CNCR80
011200020920      *
011300961219     C* Definisco chiavi di accesso
011400961219     C     KBLP          KLIST
011500961219     C                   KFLD                    KAAS
011600961219     C                   KFLD                    KLNP
011700961219     C                   KFLD                    KNRS
011800961219     C                   KFLD                    KNSP
011900961219     C     KVAB          KLIST
012000961219     C                   KFLD                    KAAS
012100961219     C                   KFLD                    KLNP
012200961219     C                   KFLD                    KNRS
012300961219     C                   KFLD                    KNSP
012400961219     C*
012500961219     C* Definisco variabili
012600961219     C     *LIKE         DEFINE    BLPAAS        KAAS
012700961219     C     *LIKE         DEFINE    BLPLNP        KLNP
012800961219     C     *LIKE         DEFINE    BLPNRS        KNRS
012900961219     C     *LIKE         DEFINE    BLPNSP        KNSP
013000060213     C     *LIKE         DEFINE    ar4TRC        KTRC
013100930208     C*
013200961219     C* Imposto a zero il fine pgm
013300961219     C                   MOVEL     'N'           WFINE             1
013400000000     C*
013500000000     C                   ENDSR
