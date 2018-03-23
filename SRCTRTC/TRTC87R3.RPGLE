000100961009     H DECEDIT('0,') DATEDIT(*DMY.)
000200961009     H*---------------------------------------------------------------------*
000300961009     H*  Ritorno date arrivo ai partner  - E.D.I. - Estero -                *
000400961009     F*---------------------------------------------------------------------*
000500961009     F*  DATA BASE                                                          *
000600961009     F*---------------------------------------------------------------------*
000700961009     FEDSUM01L  UF   E           K DISK
000800961009     F*---------
000900980326     FEDSUM04L  UF   E           K DISK
001000980326     F                                     RENAME(EDSUM000:EDSUM004)
001100980326     F*---------
001200961009     FEDBRV01L  UF   E           K DISK
001300980326     F*---------
001400980326     FEDBRV04L  UF   E           K DISK
001500980326     F                                     RENAME(EDBRV000:EDBRV004)
001600961009     F*---------
001700961010     FTRTC87D   CF   E             WORKSTN
001800961010     F                                     SFILE(TC87S03:NRR3)
001900961009     D*---------------------------------------------------------------------*
002000961009     D* DS
002100961009     D*---------------------------------------------------------------------*
002200961009     D KPJBA         E DS
002300961009     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002400961009     D CNCR80        E DS
002500961009     D TRTC87        E DS                  EXTNAME(TRTC87DS)
002600961009     C*---------------------------------------------------------------------*
002700961009     C* Ciclo principale
002800961009     C*---------------------------------------------------------------------*
002900961009      *  Scrivo errori da EDSUM - EDBRV in file Work
003000961010     C                   Z-ADD     1             NRR3              4 0
003100961009     C                   READC     TC87S03                                29
003200961010     C     *IN29         DOWEQ     '0'                                          Dow READC
003300961009      *
003400961009      *  Dati da EDBRV
003500961010     C     V3CLAB        IFNE      *ZEROS                                       Dati BRV
003600961009     C     V3CNPS        ORNE      *ZEROS
003700961009      *  Aggiorno BRV solo se ho dato il record per annullato
003800961025     C     V3CFGB        IFEQ      'A'                                          BRV annull.
003900961009     C                   Z-ADD     D87CCM        KCCM
004000961009     C                   Z-ADD     D87NFE        KNFE
004100961009     C                   Z-ADD     V3CAAS        KAAS
004200961029     C                   Z-ADD     V3CFLS        KFLS
004300961029     C                   Z-ADD     V3CLNP        KLNP
004400961009     C                   Z-ADD     V3CNRS        KNRS
004500961009     C                   Z-ADD     V3CNSP        KNSP
004600961009     C                   Z-ADD     V3CNSC        KNSC
004700961009     C                   MOVEL     V3CERB        KSTS
004800980326     C     D87LNP        IFEQ      0
004900961009     C     KBRV          CHAIN     EDBRV01L                           31
005000961015     C*  Se record di eccedenza cancello proprio la bolla
005100961111     C  N31V3CERB        IFEQ      '002'
005200961015     C                   DELETE    EDBRV000
005300961015     C                   ELSE
005400961009     C                   MOVEL     '*'           BRVFLG
005500961015     C                   EXCEPT    ANNBRV
005600961015     C                   END
005700980326     C*
005800980326     C                   ELSE
005900980326     C     KBRV1         CHAIN     EDBRV04L                           31
006000980326     C*  Se record di eccedenza cancello proprio la bolla
006100980326     C  N31V3CERB        IFEQ      '002'
006200980326     C                   DELETE    EDBRV004
006300980326     C                   ELSE
006400980326     C                   MOVEL     '*'           BRVFLG
006500980326     C                   EXCEPT    ANBRV4
006600980326     C                   END
006700980326     C*
006800980326     C                   END                                                    D87LNP=0
006900980326     C*
007000961010     C                   END                                                    BRV annull
007100961010     C                   END                                                    Dati BRV
007200961009      *
007300961009      *  Dati da EDSUM
007400961010     C     V3CLAS        IFNE      *ZEROS                                       Dati SUM
007500961025     C     V3CFGS        ANDNE     'E'
007600961009      *  Aggiorno SUM se non ho escluso record --> su altro
007700961009      *  foglio
007800961011     C                   MOVEL     *BLANKS       KFLG
007900961009     C                   Z-ADD     D87CCM        KCCM
008000961009     C                   Z-ADD     0             KNFE1
008100961009     C                   Z-ADD     V3CAAS        KAAS
008200961029     C                   Z-ADD     V3CFLS        KFLS
008300961009     C                   Z-ADD     V3CLNP        KLNP
008400961009     C                   Z-ADD     V3CNRS        KNRS
008500961009     C                   Z-ADD     V3CNSP        KNSP
008600961009     C                   Z-ADD     V3CNSC        KNSC
008700961009     C                   MOVEL     V3CERS        KSTS
008800980326     C     D87LNP        IFEQ      0
008900961009     C     KSUM          CHAIN     EDSUM01L                           31
009000961025     C     V3CFGS        IFEQ      'A'                                          SUM annull.
009100961011     C     SUMFLG        ORNE      ' '                                          SUM annull.
009200961009     C                   DELETE    EDSUM000
009300961010     C                   ELSE                                                   SUM annull
009400961009     C                   Z-ADD     D87NFE        SUMNFE
009500961009     C                   MOVEL     ' '           SUMFLG
009600961009     C                   EXCEPT    AGGSUM
009700961010     C                   END                                                    SUM annull
009800980326     C*
009900980326     C                   ELSE
010000980326     C     KSUM1         CHAIN     EDSUM04L                           31
010100980326     C     V3CFGS        IFEQ      'A'                                          SUM annull.
010200980326     C     SUMFLG        ORNE      ' '                                          SUM annull.
010300980326     C                   DELETE    EDSUM004
010400980326     C                   ELSE                                                   SUM annull
010500980326     C                   Z-ADD     D87NFE        SUMNFE
010600980326     C                   MOVEL     ' '           SUMFLG
010700980326     C                   EXCEPT    AGSUM4
010800980326     C                   END                                                    SUM annull
010900980326     C*
011000980326     C                   END
011100980326     C*
011200961010     C                   END                                                    Dati SUM
011300961009      *
011400961009      *  Leggo record successivo
011500961009     C                   READC     TC87S03                                29
011600961010     C                   END                                                    Dow READC
011700961009      *
011800961009      *  Fine lavoro
011900961009     C                   SETON                                        LR
012000961009     C*------------------------------------------------------------*
012100961009     C* OPERAZIONI INIZIALI
012200961009     C*------------------------------------------------------------*
012300961009     C     *INZSR        BEGSR
012400961009     C*
012500961009     C     *ENTRY        PLIST
012600961009     C                   PARM                    KPJBA
012700961009     C                   MOVEL     KPJBU         TRTC87
012800961009     C*  Definizione chiavi
012900961009     C     KBRV          KLIST
013000961009     C                   KFLD                    KCCM
013100961009     C                   KFLD                    KNFE
013200961029     C                   KFLD                    KFLS
013300961009     C                   KFLD                    KNRS
013400961015     C                   KFLD                    KNSC
013500961015     C                   KFLD                    KAAS
013600961029     C                   KFLD                    KLNP
013700961015     C                   KFLD                    KNSP
013800980402     C***                  KFLD           KSTS
013900961009     C     KSUM          KLIST
014000961009     C                   KFLD                    KCCM
014100961009     C                   KFLD                    KNFE1
014200961011     C                   KFLD                    KFLG
014300961029     C                   KFLD                    KFLS
014400961009     C                   KFLD                    KNRS
014500961015     C                   KFLD                    KNSC
014600961015     C                   KFLD                    KAAS
014700961029     C                   KFLD                    KLNP
014800961009     C                   KFLD                    KNSP
014900980402     C                   KFLD                    KSTS
015000980326     C     KBRV1         KLIST
015100980326     C                   KFLD                    KLNP
015200980326     C                   KFLD                    KNFE
015300980326     C                   KFLD                    KFLS
015400980326     C                   KFLD                    KNRS
015500980326     C                   KFLD                    KNSC
015600980326     C                   KFLD                    KAAS
015700980326     C                   KFLD                    KNSP
015800980402     C***                  KFLD           KSTS
015900980326     C     KSUM1         KLIST
016000980326     C                   KFLD                    KLNP
016100980326     C                   KFLD                    KNFE1
016200980326     C                   KFLD                    KFLG
016300980326     C                   KFLD                    KFLS
016400980326     C                   KFLD                    KNRS
016500980326     C                   KFLD                    KNSC
016600980326     C                   KFLD                    KAAS
016700980326     C                   KFLD                    KNSP
016800961009     C*  Definizione variabili
016900961009     C     *LIKE         DEFINE    BRVCCM        KCCM
017000961009     C     *LIKE         DEFINE    BRVNFE        KNFE
017100961009     C     *LIKE         DEFINE    BRVAAS        KAAS
017200961009     C     *LIKE         DEFINE    BRVLNP        KLNP
017300961029     C     *LIKE         DEFINE    BRVFLS        KFLS
017400961009     C     *LIKE         DEFINE    BRVNRS        KNRS
017500961009     C     *LIKE         DEFINE    BRVNSP        KNSP
017600961009     C     *LIKE         DEFINE    BRVNSC        KNSC
017700961009     C     *LIKE         DEFINE    BRVSTS        KSTS
017800961009     C     *LIKE         DEFINE    SUMNFE        KNFE1
017900961011     C     *LIKE         DEFINE    SUMFLG        KFLG
018000961010     C*
018100961010     C                   SETON                                        01
018200961010     C   01
018300961010     CANN01              EXFMT     TC87C03
018400961009     C*
018500961009     C                   ENDSR
018600961009     O*-------------------------------------------------------------*
018700961015     OEDBRV000  E            ANNBRV
018800961009     O                       BRVFLG
018900961009     OEDSUM000  E            AGGSUM
019000961009     O                       SUMNFE
019100961031     O                       SUMFLG
019200980326     OEDBRV004  E            ANBRV4
019300980326     O                       BRVFLG
019400980326     OEDSUM004  E            AGSUM4
019500980326     O                       SUMNFE
019600980326     O                       SUMFLG
