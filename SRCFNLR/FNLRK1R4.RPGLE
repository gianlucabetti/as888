000100961119     H DECEDIT('0,') DATEDIT(*DMY.)
000200980410      *--------------------------------------------------------------*
000300980410      *         - GESTIONE DISTINTA ARRIVI ESTERO -                  *
000400980410      *           - VISUALIZZAZIONE NOTE  E.D.I. -                   *
000500961119      *--------------------------------------------------------------*
000600980410     FFNLRK1D4  CF   E             WORKSTN
000700980410     F                                     SFILE(LRK1S01:NRR)
000800980410     FEDRDA01L  IF   E           K DISK
000900980410      *--------------------------------------------------------------*
001000961119     D KPJBA         E DS
001100980410     D FNLRK1        E DS                  EXTNAME(FNLRK1DS)
001200020913     D Ddatiute      E DS
001300980410      *--------------------------------------------------------------*
001400980410      *  C I C L O        P R I N C I P A L E
001500980410      *--------------------------------------------------------------*
001600961119     C     *ENTRY        PLIST
001700961119     C                   PARM                    KPJBA
001800980410     C                   MOVEL     KPJBU         FNLRK1
001900980410      *
002000980410      * Pulisco subfile
002100961119     C                   SETOFF                                       2021
002200980410     C                   WRITE     LRK1C01
002300961119     C                   SETON                                        2021
002400980416      *
002500980410     C                   Z-ADD     DK1AAS        KAAS
002600980410     C                   Z-ADD     DK1LNP        KLNP
002700980410     C                   Z-ADD     DK1NRS        KNRS
002800980410     C                   Z-ADD     DK1NSP        KNSP
002900980410      *
003000980410      * CONTROLLO SE CI SONO NOTE
003100961121     C                   MOVEL     'DD4440'      KNMC
003200961120     C                   Z-ADD     1             NRR               4 0
003300980410     C     KRDA          CHAIN     EDRDA01L                           30
003400980410      *
003500961119     C     *IN30         IFEQ      '0'
003600980410     C                   Z-ADD     DK1AAS        V1CAAS
003700980410     C                   Z-ADD     DK1LNP        V1CLNP
003800980410     C                   Z-ADD     DK1NRS        V1CNRS
003900980410     C                   Z-ADD     DK1NSP        V1CNSP
004000980410     C                   MOVEL     'S'           DK1TRN
004100961119     C     *IN30         DOWEQ     '0'
004200961119     C                   ADD       1             NRR
004300980410     C                   MOVEL     RDAVAL        V1CNOT
004400980410     C                   WRITE     LRK1S01
004500980410     C     KRDA          READE     EDRDA01L                               30
004600961119     C                   END
004700980410      *
004800980410     C                   WRITE     LRK1D02
004900980410     C                   WRITE     LRK1Z01
005000980410     C                   EXFMT     LRK1C01
005100961119     C                   END
005200980410      *
005300961217     C     FINE          TAG
005400961121     C                   SETON                                        LR
005500980410      *--------------------------------------------------------------*
005600980410     C     *INZSR        BEGSR
005700980410      *
005800980410     C     KRDA          KLIST
005900980410     C                   KFLD                    KAAS
006000980410     C                   KFLD                    KLNP
006100980410     C                   KFLD                    KNRS
006200980410     C                   KFLD                    KNSP
006300980410     C                   KFLD                    KNMC
006400980410      *
006500980410     C     *LIKE         DEFINE    RDAAAS        KAAS
006600980410     C     *LIKE         DEFINE    RDALNP        KLNP
006700980410     C     *LIKE         DEFINE    RDANRS        KNRS
006800980410     C     *LIKE         DEFINE    RDANSP        KNSP
006900980410     C     *LIKE         DEFINE    RDANMC        KNMC
007000020913      *
007100020913      * Decodifica Elaboratore
007200020913      *  TIBS34R richiamato dal FNLRK1R ha impostato i dati
007300020913      *   che servono anche in questo programma.
007400020913     c     *Dtaara       Define    §datiute      DDatiUte
007500020913     c                   In        *Dtaara
007600980410      *
007700980410     C                   ENDSR
