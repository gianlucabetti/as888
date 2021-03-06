000100980203     H DECEDIT('0,') DATEDIT(*DMY.)
000200980203     H* FNLSI1R5*----------------------------------------------------*
000300980203     H*                                                              *
000400980203     H*       - CHIUSURA BOLLA IN PARTENZA CON PRATICA IDD      -    *
000500980203     H*--------------------------------------------------------------*
000600980203     FEDTAB01L  IF   E           K DISK
000700980203     F*--------
000800980203     FFNLSI1D5  CF   E             WORKSTN
000900980203     F                                     SFILE(LSI1S01:NRR1)
001000980203     D*--------------------------------------------------------------*
001100980203     D* DS
001200980203     D*--------------------------------------------------------------*
001300980203     D FNLSI1        E DS                  EXTNAME(FNLSI1DS)
001400980203     D UTEDSE        E DS                  EXTNAME(UT�DSE0F)
001500980203     D CNCR80        E DS
001600980203     D KPJBA         E DS
001700980203     D DSEM          E DS                  EXTNAME(EDIDSEM)
001800980203     D                SDS
001900980203     D  NOMPGM                 1     10
002000980203     C*--------------------------------------------------------------*
002100980203     C*  C I C L O     P R I N C I P A L E
002200980203     C*--------------------------------------------------------------*
002300980203     C     *ENTRY        PLIST
002400980203     C                   PARM                    KPJBA
002500980203     C                   MOVEL     KPJBU         FNLSI1
002600980203     C*  Inizializzo varibili videata
002700980203     C                   EXSR      INZ01
002800980203     C*  Gestione della prima videata
002900980203     C     WFINE         DOWEQ     'N'
003000980203     C     WTPVID        CASEQ     '1S'          GESS01
003100980203     C     WTPVID        CASEQ     '1D'          GESD01
003200980203     C                   END
003300980203     C                   END
003400980203     C*  Fine pgm
003500980203     C                   MOVEL     FNLSI1        KPJBU
003600980203     C                   SETON                                        RT
003700980203     C*--------------------------------------------------------------*
003800980203     C* INZ01: Caricamento subfile eventi manuali
003900980203     C*--------------------------------------------------------------*
004000980203     C     INZ01         BEGSR
004100980203     C*
004200980203     C                   Z-ADD     0             NRR1
004300980203     C* Clear del subfile
004400980203     C                   SETOFF                                       2021
004500980203     C                   WRITE     LSI1C01
004600980203     C                   SETON                                        2021
004700980203     C* Inizio caricamento
004800980203     C                   SETOFF                                       28
004900980203     C                   MOVEL     'N'           WFINE             1
005000980203     C                   MOVEL     NOMPGM        V1CPGM
005100980203     C                   MOVEL     'EM'          KCOD
005200980203     C     KCOD          CHAIN     EDTAB01L                           31
005300980203     C     *IN31         DOWEQ     '0'
005400980203     C                   MOVEL     TABUNI        DSEM
005500980203     C* Controllo se Labarta o VGL
005600980203     C     DI1LAB        IFEQ      'S'
005700980203     C     �EMLAB        ANDEQ     'S'
005800980203     C     DI1LAB        ORNE      'S'
005900980203     C     �EMVGL        ANDEQ     'S'
006000980204     C                   MOVEL     *BLANKS       V1CSCE
006100980203     C* Codice evento
006200980204     C                   MOVEL     TABKEY        WCOMO6            6
006300980203     C                   MOVE      WCOMO6        V1CCOD
006400980203     C                   MOVEL     WCOMO6        V1CSTS
006500980203     C* Flag e decodifiche
006600980203     C                   MOVEL     �EMDES        V1DCOD
006700980203     C                   MOVEL     �EMTOT        V1CTOT
006800980203     C                   MOVEL     �EMTRA        V1CTRA
006900980203     C                   MOVEL     �EMASS        V1CASS
007000980203     C                   MOVEL     �EMBLP        V1CBLP
007100980203     C                   MOVEL     �EMBLT        V1CSGN
007200980203     C                   MOVEL     �EMCCA        V1CCCA
007300980203     C                   MOVEL     �EMFOR        V1CFRZ
007400980203     C                   MOVEL     �EMCOD        V1CC2A
007500980203     C* Scrittura record
007600980203     C                   SETON                                        22
007700980203     C                   ADD       1             NRR1              4 0
007800980203     C                   WRITE     LSI1S01
007900980203     C                   END
008000980203     C* Lettura successiva
008100980203     C     KCOD          READE     EDTAB01L                               31
008200980203     C                   END
008300980203     C*
008400980203     C     NRR1          IFEQ      0
008500980203     C                   MOVEL     '1D'          WTPVID            2
008600980203     C                   ELSE
008700980203     C                   MOVEL     '1S'          WTPVID            2
008800980203     C                   END
008900980203     C*
009000980203     C                   ENDSR
009100980203     C*--------------------------------------------------------------*
009200980203     C* GESD01: Gestione videata vuota
009300980203     C*--------------------------------------------------------------*
009400980203     C     GESD01        BEGSR
009500980203     C*
009600980203     C* Emetto videata
009700980203     C                   EXFMT     LSI1D01
009800980203     C* Ritorno
009900980203     C     *INKL         IFEQ      '1'
010000980204     C                   MOVEL     *BLANKS       DI1STS
010100980204     C                   MOVEL     *BLANKS       DI1CEV
010200980203     C                   MOVEL     'S'           WFINE
010300980203     C                   MOVEL     '12'          DI1CMD
010400980203     C                   END
010500980203     C*
010600980203     C                   ENDSR
010700980203     C*--------------------------------------------------------------*
010800980203     C* GESS01: Gestione videata selezione eventi manuali
010900980203     C*--------------------------------------------------------------*
011000980203     C     GESS01        BEGSR
011100980203     C*
011200980203     C* Emetto videata
011300980203     C                   WRITE     LSI1Z01
011400980203     C                   EXFMT     LSI1C01
011500980203     C* Ritorno
011600980203     C     *INKL         IFEQ      '1'
011700980204     C                   MOVEL     *BLANKS       DI1STS
011800980204     C                   MOVEL     *BLANKS       DI1CEV
011900980203     C                   MOVEL     'S'           WFINE
012000980203     C                   MOVEL     '12'          DI1CMD
012100980203     C                   GOTO      FINVD1
012200980203     C                   END
012300980203     C* Controlli
012400980203     C                   EXSR      CTR01
012500980203     C*
012600980203     C     FINVD1        ENDSR
012700980203     C*--------------------------------------------------------------*
012800980203     C* CTR01: CONTROLLO selezioni
012900980203     C*--------------------------------------------------------------*
013000980203     C     CTR01         BEGSR
013100980203     C*
013200980203     C                   READC     LSI1S01                                29
013300980203     C     *IN29         DOWEQ     '0'
013400980203     C* Aggiorno totale colli mancanti
013500980203     C     V1CSCE        IFEQ      '1'
013600980203     C                   MOVEL     V1CSTS        DI1STS
013700980203     C                   MOVEL     V1CCOD        DI1CEV
013800980204     C                   MOVEL     'S'           WFINE
013900980203     C                   END
014000980203     C* Aggiornamento
014100980203     C                   SETON                                        22
014200980203     C                   UPDATE    LSI1S01
014300980203     C                   SETOFF                                       22
014400980203     C* Lettura successiva
014500980203     C                   READC     LSI1S01                                29
014600980203     C                   END
014700980203     C*
014800980203     C                   ENDSR
014900980203     C*--------------------------------------------------------------*
015000980203     C* *INZSR: Operazioni iniziali
015100980203     C*--------------------------------------------------------------*
015200980203     C     *INZSR        BEGSR
015300980203     C*
015400980203     C* Richiamo xparut
015500980203     C                   Z-ADD     1             CODUT
015600980203     C                   CALL      'X�PARUT'
015700980203     C                   PARM                    UTEDSE
015800980203     C                   MOVEL     REC80         CNCR80
015900980203     C                   MOVEL     RAGUT         RSUT
016000980203      *  Definizione variabili
016100980203     C     *LIKE         DEFINE    TABCOD        KCOD
016200980203     C*
016300980203     C                   ENDSR
