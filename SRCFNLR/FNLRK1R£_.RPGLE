000100980409     H DECEDIT('0,') DATEDIT(*DMY.)
000200980409      *--------------------------------------------------------------*
000300980409      *         - GESTIONE DISTINTA ARRIVI ESTERO -                  *
000400980409      *             -- SUBFILE LINEE DI ARRIVO --                    *
000500980409      *--------------------------------------------------------------*
000600980409     FFNLRK1D   CF   E             WORKSTN
000700980409     F                                     SFILE(LRK1S01:NRR1)
000800980409     FFNCDA01L  IF   E           K DISK    USROPN
000900980409     F                                     COMMIT
001000980409     FTABEL00F  UF   E           K DISK
001100980409     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
001200980409      *----------------------------------------------------------------*
001300980409      *    s c h i e r e                                               *
001400980409      *----------------------------------------------------------------*
001500020506     D*UNI             S              3    DIM(29)                              LINEE ARRIVI
001600020506     D £1              S              3  0 DIM(30)                              FIL. GESTITE
001700980409     D ERR             S             70    DIM(2) CTDATA PERRCD(1)
001800980409     D                SDS
001900980409     D  NOMPGM                 1     10
002000980409      *----------------------------------------------------------------
002100980409     D FNLRK1        E DS                  EXTNAME(FNLRK1DS)
002200980409      *
002300980409     D DS§FIL        E DS
002400980409      *
002500020221     D og143         E DS
002600020221      *
002700020506     d trul06ds      e ds
002800020506     d  lin                    1     90  0 dim(30)
002900020506      *
003000980409     D KPJBA         E DS
003100980409     D  LIBSYS                92    100
003200980409     D  MSGERR               453    502
003300980409      *----------------------------------------------------------------
003400980409     IAZORG01L  AA
003500980409     I                             P    4    5 0ORGFIL
003600980409     I                                  6    6  ORGFVA
003700980409     I                                 11   11  ORGFL1
003800980409     I                                 14   33  ORGDES
003900020221     I                               3776 3800  ORGDE3
004000980409      *---------------------------------------------------------------*
004100980409      *  Azzeramento campi primo subfile
004200980409     C                   EXSR      INZS01
004300980409      *
004400980409      *  Loop gestione videate
004500980409     C     WFINE         DOUEQ     'S'
004600980409     C     WTPVID        CASEQ     '1'           GESS01
004700980409     C     WTPVID        CASEQ     '4'           GESD04
004800980409     C                   END
004900980409     C                   END
005000980409      *
005100980409     C     FINE          TAG
005200980409     C     WRICH         IFEQ      'S'
005300980409     C                   MOVEL     'U'           DK1TRI
005400980409     C                   EXSR      CALLK1
005500980409     C                   END
005600980409      *
005700980409     C                   SETON                                        LR
005800980409      *---------------------------------------------------------*
005900980409      *  Inizializzazione del primo subfile                     *
006000980409      *---------------------------------------------------------*
006100980409     C     INZS01        BEGSR
006200980409      *
006300980409     C                   Z-ADD     1             NR1KEY
006400980409     C                   Z-ADD     0             NRR1
006500980409     C                   SETOFF                                       202128
006600980409     C                   WRITE     LRK1C01
006700980409     C                   SETON                                        2021
006800980409     C                   SETOFF                                       40
006900980409      *
007000980409      *  Carico linee di arrivo estere
007100980409     C                   DO        §F            X                 3 0
007200980409     C                   MOVEL     *BLANKS       ORGFL1
007300980409     C     £1(X)         CHAIN     AZORG01L                           31
007400020729     C                   clear                   og143
007500020729     C  n31              MOVEL     orgde3        og143
007600020729     C                   MOVEL     *blank        flgest            1
007700020729     C  n31§OgNTW        ifeq      'FED'
007800020729     C     §OgNTW        oreq      'EEX'
007900020729     C     §OgNTW        oreq      'EUP'
008000020729     C                   MOVEL     'E'           flgest
008100020729     c                   end
008200020729     C**** ORGFL1        IFEQ      'E'
008300020729     C     flgest        IFEQ      'E'
008400980409     C     *IN31         ANDEQ     '0'
008500980409     C     ORGFVA        ANDEQ     ' '
008600020729     C*****              MOVEL     orgde3        og143
008700020221      * Non deve essere FEDEX
008800020221     C     §ogntw        IFne      'FED'
008900980409     C     ORGFIL        CHAIN     FNCDA000                           32
009000980409     C     *IN32         IFEQ      '0'
009100980409     C                   ADD       1             NRR1              4 0
009200980409     C                   MOVEL     *BLANKS       V1CSCE
009300980409     C                   MOVE      £1(X)         V1CLNA
009400980409     C                   MOVEL     ORGDES        V1DLNA
009500980409     C                   WRITE     LRK1S01
009600980409     C                   END
009700020221     c                   endif
009800980409     C                   END
009900980409      *
010000980409     C                   END
010100980409      *
010200980409     C     NRR1          IFEQ      0
010300980409     C                   MOVEL     '4'           WTPVID
010400980409     C                   END
010500980409      *
010600980409     C                   ENDSR
010700980409      *---------------------------------------------------------*
010800980409      *  Gestione del subfile                                   *
010900980409      *---------------------------------------------------------*
011000980409     C     GESS01        BEGSR
011100980409      *
011200980409     C                   WRITE     LRK1Z01
011300980409     C                   EXFMT     LRK1C01
011400980409      *
011500980409     C                   SETOFF                                       2840
011600980409     C                   SETON                                        3334
011700980409     C                   Z-ADD     1             NR1KEY
011800980409     C                   MOVEL     'N'           WRICA             1
011900980409      *  Fine Lavoro
012000980409     C     *INKC         IFEQ      '1'
012100980409     C                   MOVEL     'S'           WFINE
012200980409     C                   ELSE
012300980409      *  Controlli
012400980409     C                   EXSR      CTR01
012500980409      *  Ricarica Subfile
012600980409     C     WRICA         IFEQ      'S'
012700980409     C                   EXSR      INZS01
012800980409     C                   END
012900980409      *
013000980409     C                   END
013100980409      *
013200980409     C     FINVD1        ENDSR
013300980409      *---------------------------------------------------------*
013400980409      *  Gestione della quarta  videata                         *
013500980409      *---------------------------------------------------------*
013600980409     C     GESD04        BEGSR
013700980409      *
013800980409     C                   EXFMT     LRK1D04
013900980409     C                   SETON                                        3334
014000980409      *  Fine Lavoro
014100980409     C     *INKC         IFEQ      '1'
014200980409     C                   MOVEL     'S'           WFINE
014300980409     C                   END
014400980409      *
014500980409     C                   ENDSR
014600980409      *---------------------------------------------------------*
014700980409      *  Controlli                                              *
014800980409      *---------------------------------------------------------*
014900980409     C     CTR01         BEGSR
015000980409      *
015100980409     C                   READC     LRK1S01                                32
015200980409      *
015300980409     C     *IN32         DOWEQ     '0'
015400980409     C     *IN28         ANDEQ     '0'
015500980409     C     WFINE         ANDNE     'S'
015600980409      *
015700980409     C                   SETOFF                                       40
015800980409      *
015900980409     C     V1CSCE        IFEQ      '1'
016000980415      *
016100980415     C                   Z-ADD     1             KKUT
016200980415     C                   MOVEL     '4G'          KCOD
016300980415     C                   MOVEL     *BLANKS       KKEY
016400980415     C                   MOVEL     V1CLNA        KKEY
016500980409     C                   Z-ADD     V1CLNA        DK1LNA
016600980409     C                   MOVEL     V1DLNA        DK1DLA
016700980415     C     KTAB          CHAIN     TABEL00F                           3433
016800980415     C     *IN33         IFEQ      '1'
016900980415     C                   SETON                                        4028
017000980415     C                   MOVEL     ERR(1)        $MSG
017100980415     C                   Z-ADD     NRR1          NR1KEY
017200980415     C                   GOTO      UPDREC
017300980415     C                   ELSE
017400980415     C     *IN34         IFEQ      '1'
017500980415     C                   SETON                                        4028
017600980415     C                   MOVEL     ERR(2)        $MSG
017700980415     C                   Z-ADD     NRR1          NR1KEY
017800980415     C                   GOTO      UPDREC
017900980415     C                   ELSE
018000980414     C                   EXSR      CALLK1
018100980409     C                   MOVEL     ' '           V1CSCE
018200980409     C                   MOVEL     'S'           WRICH             1
018300980409     C                   END
018400980409      *
018500980415     C                   END
018600980415     C                   END
018700980415      *
018800980415     C     UPDREC        TAG
018900980409     C                   SETON                                        22
019000980409     C                   UPDATE    LRK1S01
019100980409     C                   SETOFF                                       22
019200980409      *
019300980409     C  N28              READC     LRK1S01                                32
019400980409     C                   END
019500980409      *
019600980409     C                   ENDSR
019700980409      *---------------------------------------------------------*
019800980409      *  Controlli                                              *
019900980409      *---------------------------------------------------------*
020000980409     C     CALLK1        BEGSR
020100980409      *
020200980414      *  Imposto il numeratore della distinta
020300980409     C     DK1TRI        IFNE      'U'
020400980409     C                   Z-ADD     1             DK1NDT
020500980409     C                   END
020600980409      *
020700980409     C                   MOVEL     *BLANKS       DK1F03
020800980409     C                   MOVEL     *BLANKS       DK1F12
020900980409     C                   MOVEL     FNLRK1        KPJBU
021000980409     C                   CALL      'FNLRK1R1'
021100980409     C                   PARM                    KPJBA
021200980409     C                   MOVEL     KPJBU         FNLRK1
021300980415      *
021400980415      *  Dealloco tabella 4G
021500980415     C     KTAB          CHAIN     TABEL00F                           3433
021600980415     C  N33
021700980415     CANN34              EXCEPT    NOAGG
021800980409      *
021900980409      *  Fine Lavoro
022000980409     C     DK1F03        IFEQ      '1'
022100980409     C                   MOVEL     'S'           WFINE
022200980409     C                   SETON                                        31
022300980409     C                   ELSE
022400980409     C                   MOVEL     'S'           WRICA             1
022500980409     C                   END
022600980409      *
022700980409     C                   CLEAR                   FNLRK1
022800980409      *
022900980409     C                   ENDSR
023000980409      *-----------------------------------------------------*
023100980409      *  Operazioni iniziali
023200980409      *-----------------------------------------------------*
023300980409     C     *INZSR        BEGSR
023400980409      *
023500980409     C     KTAB          KLIST
023600980409     C                   KFLD                    KKUT
023700980409     C                   KFLD                    KCOD
023800980409     C                   KFLD                    KKEY
023900980409      *
024000980409     C     *LIKE         DEFINE    TBLKUT        KKUT
024100980409     C     *LIKE         DEFINE    TBLCOD        KCOD
024200980409     C     *LIKE         DEFINE    TBLKEY        KKEY
024300980409      *
024400980409     C     *ENTRY        PLIST
024500980409     C                   PARM                    KPJBA
024600980409      *
024700980409      * Decodifica Elaboratore
024800980409     C                   CALL      'X§FIL'
024900980409     C                   PARM                    DS§FIL
025000980409     C                   MOVEL     DSFIRS        RSUT
025100980409      *
025200980409     C                   CLEAR                   FNLRK1
025300980409      *
025400980409     C                   TIME                    WHHDAT           12 0
025500980409     C                   MOVEL     ' '           WFINE             1
025600980409     C                   MOVEL     '1'           WTPVID            1
025700980409     C                   MOVEL     'P'           DK1TRI
025800980409     C                   MOVEL     'N'           WRICH
025900980409      *
026000980409     C                   OPEN      FNCDA01L                             16
026100980409     C   16              GOTO      FINE                                         OPEN vado a fine
026200980409      *
026300980409      *  Caricamento £1
026400020506     C                   Z-ADD     0             §F                3 0
026500020506     c                   clear                   trul06ds
026600020506     c                   eval      d06cod = '£1'
026700020506     c                   movel     dsfifi        d06key
026800020506     c                   movel     trul06ds      kpjbu
026900020506     c                   call      'TRUL06R'
027000020506     c                   parm                    kpjba
027100020506     c                   movel     kpjbu         trul06ds
027200020506     c                   movea     lin           £1
027300020506     C*                  Z-ADD     1             KKUT
027400020506     C*                  MOVEL     '£1'          KCOD
027500020506     C*                  MOVEA     *ZEROS        £1
027600020506     C*                  MOVEL     *BLANKS       KKEY
027700020506     C*                  MOVEL     DSFIFI        KKEY
027800020506     C*    KTAB          CHAIN(N)  TABEL00F                           31
027900020506     C*    *IN31         IFEQ      '0'
028000020506     C*    TBLFLG        ANDEQ     ' '
028100020506     C*                  MOVEA     TBLUNI        UNI
028200020506     C*                  Z-ADD     0             §F                3 0
028300020506     C*                  DO        29            X                 3 0
028400020506     C*    UNI(X)        IFNE      *BLANKS
028500020506     C*    UNI(X)        ANDNE     *ZEROS
028600020506     C*                  ADD       1             §F
028700020506     C*                  MOVE      UNI(X)        £1(§F)
028800020506     C*                  END
028900020506     C*                  END
029000020506     C*                  ELSE
029100020506     C*                  MOVE      DSFIFI        £1(1)
029200020506     C*                  ADD       1             §F
029300020506     C*                  END
029400980409      *
029500980409     C                   ENDSR
029600980409      *-----------------------------------------------------*
029700980409     OTABEL     E            NOAGG
029800980409**
029900980409Un altro operatore sta chiudendo la stessa linea di arrivo !!!        01
030000980409Linea di arrivo non presente su tabella 4G !!!                        02
