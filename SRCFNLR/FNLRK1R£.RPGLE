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
001500020506     D £1              S              3  0 DIM(30)                              FIL. GESTITE
001600980409     D ERR             S             70    DIM(2) CTDATA PERRCD(1)
001700980409     D                SDS
001800980409     D  NOMPGM                 1     10
001900980409      *----------------------------------------------------------------
002000980409     D FNLRK1        E DS                  EXTNAME(FNLRK1DS)
002100980409      *
002200980409     D DS§FIL        E DS
002300980409      *
002400020221     D og143         E DS
002500020221      *
002600020913     D Tibs34ds      E DS
002700020913     D Ddatiute      E DS
002800020506     d trul06ds      e ds
002900020506     d  lin                    1     90  0 dim(30)
003000020506      *
003100980409     D KPJBA         E DS
003200980409     D  LIBSYS                92    100
003300980409     D  MSGERR               453    502
003400980409      *----------------------------------------------------------------
003500980409     IAZORG01L  AA
003600980409     I                             P    4    5 0ORGFIL
003700980409     I                                  6    6  ORGFVA
003800980409     I                                 11   11  ORGFL1
003900980409     I                                 14   33  ORGDES
004000020221     I                               3776 3800  ORGDE3
004100980409      *---------------------------------------------------------------*
004200980409      *  Azzeramento campi primo subfile
004300980409     C                   EXSR      INZS01
004400980409      *
004500980409      *  Loop gestione videate
004600980409     C     WFINE         DOUEQ     'S'
004700980409     C     WTPVID        CASEQ     '1'           GESS01
004800980409     C     WTPVID        CASEQ     '4'           GESD04
004900980409     C                   END
005000980409     C                   END
005100980409      *
005200980409     C     FINE          TAG
005300980409     C     WRICH         IFEQ      'S'
005400980409     C                   MOVEL     'U'           DK1TRI
005500980409     C                   EXSR      CALLK1
005600980409     C                   END
005700980409      *
005800980409     C                   SETON                                        LR
005900980409      *---------------------------------------------------------*
006000980409      *  Inizializzazione del primo subfile                     *
006100980409      *---------------------------------------------------------*
006200980409     C     INZS01        BEGSR
006300980409      *
006400980409     C                   Z-ADD     1             NR1KEY
006500980409     C                   Z-ADD     0             NRR1
006600980409     C                   SETOFF                                       202128
006700980409     C                   WRITE     LRK1C01
006800980409     C                   SETON                                        2021
006900980409     C                   SETOFF                                       40
007000980409      *
007100980409      *  Carico linee di arrivo estere
007200020913     C                   DO        30            X                 3 0
007300020913     c                   if        £1(x) = 000
007400020913     c                   leave
007500020913     c                   end
007600980409     C                   MOVEL     *BLANKS       ORGFL1
007700980409     C     £1(X)         CHAIN     AZORG01L                           31
007800020729     C                   clear                   og143
007900020729     C  n31              MOVEL     orgde3        og143
008000020729     C                   MOVEL     *blank        flgest            1
008100020729     C  n31§OgNTW        ifeq      'FED'
008200020729     C     §OgNTW        oreq      'EEX'
008300020729     C     §OgNTW        oreq      'EUP'
008400020729     C                   MOVEL     'E'           flgest
008500020729     c                   end
008600020913     C*
008700020729     C     flgest        IFEQ      'E'
008800980409     C     *IN31         ANDEQ     '0'
008900980409     C     ORGFVA        ANDEQ     ' '
009000020913     C*
009100020221      * Non deve essere FEDEX
009200020221     C     §ogntw        IFne      'FED'
009300980409     C     ORGFIL        CHAIN     FNCDA000                           32
009400980409     C     *IN32         IFEQ      '0'
009500980409     C                   ADD       1             NRR1              4 0
009600980409     C                   MOVEL     *BLANKS       V1CSCE
009700980409     C                   MOVE      £1(X)         V1CLNA
009800980409     C                   MOVEL     ORGDES        V1DLNA
009900980409     C                   WRITE     LRK1S01
010000980409     C                   END
010100020221     c                   endif
010200980409     C                   END
010300980409      *
010400980409     C                   END
010500980409      *
010600980409     C     NRR1          IFEQ      0
010700980409     C                   MOVEL     '4'           WTPVID
010800980409     C                   END
010900980409      *
011000980409     C                   ENDSR
011100980409      *---------------------------------------------------------*
011200980409      *  Gestione del subfile                                   *
011300980409      *---------------------------------------------------------*
011400980409     C     GESS01        BEGSR
011500980409      *
011600980409     C                   WRITE     LRK1Z01
011700980409     C                   EXFMT     LRK1C01
011800980409      *
011900980409     C                   SETOFF                                       2840
012000980409     C                   SETON                                        3334
012100980409     C                   Z-ADD     1             NR1KEY
012200980409     C                   MOVEL     'N'           WRICA             1
012300980409      *  Fine Lavoro
012400980409     C     *INKC         IFEQ      '1'
012500980409     C                   MOVEL     'S'           WFINE
012600980409     C                   ELSE
012700980409      *  Controlli
012800980409     C                   EXSR      CTR01
012900980409      *  Ricarica Subfile
013000980409     C     WRICA         IFEQ      'S'
013100980409     C                   EXSR      INZS01
013200980409     C                   END
013300980409      *
013400980409     C                   END
013500980409      *
013600980409     C     FINVD1        ENDSR
013700980409      *---------------------------------------------------------*
013800980409      *  Gestione della quarta  videata                         *
013900980409      *---------------------------------------------------------*
014000980409     C     GESD04        BEGSR
014100980409      *
014200980409     C                   EXFMT     LRK1D04
014300980409     C                   SETON                                        3334
014400980409      *  Fine Lavoro
014500980409     C     *INKC         IFEQ      '1'
014600980409     C                   MOVEL     'S'           WFINE
014700980409     C                   END
014800980409      *
014900980409     C                   ENDSR
015000980409      *---------------------------------------------------------*
015100980409      *  Controlli                                              *
015200980409      *---------------------------------------------------------*
015300980409     C     CTR01         BEGSR
015400980409      *
015500980409     C                   READC     LRK1S01                                32
015600980409      *
015700980409     C     *IN32         DOWEQ     '0'
015800980409     C     *IN28         ANDEQ     '0'
015900980409     C     WFINE         ANDNE     'S'
016000980409      *
016100980409     C                   SETOFF                                       40
016200980409      *
016300980409     C     V1CSCE        IFEQ      '1'
016400980415      *
016500980415     C                   Z-ADD     1             KKUT
016600980415     C                   MOVEL     '4G'          KCOD
016700980415     C                   MOVEL     *BLANKS       KKEY
016800980415     C                   MOVEL     V1CLNA        KKEY
016900980409     C                   Z-ADD     V1CLNA        DK1LNA
017000980409     C                   MOVEL     V1DLNA        DK1DLA
017100980415     C     KTAB          CHAIN     TABEL00F                           3433
017200980415     C     *IN33         IFEQ      '1'
017300980415     C                   SETON                                        4028
017400980415     C                   MOVEL     ERR(1)        $MSG
017500980415     C                   Z-ADD     NRR1          NR1KEY
017600980415     C                   GOTO      UPDREC
017700980415     C                   ELSE
017800980415     C     *IN34         IFEQ      '1'
017900980415     C                   SETON                                        4028
018000980415     C                   MOVEL     ERR(2)        $MSG
018100980415     C                   Z-ADD     NRR1          NR1KEY
018200980415     C                   GOTO      UPDREC
018300980415     C                   ELSE
018400980414     C                   EXSR      CALLK1
018500980409     C                   MOVEL     ' '           V1CSCE
018600980409     C                   MOVEL     'S'           WRICH             1
018700980409     C                   END
018800980409      *
018900980415     C                   END
019000980415     C                   END
019100980415      *
019200980415     C     UPDREC        TAG
019300980409     C                   SETON                                        22
019400980409     C                   UPDATE    LRK1S01
019500980409     C                   SETOFF                                       22
019600980409      *
019700980409     C  N28              READC     LRK1S01                                32
019800980409     C                   END
019900980409      *
020000980409     C                   ENDSR
020100980409      *---------------------------------------------------------*
020200980409      *  Controlli                                              *
020300980409      *---------------------------------------------------------*
020400980409     C     CALLK1        BEGSR
020500980409      *
020600980414      *  Imposto il numeratore della distinta
020700980409     C     DK1TRI        IFNE      'U'
020800980409     C                   Z-ADD     1             DK1NDT
020900980409     C                   END
021000980409      *
021100980409     C                   MOVEL     *BLANKS       DK1F03
021200980409     C                   MOVEL     *BLANKS       DK1F12
021300980409     C                   MOVEL     FNLRK1        KPJBU
021400980409     C                   CALL      'FNLRK1R1'
021500980409     C                   PARM                    KPJBA
021600980409     C                   MOVEL     KPJBU         FNLRK1
021700980415      *
021800980415      *  Dealloco tabella 4G
021900980415     C     KTAB          CHAIN     TABEL00F                           3433
022000980415     C  N33
022100980415     CANN34              EXCEPT    NOAGG
022200980409      *
022300980409      *  Fine Lavoro
022400980409     C     DK1F03        IFEQ      '1'
022500980409     C                   MOVEL     'S'           WFINE
022600980409     C                   SETON                                        31
022700980409     C                   ELSE
022800980409     C                   MOVEL     'S'           WRICA             1
022900980409     C                   END
023000980409      *
023100980409     C                   CLEAR                   FNLRK1
023200980409      *
023300980409     C                   ENDSR
023400980409      *-----------------------------------------------------*
023500980409      *  Operazioni iniziali
023600980409      *-----------------------------------------------------*
023700980409     C     *INZSR        BEGSR
023800980409      *
023900020913     c**** *Dtaara       Define    §azute        AzuteDs
024000020913     c     *Dtaara       Define    §datiute      DDatiUte
024100020913      *
024200980409     C     KTAB          KLIST
024300980409     C                   KFLD                    KKUT
024400980409     C                   KFLD                    KCOD
024500980409     C                   KFLD                    KKEY
024600980409      *
024700980409     C     *LIKE         DEFINE    TBLKUT        KKUT
024800980409     C     *LIKE         DEFINE    TBLCOD        KCOD
024900980409     C     *LIKE         DEFINE    TBLKEY        KKEY
025000980409      *
025100980409     C     *ENTRY        PLIST
025200980409     C                   PARM                    KPJBA
025300980409      *
025400020913     C* Decodifica Elaboratore
025500020913     C                   CLEAR                   Tibs34Ds
025600020913     c                   EVAL      I34ute = KNMUS
025700020913     c                   EVAL      I34dta = ' '
025800020913     c                   EVAL      I34Tla = 'L'
025900020913     C                   CALL      'TIBS34R'
026000020913     C                   PARM                    Tibs34Ds
026100020913     c                   In        *Dtaara
026200980409      *
026300980409     C                   CLEAR                   FNLRK1
026400980409      *
026500980409     C                   TIME                    WHHDAT           12 0
026600980409     C                   MOVEL     ' '           WFINE             1
026700980409     C                   MOVEL     '1'           WTPVID            1
026800980409     C                   MOVEL     'P'           DK1TRI
026900980409     C                   MOVEL     'N'           WRICH
027000980409      *
027100980409     C                   OPEN      FNCDA01L                             16
027200980409     C   16              GOTO      FINE                                         OPEN vado a fine
027300980409      *
027400980409      *  Caricamento £1
027500020506     C                   Z-ADD     0             §F                3 0
027600020506     c                   clear                   trul06ds
027700020506     c                   eval      d06cod = '£1'
027800020913     c                   movel     simfel        d06key
027900020506     c                   movel     trul06ds      kpjbu
028000020506     c                   call      'TRUL06R'
028100020506     c                   parm                    kpjba
028200020506     c                   movel     kpjbu         trul06ds
028300020506     c                   movea     lin           £1
028400980409      *
028500980409     C                   ENDSR
028600980409      *-----------------------------------------------------*
028700980409     OTABEL     E            NOAGG
028800980409**
028900980409Un altro operatore sta chiudendo la stessa linea di arrivo !!!        01
029000980409Linea di arrivo non presente su tabella 4G !!!                        02
