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
001500120910     D LnaUser         S              3  0 DIM(30)                              FIL. GESTITE
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
002800120910     d***** trul06ds      e ds
002900120910     d*****  lin                    1     90  0 dim(30)
003000120910     D TRUL09DS      E DS
003100120910     D  LIN                   19    108  0 DIM(30)                              P.O. COMODO
003200020506      *
003300980409     D KPJBA         E DS
003400980409     D  LIBSYS                92    100
003500980409     D  MSGERR               453    502
003600980409      *----------------------------------------------------------------
003700980409     IAZORG01L  AA
003800980409     I                             P    4    5 0ORGFIL
003900980409     I                                  6    6  ORGFVA
004000980409     I                                 11   11  ORGFL1
004100980409     I                                 14   33  ORGDES
004200020221     I                               3776 3800  ORGDE3
004300980409      *---------------------------------------------------------------*
004400980409      *  Azzeramento campi primo subfile
004500980409     C                   EXSR      INZS01
004600980409      *
004700980409      *  Loop gestione videate
004800980409     C     WFINE         DOUEQ     'S'
004900980409     C     WTPVID        CASEQ     '1'           GESS01
005000980409     C     WTPVID        CASEQ     '4'           GESD04
005100980409     C                   END
005200980409     C                   END
005300980409      *
005400980409     C     FINE          TAG
005500980409     C     WRICH         IFEQ      'S'
005600980409     C                   MOVEL     'U'           DK1TRI
005700980409     C                   EXSR      CALLK1
005800980409     C                   END
005900980409      *
006000980409     C                   SETON                                        LR
006100980409      *---------------------------------------------------------*
006200980409      *  Inizializzazione del primo subfile                     *
006300980409      *---------------------------------------------------------*
006400980409     C     INZS01        BEGSR
006500980409      *
006600980409     C                   Z-ADD     1             NR1KEY
006700980409     C                   Z-ADD     0             NRR1
006800980409     C                   SETOFF                                       202128
006900980409     C                   WRITE     LRK1C01
007000980409     C                   SETON                                        2021
007100980409     C                   SETOFF                                       40
007200980409      *
007300980409      *  Carico linee di arrivo estere
007400020913     C                   DO        30            X                 3 0
007500120910     c                   if        LnaUser(x) = 000
007600020913     c                   leave
007700020913     c                   end
007800980409     C                   MOVEL     *BLANKS       ORGFL1
007900120910     C     LnaUser(X)    CHAIN     AZORG01L                           31
008000020729     C                   clear                   og143
008100020729     C  n31              MOVEL     orgde3        og143
008200020729     C                   MOVEL     *blank        flgest            1
008300020729     C  n31§OgNTW        ifeq      'FED'
008400020729     C     §OgNTW        oreq      'EEX'
008500020729     C     §OgNTW        oreq      'EUP'
008600020729     C                   MOVEL     'E'           flgest
008700020729     c                   end
008800020913     C*
008900020729     C     flgest        IFEQ      'E'
009000980409     C     *IN31         ANDEQ     '0'
009100980409     C     ORGFVA        ANDEQ     ' '
009200020913     C*
009300020221      * Non deve essere FEDEX
009400020221     C     §ogntw        IFne      'FED'
009500980409     C     ORGFIL        CHAIN     FNCDA000                           32
009600980409     C     *IN32         IFEQ      '0'
009700980409     C                   ADD       1             NRR1              4 0
009800980409     C                   MOVEL     *BLANKS       V1CSCE
009900120910     C                   MOVE      LnaUser(X)    V1CLNA
010000980409     C                   MOVEL     ORGDES        V1DLNA
010100980409     C                   WRITE     LRK1S01
010200980409     C                   END
010300020221     c                   endif
010400980409     C                   END
010500980409      *
010600980409     C                   END
010700980409      *
010800980409     C     NRR1          IFEQ      0
010900980409     C                   MOVEL     '4'           WTPVID
011000980409     C                   END
011100980409      *
011200980409     C                   ENDSR
011300980409      *---------------------------------------------------------*
011400980409      *  Gestione del subfile                                   *
011500980409      *---------------------------------------------------------*
011600980409     C     GESS01        BEGSR
011700980409      *
011800980409     C                   WRITE     LRK1Z01
011900980409     C                   EXFMT     LRK1C01
012000980409      *
012100980409     C                   SETOFF                                       2840
012200980409     C                   SETON                                        3334
012300980409     C                   Z-ADD     1             NR1KEY
012400980409     C                   MOVEL     'N'           WRICA             1
012500980409      *  Fine Lavoro
012600980409     C     *INKC         IFEQ      '1'
012700980409     C                   MOVEL     'S'           WFINE
012800980409     C                   ELSE
012900980409      *  Controlli
013000980409     C                   EXSR      CTR01
013100980409      *  Ricarica Subfile
013200980409     C     WRICA         IFEQ      'S'
013300980409     C                   EXSR      INZS01
013400980409     C                   END
013500980409      *
013600980409     C                   END
013700980409      *
013800980409     C     FINVD1        ENDSR
013900980409      *---------------------------------------------------------*
014000980409      *  Gestione della quarta  videata                         *
014100980409      *---------------------------------------------------------*
014200980409     C     GESD04        BEGSR
014300980409      *
014400980409     C                   EXFMT     LRK1D04
014500980409     C                   SETON                                        3334
014600980409      *  Fine Lavoro
014700980409     C     *INKC         IFEQ      '1'
014800980409     C                   MOVEL     'S'           WFINE
014900980409     C                   END
015000980409      *
015100980409     C                   ENDSR
015200980409      *---------------------------------------------------------*
015300980409      *  Controlli                                              *
015400980409      *---------------------------------------------------------*
015500980409     C     CTR01         BEGSR
015600980409      *
015700980409     C                   READC     LRK1S01                                32
015800980409      *
015900980409     C     *IN32         DOWEQ     '0'
016000980409     C     *IN28         ANDEQ     '0'
016100980409     C     WFINE         ANDNE     'S'
016200980409      *
016300980409     C                   SETOFF                                       40
016400980409      *
016500980409     C     V1CSCE        IFEQ      '1'
016600980415      *
016700980415     C                   Z-ADD     1             KKUT
016800980415     C                   MOVEL     '4G'          KCOD
016900980415     C                   MOVEL     *BLANKS       KKEY
017000980415     C                   MOVEL     V1CLNA        KKEY
017100980409     C                   Z-ADD     V1CLNA        DK1LNA
017200980409     C                   MOVEL     V1DLNA        DK1DLA
017300980415     C     KTAB          CHAIN     TABEL00F                           3433
017400980415     C     *IN33         IFEQ      '1'
017500980415     C                   SETON                                        4028
017600980415     C                   MOVEL     ERR(1)        $MSG
017700980415     C                   Z-ADD     NRR1          NR1KEY
017800980415     C                   GOTO      UPDREC
017900980415     C                   ELSE
018000980415     C     *IN34         IFEQ      '1'
018100980415     C                   SETON                                        4028
018200980415     C                   MOVEL     ERR(2)        $MSG
018300980415     C                   Z-ADD     NRR1          NR1KEY
018400980415     C                   GOTO      UPDREC
018500980415     C                   ELSE
018600980414     C                   EXSR      CALLK1
018700980409     C                   MOVEL     ' '           V1CSCE
018800980409     C                   MOVEL     'S'           WRICH             1
018900980409     C                   END
019000980409      *
019100980415     C                   END
019200980415     C                   END
019300980415      *
019400980415     C     UPDREC        TAG
019500980409     C                   SETON                                        22
019600980409     C                   UPDATE    LRK1S01
019700980409     C                   SETOFF                                       22
019800980409      *
019900980409     C  N28              READC     LRK1S01                                32
020000980409     C                   END
020100980409      *
020200980409     C                   ENDSR
020300980409      *---------------------------------------------------------*
020400980409      *  Controlli                                              *
020500980409      *---------------------------------------------------------*
020600980409     C     CALLK1        BEGSR
020700980409      *
020800980414      *  Imposto il numeratore della distinta
020900980409     C     DK1TRI        IFNE      'U'
021000980409     C                   Z-ADD     1             DK1NDT
021100980409     C                   END
021200980409      *
021300980409     C                   MOVEL     *BLANKS       DK1F03
021400980409     C                   MOVEL     *BLANKS       DK1F12
021500980409     C                   MOVEL     FNLRK1        KPJBU
021600980409     C                   CALL      'FNLRK1R1'
021700980409     C                   PARM                    KPJBA
021800980409     C                   MOVEL     KPJBU         FNLRK1
021900980415      *
022000980415      *  Dealloco tabella 4G
022100980415     C     KTAB          CHAIN     TABEL00F                           3433
022200980415     C  N33
022300980415     CANN34              EXCEPT    NOAGG
022400980409      *
022500980409      *  Fine Lavoro
022600980409     C     DK1F03        IFEQ      '1'
022700980409     C                   MOVEL     'S'           WFINE
022800980409     C                   SETON                                        31
022900980409     C                   ELSE
023000980409     C                   MOVEL     'S'           WRICA             1
023100980409     C                   END
023200980409      *
023300980409     C                   CLEAR                   FNLRK1
023400980409      *
023500980409     C                   ENDSR
023600980409      *-----------------------------------------------------*
023700980409      *  Operazioni iniziali
023800980409      *-----------------------------------------------------*
023900980409     C     *INZSR        BEGSR
024000980409      *
024100020913     c**** *Dtaara       Define    §azute        AzuteDs
024200020913     c     *Dtaara       Define    §datiute      DDatiUte
024300020913      *
024400980409     C     KTAB          KLIST
024500980409     C                   KFLD                    KKUT
024600980409     C                   KFLD                    KCOD
024700980409     C                   KFLD                    KKEY
024800980409      *
024900980409     C     *LIKE         DEFINE    TBLKUT        KKUT
025000980409     C     *LIKE         DEFINE    TBLCOD        KCOD
025100980409     C     *LIKE         DEFINE    TBLKEY        KKEY
025200980409      *
025300980409     C     *ENTRY        PLIST
025400980409     C                   PARM                    KPJBA
025500980409      *
025600020913     C* Decodifica Elaboratore
025700020913     C                   CLEAR                   Tibs34Ds
025800020913     c                   EVAL      I34ute = KNMUS
025900020913     c                   EVAL      I34dta = ' '
026000020913     c                   EVAL      I34Tla = 'L'
026100020913     C                   CALL      'TIBS34R'
026200020913     C                   PARM                    Tibs34Ds
026300020913     c                   In        *Dtaara
026400980409      *
026500980409     C                   CLEAR                   FNLRK1
026600980409      *
026700980409     C                   TIME                    WHHDAT           12 0
026800980409     C                   MOVEL     ' '           WFINE             1
026900980409     C                   MOVEL     '1'           WTPVID            1
027000980409     C                   MOVEL     'P'           DK1TRI
027100980409     C                   MOVEL     'N'           WRICH
027200980409      *
027300980409     C                   OPEN      FNCDA01L                             16
027400980409     C   16              GOTO      FINE                                         OPEN vado a fine
027500980409      *
027600980409      *  Caricamento £1
027700020506     C                   Z-ADD     0             §F                3 0
027800120910      **
027900120910     c********           clear                   trul06ds
028000120910     c********           eval      d06cod = '£1'
028100120910     c********           movel     simfel        d06key
028200120910     c********           movel     trul06ds      kpjbu
028300120910     c********           call      'TRUL06R'
028400120910     c********           parm                    kpjba
028500120910     c********           movel     kpjbu         trul06ds
028600120910     c********           movea     lin           £1
028700120910      **
028800120910     c* Carico x terminal  di arrivo
028900120910     c                   clear                   trul09ds
029000120910     c                   movel     dutPOU        i09tfa
029100120910     c                   movel     'TFA'         i09mod
029200120910     c                   call      'TRUL09R'
029300120910     c                   parm                    trul09ds
029400120910     c                   movea     lin           LnaUser
029500980409      *
029600980409     C                   ENDSR
029700980409      *-----------------------------------------------------*
029800980409     OTABEL     E            NOAGG
029900980409**
030000980409Un altro operatore sta chiudendo la stessa linea di arrivo !!!        01
030100980409Linea di arrivo non presente su tabella 4G !!!                        02
