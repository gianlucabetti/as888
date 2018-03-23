000100941025     H* FNLG42R *----------------------------------------------------*
000200920113     H*---------*                                                    *
000300900511     H*           - STAMPA STATISTICHE -                             *
000400000000     H*--------------------------------------------------------------*
000500941025     FFNLG42D   CF   E             WORKSTN
000600900619     FTABEL00F  IF   E           K DISK
000700971113     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
000800961112     D*--------------------------------------------------------------*
000900020430     D £1              S              3  0 DIM(30)
001000971113      *--------------------------------------------------------------*
001100900619     D DS2G          E DS
001200950105     D FNLG40        E DS                  EXTNAME(FNLG40DS)
001300941025     D WLBDA8          DS
001400941025     D  G02DAT                 1      8  0
001500941025     D  G02INV                 9     16  0
001600941025     D  G02ERR                17     17
001700941025     D  G02TGI                18     22  0
001800961112     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
001900961112     D CNCR80        E DS
002000020729     D OG143         E DS
002100020430     d trul06ds      e ds
002200020430     d  lin                    1     90  0 dim(30)
002300900131     D KPJBA         E DS
002400900131     D  LIBSYS                92    101
002500900131     D  MSGERR               453    502
002600971113      *----------------------------------------------------------------
002700971113     IAZORG01L  AA
002800971113     I                                 11   11  ORGFL1
002900020729     I                               3776 3800  ORGDE3
003000900131     C*---------------------------------------------------------------*
003100000000     C     CICLO         TAG
003200900511     C                   EXSR      PULVID
003300920826     C                   Z-ADD     §2GGDA        VGG1
003400920826     C                   Z-ADD     §2GGEA        VGG2
003500920826     C                   Z-ADD     §2GGRA        VGG4
003600000000     C     INIZIO        TAG
003700941025     C                   EXFMT     LG42D01
003800900423     C   KC              GOTO      FINE
003900900511     C** CONTROLLI + LANCIO LISTA
004000900511     C** NESSUNA LISTA SCELTA
004100900511     C     VSEL1         IFEQ      *BLANKS
004200950105     C     VSEL2         ANDEQ     *BLANKS
004300950105     C     VSEL4         ANDEQ     *BLANKS
004400900514     C                   SETON                                        50
004500900511     C                   GOTO      INIZIO
004600900511     C                   END
004700950105     C**
004800950105     C** SCELTA LISTA 4
004900950105     C     VSEL4         IFEQ      '1'
005000950105     C** DATA DAL
005100950105     C     VDRD          IFEQ      0
005200950105     C                   SETON                                        54
005300950105     C                   GOTO      INIZIO
005400950105     C                   ELSE
005500950105     C                   Z-ADD     VDRD          G02DAT
005600950105     C                   Z-ADD     0             G02INV
005700950105     C                   MOVEL     ' '           G02ERR
005800950105     C                   CALL      'XSRDA8'
005900950105     C                   PARM                    WLBDA8
006000950105     C     G02ERR        IFEQ      '1'
006100950105     C                   SETON                                        54
006200950105     C                   GOTO      INIZIO
006300950105     C                   END
006400950105     C                   END
006500950105     C                   Z-ADD     G02INV        AMGDRD            8 0
006600950105     C                   Z-ADD     G02DAT        VDRD
006700950105     C** DATA AL
006800950105     C     VDRA          IFEQ      0
006900950105     C                   SETON                                        55
007000950105     C                   GOTO      INIZIO
007100950105     C                   ELSE
007200950105     C                   Z-ADD     VDRA          G02DAT
007300950105     C                   Z-ADD     0             G02INV
007400950105     C                   MOVEL     ' '           G02ERR
007500950105     C                   CALL      'XSRDA8'
007600950105     C                   PARM                    WLBDA8
007700950105     C     G02ERR        IFEQ      '1'
007800950105     C                   SETON                                        55
007900950105     C                   GOTO      INIZIO
008000950105     C                   END
008100950105     C                   END
008200950105     C                   Z-ADD     G02INV        AMGDRA            8 0
008300950105     C                   Z-ADD     G02DAT        VDRA
008400950105     C** DATE DAL-AL DISCORDANTI
008500950105     C     AMGDRD        IFGT      AMGDRA
008600950105     C                   SETON                                        57
008700950105     C                   GOTO      INIZIO
008800950105     C                   END
008900950105     C**N.GIORNI DIVERSO DA 0
009000950105     C     VGG4          IFEQ      0
009100950105     C                   SETON                                        56
009200950105     C                   GOTO      INIZIO
009300950105     C                   END
009400950105     C                   END
009500950105     C** SE NON SOTTOMETTO NULLA TORNO A RIEMETTERE LA VIDEATA
009600950105     C  NKF              GOTO      INIZIO
009700950105     C*------------------------------------------------------*
009800900511     C** SCELTA LISTA 1
009900941025     C     VSEL1         IFEQ      '1'
010000950105     C                   CLEAR                   FNLG40
010100961112     C                   MOVEL     V1CTPS        D40ITA
010200950105     C                   MOVEL     VSEL1         D40SC1
010300950105     C                   Z-ADD     VGG1          D40GG1
010400950105     C                   MOVEL     FNLG40        KPJBU
010500941025     C                   MOVEL     'LG43'        KCOAZ
010600900511     C     KNMUS         IFNE      *ALL'1'
010700900511     C                   CALL      'BCH10'
010800900511     C                   PARM                    KPJBA
010900900511     C                   ELSE
011000941025     C                   CALL      'FNLG43R'
011100900511     C                   PARM                    KPJBA
011200900511     C                   END
011300900511     C                   END
011400950105     C*------------------------------------------------------*
011500900511     C** SCELTA LISTA 2
011600941025     C     VSEL2         IFEQ      '1'
011700950105     C                   CLEAR                   FNLG40
011800961112     C                   MOVEL     V1CTPS        D40ITA
011900950105     C                   MOVEL     VSEL2         D40SC2
012000950105     C                   Z-ADD     VGG2          D40GG2
012100950105     C                   MOVEL     FNLG40        KPJBU
012200941025     C                   MOVEL     'LG43'        KCOAZ
012300900511     C     KNMUS         IFNE      *ALL'1'
012400900511     C                   CALL      'BCH10'
012500900511     C                   PARM                    KPJBA
012600900511     C                   ELSE
012700941025     C                   CALL      'FNLG43R'
012800900511     C                   PARM                    KPJBA
012900900511     C                   END
013000900511     C                   END
013100950105     C*------------------------------------------------------*
013200950105     C** SCELTA LISTA 4
013300950105     C     VSEL4         IFEQ      '1'
013400950105     C                   CLEAR                   FNLG40
013500961112     C                   MOVEL     V1CTPS        D40ITA
013600950105     C                   MOVEL     VSEL4         D40SC4
013700950110     C                   Z-ADD     AMGDRD        D40DD4
013800950110     C                   Z-ADD     AMGDRA        D40DA4
013900950105     C                   Z-ADD     VGG4          D40GG4
014000950105     C                   MOVEL     FNLG40        KPJBU
014100941025     C                   MOVEL     'LG43'        KCOAZ
014200900511     C     KNMUS         IFNE      *ALL'1'
014300900511     C                   CALL      'BCH10'
014400900511     C                   PARM                    KPJBA
014500900511     C                   ELSE
014600941025     C                   CALL      'FNLG43R'
014700900511     C                   PARM                    KPJBA
014800900511     C                   END
014900900511     C                   END
015000950105     C*------------------------------------------------------*
015100000000     C                   GOTO      CICLO
015200000000     C     FINE          TAG
015300000000     C                   SETON                                        LR
015400900511     C*---------------------------------------------------------------*
015500900511     C*   P U L V I D                                                 *
015600900511     C*---------------------------------------------------------------*
015700900511     C     PULVID        BEGSR
015800961112     C*
015900961112     C                   MOVEL     'I'           V1CTPS
016000900511     C                   MOVEL     *BLANKS       VSEL1
016100900511     C                   Z-ADD     60            VGG1
016200900511     C                   MOVEL     *BLANKS       VSEL2
016300900511     C                   Z-ADD     15            VGG2
016400900511     C                   MOVEL     *BLANKS       VSEL4
016500900511     C                   Z-ADD     0             VDRD
016600900511     C                   Z-ADD     0             VDRA
016700900511     C                   Z-ADD     7             VGG4
016800900511     C*
016900950105     C                   CLEAR                   FNLG40
017000950105     C*
017100900511     C                   ENDSR
017200941025     C*---------------------------------------------------------------*
017300941025     C*  OPERAZIONI INIZIALI                                          *
017400941025     C*---------------------------------------------------------------*
017500941025     C     *INZSR        BEGSR
017600941025     C*
017700941025     C     *ENTRY        PLIST
017800941025     C                   PARM                    KPJBA
017900941025     C                   Z-ADD     1             CODUT
018000961112     C                   CALL      'X§PARUT'
018100941025     C                   PARM                    UTEDSE
018200961112     C                   MOVEL     REC80         CNCR80
018300941025     C     CODUT         CABEQ     -1            FINE
018400941025     C                   MOVEL     RAGUT         RSUT             20
018500941025     C** TABEL
018600941025     C     KTAB          KLIST
018700941025     C                   KFLD                    CODUT
018800941025     C                   KFLD                    COD               2
018900941025     C                   KFLD                    KEY               8
019000961112     C** CARICO £1
019100961112     C                   SETOFF                                           05
019200020430     c                   clear                   trul06ds
019300020430     c                   eval      d06cod = '£1'
019400020430     c                   movel     simfel        d06key
019500020430     c                   movel     trul06ds      kpjbu
019600020430     c                   call      'TRUL06R'
019700020430     c                   parm                    kpjba
019800020430     c                   movel     kpjbu         trul06ds
019900020430     c                   movea     lin           £1
020000020430     C                   DO        30            X                 3 0
020100020430     C     £1(X)         IFGT      *ZEROS
020200971113     C                   MOVEL     *BLANKS       ORGFL1
020300971120     C     *IN05         IFEQ      '0'
020400971113     C* Aggancio Azorg x sapere se filiale Estera
020500020430     C     £1(X)         CHAIN     AZORG01L                           30
020600020729     C                   clear                   OG143
020700020729     C  n30              MOVEL     ORGDE3        OG143
020800020729     C**** ORGFL1        COMP      'E'                                    05
020900020729     C                   setoff                                           05
021000020729     C     §OgNTW        ifeq      'FED'
021100020729     C     §OgNTW        oreq      'EEX'
021200020729     C     §OgNTW        oreq      'EUP'
021300020729     C                   seton                                            05
021400020729     c                   end
021500971113     C                   END
021600020430     C                   END
021700961112     C                   END
021800961112     C                   SORTA     £1
021900941025     C** RICERCO SULLA TABELLA I GG DA PROPORRE A VIDEO
022000941025     C                   MOVEL     '2G'          COD
022100941025     C                   CLEAR                   DS2G
022200941025     C                   MOVEL     *BLANKS       KEY
022300941025     C                   MOVEL     1             KEY
022400941025     C     KTAB          CHAIN     TABEL                              70
022500941025     C  N70              MOVEL     TBLUNI        DS2G
022600941025     C*
022700941025     C                   ENDSR
