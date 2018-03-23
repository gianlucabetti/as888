000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200940722     H* FNLRA8R *----------------------------------------------------*
000300940722     H*      VISUALIZZAZIONE DETTAGLIO BOLLA TRANSITO
000400000000     H*--------------------------------------------------------------*
000500940722     FFNLRA8D   CF   E             WORKSTN
000600940722     F                                     SFILE(LRA8S01:NRR1)
000700920811     FTABEL00F  IF   E           K DISK
000800020822     FFNBTT11L  IF   E           K DISK
000900940722      *
001000930115     D PARAM           DS
001100940722     D  PARAAS                14     17  0
001200940722     D  PARLNP                18     20  0
001300940722     D  PARNRS                21     22  0
001400940722     D  PARNSP                23     29  0
001500940722     D  PARDSP                30     37  0
001600940722     D  PARLNA                38     40  0
001700940722     D  PADLNA                41     60
001800940722     D  PARRSD                61     95
001900940722     D  PARRSM                96    130
002000940722     D  PARDSA               131    151
002100940722     D  PARCML               152    152
002200020821     D  PARPOT               153    155  0
002300020821     D  PADPOT               156    170
002400950116     D* PASSAGGIO DATI A INTERROGAZIONE SPUNTE -FNLR04R -
002500921013     D PARAM1          DS
002600921013     D  VI1NFV                 1      5  0
002700921013     D  VI1LNP                 6      8  0
002800921013     D  VI1LNA                 9     11  0
002900921013     D  VI1NRS                12     13  0
003000921013     D  VI1NCD                14     20  0
003100921013     D  VI1NCA                21     27  0
003200921013     D  VI1NPS                28     29  0
003300921013     D  VI1NPG                30     30  0
003400921013     D  VI1FLG                31     31
003500921013     D  VI1PGM                32     38
003600950116     D  VI1FGS                39     41  0 INZ
003700101202     d* bolla
003800101202     d  vi1bolp               42     44  0
003900101202     d  vi1bors               45     46  0
004000101202     d  vi1bosp               47     53  0
004100101202     d  vi1boan               54     57  0
004200101202     D  PA1ERR                58     58
004300940722     D WLBDAT          DS
004400940722     D  G02DAT                 1      8  0
004500940722     D  G02INV                 9     16  0
004600940722     D  G02ERR                17     17
004700940722     D  G02TGI                18     22  0
004800900517     D KPJBA         E DS
004900120709     D ds3E          E DS
005000000000     C*---------------------------------------------------------------*
005100000000     C     *ENTRY        PLIST
005200000000     C                   PARM                    KPJBA
005300910219     C                   MOVEL     KPJBU         PARAM
005400940725     C*
005500940725     C                   MOVEL     PARDSA        V1CRSU
005600940725     C                   MOVEL     PADLNA        VCDLNA                         DECOD.LINEA ARRIVO
005700020821     C                   MOVEL     PADPOT        VCDPOT                         DECOD.LINEA ARRIVO
005800940725     C                   MOVEL     PARRSM        VCDRSM                         MITTENTE
005900991021     C                   MOVEL     PARRSD        VCDRSD                         DESTIN
006000930115     C*
006100921013     C* IMPOSTA CAMPI PER INT SPUNTE
006200921013     C                   Z-ADD     0             VI1NFV
006300950116     C                   MOVEL     *ZEROS        VI1LNP
006400930512     C                   MOVEL     PARLNA        VI1LNA
006500930115     C                   MOVEL     PARNRS        VI1NRS
006600101202     C                   MOVEL     PARNRS        VI1bors
006700101202     C                   MOVEL     PARlnp        VI1bolp
006800101202     C                   MOVEL     PARnsp        VI1bosp
006900101202     C                   MOVEL     PARaas        VI1boan
007000950203     C                   MOVEL     9             VI1NPG
007100921013     C                   Z-ADD     0             VI1NCD
007200921013     C                   Z-ADD     0             VI1NCA
007300921013     C                   MOVEL     99            VI1NPS
007400101202     C                   MOVEL     'FNLRA8R'     VI1PGM
007500921013     C                   MOVEL     '1'           VI1FLG
007600910806     C   30              GOTO      FINE
007700920811     C                   MOVEL     '3E'          COD
007800910219     C*
007900940722     C* CARICO RIGHE DI DETTAGLIO SEGNACOLLI FLBTT
008000940722     C                   EXSR      CARBTT
008100910219     C*
008200921013     C     FOR01         TAG
008300940722     C                   WRITE     LRA8T01                                      INTESTAZIONE VIDEO
008400940722     C                   WRITE     LRA8Z01
008500940722     C                   EXFMT     LRA8C01
008600921013     C*
008700921013     C* CMD19 - INTERROGAZIONE SPUNTE
008800921013     C   KT              MOVEL     PARAM1        KPJBU
008900940722     C   KT              CALL      'FNLR04R'
009000921013     C                   PARM                    KPJBA
009100921013     C   KT              GOTO      FOR01
009200910219     C*
009300000000     C     FINE          TAG
009400000000     C                   SETON                                        LR
009500940722     C**************************************************************************
009600940722     C* CARICO DETTAGLIO SEGNACOLLI
009700940722     C**************************************************************************
009800940722     C     CARBTT        BEGSR
009900940722     C*
010000930512     C                   SETON                                        35
010100940722     C                   WRITE     LRA8C01
010200930512     C                   SETOFF                                       35
010300930512     C*
010400940722     C                   Z-ADD     0             NRR1              4 0
010500910806     C                   SETOFF                                       30
010600020822     C     KBTT          SETLL     FNBTT11L
010700020822     C     KBTT          READE     FNBTT11L                               30
010800940725     C  N30              MOVEL     BTTNSC        VI1NCD
010900950116     C  N30              MOVEL     BTTFLS        VI1LNP
011000910220     C*
011100910806     C     *IN30         DOWEQ     '0'
011200940725     C*
011300941220     C                   Z-ADD     BTTFLS        VSCFLS                         FIL.SEGNACOLLO
011400940725     C                   Z-ADD     BTTNSC        VSCNSC                         NR.COLLI
011500940725     C*
011600920811     C* CODICE ANOMALIA
011700940722     C                   MOVEL     *BLANKS       VSDCAN
011800940722     C     BTTCAN        IFNE      ' '
011900940722     C                   MOVEL     BTTCAN        KEY
012000940722     C     KTAB1         CHAIN     TABEL00F                           31
012100120709     C  N31              MOVEL     TBLUNI        ds3e
012200120709     C   31              clear                   ds3e
012300940722     C                   MOVEL     ' -'          VSDCAN
012400940722     C                   MOVEL     BTTCAN        VSDCAN
012500120711     c                   eval      %subst(vsdcan:3:20)=§3edes
012600920811     C                   END
012700910912     C* GIRO DATE:
012800910912     C* PARTENZA MERCE
012900940722     C                   MOVEL     BTTDFV        G02INV
013000940722     C                   Z-ADD     *ZEROS        G02DAT
013100940722     C                   MOVEL     '3'           G02ERR
013200940722     C                   CALL      'XSRDA8'
013300940722     C                   PARM                    WLBDAT
013400940722     C                   MOVEL     G02DAT        VSCDFV
013500940722     C     BTTDFV        IFGT      0
013600940722     C                   MOVEL     BTTNPP        VSCNPP
013700930111     C                   ELSE
013800940722     C                   MOVEL     '  '          VSCNPP
013900930111     C                   END
014000910912     C* ARRIVO AL TRANSITO
014100940722     C                   MOVEL     BTTDET        G02INV
014200940722     C                   Z-ADD     *ZEROS        G02DAT
014300940722     C                   MOVEL     '3'           G02ERR
014400940722     C                   CALL      'XSRDA8'
014500940722     C                   PARM                    WLBDAT
014600940722     C                   MOVEL     G02DAT        VSCDET
014700940722     C     BTTDET        IFGT      0
014800940722     C                   MOVEL     BTTPET        VSCPET
014900930111     C                   ELSE
015000940722     C                   MOVEL     '  '          VSCPET
015100930111     C                   END
015200910912     C* USCITA DAL TRANSITO
015300940722     C                   MOVEL     BTTDUT        G02INV
015400940722     C                   Z-ADD     *ZEROS        G02DAT
015500940722     C                   MOVEL     '3'           G02ERR
015600940722     C                   CALL      'XSRDA8'
015700940722     C                   PARM                    WLBDAT
015800940722     C                   MOVEL     G02DAT        VSCDUT
015900940722     C     BTTDUT        IFGT      0
016000940722     C                   MOVEL     BTTPUT        VSCPUT
016100930111     C                   ELSE
016200940722     C                   MOVEL     '  '          VSCPUT
016300930111     C                   END
016400910912     C* ARRIVO MERCE ALLA FIL ARRIVO
016500940722     C                   MOVEL     BTTDAM        G02INV
016600940722     C                   Z-ADD     *ZEROS        G02DAT
016700940722     C                   MOVEL     '3'           G02ERR
016800940722     C                   CALL      'XSRDA8'
016900940722     C                   PARM                    WLBDAT
017000940722     C                   MOVEL     G02DAT        VSCDAM
017100930115     C*
017200930115     C* VEDO SE VOLUME INSERITO MANUALMENTE
017300940725     C                   MOVEL     '   '         VSDFVC
017400990913     C                   MOVEL     ' '           VSDFPC
017500940722     C     BTTVUC        IFGT      0
017600970424     C                   SELECT
017700970424     C     BTTFVC        WHENEQ    'M'                                          'M'=MANUALE
017800940725     C                   MOVEL     'MAN'         VSDFVC
017900970424     C     BTTFVC        WHENEQ    'P'                                          'M'=MANUALE
018000970424     C                   MOVEL     'PAR'         VSDFVC
018100970424     C     BTTFVC        WHENEQ    'T'                                          'M'=MANUALE
018200970424     C                   MOVEL     'TRA'         VSDFVC
018300970424     C                   ENDSL
018400970424     C                   END
018500940722     C                   MOVEL     BTTVUC        VSCVUC
018600990913     C     BTTPUC        IFGT      0
018700990913     C                   MOVE      BTTFPC        VSDFPC
018800990913     C                   ENDIF
018900990907     C                   MOVEL     BTTPUC        VSCPUC
019000910912     C*
019100910205     C* SCRIVO SUBFILE
019200940722     C                   ADD       1             NRR1
019300940722     C                   WRITE     LRA8S01
019400910912     C*
019500020822     C     KBTT          READE     FNBTT11L                               30
019600910205     C                   END
019700910205     C*
019800921013     C* SE PIU' DI UN COLLO MEMORIZZO ANCHE AL
019900940725     C     BTTNSC        IFNE      VI1NCD
020000940725     C                   MOVEL     BTTNSC        VI1NCA
020100921013     C                   END
020200921104     C*
020300940722     C     NRR1          IFEQ      0
020400941220     C                   Z-ADD     0             VSCFLS
020500940725     C                   Z-ADD     0             VSCNSC
020600940722     C                   Z-ADD     0             VSCDFV
020700940722     C                   MOVEL     '   '         VSCNPP
020800940725     C                   Z-ADD     0             VSCDET
020900940722     C                   MOVEL     '   '         VSCPET
021000940725     C                   Z-ADD     0             VSCDUT
021100940722     C                   MOVEL     '   '         VSCPUT
021200940722     C                   Z-ADD     0             VSCDAM
021300940725     C                   MOVEL     *BLANKS       VSDCAN
021400940725     C                   Z-ADD     0             VSCVUC
021500990907     C                   Z-ADD     0             VSCPUC
021600940725     C                   MOVEL     *BLANKS       VSDFVC
021700990913     C                   MOVEL     *BLANKS       VSDFPC
021800940722     C                   ADD       1             NRR1
021900940722     C                   WRITE     LRA8S01
022000921104     C                   END
022100910205     C*
022200910205     C                   ENDSR
022300940722     C**************************************************************************
022400940722     C*   R O U T I N E    I N I Z I A L E
022500940722     C**************************************************************************
022600940722     C     *INZSR        BEGSR
022700940722     C*
022800940722     C     KBTT          KLIST
022900020822     C                   KFLD                    PARPOT
023000020822     C                   KFLD                    PARAAS
023100940722     C                   KFLD                    PARLNP
023200940722     C                   KFLD                    PARNRS
023300940722     C                   KFLD                    PARNSP
023400940722     C     KTAB1         KLIST                                                  TABEL00F
023500940722     C                   KFLD                    CODUT
023600940722     C                   KFLD                    COD
023700940722     C                   KFLD                    KEY
023800940722     C*
023900940722     C     *LIKE         DEFINE    TBLCOD        COD
024000940722     C     *LIKE         DEFINE    TBLKEY        KEY
024100940722     C*
024200940722     C                   Z-ADD     1             CODUT             1 0
024300940722     C*
024400940722     C                   ENDSR
