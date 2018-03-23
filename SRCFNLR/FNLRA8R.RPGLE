000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200940722     H* FNLRA8R *----------------------------------------------------*
000300940722     H*      VISUALIZZAZIONE DETTAGLIO BOLLA TRANSITO
000400000000     H*--------------------------------------------------------------*
000500940722     FFNLRA8D   CF   E             WORKSTN
000600940722     F                                     SFILE(LRA8S01:NRR1)
000700920811     FTABEL00F  IF   E           K DISK
000800020822     FFNBTT11L  IF   E           K DISK
000900160517     FFNart01L  IF   E           K DISK
001000940722      *
001100930115     D PARAM           DS
001200940722     D  PARAAS                14     17  0
001300940722     D  PARLNP                18     20  0
001400940722     D  PARNRS                21     22  0
001500940722     D  PARNSP                23     29  0
001600940722     D  PARDSP                30     37  0
001700940722     D  PARLNA                38     40  0
001800940722     D  PADLNA                41     60
001900940722     D  PARRSD                61     95
002000940722     D  PARRSM                96    130
002100940722     D  PARDSA               131    151
002200940722     D  PARCML               152    152
002300020821     D  PARPOT               153    155  0
002400020821     D  PADPOT               156    170
002500950116     D* PASSAGGIO DATI A INTERROGAZIONE SPUNTE -FNLR04R -
002600921013     D PARAM1          DS
002700921013     D  VI1NFV                 1      5  0
002800921013     D  VI1LNP                 6      8  0
002900921013     D  VI1LNA                 9     11  0
003000921013     D  VI1NRS                12     13  0
003100921013     D  VI1NCD                14     20  0
003200921013     D  VI1NCA                21     27  0
003300921013     D  VI1NPS                28     29  0
003400921013     D  VI1NPG                30     30  0
003500921013     D  VI1FLG                31     31
003600921013     D  VI1PGM                32     38
003700950116     D  VI1FGS                39     41  0 INZ
003800101202     d* bolla
003900101202     d  vi1bolp               42     44  0
004000101202     d  vi1bors               45     46  0
004100101202     d  vi1bosp               47     53  0
004200101202     d  vi1boan               54     57  0
004300101202     D  PA1ERR                58     58
004400940722     D WLBDAT          DS
004500940722     D  G02DAT                 1      8  0
004600940722     D  G02INV                 9     16  0
004700940722     D  G02ERR                17     17
004800940722     D  G02TGI                18     22  0
004900900517     D KPJBA         E DS
005000120709     D ds3E          E DS
005100000000     C*---------------------------------------------------------------*
005200000000     C     *ENTRY        PLIST
005300000000     C                   PARM                    KPJBA
005400910219     C                   MOVEL     KPJBU         PARAM
005500940725     C*
005600940725     C                   MOVEL     PARDSA        V1CRSU
005700940725     C                   MOVEL     PADLNA        VCDLNA                         DECOD.LINEA ARRIVO
005800020821     C                   MOVEL     PADPOT        VCDPOT                         DECOD.LINEA ARRIVO
005900940725     C                   MOVEL     PARRSM        VCDRSM                         MITTENTE
006000991021     C                   MOVEL     PARRSD        VCDRSD                         DESTIN
006100930115     C*
006200921013     C* IMPOSTA CAMPI PER INT SPUNTE
006300921013     C                   Z-ADD     0             VI1NFV
006400950116     C                   MOVEL     *ZEROS        VI1LNP
006500930512     C                   MOVEL     PARLNA        VI1LNA
006600930115     C                   MOVEL     PARNRS        VI1NRS
006700101202     C                   MOVEL     PARNRS        VI1bors
006800101202     C                   MOVEL     PARlnp        VI1bolp
006900101202     C                   MOVEL     PARnsp        VI1bosp
007000101202     C                   MOVEL     PARaas        VI1boan
007100950203     C                   MOVEL     9             VI1NPG
007200921013     C                   Z-ADD     0             VI1NCD
007300921013     C                   Z-ADD     0             VI1NCA
007400921013     C                   MOVEL     99            VI1NPS
007500101202     C                   MOVEL     'FNLRA8R'     VI1PGM
007600921013     C                   MOVEL     '1'           VI1FLG
007700910806     C   30              GOTO      FINE
007800920811     C                   MOVEL     '3E'          COD
007900910219     C*
008000940722     C* CARICO RIGHE DI DETTAGLIO SEGNACOLLI FLBTT
008100940722     C                   EXSR      CARBTT
008200910219     C*
008300921013     C     FOR01         TAG
008400940722     C                   WRITE     LRA8T01                                      INTESTAZIONE VIDEO
008500940722     C                   WRITE     LRA8Z01
008600940722     C                   EXFMT     LRA8C01
008700921013     C*
008800921013     C* CMD19 - INTERROGAZIONE SPUNTE
008900921013     C   KT              MOVEL     PARAM1        KPJBU
009000940722     C   KT              CALL      'FNLR04R'
009100921013     C                   PARM                    KPJBA
009200921013     C   KT              GOTO      FOR01
009300910219     C*
009400000000     C     FINE          TAG
009500000000     C                   SETON                                        LR
009600940722     C**************************************************************************
009700940722     C* CARICO DETTAGLIO SEGNACOLLI
009800940722     C**************************************************************************
009900940722     C     CARBTT        BEGSR
010000940722     C*
010100930512     C                   SETON                                        35
010200940722     C                   WRITE     LRA8C01
010300930512     C                   SETOFF                                       35
010400930512     C*
010500940722     C                   Z-ADD     0             NRR1              4 0
010600910806     C                   SETOFF                                       30
010700020822     C     KBTT          SETLL     FNBTT11L
010800020822     C     KBTT          READE     FNBTT11L                               30
010900940725     C  N30              MOVEL     BTTNSC        VI1NCD
011000950116     C  N30              MOVEL     BTTFLS        VI1LNP
011100910220     C*
011200910806     C     *IN30         DOWEQ     '0'
011300940725     C*
011400941220     C                   Z-ADD     BTTFLS        VSCFLS                         FIL.SEGNACOLLO
011500940725     C                   Z-ADD     BTTNSC        VSCNSC                         NR.COLLI
011600940725     C*
011700920811     C* CODICE ANOMALIA
011800940722     C                   MOVEL     *BLANKS       VSDCAN
011900940722     C     BTTCAN        IFNE      ' '
012000940722     C                   MOVEL     BTTCAN        KEY
012100940722     C     KTAB1         CHAIN     TABEL00F                           31
012200120709     C  N31              MOVEL     TBLUNI        ds3e
012300120709     C   31              clear                   ds3e
012400940722     C                   MOVEL     ' -'          VSDCAN
012500940722     C                   MOVEL     BTTCAN        VSDCAN
012600120711     c                   eval      %subst(vsdcan:3:20)=§3edes
012700920811     C                   END
012800910912     C* GIRO DATE:
012900910912     C* PARTENZA MERCE
013000940722     C                   MOVEL     BTTDFV        G02INV
013100940722     C                   Z-ADD     *ZEROS        G02DAT
013200940722     C                   MOVEL     '3'           G02ERR
013300940722     C                   CALL      'XSRDA8'
013400940722     C                   PARM                    WLBDAT
013500940722     C                   MOVEL     G02DAT        VSCDFV
013600940722     C     BTTDFV        IFGT      0
013700940722     C                   MOVEL     BTTNPP        VSCNPP
013800930111     C                   ELSE
013900940722     C                   MOVEL     '  '          VSCNPP
014000930111     C                   END
014100910912     C* ARRIVO AL TRANSITO
014200940722     C                   MOVEL     BTTDET        G02INV
014300940722     C                   Z-ADD     *ZEROS        G02DAT
014400940722     C                   MOVEL     '3'           G02ERR
014500940722     C                   CALL      'XSRDA8'
014600940722     C                   PARM                    WLBDAT
014700940722     C                   MOVEL     G02DAT        VSCDET
014800940722     C     BTTDET        IFGT      0
014900940722     C                   MOVEL     BTTPET        VSCPET
015000930111     C                   ELSE
015100940722     C                   MOVEL     '  '          VSCPET
015200930111     C                   END
015300910912     C* USCITA DAL TRANSITO
015400940722     C                   MOVEL     BTTDUT        G02INV
015500940722     C                   Z-ADD     *ZEROS        G02DAT
015600940722     C                   MOVEL     '3'           G02ERR
015700940722     C                   CALL      'XSRDA8'
015800940722     C                   PARM                    WLBDAT
015900940722     C                   MOVEL     G02DAT        VSCDUT
016000940722     C     BTTDUT        IFGT      0
016100940722     C                   MOVEL     BTTPUT        VSCPUT
016200930111     C                   ELSE
016300940722     C                   MOVEL     '  '          VSCPUT
016400930111     C                   END
016500910912     C* ARRIVO MERCE ALLA FIL ARRIVO
016600160517     c                   if        bttdam=0
016700160517     c     kart          chain     fnart01l
016800160517     c                   if        %found(fnart01l)
016900160517     c                   eval      bttdam=artdam
017000160517     c                   endif
017100160517     c                   endif
017200940722     C                   MOVEL     BTTDAM        G02INV
017300940722     C                   Z-ADD     *ZEROS        G02DAT
017400940722     C                   MOVEL     '3'           G02ERR
017500940722     C                   CALL      'XSRDA8'
017600940722     C                   PARM                    WLBDAT
017700940722     C                   MOVEL     G02DAT        VSCDAM
017800930115     C*
017900930115     C* VEDO SE VOLUME INSERITO MANUALMENTE
018000940725     C                   MOVEL     '   '         VSDFVC
018100990913     C                   MOVEL     ' '           VSDFPC
018200940722     C     BTTVUC        IFGT      0
018300970424     C                   SELECT
018400970424     C     BTTFVC        WHENEQ    'M'                                          'M'=MANUALE
018500940725     C                   MOVEL     'MAN'         VSDFVC
018600970424     C     BTTFVC        WHENEQ    'P'                                          'M'=MANUALE
018700970424     C                   MOVEL     'PAR'         VSDFVC
018800970424     C     BTTFVC        WHENEQ    'T'                                          'M'=MANUALE
018900970424     C                   MOVEL     'TRA'         VSDFVC
019000970424     C                   ENDSL
019100970424     C                   END
019200940722     C                   MOVEL     BTTVUC        VSCVUC
019300990913     C     BTTPUC        IFGT      0
019400990913     C                   MOVE      BTTFPC        VSDFPC
019500990913     C                   ENDIF
019600990907     C                   MOVEL     BTTPUC        VSCPUC
019700910912     C*
019800910205     C* SCRIVO SUBFILE
019900940722     C                   ADD       1             NRR1
020000940722     C                   WRITE     LRA8S01
020100910912     C*
020200020822     C     KBTT          READE     FNBTT11L                               30
020300910205     C                   END
020400910205     C*
020500921013     C* SE PIU' DI UN COLLO MEMORIZZO ANCHE AL
020600940725     C     BTTNSC        IFNE      VI1NCD
020700940725     C                   MOVEL     BTTNSC        VI1NCA
020800921013     C                   END
020900921104     C*
021000940722     C     NRR1          IFEQ      0
021100941220     C                   Z-ADD     0             VSCFLS
021200940725     C                   Z-ADD     0             VSCNSC
021300940722     C                   Z-ADD     0             VSCDFV
021400940722     C                   MOVEL     '   '         VSCNPP
021500940725     C                   Z-ADD     0             VSCDET
021600940722     C                   MOVEL     '   '         VSCPET
021700940725     C                   Z-ADD     0             VSCDUT
021800940722     C                   MOVEL     '   '         VSCPUT
021900940722     C                   Z-ADD     0             VSCDAM
022000940725     C                   MOVEL     *BLANKS       VSDCAN
022100940725     C                   Z-ADD     0             VSCVUC
022200990907     C                   Z-ADD     0             VSCPUC
022300940725     C                   MOVEL     *BLANKS       VSDFVC
022400990913     C                   MOVEL     *BLANKS       VSDFPC
022500940722     C                   ADD       1             NRR1
022600940722     C                   WRITE     LRA8S01
022700921104     C                   END
022800910205     C*
022900910205     C                   ENDSR
023000940722     C**************************************************************************
023100940722     C*   R O U T I N E    I N I Z I A L E
023200940722     C**************************************************************************
023300940722     C     *INZSR        BEGSR
023400940722     C*
023500940722     C     KBTT          KLIST
023600020822     C                   KFLD                    PARPOT
023700020822     C                   KFLD                    PARAAS
023800940722     C                   KFLD                    PARLNP
023900940722     C                   KFLD                    PARNRS
024000940722     C                   KFLD                    PARNSP
024100160517     C     Kart          KLIST
024200160517     C                   KFLD                    bttAAS
024300160517     C                   KFLD                    bttLNP
024400160517     C                   KFLD                    bttNRS
024500160517     C                   KFLD                    bttNSP
024600160517     C                   KFLD                    bttNSC
024700940722     C     KTAB1         KLIST                                                  TABEL00F
024800940722     C                   KFLD                    CODUT
024900940722     C                   KFLD                    COD
025000940722     C                   KFLD                    KEY
025100940722     C*
025200940722     C     *LIKE         DEFINE    TBLCOD        COD
025300940722     C     *LIKE         DEFINE    TBLKEY        KEY
025400940722     C*
025500940722     C                   Z-ADD     1             CODUT             1 0
025600940722     C*
025700940722     C                   ENDSR
