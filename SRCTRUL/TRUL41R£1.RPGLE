000100930511     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200941005     FTABEL00F  IF   E           K DISK
000300941005      *
000400941005     FFNBLP01L  IF   E           K DISK
000500941005      *
000600941005     FFNARB00F  O    E           K DISK
000700941005      *
000800941005     D WLBDA8          DS
000900941005     D  G08DAT                 1      8  0
001000941005     D  G08INV                 9     16  0
001100941005     D  G08ERR                17     17
001200941005     D  G08TGI                18     22  0
001300941005     D DS3A          E DS
001400020603     D DSBL4A        E DS
001500941005     D DSUL41        E DS                  EXTNAME(TRUL41DS)
001600941005     C     KNWB          CHAIN     FNBLP01L                           21
001700941005     C  N21              DO
001800941005     C                   MOVEL     *BLANK        ARBATB
001900941005     C                   MOVEL     BLPAAS        ARBAAS
002000941005     C                   MOVEL     BLPLNP        ARBLNP
002100941005     C                   MOVEL     BLPNRS        ARBNRS
002200941005     C                   MOVEL     BLPNSP        ARBNSP
002300941005     C                   MOVEL     BLPMGS        ARBMGS
002400941005     C                   MOVEL     BLPCBO        ARBCBO
002500941005     C                   MOVEL     BLPFNS        ARBFNS
002600941005     C                   MOVEL     BLPSCL        ARBSCL
002700941005     C                   MOVEL     BLPFBR        ARBFBR
002800941005     C                   MOVEL     BLPFDN        ARBFDN
002900941005     C                   MOVEL     BLPLNA        ARBLNA
003000941005     C                   MOVEL     BLPZNC        ARBZNC
003100941005     C                   MOVEL     BLPKSC        ARBKSC
003200941005     C                   MOVEL     BLPRSM        ARBRSM
003300941005     C                   MOVEL     BLPINM        ARBINM
003400941005     C                   MOVEL     BLPCAM        ARBCAM
003500941005     C                   MOVEL     BLPLOM        ARBLOM
003600941005     C                   MOVEL     BLPPRM        ARBPRM
003700941005     C                   MOVEL     BLPFAP        ARBFAP
003800941005     C                   MOVEL     BLPNZM        ARBNZM
003900061031     C**!!!              MOVEL     BLPCPI        ARBCPI
004000941005     C                   MOVEL     BLPRSD        ARBRSD
004100941005     C                   MOVEL     BLPIND        ARBIND
004200941005     C                   MOVEL     BLPCAD        ARBCAD
004300941005     C                   MOVEL     BLPLOD        ARBLOD
004400941005     C                   MOVEL     BLPPRD        ARBPRD
004500941005     C                   MOVEL     BLPFIN        ARBFIN
004600941005     C                   MOVEL     BLPNZD        ARBNZD
004700941005     C                   MOVEL     BLPGC1        ARBGC1
004800941005     C                   MOVEL     BLPGC2        ARBGC2
004900941005     C                   MOVEL     BLPCTR        ARBCTR
005000941005     C                   MOVEL     BLPCTS        ARBCTS
005100941005     C                   MOVEL     BLPFTM        ARBFTM
005200941005     C                   MOVEL     BLPTSP        ARBTSP
005300941005     C                   MOVEL     BLPNAS        ARBNAS
005400941005     C                   MOVEL     BLPNCL        ARBNCL
005500941005     C                   MOVEL     BLPNCP        ARBNCP
005600941005     C                   MOVEL     BLPPKC        ARBPKC
005700941005     C                   MOVEL     BLPPKB        ARBPKB
005800941005     C                   MOVEL     BLPPKF        ARBPKF
005900941005     C                   MOVEL     BLPNCR        ARBNCR
006000941005     C                   MOVEL     BLPVLC        ARBVLC
006100941005     C                   MOVEL     BLPFVB        ARBFVB
006200941005     C                   MOVEL     BLPVLB        ARBVLB
006300941005     C                   MOVEL     BLPFVF        ARBFVF
006400941005     C                   MOVEL     BLPVLF        ARBVLF
006500941005     C                   MOVEL     BLPQFT        ARBQFT
006600941005     C                   MOVEL     BLPVMD        ARBVMD
006700941005     C                   MOVEL     BLPVAD        ARBVAD
006800941005     C                   MOVEL     BLPIAS        ARBIAS
006900941005     C                   MOVEL     BLPVAS        ARBVAS
007000941005     C                   MOVEL     BLPCCM        ARBCCM
007100941005     C                   MOVEL     BLPRMN        ARBRMN
007200941005     C                   MOVEL     BLPRMA        ARBRMA
007300941005     C                   MOVEL     BLPRMO        ARBRMO
007400941005     C                   MOVEL     BLPFLS        ARBFLS
007500941005     C                   MOVEL     BLPNCD        ARBNCD
007600941005     C                   MOVEL     BLPNCA        ARBNCA
007700941005     C                   MOVEL     BLPXCO        ARBXCO
007800941005     C                   MOVEL     BLPFFD        ARBFFD
007900941005     C                   MOVEL     BLPTCR        ARBTCR
008000941005     C                   MOVEL     BLPDCR        ARBDCR
008100941005     C                   MOVEL     BLPHCR        ARBHCR
008200941005     C                   Z-ADD     BLPDTI        ARBDTI
008300941005     C                   Z-ADD     BLPHTI        ARBHTI
008400941005     C                   Z-ADD     BLPDCE        ARBDCE
008500941005     C                   Z-ADD     BLPNPI        ARBNPI
008600941005     C                   Z-ADD     BLPNCI        ARBNCI
008700941005     C                   Z-ADD     BLPNRC        ARBNRC
008800941005     C                   MOVE      BLPDBR        ARBDBR
008900941005     C                   MOVE      BLPNFV        ARBNFV
009000941005     C                   MOVE      BLPDPC        ARBDPC
009100941005     C                   MOVE      BLPDUC        ARBDUC
009200941005     C                   MOVEL     BLPFLP        ARBFLP
009300941005     C                   MOVE      *ZEROS        ARBDET
009400941005     C                   MOVE      *ZEROS        ARBDUT
009500941005     C                   MOVE      *ZEROS        ARBDAM
009600941005     C                   MOVE      *ZEROS        ARBPDC
009700941005     C                   MOVE      *ZEROS        ARBNDC
009800941005     C                   MOVE      *ZEROS        ARBDDC
009900941005     C                   MOVE      *ZEROS        ARBNTC
010000941005     C                   MOVE      *ZEROS        ARBDCP
010100941005     C                   MOVE      *ZEROS        ARBDCM
010200941005     C                   MOVE      *ZEROS        ARBHMC
010300941005     C                   MOVEL     BLPTC1        ARBTC1
010400941005     C                   MOVEL     BLPTC2        ARBTC2
010500941005     C                   MOVEL     *BLANK        ARBFAN
010600941005     C                   MOVEL     *BLANKS       ARBCCA
010700941005     C                   MOVEL     *BLANK        ARBTMC
010800941005     C                   MOVEL     *BLANK        ARBCMC
010900941005     C                   MOVEL     BLPGGA        ARBGGA
011000941005     C                   MOVEL     BLPGMA        ARBGMA
011100941005     C                   MOVEL     BLPGVA        ARBGVA
011200941005     C                   MOVEL     *BLANK        ARBFBS
011300941005     C                   MOVEL     *ZEROS        ARBSTP
011400941005     C                   MOVEL     *ZEROS        ARBNGD
011500941005     C                   MOVEL     *ZEROS        ARBNSS
011600941005     C                   MOVE      *ZEROS        ARBAMP
011700941005     C                   MOVE      3             ARBAMA
011800941005     C                   MOVEL     'S'           ARBACC
011900941005     C                   MOVEL     *BLANKS       ARBACA
012000941005     C                   MOVEL     *BLANK        ARBICC
012100941005     C                   MOVEL     *BLANK        ARBICA
012200941005     C                   MOVEL     *BLANK        ARBMIF
012300941005     C                   MOVEL     *ZEROS        ARBIFP
012400941005     C                   MOVEL     *BLANK        ARBFIP
012500941005     C                   MOVEL     *ZEROS        ARBIPP
012600941005     C                   MOVEL     *BLANK        ARBFBC
012700941005     C                   MOVEL     *ZEROS        ARBCUC
012800941005     C                   MOVEL     *BLANK        ARBFDC
012900941005     C                   MOVEL     *BLANK        ARBFNC
013000941005     C                   MOVEL     *ZEROS        ARBNMI
013100941005     C                   MOVEL     *ZEROS        ARBDMI
013200941005     C                   MOVEL     BLPFLE        ARBFLE
013300941005     C                   MOVEL     BLPTFP        ARBTFP
013400941005     C                   MOVEL     BLPFEA        ARBFEA
013500941005     C                   MOVEL     BLPTFA        ARBTFA
013600941005     C                   MOVEL     'R'           ARBFT1
013700941005     C                   Z-ADD     AMGUDT        ARBDT1
013800941005     C                   MOVEL     *BLANKS       ARBFT2
013900941005     C                   Z-ADD     0             ARBDT2
014000941005     C                   MOVEL     *BLANKS       ARBFT3
014100941005     C                   Z-ADD     0             ARBDT3
014200941005     C* FLAG ABILITAZIONE CONSEGNA ASSEGNATO
014300941005     C                   MOVEL     *BLANKS       ARBACA
014400941005     C                   MOVEL     '3A'          TBLCOD
014500941005     C                   MOVEL     *BLANKS       TBLKEY
014600941005     C                   MOVEL     ARBCBO        TBLKEY
014700941005     C     KTAB          CHAIN     TABEL                              21
014800941005     C  N21              MOVEL     TBLUNI        DS3A
014900941005     C   21              MOVEL     'F1'          §3ATB1
015000941005     C                   MOVEL     §3ATB1        D41TPB            1
015100941005     C     D41TPB        IFEQ      'F'                                              FRANCO
015200941005     C     §3ATB2        IFEQ      '  '                                             2° BOLLA
015300941005     C                   MOVEL     'S'           ARBACA
015400941005     C                   END
015500941005     C                   ELSE
015600941005     C                   MOVEL     ' '           ARBACA
015700941005     C                   END
015800061031      * prendo la partita iva/codice fiscale solo se non è bolla doppia
015900061031     c                   if        §3atb2 = *blanks
016000061031     c                   eval      arbcpi = blpcpi
016100061031     c                   endif
016200941005     C*
016300941005     C                   WRITE     FNARB000
016400060214     C                   ENDDO
016500060214     C*
016600060214     C                   SETON                                        LR
016700060214     C*------------------------------------------------------------
016800941005     C     *INZSR        BEGSR
016900060214     C*------------------------------------------------------------
017000941005     C     *ENTRY        PLIST
017100941005     C                   PARM                    DSUL41
017200941005     C* PASS. PARAMETRI DA PGM CHIAMANTE
017300941005      *
017400941005      * UDATE
017500941005     C                   TIME                    WHHUDT           14 0
017600941005     C                   MOVE      WHHUDT        G08DAT
017700941005     C                   MOVE      *ZEROS        G08INV
017800941005     C                   MOVEL     *BLANK        G08ERR
017900941005     C                   CALL      'XSRDA8'
018000941005     C                   PARM                    WLBDA8
018100941005     C                   Z-ADD     G08INV        AMGUDT            8 0
018200941005     C                   Z-ADD     G08DAT        GMAUDT            8 0
018300941005     C                   MOVEL     AMGUDT        ANNO              4 0
018400941005     C                   MOVEL     WHHUDT        ORAMIN            4 0
018500941005     C*
018600941005     C                   Z-ADD     1             CODUT             1 0
018700941005     C     KTAB          KLIST
018800941005     C                   KFLD                    CODUT
018900941005     C                   KFLD                    TBLCOD
019000941005     C                   KFLD                    TBLKEY
019100941005     C*
019200941005     C     KNWB          KLIST
019300941005     C                   KFLD                    D41AAS
019400941005     C                   KFLD                    D41LNP
019500941005     C                   KFLD                    D41NRS
019600941005     C                   KFLD                    D41NSP
019700941005     C*
019800941005     C     KNW4          KLIST
019900941005     C                   KFLD                    D41AAS
020000941005     C                   KFLD                    D41LNP
020100941005     C                   KFLD                    D41NRS
020200941005     C                   KFLD                    D41NSP
020300020603     C*
020400941005     C                   ENDSR
