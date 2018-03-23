000100060410     H DECEDIT('0,') DATEDIT(*DMY.)
000200940927     H****************************************************************
000300050630     H*             - conteggia aperture giacenze -                  *
000400940927     H****************************************************************
000500050630     FTIgcp02L  IF   E           K DISK
000600050630     FTIgcp21L  IF   E           K DISK    rename(tigcp000:tigcp21)
000700050630     FTabel00f  IF   E           K DISK
000800020222     d* codici com.giac.
000900940919     D C2F             S              1    DIM(50)
001000050614     d* riservata ced
001100050614     D r2F             S              1    DIM(50)
001200050630     d* codici com.giac.
001300050705     D C2Z             S              3    DIM(150)
001400050630     d* spese riconsegna
001500050705     D s2Z             S              1    DIM(150)
001600060809     d* codici chiusura giac.
001700060809     D C2c             S              3    DIM(150)
001800060809     d* spese riconsegna chiusura
001900060809     D s2c             S              1    DIM(150)
002000050630     D fnlg09ds      E DS
002100050630     D kpjba         E DS
002200950104     D DS2Z          E DS
002300940920     D DS2F          E DS
002400060809     D DS2c          E DS
002500940914     C*--------------------------------------------------------------*
002600940914     C*  CICLO PRINCIPALE
002700940914     C*--------------------------------------------------------------*
002800940927     C     *ENTRY        PLIST
002900940927     C                   PARM                    KPJBA
003000050630     C                   MOVEL     KPJBU         FNLG09ds
003100050630     c*
003200050630     c                   do
003300050630     c* chiudo in lr
003400050630     c                   if        d09tla = 'C'
003500050630     c                   leave
003600050630     c                   end
003700050630     c*
003800050630     C                   Z-ADD     0             d09cnt
003900050630     C                   Z-ADD     0             d09ced
004000050630     C                   Z-ADD     0             d09spe
004100050630     C* controlli passaggio parametri
004200050630     c                   if        d09ngc = 0 and d09nsp = 0
004300050630     c                   eval      d09err = '1'
004400050630     c                   leave
004500050630     c                   else
004600050630     c                   clear                   d09err
004700050630     c                   end
004800050630     c*
004900050630     c                   if        d09nsp <> 0
005000050630     C     KSPE          SETll     tigcp21L
005100050630     c                   else
005200050630     C     Kgia          SETll     tigcp02L
005300050630     c                   end
005400050629     c                   do        *hival
005500050630     c                   if        d09nsp <> 0
005600050630     C     KSPE          READE     tigcp21L                               99
005700050630     c                   else
005800050630     C     Kgia          reade     tigcp02L                               99
005900050630     c                   end
006000050630     C                   IF        *in99
006100050629     c                   leave
006200050629     c                   endif
006300050630     c* numero aperture
006400050630     c                   add       1             d09cnt
006500050630     c*
006600050629     C                   Z-ADD     1             X
006700050629     C     gcpTCM        LOOKUP    C2F(X)                                 32
006800050629    1C     *IN32         IFEQ      '1'
006900050629     c     r2f(x)        andne     *blank
007000050630     c* numero aperture tecniche
007100050630     c                   add       1             d09ced
007200050629     c                   end
007300050630     C*
007400050630     C                   Z-ADD     1             X
007500050630     C     gcpcmc        LOOKUP    C2z(X)                                 32
007600080903     C                   Z-ADD     1             Xx                3 0
007700060809     c     gcpcfg        lookup    c2c                                    33
007800050630    1C     *IN32         IFEQ      '1'
007900050630     c     s2z(x)        andne     'N'
008000060410     c     gcpasg        andne     'N'
008100060809     c     s2c(xx)       andeq     *blank
008200050630     c* numero aperture x calcolo spese riconsegna
008300050630     c                   add       1             d09spe
008400050630     c                   end
008500050629     c                   enddo
008600050630     c                   enddo
008700050630     c*
008800050630     c                   movel(p)  fnlg09ds      kpjbu
008900050630     c*
009000050630     c                   if        d09tla = 'C' or d09tla = 'L'
009100050630     c                   seton                                        lr
009200050630     c                   else
009300050630     c                   seton                                        rt
009400050630     c                   end
009500940915     C*---------------------------------------------------------------*
009600940915     C*   OPERAZIONI INIZIALI                                         *
009700940915     C*---------------------------------------------------------------*
009800050630     C     *inzsr        BEGSR
009900940915     C*
010000050630     C     KSPE          KLIST
010100050630     C                   KFLD                    d09aas
010200050630     C                   KFLD                    d09lnp
010300050630     C                   KFLD                    d09nrs
010400050630     C                   KFLD                    d09nsp
010500050630     C*
010600050630     C     Kgia          KLIST
010700050630     C                   KFLD                    d09agc
010800050630     C                   KFLD                    d09fgc
010900050630     C                   KFLD                    d09ngc
011000940915     C*
011100940915     C     KTBL2         KLIST
011200050630     C                   KFLD                    tblkut
011300050630     C                   KFLD                    tblcod
011400050630     c                   z-add     1             tblkut
011500940915     C*---------------------------------------------------
011600050630     C                   MOVEA     *HIVAL        C2F
011700080903     C                   Z-ADD     0             X                 3 0
011800050630     C                   MOVEL     '2F'          tblcod
011900050630     C     KTBL2         setll     TABEL00f
012000050630     C                   DO        *hival
012100050630     C     KTBL2         READE     TABEL00f
012200050630     C                   IF        %eof(tabel00f)
012300050630     c                   leave
012400050630     c                   endif
012500050630     C     X             iflt      50
012600050630     C     TBLFLG        andeq     ' '
012700050630     C                   MOVEL     TBLUNI        DS2F
012800050630     C                   ADD       1             X
012900050630     C                   MOVEL     TBLKEY        C2F(X)
013000050630     C                   MOVEL     §2Fris        r2F(X)
013100050630     C                   END
013200050630     C                   ENDdo
013300050630     c*
013400050630     C                   MOVEA     *HIVAL        C2Z
013500050630     C                   Z-ADD     0             X
013600050630     C                   MOVEL     '2Z'          tblcod
013700050630     C     KTBL2         setll     TABEL00f
013800050630     C                   DO        *hival
013900050630     C     KTBL2         READE     TABEL00f
014000050630     C                   IF        %eof(tabel00f)
014100050630     c                   leave
014200050630     c                   endif
014300050705     C     X             iflt      150
014400050630     C     TBLFLG        andeq     ' '
014500050630     C                   MOVEL     TBLUNI        DS2Z
014600050630     C                   ADD       1             X
014700050630     C                   MOVEL     TBLKEY        C2Z(X)
014800050630     C                   MOVEL     §2znsr        s2Z(X)
014900050630     C                   END
015000050630     C                   ENDdo
015100060809     c*
015200060809     C                   MOVEA     *blank        C2c
015300060809     C                   MOVEA     *blank        s2c
015400060809     C                   Z-ADD     0             X
015500060809     C                   MOVEL     '2C'          tblcod
015600060809     C     KTBL2         setll     TABEL00f
015700060809     C                   DO        *hival
015800060809     C     KTBL2         READE     TABEL00f
015900060809     C                   IF        %eof(tabel00f)
016000060809     c                   leave
016100060809     c                   endif
016200060809     C     X             iflt      150
016300060809     C     TBLFLG        andeq     ' '
016400060809     C                   MOVEL     TBLUNI        DS2C
016500060809     C                   ADD       1             X
016600060809     C                   MOVEL     TBLKEY        C2c(X)
016700060809     C                   MOVEL     §2nospr       s2c(X)
016800060809     C                   END
016900060809     C                   ENDdo
017000050630     c                   endsr
