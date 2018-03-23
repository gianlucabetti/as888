000100930810     H DECEDIT('0,') DATEDIT(*DMY.)
000200930810     H* FLFG76R *----------------------------------------------------*
000300930810     H*                                                              *
000400930810     H*  - STATISTICA GIACENZE IN PARTENZA - CONTROLLO UFF. GIAC. -  *
000500930810     H*                     *- LANCIO -*                             *
000600930810     H*--------------------------------------------------------------*
000700940224     H*  40 = LETTURA TABEL                                          *
000800940224     H*  50 = LETTURA FLFG761L                                       *
000900940224     H*--------------------------------------------------------------*
001000930810     FTABEL00F  IF   E           K DISK
001100050301     FFLFG765l1 IF   E           k DISK
001200930810     F* DS *-------------------------------------------------------------------*
001300930810     D PARAM           DS
001400941013     D  DAL                    1      8  0
001500941013     D  AL                     9     16  0
001600941013     D  ANNO                  17     20  0
001700941013     D  PER                   21     22  0
001800941013     D  FIL                   23     25  0
001900930810     D KPJBA         E DS
002000930730     D                 DS
002100010921     D  FG7AAS                 1      4  0
002200010921     D  FG7PER                 5      6  0
002300010921     D  CHIA                   3      6  0
002400930810     D DS2I          E DS
002601050301     D tibs56ds      E DS
002602050301     d  lin                    7    756  0 dim(250)
002700930810     D*-------------
002800930810     D* DS PER ROUTINE 'XSRDAT'
002900930810     D*-------------
003000941013     D WLBDA8          DS
003100941013     D  G02DAT                 1      8  0
003200941013     D  G02INV                 9     16  0
003300941013     D  G02ERR                17     17
003400941013     D  G02TGI                18     22  0
003500931125     D*-------------
003600931125     D* DS PER ROUTINE 'XSRGIO'
003700931125     D*-------------
003800941013     D WGIDA8          DS
003900941013     D  GIODAT                 1      8  0
004000941013     D  GIOINV                 9     16  0
004100941013     D  GIOTGI                17     21  0
004200900423     C*---------------------------------------------------------------*
004300050406     C* PRELEVO ULTIMO PERIODO ELABORATO del primo terminal
004500050406     C     001           SETGT     FLFG765l1
004700050406     C     001           READPe    FLFG765l1
006000930810     C* CON L'ULTIMO PERIODO MEMORIZZATO IN CHIAVE MI POSIZIONO SUL
006100930810     C* PERIODO SUCCESSIVO E GUARDO SE DEVO LANCIARE L'ELABORAZIONE
006200930810     C                   MOVEL     *BLANKS       KEY
006700000104     C                   MOVEL     CHIA          KEY
006800930810     C     KTAB          SETGT     TABEL00F                           40
006900931115     C  N40KTAB1         READE     TABEL                                  40
007000931115     C     *IN40         DOWEQ     '0'
007100931123     C     TBLFLG        ANDNE     *BLANKS
007200931115     C     KTAB1         READE     TABEL                                  40
007300931115     C                   END
007400931115     C  N40              MOVEL     TBLUNI        DS2I
007500940131     C                   Z-ADD     §2IAL         G02INV
007600940131     C                   MOVEL     '3'           G02ERR
007700941013     C                   CALL      'XSRDA8'
007800941013     C                   PARM                    WLBDA8
007900940131     C     G02TGI        ADD       1             GIOTGI
008000941013     C                   CALL      'XSRGI8'
008100941013     C                   PARM                    WGIDA8
008200941013     C                   Z-ADD     GIOINV        §ALGG1            8 0
008300931125     C* SE DATA ODIERNA MAGGIORE DATA FINALE PERIODO + 1 GIORNO
008400931125     C* SOTTOMETTO L'ELABORAZIONE
008500940131     C  N40DATEU         IFGT      §ALGG1
008600941013     C                   Z-ADD     §2IDAL        DAL
008700941013     C                   Z-ADD     §2IAL         AL
008800950119     C                   MOVEL     TBLKEY        ANNO2             2 0
008900950119     C                   Z-ADD     ANNO2         ANNO
009000941013     C*  Converto le date nel formato a 8
009100941013     C     ANNO          IFGT      60
009200941013     C                   ADD       19000000      DAL
009300941013     C                   ADD       19000000      AL
009400941013     C                   ADD       1900          ANNO
009500941013     C                   ELSE
009600941013     C                   ADD       20000000      DAL
009700941013     C                   ADD       20000000      AL
009800941013     C                   ADD       2000          ANNO
009900941013     C                   END
010000930811     C                   MOVEL     TBLKEY        CHIA
010100930810     C                   Z-ADD     FG7PER        PER
010101050406     c* loop su tutti i po
010102050406     c                   do        250           i                 3 0
010103050406     c*
010104050406     c                   if        lin(i) = 0
010105050406     c                   leave
010106050406     c                   endif
010107050406     c*
010108050406     c                   if        lin(i) = 046
010109050406     c                   iter
010110050406     c                   endif
010111050406     c                   eval      fil = lin(i)
010200930810     C                   MOVEL     PARAM         KPJBU
010300930810     C* ESTRAZIONE
010400941013     C                   CALL      'FNLG77R'
010500930810     C                   PARM                    KPJBA
010901050301     C                   ENDDO
010902050406     C                   ENDIF
011000930810     C*
011200930804     C                   SETON                                        LR
011300930804     C*------------------------------------------------------------------------*
011400930803     C* SUBROUTINE INIZIALE
011500930803     C*-----------
011600930803     C     *INZSR        BEGSR
011700941013     C*
011800930803     C     *ENTRY        PLIST
011900930803     C                   PARM                    KPJBA
011901050301     C                   Z-ADD     1             CODUT             1 0
012301050301     C* CARICO I SIMFEL dell'as
012303050301     c                   clear                   tibs56ds
012304050301     c                   eval      i56ppo=046
012305050301     c                   eval      i56mod='AZI'
012306050301     c                   call      'TIBS56R'
012307050301     c                   parm                    tibs56ds
012400930810     C                   MOVEL     '2I'          COD
012500930810     C                   Z-ADD     0             FIL
012600930810     C** GIRO LA DATA DEL GIORNO
012700941013     C                   TIME                    WHHDAT           14 0
012800941013     C                   MOVE      WHHDAT        G02DAT
012900941013     C                   CALL      'XSRDA8'
013000941013     C                   PARM                    WLBDA8
013100941013     C                   Z-ADD     G02INV        DATEU             8 0
013200000111     C                   Z-ADD     G02INV        OGGI6             6 0
013300930811     C** CHIAVI DI TABEL
013400930811     C     KTAB          KLIST
013500930803     C                   KFLD                    CODUT
013600930803     C                   KFLD                    COD               2
013700930803     C                   KFLD                    KEY               8
013800930810     C*
013900930810     C     KTAB1         KLIST
014000930810     C                   KFLD                    CODUT
014100930810     C                   KFLD                    COD
014200930811     C*
014300930803     C                   ENDSR
