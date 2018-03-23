000100960117     H DECEDIT('0,') DATEDIT(*DMY.)
000200960117     H* FNLR80R1*----------------------------------------------------*
000300960117     H*-----------------*                                            *
000400960117     H*        - PULIZIA ARCHIVIO X STAMPA ELENCO BOLLE CON  -       *
000500960117     H*        - OBBLIGO RESTITUZIONE BAM/LETTERA DI VETTURA -       *
000600960117      *--------------------------------------------------------------*
000700960117     FFNARBX1L  UF   E           K DISK
000800960117     FTABEL00F  IF   E           K DISK
000900960117      *--------------------------------------------------------------*
001000960117      *   D s                                                        *
001100960117      *--------------------------------------------------------------*
001200960117     D FNLR80        E DS                  EXTNAME(FNLR80DS)
001300970718     D DS5A2         E DS
001400960117     D*  DS  per controllo/conversione data
001500960117     D WLBDA8          DS
001600960117     D  G02DAT                 1      8  0
001700960117     D  G02INV                 9     16  0
001800960117     D  G02ERR                17     17
001900960117     D  G02TGI                18     22  0
002000960117     D WGIDAT          DS
002100960117     D  GI8DAT                 1      8  0
002200960117     D  GI8INV                 9     16  0
002300960117     D  GI8TGI                17     21  0
002400960117     D CNCR80        E DS
002500960117     D KPJBA         E DS
002600960117     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002700960117      *--------------------------------------------------------------*
002800960117      *   CICLO PRINCIPALE                                           *
002900960117      *--------------------------------------------------------------*
003000960117      *  Provvedo alla pulizia dei dati delle bolle fino a quando la
003100960117      *  data di stampa è minore/uguale alla data del giorno - gg.pul.
003200021011      *  OPPURE SE LA DATA DELLE DISTINTA È MAGGIORE DI 30 GG
003300021011     C*          KARB1     SETGTFNARBX1L
003400021011     C     D80FGS        SETLL     FNARBX1L
003500021011     C     D80FGS        READE     FNARBX1L                               30
003600960117     C     *IN30         DOWEQ     '0'
003700110224     c*                  if        arbtpl = ' ' or arbtpl = 'C'
003800021011     C     ARBDST        IFGT      0
003900021011     C     ARBDST        ANDLE     WDTPUL
004000021011     C     ARBDST        oreq      0
004100021011     C     ARBDDC        andlt     WDTDDC
004200960117     C                   DELETE    FNARBX1L
004300021011     C                   END
004400110224     C*                  endif
004500021011     C     D80FGS        READE     FNARBX1L                               30
004600960117     C                   END
004700960117      *
004800960117     C     FINE          TAG
004900960117     C                   SETON                                        LR
005000960117      *--------------------------------------------------------------*
005100960117      *   Operazioni iniziali                                        *
005200960117      *--------------------------------------------------------------*
005300960117     C     *INZSR        BEGSR
005400960117      *
005500960117     C     *ENTRY        PLIST
005600960117     C                   PARM                    KPJBA
005700960117     C                   MOVEL     KPJBU         FNLR80
005800960117      *  Call a XPARUT
005900960117     C                   Z-ADD     1             CODUT
006000960117     C                   CALL      'X§PARUT'
006100960117     C                   PARM                    UT§DSE
006200960117     C     CODUT         CABEQ     -1            FINE
006300960117     C                   MOVEL     RAGUT         RSUT             20
006400960117     C                   MOVEL     REC80         CNCR80
006500960117      *----------------------------------------------*
006600960117      *  Definizione chiavi di accesso
006700960117     C     KTAB          KLIST
006800960117     C                   KFLD                    KKUT
006900960117     C                   KFLD                    KCOD
007000960117     C                   KFLD                    KKEY
007100960117      *----------------------------------------------*
007200960117      *  Definizione variabili
007300960117     C     *LIKE         DEFINE    TBLKUT        KKUT
007400960117     C     *LIKE         DEFINE    TBLCOD        KCOD
007500960117     C     *LIKE         DEFINE    TBLKEY        KKEY
007600960117      *----------------------------------------------*
007700960117      *  Calcolo la data del giorno
007800960117     C                   TIME                    WHHDAT           14 0
007900960117     C                   CLEAR                   WLBDA8
008000960117     C                   Z-ADD     WHHDAT        G02DAT
008100960117     C                   CALL      'XSRDA8'
008200960117     C                   PARM                    WLBDA8
008300960117     C                   Z-ADD     G02INV        WOGGI             8 0
008400960117     C                   Z-ADD     G02TGI        WGIOR             5 0
008500960117      *  Carico gg. per controllo data pulizia
008600960117     C                   Z-ADD     1             KKUT
008700970718     C                   MOVEL     '5A'          KCOD
008800960117     C                   MOVEL     *BLANKS       KKEY
008900970718     C                   MOVEL     '2'           KKEY
009000960117     C     KTAB          CHAIN     TABEL00F                           30
009100960117     C     *IN30         IFEQ      '0'
009200960117     C     TBLFLG        ANDEQ     ' '
009300970718     C                   MOVEL     TBLUNI        DS5A2
009400960117     C                   END
009500960117      *  Calcolo data limite pulizia
009600970718     C     WGIOR         SUB       §5ASTB        WGGPUL            5 0
009700960117     C                   MOVE      *ZEROS        GI8DAT
009800960117     C                   MOVE      *ZEROS        GI8INV
009900960117     C                   Z-ADD     WGGPUL        GI8TGI
010000960117     C                   CALL      'XSRGI8'
010100960117     C                   PARM                    WGIDAT
010200960117     C                   MOVE      GI8INV        WDTPUL            8 0
010300021011      *  Calcolo data MAX DELLE DISTINTE DA TENERE
010400021011     C     WGIOR         SUB       30            WGGPUL            5 0
010500021011     C                   MOVE      *ZEROS        GI8DAT
010600021011     C                   MOVE      *ZEROS        GI8INV
010700021011     C                   Z-ADD     WGGPUL        GI8TGI
010800021011     C                   CALL      'XSRGI8'
010900021011     C                   PARM                    WGIDAT
011000021011     C                   MOVE      GI8INV        WDTDDC            8 0
011100960117      *
011200960117     C                   ENDSR
