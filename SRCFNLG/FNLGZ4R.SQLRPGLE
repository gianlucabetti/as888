000100000414     H* FNLGZ4R *----------------------------------------------------*
000200000000     H*-----------------*                                            *
000300000414     H* - RICHIESTA PULIZIA STORICO RICEZIONE DISPOSIZIONI GIACENZE -*
000400000414     H* -                      DA CLIENTI                           -*
000500940224     H*--------------------------------------------------------------*
000600051007     FTABEL00F  IF   E           K DISK
000700941019     I*--------------------------------------------------------------*
000800051007     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
000900051007     D WLBDA8          DS
001000051007     D  G02DAT                 1      8  0
001100051007     D  G02INV                 9     16  0
001200051007     D  G02ERR                17     17
001300051007     D  G02TGI                18     22  0
001400051007     D WGIDAT          DS
001500051007     D  GI8DAT                 1      8  0
001600051007     D  GI8INV                 9     16  0
001700051007     D  GI8TGI                17     21  0
001800051007     D DS5A2         E DS
001900051007     D KPJBA         E DS
002000051007     D  LIBSYS                92    101
002100051007     D  MSGERR               453    502
002200900131     C*---------------------------------------------------------------*
002300051007     C/EXEC SQL
002400080326     C+ DELETE FROM FNVAR10S WHERE vardrd <= :datpul
002500051007     C/END-EXEC
002600051007     C     FINE          TAG
002700051007     C                   SETON                                        LR
002800051007     C*-----------------------------------------------------*
002900051007     C*  OPERAZIONI INIZIALI
003000051007     C*-----------------------------------------------------*
003100051007     C     *INZSR        BEGSR
003200051007     C*
003300051007     C     *ENTRY        PLIST
003400051007     C                   PARM                    KPJBA
003500051007     C                   Z-ADD     1             CODUT
003600051007     C                   CALL      'XPARUT'
003700051007     C                   PARM                    UTEDSE
003800051007     C     CODUT         CABEQ     -1            FINE
003900051007     C                   MOVEL     RAGUT         RSUT             20
004000051007     C*-----------------------------------------------------*
004100051007     C     *LIKE         DEFINE    G02TGI        GGUDAT
004200051007     C     *LIKE         DEFINE    G02TGI        GGDELT
004300051007     C     *LIKE         DEFINE    G02TGI        SAVTGI
004400051007     C*-----------------------------------------------------*
004500051007     C     KTAB          KLIST
004600051007     C                   KFLD                    CODUT
004700051007     C                   KFLD                    COD               2
004800051007     C                   KFLD                    KEY               8
004900051007     C                   MOVEL     '5A'          COD
005000051007     C                   MOVEL     *BLANKS       KEY
005100051007     C                   MOVEL     '2'           KEY
005200051007     C*----------------------------------------------------*
005300051007     C     KTAB          CHAIN     TABEL                              10
005400051007     C  N10              MOVEL     TBLUNI        DS5A2
005500051007     C  N10              Z-ADD     §5AGEG        GGPUL             3 0
005600051007     C   10              Z-ADD     40            GGPUL
005700051007     C*
005800051007     C                   CLEAR                   WLBDA8
005900051007     C                   CLEAR                   WGIDAT
006000051007     C* Data pulizia giacenze in partenza
006100051007     C                   TIME                    WHHDAT           14 0
006200051007     C                   MOVE      WHHDAT        G02DAT
006300051007     C                   MOVEL     *BLANK        G02ERR
006400051007     C                   CALL      'XSRDA8'
006500051007     C                   PARM                    WLBDA8
006600051007     C                   Z-ADD     G02TGI        GGUDAT
006700051007     C                   MOVE      G02TGI        SAVTGI
006800051007     C     G02TGI        SUB       GGPUL         G02TGI
006900051007     C                   MOVE      *ZEROS        GI8DAT
007000051007     C                   MOVE      *ZEROS        GI8INV
007100051007     C                   Z-ADD     G02TGI        GI8TGI
007200051007     C                   CALL      'XSRGI8'
007300051007     C                   PARM                    WGIDAT
007400051007     C                   MOVE      GI8INV        DATPUL            8 0
007500051007     C*
007600051007     C                   ENDSR
