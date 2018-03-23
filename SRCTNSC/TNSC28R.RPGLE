000100931025     H DECEDIT('0,') DATEDIT(*DMY.)
000200931025?     *--------------------------------------------------------------*
000300941104      * TNSC28R                                                      *
000400931025      *       *-----------------------------------------*            *
000500931025      *       *  PULIZIA ARCHIVIO CONTRASSEGNI 3 MESI   *            *
000600931025      *       *  DA AVVENUTO PAGAMENTO: ESECUZIONE      *            *
000700931025      *       *-----------------------------------------*            *
000800931025      *                                                              *
000900931025?     *--------------------------------------------------------------*
001000941104      * 70 -CHAIN SU FNBLP01L                                        *
001100941104      * 80 -READ  SU FNCSB02L                                        *
001200940218?     *--------------------------------------------------------------*
001300941104     FFNCSB02L  UF   E           K DISK
001400940112?     *-----------
001500941104     FFNBLP01L  IF   E           K DISK
001600021028     FTABEL00F  IF   E           K DISK    USROPN
001700931025?     *--------------------------------------------------------------*
001800021028     D WLBDA8          DS
001900021028     D  G02DAT                 1      8  0
002000021028     D  G02INV                 9     16  0
002100021028     D  G02ERR                17     17
002200021028     D  G02TGI                18     22  0
002300021028     D WLBGIO          DS
002400021028     D  GIODAT                 1      8  0
002500021028     D  GIOINV                 9     16  0
002600021028     D  GIOTGI                17     21  0
002700021028     D DS5A          E DS
002800931021     D KPJBA         E DS
002900931021     C*--------------------------------------------------------------*
003000931025     C*  DELETE RECORD DA PAGATI FINO A 3 MESI DA UDATE              *
003100931021     C*--------------------------------------------------------------*
003200931021     C     *ENTRY        PLIST
003300931021     C                   PARM                    KPJBA
003400021028     C* CALCOLA LA DATA PER LA PULIZIA
003500021028     C                   EXSR      DATPUL
003600931021     C*
003700941104     C* FNCSB02L
003800931025     C     KCSB          KLIST
003900941104     C                   KFLD                    CSWDDP            8 0
004000940112     C*
004100941104     C* FNBLP01L
004200940112     C     KBLP          KLIST
004300940112     C                   KFLD                    CSBAAS
004400940112     C                   KFLD                    CSBLNP
004500940112     C                   KFLD                    CSBNRS
004600940112     C                   KFLD                    CSBNSP
004700021028     C*  CHIAVE TABELLA 5A
004800021028     C     KTAB          KLIST
004900021028     C                   KFLD                    TBWKUT
005000021028     C                   KFLD                    TBWCOD
005100021028     C                   KFLD                    TBWKEY
005200021028     C     *LIKE         DEFINE    TBLKUT        TBWKUT
005300021028     C     *LIKE         DEFINE    TBLCOD        TBWCOD
005400021028     C     *LIKE         DEFINE    TBLKEY        TBWKEY
005500931025     C*
005600931025     C* PULISCO SOLO C/A PAGATI: CSBDDP > 0
005700931025     C                   MOVE      *ZERO         CSWDDP
005800941104     C     KCSB          SETGT     FNCSB02L
005900941104     C                   READ      FNCSB02L                               80
006000931021     C*
006100931025     C* FINO A FINE FILE O FINO A DATA DI PAGAMENTO > UDATE - 91 GG.
006200931025     C     CSBDDP        DOWLE     DATAA
006300931021     C     *IN80         ANDEQ     '0'
006400940112     C* CANCELLO SOLO I C/A RELATIVI ALLE BOLLE SU CUI NON ESISTE PIU'
006500940112     C* LA BOLLA
006600960918     C     CSBSTA        IFEQ      0
006700960923     C     CSBDDC        ANDGT     0
006800941104     C     KBLP          CHAIN     FNBLP01L                           70
006900940112     C     *IN70         IFEQ      '1'
007000941104     C                   DELETE    FNCSB02L
007100940112     C                   END
007200960918     C                   END
007300940112     C*
007400941104     C                   READ      FNCSB02L                               80
007500931025     C                   END
007600931021     C*
007700931025     C                   SETON                                        LR
007800021028     C*--------------------------------------------------------------*
007900021028     C*   DATPUL: CALCOLO LA DATA pulizia C/A                        *
008000021028     C*--------------------------------------------------------------*
008100021028     C     DATPUL        BEGSR
008200021028     C*
008300021028     C                   Z-ADD     1             TBWKUT
008400021028     C                   MOVE      '5A'          TBWCOD
008500021028     C                   MOVEL     *BLANKS       TBWKEY
008600021028     C                   MOVEL     '1'           TBWKEY
008700021028     C                   CLEAR                   DATAA
008800021028     C                   CLEAR                   DS5A
008900021028     C                   OPEN      TABEL00F
009000021028     C*----------------------------------------------------*
009100021028     C     KTAB          CHAIN     TABEL                              10
009200021028     C     *IN10         IFEQ      '0'
009300021028     C                   MOVEL     TBLUNI        DS5A
009400021028     C                   Z-ADD     §5AFCA        GIORCA            3 0
009500021028     C*----------------------------------------------------*
009600021028     C*  Calcolo data inizio pulizia contrassegni pagati
009700021028     C                   Z-ADD     UDATE         G02DAT
009800021028     C                   MOVEL     *BLANK        G02ERR
009900021028     C                   CALL      'XSRDA8'
010000021028     C                   PARM                    WLBDA8
010100021028     C     G02TGI        SUB       GIORCA        G02TGI
010200021028     C                   MOVE      *ZEROS        GIODAT
010300021028     C                   MOVE      *ZEROS        GIOINV
010400021028     C                   Z-ADD     G02TGI        GIOTGI
010500021028     C                   CALL      'XSRGI8'
010600021028     C                   PARM                    WLBGIO
010700021028     C                   Z-ADD     GIODAT        G02DAT
010800021028     C                   MOVEL     *BLANK        G02ERR
010900021028     C                   CALL      'XSRDA8'
011000021028     C                   PARM                    WLBDA8
011100021028     C                   Z-ADD     G02INV        DATAA             8 0
011200021028     C                   END
011300021028     C                   CLOSE     TABEL00F
011400021028     C                   ENDSR
