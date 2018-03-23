000100891030     H DFTNAME(CN0500) DATEDIT(*YMD)
000200891030     F*--------------------------------------------------------------*
000300891113     F* GESTIONE TABELLA TIPI TARIFFA
000400891030     F*----------------------* USO DEGLI INDICATORI *----------------*
000500891030     F*  01        INSERIMENTO                                       *
000600891030     F*  02        VARIAZIONE                                        *
000700891107     F*  29        CODICE OBBLIGATORIO                               *
000800891030     F*  30        DESCRIZIONE OBBLIGATORIA                          *
000900891107     F*  79        COMANDO DI ANNULLAMENTO CONSENTITO SOLO DA EDP    *
001000891030     F*--------------------------------------------------------------*
001100891113     FTRTB11D   CF   E             WORKSTN
001200891030     FTABEL00F  UF A E           K DISK
001300891030     D*--------------------------------------------------------------*
001400891030     D*                        I N P U T                             *
001500891030     D*--------------------------------------------------------------*
001600891113     D DSTR          E DS
001700891030     D KPJBA         E DS
001800891030     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
001900891030     C******************************************************
002000891030     C     *ENTRY        PLIST
002100891030     C                   PARM                    KPJBA
002200891030     C                   Z-ADD     1             CODUT
002300891030     C                   CALL      'XPARUT'
002400891030     C                   PARM                    UTEDSE
002500891030     C     CODUT         CABEQ     -1            FINE
002600891030     C                   MOVEL     RAGUT         RSUT
002700891030     C******************************************************
002800891030     C* POSIZIONAMENTO TABELLE                             *
002900891107     C     KTAB          KLIST
003000891030     C                   KFLD                    §KUT
003100891030     C                   KFLD                    §COD
003200891030     C                   KFLD                    §KEY
003300891030     C******************************************************
003400891030     C     *LIKE         DEFINE    TBLKUT        §KUT
003500891030     C     *LIKE         DEFINE    TBLCOD        §COD
003600891030     C     *LIKE         DEFINE    TBLKEY        §KEY
003700891102     C     STAR          TAG
003800891102     C                   SETOFF                                       79
003900891102     C                   MOVEL     KNMUS         PROF              3
004000891102     C     PROF          IFNE      'EDP'
004100891102     C                   SETON                                        79
004200891102     C                   END
004300891108     C                   MOVEL     *BLANKS       §DES             20
004400891107     C                   MOVEL     *BLANKS       VCOD
004500891107     C                   MOVEL     '?'           VCOD
004600891030     C     INIZIO        TAG
004700891113     C                   EXFMT     TB11D1
004800980311     C   KC              GOTO      FINE
004900891107     C     VCOD          IFEQ      *BLANKS
005000891030     C                   SETON                                        29
005100891030     C                   GOTO      INIZIO
005200891030     C                   END
005300891030     C*§  RICERCA IN TABEL00F
005400891107     C     VCOD          IFEQ      '?'
005500891030     C                   MOVEL     CODUT         §KUT
005600891113     C                   MOVEL     'TR'          §COD
005700891107     C                   MOVE      *ZEROS        VCOD
005800891030     C                   CALL      'X§TABER'
005900891030     C                   PARM                    §KUT
006000891030     C                   PARM                    §COD
006100891030     C                   PARM                    §KEY
006200891030     C                   PARM                    §DES
006300891107     C                   MOVEL     §KEY          VCOD
006400891030     C                   GOTO      INIZIO
006500891102     C                   ELSE
006600891102     C                   MOVEL     CODUT         §KUT
006700891113     C                   MOVEL     'TR'          §COD
006800891107     C                   MOVEL     VCOD          §KEY
006900891030     C                   END
007000891030     C                   SETOFF                                       0102
007100891107     C                   EXSR      PULVID
007200891107     C     KTAB          CHAIN     TABEL                              80
007300891030     C     *IN80         IFEQ      '1'
007400891030     C                   SETON                                        01
007500891030     C                   ELSE
007600891113     C                   MOVEL     TBLUNI        DSTR
007700900615     C     TBLFLG        IFEQ      '*'
007800900615     C                   SETON                                        03
007900900615     C                   ELSE
008000891030     C                   SETON                                        02
008100891030     C                   END
008200900615     C                   END
008300891030     C     SU            TAG
008400891030     C                   SETOFF                                       99
008500891113     C                   EXFMT     TB11D2
008600980311     C   KC              GOTO      FINE
008700980311     C   KL              GOTO      INIZIO
008800891030     C**--- ANNULLAMENTO
008900900615     C   KQ              DO
009000900615     C                   MOVEL     '*'           TBLFLG
009100900615     C                   UPDATE    TABEL
009200900615     C                   GOTO      INIZIO
009300891030     C                   END
009400900615     C** RIPRISTINO
009500900615     C   KE              DO
009600900615     C                   MOVEL     *BLANKS       TBLFLG
009700900615     C                   UPDATE    TABEL
009800900615     C                   GOTO      INIZIO
009900900615     C                   END
010000900615     C**
010100891102     C                   EXSR      CTRFMT
010200891102     C   99              GOTO      SU
010300891030     C                   EXSR      SCRIVI
010400891102     C                   GOTO      STAR
010500891030     C     FINE          TAG
010600891030     C                   SETON                                        LR
010700891030     C**------------------------
010800891030     C** PULISCI CAMPI VIDEO
010900891030     C**------------------------
011000891030     C     PULVID        BEGSR
011100891113     C                   MOVEL     *BLANKS       §TRDES
011200891113     C                   Z-ADD     0             §TRMTS
011300060209     C                   MOVEL     *BLANKS       §TRvAS
011400891113     C                   MOVEL     *BLANKS       §TRDE1
011500891113     C                   MOVEL     *BLANKS       §TRDE2
011600891113     C                   MOVEL     *BLANKS       §TRDE3
011700940920     C                   MOVEL     *BLANKS       §TRUTC
011800940920     C                   MOVEL     *BLANKS       §TRUTP
011900980311     C                   MOVEL     *BLANKS       §TRTAP
012000980311     C                   MOVEL     *BLANKS       §TRSAP
012100980311     C                   MOVEL     *BLANKS       §TRUTE
012200980311     C                   MOVEL     *BLANKS       §TRUPA
012300980311     C                   MOVEL     *BLANKS       §TRUCV
012400980504     C                   MOVEL     *BLANKS       §TRARR
012500980504     C                   MOVEL     *BLANKS       §TRRPV
012600940920     C                   Z-ADD     0             §TRATA
012700891030     C                   ENDSR
012800891030     C**------------------------
012900891030     C** CONTROLLI 2 VIDEATA
013000891030     C**------------------------
013100891030     C     CTRFMT        BEGSR
013200891107     C**--- DESCRIZIONE
013300891113     C     §TRDES        IFEQ      *BLANKS
013400891107     C                   SETON                                        3099
013500891030     C                   GOTO      END1
013600891030     C                   END
013700891030     C     END1          ENDSR
013800891030     C**----------------------------
013900891030     C** AGGIORNAMENTO / INSERIMENTO
014000891030     C**----------------------------
014100891030     C     SCRIVI        BEGSR
014200891107     C     KTAB          CHAIN     TABEL                              80
014300891030     C                   MOVEL     §KUT          TBLKUT
014400891030     C                   MOVEL     §COD          TBLCOD
014500891030     C                   MOVEL     §KEY          TBLKEY
014600891113     C                   MOVEL     DSTR          TBLUNI
014700900420     C*§
014800900420     C                   MOVE      '1'           TBLFTT
014900900420     C                   MOVE      *ZERO         TBLFLT
015000900420     C                   MOVE      *BLANK        TBLFTR
015100900420     C                   MOVE      *ZERO         TBLDTR
015200900420     C*§
015300891102     C   80              WRITE     TABEL                                98
015400891102     C  N80              UPDATE    TABEL                                98
015500891030     C                   ENDSR
