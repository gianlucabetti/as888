000100891106     H DFTNAME(CN0500) DATEDIT(*YMD)
000200891106     F*--------------------------------------------------------------*
000300971124     F* GESTIONE TABELLE CODICI AUTISTA                              *
000400891106     F*----------------------* USO DEGLI INDICATORI *----------------*
000500891106     F*  01        INSERIMENTO                                       *
000600891106     F*  02        VARIAZIONE                                        *
000700900629     F*  03        ANNULLATO
000800891129     F*  30        CODICE ERRATO                                     *
000900891129     F*  31        DESCRIZIONE OBBLIGATORIA                          *
001000891106     F*--------------------------------------------------------------*
001100011211     FTRmz40D   CF   E             WORKSTN
001200891106     FTABEL00F  UF A E           K DISK
001300891106     D*--------------------------------------------------------------*
001400891106     D*                        I N P U T                             *
001500891106     D*--------------------------------------------------------------*
001600971210     D TRUL12        E DS                  EXTNAME(TRUL12DS)
001700891106     D KPJBA         E DS
001800971124     D DSXN          E DS
002000971124     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
002100891106     C******************************************************
002200891106     C     *ENTRY        PLIST
002300891106     C                   PARM                    KPJBA
002400891129     C                   MOVEL     KPJBU         VKEY
002500891106     C                   Z-ADD     1             CODUT
002600891106     C                   CALL      'XPARUT'
002700891106     C                   PARM                    UTEDSE
002800891106     C                   MOVEL     RAGUT         RSUT
002900891106     C******************************************************
003000891106     C* POSIZIONAMENTO TABELLE                             *
003100891106     C     KTAB          KLIST                                                  *
003200891106     C                   KFLD                    §KUT                           *
003300891106     C                   KFLD                    §COD                           *
003400891106     C                   KFLD                    §KEY                           *
003500891106     C******************************************************
003600891106     C     *LIKE         DEFINE    TBLKUT        §KUT
003700891106     C     *LIKE         DEFINE    TBLCOD        §COD
003800891106     C     *LIKE         DEFINE    TBLKEY        §KEY
003900971210     C******************************************************
004000971210     C* Richiamo pgm per controllo modificabilità tabella
004100971210     C                   CLEAR                   TRUL12
004200971210     C                   MOVEL     'L'           I12TLA
004300971210     C                   MOVEL     KNSIF         I12NSI
004400971210     C                   MOVEL     'XN'          I12COD
004500971210     C                   CALL      'TRUL12R'
004600971210     C                   PARM                    TRUL12
004700971210     C     O12ERR        IFEQ      '1'
004800971210     C                   GOTO      FINE
004900971210     C                   END
005000971210     C******************************************************
005100891106     C     STAR          TAG
005200891106     C                   SETOFF                                       79
005300891106     C                   MOVEL     KNMUS         PROF              3
005400891106     C     PROF          IFNE      'EDP'
005500891106     C                   SETON                                        79
005600891106     C                   END
005700891106     C                   MOVEL     *BLANKS       §DES             25
005800891129     C                   MOVEL     *BLANKS       VCOD
006000891129     C                   MOVEL     '?'           VCOD
006100891106     C     INIZIO        TAG
006200971124     C                   EXFMT     TB01D1
006300891106     C   KL              GOTO      FINE
006400891129     C     VCOD          IFEQ      *BLANKS
006500891106     C                   SETON                                        30
006600891106     C                   GOTO      INIZIO
006700891106     C                   END
006800891106     C*§  RICERCA IN TABEL00F
006900891107     C     NOVID         TAG
007000891129     C     VCOD          IFEQ      '?'
007100891106     C                   MOVEL     CODUT         §KUT
007200891129     C                   MOVEL     VKEY          §COD
007300891129     C                   MOVE      *ZEROS        VCOD
007400891106     C                   CALL      'X§TABER'
007500891106     C                   PARM                    §KUT
007600891106     C                   PARM                    §COD
007700891106     C                   PARM                    §KEY
007800891106     C                   PARM                    §DES
007900891129     C                   MOVEL     §KEY          VCOD
008000891106     C                   GOTO      INIZIO
008100891106     C                   ELSE
008200891106     C                   MOVEL     CODUT         §KUT
008300891129     C                   MOVEL     VKEY          §COD
008400891121     C                   MOVEL     *BLANKS       §KEY
008500891129     C                   MOVEL     VCOD          §KEY
008600891106     C                   END
008700891106     C*
008800891106     C                   SETOFF                                       0102
008900900629     C                   EXSR      PULVID
009000891106     C     KTAB          CHAIN     TABEL                              80
009100891106     C     *IN80         IFEQ      '1'
009200891106     C                   SETON                                        01        *IMMISSIONE
009300891106     C                   ELSE
009400971124     C                   MOVEL     TBLUNI        DSXN
009500971124     C                   MOVEL     §XNDES        VDES
010001011211     C                   movel     §xnper        Vper
010100971124     C**
010200900629     C                   MOVEL     TBLFTT        VFTT
010300900629     C                   Z-ADD     TBLFLT        VFLT
010400900629     C     TBLFLG        IFEQ      '*'
010500900629     C                   SETON                                        03        *ANNULLATO
010600900629     C                   ELSE
010700891106     C                   SETON                                        02        *VARIAZIONE
010800891106     C                   END
010900900629     C                   END
011000891106     C     SU            TAG
011100891106     C                   SETOFF                                       99
011200971124     C                   EXFMT     TB01D2
011300891106     C   KL              GOTO      FINE
011400891106     C   KD              GOTO      INIZIO
011500900629     C**--- ANNULLAMENTO
011600900629     C   KQ              DO
011700900629     C                   MOVEL     '*'           TBLFLG
011800900629     C                   UPDATE    TABEL
011900900629     C                   GOTO      INIZIO
012000900629     C                   END
012100900629     C** RIPRISTINO
012200900629     C   KE              DO
012300900629     C                   MOVEL     *BLANKS       TBLFLG
012400900629     C                   UPDATE    TABEL
012500900629     C                   GOTO      INIZIO
012600900629     C                   END
012700900629     C**
012800900629     C                   EXSR      CTRFMT
012900971124     C  NKF
013000971124     COR 99              GOTO      SU
013100900629     C**
013200971124     C                   EXSR      SCRIVI
013300891129     C                   GOTO      STAR
013400891106     C     FINE          TAG
013500891106     C                   SETON                                        LR
013600900629     C**------------------------
013700900629     C** PULISCI CAMPI VIDEO
013800900629     C**------------------------
013900900629     C     PULVID        BEGSR
014000900629     C                   MOVEL     *BLANKS       VDES
014100900629     C                   MOVEL     *BLANKS       VFTT
014200900629     C                   Z-ADD     0             VFLT
014300900629     C                   ENDSR
014400900629     C**------------------------
014500900629     C** CONTROLLI 2 VIDEATA
014600900629     C**------------------------
014700900629     C     CTRFMT        BEGSR
014800900629     C** CONTROLLO DESCRIZIONE
014900900629     C     VDES          IFEQ      *BLANKS
015000900629     C                   SETON                                        3199
015100900629     C                   GOTO      ENDFMT
015200900629     C                   END
015300900629     C     VFTT          IFNE      '2'
015400900629     C     VFLT          ANDGT     0
015500900629     C                   SETON                                        3399
015600900629     C                   GOTO      ENDFMT
015700900629     C                   END
015800900629     C* SE IL TIPO TRASM. è = A 2 IL COD.FILIALE è OBBLIGATORIO
015900900629     C     VFTT          IFEQ      '2'
016000900629     C     VFLT          ANDEQ     0
016100900629     C                   SETON                                        3299
016200900629     C                   GOTO      ENDFMT
016300900629     C                   END
016400900629     C**
016500900629     C     ENDFMT        ENDSR
016600891106     C**----------------------------
016700891106     C** AGGIORNAMENTO / INSERIMENTO
016800891106     C**----------------------------
016900891106     C     SCRIVI        BEGSR
017000891106     C     KTAB          CHAIN     TABEL                              80
017100891106     C                   MOVEL     §KUT          TBLKUT
017200891106     C                   MOVEL     §COD          TBLCOD
017300891121     C                   MOVEL     *BLANKS       TBLKEY
017400891106     C                   MOVEL     §KEY          TBLKEY
017500971124     C                   MOVEL     VDES          §XNDES
017601011211     C                   MOVEL     Vper          §XNper
017700971124     C                   MOVEL     DSXN          TBLUNI
017800900420     C*§
017900900629     C                   MOVE      VFTT          TBLFTT
018000900629     C                   MOVE      VFLT          TBLFLT
018100900629     C                   MOVE      *BLANK        TBLFTR
018200900629     C                   MOVE      *ZERO         TBLDTR
018300900420     C*§
018400891106     C   80              WRITE     TABEL                                98
018500891106     C  N80              UPDATE    TABEL                                98
018600891106     C                   ENDSR
