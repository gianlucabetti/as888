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
001100011220     FTRmz43D   CF   E             WORKSTN
001200891106     FTABEL00F  UF A E           K DISK
001300891106     D*--------------------------------------------------------------*
001400891106     D*                        I N P U T                             *
001500891106     D*--------------------------------------------------------------*
001600971210     D TRUL12        E DS                  EXTNAME(TRUL12DS)
001700891106     D KPJBA         E DS
001800011220     D DSXa          E DS
001900971124     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
002000891106     C******************************************************
002100891106     C     *ENTRY        PLIST
002200891106     C                   PARM                    KPJBA
002300891129     C                   MOVEL     KPJBU         VKEY
002400891106     C                   Z-ADD     1             CODUT
002500891106     C                   CALL      'XPARUT'
002600891106     C                   PARM                    UTEDSE
002700891106     C                   MOVEL     RAGUT         RSUT
002800891106     C******************************************************
002900891106     C* POSIZIONAMENTO TABELLE                             *
003000891106     C     KTAB          KLIST                                                  *
003100891106     C                   KFLD                    §KUT                           *
003200891106     C                   KFLD                    §COD                           *
003300891106     C                   KFLD                    §KEY                           *
003400891106     C******************************************************
003500891106     C     *LIKE         DEFINE    TBLKUT        §KUT
003600891106     C     *LIKE         DEFINE    TBLCOD        §COD
003700891106     C     *LIKE         DEFINE    TBLKEY        §KEY
003800971210     C******************************************************
003900971210     C* Richiamo pgm per controllo modificabilità tabella
004000971210     C                   CLEAR                   TRUL12
004100971210     C                   MOVEL     'L'           I12TLA
004200971210     C                   MOVEL     KNSIF         I12NSI
004300011220     C                   MOVEL     'XA'          I12COD
004400971210     C                   CALL      'TRUL12R'
004500971210     C                   PARM                    TRUL12
004600971210     C     O12ERR        IFEQ      '1'
004700971210     C                   GOTO      FINE
004800971210     C                   END
004900971210     C******************************************************
005000891106     C     STAR          TAG
005100891106     C                   SETOFF                                       79
005200891106     C                   MOVEL     KNMUS         PROF              3
005300891106     C     PROF          IFNE      'EDP'
005400891106     C                   SETON                                        79
005500891106     C                   END
005600891106     C                   MOVEL     *BLANKS       §DES             25
005700891129     C                   MOVEL     *BLANKS       VCOD
005800891129     C                   MOVEL     '?'           VCOD
005900891106     C     INIZIO        TAG
006000971124     C                   EXFMT     TB01D1
006100891106     C   KL              GOTO      FINE
006200891129     C     VCOD          IFEQ      *BLANKS
006300891106     C                   SETON                                        30
006400891106     C                   GOTO      INIZIO
006500891106     C                   END
006600891106     C*§  RICERCA IN TABEL00F
006700891107     C     NOVID         TAG
006800891129     C     VCOD          IFEQ      '?'
006900891106     C                   MOVEL     CODUT         §KUT
007000891129     C                   MOVEL     VKEY          §COD
007100891129     C                   MOVE      *ZEROS        VCOD
007200891106     C                   CALL      'X§TABER'
007300891106     C                   PARM                    §KUT
007400891106     C                   PARM                    §COD
007500891106     C                   PARM                    §KEY
007600891106     C                   PARM                    §DES
007700891129     C                   MOVEL     §KEY          VCOD
007800891106     C                   GOTO      INIZIO
007900891106     C                   ELSE
008000891106     C                   MOVEL     CODUT         §KUT
008100891129     C                   MOVEL     VKEY          §COD
008200891121     C                   MOVEL     *BLANKS       §KEY
008300891129     C                   MOVEL     VCOD          §KEY
008400891106     C                   END
008500891106     C*
008600891106     C                   SETOFF                                       0102
008700900629     C                   EXSR      PULVID
008800891106     C     KTAB          CHAIN     TABEL                              80
008900891106     C     *IN80         IFEQ      '1'
009000891106     C                   SETON                                        01        *IMMISSIONE
009100891106     C                   ELSE
009200011220     C                   MOVEL     TBLUNI        DSXA
009300011220     C                   MOVEL     §XaDES        VDES
009400011220     C                   movel     §xadau        Vdta
009500011220     C                   movel     §xadsc        Vdts
009600011220     C                   movel     §xadur        Vdur
009700011220     C                   movel     §xadtc        Vdtc
009800011220     C                   movel     §xarsm        Vrim
009900031106     C                   movel     §xatel        telev
010000971124     C**
010100900629     C                   MOVEL     TBLFTT        VFTT
010200900629     C                   Z-ADD     TBLFLT        VFLT
010300900629     C     TBLFLG        IFEQ      '*'
010400900629     C                   SETON                                        03        *ANNULLATO
010500900629     C                   ELSE
010600891106     C                   SETON                                        02        *VARIAZIONE
010700891106     C                   END
010800900629     C                   END
010900891106     C     SU            TAG
011000891106     C                   SETOFF                                       99
011100971124     C                   EXFMT     TB01D2
011200891106     C   KL              GOTO      FINE
011300891106     C   KD              GOTO      INIZIO
011400900629     C**--- ANNULLAMENTO
011500900629     C   KQ              DO
011600900629     C                   MOVEL     '*'           TBLFLG
011700900629     C                   UPDATE    TABEL
011800900629     C                   GOTO      INIZIO
011900900629     C                   END
012000900629     C** RIPRISTINO
012100900629     C   KE              DO
012200900629     C                   MOVEL     *BLANKS       TBLFLG
012300900629     C                   UPDATE    TABEL
012400900629     C                   GOTO      INIZIO
012500900629     C                   END
012600900629     C**
012700900629     C                   EXSR      CTRFMT
012800971124     C  NKF
012900971124     COR 99              GOTO      SU
013000900629     C**
013100971124     C                   EXSR      SCRIVI
013200891129     C                   GOTO      STAR
013300891106     C     FINE          TAG
013400891106     C                   SETON                                        LR
013500900629     C**------------------------
013600900629     C** PULISCI CAMPI VIDEO
013700900629     C**------------------------
013800900629     C     PULVID        BEGSR
013900900629     C                   MOVEL     *BLANKS       VDES
014000900629     C                   MOVEL     *BLANKS       VFTT
014100011220     C                   MOVEL     *BLANKS       Vdta
014200011220     C                   MOVEL     *BLANKS       Vdts
014300011220     C                   MOVEL     *BLANKS       Vdur
014400011220     C                   MOVEL     *BLANKS       Vdtc
014500011220     C                   MOVEL     *BLANKS       Vrim
014600031106     C                   MOVEL     *BLANKS       telev
014700900629     C                   Z-ADD     0             VFLT
014800900629     C                   ENDSR
014900900629     C**------------------------
015000900629     C** CONTROLLI 2 VIDEATA
015100900629     C**------------------------
015200900629     C     CTRFMT        BEGSR
015300900629     C** CONTROLLO DESCRIZIONE
015400900629     C     VDES          IFEQ      *BLANKS
015500900629     C                   SETON                                        3199
015600900629     C                   GOTO      ENDFMT
015700900629     C                   END
015800900629     C     VFTT          IFNE      '2'
015900900629     C     VFLT          ANDGT     0
016000900629     C                   SETON                                        3399
016100900629     C                   GOTO      ENDFMT
016200900629     C                   END
016300900629     C* SE IL TIPO TRASM. è = A 2 IL COD.FILIALE è OBBLIGATORIO
016400900629     C     VFTT          IFEQ      '2'
016500900629     C     VFLT          ANDEQ     0
016600900629     C                   SETON                                        3299
016700900629     C                   GOTO      ENDFMT
016800900629     C                   END
016900900629     C**
017000900629     C     ENDFMT        ENDSR
017100891106     C**----------------------------
017200891106     C** AGGIORNAMENTO / INSERIMENTO
017300891106     C**----------------------------
017400891106     C     SCRIVI        BEGSR
017500891106     C     KTAB          CHAIN     TABEL                              80
017600891106     C                   MOVEL     §KUT          TBLKUT
017700891106     C                   MOVEL     §COD          TBLCOD
017800891121     C                   MOVEL     *BLANKS       TBLKEY
017900891106     C                   MOVEL     §KEY          TBLKEY
018000011220     C                   MOVEL     VDES          §XaDES
018100011220     C                   MOVEL     Vdta          §Xadau
018200011220     C                   MOVEL     Vdts          §Xadsc
018300011220     C                   MOVEL     Vdur          §Xadur
018400011220     C                   MOVEL     Vdtc          §Xadtc
018500011220     C                   MOVEL     Vrim          §Xarsm
018600031106     C                   MOVEL     telev         §Xatel
018700011220     C                   MOVEL     DSXA          TBLUNI
018800900420     C*§
018900900629     C                   MOVE      VFTT          TBLFTT
019000900629     C                   MOVE      VFLT          TBLFLT
019100900629     C                   MOVE      *BLANK        TBLFTR
019200900629     C                   MOVE      *ZERO         TBLDTR
019300900420     C*§
019400891106     C   80              WRITE     TABEL                                98
019500891106     C  N80              UPDATE    TABEL                                98
019600891106     C                   ENDSR
