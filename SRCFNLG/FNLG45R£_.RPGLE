000100931125     H DECEDIT('0,') DATEDIT(*DMY.)
000200931125?     *--------------------------------------------------------------*
000300941013      * FNLG45R                                                      *
000400931125      *     *----------------------------------------------*         *
000500931125      *       INTERROGAZIONE TABELLA PERIODI X STATISTICHE           *
000600931125      *                      UFFICIO GIACENZE                        *
000700931125      *     *---------------------------------------------*          *
000800931125      *                                                              *
000900931125?     *--------------------------------------------------------------*
001000931125      * 70 -LETTURA SFL RECORD MODIFICATI                            *
001100931125      * 80 -LETTURA ARCHIVIO STATISTICHE                             *
001200931125      * 81 -LETTURA TABELLA PERIODI                                  *
001300931125      * 91 -SFLDSP: VISUALIZZAZIONE SUBFILE                          *
001400931125      * 92 -SFLDSPCTL: VISUALIZZAZIONE SUBFILE DI CONTROLLO          *
001500931125?     *--------------------------------------------------------------*
001600931125      * KC -FINE LAVORO                                              *
001700931125?     *--------------------------------------------------------------*
001800931125     FTABEL00F  IF   E           K DISK
001900931125     F*--------
002000010921     FFLFG765L  IF   E           K DISK
002100931125     F*--------
002200941013     FFNLG45D   CF   E             WORKSTN
002300941013     F                                     SFILE(LG45S01:NRR)
002400931125     D*---------------------------------------------------------------*
002500931125     D* SCHIERA
002600931125     D*---------------------------------------------------------------*
002700021030     D CDP             S              4  0 DIM(900)
002800021030     D AAP             S              6  0 DIM(900)
002900021030     D DES             S             20    DIM(900)
003000021030     D DTA             S              8  0 DIM(900)
003100021030     D DTD             S              8  0 DIM(900)
003200931125?     *--------------------------------------------------------------*
003300931125?     *  DS                                                          *
003400931125?     *--------------------------------------------------------------*
003500931125     D KPJBA         E DS
003600931125      *
003700941013     D FNLG45        E DS                  EXTNAME(FNLG45DS)
003800931125      *
003900931125     D DS2I          E DS
004000931125     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004100931125     D CNCR80        E DS
004200931125      *
004300931125     D                 DS
004400010921     D  FG7AAS                 1      4  0
004500010921     D  FG7PER                 5      6  0
004600010921     D  GIAPER                 3      6  0
004700941013     D                 DS
004800941013     D  AAPER                  3      6  0
004900941013     D  AA2                    3      4  0
005000941013     D  AA4                    1      4  0
005100941013     D  AA4PER                 1      6  0
005200941013     D WLBDA8          DS
005300941013     D  G02DAT                 1      8  0
005400941013     D  G02INV                 9     16  0
005500941013     D  G02ERR                17     17
005600941013     D  G02TGI                18     22  0
005700931125     D                SDS
005800931125     D  NOMPG                  1     10
005900931125      *---------------------------------------------------------------*
006000931125      *   C I C L O      P R I N C I P A L E                          *
006100931125      *---------------------------------------------------------------*
006200931125     C*  INIZIALIZZAZIONE VIDEATA
006300931125     C                   EXSR      INZVID
006400931125     C*
006500931125     C*  LOOP
006600931125     C     £FINE         DOUEQ     'S'
006700931125     C*
006800931125     C     TIPVID        CASEQ     '1'           GESVD1
006900931125     C     TIPVID        CASEQ     '2'           GESVD2
007000931125     C                   END
007100931125     C*
007200931125     C                   END
007300931125     C*
007400941013     C                   MOVEL     FNLG45        KPJBU
007500931125     C*
007600931125     C                   SETON                                        LR
007700931125      *---------------------------------------------------------------*
007800931125      *  INZVID: INIZIALIZZAZIONE PARAMETRI VIDEO                     *
007900931125      *---------------------------------------------------------------*
008000931125     C     INZVID        BEGSR
008100931125     C*
008200931125     C                   MOVEL     NOMPG         NOMPGM
008300931125     C                   Z-ADD     0             NRR               4 0
008400931125     C*
008500931125     C*  Inizializzo subfile
008600931125     C                   SETOFF                                         9192
008700941013     C                   WRITE     LG45C01
008800931125     C                   SETON                                          9192
008900931126     C*----------------------------------------------------*
009000931125     C*  Loop letture su archivio statistiche giacenze x caricamento dati
009100010921     C     *LOVAL        SETLL     FLFG765L
009200010921     C                   READ      FLFG765L                               80
009300931125     C     *IN80         DOWEQ     '0'
009400931125     C*  Carico solo periodi non annullati
009500931125     C                   MOVE      GIAPER        SAVPER            4 0
009600931125     C                   Z-ADD     1             XX
009700941013     C     GIAPER        LOOKUP    CDP(XX)                                44
009800931125     C     *IN44         IFEQ      '1'
009900931125     C                   MOVE      ' '           S1SCE
010000941013     C                   MOVE      AAP(XX)       SFLPRD
010100941013     C                   MOVEL     AAP(XX)       SFLAAP
010200931125     C                   Z-ADD     DTD(XX)       SFLDA
010300931125     C                   Z-ADD     DTA(XX)       SFLA
010400931125     C                   ADD       1             NRR
010500941013     C                   WRITE     LG45S01
010600931125     C                   END
010700931125     C*  Lettura successiva
010800931126     C                   MOVE      FG7AAS        FGWAAS
010900931126     C                   MOVE      FG7PER        FGWPER
011000010921     C     KEYFG7        SETGT     FLFG765L
011100010921     C                   READ      FLFG765L                               80
011200931125     C                   END
011300931126     C*----------------------------------------------------*
011400931125     C*  Controllo se ho scritto almeno un record
011500931125     C     NRR           IFEQ      0
011600931125     C                   MOVE      '2'           TIPVID            1
011700931125     C                   ELSE
011800931125     C                   MOVE      '1'           TIPVID
011900931125     C                   END
012000931126     C*----------------------------------------------------*
012100931125     C                   ENDSR
012200931125      *---------------------------------------------------------------*
012300931125      *  GESVD1: GESTIONE TABELLA PERIODI                             *
012400931125      *---------------------------------------------------------------*
012500931125     C     GESVD1        BEGSR
012600931125     C*
012700931125     C*  Emissione videata
012800931125     C                   Z-ADD     1             S1RCD
012900941013     C                   WRITE     LG45T01
013000941013     C                   WRITE     LG45Z01
013100941013     C                   EXFMT     LG45C01
013200931125     C*----------------------------------------------------*
013300931125     C* Fine pgm
013400931125     C     *INKC         IFEQ      '1'
013500931125     C                   MOVEL     'S'           £FINE             1
013600941013     C                   MOVEL     '3'           D45CMD
013700931125     C                   GOTO      FINVD1
013800931125     C                   END
013900931125     C*----------------------------------------------------*
014000931125     C*  Lettura sfl
014100941013     C                   READC     LG45S01                                70
014200931125     C     *IN70         DOWEQ     '0'
014300931125     C     S1SCE         ANDNE     '1'
014400941013     C                   READC     LG45S01                                70
014500931125     C                   END
014600931125     C*  Controllo se e stata effettuata la scelta e se si
014700931125     C*  ritorno al pgm. chiamante
014800931125     C     S1SCE         IFEQ      '1'
014900941013     C                   MOVE      SFLPRD        D45PRD
015000941013     C                   MOVE      SFLAAP        D45PAA
015100931125     C                   MOVEL     'S'           £FINE
015200931125     C                   END
015300931125     C*----------------------------------------------------*
015400931125     C     FINVD1        ENDSR
015500931125      *---------------------------------------------------------------*
015600931125      *  GESVD2: GESTIONE SUBFILE VUOTO                               *
015700931125      *---------------------------------------------------------------*
015800931125     C     GESVD2        BEGSR
015900931125     C*
016000931125     C*  Emissione videata
016100931125     C                   SETOFF                                       91
016200941013     C                   WRITE     LG45T01
016300941013     C                   WRITE     LG45Z01
016400941013     C                   WRITE     LG45D01
016500941013     C                   EXFMT     LG45C01
016600931125     C*----------------------------------------------------*
016700931125     C* Fine pgm
016800931125     C     *INKC         IFEQ      '1'
016900931125     C                   MOVEL     'S'           £FINE
017000941013     C                   MOVEL     '3'           D45CMD
017100931125     C                   END
017200931125     C*----------------------------------------------------*
017300931125     C                   ENDSR
017400931125      *---------------------------------------------------------------*
017500931125      *  DEFCAM: OPERAZIONI INIZIALI                                  *
017600931125      *---------------------------------------------------------------*
017700941013     C     *INZSR        BEGSR
017800931125     C*
017900931125     C     *ENTRY        PLIST
018000931125     C                   PARM                    KPJBA
018100931125     C*
018200941013     C                   CLEAR                   FNLG45
018300931125     C*----------------------------------------------------*
018400931125     C*  Richiamo XPARUT
018500931125     C                   Z-ADD     1             CODUT             1 0
018600931125     C                   CALL      'X§PARUT'
018700931125     C                   PARM                    UT§DSE
018800931125     C                   MOVEL     REC80         CNCR80
018900931125     C                   MOVEL     RAGUT         RSUT             20
019000931125     C*----------------------------------------------------*
019100931125     C*  Chiavi
019200931125     C     KEYTAB        KLIST
019300931125     C                   KFLD                    TBLWUT
019400931125     C                   KFLD                    TBLWOD
019500931126     C*
019600931126     C     KEYFG7        KLIST
019700931126     C                   KFLD                    FGWAAS
019800931126     C                   KFLD                    FGWPER
019900931125     C*----------------------------------------------------*
020000931125     C*  Definizione variabili
020100931125     C     *LIKE         DEFINE    TBLKUT        TBLWUT
020200931125     C     *LIKE         DEFINE    TBLCOD        TBLWOD
020300931126     C     *LIKE         DEFINE    FG7AAS        FGWAAS
020400931126     C     *LIKE         DEFINE    FG7PER        FGWPER
020500931125     C*----------------------------------------------------*
020600931125     C*  Inizializzazione
020700931125     C                   Z-ADD     1             TBLWUT
020800931125     C                   MOVE      '2I'          TBLWOD
020900931125     C*----------------------------------------------------*
021000931125     C                   Z-ADD     0             XX                4 0
021100931125     C*  Loop letture tabella periodi x caricamento dati
021200931125     C     KEYTAB        CHAIN     TABEL00F                           81
021300931125     C     *IN81         DOWEQ     '0'
021400931125     C*  Carico solo periodi non annullati
021500931125     C     TBLFLG        IFEQ      ' '
021600931125     C                   ADD       1             XX
021700941013     C                   MOVEL     TBLKEY        CDP(XX)
021800941013     C                   Z-ADD     0             AA4PER
021900941013     C                   MOVEL     TBLKEY        AAPER             4 0
022000941013     C     AA2           IFGT      60
022100941013     C                   ADD       1900          AA4
022200941013     C                   ELSE
022300941013     C                   ADD       2000          AA4
022400941013     C                   END
022500941013     C                   MOVEL     AA4PER        AAP(XX)
022600941013     C*
022700931125     C                   MOVEL     TBLUNI        DS2I
022800941013     C                   Z-ADD     §2IDAL        G02INV
022900941013     C                   MOVEL     '3'           G02ERR
023000941013     C                   CALL      'XSRDA8'
023100941013     C                   PARM                    WLBDA8
023200941013     C                   Z-ADD     G02DAT        DTD(XX)
023300941013     C                   Z-ADD     §2IAL         G02INV
023400941013     C                   MOVEL     '3'           G02ERR
023500941013     C                   CALL      'XSRDA8'
023600941013     C                   PARM                    WLBDA8
023700941013     C                   Z-ADD     G02DAT        DTA(XX)
023800931125     C                   END
023900931125     C*  Lettura successiva
024000931125     C     KEYTAB        READE     TABEL00F                               81
024100931125     C                   END
024200931125     C*----------------------------------------------------*
024300931125     C                   ENDSR
