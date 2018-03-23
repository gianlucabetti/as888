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
001800010921     FFLFG765L  IF   E           K DISK
001900931125     F*--------
002000941013     FFNLG45D   CF   E             WORKSTN
002100941013     F                                     SFILE(LG45S01:NRR)
002200931125     D*---------------------------------------------------------------*
002300931125?     *  DS                                                          *
002400931125?     *--------------------------------------------------------------*
002500931125     D KPJBA         E DS
002600931125      *
002700941013     D FNLG45        E DS                  EXTNAME(FNLG45DS)
002800931125      *
002900931125     D DS2I          E DS
003000931125     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003100931125     D CNCR80        E DS
003200141217     D DATECNV0F     E DS
003300141217     D*------------------
003400141217     D da_lunedi       S               D
003500141217     D a_domenica      S               D
003600141217     d DataISO         s               d   datfmt(*iso)
003700141217     d DataEUR         s               d   datfmt(*eur)
003800141217     D*------------------
003900931125     D                 DS
004000010921     D  FG7AAS                 1      4  0
004100141217     D  FG7soloAnno            3      4  0
004200010921     D  FG7PER                 5      6  0
004300010921     D  GIAPER                 3      6  0
004400141217      *
004500941013     D WLBDA8          DS
004600941013     D  G02DAT                 1      8  0
004700941013     D  G02INV                 9     16  0
004800941013     D  G02ERR                17     17
004900941013     D  G02TGI                18     22  0
005000931125     D                SDS
005100931125     D  NOMPG                  1     10
005200931125      *---------------------------------------------------------------*
005300931125      *   C I C L O      P R I N C I P A L E                          *
005400931125      *---------------------------------------------------------------*
005500931125     C*  INIZIALIZZAZIONE VIDEATA
005600931125     C                   EXSR      INZVID
005700931125     C*
005800931125     C*  LOOP
005900931125     C     £FINE         DOUEQ     'S'
006000931125     C*
006100931125     C     TIPVID        CASEQ     '1'           GESVD1
006200931125     C     TIPVID        CASEQ     '2'           GESVD2
006300931125     C                   END
006400931125     C*
006500931125     C                   END
006600931125     C*
006700941013     C                   MOVEL     FNLG45        KPJBU
006800931125     C*
006900931125     C                   SETON                                        LR
007000931125      *---------------------------------------------------------------*
007100931125      *  INZVID: INIZIALIZZAZIONE PARAMETRI VIDEO                     *
007200931125      *---------------------------------------------------------------*
007300931125     C     INZVID        BEGSR
007400931125     C*
007500931125     C                   MOVEL     NOMPG         NOMPGM
007600931125     C                   Z-ADD     0             NRR               4 0
007700931125     C*
007800931125     C*  Inizializzo subfile
007900931125     C                   SETOFF                                         9192
008000941013     C                   WRITE     LG45C01
008100931125     C                   SETON                                          9192
008200931126     C*----------------------------------------------------*
008300931125     C*  Loop letture su archivio statistiche giacenze x caricamento dati
008400010921     C     *LOVAL        SETLL     FLFG765L
008500010921     C                   READ      FLFG765L                               80
008600141217      *
008700931125     C     *IN80         DOWEQ     '0'
008800931125     C*  Carico solo periodi non annullati
008900931125     C                   MOVE      GIAPER        SAVPER            4 0
009000141217      ***
009100141217      ***  Data una settimana dell'Anno
009200141217      ***    ricava il primo e ultimo giorno
009300141217      ***       AAAAWW --> lunedì e domenica da impostare a video
009400141217      ***
009500141217
009600141217     C/EXEC SQL
009700141217     C+ SELECT DISTINCT
009800141217     C+   DC_WEEK_STARTING_DATE, DC_WEEK_ENDING_DATE
009900141217     C+ INTO         :da_Lunedi, :a_domenica
010000141217     C+ FROM DATECNV0F
010100141217     C+ WHERE DC_WOY_ISO= :FG7PER
010200141217     C+              and
010300141217     C+           DC_YY = :FG7soloAnno
010400141217     C/END-EXEC
010500141217     c                   if        sqlCOD = 0
010600141217     c                   move      da_lunedi     dataeur
010700141217     c                   move      dataeur       SFLda
010800141217      *
010900141217     c                   move      a_domenica    dataeur
011000141217     c                   move      dataeur       SFLa
011100141217     *****
011200931125     C                   MOVE      ' '           S1SCE
011300141217     C                   z-add     FG7PER        SFLPRD
011400141217     C                   z-add     FG7AAS        SFLAAP
011500141217     *****
011600931125     C                   ADD       1             NRR
011700941013     C                   WRITE     LG45S01
011800931125     C                   END
011900931125     C*  Lettura successiva
012000931126     C                   MOVE      FG7AAS        FGWAAS
012100931126     C                   MOVE      FG7PER        FGWPER
012200010921     C     KEYFG7        SETGT     FLFG765L
012300010921     C                   READ      FLFG765L                               80
012400931125     C                   END
012500931126     C*----------------------------------------------------*
012600931125     C*  Controllo se ho scritto almeno un record
012700931125     C     NRR           IFEQ      0
012800931125     C                   MOVE      '2'           TIPVID            1
012900931125     C                   ELSE
013000931125     C                   MOVE      '1'           TIPVID
013100931125     C                   END
013200931126     C*----------------------------------------------------*
013300931125     C                   ENDSR
013400931125      *---------------------------------------------------------------*
013500931125      *  GESVD1: GESTIONE TABELLA PERIODI                             *
013600931125      *---------------------------------------------------------------*
013700931125     C     GESVD1        BEGSR
013800931125     C*
013900931125     C*  Emissione videata
014000931125     C                   Z-ADD     1             S1RCD
014100941013     C                   WRITE     LG45T01
014200941013     C                   WRITE     LG45Z01
014300941013     C                   EXFMT     LG45C01
014400931125     C*----------------------------------------------------*
014500931125     C* Fine pgm
014600931125     C     *INKC         IFEQ      '1'
014700931125     C                   MOVEL     'S'           £FINE             1
014800941013     C                   MOVEL     '3'           D45CMD
014900931125     C                   GOTO      FINVD1
015000931125     C                   END
015100931125     C*----------------------------------------------------*
015200931125     C*  Lettura sfl
015300941013     C                   READC     LG45S01                                70
015400931125     C     *IN70         DOWEQ     '0'
015500931125     C     S1SCE         ANDNE     '1'
015600941013     C                   READC     LG45S01                                70
015700931125     C                   END
015800931125     C*  Controllo se e stata effettuata la scelta e se si
015900931125     C*  ritorno al pgm. chiamante
016000931125     C     S1SCE         IFEQ      '1'
016100941013     C                   MOVE      SFLPRD        D45PRD
016200941013     C                   MOVE      SFLAAP        D45PAA
016300931125     C                   MOVEL     'S'           £FINE
016400931125     C                   END
016500931125     C*----------------------------------------------------*
016600931125     C     FINVD1        ENDSR
016700931125      *---------------------------------------------------------------*
016800931125      *  GESVD2: GESTIONE SUBFILE VUOTO                               *
016900931125      *---------------------------------------------------------------*
017000931125     C     GESVD2        BEGSR
017100931125     C*
017200931125     C*  Emissione videata
017300931125     C                   SETOFF                                       91
017400941013     C                   WRITE     LG45T01
017500941013     C                   WRITE     LG45Z01
017600941013     C                   WRITE     LG45D01
017700941013     C                   EXFMT     LG45C01
017800931125     C*----------------------------------------------------*
017900931125     C* Fine pgm
018000931125     C     *INKC         IFEQ      '1'
018100931125     C                   MOVEL     'S'           £FINE
018200941013     C                   MOVEL     '3'           D45CMD
018300931125     C                   END
018400931125     C*----------------------------------------------------*
018500931125     C                   ENDSR
018600931125      *---------------------------------------------------------------*
018700931125      *  DEFCAM: OPERAZIONI INIZIALI                                  *
018800931125      *---------------------------------------------------------------*
018900941013     C     *INZSR        BEGSR
019000931125     C*
019100931125     C     *ENTRY        PLIST
019200931125     C                   PARM                    KPJBA
019300931125     C*
019400941013     C                   CLEAR                   FNLG45
019500931125     C*----------------------------------------------------*
019600931125     C*  Richiamo XPARUT
019700931125     C                   Z-ADD     1             CODUT             1 0
019800931125     C                   CALL      'X§PARUT'
019900931125     C                   PARM                    UT§DSE
020000931125     C                   MOVEL     REC80         CNCR80
020100931125     C                   MOVEL     RAGUT         RSUT             20
020200931125     C*----------------------------------------------------*
020300931125     C*  Chiavi
020400931126     C     KEYFG7        KLIST
020500931126     C                   KFLD                    FGWAAS
020600931126     C                   KFLD                    FGWPER
020700931125     C*----------------------------------------------------*
020800931125     C*  Definizione variabili
020900931126     C     *LIKE         DEFINE    FG7AAS        FGWAAS
021000931126     C     *LIKE         DEFINE    FG7PER        FGWPER
021100931125     C*----------------------------------------------------*
021200931125     C                   ENDSR
