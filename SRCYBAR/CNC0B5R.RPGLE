000100000000     H
000200931210     F*---------------------------------------------------------------*
000300931210     F*                  * USO DEGLI INDICATORI *                     *
000400931210     F*---------------------------------------------------------------*
000500931210     F*  KC = FINE LAVORO                                             *
000600931210     F*  KF = CONFERMA                                                *
000700931210     F*---------------------------------------------------------------*
000800931210     F*  70 = MESSAGGIO DI ERRORE RECORD BLOCCATI O INESISTENTI       *
000900931210     F*  71 = CHAIN SU TABEL                                          *
001000931210     F*  79 = DISPLAY RECORD CONTROLLO SUBFILE SE OFF                 *
001100931210     F*  80 = DISPLAY SUBFILE SE OFF                                  *
001200931210     F*  81 = READC SU SUBFILE                                        *
001300931210     F*  82 = CHAIN SU TABEL                                          *
001400931210     F*  90 = INDICATORE GENERICO DI ERRORE                           *
001500931206     F*---------------------------------------------------------------*
001600931206     F*  D A T A      B A S E                                         *
001700931206     F*---------------------------------------------------------------*
001800970205     FTABEL00F  UF A E           K DISK
001900931206     F*----------
002000931206     FCNC0B5D   CF   E             WORKSTN
002100931206     F                                     SFILE(C0B5D02:NREC)
002200970205     D*---------------------------------------------------------------*
002300970205     D*  SCHIERE
002400970205     D*---------------------------------------------------------------*
002500020507     D £1              S              3  0 DIM(30)
002600020507     D*FIL             S              3    DIM(29)
002700970205     D*---------------------------------------------------------------*
002800931206     D*-------- DATA AREA : NUMERO REGISTRAZIONE CASSA                *
002900991206     D**DTACAS      DS
003000991206     D**                                        1   60NUMCAS
003100991206     D**                                        7  120DATCAS
003200991206     D**                                        1   60NUMVER
003300980130     D TRUL12        E DS                  EXTNAME(TRUL12DS)
003400000000?    D*-
003500000000     D KPJBA         E DS
003600000000     D  MSGERR               453    502
003700000000     D/SPACE 3
003800970205     D CNCR80        E DS
003801020507     D TR06DS        E DS                  EXTNAME(TRUL06DS)
003802020507     D  LIN                    1     90  0 dim(30)
003900970205     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
004000000000     I/EJECT
004100000000     I*---------------------------------------------------------------*
004200931206     I*   CICLO  PRINCIPALE                                           *
004300931206     C*----------------------------------------------------------------
004400931206     C*-----  Inizializzazione Prima Videata
004500931206     C                   EXSR      INZVD1
004600931206     C*-----  Caricamento subfile
004700931206     C                   EXSR      CARVER
004800931206     C*----------------------------------------------------------------
004900000000     C     VIDEO         TAG
005000931206     C                   WRITE     C0B5D01
005100931206     C                   WRITE     C0B5D04
005200931206     C                   EXFMT     C0B5D03
005300931206     C*----------------------------------------------------------------
005400931209     C   KC
005500900612     COR 70              GOTO      FINE
005600931206      *----------------------------------------------------------------
005700931206      *-----  AGGIORNAMENTO DATI
005800931209     C   KF              EXSR      AGGDAT
005900931206     C                   GOTO      VIDEO
006000931206     C*----------------------------------------------------------------
006100931206     C*  Aggiorno data area
006200000000     C     FINE          TAG
006300991206     C**N70                OUT  DTACAS
006400980130     C     FINE1         TAG
006500931206     C                   SETON                                        LR
006600931206      *----------------------------------------------------------------
006700931206      *-----  INIZIALIZZAZIONE PRIMA VIDEATA
006800931206     C*----------------------------------------------------------------
006900931206     C     INZVD1        BEGSR
007000931206     C*
007100931206     C     SOPRA         TAG
007200931206     C                   SETOFF                                       70
007300931206     C                   Z-ADD     0             VIDDAT
007400931206     C                   Z-ADD     0             NUMCAS
007500931206     C*
007600931206     C                   SETON                                        7980
007700931206     C                   WRITE     C0B5D03
007800931206     C                   SETOFF                                       7980
007900931206     C*-------SE SBAGLIA LETTURA AGISCE UN TIMER CHE RITARDA L'ERRORE
008000991206     C***        *LOCK     IN   DTACAS                 90
008100991206     C***90                ADD  1         CTR
008200991206     C***90      CTR       CABLT2         SOPRA
008300991206     C***90                SETON                     7080
008400991206     C***70                GOTO VIDEO
008500931206     C*-------VIDEATA------------------------------------------------
008600991206     C***                  Z-ADDDATCAS    VIDDAT
008700991206     C***                  MOVE DATCAS    GGWW    20
008800991206     C***                  MOVELDATCAS    AAWW    20
008900991206     C***                  MOVELGGWW      VIDDAT
009000991206     C***                  MOVE AAWW      VIDDAT
009100991206     C                   Z-ADD     1             NREC              4 0
009200991206     C                   Z-ADD     1             SFNREC
009300931206     C*
009400931206     C                   ENDSR
009500931206      *----------------------------------------------------------------
009600931206      *-----  CARICAMENTO DATI DA TABELLA 4D
009700931206     C*----------------------------------------------------------------
009800931206     C     CARVER        BEGSR
009900931206     C*
010000931206     C                   Z-ADD     1             TBWKUT
010100931206     C                   MOVEL     '4D'          TBWCOD
010200931206     C*
010300931206     C     KEYTAB        CHAIN     TABEL00F                           82
010400970205     C*  Se non c'è la tabella la creo in base alle filiale in £3
010500970205     C     *IN82         IFEQ      '1'
010600970205     C                   DO        Y             X
010700970205     C                   CLEAR                   TABEL
010800970205     C                   Z-ADD     1             TBLKUT
010900970205     C                   MOVEL     '4D'          TBLCOD
011000020507     C                   MOVEL     £1(X)         TBLKEY
011100970205     C                   MOVEL     '000000'      TBLUNI
011200970205     C                   WRITE     TABEL
011300970205     C                   END
011400970205     C                   END
011500970205     C*  Richaino la bolla
011600970205     C     KEYTAB        CHAIN     TABEL00F                           82
011700970205     C     *IN82         DOWEQ     '0'
011800931206     C     TBLFLG        IFEQ      ' '
011900931206     C                   MOVEL     TBLKEY        CODFIL
012000931206     C                   MOVEL     TBLUNI        NUMFIL
012100931206     C                   WRITE     C0B5D02
012200931206     C                   ADD       1             NREC
012300931206     C                   END
012400931206     C     KEYTAB        READE     TABEL00F                               82
012500931206     C                   END
012600931206     C*
012700931206     C     NREC          IFEQ      1
012800931206     C                   SETON                                        7080
012900931206     C                   GOTO      VIDEO
013000931206     C                   END
013100931206     C*
013200931206     C                   ENDSR
013300931206      *----------------------------------------------------------------
013400931206      *-----  Aggiornamento Dati
013500931206     C*----------------------------------------------------------------
013600931206     C     AGGDAT        BEGSR
013700931206     C*
013800931206     C*  Aggiorno Data Area Cassa
013900991206     C***                  Z-ADDVIDDAT    DATCAS
014000991206     C***                  MOVE VIDDAT    AAWW    20
014100991206     C***                  MOVELVIDDAT    GGWW    20
014200991206     C***                  MOVELAAWW      DATCAS
014300991206     C***                  MOVE GGWW      DATCAS
014400931206     C*
014500931206     C*  Aggiorno tabella 4D  da Subfile
014600931206     C                   READC     C0B5D02                                81
014700931206     C     *IN81         DOWEQ     '0'
014800931206     C                   MOVEL     CODFIL        TBWKEY
014900931206     C     KEYTA1        CHAIN     TABEL00F                           71
015000931206     C     *IN71         IFEQ      '0'
015100931206     C                   MOVEL     NUMFIL        TBLUNI
015200931206     C                   EXCEPT    AGGNUM
015300931206     C                   END
015400931206     C                   READC     C0B5D02                                81
015500931206     C                   END
015600931206     C                   ENDSR
015700931206      *----------------------------------------------------------------
015800931206      *-----  OPERAZIONI INIZIALI
015900931206     C*----------------------------------------------------------------
016000931206     C     *INZSR        BEGSR
016100931206     C*
016200931206     C     *ENTRY        PLIST
016300931206     C                   PARM                    KPJBA
016400931206     C*
016500931206     C                   Z-ADD     1             CODUT
016600970205     C                   CALL      'X§PARUT'
016700931206     C                   PARM                    UTEDSE
016800931206     C     CODUT         CABEQ     -1            FINE
016900931206     C                   MOVEL     RAGUT         RSUT             20
017000970205     C                   MOVEL     REC80         CNCR80
017100991206     C****       *NAMVAR   DEFN           DTACAS
017200931206     C                   Z-ADD     0             CTR               8 0
017300931206      *----------------------------------------------------------------
017400931206      *  Definizione chiavi per caricarmento/aggiorn. tabella 4d
017500931206     C     KEYTAB        KLIST
017600931206     C                   KFLD                    TBWKUT
017700931206     C                   KFLD                    TBWCOD
017800931206     C*
017900931206     C     KEYTA1        KLIST
018000931206     C                   KFLD                    TBWKUT
018100931206     C                   KFLD                    TBWCOD
018200931206     C                   KFLD                    TBWKEY
018300931206      *  Definizione campi
018400931206     C     *LIKE         DEFINE    TBLKUT        TBWKUT
018500931206     C     *LIKE         DEFINE    TBLCOD        TBWCOD
018600931206     C     *LIKE         DEFINE    TBLKEY        TBWKEY
018700980130     C* Richiamo pgm per controllo modificabilità tabella
018800980130     C                   CLEAR                   TRUL12
018900980130     C                   MOVEL     'L'           I12TLA
019000980130     C                   MOVEL     KNSIF         I12NSI
019100980130     C                   MOVEL     '4D'          I12COD
019200980130     C                   CALL      'TRUL12R'
019300980130     C                   PARM                    TRUL12
019400980130     C     O12ERR        IFEQ      '1'
019500980130     C                   GOTO      FINE1
019600980130     C                   END
019700980130     C*----------------------------------------------------*
019800020507      *  Carico filiali £1
019801020507     C                   Z-ADD     0             Y                 3 0
019802020507     C                   Z-ADD     0             x                 3 0
019803020507     C                   CLEAR                   TR06DS
019804020507     C                   MOVEL     '£1'          D06COD
019805020507     C                   MOVEL     simfel        D06KEY
019806020507     C                   MOVEL     TR06DS        KPJBU
019807020507     C                   CALL      'TRUL06R'
019808020507     C                   PARM                    KPJBA
019809020507     C                   MOVEL     KPJBU         TR06DS
019810020507     C                   MOVEA     LIN           £1
019900020507     C*                  Z-ADD     1             TBWKUT
020000020507     C*                  MOVEL     '£3'          TBWCOD
020100020507     C*                  MOVEL     *BLANKS       TBWKEY
020200020507     C*                  MOVEL     SIMFEL        TBWKEY
020300020507     C*    KEYTAB        CHAIN     TABEL00F                           31
020400020507     C*    *IN31         IFEQ      '0'
020500020507     C*    TBLFLG        ANDEQ     *BLANKS
020600020507     C*                  MOVEA     TBLUNI        FIL
020700020507     C*                  END
020800970205     C*  Carico £3 da FIL
021000020507     C*                  DO        29            X                 3 0
021100020507     C*    FIL(X)        IFGT      *ZEROS
021200020507     C*                  ADD       1             Y
021300020507     C*                  MOVEL     FIL(X)        £3(Y)
021400020507     C*                  END
021500020507     C*                  END
021600931206      *
021700931206     C                   ENDSR
021800931206     O*-----------------------------------------------------*
021900931206     OTABEL     E            AGGNUM
022000931206     O                       TBLUNI
