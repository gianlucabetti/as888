000100000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F*------------------------------------------------------------------------*
000300000000     F*                   - RICERCA FASCE DI PESO -
000400000000     F*------------------------------------------------------------------------*
000500000000     F* NELLE PRIME TRE POSIZIONI DI §KEY DEVE ESSERCI IL CODICE DEL FILE DEL
000600000000     F* QUALE SI VOGLIONO SELEZIONARE LE FASCE DI PESO
000700000000     F* SE NELLE PRIME 2 POSIZIONI DEL PARAMETRO §DES SI TROVANO I CARATTERI §§
000800000000     F* CONSIDERO ANCHE I RECORD DI TABELLA ANNULLATI
000900000000     F*------------------------------------------------------------------------*
001000000000     FTABEL00F  IF   E           K DISK
001100020117     FTISE67D   CF   E             WORKSTN
001200020117     F                                     SFILE(SE67SF1:REC1)
001300000000     D*------------------------------------------------------------------------*
001400000000     D*------------------
001500000000     D* DS FASCE DI PESO
001600000000     D*------------------
001700000000     D DS2L          E DS
001800000000     C*------------------------------------------------------------------------*
001900000000     C     *ENTRY        PLIST
002000000000     C                   PARM                    §KUT
002100000000     C                   PARM                    §COD
002200000000     C                   PARM                    §KEY
002300000000     C                   PARM                    §DES             25
002400000000     C*
002500000000     C                   MOVEL     §DES          §AN2              2            *SE ANNULLATI
002600000000     C                   MOVEL     §KEY          §COF              3            *CODICE FILE
002700000000     C*
002800000000     C     *LIKE         DEFINE    TBLKUT        §KUT
002900000000     C     *LIKE         DEFINE    TBLCOD        §COD
003000000000     C     *LIKE         DEFINE    TBLKEY        §KEY
003100000000     C     *LIKE         DEFINE    TBLCOD        §CODK
003200000000     C     *LIKE         DEFINE    TBLKEY        §KEYK
003300000000     C*
003400000000     C     KEYTAB        KLIST
003500000000     C                   KFLD                    §KUT
003600000000     C                   KFLD                    §COD
003700000000     C*------------------------------------------------------------------------*
003800020117     C                   WRITE     SE67D1                                       *TESTATA
003900000000     C**
004000000000     C* GESTIONE SFL
004100000000     C**
004200000000     C     KEYTAB        SETLL     TABEL00F                           90  91
004300000000     C  N91              SETON                                        90
004400000000     C   90              GOTO      FINE
004500000000     C                   Z-ADD     0             REC1              4 0
004600000000     C                   MOVEL     *BLANKS       SCELTA
004700000000     C**
004800000000     C* PULIZIA SFL
004900000000     C**
005000000000     C                   SETON                                            70
005100020117     C                   WRITE     SE67SC1
005200000000     C                   SETOFF                                       7071
005300000000     C**
005400000000     C* CARICAMENTO SFL
005500000000     C**
005600000000     C     SOPRA         TAG
005700000000     C     KEYTAB        READE     TABEL                                  91
005800000000     C   91              GOTO      EMISF1
005900000000     C* ANNULLATA
006000000000IF  1C     TBLFLG        IFEQ      '*'
006100000000     C     §AN2          ANDNE     '§§'
006200000000     C                   GOTO      SOPRA
006300000000E   1C                   ENDIF
006400000000     C* FASCIA NON DEL FILE
006500000000     C                   MOVEL     TBLKEY        COMCOF            3
006600000000IF  1C     COMCOF        IFNE      §COF                                         *SOLO FASCE DEL FILE
006700000000     C                   GOTO      SOPRA
006800000000E   1C                   ENDIF
006900000000     C* FASCIA VALIDA
007000000000     C                   MOVEL     TBLKEY        ALF5              5            *XXX+XX
007100000000     C                   MOVE      ALF5          ALF2              2            *XX
007200000000     C*
007300000000     C                   MOVEL     ALF2          CODTAB                         *CODICE FASCIA
007400000000     C                   MOVEL     TBLUNI        DS2L                           *DESCRIZIONE
007500000000     C                   MOVEL     §2LDFP        DESCRI                         *DECODIFICA FASCIA
007600000000     C                   MOVEL     TBLFLG        STATTB
007700000000     C     REC1          ADD       1             REC1                 71
007800020117     C                   WRITE     SE67SF1
007900000000     C                   GOTO      SOPRA
008000000000     C**
008100000000     C* EMISSIONE SFL
008200000000     C**
008300000000     C     EMISF1        TAG
008400020117     C                   EXFMT     SE67SC1
008500000000     C   KC              SETON                                            90
008600000000     C   KC              GOTO      FINE
008700000000     C**
008800000000     C* LETTURA RECORD VARIATI
008900000000     C**
009000020117     C   71              READC     SE67SF1                                91
009100000000     C  N71
009200000000     COR 91              GOTO      EMISF1
009300000000     C     SCELTA        CABNE     '1'           EMISF1
009400000000     C                   MOVEL     CODTAB        §KEY
009500000000     C                   MOVEL     DESCRI        §DES
009600000000     C**
009700000000     C     FINE          TAG
009800000000     C   90              MOVE      *BLANKS       §KEY
009900000000     C   90              MOVE      *BLANKS       §DES
010000000000     C   90              MOVEL     'ERRORE'      §DES
010100000000     C**
010200000000     C                   SETON                                            LR
