000100960610      ***********************************************************************
000200960610      *   TRSMISSIONE TABELLE  E.D.I. ESTERO                                *
000300960610      ***********************************************************************
000400960610     H DECEDIT('0,') DATEDIT(*DMY.)
000500960610     F*---------------------------------------------------------------------*
000600960610     F*  DATA BASE                                                          *
000700960610     F*---------------------------------------------------------------------*
000800960611     FEDTAB00R  UF   E             DISK    USROPN
000900960610     F*---------
001000960610     FEDTAB01L  UF A E           K DISK
001100960610     F                                     RENAME(EDTAB000:EDTABF)
001200960610     D*---------------------------------------------------------------------*
001300960610     D*  SCHIERE                                                            *
001400960610     D*---------------------------------------------------------------------*
001500960610     D CMD1            S             55    DIM(1) CTDATA PERRCD(1)
001600960610     D CMD2            S             55    DIM(1) CTDATA PERRCD(1)
001700960610     D CMD3            S             55    DIM(1) CTDATA PERRCD(1)
001800960610     D CMD5            S             55    DIM(1) CTDATA PERRCD(1)
001900960610     D*---------------------------------------------------------------------*
002000960610     D*  DS                                                                 *
002100960610     D*---------------------------------------------------------------------*
002200960610     D KPJBA         E DS
002300960610     D  LIBSYS                92    101
002400960611     D  LEGFIL               483    492
002500960611     D  MBRFIL               493    502
002600960610     D*  DS x salvataggio dati impostati in FNVAB
002700960610     D SAVTAB        E DS                  EXTNAME(EDTAB01L)
002800960610     D SALVA           DS           141
002900960610     D WLBDA8          DS
003000960610     D  G02DAT                 1      8  0
003100960610     D  G02INV                 9     16  0
003200960610     D  G02ERR                17     17
003300960610     D  G02TGI                18     22  0
003400960610     C*---------------------------------------------------------------------*
003500960610     C*  Ciclo Principale
003600960610     C*---------------------------------------------------------------------*
003700960610     C*  Eseguo OVRDBF
003800960610     C                   EXSR      CAEXA
003900960610     C*  Lettura record trasmissione e aggiornamento dati
004000960610     C                   READ      EDTAB00R                               30
004100960610     C     *IN30         DOWEQ     '0'
004200960610     C                   EXSR      AGGTAB
004300960610     C                   READ      EDTAB00R                               30
004400960610     C                   END
004500960610     C*  Eseguo DLTOVR
004600960610     C                   EXSR      CAEXB
004700960610     C*
004800960611     C     STOP          TAG
004900960610     C                   SETON                                        LR
005000960610     C**---------------------------------------------------------
005100960610     C** Aggiornamento tabelle
005200960610     C**---------------------------------------------------------
005300960610     C     AGGTAB        BEGSR
005400960610     C*  Salvo i dati del record di ricezione
005500960610     C                   MOVEL     SAVTAB        SALVA
005600960610     C*  Eseguo Chain per verificare se devo eseguire scrittura,
005700960610     C*  annullamento, modifica
005800960610     C                   MOVEL     TABCOD        KCOD
005900960610     C                   MOVEL     TABKEY        KKEY
006000960610     C     KTAB          CHAIN     EDTAB01L                           30
006100960610     C     *IN30         IFEQ      '0'
006200960610     C                   MOVEL     SALVA         SAVTAB
006300960610     C*  Controllo se devo annullare il record
006400960610     C     TABFLG        IFEQ      '*'
006500960610     C                   DELETE    EDTABF
006600960610     C                   ELSE
006700960610     C                   MOVEL     'R'           TABFTR
006800960610     C                   Z-ADD     WOGGI         TABDTR
006900960610     C                   UPDATE    EDTABF
007000960610     C                   END
007100960610     C*  Scrittura nuovo archivio
007200960610     C                   ELSE
007300960610     C                   MOVEL     'R'           TABFTR
007400960610     C                   Z-ADD     WOGGI         TABDTR
007500960610     C                   WRITE     EDTABF
007600960610     C                   END
007700960610     C*  Deleto record in ricezione
007800960610     C                   DELETE    EDTAB000
007900960610     C*
008000960610     C                   ENDSR
008100960610     C**---------------------------------------------------------
008200960610     C** ESEGUE QCAEXEC DI APERTURA SUL MEMBRO DI EDTAB00R      -
008300960610     C**---------------------------------------------------------
008400960610     C     CAEXA         BEGSR
008500960610     C                   MOVE      ')'           VAR11            11
008600960610     C                   MOVEL     MBRFIL        VAR11
008700960610     C                   MOVE      '))'          VAR12            12
008800960610     C                   MOVEL     MBRFIL        VAR12
008900960610     C                   Z-ADD     55            LUNG             15 5
009000960610     C*
009100960610     C                   SETOFF                                       20
009200960610     C                   MOVE      VAR11         CMD1
009300960610     C                   MOVEL     *BLANKS       COMMAN
009400960610     C                   MOVEA     CMD1(1)       COMMAN           80
009500960610     C                   CALL      'QCMDEXC'                            20
009600960610     C                   PARM                    COMMAN
009700960610     C                   PARM                    LUNG
009800960610     C   20              GOTO      STOP
009900960610     C** CONTROLLO SE ESISTE IL MEMBRO
010000960610     C** SE NON ESISTE CHIUDO IL PROGRAMMA
010100960610     C                   SETOFF                                       20
010200960610     C                   MOVE      VAR12         CMD2
010300960610     C                   MOVEL     *BLANKS       COMMAN
010400960610     C                   MOVEA     CMD2(1)       COMMAN
010500960610     C                   CALL      'QCMDEXC'                            20
010600960610     C                   PARM                    COMMAN
010700960610     C                   PARM                    LUNG
010800960610     C   20              GOTO      STOP
010900960610     C** ALLOCO MEMBRO
011000960610     C** SE NON RIESCE CHIUDO IL PROGRAMMA
011100960610     C                   SETOFF                                       20
011200960610     C                   MOVE      VAR11         CMD3
011300960610     C                   MOVEL     *BLANKS       COMMAN
011400960610     C                   MOVEA     CMD3(1)       COMMAN
011500960610     C                   CALL      'QCMDEXC'                            20
011600960610     C                   PARM                    COMMAN
011700960610     C                   PARM                    LUNG
011800960610     C   20              GOTO      STOP
011900960610     C** ESEGUE OVRDBF DEL MEMBRO DESIDERATO
012000960610     C** SE NON RIESCE CHIUDO IL PROGRAMMA
012100960610     C                   OPEN      EDTAB00R
012200960610     C                   ENDSR
012300960610     C/SPACE 005
012400960610     C**---------------------------------------------------------
012500960610     C** ESEGUE QCAEXEC DI CHIUSURA SUL MEMBRO DI EDTAB00R      -
012600960610     C**---------------------------------------------------------
012700960610     C     CAEXB         BEGSR
012800960610     C                   MOVE      ')'           VAR11            11
012900960610     C                   MOVEL     MBRFIL        VAR11
013000960610     C                   MOVE      '))'          VAR12            12
013100960610     C                   MOVEL     MBRFIL        VAR12
013200960610     C                   Z-ADD     55            LUNG             15 5
013300960610     C*
013400960610     C                   CLOSE     EDTAB00R
013500960610     C*
013600960610     C                   MOVE      VAR12         CMD5
013700960610     C                   MOVEL     *BLANKS       COMMAN
013800960610     C                   MOVEA     CMD5(1)       COMMAN
013900960610     C                   CALL      'QCMDEXC'                            20
014000960610     C                   PARM                    COMMAN
014100960610     C                   PARM                    LUNG
014200960610     C** DISALLOCA IL MEMBRO AGG.TO
014300960610     C                   ENDSR
014400960610     C*---------------------------------------------------------------------*
014500960610     C*  *INZSR: Operazioni iniziali
014600960610     C*---------------------------------------------------------------------*
014700960610     C     *INZSR        BEGSR
014800960610     C*
014900960610     C     *ENTRY        PLIST
015000960610     C                   PARM                    KPJBA
015100960610     C*  Definisco chiavi di accesso
015200960610     C     KTAB          KLIST
015300960610     C                   KFLD                    KCOD
015400960610     C                   KFLD                    KKEY
015500960610     C*  Definisco variabili
015600960610     C     *LIKE         DEFINE    TABCOD        KCOD
015700960610     C     *LIKE         DEFINE    TABKEY        KKEY
015800960610     C*  Reperisco data del giorno
015900960610     C                   TIME                    WHHDAT           14 0
016000960610     C                   MOVE      WHHDAT        G02DAT
016100960610     C                   MOVEL     *BLANK        G02ERR
016200960610     C                   CALL      'XSRDA8'
016300960610     C                   PARM                    WLBDA8
016400960610     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
016500960610     C*
016600960610     C                   ENDSR
016700960610**         CMD1
016800960610CHKOBJ OBJ(EDTAB00R) OBJTYPE(*FILE)     MBR(M234567890)
016900960610**         CMD2
017000960610ALCOBJ OBJ((EDTAB00R *FILE *EXCL           M234567890))
017100960610**         CMD3
017200960610OVRDBF FILE(EDTAB00R)                   MBR(M234567890)
017300960610**         CMD5
017400960610DLCOBJ OBJ((EDTAB00R *FILE *EXCL           M234567890))
