000100010205     H DECEDIT('0,') DATEDIT(*DMY.)
000200010205     FTITAS30C  IF   E           K DISK
000300020619     F                                     IGNORE(TITASP00)
000400010205     FECERR03L  IF   E           K DISK
000500010205     F                                     RENAME(ECERR:ECERR3)
000600010205     FECERR02L  UF   E           K DISK
000700010205     F                                     RENAME(ECERR:ECERR2)
000800010205     D*------------------------------------------------------------------------*
000900010205     D* SCHIERE
001000010205     D*------------------------------------------------------------------------*
001100010205     D*------------------------------------------------------------------------*
001200010205     D* INPUT
001300010205     D*------------------------------------------------------------------------*
001400010205     D*-------------------
001500010205     D* DS ESTERNA PER GUFO
001600010205     D*-------------------
001700010205     D DS£ECO        E DS
001800010205     D*-------------------
001900010205     D* DS VALORIZZAZIONE IMMAGINE ERRORE DA TITAS
002000010205     D*-------------------
002100010205     D TRECDS        E DS
002200010205     D  D$PDS1                 1    250
002300010205     D  D$PDS2               251    500
002400010205     D  D$DDS1               501    750
002500010205     D  D$DDS2               751   1000
002600010205     D*-------------------
002700010205     D* DS VALORIZZAZIONE IMMAGINE ERRORE DA TNASC
002800010205     D*-------------------
002900010205     D DS$CSB        E DS
003000010205     D  DCPDS1                 1    250
003100010205     D  DCPDS2               251    500
003200010205     D  DCDDS1               501    750
003300010205     D  DCDDS2               751   1000
003400010205     D*-------------------
003500010205     D* SCOMPOSIZIONI DESCRIZIONE ERRORE
003600010205     D*-------------------
003700010205     D                 DS
003800010205     D  TAR                   29     31
003900010205     D  CTS                   41     42
004000010205     D  LNP                   33     35
004100010205     D  LNA                   37     39
004200010205     D  ERRDER                 1     80
004300010205     C*------------------------------------------------------------------------*
004400010205     C* MAIN LINES
004500010205     C*------------------------------------------------------------------------*
004600010205     C***
004700010205     C* LEGGE GLI ERRORI
004800010205     C***
004900010205     C     *LOVAL        SETLL     ECERR03L
005000010205     C                   READ      ECERR03L                               99
005100010205DO  1C     *IN99         DOWEQ     *OFF
005200010205     C*
005300010205     C* CONTROLLA SE ERRORE DA CONSIDERARE
005400010205     C                   EXSR      CHKERR
005500010205IF  2C     WRECOK        IFEQ      'S'
005600010205     C***
005700010205     C* LEGGE LA BOLLA
005800010205     C***
005900010205     C                   Z-ADD     ERRAAS        KASAAS
006000010205     C                   Z-ADD     ERRLNP        KASLNP
006100010205     C                   Z-ADD     ERRNRS        KASNRS
006200010205     C                   Z-ADD     ERRNSP        KASNSP
006300010205     C                   MOVEL     ERRTBL        KASTBL
006400010205     C     KEYTAS        CHAIN     TITAS30C                           98
006500010205IF  3C     *IN98         IFEQ      *OFF
006600010205     C                   EXSR      APDTAQ
006700010205     C                   EXSR      CDDTAQ
006800010205     C                   MOVEL     D$PDS1        ERRRP1
006900010205     C                   MOVEL     D$PDS2        ERRRP2
007000010205     C                   MOVEL     D$DDS1        ERRRD1
007100010205     C                   MOVEL     D$DDS2        ERRRD2
007200010205     C***
007300010205     C* RILANCIA IL CONTO ECONOMICO
007400010205     C***
007500010205     C                   EXSR      GUFO
007600010205     C***
007700010205     C* CANCELLA GLI ERRORI DELLA BOLLA
007800010205     C***
007900010205     C                   Z-ADD     TASAAS        KRRAAS
008000010205     C                   Z-ADD     TASLNP        KRRLNP
008100010205     C                   Z-ADD     TASNRS        KRRNRS
008200010205     C                   Z-ADD     TASNSP        KRRNSP
008300010205     C                   MOVEL     TASTBL        KRRTBL
008400010205     C     KEYERR        SETLL     ECERR02L
008500010205     C     KEYERR        READE     ECERR02L                               97
008600010205DO  4C     *IN97         DOWEQ     *OFF
008700010205     C                   DELETE    ECERR2
008800010205     C     KEYERR        READE     ECERR02L                               97
008900010205E   4C                   ENDDO
009000010205E   3C                   ENDIF
009100010205E   2C                   ENDIF
009200010205     C*
009300010205     C* LETTURA SUCCESSIVA ERRORI DA RILANCIARE
009400010205     C                   READ      ECERR03L                               99
009500010205E   1C                   ENDDO
009600010205     C*
009700010205     C                   SETON                                        LR
009800010205     C*------------------------------------------------------------------------*
009900010205     C* GUFO - LANCIA IL PGM CHE RIMETTE LA BOLLA IL CODA PER ESSERE ELABORATA
010000010205     C*------------------------------------------------------------------------*
010100010205     C     GUFO          BEGSR
010200010205     C*
010300010205     C* LANCIA IL PROGRAMMA DI ACCODAMENTO
010400010205     C                   MOVEL     ERRSTS        D£GSTS
010500010205     C                   Z-ADD     ERRDAM        D£GDAT
010600010205     C                   Z-ADD     ERRORM        D£GTIM
010700010205     C                   MOVEL     ERRUSR        D£GUSR
010800010205     C                   MOVEL     ERRPGF        D£GPGF
010900010205     C                   MOVEL     ERRFIL        D£GFIL
011000010205     C                   MOVEL     ERRMBR        D£GMBR
011100010205     C                   MOVEL     ERRLIB        D£GLIB
011200010205     C                   MOVEL     'S'           D£GPDE
011300020808     C                   MOVEL     'B'           D£GTJB
011400010205     C                   MOVEL     *BLANKS       D£GXXX
011500010205     C                   MOVEL     ERRRP1        D£PRE1
011600010205     C                   MOVEL     ERRRP2        D£PRE2
011700010205     C                   MOVEL     ERRRD1        D£DRE1
011800010205     C                   MOVEL     ERRRD2        D£DRE2
011900010205     C*
012000020619     C                   CALL      'TRECD2R'
012100010205     C                   PARM                    DS£ECO
012200010205     C*
012300010205     C                   ENDSR
012400010205     C*------------------------------------------------------------------------*
012500010205     C* CHKERR - CONTROLLA VALIDITA' ERRORE
012600010205     C*------------------------------------------------------------------------*
012700010205     C     CHKERR        BEGSR
012800010205     C*
012900010205     C                   MOVEL     'S'           WRECOK            1
013000020805     C*
013100020805     C* SOLO COME INSERIMENTO !!!!!
013200020805IF  1C     ERRSTS        IFNE      'I'
013300020805     C                   MOVEL     'N'           WRECOK
013400020805E   1C                   ENDIF
013500010205     C*
013600030206IF  1C     ERRCER        IFEQ      'P50V'
013700030206     C                   MOVEL     'N'           WRECOK
013800030206E   1C                   ENDIF
013900010205     C*
014000010205     C                   ENDSR
014100010205     C*------------------------------------------------------------------------*
014200010205     C*  *INZSR - ROUTINE INIZIALE
014300010205     C*------------------------------------------------------------------------*
014400010205     C     *INZSR        BEGSR
014500010205     C*--------------------
014600010205     C* CAMPI RIFERITI AL DATA BASE
014700010205     C*--------------------
014800010205     C     *LIKE         DEFINE    ERRAAS        KRRAAS
014900010205     C     *LIKE         DEFINE    ERRLNP        KRRLNP
015000010205     C     *LIKE         DEFINE    ERRNRS        KRRNRS
015100010205     C     *LIKE         DEFINE    ERRNSP        KRRNSP
015200010205     C     *LIKE         DEFINE    ERRTBL        KRRTBL
015300010205     C     *LIKE         DEFINE    TASAAS        KASAAS
015400010205     C     *LIKE         DEFINE    TASLNP        KASLNP
015500010205     C     *LIKE         DEFINE    TASNRS        KASNRS
015600010205     C     *LIKE         DEFINE    TASNSP        KASNSP
015700010205     C     *LIKE         DEFINE    TASTBL        KASTBL
015800010205     C*--------------------
015900010205     C* CHIAVI DI LETTURA
016000010205     C*--------------------
016100010205     C* ECERR02L - PARZIALE
016200010205     C     KEYERR        KLIST
016300010205     C                   KFLD                    KRRAAS
016400010205     C                   KFLD                    KRRLNP
016500010205     C                   KFLD                    KRRNRS
016600010205     C                   KFLD                    KRRNSP
016700010205     C                   KFLD                    KRRTBL
016800010205     C* TITAS30C - COMPLETA
016900010205     C     KEYTAS        KLIST
017000010205     C                   KFLD                    KASAAS
017100010205     C                   KFLD                    KASLNP
017200010205     C                   KFLD                    KASNRS
017300010205     C                   KFLD                    KASNSP
017400010205     C                   KFLD                    KASTBL
017500010205     C*
017600010205     C                   ENDSR
017700010205     C*------------------------------------------------------------------------*
017800010205     C* ROUTINE GENERALIZZATA PASSAGGIO IMMAGINI BOLLA DI SEDE
017900010205     C*------------------------------------------------------------------------*
018000010205      /COPY GAITRASRC/SRCTREC,TRECDT
