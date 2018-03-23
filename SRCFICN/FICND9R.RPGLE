000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011114     H* FICND9R *----------------------------------------------------*
000300000000     H*-----------------*                                            *
000400931202     H*         SVALORIZZAZIONE SIMULAZIONE PADRONCINI               *
000500000000     H*--------------------------------------------------------------*
000600011114     Ffifst02L  UF   E           K DISK
000700011114     F                                     RENAME(fiftt000:fifst000)
000800011114     Ffifsd01L  UF   E           K DISK
000900011114     F                                     RENAME(fiftd000:fifsd000)
001000931202     FTABEL00F  IF   E           K DISK
001100930707     F*
001200931202     D TSR             S              1    DIM(30)                              TIPI PRESTAZ
001300931129     D PARAM           DS
001400931202     D* CODICE PADRONCINO
001500931202     D  PARPDR                 1      7  0
001600931202     D* TIPO PRESTAZIONE ' ' = TUTTI
001700931202     D  PARTSR                11     11
001800950126     D* DATA DISTINTE/RITIRI  (AAAAMMGG)
001900950126     D  PARDDC                12     19  0
002000931129     D*
002100931202     D DS1Z          E DS
002200931202     D KPJBA         E DS
002300900521     C*****************************************************************
002400000000     C     *ENTRY        PLIST
002500000000     C                   PARM                    KPJBA
002600931130     C                   MOVEL     KPJBU         PARAM
002700900511     C*---------------------------------------------------------------*
002800950126     C* ACCESSO TABEL00F
002900931202     C     KTAB2         KLIST
003000931202     C                   KFLD                    CODUT
003100931202     C                   KFLD                    COD               2
003200011114     C* ACCESSO fifst02L
003300931202     C     KFTT2         KLIST
003400931202     C                   KFLD                    PARPDR
003500931202     C                   KFLD                    COMFVL
003600931202     C                   KFLD                    PARDDC
003700931202     C                   KFLD                    PARTSR
003800011114     C* ACCESSO fifsd01L
003900931202     C     KFTD          KLIST
004000931202     C                   KFLD                    FTTPDR
004100931202     C                   KFLD                    FTTTSR
004200931202     C                   KFLD                    FTTNDC
004300950126     C                   KFLD                    FTTDDC
004400931202     C*
004500931202     C* DEFINIZIONE CAMPI
004600931202     C     *LIKE         DEFINE    FTTFVL        COMFVL
004700931116     C*---------------------------------------------------------------*
004800931202     C                   MOVEL     'C'           COMFVL
004900931202     C*
005000931202     C     PARTSR        IFNE      ' '
005100931202     C                   MOVEL     PARTSR        TSR(1)
005200931202     C                   ELSE
005300931202     C                   EXSR      CARTAB
005400931202     C                   ENDIF
005500931202     C* SEMPRE
005600931202     C* PARTSR = *BLANK ===> DELETO RECORD DI TOTALI
005700931202     C     PARTSR        IFEQ      ' '
005800011114     C     KFTT2         CHAIN     fifst000                           30
005900011114     C  N30              DELETE    fifst000
006000931202     C                   ENDIF
006100931202     C*
006200931202     C                   Z-ADD     1             C
006300931202    1C     TSR(C)        DOWNE     ' '
006400931202     C*
006500931202     C                   MOVEL     TSR(C)        PARTSR
006600011114     C     KFTT2         SETLL     fifst000
006700011114     C     KFTT2         READE     fifst000                               30
006800931202     C*
006900931202    2C     *IN30         DOWEQ     *OFF
007000011114     C** SVALORIZZO fifst
007100931202     C                   EXSR      SVLFTT
007200011114     C                   UPDATE    fifst000
007300931202     C*
007400011114     C     KFTD          SETLL     fifsd000
007500011114     C     KFTD          READE     fifsd000                               31
007600931202     C*
007700931202    3C     *IN31         DOWEQ     *OFF
007800011114     C** SVALORIZZO fifsd
007900931202     C                   EXSR      SVLFTD
008000931202     C*
008100011114     C                   UPDATE    fifsd000
008200931130     C*
008300011114     C     KFTD          READE     fifsd000                               31
008400931202    3C                   ENDDO
008500931202     C*
008600011114     C     KFTT2         READE     fifst000                               30
008700931202    2C                   ENDDO
008800900611     C*
008900931202     C                   ADD       1             C
009000931202    1C                   ENDDO
009100931202     C*
009200931027     C                   SETON                                        LR
009300931111     C*
009400011114     C*--- SVALORIZZO RECORD DI fifst00F -----------------------------*
009500931202     C     SVLFTT        BEGSR
009600931202     C                   CLEAR                   FTTFVL
009700931202     C                   CLEAR                   FTTSNP
009800931202     C                   CLEAR                   FTTSNA
009900950126     C                   CLEAR                   FTTCPE
010000950126     C                   CLEAR                   FTTSNE
010100931202     C                   CLEAR                   FTTCLA
010200931202     C                   CLEAR                   FTTKFA
010300931202     C                   CLEAR                   FTTTVL
010400011114     C******** ?????     clear                   fttdiv
010500931202     C                   CLEAR                   FTTITT
010600931202     C                   CLEAR                   FTTFIT
010700931202     C                   CLEAR                   FTTITA
010800931202     C                   CLEAR                   FTTFIA
010900931202     C                   CLEAR                   FTTTPE
011000931202     C                   CLEAR                   FTTTBN
011100931202     C                   CLEAR                   FTTTIM
011200931202     C                   CLEAR                   FTTMNT
011300950126     C                   CLEAR                   FTTDCV
011400931129     C                   ENDSR
011500931201     C*
011600011114     C*--- SVALORIZZO RECORD DI fifsd00F -----------------------------*
011700931202     C     SVLFTD        BEGSR
011800931202     C                   CLEAR                   FTDCTP
011900931202     C                   CLEAR                   FTDITT
012000931202     C                   CLEAR                   FTDFIT
012100931202     C                   CLEAR                   FTDITA
012200931202     C                   CLEAR                   FTDFIA
012300931202     C                   ENDSR
012400931202     C*
012500931202     C*--- CARICO TABELLE --------------------------------------------*
012600931202     C     CARTAB        BEGSR
012700931202     C                   Z-ADD     1             CODUT             1 0
012800931202     C***
012900931202     C* CARICO TABELLA DEI TIPI PRESTAZIONE
013000931202     C***
013100931202     C                   Z-ADD     1             C                 3 0
013200931202     C                   MOVEL     '1Z'          COD               2
013300931202     C     KTAB2         SETLL     TABEL
013400931202     C     KTAB2         READE     TABEL                                  30
013500931202     C*
013600931202     C     *IN30         DOWEQ     *OFF
013700931202     C     TBLFLG        IFEQ      ' '
013800931202     C                   MOVEL     TBLUNI        DS1Z
013900931202     C     §1ZUPD        IFEQ      'S'
014000931202     C                   MOVEL     TBLKEY        TSR(C)
014100931202     C                   ADD       1             C
014200931202     C                   ENDIF
014300931202     C                   ENDIF
014400931202     C     KTAB2         READE     TABEL                                  30
014500931202     C                   ENDDO
014600931202     C                   ENDSR
