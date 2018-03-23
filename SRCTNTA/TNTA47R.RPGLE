000100980415     H*------------------------------------------------------------------------*
000200980415     H* ANNULLAMENTO DATI TIPO TARIFFE DA CONTO ECONOMICO - ESEGUE             *
000300980415     H*------------------------------------------------------------------------*
000400980415     H DECEDIT('0,') DATEDIT(*DMY.)
000500980415     F*------------------------------------------------------------------------*
000600980415     F* DATA BASE                                                              *
000700980415     F*------------------------------------------------------------------------*
000800980415     F*---
000900980415     F* IN LETTURA
001000980415     F*---
001100011001     FTEETC01L  IF   E           K DISK
001200980415     F                                     INFDS(INFETC)
001300011001     F                                     RENAME(TEETC000:TEETC)
001400011001     FTEETS01L  IF   E           K DISK
001500980415     F                                     INFDS(INFETS)
001600011001     F                                     RENAME(TEETS000:TEETS)
001601061009     FTEETS11L  IF   E           K DISK
001602061009     F                                     INFDS(INFETS1)
001603061009     F                                     RENAME(TEETS000:TEETS11)
001700011001     FTEETT01L  IF   E           K DISK
001800980415     F                                     INFDS(INFETT)
001900011001     F                                     RENAME(TEETT000:TEETT)
002000980415     F*---
002100980415     F* IN AGGIORNAMENTO
002200980415     F*---
002300011001     FTEETC00F  UF   E             DISK
002400011001     FTEETS00F  UF   E             DISK
002401061009     FTEETS10F  UF   E             DISK
002402061009     F                                     RENAME(TEETS000:TEETS10)
002500011001     FTEETT00F  UF   E             DISK
002600980415     D*------------------------------------------------------------------------*
002700980415     D* INPUT
002800980415     D*------------------------------------------------------------------------*
002900980415     D*-------------
003000980415     D* DS INFORMAZIONI
003100980415     D*-------------
003200980415     D INFETC          DS
003300980415     D  NRRETC               397    400B 0
003400980415     D INFETS          DS
003500980415     D  NRRETS               397    400B 0
003501061009     D INFETS1         DS
003502061009     D  NRRETS1              397    400B 0
003600980415     D INFETT          DS
003700980415     D  NRRETT               397    400B 0
003800980415     D*-------------
003900980415     D* DS SCOMPOSIZIONE DATA (8)
004000980415     D*-------------
004100980415     D WLBDA8          DS                  INZ
004200980415     D  G08DAT                 1      8  0
004300980415     D  G08INV                 9     16  0
004400980415     D  G08ERR                17     17
004500980415     D  G08TGI                18     22  0
004600980415     D*-------------
004700980415     D* DS DI PROCEDURA
004800980415     D*-------------
004900980415     D DSTA47        E DS                  EXTNAME(TNTA47DS)
005000980415     D*-------------
005100980415     D* ARCHITETTURA
005200980415     D*-------------
005300980415     D KPJBA         E DS
005400980415     C*------------------------------------------------------------------------*
005500980415     C* MAIN LINES                                                             *
005600980415     C*------------------------------------------------------------------------*
005700040308     C* RICEVIMENTO PARAMETRI
005800040308     C     *ENTRY        PLIST
005900040308     C                   PARM                    KPJBA
006000040308     C                   MOVEL     KPJBU         DSTA47
006100980415     C*
006200980415IF  1C     D47TLA        IFNE      'C'
006300040303     C* IMPOSTA LA CHIAVE DI LETTURA
006400040303     C                   MOVEL     D47DSF        KTXDSF
006500040303     C                   MOVEL     D47CTO        KTXCTO
006600040303     C                   Z-ADD     D47KSC        KTXKSC
006700040303     C                   Z-ADD     D47CTR        KTXCTR
006800040303     C                   Z-ADD     D47PRG        KTXPRG
006900040303     C*
007000980415IF  2C     D47TUP        IFEQ      '1'
007100980415     C     D47TUP        OREQ      '2'
007200980415     C     D47TUP        OREQ      '4'
007300980415     C*---
007400980415     C* DATI TIPO GENERALI
007500980415     C*---
007600011001     C     KEYETX        SETLL     TEETC01L
007700011001     C     KEYETX        READE     TEETC01L                               99
007800980415DO  3C     *IN99         DOWEQ     *OFF
007900011001     C     NRRETC        CHAIN     TEETC00F                           98
008000980415IF  4C     D47TUP        IFEQ      '2'
008100011001     C                   DELETE    TEETC000
008200980415X   4C                   ELSE
008300980415     C                   MOVEL     'A'           ETCATB
008400980415     C                   MOVEL     DATCOR        ETCDUV
008500980415     C                   MOVEL     *BLANKS       ETCFTR
008600980415     C                   Z-ADD     *ZEROS        ETCDTR
008700011001     C                   UPDATE    TEETC000
008800980415E   4C                   ENDIF
008900011001     C     KEYETX        READE     TEETC01L                               99
009000980415E   3C                   ENDDO
009100980415     C*---
009200061009     C* DATI TIPO SCAGLIONI x ricavi
009300980415     C*---
009400011001     C     KEYETX        SETLL     TEETS01L
009500011001     C     KEYETX        READE     TEETS01L                               99
009600980415DO  3C     *IN99         DOWEQ     *OFF
009700011001     C     NRRETS        CHAIN     TEETS00F                           98
009800980415IF  4C     D47TUP        IFEQ      '2'
009900011001     C                   DELETE    TEETS000
010000980415X   4C                   ELSE
010100980415     C                   MOVEL     'A'           ETSATB
010200980415     C                   MOVEL     DATCOR        ETSDUV
010300980415     C                   MOVEL     *BLANKS       ETSFTR
010400980415     C                   Z-ADD     *ZEROS        ETSDTR
010500011001     C                   UPDATE    TEETS000
010600980415E   4C                   ENDIF
010700011001     C     KEYETX        READE     TEETS01L                               99
010800980415E   3C                   ENDDO
010801061009     C*---
010802061009     C* DATI TIPO SCAGLIONI x competenze
010803061009     C*---
010804061009     C     KEYETX        SETLL     TEETS11L
010805061009     C     KEYETX        READE     TEETS11L                               99
010806061009DO  3C     *IN99         DOWEQ     *OFF
010807061009     C     NRRETS1       CHAIN     TEETS10F                           98
010808061009IF  4C     D47TUP        IFEQ      '2'
010809061009     C                   DELETE    TEETS10
010810061009X   4C                   ELSE
010811061009     C                   MOVEL     'A'           ETSATB
010812061009     C                   MOVEL     DATCOR        ETSDUV
010813061009     C                   MOVEL     *BLANKS       ETSFTR
010814061009     C                   Z-ADD     *ZEROS        ETSDTR
010815061009     C                   UPDATE    TEETS10
010816061009E   4C                   ENDIF
010817061009     C     KEYETX        READE     TEETS11L                               99
010818061009E   3C                   ENDDO
010900980415     C*---
011000980415     C* DATI TIPO CODICI TASSAZIONE
011100980415     C*---
011200011001     C     KEYETX        SETLL     TEETT01L
011300011001     C     KEYETX        READE     TEETT01L                               99
011400980415DO  3C     *IN99         DOWEQ     *OFF
011500011001     C     NRRETT        CHAIN     TEETT00F                           98
011600980415IF  4C     D47TUP        IFEQ      '2'
011700011001     C                   DELETE    TEETT000
011800980415X   4C                   ELSE
011900980415     C                   MOVEL     'A'           ETTATB
012000980415     C                   MOVEL     DATCOR        ETTDUV
012100980415     C                   MOVEL     *BLANKS       ETTFTR
012200980415     C                   Z-ADD     *ZEROS        ETTDTR
012300011001     C                   UPDATE    TEETT000
012400980415E   4C                   ENDIF
012500011001     C     KEYETX        READE     TEETT01L                               99
012600980415E   3C                   ENDDO
012700980415     C*
012800980415X   2C                   ELSE
012900980415     C*---
013000980415     C* DATI TIPO GENERALI
013100980415     C*---
013200011001     C     KE2ETX        SETLL     TEETC01L
013300011001     C     KE2ETX        READE     TEETC01L                               99
013400980415DO  3C     *IN99         DOWEQ     *OFF
013500011001     C     NRRETC        CHAIN     TEETC00F                           98
013600011001     C                   DELETE    TEETC000
013700011001     C     KE2ETX        READE     TEETC01L                               99
013800980415E   3C                   ENDDO
013900980415     C*---
014000061009     C* DATI TIPO SCAGLIONI x ricavi
014100980415     C*---
014200011001     C     KE2ETX        SETLL     TEETS01L
014300011001     C     KE2ETX        READE     TEETS01L                               99
014400980415DO  3C     *IN99         DOWEQ     *OFF
014500011001     C     NRRETS        CHAIN     TEETS00F                           98
014600011001     C                   DELETE    TEETS000
014700011001     C     KE2ETX        READE     TEETS01L                               99
014800980415E   3C                   ENDDO
014801061009     C*---
014802061009     C* DATI TIPO SCAGLIONI x competenze
014803061009     C*---
014804061009     C     KE2ETX        SETLL     TEETS11L
014805061009     C     KE2ETX        READE     TEETS11L                               99
014806061009DO  3C     *IN99         DOWEQ     *OFF
014807061009     C     NRRETS1       CHAIN     TEETS10F                           98
014808061009     C                   DELETE    TEETS10
014809061009     C     KE2ETX        READE     TEETS11L                               99
014810061009E   3C                   ENDDO
014900980415     C*---
015000980415     C* DATI TIPO CODICI TASSAZIONE
015100980415     C*---
015200011001     C     KE2ETX        SETLL     TEETT01L
015300011001     C     KE2ETX        READE     TEETT01L                               99
015400980415DO  3C     *IN99         DOWEQ     *OFF
015500011001     C     NRRETT        CHAIN     TEETT00F                           98
015600011001     C                   DELETE    TEETT000
015700011001     C     KE2ETX        READE     TEETT01L                               99
015800980415E   3C                   ENDDO
015900980415     C*
016000980415E   2C                   ENDIF
016100980415E   1C                   ENDIF
016200980415     C*
016300980415     C* CHIUSURA PROGRAMMA
016400980415IF  1C     D47TLA        IFEQ      *BLANKS
016500980415     C                   RETURN
016600980415X   1C                   ELSE
016700980415     C                   SETON                                        LR
016800980415E   1C                   ENDIF
016900980415     C*------------------------------------------------------------------------*
017000980415     C* *INZSR - OPERAZIONI INIZIALI                                           *
017100980415     C*------------------------------------------------------------------------*
017200980415     C     *INZSR        BEGSR
017300980415     C*
017400980415     C* VARIABILI RIFERITE AL DATABASE
017500011001     C     *LIKE         DEFINE    ETCDSF        KTXDSF
017600980526     C     *LIKE         DEFINE    ETCCTO        KTXCTO
017700980415     C     *LIKE         DEFINE    ETCKSC        KTXKSC
017800980415     C     *LIKE         DEFINE    ETCCTR        KTXCTR
017900980415     C     *LIKE         DEFINE    ETCPRG        KTXPRG
018000980415     C*
018100980415     C* CHIAVI DI LETTURA
018200011001     C     KEYETX        KLIST
018300980526     C                   KFLD                    KTXDSF
018400980526     C                   KFLD                    KTXCTO
018500980415     C                   KFLD                    KTXKSC
018600980415     C                   KFLD                    KTXCTR
018700980415     C                   KFLD                    KTXPRG
018800011001     C     KE2ETX        KLIST
018900980526     C                   KFLD                    KTXDSF
019000980526     C                   KFLD                    KTXCTO
019100980526     C                   KFLD                    KTXKSC
019200980415     C*
019300980415     C* REPERIMENTO DATA CORRENTE
019400980415     C                   TIME                    N14              14 0
019500980415     C                   MOVE      N14           N8                8 0
019600980415     C                   Z-ADD     N8            G08DAT
019700980415     C                   Z-ADD     *ZEROS        G08INV
019800980415     C                   MOVEL     '0'           G08ERR
019900980415     C                   CALL      'XSRDA8'
020000980415     C                   PARM                    WLBDA8
020100980415     C                   Z-ADD     G08INV        DATCOR            8 0
020200980415     C*
020300980415     C                   ENDSR
