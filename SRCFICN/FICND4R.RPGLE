000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011114     H* FICND4R *----------------------------------------------------*
000300000000     H*-----------------*                                            *
000400931129     H*         CREAZIONE FILE DI SIMULAZIONE PADRONCINI  - BATCH -
000500000000     H*--------------------------------------------------------------*
000600011114     Ffiftt01L  IF   E           K DISK
000700011114     F                                     RENAME(fiftt000:fiftt001)
000800011114     Ffiftt04L  IF   E           K DISK
000900011114     F                                     RENAME(fiftt000:fiftt004)
001000011114     Ffiftd03L  IF   E           K DISK
001100011114     Ffifst01L  UF A E           K DISK
001200011114     F                                     RENAME(fiftt000:fifst000)
001300011114     Ffifsd00F  O  A E           K DISK
001400011114     F                                     RENAME(fiftd000:fifsd000)
001500930707     F*
001600931129     D PARAM           DS
001700931129     D* TIPO PRESTAZIONE
001800931129     D  PARTSR                 1      1
001900931129     D* TIPO DISTINTE/RITIRI
002000931129     D  PARTVL                 2      2
002100931129     D* CODICE PADRONCINO DAL
002200931129     D  PARFP1                 3      5  0
002300931129     D  PARCP1                 6      9
002400931129     D  PARCPD                 3      9  0
002500931129     D* CODICE PADRONCINO AL
002600931129     D  PARFP2                10     12  0
002700931129     D  PARCP2                13     16
002800931129     D  PARCPA                10     16  0
002900950125     D* DATA DISTINTE/RITIRI DAL (AAAAMMGG)
003000950125     D  PARDRD                17     24  0
003100950125     D* DATA DISTINTE/RITIRI AL  (AAAAMMGG)
003200950125     D  PARDRA                25     32  0
003300940325     D*
003400931129     D KPJBA         E DS
003500900521     C****************************************************************
003600900521     C*  RIEPILOGO INDICATORI
003700900521     C***************************************************************
003800931129     C* 01    - RICHIESTI TUTTI I TIPI PRESTAZIONE
003900931130     C* 02    - RICHIESTE SIA LE VALORIZZATE CHE QUELLE DA VALORIZZARE
004000931129     C* 03    - RICHIESTE QUELLE DA VALORIZZARE (FTTFVL===>' ' o 'B')
004100931129     C* 04    - RICHIESTO IL CODICE PADRONCINO AL
004200931130     C* 05    - RICHIESTA LA DATA DISTINTE/RITIRI AL
004300931201     C* 07    - NON ESEGUO ESCLUSIONI PER IL FLAG VALORIZZAZ. PERCHE'
004400931201     C*         NEL CASO VENGANO SCELTE SOLO LE VALORIZZATE USO LA
004500011114     C*         VISTA LOGICA fiftt04L
004600931201     C* 08    - OFF  ESEGUO SETLL PER POSIZIONAMENTO INIZIO LETTURA
004700931130     C* 30/32 - DI COMODO
004800900521     C*****************************************************************
004900000000     C     *ENTRY        PLIST
005000000000     C                   PARM                    KPJBA
005100931130     C                   MOVEL     KPJBU         PARAM
005200900511     C*---------------------------------------------------------------*
005300011114     C* ACCESSO fiftt01L
005400931130     C     KFTT1         KLIST
005500931130     C                   KFLD                    PARCPD
005600931130     C                   KFLD                    PARTSR
005700011114     C* ACCESSO fiftt04L
005800931201     C     KFTT2         KLIST
005900931130     C                   KFLD                    PARTVL
006000931130     C                   KFLD                    PARDRD
006100931130     C                   KFLD                    PARCPD
006200931201     C     KFTT3         KLIST
006300931201     C                   KFLD                    PARTVL
006400931201     C                   KFLD                    PARDRD
006500011114     C* ACCESSO fiftd03L/fifst01L
006600931130     C     KFTD          KLIST
006700931130     C                   KFLD                    FTTPDR
006800931130     C                   KFLD                    FTTTSR
006900931130     C                   KFLD                    FTTNDC
007000950125     C                   KFLD                    FTTDDC
007100931116     C*---------------------------------------------------------------*
007200931201     C** CONTROLLO RICHIESTE FATTE
007300931129     C                   EXSR      CONTR
007400931129     C*
007500931201     C** IMPOSTAZIONE LETTURE
007600931201     C                   EXSR      IMPO
007700931130     C*
007800931201    1C     *IN30         DOWEQ     *OFF
007900931130     C*
008000931201     C**ESEGUO  ESCLUSIONI
008100931201     C                   EXSR      ESCLUS
008200931130     C*
008300931201    2C     *IN30         IFEQ      *OFF
008400011114     C** SE IL RECORD IN fifst00F ESISTE GIA' NON RISCRIVO I FILE DI
008500931201     C**   SIMULAZIONE
008600011114     C     KFTD          SETLL     fifst000                               31
008700931130     C*
008800011114     C** DUPLICA DI fifst00F
008900931201    3C     *IN31         IFEQ      *OFF
009000960802     C* BLOCCO MOMENTANEAMENTE LA TESTATA (FTTFVL="B") IN MODO CHE NON
009100960802     C*   SI RIESCA A LAVORARE SUL PADRONCINO FINCHE' NON E' STATO
009200960802     C*   DUPLICATO ANCHE TUTTO IL SUO DETTAGLIO
009300960802     C                   MOVEL     'B'           FTTFVL
009400960802     C*
009500951211     C                   CLEAR                   FTTFLA
009600951211     C                   CLEAR                   FTTSNA
009700951211     C                   CLEAR                   FTTCLA
009800951211     C                   CLEAR                   FTTCPE
009900951211     C                   CLEAR                   FTTSNE
010000951211     C                   CLEAR                   FTTSNP
010100951211     C                   CLEAR                   FTTKFA
010200951211     C                   CLEAR                   FTTTVL
010300951211     C                   CLEAR                   FTTITT
010400951211     C                   CLEAR                   FTTFIT
010500951211     C                   CLEAR                   FTTITA
010600951211     C                   CLEAR                   FTTFIA
010700011114     C****** ??????      clear                   fttdiv
010800011114      * >>>>> divisa <-> fttdiv  <<<<<<
010900940325     C*
011000011114     C                   WRITE     fifst000
011100931130     C*
011200011114     C** DUPLICA DI fifsd00F
011300931130     C                   SETOFF                                           32
011400011114     C     KFTD          SETLL     fiftd000
011500940325    4C     *IN32         DOWEQ     *OFF
011600011114     C     KFTD          READE     fiftd000                               32
011700951211     C                   CLEAR                   FTDCTP
011800951211     C                   CLEAR                   FTDITT
011900951211     C                   CLEAR                   FTDFIT
012000951211     C                   CLEAR                   FTDITA
012100951211     C                   CLEAR                   FTDFIA
012200011114     C  N32              WRITE     fifsd000
012300940325    4C                   ENDDO
012400940325     C*
012500960802     C* UNA VOLTA COPIATO IL DETTAGLIO, SBLOCCO LA TESTATA (FTTFVL=" ")
012600011114     C     KFTD          CHAIN     fifst000                           33
012700960802     C     *IN33         IFEQ      *OFF
012800960802     C                   CLEAR                   FTTFVL
012900011114     C                   UPDATE    fifst000
013000960802     C                   ENDIF
013100960802     C*
013200931201    3C                   ENDIF
013300931201    2C                   ENDIF
013400931130     C*
013500931201     C** LETTURE
013600931201     C                   SETON                                        08
013700931201     C                   EXSR      IMPO
013800931130     C*
013900931201    1C                   ENDDO
014000900611     C*
014100931027     C                   SETON                                        LR
014200931111     C*
014300931129     C*--- CONTROLLO RICHIESTE FATTE ---------------------------------*
014400931129     C     CONTR         BEGSR
014500910320     C*
014600931129     C****  TIPO PRESTAZIONE  ****
014700931129     C* 01 ON  - RICHIESTI TUTTI I TIPI PRESTAZIONE
014800931129     C     PARTSR        IFEQ      ' '
014900931129     C                   SETON                                        01
015000931129     C                   ENDIF
015100931129     C*
015200931129     C****  TIPO DISTINTE/RITIRI  ****
015300931130     C     PARTVL        IFEQ      'T'
015400931130     C* 02 ON  - RICHIESTE SIA LE VALORIZZATE CHE QUELLE DA VALORIZZARE
015500931130     C                   SETON                                        02
015600931130     C                   ELSE
015700931129     C     PARTVL        IFEQ      ' '
015800931130     C* 03 ON  - RICHIESTE QUELLE DA VALORIZZARE (FTTFVL===>' ' o 'B')
015900931129     C                   SETON                                        03
016000931129     C                   ENDIF
016100931130     C                   ENDIF
016200931129     C*
016300931129     C****  CODICE PADRONCINO AL  ****
016400931129     C* 04 ON  - RICHIESTO ANCHE IL CODICE PADRONCINO AL
016500931130     C     PARCP2        IFNE      *BLANKS
016600931130     C     PARCP2        ANDNE     *ZEROS
016700931201     C     PARCP1        ANDNE     PARCP2
016800931129     C                   SETON                                        04
016900931129     C                   ENDIF
017000931129     C*
017100931130     C****  DATA DISTINTE/RITIRI AL  ****
017200931130     C* 05 ON  - RICHIESTA ANCHE LA DATA DISTINTE/RITIRI AL
017300931130     C     PARDRA        IFGT      0
017400931201     C     PARDRD        ANDNE     PARDRA
017500931130     C                   SETON                                        05
017600931130     C                   ENDIF
017700931130     C*
017800931129     C                   ENDSR
017900931201     C*
018000931201     C*--- IMPOSTAZIONE LETTURE --------------------------------------*
018100931201     C     IMPO          BEGSR
018200931201     C*
018300931201    1C     *IN01         IFEQ      *OFF
018400931201     C*
018500011114     C  N08KFTT1         SETLL     fiftt001
018600931201     C*
018700011114     C  N04KFTT1         READE     fiftt001                               30
018800011114     C   04              READ      fiftt001                               30
018900931201     C*
019000931201     C*
019100931201   X1C                   ELSE
019200931201     C*
019300931201     C*
019400931201    2C     *IN02         IFEQ      *OFF
019500931201     C     *IN03         ANDEQ     *OFF
019600931201     C*
019700011114     C  N08KFTT2         SETLL     fiftt004
019800931201     C*
019900931201    3C     *IN05         IFEQ      *OFF
020000011114     C  N04KFTT2         READE     fiftt004                               30
020100011114     C   04KFTT3         READE     fiftt004                               30
020200931201   X3C                   ELSE
020300011114     C     PARTVL        READE     fiftt004                               30
020400931201    3C                   ENDIF
020500931201     C*
020600931201     C                   SETON                                        07
020700931201     C*
020800931201   X2C                   ELSE
020900931201     C*
021000011114     C  N08PARCPD        SETLL     fiftt001
021100931201     C*
021200011114     C  N04PARCPD        READE     fiftt001                               30
021300011114     C   04              READ      fiftt001                               30
021400931201    2C                   ENDIF
021500931201     C*
021600931201    1C                   ENDIF
021700931201     C*
021800931201     C                   ENDSR
021900931201     C*
022000931201     C*--- ESEGUO ESCLUSIONI -----------------------------------------*
022100931201     C     ESCLUS        BEGSR
022200931201     C*
022300931201     C  N01
022400931201    1CAN 04FTTTSR        IFNE      PARTSR
022500931201     C                   SETON                                        30
022600931201    1C                   ENDIF
022700931201     C*
022800931201    1C  N30*IN07         IFEQ      *OFF
022900931201     C     FTTFVL        IFEQ      'C'
023000931201     C                   SETON                                        30
023100931201     C                   ENDIF
023200931201     C*
023300931201     C  N30
023400931201     CANN02
023500931201     CANN03FTTFVL        IFNE      'V'
023600931201     C                   SETON                                        30
023700931201     C                   ENDIF
023800931201    1C                   ENDIF
023900931201     C*
024000931201     C  N30
024100931201     CANN02
024200931201    1CAN 03FTTFVL        IFNE      ' '
024300931201     C     FTTFVL        ANDNE     'B'
024400931201     C                   SETON                                        30
024500931201    1C                   ENDIF
024600931201     C*
024700931201     C  N30
024800931201    1CAN 04FTTPDR        IFGT      PARCPA
024900931201     C                   SETON                                        30
025000931201    1C                   ENDIF
025100931201     C*
025200931201    1C  N30*IN07         IFEQ      *ON
025300931201     C   05FTTDDC        IFGT      PARDRA
025400931201     C                   SETON                                        30
025500931201     C                   ENDIF
025600931201     C*
025700931201   X1C                   ELSE
025800931201     C*
025900931201    2C     *IN05         IFEQ      *OFF
026000931201     C     FTTDDC        IFNE      PARDRD
026100931201     C                   SETON                                        30
026200931201     C                   ENDIF
026300931201     C*
026400931201   X2C                   ELSE
026500931201     C*
026600931201     C     FTTDDC        IFLT      PARDRD
026700931201     C     FTTDDC        ORGT      PARDRA
026800931201     C                   SETON                                        30
026900931201     C                   ENDIF
027000931201     C*
027100931201    2C                   ENDIF
027200931201    1C                   ENDIF
027300931201     C*
027400931201     C                   ENDSR
027500931201     C*
