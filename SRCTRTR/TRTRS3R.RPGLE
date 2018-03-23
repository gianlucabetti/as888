000100931117     H DECEDIT('0,') DATEDIT(*DMY.)
000200941102     F* TRTRS3R  *---------------------------------------------------*
000300931020     F*----------*                                                   *
000400931020     F* - RICEZIONE DATI CONTRASSEGNI PAGATI IN FILIALE DA SEDE      *
000500931020     F*--------------------------------------------------------------*
000600931213     F* - I N D I C A T O R I                                        *
000700931213     F*--------------------------------------------------------------*
000800931213     F* 20 - ERRORE IN CALL A QCMDEXEC                               *
000900931213     F* 80 - READ SU FLCSB00R  X CARICAMENTO DATI TRASMESSI          *
001000931213     F* 90 - CHAIN SU FLCSB01L X CONTROLLARE SE RECORD ESISTE GIA'   *
001100931213     F*--------------------------------------------------------------*
001200941102     FFNCSB00R  UF   E             DISK    USROPN
001300941102     F                                     RENAME(FNCSB000:FNCSBR)
001400931020     F*-------------
001500941102     FFNCSB01L  UF A E           K DISK
001600931021     F*-------------
001700931021     FCMRIC01L  UF   E           K DISK
001800931020     D*--------------------------------------------------------------*
001900931020     D*  SCHIERE X ESECUZIONE QCMDEXEC
002000931020     D*--------------------------------------------------------------*
002100931020     D CMD1            S             55    DIM(1) CTDATA PERRCD(1)              CHKOBJ MBR...
002200931020     D CMD2            S             55    DIM(1) CTDATA PERRCD(1)              ALCOBJ MBR...
002300931020     D CMD3            S             55    DIM(1) CTDATA PERRCD(1)              OVRDBF MBR..
002400931020     D CMD4            S             55    DIM(1) CTDATA PERRCD(1)              DLTOVR *ALL
002500931020     D CMD5            S             55    DIM(1) CTDATA PERRCD(1)              DLCOBJ MBR..
002600931020     D*--------------------------------------------------------------*
002700931020     D*   DS
002800931020     D*--------------------------------------------------------------*
002900931020     D KPJBA         E DS
003000931020     D  LEGFIL               483    492
003100931020     D  MBRFIL               493    502
003200931020     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003300931020     I*--------------------------------------------------------------*
003400931020     I*   Ridenomino campi file CSB. di arrivo da aggiornare
003500931020     I*--------------------------------------------------------------*
003600941102     IFNCSB000
003700931020     I              CSBLNA                      LNA
003800941102     I              CSBMGS                      MGS
003900941102     I              CSBCAS                      CAS
004000941102     I              CSBVCA                      VCA
004100931020     I              CSBNRA                      NRA
004200931020     I              CSBDTE                      DTE
004300931020     I              CSBABI                      ABI
004400931020     I              CSBCAB                      CAB
004500931020     I              CSBTPA                      TPA
004600931020     I              CSBTPI                      TPI
004700931020     I              CSBDDC                      DDC
004800931020     I              CSBNDT                      NDT
004900931020     I              CSBCDI                      CDI
005000931020     I              CSBRSM                      RSM
005100931020     I              CSBRSD                      RSD
005200941102     I              CSBRMA                      RMA
005300931020     I              CSBRMN                      RMN
005400931020     I              CSBFPC                      FPC
005500931020     I              CSBPAB                      PAB
005600931020     I              CSBPCA                      PCA
005700931020     I              CSBNDP                      NDP
005800931020     I              CSBDDP                      DDP
005900931020     I              CSBDGC                      DGC
006000931020     I              CSBPAA                      PAA
006100931020     I              CSBPLP                      PLP
006200931020     I              CSBPNS                      PNS
006300931020     I              CSBPSP                      PSP
006400960918     I              CSBSTA                      STA
006500931020     C*--------------------------------------------------------------*
006600931020     C*   C I C L O    P R I N C I P A L E                           *
006700931020     C*--------------------------------------------------------------*
006800931020     C*  Apertura archivio di ricezione
006900931020     C                   EXSR      OPNMBR
007000931020     C*  Prima lettura
007100941102     C                   READ      FNCSB00R                               80
007200931020     C*  Ciclo
007300931020     C     *IN80         DOWEQ     '0'
007400931117     C*  Aggiorno FLCSB
007500931020     C                   EXSR      UPDCSB
007600941102     C                   READ      FNCSB00R                               80
007700931020     C                   END
007800931020     C*
007900931020     C*  Provvedo alla chiusura del file di riceizione
008000931020     C                   EXSR      CLOMBR
008100931020     C*
008200931020     C     FINE          TAG
008300931020     C                   SETON                                        LR
008400931020     C*--------------------------------------------------------------*
008500931020     C*   OPNMBR: Apertura membro x ricezione movimenti              *
008600931020     C*--------------------------------------------------------------*
008700931020     C     OPNMBR        BEGSR
008800931020     C*
008900931020     C***  Controllo se il membro esiste
009000931020     C***  se no vado a fine pgm.
009100931020     C                   MOVE      ')'           VAR11            11
009200931020     C                   MOVEL     MBRFIL        VAR11
009300931020     C                   MOVE      '))'          VAR12            12
009400931020     C                   MOVEL     MBRFIL        VAR12
009500931020     C                   Z-ADD     55            LUNG             15 5
009600931020     C                   Z-ADD     64            LUNG1            15 5
009700931020     C                   SETOFF                                       20
009800931020     C                   MOVE      VAR11         CMD1
009900931020     C                   MOVEL     *BLANKS       COMMAN
010000931020     C                   MOVEA     CMD1(1)       COMMAN           80
010100931020     C                   CALL      'QCMDEXC'                            20
010200931020     C                   PARM                    COMMAN
010300931020     C                   PARM                    LUNG
010400931020     C   20              GOTO      FINE
010500931020     C** Alloco membro
010600931020     C** se non riesce chiudo il programma
010700931020     C                   SETOFF                                       20
010800931020     C                   MOVE      VAR12         CMD2
010900931020     C                   MOVEL     *BLANKS       COMMAN
011000931020     C                   MOVEA     CMD2(1)       COMMAN
011100931020     C                   CALL      'QCMDEXC'                            20
011200931020     C                   PARM                    COMMAN
011300931020     C                   PARM                    LUNG
011400931020     C   20              GOTO      FINE
011500931020     C** Esegue ovrdbf del membro desiderato
011600931020     C** se non riesce chiudo il programma
011700931020     C                   MOVE      VAR11         CMD3
011800931020     C                   MOVEL     *BLANKS       COMMAN
011900931020     C                   MOVEA     CMD3(1)       COMMAN
012000931020     C                   CALL      'QCMDEXC'                            20
012100931020     C                   PARM                    COMMAN
012200931020     C                   PARM                    LUNG
012300931020     C   20              GOTO      FINE
012400931020     C**** Apertura file di ricezione dati CONTRASSEGNI
012500941102     C                   OPEN      FNCSB00R
012600931020     C*
012700931020     C                   ENDSR
012800931020     C*--------------------------------------------------------------*
012900931020     C*   UPDCSB: Aggiorno dati contrassegno in base a dati di sede  *
013000931020     C*--------------------------------------------------------------*
013100931020     C     UPDCSB        BEGSR
013200931020     C*
013300931020     C                   MOVE      CSBAAS        CSWAAS
013400931020     C                   MOVE      CSBLNP        CSWLNP
013500931020     C                   MOVE      CSBNRS        CSWNRS
013600931020     C                   MOVE      CSBNSP        CSWNSP
013700941102     C     KEYCSB        CHAIN     FNCSB01L                           90
013800931021     C                   ADD       1             CSBREC            5 0
013900931020     C*  Aggiorno campi
014000931020     C                   MOVE      CSBLNA        LNA
014100941102     C                   MOVE      CSBMGS        MGS
014200941102     C                   MOVE      CSBCAS        CAS
014300941102     C                   MOVE      CSBVCA        VCA
014400931020     C                   MOVE      CSBNRA        NRA
014500931020     C                   MOVE      CSBDTE        DTE
014600931020     C                   MOVE      CSBABI        ABI
014700931020     C                   MOVE      CSBCAB        CAB
014800931020     C                   MOVE      CSBTPA        TPA
014900931020     C                   MOVE      CSBTPI        TPI
015000931020     C                   MOVE      CSBDDC        DDC
015100931020     C                   MOVE      CSBNDT        NDT
015200931020     C                   MOVE      CSBCDI        CDI
015300931020     C                   MOVE      CSBRSM        RSM
015400931020     C                   MOVE      CSBRSD        RSD
015500941102     C                   MOVE      CSBRMA        RMA
015600931020     C                   MOVE      CSBRMN        RMN
015700931020     C                   MOVE      CSBFPC        FPC
015800931020     C                   MOVE      CSBPAB        PAB
015900931020     C                   MOVE      CSBPCA        PCA
016000931020     C                   MOVE      CSBNDP        NDP
016100931020     C                   MOVE      CSBDDP        DDP
016200931020     C                   MOVE      CSBDGC        DGC
016300931020     C                   MOVE      CSBPAA        PAA
016400931020     C                   MOVE      CSBPLP        PLP
016500931020     C                   MOVE      CSBPNS        PNS
016600931020     C                   MOVE      CSBPSP        PSP
016700960918     C                   MOVE      CSBSTA        STA
016800931020     C*
016900941102     C  N90              UPDATE    FNCSB000
017000941102     C   90              WRITE     FNCSB000
017100931020     C*  Cancello record da archivio di ricezione
017200941102     C                   DELETE    FNCSBR
017300931020     C*
017400931020     C                   ENDSR
017500931020     C*--------------------------------------------------------------*
017600931020     C*   CLOMBR: Chiusura membro x ricezione movimenti              *
017700931020     C*--------------------------------------------------------------*
017800931020     C     CLOMBR        BEGSR
017900931020     C*
018000931020     C                   MOVE      ')'           VAR11            11
018100931020     C                   MOVEL     MBRFIL        VAR11
018200931020     C                   MOVE      '))'          VAR12            12
018300931020     C                   MOVEL     MBRFIL        VAR12
018400931020     C                   Z-ADD     55            LUNG             15 5
018500931020     C** Chiudo File ricezione dati contraasegni pagati
018600941102     C                   CLOSE     FNCSB00R
018700931020     C** Dltovr *All
018800931020     C                   SETOFF                                       20
018900931020     C                   MOVEL     *BLANKS       COMMAN
019000931020     C                   MOVEA     CMD4(1)       COMMAN
019100931020     C                   CALL      'QCMDEXC'                            20
019200931020     C                   PARM                    COMMAN
019300931020     C                   PARM                    LUNG
019400931020     C** Disalloco il membro aggiornato
019500931020     C                   MOVE      VAR12         CMD5
019600931020     C                   MOVEL     *BLANKS       COMMAN
019700931020     C                   MOVEA     CMD5(1)       COMMAN
019800931020     C                   CALL      'QCMDEXC'                            20
019900931020     C                   PARM                    COMMAN
020000931020     C                   PARM                    LUNG
020100931020     C*
020200931021     C* 1° ARCHIVIO = CNMOV
020300931021     C     LEGFIL        CHAIN     CMRIC01L                           20
020400931021     C  N20              DO
020500931021     C                   Z-ADD     CSBREC        RICNR1
020600931021     C                   MOVE      '1'           RICFL2                          AGG.TO
020700931021     C                   UPDATE    CMRIC000
020800931021     C                   END
020900931020     C                   ENDSR
021000931020     C*--------------------------------------------------------------*
021100941102     C*   *INZSR: Operazioni iniziali                                *
021200931020     C*--------------------------------------------------------------*
021300941102     C     *INZSR        BEGSR
021400931020     C*
021500931020     C     *ENTRY        PLIST
021600931020     C                   PARM                    KPJBA
021700931020     C* Se il membro da trascodificare non è stato passato
021800931020     C* assumo il primo membro del file
021900931020     C     MBRFIL        IFEQ      *BLANK
022000931020     C                   MOVEL     '*FIRST'      MBRFIL
022100931020     C                   ELSE
022200931020     C                   MOVEL     MBRFIL        §ALF4             4
022300931020     C                   MOVE      §ALF4         §FIPRI            3 0
022400931020     C                   END
022500931020     C* Richiamo XPARUT
022600931020     C                   Z-ADD     1             CODUT
022700931020     C                   CALL      'X§PARUT'
022800931020     C                   PARM                    UT§DSE
022900931020     C     CODUT         CABEQ     -1            FINE
023000931020     C*
023100931020     C* Definizione KLIST
023200931020     C     KEYCSB        KLIST
023300931020     C                   KFLD                    CSWAAS
023400931020     C                   KFLD                    CSWLNP
023500931020     C                   KFLD                    CSWNRS
023600931020     C                   KFLD                    CSWNSP
023700931020     C* Definizione varibili
023800931020     C     *LIKE         DEFINE    CSBAAS        CSWAAS
023900931020     C     *LIKE         DEFINE    CSBLNP        CSWLNP
024000931020     C     *LIKE         DEFINE    CSBNRS        CSWNRS
024100931020     C     *LIKE         DEFINE    CSBNSP        CSWNSP
024200931020     C*
024300931020     C                   ENDSR
024400941102**         CMD1
024500941102CHKOBJ OBJ(FNCSB00R) OBJTYPE(*FILE)     MBR(M234567890)
024600931020**         CMD2
024700941102ALCOBJ OBJ((FNCSB00R *FILE *EXCL           M234567890))
024800931020**         CMD3
024900941102OVRDBF FILE(FNCSB00R)                   MBR(M234567890)
025000931020**         CMD4
025100931020DLTOVR FILE(*ALL)
025200931020**         CMD5
025300941102DLCOBJ OBJ((FNCSB00R *FILE *EXCL           M234567890))
