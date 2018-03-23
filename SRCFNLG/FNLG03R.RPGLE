000100970623     H DECEDIT('0,') DATEDIT(*DMY.)
000200970623      *---------*----------------------------------------------------*
000300970624      * FNLG03R *                                                    *
000400970623      *---------*                                                    *
000500970623      *         - IMMISSIONE NOTE APERTURA GIACENZE IN ARRIVO -      *
000600970623      *--------------------------------------------------------------*
000700970624     FFNLG03D   CF   E             WORKSTN
000800970623     F*---------
000900050223     FTIGNP01L  UF A E           K DISK
001000970624     D*--------------------------------------------------------------*
001100970624     D*  SCHIERE
001200970624     D*--------------------------------------------------------------*
001300970624     D ERR             S             70    DIM(2) CTDATA PERRCD(1)
001400970623     D*--------------------------------------------------------------*
001500970623     D*  DS
001600970623     D*--------------------------------------------------------------*
001700970623     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
001800970623     D CNCR80        E DS
001900970623     D WLBDA8          DS
002000970623     D  G02DAT                 1      8  0
002100970623     D  G02INV                 9     16  0
002200970623     D  G02ERR                17     17
002300970623     D  G02TGI                18     22  0
002400970624     D FNLG03        E DS                  EXTNAME(FNLG03DS)
002500970623     D                SDS
002600970623     D  NOMPGM                 1     10
002700970623     C*--------------------------------------------------------------*
002800970623     C* CICLO PRINCIPALE
002900970623     C*--------------------------------------------------------------*
003000970623     C* Ricevo KPJBA
003100970623     C     *ENTRY        PLIST
003200970624     C                   PARM                    FNLG03
003300970623     C* Imposto prima videata
003400970623     C                   EXSR      INZ01
003500970623     C* Loop fino a F6/F12
003600970624     C     WFINE         DOWNE     'S'
003700970623     C                   EXSR      GESD01
003800970623     C                   END
003900970623     C* Per F6 Aggiorno/scrivo nota aggiuntiva
004000970623     C     *INKF         IFEQ      '1'
004100970623     C                   EXSR      AGGNOT
004200970623     C                   END
004300970623     C* Fine
004400970623     C                   SETON                                        RT
004500970623     C*--------------------------------------------------------------*
004600970623     C* Inizializzo variabili
004700970623     C*--------------------------------------------------------------*
004800970623     C     INZ01         BEGSR
004900970623     C*
005000970624     C* Per visualizzazione accendo 01
005100970624     C     D03TPV        COMP      'V'                                    01
005200970624     C* Se riapertura accendo 42
005300970624     C     D03DUR        COMP      0                                  42
005400970624     C                   Z-ADD     D03LNP        V1CLNP
005500970624     C                   Z-ADD     D03NRS        V1CNRS
005600970624     C                   Z-ADD     D03NSP        V1CNSP
005700970624     C                   Z-ADD     D03DSP        V1CDSP
005800970624     C                   MOVEL     D03CBO        V1CCBO
005900970624     C                   MOVEL     D03DBO        V1DCBO
006000970624     C                   Z-ADD     D03FGC        V1CFGC
006100970624     C                   MOVEL     D03DFG        V1DFGC
006200970624     C                   Z-ADD     D03NGC        V1CNGC
006300970624     C                   Z-ADD     D03DGC        V1CDGC
006400970624     C                   Z-ADD     D03DUR        V1CDUR
006500970624     C                   Z-ADD     D03NRC        V1CNRC
006600970624     C                   Z-ADD     D03DXD        V1CDXD
006700970624     C                   MOVEL     D03CCR        V1CCCR
006800970624     C                   MOVEL     D03DCR        V1DCCR
006900970624     C                   Z-ADD     D03KSC        V1CKSC
007000970624     C                   Z-ADD     D03RMN        V1CRMN
007100970624     C                   MOVEL     D03RMA        V1CRMA
007200970624     C                   MOVEL     D03RSM        V1CRSM
007300970624     C                   MOVEL     D03INM        V1CINM
007400970624     C                   MOVEL     D03CAM        V1CCAM
007500970624     C                   MOVEL     D03LOM        V1CLOM
007600970624     C                   MOVEL     D03PRM        V1CPRM
007700970624     C                   MOVEL     D03NZM        V1CNZM
007800970624     C                   MOVEL     D03RSD        V1CRSD
007900970624     C                   MOVEL     D03RS2        V1CRS2
008000970624     C                   MOVEL     D03IND        V1CIND
008100970624     C                   MOVEL     D03CAD        V1CCAD
008200970624     C                   MOVEL     D03LOD        V1CLOD
008300970624     C                   MOVEL     D03PRD        V1CPRD
008400970624     C                   MOVEL     D03NZD        V1CNZD
008500970624     C                   MOVEL     D03CMC        V1CCCM
008600970624     C                   MOVEL     D03CMD        V1DCCM
008700970624     C                   MOVEL     D03DMC        V1CDMC
008800970624     C                   MOVEL     *BLANKS       V1CNOT
008900970624     C                   MOVEL     'N'           WFINE             1
009000970624     C* Reperisco note aggiuntive
009100970624     C                   MOVE      D03DGC        KAGC
009200970624     C                   Z-ADD     D03FGC        KFGC
009300970624     C                   Z-ADD     D03NGC        KNGC
009400970624     C                   Z-ADD     0             KFRG
009500970624     C                   Z-ADD     12            KFAS
009600050223     C     KGNA          CHAIN(N)  TIGNP01L                           30
009700970624     C     *IN30         IFEQ      '0'
009800970624     C                   MOVEL     GNPDMC        V1CNOT
009900970624     C                   ELSE
010000970624     C     D03TPV        IFEQ      'V'
010100970624     C                   MOVEL     ERR(1)        D03MSG
010200970624     C                   MOVEL     '1'           D03ERR
010300970624     C                   MOVEL     'S'           WFINE
010400970624     C                   END
010500970624     C                   END
010600970624     C*
010700970624     C                   ENDSR
010800970624     C*--------------------------------------------------------------*
010900970624     C* Gestione videata visualizzazione note
011000970624     C*--------------------------------------------------------------*
011100970624     C     GESD01        BEGSR
011200970624     C*
011300970624     C                   EXFMT     LG03D01
011400970624     C                   SETOFF                                       4028
011500970624     C* Ritorno
011600970624     C     *INKL         IFEQ      '1'
011700970624     C                   MOVEL     'S'           WFINE
011800970624     C                   GOTO      FINVD1
011900970624     C                   END
012000970624     C* Controlli
012100970624     C     V1CNOT        IFEQ      *BLANKS
012200970624     C                   SETON                                        2840
012300970624     C                   MOVEL     ERR(2)        $MSG
012400970624     C                   GOTO      FINVD1
012500970624     C                   END
012600970624     C* Conferma
012700970624     C     *INKF         IFEQ      '1'
012800970624     C                   MOVEL     'S'           WFINE
012900970624     C                   GOTO      FINVD1
013000970624     C                   END
013100970624     C*
013200970624     C     FINVD1        ENDSR
013300970624     C*--------------------------------------------------------------*
013400970624     C* Aggiorno note aggiuntive
013500970624     C*--------------------------------------------------------------*
013600970624     C     AGGNOT        BEGSR
013700970624     C*
013800970624     C* Reperisco note aggiuntive
013900970624     C                   MOVE      D03DGC        KAGC
014000970624     C                   Z-ADD     D03FGC        KFGC
014100970624     C                   Z-ADD     D03NGC        KNGC
014200970624     C                   Z-ADD     0             KFRG
014300970624     C                   Z-ADD     12            KFAS
014400050223     C     KGNA          CHAIN     TIGNP01L                           30
014500970624     C     *IN30         IFEQ      '0'
014600970624     C                   MOVEL     V1CNOT        GNPDMC
014700050223     C                   UPDATE    TIGNP000
014800970624     C                   ELSE
014900970624     C                   MOVE      D03DGC        GNPAGC
015000970624     C                   Z-ADD     D03FGC        GNPFGC
015100970624     C                   Z-ADD     D03NGC        GNPNGC
015200970624     C                   Z-ADD     0             GNPFRG
015300970624     C                   Z-ADD     12            GNPFAS
015400970624     C                   MOVEL     V1CNOT        GNPDMC
015500970624     C                   MOVEL     'I'           GNPTPN
015600970624     C                   Z-ADD     1             GNPNPR
015700990625     C                   Z-ADD     0             GNPDIF
015800990625     C                   Z-ADD     0             GNPNRI
015900050223     C                   WRITE     TIGNP000
016000970624     C                   END
016100970624     C*
016200970624     C                   ENDSR
016300970624     C*--------------------------------------------------------------*
016400970624     C* Operazioni iniziali
016500970624     C*--------------------------------------------------------------*
016600970624     C     *INZSR        BEGSR
016700970624     C*
016800970624     C* Richiamo XPARUT
016900970624     C                   Z-ADD     1             CODUT
017000970624     C                   CALL      'X§PARUT'
017100970624     C                   PARM                    UTEDSE
017200970624     C                   MOVEL     REC80         CNCR80
017300970624     C                   MOVEL     RAGUT         RSUT
017400970624     C* Definizione chiavi
017500970624     C     KGNA          KLIST
017600970624     C                   KFLD                    KAGC
017700970624     C                   KFLD                    KFGC
017800970624     C                   KFLD                    KNGC
017900970624     C                   KFLD                    KFRG
018000970624     C                   KFLD                    KFAS
018100970624     C*  Definizione variabili
018200970624     C     *LIKE         DEFINE    GNPAGC        KAGC
018300970624     C     *LIKE         DEFINE    GNPFGC        KFGC
018400970624     C     *LIKE         DEFINE    GNPNGC        KNGC
018500970624     C     *LIKE         DEFINE    GNPFRG        KFRG
018600970624     C     *LIKE         DEFINE    GNPFAS        KFAS
018700970624     C*  Data del giorno
018800970624     C                   TIME                    WHHDAT           14 0
018900970624     C                   MOVE      WHHDAT        G02DAT
019000970624     C                   MOVE      *ZEROS        G02INV
019100970624     C                   MOVEL     *BLANK        G02ERR
019200970624     C                   CALL      'XSRDA8'
019300970624     C                   PARM                    WLBDA8
019400970624     C                   Z-ADD     G02INV        WOGGI             8 0
019500970624     C*
019600970624     C                   ENDSR
019700970624**
019800970624Non sono state immesse note in apertura per la giacenza selezionata
019900970624Immettere note aggiuntive
