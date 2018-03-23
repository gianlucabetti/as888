000100150430      *PARMS CVTOPT(*NONE)
000200150430     /*PRM  cvtopt(*none)
000300150430     /*END
000400030424     H DECEDIT('0,') DATEDIT(*YMD)
000500150430     h dftactgrp(*no)
000600150430     h bnddir('UBBNDDIR')
000700930922     F********************************************************************
000800150430     F* UBLANCIOR (ex BAK001R)                                           *
000900030424     F*                                                                  *
001000930922     F* LANCIO SIMULATO AZIONI                                           *
001100000000     F*                                                                  *
001200000000     F********************************************************************
001300150430     FKPPRF01L  IF   E           K DISK
001400150430     fAZUTE01L  if   e           k disk
001500150430     FUBLANCIOD CF   E             WORKSTN
001600001013     F                                     SFILE(BAK0014V:NRR01)
001700930922     F                                     INFDS(DINFD)
001800150430      *
001900930922     D KJU             S              1    DIM(256)
002000930922     D ESA             S              1    DIM(16) CTDATA PERRCD(16)
002100930922     D DAA             S              7    DIM(6) CTDATA PERRCD(6)
002200930922     D DAB             S              9    DIM(6) CTDATA PERRCD(6)
002300930922     D HEXZ            S              1    DIM(257)
002400930922     D HEXD            S              1    DIM(257)
002500930922     D RCDX            S              1    DIM(257)
002600930922     D RCDN            S              1    DIM(50)
002700930922     D RCXX            S              1    DIM(50)
002800930922     D                SDS
002900930922     D  KNMTDV               244    253
003000930922     D  KNMUSV               254    263
003100930922     D  NUMJOB               264    269  0
003200930922     D DINFD           DS
003300930922     D  LINETR               152    153B 0
003400930922     D KPJBA         E DS
003500930927     D* salvataggio del lancio
003600150430     D UBLANCIO        DS           512
003700930927     D*
003800930922     D                 DS
003900930922     D  RCXC                   1     50
004000930922     D  RCDC                   1     50
004100930922     D                                     DIM(50)
004200930922     D                 DS
004300930922     D  RCXZ                   1     50
004400930922     D  RCDZ                   1     50
004500930922     D                                     DIM(50)
004600930922     D                 DS
004700930922     D  RCXD                   1     50
004800930922     D  RCDD                   1     50
004900930922     D                                     DIM(50)
005000930922     D                 DS
005100930922     D  ZEROB                  1      1
005200930922     D  FLD                    2      2
005300930922     D  FLDB                   1      2B 0
005400150430
005500150430       //--------------------------------------------------------------
005600150430       //?Definizione prototipi procedure.                             ?
005700150430       //--------------------------------------------------------------
005800150430
005900150430       // -?Nome del sistema?
006000150430     d currSysNeta     s              8a   inz
006100150430       // -?Reperimento NETA sistema AS/400 corrente?
006200150430      /copy gaitrasrc/srcProtoPR,UBRTVNETA
006300150430
006400031020X0206C*------------------------------------------------------------------*
006500031020  "  C*
006600031020  "  C     *ENTRY        PLIST
006700031020X0206C                   PARM                    I_PGM            10
006800030424     C*
006900930922     C                   EXSR      DEFCAM                                       DEFINIZIONI
007000930922     C*
007100930922     C*** FLUSSO PRINCIPALE DEL PROGRAMMA -------------------------------*
007200930922     C*
007300930923     C                   SETON                                        19
007400930922     C                   MOVE      *BLANKS       KPJBA
007500031020     C*
007600031020     C* CARICAMENTO "KPJBA"
007700150430     C                   MOVEL     UBLANCIO      NOMPGM
007800150430     C                   MOVE      UBLANCIO      KPJBA
007900930922     C*
008000930922     C                   MOVE      KNMTDV        KNMTD
008100930922     C                   MOVE      KNMTDV        KNMEB
008200930922     C                   MOVE      KNMUSV        KNMUS
008300150430     C     KNMUS         CHAIN     KPPRF01L                           01        PROFILI
008400150430X0094C     KNSIF         IFEQ      *BLANK
008500150430      *
008600150430     c     KNMUS         chain     AZUTE000                                     PROFILI
008700150430     c                   If        %found(AZUTE01L)
008800150430     c                   select
008900150430     c                   when      %subst ( currSysNeta : 1 : 6 ) = 'AS888 '
009000150430     c                             and      UTEfil                = 046
009100150430     c                   eval      KNSIF  = 'GAITRAPSP '
009200150430     c                   when      %subst ( currSysNeta : 1 : 6 ) = 'AS888 '
009300150430     c                             and      UTEfil               <> 046
009400150430     c                   eval      KNSIF  = 'FILTRAPFP '
009500150430     c                   when      %subst ( currSysNeta : 1 : 6 ) = 'SETRAS'
009600150430     c                             and      UTEfil                = 046
009700150430     c                   eval      KNSIF  = 'GAITRA201 '
009800150430     c                   when      %subst ( currSysNeta : 1 : 6 ) = 'SETRAS'
009900150430     c                             and      UTEfil               <> 046
010000150430     c                   eval      KNSIF  = 'FILTRA201 '
010100150430     c                   other
010200150430     c                   eval      KNSIF  = PRFnsi
010300150430     c                   endsl
010400150430     c                   Else
010500150430     C                   MOVEL     PRFNSI        KNSIF                          SIS.INF.
010600150430     c                   EndIf
010700150430      *
010800150430X0094C                   ENDIF
010900930922     C                   Z-ADD     NUMJOB        KNRJO
011000930922     C                   Z-ADD     0             KNRAZ
011100930922     C                   Z-ADD     0             KNRVJ
011200930922     C                   Z-ADD     NUMJOB        KNRJO
011300930922     C                   Z-ADD     0             KTMIN
011400930922     C                   Z-ADD     0             KNMSB
011500930922     C                   Z-ADD     0             KRCDS
011600930922     C                   Z-ADD     0             KKEYF
011700930922     C   01              MOVE      'M'           KTPWS
011800930922     C   18              MOVE      'C'           KTPWS
011900930922     C  N01
012000930922     CANN18              MOVE      'N'           KTPWS
012100031020X0206C*
012200031020  "  C* Se ricevuto pgm da chiamare (no prompt) salto emissione video
012300031020  "  C     I_PGM         IFNE      '*PROMPT'
012400031020  "  C                   MOVE      I_PGM         NOMPGM
012500031020  "  C                   GOTO      CALLPGM
012600031020X0206C                   ENDIF
012700930928     C*
012800031020     C     INIZIO        TAG
012900930928     C*
013000001013     C                   EXFMT     BAK0012V                             12
013100930922     C   12              GOTO      FINE
013200930922     C   11              GOTO      INIZIO
013300031020X0206C* Controllo esistenza pgm
013400031020  "  C                   Call      'BAK922C'
013500031020  "  C                   Parm      NOMPGM        PgmCmd922        10
013600031020  "  C                   Parm      'P'           Flag922           1
013700031020  "  C                   Parm      *Blank        File922          10
013800031020  "  C                   Parm      *Blank        LibFile922       10
013900031020  "  C                   If        Flag922 = '1'
014000031020  "  C                   SetOn                                        54
014100031020  "  C                   GoTo      INIZIO
014200031020X0206C                   EndIf
014300031020     C* CARICAMENTO "KPJBU"
014400001013     C                   WRITE     BAK0013V                             12      HDR VIDEO
014500930922     C   12              GOTO      FINE
014600930922     C                   MOVEA     KPJBU         RCDX
014700930922     C                   EXSR      RAZHEX
014800931129     C*
0149009311291    C                   DO        *HIVAL
015000001013     C                   EXFMT     BAK0015V                             12
015100930922     C   12              GOTO      FINE
015200930922     C                   SETOFF                                       52        OFF SFLMSG
015300931129     C*
0154009311292    C     *IN10         IFEQ      *ON                                          RIPRISTINO
015500931129     C     *IN11         OREQ      *ON                                          RIT. INIZ.
015600930922     C                   SETON                                        25
015700001013     C                   WRITE     BAK0015V                             12      PULISCE SFL
015800930922     C   12              GOTO      FINE
015900930922     C                   SETOFF                                       25  40
016000930922     C   11              GOTO      INIZIO
016100930922     C                   MOVEA     KPJBU         RCDX
016200930922     C                   EXSR      RAZHEX
0163009311292-   C                   ENDIF                                                  FINE RIPR.
016400931129     C*
0165009311292    C     *IN14         IFEQ      *ON                                          AGGIORN.
016600931129     C     *IN15         OREQ      *ON
016700930922     C   14              Z-ADD     0             SFLNBR
016800001013     C                   READC     BAK0014V                               59
0169009311293    C     *IN59         DOWNE     '1'
017000930922     C                   EXSR      UPDATR
017100001013     C                   READC     BAK0014V                               59
0172009311293-   C                   ENDDO                                                  *IN59 DOWNE
017300931129     C*
0174009311293    C   14SFLNBR        IFEQ      0
017500930922     C                   Z-ADD     1             SFLNBR
0176009311293-   C                   ENDIF
0177009311292-   C                   ENDIF                                                  15 OR 14
017800931129     C  N11
017900931129     CANN12
0180009311291-   CAN 33              ENDDO                                                  DO *HIVAL
018100930922     C   40              MOVEA     KJU           KPJBU                          AGG."KPJBU"
018200031020X0206C*
018300031020X0206C     CALLPGM       TAG
018400930927     C* Salvo campi di Lancio
018500150430     C                   MOVEL     NOMPGM        UBLANCIO
018600150430     C                   MOVE      KPJBA         UBLANCIO
018700150430     C                   OUT       UBLANCIO
018800031020     C* ESECUZIONE PGM
018900930922     C                   CALL      NOMPGM
019000930922     C                   PARM                    KPJBA
019100031020X0206C* Se ricevuto pgm da chiamare (no prompt) vado a fine
019200031020X0206C     I_PGM         CABNE     '*PROMPT'     FINE
019300931129     C* DA CAPO
019400150430     C     *LOCK         IN        UBLANCIO
019500931129     C                   GOTO      INIZIO
019600931129     C*
019700930922     C     FINE          TAG
019800150430     C                   UNLOCK    UBLANCIO
019900930922     C                   SETON                                        LR
020000930922     C********************************************************************
020100930922     C*                    SUBROUTINES                                   *
020200930922     C********************************************************************
020300930922     C*** TRADUZIONE IN ESADECIMALE DELLA "KPJBU" E CARICAMENTO SUBFILE
020400930922     C     RAZHEX        BEGSR
020500930922     C                   MOVEA     KPJBU         KJU
020600930922     C                   SETOFF                                       505152
020700930922     C                   Z-ADD     1             SFLNBR
020800930922     C* TRADUZIONE ESADECIMALE
020900930922     C                   DO        256           R                 5 0
021000930922     C                   MOVE      RCDX(R)       FLD
021100930922     C     RCDX(R)       COMP      ' '                                  34
021200930922     C   34              MOVE      X1C           RCDX(R)
021300930922     C                   Z-ADD     FLDB          X                 3 0
021400930922     C     X             DIV       D16           Z                 3 0
021500930922     C                   MVR                     D                 3 0
021600930922     C                   ADD       1             Z
021700930922     C                   ADD       1             D
021800930922     C                   MOVE      ESA(D)        HEXD(R)
021900930922     C                   MOVE      ESA(Z)        HEXZ(R)
022000930922     C                   END
022100930922     C                   Z-ADD     257           R
022200930922     C                   MOVE      X27           RCDX(R)
022300930922     C                   MOVE      X27           HEXZ(R)
022400930922     C                   MOVE      X27           HEXD(R)
022500930922     C* CARICAMENTO DEL SUBFILE
022600930922     C                   Z-ADD     1             WW                3 0
022700930922     C                   DO        6             NR                3 0
022800930922     C                   Z-ADD     NR            NRR01             3 0
022900930922     C                   MOVEA     RCDX(WW)      RCHARA
023000930922     C                   MOVEA     HEXZ(WW)      RZONAT
023100930922     C                   MOVEA     HEXD(WW)      RDIGIT
023200930922     C                   MOVE      DAA(NR)       CMPDAA
023300930922     C                   MOVE      DAB(NR)       CMPDA2
023400001013     C                   WRITE     BAK0014V                             32
023500930922     C                   ADD       D50           WW
023600930922     C                   END
023700930922     C                   ENDSR
023800930922     C********************************************************************
023900930922     C*** AGGIORNAMENTO DI UNA PARTE DELLA "KPJBU"
024000930922     C     UPDATR        BEGSR
024100930922     C                   SETON                                        40
024200930922     C                   MOVEA     RCHARA        RCDN
024300930922     C                   MOVEA     RZONAT        RCDZ
024400930922     C                   MOVEA     RDIGIT        RCDD
024500930922     C     WW            IFEQ      251
024600930922     C                   Z-ADD     6             NVOLTE
024700930922     C                   MOVE      X27           RCDN(7)
024800930922     C                   MOVE      X27           RCDZ(7)
024900930922     C                   MOVE      X27           RCDD(7)
025000930922     C                   ELSE
025100930922     C                   Z-ADD     D50           NVOLTE
025200930922     C                   END
025300930922     C                   SETOFF                                       5051
025400930922     C   15              EXSR      UPDCAR
025500930922     C   14              EXSR      UPDHEX
025600930922     C                   MOVEA     RCDN          RCHARA
025700930922     C                   MOVEA     RCDZ          RZONAT
025800930922     C                   MOVEA     RCDD          RDIGIT
025900930922     C  N52
026000930922     CAN 15              MOVEA     RCDN          KJU(WW)
026100930922     C  N52
026200930922     CAN 14              MOVEA     RCXX          KJU(WW)
026300930922     C   52SFLNBR        IFEQ      0
026400930922     C                   Z-ADD     NRR01         SFLNBR
026500930922     C                   END
026600001013     C                   UPDATE    BAK0014V
026700930922     C                   ENDSR
026800930922     C********************************************************************
026900930922     C*** AGGIORNAMENTO DI UNA PARTE DELLA "KPJBU" VIA CARATTERE
027000930922     C     UPDCAR        BEGSR
027100930922     C                   DO        NVOLTE        R
027200930922     C     RCDN(R)       IFNE      X1C
027300930922     C                   MOVE      RCDN(R)       FLD
027400930922     C                   Z-ADD     FLDB          X                 3 0
027500930922     C     X             DIV       D16           Z                 3 0
027600930922     C                   MVR                     D                 3 0
027700930922     C                   ADD       1             Z
027800930922     C                   ADD       1             D
027900930922     C                   MOVE      ESA(D)        RCDD(R)
028000930922     C                   MOVE      ESA(Z)        RCDZ(R)
028100930922     C                   END
028200930922     C                   END
028300930922     C                   ENDSR
028400930922     C********************************************************************
028500930922     C*** AGGIORNAMENTO DI UNA PARTE DELLA "KPJBU" VIA ESADECIMALE
028600930922     C     UPDHEX        BEGSR
028700930922     C                   DO        NVOLTE        R
028800930922     C                   Z-ADD     1             X
028900930922     C     RCDZ(R)       LOOKUP    ESA(X)                                 20
029000930922     C  N20              SETON                                        505253
029100930922     C   20              DO
029200930922     C                   SUB       1             X
029300930922     C     X             MULT      D16           FLDB
029400930922     C                   END
029500930922     C                   Z-ADD     1             X
029600930922     C     RCDD(R)       LOOKUP    ESA(X)                                 20
029700930922     C  N20              SETON                                        515253
029800930922     C   20              DO
029900930922     C                   SUB       1             X
030000930922     C                   ADD       X             FLDB
030100930922     C                   MOVE      FLD           RCXX(R)
030200930922     C  N53FLD           IFGE      ' '
030300930922     C                   MOVE      FLD           RCDN(R)
030400930922     C                   ELSE
030500930922     C                   MOVE      X1C           RCDN(R)
030600930922     C                   END
030700930922     C   53              MOVE      ' '           RCDN(R)
030800930922     C                   SETOFF                                       53
030900930922     C                   END
031000930922     C                   END
031100930922     C                   ENDSR
031200930922     C*****************************************************************
031300930922     C*** DEFINIZIONE CAMPI E OPERAZIONI INIZIALI
031400930922     C     DEFCAM        BEGSR
031500930922     C     LINETR        COMP      16                                   0118
031600930922     C                   SETON                                        60
031700930922     C                   Z-ADD     0             AA                3 0
031800930922     C*
031900930922     C                   Z-ADD     0             NVOLTE            3 0
032000930922     C                   Z-ADD     016           D16               3 0
032100930922     C                   Z-ADD     069           D69               3 0
032200930922     C                   Z-ADD     050           D50               3 0
032300930922     C                   BITON     '345'         X1C               1
032400930922     C                   BITOFF    '1'           X1C
032500930922     C                   BITOFF    '1'           X27               1
032600930922     C                   BITON     '2567'        X27
032700930922     C                   BITOFF    '1'           ZEROB
032800930927     C*
032900150430     C     *DTAARA       DEFINE                  UBLANCIO
033000150430     C     *LOCK         IN        UBLANCIO
033100150430      /free
033200150430         // -?Verifica del sistema AS/400 corrente?
033300150430         UBRTVNETA_Rtv( currSysNeta );
033400150430      /end-free
033500930922     C                   ENDSR
033600930922**  VALORI ESADECIMALI
0337009309220123456789ABCDEF
033800930922**
033900930922  1- 50 51-100101-150151-200201-250251-256
034000930922**
034100930922(247-296)(297-346)(347-396)(397-446)(447-496)(497-502)
