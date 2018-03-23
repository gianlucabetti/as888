000100021023     H*PARMS DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND) ACTGRP(QILE)
000200021023     H*PARMS OPTION(*NOXREF) CVTOPT(*DATETIME)
000300021023     H DECEDIT('0,') DATEDIT(*DMY.)
000400021023     F*--------------------------------------------------------------*
000500041006     F*               SPEDIZIONE RID HOME BANKING                    *
000600021023     F*--------------------------------------------------------------*
000700021023     F* ATTENZIONE: Per compilare questo programma è necessario il   *
000800021023     F*             file CBITX che risiede nella libreria dati di    *
000900021023     F*             Home Banking                                     *
001000021023     F*--------------------------------------------------------------*
001100021023     FNDEFR01L  UF   E           K DISK
001200021023     FNDEFA02L  IF   E           K DISK
001300021023     FNDDPR01L  IF   E           K DISK
001400021023     FNDRBT00F  IF   E           K DISK
001500021023     FANSIF01L  IF   E           K DISK
001600021023     FNDREG01L  IF   E           K DISK
001700021023     FANUNI01L  IF   E           K DISK
001800071113C2062FAnAcr01L  IF   E           K DISK
001900041006     FNDRBW00F  O    E             DISK
002000080718C1935F                                     USROPN
002100021023     FCBITX     O    E             DISK    RENAME(CBITX: CBIR)
002200080718C1935F                                     USROPN
002300021023     FNDLOG00F  O    E             DISK
002400021023     FNDCOS00F  O    E             DISK
002500021023     D*-------------
002600021023     D RAP             S              8    DIM(250)
002700021023     D COS             S              5  0 DIM(250)
002800021023     D*-------------
002900021023     D* Prototipazione procedure EURO
003000021023     D/COPY *LIBL/SRCCPY,PJX002PR
003100021023     D*-------------
003200021023     D KPJBA         E DS
003300021023     D*-------------
003400021023     D* controllo ABI/CAB
003500021023     D X59ABIDS      E DS                  INZ
003600021023     D*-------------
003700021023     D NDCK73DS      E DS
003800021023     D*-------------
003900021023     D SOC001        E DS                  EXTNAME(XSOC001DS)
004000021023     D XSOCDS          DS          1000
004100021023     D*-------------
004200021023     D XATBDS        E DS
004300021023     D TABK03        E DS                  EXTNAME(ANGK03DS)
004400021023     D TABK04        E DS                  EXTNAME(ANGK04DS)
004500071113C2062D TABG12        E DS                  EXTNAME(ANGG12DS)
004600021023     D*-------------
004700021023     D KEYTAB          DS
004800021023     D  KEYCodice              1      4
004900021023     D  KEYBanca               5      8
005000021023     D  KEYLinea               9     12
005100021023     D*-------------
005200021023     D NDCKRBRB      E DS
005300021023     D  CRBNOM                20     39
005400021023     D  CRBLIB                40     45
005500041007     D
005600021023     D NDCKRB14      E DS
005700061215D2101D  RDBKCC                98    103
005800061215D2101D  RDBKSC               104    111
005900061215D2101D  RDBFIL               112    113
006000041006     D  RDBTFD               114    114
006100050124     D  RDBSOR               115    119  0
006200050124D1728D  RDBSO1               119    119
006300021023     D NDCKRB30      E DS
006400021023     D NDCKRB40      E DS
006500021023     D NDCKRB51      E DS
006600021023     D*-------------
006700021023     D TREF            DS
006800021023     D  SIAEF                  1      5
006900021023     D  ABIEF                  6     10  0
007000021023     D  DTCEF                 11     16  0
007100021023     D  DESEF                 17     36
007200021023     D  FI1EF                 37     42
007300021023     D  DISEF                 43     49  0
007400021023     D  INCEF                 50     64  0
007500021023     D  PAGEF                 65     79  0
007600021023     D  NRPEF                 80     86  0
007700021023     D  FI2EF                 87    110
007800021023     D  CDDEF                111    111
007900021023     D  FI3EF                112    117
008000091103D2562D*----------
008100091103  "  X*TR16            DS
008200091103  "  X* DSP16                  4     10  0
008300091103  "  X* STA16                 11     12
008400091103  "  X* CHK16   ex C2062      13     14
008500091103  "  X* BBA16                 15     44
008600091103D2562X* FI116                 45    117
008700091103D2562D*----------
008800091103  "  D TR16            DS
008900091103  "  D  DSP16                  1      7  0
009000091103  "  D  STA16                  8      9
009100091103  "  D  CHK16                 10     11
009200091103  "  D  BBA16                 12     41
009300091103D2562D  FI116                 42    117
009400021023     D*-------------
009500021023     D TR20            DS
009600021023     D  DSP20                  1      7  0
009700021023     D  DO120                  8     31
009800021023     D  DO220                 32     55
009900021023     D  DO320                 56     79
010000021023     D  DO420                 80    103
010100021023     D  FI120                104    117
010200021023     D*-------------
010300021023     D TR50            DS
010400021023     D  DSP50                  1      7  0
010500041118c1790x* II150*****             8     47
010600041118  "  x* II250*****            35     40
010700041118  "  x* II350*****            42     47
010800041118  "  x* II450*****            48     48
010900041118  "  X* II550                 49     87
011000041118  "  D*
011100041118  "  D  II150                  8     18
011200041122  "  D  II250                 16     24
011300041122  "  D  II350                 30     39
011400051025C1790D  II450                 48     56
011500051025C1887x** ex C1790  II550                 56     87
011600051025C1887D  II550                 57     87
011700041006     D  II650                 88     97
011800041006     D  CFP50                 98    113
011900041006     D  FI150                114    117
012000021023     D*-------------
012100021023     D TR70            DS
012200021023     D  DSP70                  1      7  0
012300041007     D  FI170                  8    117
012400021023     D*-------------
012500021023     D DSRBRB        E DS                  EXTNAME(NdRiBaRBDs)
012600021023     D DSRB14        E DS                  EXTNAME(NdRiBa14Ds)
012700021023     D DSRB20        E DS                  EXTNAME(NdRiBa20Ds)
012800021023     D DSRB30        E DS                  EXTNAME(NdRiBa30Ds)
012900021023     D DSRB40        E DS                  EXTNAME(NdRiBa40Ds)
013000021023     D DSRB50        E DS                  EXTNAME(NdRiBa50Ds)
013100021023     D  DSNEF                 22     30
013200021023     D  DSNRP                 62     70
013300021023     D  DSGRP                 76     77
013400021023     D  DSMRP                 79     80
013500021023     D  DSARP                 82     85
013600021023     D DSRB51        E DS                  EXTNAME(NdRiBa51Ds)
013700021023     D DSRB70        E DS                  EXTNAME(NdRiBa70Ds)
013800021023     D DSRBEF        E DS                  EXTNAME(NdRiBaEFDs)
013900071113C2062 *-------------
014000071113  "  D CBAIBANDS     E DS
014100071113  "  D $Stato          s                   like(CbaStato)
014200071113  "  D $check          s                   like(CbaCheck)
014300071113  "  D $Coori          s                   like(CbaCoori)
014400071113  "  D $err            s              1
014500071113  "  D $Opz            s              2
014600071113C2062 *****************************************************************
014700021023     D*----------
014800021023     D EFARAP          DS
014900021023     D  EFABanca               1      4
015000021023     D  EFALinea               5      8
015100021023     D*----------
015200021023     D Oggi            S               D
015300021023     D PROG            S              7  0
015400021023     D CODSIA          S                   LIKE(SIFCdSIA)
015500021023     D CODSI2          S                   LIKE(SIFCdSIAGr)
015600021023     D IMPGEN          S                   LIKE(EFAImportD)
015700021023     D WWImporto       S                   LIKE(EFAImportD)
015800021023     D WWCdIntDiv      S                   LIKE(DVSCdInter)
015900021023     D WWDataISO       S               D
016000021023     D WWData6         S              6  0
016100041006     D*-------------
016200041006     D* Parametri per XPAR
016300041006     D  XPASOC         S              3
016400041006     D  XPAPAR         S              8
016500041006     D  XPAOUT         S           2000
016600041006     D  XPAERR         S              1
016700041006     D  XPARIC         S              1
016800041006     D*-------------
016900041006     D* Parametri di base H.B.
017000041006     D NDCK90DS      E DS                  INZ
017100041006     D*-------------
017200041006     D DATCAT          DS
017300041006     D  spazi1                 1      1
017400041006     D  TIPREC                 2      3
017500041006     D  DAT117                 4    120
017600041006     D* ds necessarie per call pgm API per reperire i millesimi di sec.
017700041006     Dinpfmt           S             10    INZ( '*CURRENT' )
017800041006     DinpTIM           S             16    INZ( *ZEROS )
017900041006     DOUTFMT           S             10    INZ( '*YYMD'  )
018000041006     D*---------
018100041006     D Curdate         DS
018200041006     D Curdat                         8S 0
018300041006     D CurTim                         9S 0
018400041006     D*---------
018500041006     D errdata         DS
018600041006     D bytesprov               1      4B 0 inz( 272 )
018700041006     D bytesaval               5      8B 0
018800041006     D excpid                  9     15A
018900041006     D reserved               16     16A
019000041006     D excpdata               17    272A
019100021023     C*----------------------------------------------------*
019200021023     C*        M A I N   L I N E   P R O G R A M           *
019300021023     C*----------------------------------------------------*
019400080718C1935C*
019500080718C1935C                   Open      CBITX
019600080718C1935C                   Open      NDRBW00F
019700080718C1935C*
019800021023     C                   EXSR      INZVAR
019900021023     C*
020000021023     C                   EXSR      CBI
020100080703C1935C*
020200080718  "  C                   Close     CBITX
020300080718  "  C                   Close     NDRBW00F
020400080718  "  C*
020500080703  "  C* se previsto, eseguo lo scarico anche su file IFS
020600090213  "  C                   Eval      FileIFS = *Blanks
020700080703  "  C                   If        IndIfsSK90 <> *Blanks
020800080703  "  C                   ExSr      ScaricoIFS
020900080703C1935C                   EndIf
021000021023     C*
021100021023     C     FINE          TAG
021200021023     C                   MOVEL     NDCK73DS      KPJBU
021300021023     C                   SETON                                        LR
021400021023     C*
021500021023     C***------------------------------------------------***
021600021023     C**--------------- SCRIVO FILE CBI -----------------***
021700021023     C***------------------------------------------------***
021800021023     C     CBI           BEGSR                                                  *
021900021023     C*
022000021023     C                   SETOFF                                           99    *
022100021023     C*
022200021023     C     SocK73        CHAIN     ANSIF01L                           21        *
022300021023     C     *IN21         IFEQ      '0'
022400021023     C     SIFSalDoc     IfNe      *Blank
022500021023     C                   MOVEL     SIFCdSIA      CODSIA                         *
022600021023     C                   MOVEL     SIFCdSIAGr    CODSI2                         *
022700021023     C     CODSIA        COMP      *BLANK                                 21    *
022800021023     C  N21CODSI2        COMP      *BLANK                                 21    *
022900021023     C                   Else
023000021023     C                   SetOn                                        21
023100021023     C                   EndIf
023200021023     C                   ENDIF
023300021023     C     *IN21         IFEQ      '1'
023400021023     C                   Move      '3'           ErrK73
023500021023     C                   GOTO      FINE
023600021023     C                   ENDIF
023700021023     C*
023800071113C2062C                   Z-ADD     0             DSP16                          *
023900021023     C                   Z-ADD     0             RBDS14                         *
024000021023     C                   Z-ADD     0             DSP20                          *
024100021023     C                   Z-ADD     0             RBDS30                         *
024200021023     C                   Z-ADD     0             RBDS40                         *
024300021023     C                   Z-ADD     0             DSP50                          *
024400021023     C                   Z-ADD     0             RBDS51                         *
024500021023     C                   Z-ADD     0             DSP70                          *
024600021023     C                   Z-ADD     0             PROG                           *
024700021023     C                   CLEAR                   RAP
024800021023     C                   CLEAR                   COS
024900021023     C*
025000021023     C* Lettura effetti da inviare
025100021023     C                   Move      '1'           EFRTip
025200021023     C                   Move      NDtK73        EFRNDt
025300021023     C                   Move      DDtK73        EFRDDt
025400021023     C     k04efr01      SETLL     NDEFR01L                                     *
025500021023     C*
025600021023     C     READ          TAG                                                    *
025700021023     C*
025800021023     C     k04efr01      READE     NDEFR01L                               21    *
025900021023     C   21              GOTO      SOTTO                                        *
026000021023     C                   Clear                   UNIDesBrev
026100021023     C*
026200021023     C* Considero solo gli effetti non spediti
026300021023     C     EFRSTA        IFEQ      'I'
026400021023     C*
026500021023     C                   Move      'CG'          EFACtb
026600021023     C                   Move      EFRNEf        EFANrEff
026700021023     C     k03efa02      CHAIN     NDEFA02L                           22
026800021023     C     *In22         IfEq      *Off
026900021023     C* Unità di registrazione
027000021023     C                   EVAL      REGSYS = EFASYS
027100021023     C                   EVAL      REGNRASREG = EFANRASREG
027200021023     C     k02reg01      CHAIN     NDREG01L                           22
027300021023     C     *In22         IfEq      *Off
027400021023     C     REGUnita      AndNe     *Blank
027500021023     C                   EVAL      UNIUnita = REGUnita
027600021023     C     k02uni01      CHAIN     ANUNI01L                           22
027700021023     C                   EndIf
027800021023     C                   Else
027900021023     C                   Clear                   NDEFA000
028000021023     C                   EndIf
028100021023     C*
028200041007     C* SCRIVO TIPO RECORD DI TESTA *** IR ***
028300021023     C*
028400021023     C  N99              DO                                                     *
028500041007     C                   MOVE      'IR'          TIPREC                         *
028600021023     C                   MOVEL     CODSI2        RBTAZN                         *
028700021023     C*
028800021023     C                   If        EfrABO = *Blank or
028900021023     C                             EfrCAO = *Blank or
029000021023     C                             EfrCCO = *Blank
029100021023     C                   Move      '2'           ErrK73
029200021023     C                   GOTO      FINE
029300021023     C                   EndIf
029400021023     C*
029500021023     C                   MOVEL     EFRABO        RBTABI                         *
029600021023     C                   Move      EFRDat        WWDataISO
029700021023     C                   If        WWDataISO <> *LoVal
029800021023     C     *DMY          Move      WWDataISO     RBTCRE
029900021023     C                   Else
030000021023     C                   Clear                   RBTCRE
030100021023     C                   EndIf
030200021023     C                   MOVEL     BRBRM         CRBNOM                         *
030300041006     C* reperisce i millesimi di secondo
030400041006     C                   CALL      'QWCCVTDT'
030500041006     C                   PARM                    INPFMT
030600041006     C                   PARM                    INPTIM
030700041006     C                   PARM                    OUTFMT
030800041006     C                   PARM                    CURDATE
030900041006     C                   PARM                    ERRDATA
031000041006     C                   Move      CurDate       CRBNOM
031100021023     C                   MOVEL     *BLANKS       CRBLIB
031200021023     C                   MOVEL     *BLANK        RBB1RB                         *
031300021023     C                   MOVEL     *BLANK        RBB2RB                         *
031400021023     C* Reperisco il codice internazionale della divisa
031500021023     C                   Move      *Blank        WWCdIntDiv
031600021023     C                   If        X0201RpDVS(EFADivisa: EFADtEff:
031700021023     C                                       ANDVS00F: %size(ANDVS00F):
031800021023     C                                       ANDVX00F: %size(ANDVX00F)) <> 1
031900021023     C                   Move      DVSCdInter    WWCdIntDiv
032000021023     C                   EndIf
032100021023     C* Se codice internazionale della divisa è 'EUR' (euro)
032200021023     C*  imposto 'E' in codice divisa, altrimenti 'I' (lire)
032300021023     C                   If        WWCdIntDiv = 'EUR'
032400021023     C                   Move      'E'           RBTCDD
032500021023     C                   Else
032600021023     C                   Move      'I'           RBTCDD
032700021023     C                   EndIf
032800021023     C                   ADD       1             PROG                           *
032900021023     C                   MOVE      NDCKRBRB      DAT117                         *
033000021023     C                   WRITE     CBIR                                         *
033100041006     C                   If        ArcScaK90=*on
033200041006     C                   MOVEL     DATCAT        RBWREK                         *
033300041006     C                   WRITE     NDRBW000                                     *
033400041006     c                   endif
033500021023     C                   SETON                                            99    *
033600021023     C                   END                                                    *
033700021023     C*
033800041007     C* SCRIVO TIPO RECORD *** 10 ***
033900021023     C*
034000041007     C                   MOVE      '10'          TIPREC                         *
034100021023     C                   ADD       1             RBDS14                         *
034200021023     C                   MOVE      RBTCRE        RBDCRE                         *
034300021023     C                   Z-ADD     0             RBVSIA                         *
034400021023     C                   Move      EFADtSca      WWDataISO
034500021023     C                   If        WWDataISO <> *LoVal
034600021023     C     *DMY          Move      WWDataISO     RBDSCA
034700021023     C                   Else
034800021023     C                   Clear                   RBDSCA
034900021023     C                   EndIf
035000041007     C                   Z-ADD     50000         RBCAUS                         *
035100021023     C* Se codice internazionale della divisa è 'EUR' (euro)
035200021023     C*  le ultime 2 posizioni dell'importo sono i decimali
035300021023     C*  (quindi moltiplico l'importo per 100)
035400021023     C                   Z-ADD     EFAImportD    WWImporto                      *
035500021023     C                   If        WWCdIntDiv = 'EUR'
035600021023     C                   Mult      100           WWImporto
035700021023     C                   EndIf
035800021023     C                   Z-ADD     WWImporto     RBIMPO                         *
035900021023     C                   ADD       WWImporto     IMPGEN                         *
036000021023     C                   MOVE      '-'           RBSEGN                         *
036100021023     C                   MOVE      EFRABO        RBOABI                         *
036200021023     C                   MOVE      EFRCAO        RBOCAB                         *
036300021023     C                   MOVEL     EFRCCO        RBOCOC                         *
036400021023     C                   MOVE      EFAABI        RBDABI                         *
036500021023     C                   MOVE      EFACAB        RBDCAB                         *
036600021023     C                   MOVEL     EFRCCB        RBDCOC                         *
036700021023     C                   MOVEL     CODSIA        RBAAZN                         *
036800021023     C* Tipo individuo = cliente
036900021023     C                   MOVE      '4'           RBATCO                         *
037000050124D1728C****               MOVEL     *BLANK        RBACLI
037100050124D1728C****               MOVEL     EFAKcc        RBAKCC                         *
037200061215D2101C*** ex D1728       MOVEL     EFaKCC        RBACLI
037300061215D2101C****               MOVEL     EFAKsc        RBAKSC                         *
037400061215D2101C                   MoveL     EFAKCC        RDBKcc
037500061215D2101C                   MoveL     EFAKsc        RDBKsc                         *
037600061215D2101C                   Move      *BLANKS       RDBFil
037700050124D1728C***                Z-ADD     EFRNEF        RDBSOR                         *
037800050124D1728C                   Movel     *blanks       RDBSO1                         *
037900041006     C                   MOVEL     *BLANKS       RDBTFD
038000021023     C* Se codice internazionale della divisa è 'EUR' (euro)
038100021023     C*  imposto 'E' in codice divisa, altrimenti 'I' (lire)
038200021023     C                   If        WWCdIntDiv = 'EUR'
038300021023     C                   Move      'E'           RBCDD
038400021023     C                   Else
038500021023     C                   Move      'I'           RBCDD
038600021023     C                   EndIf
038700021023     C                   MOVE      NDCKRB14      DAT117                         *
038800021023     C                   ADD       1             PROG                           *
038900021023     C                   WRITE     CBIR                                         *
039000041006     C                   If        ArcScaK90=*on
039100041006     C                   MOVEL     DATCAT        RBWREK                         *
039200041006     C                   WRITE     NDRBW000                                     *
039300041006     c                   endif
039400071113C2062C*
039500071113  "  C* SCRIVO TIPO RECORD *** 16 ***
039600071113  "  C*  Recupero coordinata IBAN
039700071113  "  C                   Move      *off          $err
039800071113  "  C                   exsr      RepIBAN
039900071113  "  C*
040000071113  "  C                   If        $err       = *off
040100071113  "  C                   MOVE      '16'          TIPREC                         *
040200071113  "  C                   move      RBDS14        DSP16                          *
040300071113  "  C                   MOVEL     $Stato        Sta16                          *
040400071113  "  C                   MOVEL     $Check        Chk16                          *
040500071113  "  C                   MOVE      *blanks       BBA16                          *
040600071113  "  C                   MOVEL     $coori        BBA16                          *
040700071113  "  C                   MOVEL     TR16          DAT117                         *
040800071113  "  C                   MOVEL     *BLANK        FI116                          *
040900071113  "  C                   ADD       1             PROG                           *
041000071113  "  C                   WRITE     CBIR                                         *
041100071113  "  C                   If        ArcScaK90=*on
041200071113  "  C                   MOVEL     DATCAT        RBWREK                         *
041300071113  "  C                   WRITE     NDRBW000                                     *
041400071113  "  c                   endif
041500071113C2062C                   EndIf
041600021023     C*
041700021023     C* SCRIVO TIPO RECORD *** 20 ***
041800021023     C*
041900021023     C                   MOVE      '20'          TIPREC                         *
042000021023     C                   ADD       1             DSP20                          *
042100021023     C                   MOVEL     *BLANK        FI120                          *
042200021023     C                   MOVEL     B20DO1        DO120
042300021023     C                   MOVEL     B20DO2        DO220
042400021023     C                   MOVEL     B20DO3        DO320
042500021023     C                   MOVEL     B20DO4        DO420
042600021023     C                   MOVEL     TR20          DAT117                         *
042700021023     C                   ADD       1             PROG                           *
042800021023     C                   WRITE     CBIR                                         *
042900041006     C                   If        ArcScaK90=*on
043000041006     C                   MOVEL     DATCAT        RBWREK                         *
043100041006     C                   WRITE     NDRBW000                                     *
043200041006     c                   endif
043300021023     C*
043400021023     C* SCRIVO TIPO RECORD *** 30 ***
043500021023     C*
043600021023     C                   MOVE      '30'          TIPREC                         *
043700021023     C                   ADD       1             RBDS30                         *
043800021023     C                   MOVEL     EFRRAG        RBDRS1                         *
043900021023     C                   MOVEL     *BLANK        RBDRS2                         *
044000021023     C                   MOVEL     EFRPIV        RBCF30                         *
044100021023     C                   MOVEL     *BLANK        RBBB30                         *
044200021023     C                   MOVE      NDCKRB30      DAT117                         *
044300021023     C                   ADD       1             PROG                           *
044400021023     C                   WRITE     CBIR                                         *
044500041006     C                   If        ArcScaK90=*on
044600041006     C                   MOVEL     DATCAT        RBWREK                         *
044700041006     C                   WRITE     NDRBW000                                     *
044800041006     c                   endif
044900021023     C*
045000021023     C* SCRIVO TIPO RECORD *** 40 ***
045100021023     C*
045200021023     C                   MOVE      '40'          TIPREC                         *
045300021023     C                   ADD       1             RBDS40                         *
045400021023     C                   MOVEL     EFRIND        RBDIND                         *
045500021023     C                   MOVEL     EFRCAP        RBDCAP                         *
045600021023     C                   MOVEL     EFRLOC        RBDLOC                         *
045700021023     C                   MOVE      EFRPRV        RBDLOC                         *
045800021023     C* descrizione banca
045900021023     C                   Reset                   X59AbiDs
046000021023     C                   Movel     XscSoc        X59Soc
046100021023     C                   MOVE      EFAABI        X59ABI
046200021023     C                   MOVE      EFACAB        X59CAB
046300021023     C                   Call      'X59ABI'
046400021023     C                   Parm                    X59AbiDs
046500021023     C                   If        X59Err <> '1'
046600021023     C                   Movel     *BLANK        RBSPTR                         *
046700021023     C                   Movel     X59DesBrIs    RBSPTR                         *
046800021023     C     RbsPtr        Cat       X59DesAge:1   RbsPtr
046900021023     C                   Else
047000021023     C                   Movel     *BLANK        RBSPTR                         *
047100021023     C                   EndIf
047200021023     C                   MOVEL     *BLANK        RBBB40                         *
047300021023     C                   MOVE      NDCKRB40      DAT117                         *
047400021023     C                   ADD       1             PROG                           *
047500021023     C                   WRITE     CBIR                                         *
047600041006     C                   If        ArcScaK90=*on
047700041006     C                   MOVEL     DATCAT        RBWREK                         *
047800041006     C                   WRITE     NDRBW000                                     *
047900041006     c                   endif
048000021023     C*
048100021023     C* SCRIVO TIPO RECORD *** 50 ***
048200021023     C*
048300021023     C                   MOVE      '50'          TIPREC                         *
048400021023     C                   ADD       1             DSP50                          *
048500050124D1728C                   ADD       1             RBDS51                         *
048600021023     C                   MOVEL     DE1K03        II150                          *
048700050124     C                   MOVE      EFANrPar      II250                          *
048800050124     C                   Move      EFADtPar      WWDataISO
048900021023     C                   If        WWDataISO <> *LoVal
049000021023     C     *DMY          Move      WWDataISO     WWData6
049100021023     C                   Else
049200021023     C                   Clear                   WWData6
049300021023     C                   EndIf
049400041118c1790x***                Move      WWData6       II350
049500041118  "  c                   Move      EfaDtPar      II350
049600041118  "  X***                MOVEL     *BLANK        II450                          *
049700041118  "  C                   move      EfaNreff      ChNrEff           9
049800041118C1790C                   MOVE      ChNrEff       II450                          *
049900021023     C                   MOVEL     DE2K03        II550                          *
050000021023     C                   CAT       UNIDesBrev:1  II550                          *
050100021023     C                   MOVEL     DE3K03        II650                          *
050200041006     C                   If        XSCCdf       <> *blanks
050300041006     C                   Movel     XSCCdf        CFP50
050400041006     C                   ElSe
050500041006     C                   Movel     XSCIva        CFP50
050600041006     C                   endif
050700021023     C                   MOVEL     TR50          DAT117                         *
050800021023     C                   ADD       1             PROG                           *
050900021023     C                   WRITE     CBIR                                         *
051000041006     C                   If        ArcScaK90=*on
051100041006     C                   MOVEL     DATCAT        RBWREK                         *
051200041006     C                   WRITE     NDRBW000                                     *
051300041006     c                   endif
051400021023     C*
051500021023     C* SCRIVO TIPO RECORD *** 70 ***
051600021023     C*
051700021023     C                   MOVE      '70'          TIPREC                         *
051800021023     C                   ADD       1             DSP70                          *
051900021023     C                   MOVEL     *BLANK        FI170                          *
052000021023     C                   MOVEL     TR70          DAT117                         *
052100021023     C                   ADD       1             PROG                           *
052200021023     C                   WRITE     CBIR                                         *
052300041006     C                   If        ArcScaK90=*on
052400041006     C                   MOVEL     DATCAT        RBWREK                         *
052500041006     C                   WRITE     NDRBW000                                     *
052600041006     c                   endif
052700021023     C*
052800021023     C* AGGIORNAMENTO STATO RECORD NDEFR00F COME "SPEDITO"
052900021023     C*
053000021023     C                   MOVE      'S'           EFRSTA                         *
053100021023     C                   UPDATE    NDEFR000                                     *
053200021023     C*
053300021023     C                   ENDIF
053400021023     C*
053500021023     C                   GOTO      READ                                         *
053600021023     C*
053700021023     C* SCRIVO TIPO RECORD DI CODA  *** EF ***
053800021023     C*
053900021023     C     SOTTO         TAG                                                    *
054000021023     C                   If        *in99      = *on
054100021023     C                   MOVE      'EF'          TIPREC                         *
054200021023     C                   MOVEL     CODSI2        SIAEF                          *
054300021023     C                   MOVE      EFRABO        ABIEF                          *
054400021023     C                   Move      EFRDat        WWDataISO
054500021023     C                   If        WWDataISO <> *LoVal
054600021023     C     *DMY          Move      WWDataISO     DTCEF
054700021023     C                   Else
054800021023     C                   Clear                   DTCEF
054900021023     C                   EndIf
055000021023     C                   MOVEL     BEFRM         DESEF                          *
055100041006     c                   Move      CurDate       DESEF
055200021023     C                   MOVEL     *BLANK        FI1EF                          *
055300050124     C                   Z-ADD     RBDS51        DISEF                          *
055400021023     C                   Z-ADD     IMPGEN        INCEF                          *
055500021023     C                   Z-ADD     *ZERO         PAGEF                          *
055600021023     C                   ADD       1             PROG                           *
055700021023     C                   Z-ADD     PROG          NRPEF                          *
055800021023     C                   MOVEL     *BLANK        FI2EF                          *
055900021023     C                   MOVEL     *BLANK        FI3EF                          *
056000021023     C* Se codice internazionale della divisa è 'EUR' (euro)
056100021023     C*  imposto 'E' in codice divisa, altrimenti 'I' (lire)
056200021023     C                   If        WWCdIntDiv = 'EUR'
056300021023     C                   Move      'E'           CDDEF
056400021023     C                   Else
056500021023     C                   Move      'I'           CDDEF
056600021023     C                   EndIf
056700021023     C                   MOVEL     TREF          DAT117                         *
056800021023     C                   WRITE     CBIR                                         *
056900041006     C                   If        ArcScaK90=*on
057000041006     C                   MOVEL     DATCAT        RBWREK                         *
057100041006     C                   WRITE     NDRBW000                                     *
057200041006     c                   endif
057300021023     C*
057400021023     C                   Z-ADD     0             IMPGEN
057500021023     C*
057600021023     C* SCRIVO FILE COSTI
057700021023     C*
057800021023     C                   EXSR      ACCOUN                                       *
057900021023     C*
058000021023     C* SCRIVO RECORD DI LOG SPEDIZIONE
058100021023     C*
058200021023     C                   MOVE      SocK73        LOGSOCIETA
058300021023     C                   Z-ADD     NrjK73        LOGAZI
058400021023     C                   Z-ADD     EFRNRJ        LOGAZO
058500021023     C                   MOVEL     UteK73        LOGUTE
058600021023     C                   MOVEL     DspK73        LOGVID
058700021023     C                   MOVEL     InfK73        LOGSIF
058800021023     C                   MOVEL     'RIBA'        LOGTIP
058900021023     C                   MOVE      Oggi          LOGUDA
059000021023     C                   TIME                    LOGTIM
059100021023     C                   MOVEL     EFABanca      LOGBANCA
059200021023     C                   MOVEL     EFALinea      LOGLINEA
059300021023     C                   Z-ADD     EFANrDist     LOGNDT
059400021023     C                   Z-ADD     EFANrEff      LOGNEF
059500021023     C                   Z-ADD     INCEF         LOGIMP
059600021023     C                   Z-ADD     EFASys        LOGSYS
059700021023     C                   Z-ADD     EFANrAsReg    LOGNRA
059800021023     C                   WRITE     NDLOG000
059900021023     C                   EndIf
060000021023     C*
060100021023     C                   ENDSR                                                  *
060200021023     C***------------------------------------------------***
060300021023     C**--------- SCRIVO FILE DI ACCOUNTING -------------***
060400021023     C***------------------------------------------------***
060500021023     C     ACCOUN        BEGSR                                                  *
060600021023     C*
060700021023     C                   Setoff                                       85
060800021023     C                   Z-ADD     1             K
060900021023     C*
061000021023     C                   MOVE      SocK73        COSSOCIETA                     *
061100021023     C                   MOVEL     CODSIA        COSSIA                         *
061200021023     C                   MOVEL     'RIBA'        COSCOD                         *
061300021023     C                   Z-ADD     EFRNDT        COSNRT                         *
061400021023     C                   MOVE      EFRDAT        COSDAT                         *
061500021023     C                   Z-ADD     EFRNRJ        COSNRJ                         *
061600021023     C     RAP(K)        DOWNE     *BLANK
061700021023     C                   MOVE      XSCCUT        XTBUTE
061800021023     C                   MOVE      '1'           XTBRIC
061900021023     C                   MOVE      XSCSOC        XTBAZI
062000021023     C                   MOVE      *ZERO         XTBKEY
062100021023     C                   MOVE      'K04'         XTBCOD
062200021023     C                   MOVE      COSCOD        KEYCodice
062300021023     C                   MOVEL     RAP(K)        KEYBanca
062400021023     C                   MOVE      RAP(K)        KEYLinea
062500021023     C                   MOVE      KEYTAB        XTBKEY
062600021023     C                   CALLb     'XATB'
062700021023     C                   PARM                    XATBDS
062800021023     C     XTBERR        IFEQ      *ON
062900021023     C* Prova solo codice
063000021023     C                   MOVE      *BLANK        KEYBanca
063100021023     C                   MOVE      *BLANK        KEYLinea
063200021023     C                   MOVE      KEYTAB        XTBKEY
063300021023     C                   CALLb     'XATB'
063400021023     C                   PARM                    XATBDS
063500021023     C     XTBERR        IFEQ      *ON
063600021023     C* Prova solo con codice generico '****'
063700021023     C                   MOVE      *ALL'*'       KEYCodice
063800021023     C                   MOVE      KEYTAB        XTBKEY
063900021023     C                   CALLb     'XATB'
064000021023     C                   PARM                    XATBDS
064100021023     C     XTBERR        IFEQ      *ON
064200021023     C                   SETON                                        85
064300021023     C                   ENDIF
064400021023     C                   ENDIF
064500021023     C                   ENDIF
064600021023     C   85              Z-ADD     0             COSTO            19 3
064700021023     C  N85              MOVEL     XTBUNI        TABK04
064800021023     C  N85              Z-ADD     IMPK04        COSTO
064900021023     C                   MOVEL     RAP(K)        COSBANCA
065000021023     C                   MOVE      RAP(K)        COSLINEA
065100021023     C                   Z-ADD     COS(K)        COSTNT
065200021023     C     COS(K)        MULT      COSTO         COSCTR
065300021023     C                   WRITE     NDCOS000                                     *
065400021023     C                   ADD       1             K                 5 0
065500021023     C                   ENDDO
065600021023     C*
065700021023     C                   ENDSR                                                  *
065800021023     C***------------------------------------------------***
065900021023     C**----------- OPERAZIONI INIZIALI -----------------***
066000021023     C***------------------------------------------------***
066100021023     C     INZVAR        BEGSR
066200021023     C*
066300021023     C     *ENTRY        PLIST                                                  *
066400021023     C                   PARM                    KPJBA                          *
066500090206C1935C                   PARM                    FILEIFS         255            *
066600090206  "  C*
066700090206C1935C                   CLEAR                   FILEIFS
066800021023     C*
066900021023     C                   MOVEL     KPJBU         NDCK73DS
067000021023     C*
067100021023     C* KList
067200021023     C*
067300021023     C     k03efa02      KLIST                                                  *
067400021023     C                   KFLD                    SocK73                         *
067500021023     C                   KFLD                    EFACtb                         *
067600021023     C                   KFLD                    EFANrEff                       *
067700021023     C*
067800021023     C     k04efr01      KLIST                                                  *
067900021023     C                   KFLD                    SocK73                         *
068000021023     C                   KFLD                    EFRTip                         *
068100021023     C                   KFLD                    EFRNDt                         *
068200021023     C                   KFLD                    EFRDDt                         *
068300021023     C*
068400021023     C     k05dpr01      KLIST
068500021023    >C                   KFLD                    SocK73
068600021023    >C                   KFLD                    DPRCtb
068700021023    >C                   KFLD                    DPRTipo
068800021023    >C                   KFLD                    DPRNrDist
068900021023     C                   KFLD                    DPRDtDist
069000021023     C*
069100021023     C     k05rbt00      KLIST
069200021023     C                   KFLD                    SocK73
069300021023     C                   KFLD                    RBTUnita
069400021023     C                   KFLD                    RBTTip
069500021023     C                   KFLD                    RBTBan
069600021023     C                   KFLD                    RBTPrg
069700021023     C*
069800021023     C     k02reg01      KLIST
069900021023     C                   KFLD                    REGSys
070000021023     C                   KFLD                    REGNrAsReg
070100021023     C*
070200021023     C     k02uni01      KLIST
070300021023     C                   KFLD                    SocK73
070400021023     C                   KFLD                    UniUnita
070500071113      *
070600071113c2062C     K02Acr01      KLIST
070700071113  "  C                   kfld                    AcrAbi
070800071113c2062C                   kfld                    AcrCab
070900021023     C*
071000021023     C                   MOVE      SocK73        SOCXSC
071100021023     C                   MOVEL     'SOC001'      TIPXSC
071200021023     C                   EXSR      REPSOC
071300021023     C     RTNXSC        IFNE      '1'
071400021023     C                   MOVEL     XSOCDS        SOC001
071500021023     C                   ELSE
071600021023     C                   Move      *On           ErrK73
071700021023     C                   GOTO      FINE
071800021023     C                   ENDIF
071900041006     C*
072000041006     C* Reperimento parametri di base H.B.
072100041006     C*
072200041006     C                   CallB     'XPAR'
072300041006     C                   Parm      XScSoc        XPaSoc
072400041006     C                   Parm      'HBPARBAS'    XPaPar
072500041006     C                   Parm                    XPaOut
072600041006     C                   Parm      *Off          XPaErr
072700041006     C                   Parm      *Blank        XPaRic
072800041006     C     XPaErr        IfEq      *Off
072900041006     C                   Movel     XPaOut        NdcK90DS
073000041006     C                   EndIf
073100021023     C*
073200021023     C     *DMY          MOVE      UDate         Oggi
073300021023     C*
073400021023     C* Tabella descrizioni
073500021023     C                   MOVE      XSCCUT        XTBUTE
073600021023     C                   MOVE      '1'           XTBRIC
073700021023     C                   MOVE      XSCSOC        XTBAZI
073800021023     C                   MOVE      *ZERO         XTBKEY
073900021023     C                   MOVE      'K03'         XTBCOD
074000050124D1728C****               MOVE      'RID'         XTBKEY
074100050124D1728C                   MOVE      'RID '        XTBKEY
074200021023     C                   CALLb     'XATB'
074300021023     C                   PARM                    XATBDS
074400021023     C     XTBERR        IFEQ      *OFF
074500021023     C                   MOVEL     XTBUNI        TABK03
074600021023     C                   ENDIF
074700021023     C*
074800021023     C* Tabella attribuzioni costi
074900021023     C                   MOVE      XSCCUT        XTBUTE
075000021023     C                   MOVE      '1'           XTBRIC
075100021023     C                   MOVE      XSCSOC        XTBAZI
075200021023     C                   MOVE      *ZERO         XTBKEY
075300021023     C                   MOVE      'K04'         XTBCOD
075400041007     C                   MOVE      'RID'         KEYCodice
075500021023     C                   MOVE      *BLANK        KEYBanca
075600021023     C                   MOVE      *BLANK        KEYLinea
075700021023     C                   MOVE      KEYTAB        XTBKEY
075800021023     C                   CALLb     'XATB'
075900021023     C                   PARM                    XATBDS
076000021023     C     XTBERR        IFEQ      *OFF
076100021023     C                   MOVEL     XTBUNI        TABK04
076200021023     C                   ELSE
076300021023     C                   MOVE      *ALL'*'       KEYCodice
076400021023     C                   MOVE      *BLANK        KEYBanca
076500021023     C                   MOVE      *BLANK        KEYLinea
076600021023     C                   MOVE      KEYTAB        XTBKEY
076700021023     C                   CALLb     'XATB'
076800021023     C                   PARM                    XATBDS
076900021023     C     XTBERR        IFEQ      *OFF
077000021023     C                   MOVEL     XTBUNI        TABK04
077100021023     C                   ENDIF
077200021023     C                   ENDIF
077300021023     C*
077400021023     C* Recupero unità/banca di presentazione
077500021023     C                   Eval      DprCtb = 'CG'
077600021023     C                   Eval      DprTipo = '1'
077700021023     C                   Eval      DprNrDist = NDtK73
077800021023     C                   Eval      DprDtDist = DDtK73
077900021023     C     k05dpr01      Chain     NdDpr01l                           21
078000021023     C     *In21         IfEq      *On
078100021023     C                   Move      *On           ErrK73
078200021023     C                   GOTO      FINE
078300021023     C                   EndIf
078400021023     C*
078500041007     C* Recupero immagini Ri.Ba./Rid
078600021023     C                   Exsr      ImmRiBa
078700021023     C*
078800021023     C                   ENDSR
078900021023     C************************************************************
079000021023     C* Reperimento immagini Ri.Ba.
079100021023     C************************************************************
079200021023     C     ImmRiBa       BEGSR
079300021023     C*
079400021023     C                   Move      DprUnita      RbtUnita
079500021023     C                   Move      'RB'          RbtTip
079600021023     C                   Move      DprBanca      RbtBan
079700021023     C*
079800021023     C                   DO        9             J                 3 0
079900021023     C                   Z-ADD     J             RbtPrg
080000021023     C*
080100021023     C     k05rbt00      Chain     NdRbt00F                           21
080200021023     C*
080300021023     C                   If        *In21 = *On
080400021023     C                   Clear                   RbtUnita
080500021023     C     k05rbt00      Chain     NdRbt00F                           21
080600021023     C                   Move      DprUnita      RbtUnita
080700021023     C                   EndIf
080800021023     C*
080900021023     C                   If        *in21 = *On
081000021023     C                   Clear                   RbtUnita
081100021023     C                   Clear                   RbtBan
081200021023     C     k05rbt00      Chain     NdRbt00F                           21
081300021023     C                   Move      DprUnita      RbtUnita
081400021023     C                   Move      DprBanca      RbtBan
081500021023     C                   EndIf
081600021023     C*
081700021023     C                   Select
081800021023     C                   When      RbtPrg = 1
081900021023     C                   Movel     RbtRek        DSRBRB
082000021023     C                   When      RbtPrg = 2
082100021023     C                   Movel     RbtRek        DSRB14
082200021023     C                   When      RbtPrg = 3
082300021023     C                   Movel     RbtRek        DSRB20
082400021023     C                   When      RbtPrg = 4
082500021023     C                   Movel     RbtRek        DSRB30
082600021023     C                   When      RbtPrg = 5
082700021023     C                   Movel     RbtRek        DSRB40
082800021023     C                   When      RbtPrg = 6
082900021023     C                   Movel     RbtRek        DSRB50
083000021023     C                   When      RbtPrg = 7
083100021023     C                   Movel     RbtRek        DSRB51
083200021023     C                   When      RbtPrg = 8
083300021023     C                   Movel     RbtRek        DSRB70
083400021023     C                   When      RbtPrg = 9
083500021023     C                   Movel     RbtRek        DSRBEF
083600021023     C                   EndSl
083700021023     C*
083800021023     C                   EndDo
083900021023     C*
084000021023     C                   ENDSR
084100021023     C************************************************************
084200021023     C* Reperimento dati società
084300021023     C************************************************************
084400021023     C     REPSOC        BEGSR
084500021023     C*
084600021023     C                   CALLB     'XSOC'
084700021023     C                   PARM                    TIPXSC            6
084800021023     C                   PARM                    SOCXSC            3
084900021023     C                   PARM                    CDSXSC            9 0
085000021023     C                   PARM                    MODXSC            3
085100021023     C                   PARM      *BLANK        RTNXSC            1
085200021023     C                   PARM                    XSOCDS
085300021023     C                   PARM                    KPJBA
085400021023     C*
085500021023     C                   ENDSR
085600080703C1935C************************************************************
085700080703  "  C* Scarico file su IFS
085800080703  "  C************************************************************
085900080703C1935C     ScaricoIFS    BEGSR
086000080703     C*
086100080703     C                   If        ArcScaK90 = *On
086200080714     C                   Eval      Lib1001 = NomLibK73
086300080703     C                   Eval      File1001 = NomFilK73
086400080703     C                   Else
086500080714     C                   Eval      Lib1001 = Libk90
086600080703     C                   Eval      File1001 = 'CBITX'
086700080703     C                   EndIf
086800080703     C*
086900080703     C                   Eval      DirIfs1001 = IndIfsSk90
087000080703     C                   Eval      Est1001 = *Blanks
087100080703     C                   Eval      DesFile1001 = *Blanks
087200080703     C                   Eval      Rnm1001 = File1001
087300080703     C*
087400080703     C                   CallB     'PJXX1001C'
087500080703     C                   Parm                    DirIfs1001      255
087600080703     C                   Parm                    Est1001          10
087700080703     C                   Parm                    Lib1001          10
087800080703     C                   Parm                    File1001         10
087900080703     C                   Parm                    DesFile1001      50
088000080703     C                   Parm                    Rnm1001          50
088100080703     C                   Parm      *Off          Err1001           1
088200100622B1128C                   Parm      *Blank        Mbrxx1001        10
088300100622B1128C                   Parm      *Blank        Tpuxx1001         1
088400090206     C*
088500090206     C                   If        Err1001 = *Off
088600090206     C                   Eval      FileIFS = %Trim(DirIfs1001) + '/' +
088700090206     C                                       %Trim(Rnm1001)
088800090206     C                   EndIf
088900080703     C*
089000080703C1935C                   ENDSR
089100071113C2062C*****************************************************************
089200071113  "  C* Reperisco le coordinate estere iban
089300071113c2062C*****************************************************************
089400071113     C     RepIBAN       Begsr
089500071113     c*
089600071113     c                   clear                   $stato
089700071113     c                   clear                   $check
089800071113     c                   clear                   $coori
089900071113     c                   clear                   $err
090000071113     c                   clear                   $opz
090100071113     c*
090200071113     c                   eval      acrAbi=EfrAbo
090300071113     c                   eval      acrCab=EfrCAo
090400071113     c     K02Acr01      chain     Anacr01l
090500071113     c                   if        %found(anacr01L)
090600071113     C* Tabella stato
090700071113     C                   MOVE      XSCCUT        XTBUTE
090800071113     C                   MOVE      '1'           XTBRIC
090900071113     C                   MOVE      XSCSOC        XTBAZI
091000071113     C                   MOVE      *ZERO         XTBKEY
091100071113     C                   MOVE      'G12'         XTBCOD
091200071113     C                   MOVE      AcrStato      XTBKEY
091300071113     C                   CALLb     'XATB'
091400071113     C                   PARM                    XATBDS
091500071113     C     XTBERR        IFEQ      *OFF
091600071113     C                   MOVEL     XTBUNI        TABG12
091700071113     c                   eval      $Stato = NazG12
091800071113     C                   ENDIF
091900071113     c                   endif
092000071113     c*
092100071113     c* proseguo solo se italia
092200071113     C                   If        ($stato     = 'IT' or
092300071113     C                              $stato     = 'SM')
092400071113     C                   Eval      $Coori = EfrCIO+EfrABO +EfrCAO +EfrCCO
092500071113     c*
092600071113     C                   Call      'XCHKIBAN'
092700071113     C                   Parm                    $Stato
092800071113     C                   Parm                    $Check
092900071113     C                   Parm                    $Coori
093000071113     C                   Parm      *Off          $Err
093100071113     C                   Parm      '01'          $Opz
093200071113     c
093300071113     c                   endif
093400071113      *
093500071113C2062C                   ENDSR
093600071113     C************************************************************
