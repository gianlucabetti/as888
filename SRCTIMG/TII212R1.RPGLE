000100990419     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990419     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300850614     F* MPH932R1 *-MP/38-*-------------------------------------------*
000400000000     F*------------------*                                           *
000500850208     F*       - ESEGUE RIGENERAZIONE SALDI MAGAZZ. -MP-              *
000600850208     F*       -----------------------------------------              *
000700850208     F*                                                   AZN.: H931 *
000800000000     F*         FILE : MPPRTSTD-MPPIL00F-MPMMP01L-ANTAB00F           *
000900850208     F*                MPSMP01L                                      *
001000000000     F*                                                              *
001100000000     F*         PGM. : RIC001R   - RICERCA INTESTAZIONE DITTA        *
001200000000     F*--------------------------------------------------------------*
001300000000     F*     IMPOSTAZIONE CHIAVI PILOTA :                             *
001400000000     F*             PILK01 = MAGAZZINO                  'XXXXXXX   ' *
001500000000     F*             PILK02 = CLASSE O S/CLASSE          'XXX.X     ' *
001600000000     F*             PILK03 = CODICE PRODOTTO            'XXXXXXX   ' *
001700000000     F*             PILK04 = VERSIONE                   'XX        ' *
001800000000     F*             PILFIL = DESCRIZIONE PRODOTTO E UMA 'XXXX.. XXX' *
001900000000     F*--------------------------------------------------------------*
002000850208     F*     04      - RICHIESTA RIGEN. PER   PRODOTTO                *
002100850208     F*     05      - RICHIESTA RIGEN. PER CLASSE   MERCEOLOGICA     *
002200850208     F*     06      - RICHIESTA RIGEN. PER S/CLASSE MERCEOLOGICA     *
002300000000     F*--------------------------------------------------------------*
002400990419     FtmPIL00F  IP   E           K DISK
002500000000     FANTAB00F  IF   E           K DISK
002600990419     FtmMMP01L  UF   E           K DISK
002700850208     F                                     INFDS(INF1)
002800990419     FtmSMP01L  UF A E           K DISK
002900850208     FMPPRTSTD  O    F  132        PRINTER OFLIND(*INOF)
003000850208     D AZ              S              1    DIM(15)                              *AZIONI CAUS.
003100850208     D QTK             S             11  2 DIM(10)                              *Q.TA WORK SALDI
003200850208     D ERR             S             50    DIM(11) CTDATA PERRCD(1)             *ERRORI
003300000000     D SK1             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 1
003400000000     D SK2             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 2
003500000000     D SK3             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 3
003600000000     D SK4             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 4
003700850208     D INF1            DS           502
003800850208     D*-----INFORMATION DS X FILE  MPMMP00F :
003900850208     D  NREL1                397    400B 0
004000990419     D RECSAL        E DS                  EXTNAME(tmSMP00F)
004100850208     D*--------------------- ARCHIVIO SALDI :
004200990419     D  QTS                   86    145P 2
004300850208     D                                     DIM(10)                              *Q.TA'  SALDI
004400000000     D KPJBA         E DS
004500000000     D*----- PASSAGGIO PARAMETRI
004600990419     D  AAA930               247    250  0
004700990419     D  MAG930               251    257
004800990419     D  MAT930               276    287
004900990419     D  CLS930               267    269
005000990419     D  SCL930               270    274
005100990419     D  TIP930               275    275
005200000103     D  socpas               288    290
005300990419     D  MSGERR               453    502
005400990419     D soc001        E DS                  EXTNAME(xsoc001ds)
005500990419     D xsocds          DS          1000
005600990419     d dataiso         s               d   datfmt(*iso)
005700990419     d dataeur         s               d   datfmt(*eur)
005800990419     ItmPIL000      01
005900000000     I                                          PILK01        L4
006000000000     I                                          PILK02        L3
006100000000     I                                          PILK03        L2
006200000000     I                                          PILK04        L1
006300000000     C*----------------------------------------------------*
006400000000     C     *ENTRY        PLIST                                                  *
006500000000     C                   PARM                    KPJBA                          *
006600850208     C*---------- SALDI DI MAGAZZINO :                     *
006700850208     C*                                                    *
006800850208     C     KEYSMP        KLIST                                                  *
006900990419     C                   KFLD                    AA                4 0          *
007000850208     C                   KFLD                    MAGKKK            7            *
007100871019     C                   KFLD                    MATKKK           12            *
007200850208     C*---------- MOVIMENTI DI MAGAZZINO :                 *
007300850208     C     KEYMMP        KLIST                                                  *
007400850208     C                   KFLD                    AA                             *
007500850208     C                   KFLD                    MAGAZZ            7            *
007600871019     C                   KFLD                    PRODOT           12            *
007700000000     C*---------- ANAGRAFICO TABELLE :                     *
007800000000     C     KEYTAB        KLIST                                                  *
007900000000     C                   KFLD                    TABGRU                         *
008000000000     C                   KFLD                    TABCOD                         *
008100000000     C*---------- ROTTURA DI MAGAZZINO --------------------*
008200000000     C   L4              DO                                                     *
008300850208     C                   Z-ADD     0             CTRCLS            5 0          *
008400850208     C                   Z-ADD     0             CTRMAT            5 0          *
008500850208     C                   Z-ADD     0             CTRVER            7 0          *
008600850208     C                   Z-ADD     0             CTRMOV            7 0          *
008700850208     C                   Z-ADD     0             CTRERR            5 0          *
008800850208     C                   Z-ADD     55            PAGE1             3 0          *
008900000000     C*---------- RICERCA DITTA :                          *
009000000000     C  N02              DO                                                     *
009100990419     C                   MOVEL     'SOC001'      TIPXSC
009200990419     C                   MOVEL     *blank        SOCXSC
009300990419     C                   EXSR      REPSOC
009400990419     C     RTNXSC        IFNE      '1'
009500990419     C                   MOVEL     XSOCDS        SOC001
009600990419     C                   MOVEL     xscsoc        socpas
009700990419     C                   MOVEL     xscrgs        desdit           20
009800990419     c                   else
009900990419     c                   seton                                        90
010000990419     c   90              goto      fine
010100990419     c                   end
010200990419     c                   time                    como14           14 0
010300990419     c                   move      como14        woggi             8 0
010400990419     c                   move      woggi         dataeur
010500990419     c                   move      dataeur       dataiso
010600990419     c                   move      dataiso       woggig            8 0
010700850208     C*---------- TIPO SELEZIONE --------------------------*
010800850208     C     TIP930        COMP      '1'                                    04    * X PRODOTTO
010900850208     C     TIP930        COMP      '2'                                    05    * X CLASSE
011000850208     C     TIP930        COMP      '3'                                    06    * X S/CLASSE
011100850208     C                   Z-ADD     0             CTRMAG            5 0          *
011200850208     C                   Z-ADD     0             PAGE              4 0          *
011300990419     C                   move      woggig        DATMAN            8 0          *
011400850208     C                   SETON                                            02    *
011500850208     C                   END                                                    *
011600000000     C*---------- RICERCA MAGAZZINO -----------------------*
011700000000     C                   MOVEL     PILK01        MAGAZZ                         *
011800850208     C                   MOVEL     AAA930        AA                             *
011900000000     C                   MOVE      '153'         TABGRU                         *
012000000000     C                   MOVE      *ZEROS        TABCOD                         *
012100000000     C                   MOVE      MAGAZZ        TABCOD                         *
012200000000     C     KEYTAB        CHAIN     ANTAB00F                           91        *
012300000000     C   91              MOVEL     *ALL'*'       DESMAG           30            *
012400850208     C  N91              MOVEL     TABDES        DESMAG                         *
012500000000     C                   END                                                    *
012600850208     C*---------- ROTTURA DI CLASSE - S/CLASSE ------------*
012700850208     C   L3
012800850208     CANN04              DO                                                     *
012900850208     C*---------- RICERCA CLASSE --------------------------*
013000850208     C   05              DO                                                     *
013100850208     C                   MOVEL     PILK02        CLS               3            *
013200850208     C                   MOVE      '155'         TABGRU                         *
013300850208     C                   MOVE      *ZEROS        TABCOD                         *
013400850208     C                   MOVE      CLS           TABCOD                         *
013500850208     C     KEYTAB        CHAIN     ANTAB00F                           91        *
013600850208     C   91              MOVE      *ALL'*'       DESCLS           25            *
013700850208     C  N91              MOVEL     TABDES        DESCLS                         *
013800850208     C                   END                                                    *
013900850208     C*---------- RICERCA S/CLASSE ------------------------*
014000850208     C   06              DO                                                     *
014100850208     C                   MOVEL     PILK02        SCL               5            *
014200850208     C                   MOVE      '156'         TABGRU                         *
014300850208     C                   MOVE      *ZEROS        TABCOD                         *
014400850208     C                   MOVE      SCL           TABCOD                         *
014500850208     C     KEYTAB        CHAIN     ANTAB00F                           91        *
014600850208     C   91              MOVE      *ALL'*'       DESSCL           25            *
014700850208     C  N91              MOVEL     TABDES        DESSCL                         *
014800850208     C                   END                                                    *
014900850208     C                   END                                                    *
015000000000     C*---------- ROTTURA DI PRODOTTO ---------------------*
015100850613     C   L2              MOVEL     PILK03        PRODOT                         *
015200000000     C*---------- ROTTURA DI VERSIONE ---------------------*
015300000000     C   L1              DO                                                     *
015400850208     C                   MOVE      '...'         OLDCAU            3            *
015500850208     C                   Z-ADD     0             QTK                            *
015600850613     C                   MOVE      PILFIL        UMA               3            *
015700850613     C                   SETOFF                                           53    *
015800000000     C                   END                                                    *
015900850208     C*---------- DETTAGLIO DI PRODOTTO/VERSIONE ----------*
016000990419     C     KEYMMP        SETLL     tmMMP01L                           91  92    *
016100850208     C   91                                                                     *
016200850208     CORN92              GOTO      FINE                                         *
016300000000     C     RICMOV        TAG                                                    *
016400850208     C                   SETOFF                                         5152    *
016500990419     C     KEYMMP        READE     tmMMP01L                               91    *
016600000000     C   91              GOTO      FINE                                         *
016700850613     C*---------- CONTROLLO VERSIONE ----------------------*
016800850208     C                   MOVE      MAGAZZ        MAGKKK                         *
016900850208     C                   MOVE      PRODOT        MATKKK                         *
017000850208     C                   MOVE      UMA           UMAKKK            3            *
017100850613     C*---------- AZZERO SALDI ----------------------------*
017200850613     C  N53              MOVEA     MMPOPC        AZ                             *
017300850613     C  N53              EXSR      AGGSAL                                       *AZZERO SALDI
017400850613     C                   SETON                                            53    *
017500000000     C*---------- CONTROLLO CAUSALE :                      *
017600850208     C     MMPCAU        IFNE      OLDCAU                                       *
017700850208     C                   MOVE      '152'         TABGRU                         *
017800850208     C                   MOVE      *ZEROS        TABCOD                         *
017900850208     C                   MOVEL     'MP'          CINQUE            5            *
018000850208     C                   MOVE      MMPCAU        CINQUE                         *
018100850208     C                   MOVE      CINQUE        TABCOD                         *
018200850208     C     KEYTAB        CHAIN     ANTAB00F                           51        *
018300850208     C  N51              MOVE      MMPCAU        OLDCAU                         *
018400850208     C                   END                                                    *
018500850208     C*---------- CONTROLLO U.MISURA :                     *
018600850208     C  N51UMAKKK        COMP      MMPUMA                             5252      *
018700850208     C*---------- ERRORI ----------------------------------*
018800850208     C   51                                                                     *
018900850208     COR 52              DO                                                     *
019000850208     C     CTRERR        ADD       1             CTRERR                         *
019100850208     C                   Z-ADD     NREL1         NREL              7 0          *
019200850208     C                   MOVE      'A'           MMPATT                         *ANNULLAM.
019300990419     C                   move      xsccut        MMPUTE                         *
019400990419     C                   move      DATMAN        MMPDMN                         *
019500850208     C                   EXSR      RUTSTA                                       *
019600850208     C                   EXCEPT    ANNULL                                       *
019700850208     C                   GOTO      RICMOV                                       *
019800850208     C                   END                                                    *
019900000000     C*---------- IMPOSTO AZIONI CAUSALE ------------------*
020000850613     C                   MOVEA     MMPOPC        AZ                             *
020100000000     C     1             DO        10            X                 3 0          *
020200000000     C     AZ(X)         COMP      '+'                                    41    *   +
020300000000     C     AZ(X)         COMP      '-'                                    42    *   -
020400850208     C   41QTK(X)        ADD       MMPQTA        QTK(X)                         *
020500850208     C   42QTK(X)        SUB       MMPQTA        QTK(X)                         *
020600850208     C                   END                                                    *
020700860129     C     MMPCAU        IFEQ      'GIA'                                        *
020800860129     C                   ADD       MMPQTA        GIAINI           11 2          * GIAC.INIZIALE
020900860129     C                   END                                                    *
021000850208     C                   SETON                                        262728    *
021100850208     C                   SETON                                        2930      *
021200850208     C     CTRMOV        ADD       1             CTRMOV                         *
021300000000     C                   GOTO      RICMOV                                       *
021400000000     C*-----------                                         *
021500000000     C     FINE          TAG                                                    *
021600000103     c   90              movel     'KPJB207'     KMSMN
021700000103     C   90              SETON                                            LR    *
021800000000     C*----------- TOTALI X VERSIONE ----------------------*
021900000000     CL1N90
022000000000     CAN 30              DO                                                     *
022100850208     CL1                 SETON                                            31    *
022200850208     CL1   CTRVER        ADD       1             CTRVER                         *
022300850613     CL1 53              EXSR      AGGSAL                                       *
022400850208     CL1                 SETOFF                                         3031    *
022500000000     CL1                 END                                                    *
022600000000     C*----------- TOTALI X PRODOTTO ----------------------*
022700000000     CL2N90
022800000000     CAN 26              DO                                                     *
022900850208     CL2   CTRMAT        ADD       1             CTRMAT                         *
023000850208     CL2                 SETOFF                                           26    *
023100000000     CL2                 END                                                    *
023200000000     C*----------- TOTALI X CLASSE O S/CLASSE -------------*
023300000000     CL3N90
023400000000     CAN 27
023500000000     CANN04              DO                                                     *
023600850208     CL3   CTRCLS        ADD       1             CTRCLS                         *
023700850208     CL3                 SETOFF                                           27    *
023800000000     CL3                 END                                                    *
023900000000     C*----------- TOTALI X MAGAZZINO ---------------------*
024000000000     CL4N90
024100000000     CAN 28              DO                                                     *
024200850208     CL4                 SETON                                            34    *
024300850208     CL4   CTRCLS        IFGT      CTRGCL                                       *
024400850208     CL4                 Z-ADD     CTRCLS        CTRGCL            5 0          *
024500850208     CL4                 END                                                    *
024600850208     CL4   CTRGMT        ADD       CTRMAT        CTRGMT            5 0          *
024700850208     CL4   CTRGVR        ADD       CTRVER        CTRGVR            7 0          *
024800850208     CL4   CTRGMV        ADD       CTRMOV        CTRGMV            7 0          *
024900850208     CL4   CTRGER        ADD       CTRERR        CTRGER            5 0          *
025000850208     CL4   CTRMAG        ADD       1             CTRMAG                         *
025100850208     CL4                 EXSR      RUTSTA                                       *
025200850208     CL4                 SETOFF                                         2834    *
025300000000     CL4                 END                                                    *
025400000000     C*----------- TOTALI GENERALI ------------------------*
025500000000     CLRN90
025600000000     CAN 29              DO                                                     *
025700850208     CLR                 SETON                                            35    *
025800850208     CLR                 Z-ADD     CTRGCL        CTRCLS                         *
025900850208     CLR                 Z-ADD     CTRGMT        CTRMAT                         *
026000850208     CLR                 Z-ADD     CTRGVR        CTRVER                         *
026100850208     CLR                 Z-ADD     CTRGMV        CTRMOV                         *
026200850208     CLR                 Z-ADD     CTRGER        CTRERR                         *
026300850208     CLR                 EXSR      RUTSTA                                       *
026400850208     CLR                 SETOFF                                           29    *
026500000000     CLR                 END                                                    *
026600850208     C**--------- EMISSIONE STAMPA ----------------------***
026700850208     CSR   RUTSTA        BEGSR                                                  *
026800850208     C     PAGE1         IFGE      55                                           *
026900850208     C                   EXCEPT    TESTA                                        *
027000850208     C                   Z-ADD     0             PAGE1                          *
027100850208     C                   END                                                    *
027200850208     C   51                                                                     *
027300850208     COR 52              EXCEPT    ERROR                                        *
027400850208     C   34                                                                     *
027500850208     COR 35              EXCEPT    TOTALE                                       *
027600850208     CSR                 ENDSR                                                  *
027700850208     C**--------- AGGIORNAMENTO SALDI -------------------***
027800850208     CSR   AGGSAL        BEGSR                                                  *
027900990419     C     KEYSMP        CHAIN     tmSMP01L                           79        *
028000850208     C                   Z-ADD     DATMAN        SMPDMN                         *DATA MANUT.
028100850208     C   79              DO                                                     *
028200850208     C                   MOVE      ' '           SMPATT                         *
028300990419     C                   move      xsccut        SMPUTE                         *
028400850208     C                   Z-ADD     AA            SMPAAC                         *ANNO COMP.
028500850208     C                   MOVE      MAGKKK        SMPMAG                         *MAGAZZINO
028600850208     C                   MOVE      MATKKK        SMPMAT                         *PRODOTTO
028700850208     C                   Z-ADD     0             SMPPLF                         *LIFO FISC.
028800850208     C                   Z-ADD     0             SMPPLV                         *LIFO VAR.
028900850208     C                   Z-ADD     0             SMPQGI                         *GIAC.INIZ.
029000850208     C                   Z-ADD     0             SMPQOR                         *QTA ORDINI
029100850208     C                   Z-ADD     0             SMPQPR                         *QTA PRODUZ.
029200850208     C                   Z-ADD     0             SMPQIM                         *QTA IMPEG.
029300870518     C                   Z-ADD     0             SMPQIP                         *QTA IMPEG.
029400850208     C                   Z-ADD     0             SMPQE1                         *QTA E-1
029500850208     C                   Z-ADD     0             SMPQE2                         *QTA E-2
029600850208     C                   Z-ADD     0             SMPQE3                         *QTA E-3
029700850208     C                   Z-ADD     0             SMPQE4                         *QTA E-4
029800850208     C                   Z-ADD     0             SMPQE5                         *QTA E-5
029900850208     C                   Z-ADD     0             SMPQU1                         *QTA U-1
030000850208     C                   Z-ADD     0             SMPQU2                         *QTA U-2
030100850208     C                   Z-ADD     0             SMPQU3                         *QTA U-3
030200850208     C                   Z-ADD     0             SMPQU4                         *QTA U-4
030300850208     C                   Z-ADD     0             SMPQU5                         *QTA U-5
030400850208     C                   Z-ADD     DATMAN        SMPDMS                         *DATA SALDO
030500850208     C                   Z-ADD     0             SMPNMS                         *MOV. SALDO
030600850208     C                   Z-ADD     0             SMPPMC                         *PRZ. MEDIO
030700850208     C                   MOVE      *BLANKS       SMPFLG                         *FLAGS
030800850208     C                   END                                                    *
030900860129     C                   MOVE      UMAKKK        SMPUMA                         *U.M. ANAGR.
031000860129     C*--- AGGIORNAMENTO ENTRATE/USCITE -------------------*
031100860129     C                   Z-ADD     QTK           QTS                            *
031200860129     C*--- AGGIORNAMENTO GIACENZA FINALE ------------------*
031300850208     C                   Z-ADD     0             SMPQGF                         *GIAC.FINALE
031400850208     C     1             DO        5             X                              *ENTRATE
031500850208     C     SMPQGF        ADD       QTS(X)        SMPQGF                         *
031600850208     C                   END                                                    *
031700850208     C     6             DO        10            X                              *USCITE
031800850208     C     SMPQGF        SUB       QTS(X)        SMPQGF                         *
031900850208     C                   END                                                    *
032000860129     C*--- AGGIORNAMENTO GIACENZA INIZIALE ----------------*
032100860129     C                   Z-ADD     GIAINI        SMPQGI                         *
032200860129     C*--- AGGIORNAMENTO SALDI :                           *
032300990419     C   79              WRITE     tmSMP000                                     *UPDATE
032400990419     C  N79              UPDATE    tmSMP000                                     *UPDATE
032500000000     CSR                 ENDSR                                                  *
032600000000     C***------------------------------------------------***
032700990419     C*----------------------------------------------------*
032800990419     C* Reperimento dati società
032900990419     C*----------------------------------------------------*
033000990419     C     REPSOC        BEGSR
033100990419     C*
033200990419     C                   CALLB     'XSOC'
033300990419     C                   PARM                    TIPXSC            6
033400990419     C                   PARM      socpas        SOCXSC            3
033500990419     C                   PARM                    CDSXSC            9 0
033600990419     C                   PARM                    MODXSC            3
033700990419     C                   PARM      *blanks       RTNXSC            1
033800990419     C                   PARM                    XSOCDS
033900990419     C                   PARM                    KPJBA
034000990419     C*
034100990419     C                   ENDSR
034200850208     O*--------------------------------------------------------------*
034300860129     O*---  ANNULLAMENTO MOVIMENTO :                                 *
034400990419     OtmMMP000  E            ANNULL
034500850208     O                       MMPATT
034600850208     O                       MMPUTE
034700850208     O                       MMPDMN
034800000000     O*--------------------------------------------------------------*
034900000000     O*     TESTATA                                                  *
035000000000     O*--------------------------------------------------------------*
035100000000     OMPPRTSTD  E            TESTA            01
035200000000     O                       SK1                132
035300000000     O                       DESDIT              20
035400000000     O                       UDATE         Y    118
035500000000     O                       PAGE          Z    132
035600000000     O          E            TESTA       1
035700000000     O                       SK2                132
035800990419     O                       AAA930             103
035900000000     O          E            TESTA       1
036000000000     O                       SK3                132
036100000000     O                       KNMUS              132
036200850208     O          E            TESTA       2
036300000000     O                                           41 'Magazzino :'
036400000000     O                       MAGAZZ              49
036500000000     O                       DESMAG              80
036600000000     O               05                          94 'Classe  :'
036700000000     O               05      CLS                 99
036800000000     O               05      DESCLS             125
036900000000     O               06                          94 'S/Classe:'
037000000000     O               06      SCL                 99
037100000000     O               06      DESSCL             125
037200850208     O          E            TESTA       2
037300000000     O                       PAGE1               80
037400850208     O                       SK4                132
037500000000     O*--------------------------------------------------------------*
037600000000     O*     DETTAGLIO   GIACENZA                                     *
037700000000     O*--------------------------------------------------------------*
037800850208     O          E            ERROR       1  1
037900000000     O                       PAGE1                4
038000850208     O               51      ERR(9)              50
038100850208     O               51      MMPCAU              16
038200850208     O               51      MMPOPC              32
038300850208     O               52      ERR(10)             50
038400850208     O               52      MMPUMA              16
038500850208     O               52      UMAKKK              43
038600871019     O                       MATKKK              62
038700850208     O                       MMPNMV        Z     67
038800850208     O                       MMPDMV        Y     76
038900850208     O                       MMPNDC        Z     84
039000850208     O                       MMPDDC        Y     93
039100850208     O                       MMPUMA              98
039200850208     O                       MMPQTA             112 '       0 ,  -'
039300850208     O                       NREL          Z    123
039400850208     O          E    35      TOTALE      2
039500850208     O                       PAGE1                4
039600850208     O                       ERR(11)             50
039700850208     O          E            TOTALE      1
039800000000     O                       PAGE1                4
039900850208     O                       ERR(1)              50
040000850208     O                       CTRERR              41 '   0 '
040100850208     O          E            TOTALE      1
040200850208     O                       PAGE1                4
040300850208     O                       ERR(2)              50
040400850208     O                       CTRMOV        Z     41
040500850208     O          E            TOTALE      1
040600850208     O                       PAGE1                4
040700850208     O                       ERR(3)              50
040800850208     O                       CTRVER        Z     41
040900850208     O          E            TOTALE      1
041000850208     O                       PAGE1                4
041100850208     O                       ERR(4)              50
041200850208     O                       CTRMAT        Z     41
041300850208     O          E   N04      TOTALE      1
041400850208     O                       PAGE1                4
041500850208     O                  05   ERR(5)              50
041600850208     O                  06   ERR(6)              50
041700850208     O                       CTRCLS        Z     41
041800850208     O          E    35      TOTALE      1
041900850208     O                       PAGE1                4
042000850208     O                       ERR(7)              50
042100850208     O                       CTRMAG        Z     41
042200000000     O*--------------------------------------------------------------*
042300850208**    ERR                       ERRORI
042400850208*- TOTALE ERRORI    RISCONTRATI :   XXXXX          ERR,1
042500850208*- TOTALE MOVIMENTI RIGENERATI  : XXXXXXX          ERR,2
042600850208*- TOTALE VERSIONI  RIGENERATE  : XXXXXXX          ERR,3
042700850208*- TOTALE PRODOTTI  RIGENERATI  :   XXXXX          ERR,4
042800850208*- TOTALE CLASSI    RIGENERATE  :   XXXXX          ERR,5
042900850208*- TOTALE S/CLASSI  RIGENERATE  :   XXXXX          ERR,6
043000850208*- TOTALE MAGAZZINI RIGENERATI  :   XXXXX          ERR,7
043100850208*- Cod. DITTA non Valido - Fare inserimento     -* ERR,8
043200850208*- Causale : XXX.xxxxxxxxxxxxxxx.non in Tabella :  ERR,9
043300850208*- U.Misura  XXX diversa da Anagrafica  XXX     :  ERR,10
043400850208*--------- T O T A L E    G E N E R A L E -------* ERR,11
043500000000**   SK1
043600000000*-DESDIT-----------****-------------------------------------------SK01
043700000000-------------------------------------*** il XX/XX/XX      Pag. XXXSK01
043800000000**   SK2
043900850208                    ** Gest.Magazzino -M.P.-  Lista di Controllo  SK02
044000990419  RIGENERAZIONE   SALDI     Anno XX   **        PGM.:   TII212R1  SK02
044100000000**   SK3
044200000000                    ***-------------------------------------------SK03
044300000000-------------------------------------***        Utente  XXXXXXXXXXSK03
044400000000**   SK4
044500850208*------------ E R R O R E ------------------------*-Prodotto-*---MSK04
044600850208ovimento--*---Documento----*-UM-*--Quantità--*-Num.Record-*       SK04
