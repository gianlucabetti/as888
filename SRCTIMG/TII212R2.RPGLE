000100990419     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990419     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300850614     F* MPH932R2 *-MP/38-*-------------------------------------------*
000400000000     F*------------------*                                           *
000500850614     F*       - ESEGUE ELIMINAZIONE  SALDI MAGAZZ. -MP-              *
000600000000     F*       -----------------------------------------              *
000700850614     F*                                                   AZN.: H931 *
000800850614     F*         FILE : MPPRTSTD-MPPIL00F-MPMMP01L-MPSMP01L           *
000900000000     F*                                                              *
001000000000     F*         PGM. : RIC001R   - RICERCA INTESTAZIONE DITTA        *
001100000000     F*--------------------------------------------------------------*
001200000000     F*     IMPOSTAZIONE CHIAVI PILOTA :                             *
001300000000     F*             PILK01 = MAGAZZINO                  'XXXXXXX   ' *
001400850614     F*             PILK02 = CLASSE O S/CLASSE          'XXX.X     ' *
001500000000     F*             PILK03 = CODICE PRODOTTO            'XXXXX     ' *
001600000000     F*             PILK04 = VERSIONE                   'XX        ' *
001700000000     F*             PILK05 = BLANKS                     '          ' *
001800000000     F*             PILFIL = DESCRIZIONE PRODOTTO E UMA 'XXXX.. XXX' *
001900000000     F*--------------------------------------------------------------*
002000990419     FtmPIL00F  IP   E           K DISK
002100990419     FtmMMP01L  IF   E           K DISK
002200990419     FtmSMP01L  UF   E           K DISK
002300850614     FMPPRTSTD  O    F  132        PRINTER OFLIND(*INOF)
002400000000     D ERR             S             50    DIM(3) CTDATA PERRCD(1)              *ERRORI
002500000000     D SK1             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 1
002600000000     D SK2             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 2
002700000000     D SK3             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 3
002800000000     D SK4             S             66    DIM(1) CTDATA PERRCD(1)              *TESTATA 3
002900990419     D RECSAL        E DS                  EXTNAME(tmSMP00F)
003000000000     D*--------------------- ARCHIVIO SALDI :
003100990419     D  QTS                   56    145P 2
003200000000     D                                     DIM(15)                              *Q.TA'  SALDI
003300000000     D KPJBA         E DS
003400000000     D*----- PASSAGGIO PARAMETRI
003500990419     D  AAA930               247    250  0
003600990419     D  MAG930               251    257
003700990419     D  MAT930               276    287
003800990419     D  CLS930               267    269
003900990419     D  SCL930               270    274
004000990419     D  TIP930               275    275
004100000103     D  socpas               288    290
004200990419     D  MSGERR               453    502
004300990419     D soc001        E DS                  EXTNAME(xsoc001ds)
004400990419     D xsocds          DS          1000
004500990419     ItmPIL000      01
004600000000     I                                          PILK01        L2
004700000000     I                                          PILK03        L1
004800000000     C*----------------------------------------------------*
004900000000     C     *ENTRY        PLIST                                                  *
005000000000     C                   PARM                    KPJBA                          *
005100850614     C*                                                    *
005200850614     C     KEYSMP        KLIST                                                  *
005300990419     C                   KFLD                    AA                4 0          *
005400000000     C                   KFLD                    MAGAZZ            7            *
005500871019     C                   KFLD                    PRODOT           12            *
005600000000     C*---------- MOVIMENTI DI MAGAZZINO :                 *
005700850614     C     KEYMMP        KLIST                                                  *
005800850614     C                   KFLD                    SMPAAC                         *
005900850614     C                   KFLD                    SMPMAG                         *
006000850614     C                   KFLD                    SMPMAT                         *
006100000000     C*---------- ROTTURA DI MAGAZZINO --------------------*
006200000000     C   L2
006300000000     CANN02              DO                                                     *
006400990419     C                   MOVEL     'SOC001'      TIPXSC
006500990419     C                   MOVEL     *blank        SOCXSC
006600990419     C                   EXSR      REPSOC
006700990419     C     RTNXSC        IFNE      '1'
006800990419     C                   MOVEL     XSOCDS        SOC001
006900990419     C                   MOVEL     xscsoc        socpas
007000990419     C                   MOVEL     xscrgs        desdit           20
007100990419     c                   else
007200990419     c                   seton                                        90
007300990419     c   90              goto      fine
007400990419     c                   end
007500000000     C                   MOVEL     AAA930        AA                             *
007600000000     C                   SETON                                            02    *
007700000000     C                   Z-ADD     0             PAGE              3 0          *
007800000000     C                   END                                                    *
007900000000     C*----------                                          *
008000000000     C   L2              MOVEL     PILK01        MAGAZZ                         *
008100000000     C   L2              Z-ADD     50            PAGE1             3 0          *
008200000000     C*---------- ROTTURA DI PRODOTTO ---------------------*
008300000000     C   L1              MOVEL     PILK03        PRODOT                         *
008400000000     C*---------- CALCOLI PER VERSIONE --------------------*
008500000000     C                   SETOFF                                       919293    *
008600990419     C     KEYSMP        SETLL     tmSMP01L                           91        *
008700000000     C   91              GOTO      FINE                                         *
008800000000     C*---------- LETTURA DEI SALDI -----------------------*
008900000000     C     RICSAL        TAG                                                    *
009000990419     C     KEYSMP        READE     tmSMP01L                               92    *
009100000000     C   92              GOTO      FINE                                         *
009200000000     C*---------- RICERCA  MOVIMENTI :                     *
009300000000     C                   SETOFF                                       9394      *
009400990419     C     KEYMMP        SETLL     tmMMP01L                           93        *
009500000000     C   93              EXSR      CTLSAL                                       *
009600990419     C     KEYMMP        READE     tmMMP01L                               94    *
009700000000     C   94              EXSR      CTLSAL                                       *
009800000000     C                   GOTO      RICSAL                                       *
009900000000     C*-----------                                         *
010000000000     C     FINE          TAG                                                    *
010100000103     c   90              movel     'KPJB207'     KMSMN
010200000103     C   90              SETON                                            LR    *
010300000000     C*----------- EMISSIONE LISTA ANNULLAMENTO -----------*
010400000000     CL2N90              SETON                                            30    *
010500000000     CL2N90              EXSR      RUTSTA                                       *
010600000000     CL2N90              SETOFF                                           30    *
010700000000     CLRN90              EXCEPT    TOTGEN                                       *
010800990419     C**-------------------------------------------------***
010900000000     C**--------- CONTROLLO SALDI -----------------------***
011000990419     C**-------------------------------------------------***
011100000000     CSR   CTLSAL        BEGSR                                                  *
011200000000     C                   SETOFF                                           95    *
011300850614     C                   MOVE      ' '           SMPATT                         *
011400000000     C     5             DO        15            X                 3 0          *
011500000000     C                   Z-ADD     0             QTS(X)                         *
011600000000     C                   END                                                    *
011700000000     C     1             DO        4             X                              *
011800000000     C     QTS(X)        COMP      0                                      95    *
011900000000     C   95              END                                                    *
012000850614     C   95              MOVE      'A'           SMPATT                         *ANNULLAM.
012100000000     C   95CTRMOV        ADD       1             CTRMOV            7 0          *
012200000000     C   95CTRGEN        ADD       1             CTRGEN            7 0          *
012300990419     C   95              move      xsccut        SMPUTE                         *
012400850624     C   95              EXCEPT    AGGSAL                                       *
012500850624     C   95              EXSR      RUTSTA                                       *
012600000000     C                   GOTO      RICSAL                                       *
012700000000     CSR                 ENDSR                                                  *
012800000000     C***------------------------------------------------***
012900000000     C**--------- EMISSIONE STAMPA ----------------------***
013000000000     C***------------------------------------------------***
013100000000     CSR   RUTSTA        BEGSR                                                  *
013200000000     C     PAGE1         IFGE      50                                           *
013300000000     C                   EXCEPT    TESTA                                        *
013400000000     C                   Z-ADD     0             PAGE1                          *
013500000000     C                   END                                                    *
013600000000     C  N30              EXCEPT    DETAIL                                       *
013700000000     C   30              EXCEPT    TOTMAG                                       *
013800000000     CSR                 ENDSR                                                  *
013900990419     C*----------------------------------------------------*
014000990419     C* Reperimento dati società
014100990419     C*----------------------------------------------------*
014200990419     C     REPSOC        BEGSR
014300990419     C*
014400990419     C                   CALLB     'XSOC'
014500990419     C                   PARM                    TIPXSC            6
014600990419     C                   PARM      socpas        SOCXSC            3
014700990419     C                   PARM                    CDSXSC            9 0
014800990419     C                   PARM                    MODXSC            3
014900990419     C                   PARM      *blanks       RTNXSC            1
015000990419     C                   PARM                    XSOCDS
015100990419     C                   PARM                    KPJBA
015200990419     C*
015300990419     C                   ENDSR
015400000000     O*-----AGGIORNAMENTO SALDO -------------------------------------*
015500990419     OtmSMP000  E            AGGSAL
015600850614     O                       SMPATT
015700850614     O                       SMPUTE
015800850614     O                       SMPQGI
015900850614     O                       SMPQOR
016000850614     O                       SMPQPR
016100850614     O                       SMPQIM
016200870518     O                       SMPQIP
016300850614     O                       SMPQGF
016400850614     O                       SMPQE1
016500850614     O                       SMPQE2
016600850614     O                       SMPQE3
016700850614     O                       SMPQE4
016800850614     O                       SMPQE5
016900850614     O                       SMPQU1
017000850614     O                       SMPQU2
017100850614     O                       SMPQU3
017200850614     O                       SMPQU4
017300850614     O                       SMPQU5
017400850614     OMPPRTSTD  E            TESTA            01
017500000000     O*--------------------------------------------------------------*
017600000000     O*     TESTATA                                                  *
017700000000     O*--------------------------------------------------------------*
017800000000     O                       SK1                132
017900000000     O                       DESDIT              20
018000000000     O                       UDATE         Y    118
018100000000     O                       PAGE          Z    132
018200000000     O          E            TESTA       1
018300000000     O                       SK2                132
018400990419     O                       AAA930             103
018500000000     O          E            TESTA       1
018600000000     O                       SK3                132
018700000000     O                       KNMUS              132
018800000000     O          E            TESTA       2
018900000000     O                       SK4                 85
019000000000     O          E            DETAIL      1
019100850624     O                       PAGE1               49
019200000000     O                       MAGAZZ              49
019300870108     O                       PRODOT              64
019400000000     O          E            TOTMAG      2
019500000000     O                       ERR(2)              89
019600000000     O                       MAGAZZ              59
019700000000     O                       CTRMOV        ZB    85
019800000000     O          E            TOTGEN      2
019900000000     O                       ERR(3)              89
020000000000     O                       CTRGEN        Z     85
020100000000     O*--------------------------------------------------------------*
020200850614**    ERR                       ERRORI
020300000000*- Cod. DITTA non Valido - Fare inserimento     -* ERR,1
020400000000*- MAGAZZINO XXXXXXX SALDI ANNULLATI :
020500000000*- TOTALE .......... SALDI ANNULLATI :
020600000000**   SK1
020700000000*-DESDIT-----------****-------------------------------------------SK01
020800000000-------------------------------------*** il XX/XX/XX      Pag. XXXSK01
020900000000**   SK2
021000850614                    ** Gest.Magazzino -M.P.-  Lista di Controllo  SK02
021100850614  ELIMINAZIONE SALDI        Anno XX   **        PGM.:   MPH932R2  SK02
021200000000**   SK3
021300000000                    ***-------------------------------------------SK03
021400000000-------------------------------------***        Utente  XXXXXXXXXXSK03
021500000000**   SK4
021600850614                    *-Magazzino-*---Prodotto---*                  SK04
