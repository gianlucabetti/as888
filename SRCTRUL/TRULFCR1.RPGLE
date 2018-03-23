000100990908     H dftactgrp(*yes)
000200990908
000300030403     D*------------
000400030403     D* DS REPERIMENTO ATTRIBUTI FILE DI SPOOL
000500030403     D*------------
000600030403     D FIELD           DS           128
000700030403     D  FJOB                  13     22
000800030403     D  FUSR                  23     32
000900030403     D  FJOBN                 33     38
001000030403     D  FSPL                  39     48
001100030403     D  FSPLN                 49     52B 0
001200030403     C*
001300030403     D                 DS
001400030403     D  RCVLEN                 9     12B 0
001500030403     D*
001600030403     D JOBINF          DS
001700030403     D    JOB                        10
001800030403     D    USER                       10
001900030403     D    JOBN                        6
002000030403     D*-----------------------------------------
002100030403     D* DS  PSSPLA0100 DI QUSRTOOL/QATTRPG
002200030403     D*-----------------------------------------
002300030403     D RCVAR1          DS
002400030403     D  BYTRTN                 1      4B 0
002500030403     D  BYTVAL                 5      8B 0
002600030403     D  IJOBID                 9     24
002700030403     D  ISPLID                25     40
002800030403     D  JOBNAM                41     50
002900030403     D  USRNAM                51     60
003000030403     D  JOBNUM                61     66
003100030403     D  FILNAM                67     76
003200030403     D  FILNUM                77     80B 0
003300030403     D  FRMTYP                81     90
003400030403     D  USRDTA                91    100
003500030403     D  STATUS               101    110
003600030403     D  FILAVL               111    120
003700030403     D  HLDF                 121    130
003800030403     D  SAVF                 131    140
003900030403     D  TOTPAG               141    144B 0
004000030403     D  PAGWRT               145    148B 0
004100030403     D  STRPAG               149    152B 0
004200030403     D  ENDPAG               153    156B 0
004300030403     D  LASPAG               157    160B 0
004400030403     D  RESPRT               161    164B 0
004500030403     D  TOTCPY               165    168B 0
004600030403     D  CPYLFT               169    172B 0
004700030403     D  LPI                  173    176B 0
004800030403     D  CPI                  177    180B 0
004900030403     D  OUTPRI               181    182
005000030403     D  OUTQNM               183    192
005100030403     D  OUTQLB               193    202
005200030403     D  DATFOP               203    209
005300030403     D  TIMFOP               210    215
005400030403     D  DEVFNA               216    225
005500030403     D  DEVFLB               226    235
005600030403     D  PGMOPF               236    245
005700030403     D  PGMOPL               246    255
005800030403     D  ACCCOD               256    270
005900030403     D  PRTTXT               271    300
006000030403     D  RCDLEN               301    304B 0
006100030403     D  MAXRCD               305    308B 0
006200030403     D  DEVCLS               309    318
006300030403     D  PRTTYP               319    328
006400030403     D  DOCNAM               329    340
006500030403     D  FLDNAM               341    404
006600030403     D  S36PRC               405    412
006700030403     D  PRTFID               413    422
006800030403     D  RPLUN                423    423
006900030403     D  RPLCHR               424    424
007000030403     D  PAGLEN               425    428B 0
007100030403     D  PAGWID               429    432B 0
007200030403     D  NUMSEP               433    436B 0
007300030403     D  OVRLIN               437    440B 0
007400030403     D  DBCSDA               441    450
007500030403     D  DBCSEC               451    460
007600030403     D  DBCSSO               461    470
007700030403     D  DBCSCR               471    480
007800030403     D  DBCSCI               481    484B 0
007900030403     D  GRAPHI               485    494
008000030403     D  CODPAG               495    504
008100030403     D  FORNAM               505    514
008200030403     D  FORLIB               515    524
008300030403     D  SRCDRW               525    528B 0
008400030403     D  PRTFON               529    538
008500030403     D  S36SPL               539    544
008600030403     D  PAGROT               545    548B 0
008700030403     D  JUSTIF               549    552B 0
008800030403     D  PRTBOT               553    562
008900030403     D  FLDRCD               563    572
009000030403     D  CTLCHR               573    582
009100030403     D  ALGFRM               583    592
009200030403     D  PRTQUA               593    602
009300030403     D  FRMFED               603    612
009400030403     D  VOLUME               613    683
009500030403     D  FLABID               684    700
009600030403     D  EXCTYP               701    710
009700030403     D  CHRCOD               711    720
009800030403     D  TOTRCD               721    724B 0
009900030403     D  USERTEXT            1157   1411
010000030403     D*------------
010100030403     D* DS DI RPOCEDURA
010200030403     D*------------
010300030403     D  TRULFCDS     E DS
010400030403     D*------------
010500030403
010600030403      /COPY QSYSINC/QRPGLESRC,QUSEC
010700030403
010800030403     C*------------
010900030403     C* MAIN
011000030403     C*------------
011100000616     C*
011200030403     C     *ENTRY        PLIST
011300030403     C                   PARM                    TRULFCDS
011400030404     C                   PARM                    SPOOLLEN          5
011500030403     C*
011600030403     C* Reperisce attributi del file di spool
011700030403     C                   MOVEL     DFCJNAM       JOB
011800030403     C                   MOVEL     DFCUNAM       USER
011900030403     C                   MOVEL     DFCJNUM       JOBN
012000030403     C                   MOVEL     DFCFNAM       FSPL
012100030514     C*
012200030514     C* Effettup particolari considerazioni sul numero dello spool x gestire anche *ONLY e *LAST
012300030514     C                   IF        %trim(DFCFNUM) = '*ONLY'
012400030514     C                   EVAL      FSPLN = 0
012500030514     C                   ELSE
012600030514     C                   IF        %trim(DFCFNUM) = '*LAST'
012700030514     C                   EVAL      FSPLN = -1
012800030514     C                   ELSE
012900030403     C                   MOVE(P)   DFCFNUM       FSPLN
013000030514     C                   ENDIF
013100030514     C                   ENDIF
013200030403     C*
013300030403     C                   CLEAR                   QUSEC
013400030403     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
013500030403     C*
013600030403     C                   CALL      'QUSRSPLA'                           21
013700030403     C                   PARM                    RCVAR1
013800030403     C                   PARM      1500          RCVLEN
013900030403     C                   PARM      'SPLA0100'    FMTNM1            8
014000030403     C                   PARM                    JOBINF
014100030403     C                   PARM                    JOBid            16
014200030403     C                   PARM                    SPLFID           16
014300030403     C                   PARM                    FSPL
014400030403     C                   PARM                    FSPLN
014500030403     C                   PARM                    QUSEC
014600000613     C*
014700030404     C* Valorizzo il parametro di ritorno con la lunghezza del file di spool corrente
014800030404     C                   Z-ADD     PAGWID        SPOOLLEN_n        5 0
014900030404     C* Gestisco il byte in più x l'inserimento dei caratteri di controllo *FCFC
015000030404     C                   ADD       1             SPOOLLEN_n
015100030404     C                   MOVE(P)   SPOOLLEN_n    SPOOLLEN
015200030404     C*
015300030403     C                   SETON                                        LR
