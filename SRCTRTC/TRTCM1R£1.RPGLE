000100030401     H*--------------------------------------------------------------------------------------------*
000200040930     H* Polling su OUTQ di "IN" x mailing automatico spool.
000300030401     H*--------------------------------------------------------------------------------------------*
000400030401     H DECEDIT('0,') DATEDIT(*DMY.)
000500030401     H*--------------------------------------------------------------------------------------------*
000600030402     F*
000700990513     D FIELD           DS           128
000800990513     D  FJOB                  13     22
000900990513     D  FUSR                  23     32
001000990513     D  FJOBN                 33     38
001100990513     D  FSPL                  39     48
001200990513     D  FSPLN                 49     52B 0
001300001006     D*
001400990513     D RCVVAR          DS
001500990513     D  OFFSET                 1      4B 0
001600990513     D  NOENTR                 9     12B 0
001700990513     D  LSTSIZ                13     16B 0
001800990513     D                 DS
001900990513     D  RCVLEN                 9     12B 0
002000990513     D*-----------------------------------------
002100990513     D* DS  PSSPLA0100 DI QUSRTOOL/QATTRPG
002200990513     D*-----------------------------------------
002300990513     D RCVAR1          DS
002400990513     D  BYTRTN                 1      4B 0
002500990513     D  BYTVAL                 5      8B 0
002600990513     D  IJOBID                 9     24
002700990513     D  ISPLID                25     40
002800990513     D  JOBNAM                41     50
002900990513     D  USRNAM                51     60
003000990513     D  JOBNUM                61     66
003100990513     D  FILNAM                67     76
003200990513     D  FILNUM                77     80B 0
003300990513     D  FRMTYP                81     90
003400990513     D  USRDTA                91    100
003500990513     D  STATUS               101    110
003600990513     D  FILAVL               111    120
003700990513     D  HLDF                 121    130
003800990513     D  SAVF                 131    140
003900990513     D  TOTPAG               141    144B 0
004000990513     D  PAGWRT               145    148B 0
004100990513     D  STRPAG               149    152B 0
004200990513     D  ENDPAG               153    156B 0
004300990513     D  LASPAG               157    160B 0
004400990513     D  RESPRT               161    164B 0
004500990513     D  TOTCPY               165    168B 0
004600990513     D  CPYLFT               169    172B 0
004700990513     D  LPI                  173    176B 0
004800990513     D  CPI                  177    180B 0
004900990513     D  OUTPRI               181    182
005000990513     D  OUTQNM               183    192
005100990513     D  OUTQLB               193    202
005200990513     D  DATFOP               203    209
005300990513     D  TIMFOP               210    215
005400990513     D  DEVFNA               216    225
005500990513     D  DEVFLB               226    235
005600990513     D  PGMOPF               236    245
005700990513     D  PGMOPL               246    255
005800990513     D  ACCCOD               256    270
005900990513     D  PRTTXT               271    300
006000990513     D  RCDLEN               301    304B 0
006100990513     D  MAXRCD               305    308B 0
006200990513     D  DEVCLS               309    318
006300990513     D  PRTTYP               319    328
006400990513     D  DOCNAM               329    340
006500990513     D  FLDNAM               341    404
006600990513     D  S36PRC               405    412
006700990513     D  PRTFID               413    422
006800990513     D  RPLUN                423    423
006900990513     D  RPLCHR               424    424
007000990513     D  PAGLEN               425    428B 0
007100990513     D  PAGWID               429    432B 0
007200990513     D  NUMSEP               433    436B 0
007300990513     D  OVRLIN               437    440B 0
007400990513     D  DBCSDA               441    450
007500990513     D  DBCSEC               451    460
007600990513     D  DBCSSO               461    470
007700990513     D  DBCSCR               471    480
007800990513     D  DBCSCI               481    484B 0
007900990513     D  GRAPHI               485    494
008000990513     D  CODPAG               495    504
008100990513     D  FORNAM               505    514
008200990513     D  FORLIB               515    524
008300990513     D  SRCDRW               525    528B 0
008400990513     D  PRTFON               529    538
008500990513     D  S36SPL               539    544
008600990513     D  PAGROT               545    548B 0
008700990513     D  JUSTIF               549    552B 0
008800990513     D  PRTBOT               553    562
008900990513     D  FLDRCD               563    572
009000990513     D  CTLCHR               573    582
009100990513     D  ALGFRM               583    592
009200990513     D  PRTQUA               593    602
009300990513     D  FRMFED               603    612
009400990513     D  VOLUME               613    683
009500990513     D  FLABID               684    700
009600990513     D  EXCTYP               701    710
009700990513     D  CHRCOD               711    720
009800990513     D  TOTRCD               721    724B 0
009900041001     D  USERTEXT            1017   1116
010000041001     D  USEREXTD            1157   1411
010100001006     D*---
010200001006     D* DS per gestione dati di servizio sulla stampa
010300001006     D*---
010400030401     D trtcm1ds      e ds
010500030404     D*---
010600030404     D* DS per chiamata pgm utilità x passaggio da spool a file *FCFC "normalizzato"
010700030404     D*---
010800030404     D trulfcds      e ds
010900001006     D*---
011000001006     D* Variabili di wrk
011100001006     D*---
011200001006     D*
011300990513     D jobinf          ds
011400990513     D    job                        10
011500990513     D    user                       10
011600990513     D    jobn                        6
011700030401     D*---
011800030401     D* Costanti
011900030401     D*---
012000040930     D* La costante "dtaqlib" è valorizzata espressamente in GAITRAOBJ e nn *LIBL xchè
012100040930     D* l'API che ne reperisce da DTAQ relativa in questo modo è + veloce.
012200040930     D dtaqindft       c                   const('EMAILIN   ')
012300040930     D dtaqoutdft      c                   const('EMAILOUT  ')
012400040930     D dtaqlib         c                   const('GAITRAOBJ ')
012500040930     C*
012600040930     C* Ricevimento parametri
012700040930     C     *ENTRY        PLIST
012800040930     C                   PARM                    p_dtaqIN         10
012900040930     C                   PARM                    p_dtaqOUT        10
013000040930     C*
013100040930     C* Gestisco il ricevimento parametri
013200040930     C                   IF        %parms <> 2
013300040930     C                   MOVEL(P)  dtaqindft     dtaqIN           10
013400040930     C                   MOVEL(P)  dtaqoutdft    dtaqOUT          10
013500040930     C                   ELSE
013600040930     C                   MOVEL     p_dtaqIN      dtaqIN
013700040930     C                   MOVEL     p_dtaqOUT     dtaqOUT
013800040930     C                   ENDIF
013900040930     C*
014000040930     C* Verifico correttezza parametri
014100040930     C                   IF        dtaqIN = *blanks
014200040930     C                   MOVEL(P)  dtaqindft     dtaqIN
014300040930     C                   ENDIF
014400040930     C                   IF        dtaqOUT = *blanks
014500040930     C                   MOVEL(P)  dtaqoutdft    dtaqOUT
014600040930     C                   ENDIF
014700990513     C*
014800040930     C* Sta in ascolto sulla DTAQ "IN" abbinata alla OUTQ "IN"
014900990513     C                   DO        *HIVAL
015000990513     C                   CALL      'QRCVDTAQ'                           21
015100040930     C                   PARM      DTAQIN        QNAME            10
015200030401     C                   PARM      DTAQLIB       QLIB             10
015300990513     C                   PARM      128           FLDLEN            5 0
015400990513     C                   PARM                    FIELD           128
015500990513     C                   PARM      -1            WAIT              5 0
015600990513     C*
015700990513     C                   MOVEL     FJOB          JOB
015800990513     C                   MOVEL     FUSR          USER
015900990513     C                   MOVEL     FJOBN         JOBN
016000990513     C*
016100990513     C* Reperisce attributi del file
016200990513     C                   CALL      'QUSRSPLA'                           21
016300990513     C                   PARM                    RCVAR1
016400001006     C                   PARM      1500          RCVLEN
016500990513     C                   PARM      'SPLA0100'    FMTNM1            8
016600990513     C                   PARM                    JOBINF
016700001006     C                   PARM                    JOBid            16
016800990513     C                   PARM                    SPLFID           16
016900990513     C                   PARM                    FSPL
017000990513     C                   PARM                    FSPLN
017100001006     C*
017200030401     C* Lancia il CL di conversioine spool in file *FCFC nell IFS
017300030401     C                   MOVEL(p)  *ALL'0'       NUMFIL            6
017400030401     C                   MOVE      FILNUM        NUMFIL
017500041001     C                   MOVEL     USEREXTD      TRTCM1DS
017600030404     C* Valorizzo la DS pgm utilità TRULFCC
017700030404     C                   CLEAR                   TRULFCDS
017800030404     C                   EVAL      DFCFNAM  = FILNAM
017900030404     C                   EVAL      DFCJNAM  = JOBNAM
018000030404     C                   EVAL      DFCUNAM  = USRNAM
018100030404     C                   EVAL      DFCJNUM  = JOBNUM
018200030404     C                   EVAL      DFCFNUM  = NUMFIL
018300030404     C                   EVAL      DFCFOUT  = 'TRTCMWWF'
018400040907     C                   EVAL      DFCLOUT  = 'QTEMP'
018500030514     C                   EVAL      DFCFOPT  = 'R'
018600030404     C*
018700030401     C                   CALL(e)   'TRTCM1C'
018800030401     C                   PARM                    FILNAM
018900030401     C                   PARM                    JOBNAM
019000030401     C                   PARM                    USRNAM
019100030401     C                   PARM                    JOBNUM
019200030401     C                   PARM                    NUMFIL
019300030402     C                   PARM      *blanks       MESSAGGIO        80
019400030401     C                   PARM      '0'           ESITO             1
019500030404     C                   PARM                    TRULFCDS
019600040930     C                   PARM                    dtaqIN
019700040930     C                   PARM                    dtaqOUT
019800060705     C                   PARM                    PRTTYP
019900060721     C                   PARM                    §CM1PCL
020000031210     C*
020100031210     C* Se errore nel CL d invio file nell'IFS => imposto a stato HOLD il record del log del mailing
020200031210     C                   IF        ESITO = '2'
020300031210     C                   EVAL      §CM1STS = 'H'
020400031210     C                   ENDIF
020500030401     C*
020600030402     C* Inserisco il record nel file di log Mailing Automatico
020700031211     C                   MOVE(P)   TOTPAG        WTPAG             9 0
020800031211     C                   CALL      'TRTCM1R1'
020900030401     C                   PARM                    FILNAM
021000030401     C                   PARM                    JOBNAM
021100030401     C                   PARM                    USRNAM
021200030401     C                   PARM                    JOBNUM
021300030401     C                   PARM                    NUMFIL
021400030604     C                   PARM                    USRDTA
021500031211     C                   PARM                    WTPAG
021600040615     C                   PARM      PAGWID        PRTFLEN          10 0
021700040722     C                   PARM      CPI           SPLCPI           10 0
021800030402     C                   PARM                    TRTCM1DS
021900060705     C                   PARM                    PRTTYP
022000030402     C                   PARM                    MESSAGGIO
022100030402     C                   PARM                    ESITO
022200001006     C*
022300030401     C                   ENDDO
022400001006     C*
022500030401     C                   RETURN
