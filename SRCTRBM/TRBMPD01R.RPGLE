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
010100080526     D  JOBSYSN             1513   1520
010200001006     D*---
010300001006     D* DS per gestione dati di servizio sulla stampa
010400001006     D*---
010500091211     D TRBMM1DS      e ds
010600030404     D*---
010700030404     D* DS per chiamata pgm utilità x passaggio da spool a file *FCFC "normalizzato"
010800030404     D*---
010900091211     D TRBMFCDS      e ds
011000001006     D*---
011100001006     D* Variabili di wrk
011200001006     D*---
011300001006     D*
011400990513     D jobinf          ds
011500990513     D    job                        10
011600990513     D    user                       10
011700990513     D    jobn                        6
011800030401     D*---
011900030401     D* Costanti
012000030401     D*---
012100040930     D* La costante "dtaqlib" è valorizzata espressamente in GAITRAOBJ e nn *LIBL xchè
012200040930     D* l'API che ne reperisce da DTAQ relativa in questo modo è + veloce.
012300040930     D dtaqindft       c                   const('EMAILIN   ')
012400040930     D dtaqoutdft      c                   const('EMAILOUT  ')
012500040930     D dtaqlib         c                   const('GAITRAOBJ ')
012600040930     C*
012700040930     C* Ricevimento parametri
012800040930     C     *ENTRY        PLIST
012900040930     C                   PARM                    p_dtaqIN         10
013000040930     C                   PARM                    p_dtaqOUT        10
013100080311     C                   PARM                    p_idWSCST        10
013200040930     C*
013300040930     C* Gestisco il ricevimento parametri
013400080312     C                   IF        %parms <> 3
013500040930     C                   MOVEL(P)  dtaqindft     dtaqIN           10
013600040930     C                   MOVEL(P)  dtaqoutdft    dtaqOUT          10
013700040930     C                   ELSE
013800040930     C                   MOVEL     p_dtaqIN      dtaqIN
013900040930     C                   MOVEL     p_dtaqOUT     dtaqOUT
014000040930     C                   ENDIF
014100040930     C*
014200040930     C* Verifico correttezza parametri
014300040930     C                   IF        dtaqIN = *blanks
014400040930     C                   MOVEL(P)  dtaqindft     dtaqIN
014500040930     C                   ENDIF
014600040930     C                   IF        dtaqOUT = *blanks
014700040930     C                   MOVEL(P)  dtaqoutdft    dtaqOUT
014800040930     C                   ENDIF
014900990513     C*
015000040930     C* Sta in ascolto sulla DTAQ "IN" abbinata alla OUTQ "IN"
015100990513     C                   DO        *HIVAL
015200990513     C                   CALL      'QRCVDTAQ'                           21
015300040930     C                   PARM      DTAQIN        QNAME            10
015400030401     C                   PARM      DTAQLIB       QLIB             10
015500990513     C                   PARM      128           FLDLEN            5 0
015600990513     C                   PARM                    FIELD           128
015700990513     C                   PARM      -1            WAIT              5 0
015800990513     C*
015900990513     C                   MOVEL     FJOB          JOB
016000990513     C                   MOVEL     FUSR          USER
016100990513     C                   MOVEL     FJOBN         JOBN
016200990513     C*
016300990513     C* Reperisce attributi del file
016400080526     C                   EVAL      RCVLEN = %size(RCVAR1)
016500990513     C                   CALL      'QUSRSPLA'                           21
016600990513     C                   PARM                    RCVAR1
016700080526     C                   PARM                    RCVLEN
016800990513     C                   PARM      'SPLA0100'    FMTNM1            8
016900990513     C                   PARM                    JOBINF
017000001006     C                   PARM                    JOBid            16
017100990513     C                   PARM                    SPLFID           16
017200990513     C                   PARM                    FSPL
017300990513     C                   PARM                    FSPLN
017400080526     C*
017500080526     C* Elaboro solo gli spool in *OUTQ EMAILIN*
017600080526     C                   IF        %scan('EMAILIN':OUTQNM) > *zeros AND
017700080526     C                             %trim(OUTQLB) = 'GAITRAOBJ'
017800001006     C*
017900030401     C* Lancia il CL di conversioine spool in file *FCFC nell IFS
018000030401     C                   MOVEL(p)  *ALL'0'       NUMFIL            6
018100030401     C                   MOVE      FILNUM        NUMFIL
018200091211     C                   MOVEL     USEREXTD      TRBMM1DS
018300091211     C*
018400091211     C* Forzature specifiche sui dati utente estesi
018500091211     C                   EVAL      §CM1TIPS   = 'PDF'
018600091211     C                   EVAL      §CM1PO     = '046'
018700091211     C                   EVAL      §CM1IDP    = '3'
018800091211     C                   EVAL      §CM1PCL    = 'S'
018900091211     C*
019000091211     C* Valorizzo la DS pgm utilità TRBMFCC
019100091211     C                   CLEAR                   TRBMFCDS
019200030404     C                   EVAL      DFCFNAM  = FILNAM
019300030404     C                   EVAL      DFCJNAM  = JOBNAM
019400030404     C                   EVAL      DFCUNAM  = USRNAM
019500030404     C                   EVAL      DFCJNUM  = JOBNUM
019600030404     C                   EVAL      DFCFNUM  = NUMFIL
019700091211     C                   EVAL      DFCFOUT  = 'TRBMUWWF'
019800040907     C                   EVAL      DFCLOUT  = 'QTEMP'
019900030514     C                   EVAL      DFCFOPT  = 'R'
020000030404     C*
020100091211     C                   CALL(e)   'TRBMPD02C'
020200030401     C                   PARM                    FILNAM
020300030401     C                   PARM                    JOBNAM
020400030401     C                   PARM                    USRNAM
020500030401     C                   PARM                    JOBNUM
020600030401     C                   PARM                    NUMFIL
020700030402     C                   PARM      *blanks       MESSAGGIO        80
020800030401     C                   PARM      '0'           ESITO             1
020900091211     C                   PARM                    TRBMFCDS
021000040930     C                   PARM                    dtaqIN
021100040930     C                   PARM                    dtaqOUT
021200060705     C                   PARM                    PRTTYP
021300060721     C                   PARM                    §CM1PCL
021400080311     C                   PARM                    p_idWSCST
021500031210     C*
021600031210     C* Se errore nel CL d invio file nell'IFS => imposto a stato HOLD il record del log del mailing
021700031210     C                   IF        ESITO = '2'
021800031210     C                   EVAL      §CM1STS = 'H'
021900031210     C                   ENDIF
022000030401     C*
022100030402     C* Inserisco il record nel file di log Mailing Automatico
022200031211     C                   MOVE(P)   TOTPAG        WTPAG             9 0
022300091211     C                   CALL      'TRBMPD02R'
022400030401     C                   PARM                    FILNAM
022500030401     C                   PARM                    JOBNAM
022600030401     C                   PARM                    USRNAM
022700030401     C                   PARM                    JOBNUM
022800030401     C                   PARM                    NUMFIL
022900080526     C                   PARM                    JOBSYSN
023000030604     C                   PARM                    USRDTA
023100031211     C                   PARM                    WTPAG
023200040615     C                   PARM      PAGWID        PRTFLEN          10 0
023300040722     C                   PARM      CPI           SPLCPI           10 0
023400091211     C                   PARM                    TRBMM1DS
023500060705     C                   PARM                    PRTTYP
023600030402     C                   PARM                    MESSAGGIO
023700030402     C                   PARM                    ESITO
023800080526     C*
023900080526     C                   ENDIF
024000001006     C*
024100030401     C                   ENDDO
024200001006     C*
024300030401     C                   RETURN
