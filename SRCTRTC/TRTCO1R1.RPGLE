000100030401     H*--------------------------------------------------------------------------------------------*
000200070607     H* Polling su OUTQ di "IN" x faxing automatico spool.
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
010100030404     D*---
010200030404     D* DS per chiamata pgm utilità x passaggio da spool a file *FCFC "normalizzato"
010300030404     D*---
010400030404     D trulfcds      e ds
010500001006     D*---
010600001006     D* Variabili di wrk
010700001006     D*---
010800990513     D jobinf          ds
010900990513     D    job                        10
011000990513     D    user                       10
011100990513     D    jobn                        6
011200071126     D*
011300071126     D dtaqOUT         S             10    inz
011400070607     D*---
011500070607     D* Costanti
011600070607     D*---
011700070607     D* La costante "dtaqlib" è valorizzata espressamente in GAITRAOBJ e nn *LIBL xchè
011800070607     D* l'API che ne reperisce da DTAQ relativa in questo modo è + veloce.
011900070607     D dtaqlib         c                   const('GAITRAOBJ ')
012000070607     C*
012100070607     C* Ricevimento parametri
012200070607     C     *ENTRY        PLIST
012300070607     C                   PARM                    dtaqIN           10
012400070607     C                   PARM                    dfil              3
012500071031     C*
012600071126     C                   EVAL      dtaqOUT = 'FAXOUT' + dfil
012700040930     C*
012800040930     C* Verifico correttezza parametri
012900070607     C                   IF        dtaqIN = *blanks
013000070607     C                   ELSE
013100990513     C*
013200040930     C* Sta in ascolto sulla DTAQ "IN" abbinata alla OUTQ "IN"
013300170306     C                   EVAL      FLDLEN = 128
013400170306     C                   DOW       FLDLEN > *zeros
013500990513     C                   CALL      'QRCVDTAQ'                           21
013600040930     C                   PARM      DTAQIN        QNAME            10
013700030401     C                   PARM      DTAQLIB       QLIB             10
013800170306     C                   PARM                    FLDLEN            5 0
013900990513     C                   PARM                    FIELD           128
014000170306     C                   PARM      *zeros        WAIT              5 0
014100990513     C*
014200170306     C* Se reperito qualcosa
014300170308     C                   IF        FLDLEN > *zeros  AND
014400170308     C                             FJOB  <> *blanks AND
014500170306     C                             FUSR  <> *blanks AND
014600170306     C                             FJOBN <> *blanks
014700170306     C*
014800990513     C                   MOVEL     FJOB          JOB
014900990513     C                   MOVEL     FUSR          USER
015000990513     C                   MOVEL     FJOBN         JOBN
015100990513     C*
015200990513     C* Reperisce attributi del file
015300990513     C                   CALL      'QUSRSPLA'                           21
015400990513     C                   PARM                    RCVAR1
015500001006     C                   PARM      1500          RCVLEN
015600990513     C                   PARM      'SPLA0100'    FMTNM1            8
015700990513     C                   PARM                    JOBINF
015800001006     C                   PARM                    JOBid            16
015900990513     C                   PARM                    SPLFID           16
016000990513     C                   PARM                    FSPL
016100990513     C                   PARM                    FSPLN
016200001006     C*
016300030401     C* Lancia il CL di conversioine spool in file *FCFC nell IFS
016400030401     C                   MOVEL(p)  *ALL'0'       NUMFIL            6
016500030401     C                   MOVE      FILNUM        NUMFIL
016600030404     C* Valorizzo la DS pgm utilità TRULFCC
016700030404     C                   CLEAR                   TRULFCDS
016800030404     C                   EVAL      DFCFNAM  = FILNAM
016900030404     C                   EVAL      DFCJNAM  = JOBNAM
017000030404     C                   EVAL      DFCUNAM  = USRNAM
017100030404     C                   EVAL      DFCJNUM  = JOBNUM
017200030404     C                   EVAL      DFCFNUM  = NUMFIL
017300030404     C                   EVAL      DFCFOUT  = 'TRTCMWWF'
017400040907     C                   EVAL      DFCLOUT  = 'QTEMP'
017500030514     C                   EVAL      DFCFOPT  = 'R'
017600070607     C*
017700070607     C                   MOVEL     *BLANKS       subFldr          50
017800070607     C                   IF        dfil <> *blanks
017900160608     C                   EVAL      subFldr = 'FAX/' + %trim(dfil) + '/'
018000070607     C                   ENDIF
018100030404     C*
018200160607     C                   CALL(e)   'TRBMAF05R'
018300160607     C                   PARM      'TRTCO1R'     APLKEY           15
018400160607     C                   PARM      *blanks       IFSNAM           10
018500160607     C                   PARM                    FILNAM
018600160607     C                   PARM                    JOBNAM
018700160607     C                   PARM                    USRNAM
018800160607     C                   PARM                    JOBNUM
018900160607     C                   PARM                    NUMFIL
019000160607     C                   PARM      'N'           FLGDATAORA        1
019100160607     C                   PARM      *blanks       FILEOUT         255
019200160607     C                   PARM      subFldr       SUBFLDR          50
019300160607     C                   PARM      'S'           FLGSUBFLDR        1
019400160607     C                   PARM      'S'           FLGREPLACE        1
019500160607     C                   PARM      'S'           FLGLOG            1
019600160607     C                   PARM                    ESITO             1
019700160607     C*
019800160607     C                   IF        not %error AND ESITO <> '2'
019900160607     C                   CALL(e)   'TRTCO2C'
020000160607     C                   PARM      'CC'          OPZIONE           2
020100160607     C                   PARM                    FILNAM
020200160607     C                   PARM                    JOBNAM
020300160607     C                   PARM                    USRNAM
020400160607     C                   PARM                    JOBNUM
020500160607     C                   PARM                    NUMFIL
020600160607     C                   PARM                    dtaqIN
020700160607     C                   PARM                    dtaqOUT
020800160607     C                   PARM                    ESITO
020900160607     C                   ENDIF
021000001006     C*
021100170306     C                   ENDIF
021200170306     C*
021300030401     C                   ENDDO
021400070607     C*
021500070607     C                   ENDIF
021600001006     C*
021700030401     C                   RETURN
