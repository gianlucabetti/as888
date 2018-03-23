000100040224
000200040224     FWFOUTQ0F  O    E             DISK
000300040224
000400000000     D* DS for the entry length
000500000000     D DSLEN           DS
000600000000     D  ENTLN                  1      4B 0
000700000000     D* DS for the start position
000800000000     D DSSTR           DS
000900000000     D  STRPS                  1      4B 0
001000000000     D* DS for Hex 00s
001100000000     D DSHEX0          DS
001200000000     D  HEX00                  1      4B 0
001300000000     D* DS for the entry length of SPLFA DS
001400000000     D DSSPLN          DS
001500000000     D  SPAB                   1      4B 0
001600000000     D* OSSPLF0100 cannot be used because same names exist in the
001700000000     D*  in the OPSPLA0100 data structure
001800000000     D DSSPLF          DS
001900000000     D  OUTQ                  11     20
002000000000     D  OUTQL                 21     30
002100000000     D  INTJID                51     66
002200000000     D  INTFID                67     82
002300000000     D ERRSTS         SDS
002400000000     D  MSGID                 40     46
002500040223      *---------------------------------------
002600040223      *---- INGLOBATO COPY -------------------
002700040223      *---------------------------------------
002800040223      *I/COPY QUSRTOOL/QATTRPG,OPSPLA0100
002900040223     D RCVVAR          DS
003000040223     D  BYTRTN                 1      4B 0
003100040223     D  BYTVAL                 5      8B 0
003200040223     D  JOBID                  9     24
003300040223     D  SPLFID                25     40
003400040223     D  JOBNAM                41     50
003500040223     D  USRNAM                51     60
003600040223     D  JOBNUM                61     66
003700040223     D  FILNAM                67     76
003800040223     D  FILNUM                77     80B 0
003900040223     D  FRMTYP                81     90
004000040223     D  USRDTA                91    100
004100040223     D  STATUS               101    110
004200040223     D  FILAVL               111    120
004300040223     D  HLDF                 121    130
004400040223     D  SAVF                 131    140
004500040223     D  TOTPAG               141    144B 0
004600040223     D  PAGWRT               145    148B 0
004700040223     D  STRPAG               149    152B 0
004800040223     D  ENDPAG               153    156B 0
004900040223     D  LASPAG               157    160B 0
005000040223     D  RESPRT               161    164B 0
005100040223     D  TOTCPY               165    168B 0
005200040223     D  CPYLFT               169    172B 0
005300040223     D  LPI                  173    176B 0
005400040223     D  CPI                  177    180B 0
005500040223     D  OUTPRI               181    182
005600040223     D  OUTQNM               183    192
005700040223     D  OUTQLB               193    202
005800040223     D  DATFOP               203    209
005900040223     D  TIMFOP               210    215
006000040223     D  DEVFNA               216    225
006100040223     D  DEVFLB               226    235
006200040223     D  PGMOPF               236    245
006300040223     D  PGMOPL               246    255
006400040223     D  ACCCOD               256    270
006500040223     D  PRTTXT               271    300
006600040223     D  RCDLEN               301    304B 0
006700040223     D  MAXRCD               305    308B 0
006800040223     D  DEVCLS               309    318
006900040223     D  PRTTYP               319    328
007000040223     D  DOCNAM               329    340
007100040223     D  FLDNAM               341    404
007200040223     D  S36PRC               405    412
007300040223     D  PRTFID               413    422
007400040223     D  RPLUN                423    423
007500040223     D  RPLCHR               424    424
007600040223     D  PAGLEN               425    428B 0
007700040223     D  PAGWID               429    432B 0
007800040223     D  NUMSEP               433    436B 0
007900040223     D  OVRLIN               437    440B 0
008000040223     D  DBCSDA               441    450
008100040223     D  DBCSEC               451    460
008200040223     D  DBCSSO               461    470
008300040223     D  DBCSCR               471    480
008400040223     D  DBCSCI               481    484B 0
008500040223     D  GRAPHI               485    494
008600040223     D  CODPAG               495    504
008700040223     D  FORNAM               505    514
008800040223     D  FORLIB               515    524
008900040223     D  SRCDRW               525    528B 0
009000040223     D  PRTFON               529    538
009100040223     D  S36SPL               539    544
009200040223     D  PAGROT               545    548B 0
009300040223     D  JUSTIF               549    552B 0
009400040223     D  PRTBOT               553    562
009500040223     D  FLDRCD               563    572
009600040223     D  CTLCHR               573    582
009700040223     D  ALGFRM               583    592
009800040223     D  PRTQUA               593    602
009900040223     D  FRMFED               603    612
010000040223     D  VOLUME               613    683
010100040223     D  FLABID               684    700
010200040223     D  EXCTYP               701    710
010300040223     D  CHRCOD               711    720
010400040223     D  TOTRCD               721    724B 0
010500040223     D  PGPSID               725    728B 0
010600040223     D  FOVNAM               729    738
010700040223     D  FOVLIB               739    748
010800040223     D  FOVOFD               749    756P 5
010900040223     D  FOVOFA               757    764P 5
011000040223     D  BOVNAM               765    774
011100040223     D  BOVLIB               775    784
011200040223     D  BOVOFD               785    792P 5
011300040223     D  BOVOFA               793    800P 5
011400040223     D  UOM                  801    810
011500040223     D  PAGNAM               811    820
011600040223     D  PAGLIB               821    830
011700040223     D  LINSPC               831    840
011800040223     D  PNTSIZ               841    848P 5
011900040223     D  FMRFOD               849    856P 5
012000040223     D  FMRFOA               857    864P 5
012100040223     D  BMRFOD               865    872P 5
012200040223     D  BMRFOA               873    880P 5
012300040223     D  MPGLEN               881    888P 5
012400040223     D  MPGWID               889    896P 5
012500040223     D  MEAMTH               897    906
012600040223     D  AFPRES               907    907
012700040223     D  FCHSNM               908    917
012800040223     D  FCHSLB               918    927
012900040223     D  CDPGNM               928    937
013000040223     D  CDPGLB               938    947
013100040223     D  CFNTNM               948    957
013200040223     D  CFNTLB               958    967
013300040223     D  DCFTNM               968    977
013400040223     D  DCFTLB               978    987
013500040223     D  USRFIL               988    997
013600040223     D  REDUCE               998   1007
013700040223     D  RSVRD5              1008   1008
013800040223     D  OUTBIN              1009   1012B 0
013900040223     D  PRTCCD              1013   1016B 0
014000040223     D  USRPRT              1017   1116
014100040223     D  ORGSYS              1117   1124
014200040223     D  ORGSID              1125   1132
014300040223     D  ORGUSR              1133   1142
014400040224     D  USRDDT              1157   1411
014500040223      *---------------------------------------
014600040223      *-------- FINE COPY --------------------
014700040223      *---------------------------------------
014800000000     C     *ENTRY        PLIST                                                  Parm list
014900040224     C                   PARM                    wfSYS                          System name
015000000000     C                   PARM                    DSSTR                          Start pos
015100000000     C                   PARM                    NBRENT            9 0          Nbr of entrs
015200000000     C                   PARM                    DSLEN                          Entry length
015300000000     C                   PARM                    FULSPC           20            Full usr spc
015400000000     C* Initialization
015500000000     C                   Z-ADD     0             HEX00                          Hex 00s
015600040224     C                   Z-ADD     1411          SPAB                           Len of SPLA
015700000000     C* Do for the number of entries received
015800000000     C                   DO        NBRENT                                       Do nbr of
015900000000     C                   CALL      'QUSRTVUS'                                   Call extract
016000000000     C                   PARM                    FULSPC                         User space
016100000000     C                   PARM                    DSSTR                          Start pos
016200000000     C                   PARM                    DSLEN                          Entry length
016300000000     C                   PARM                    DSSPLF                         Data struct
016400000000     C* For first time, extract OUTQ name and date/time converted
016500000000     C     HDGSW         IFEQ      *BLANK                                       Heading sw
016600000000     C                   TIME                    FULTIM           12 0          Full time
016700040224     C                   MOVEL     FULTIM        wfCVTT                         Convert time
016800040224     C                   MOVE      FULTIM        wfCVTD                         Convert date
016900040224     C                   MOVE      OUTQ          wfOUTQ                         OUTQ name
017000040224     C                   MOVE      OUTQL         wfOQLB                         OUTQ lib
017100000000     C                   MOVE      'X'           HDGSW             1            Set hdg sw
017200000000     C                   END                                                    Heading sw
017300000000     C* Call QUSRSPLA to get detail attributes of spooled file
017400000000     C*  Inlz fields to be used for the retrieve
017500000000     C                   MOVEL     '*INT'        JOB              26            Job name
017600000000     C                   MOVEL     '*INT'        SPLFNM           10
017700040223     C                   MOVE      DSHEX0        SPLFNB            4
017800000000     C                   CALL      'QUSRSPLA'                           20
017900000000     C                   PARM                    RCVVAR                         SPLFA DS
018000000000     C                   PARM                    DSSPLN            4            DS len
018100000000     C                   PARM      'SPLA0100'    FORMAT            8            DS rcv
018200000000     C                   PARM                    JOB                            Job name
018300000000     C                   PARM                    INTJID                         Job ID
018400000000     C                   PARM                    INTFID                         SPLF ID
018500000000     C                   PARM                    SPLFNM                         SPLF name
018600000000     C                   PARM                    SPLFNB                         SPLF number
018700000000     C* Indicator 20 can occur if the spooled file no longer exists
018800000000     C   20              DO                                                     If error
018900000000     C* Check status DS for specific message
019000000000     C*   Spooled file not in the system
019100000000     C     MSGID         IFEQ      'CPF3344'                                    No SPLF
019200000000     C                   GOTO      ENDSPF                                       Branch
019300000000     C                   ENDIF                                                  No SPLF
019400000000     C*   Job not in the system
019500000000     C     MSGID         IFEQ      'CPF3342'                                    No job
019600000000     C                   GOTO      ENDSPF                                       Branch
019700000000     C                   ENDIF                                                  No job
019800000000     C* If some other message ID exists, set H5 and return
019900000000     C                   SETON                                        H5        Error indic
020000000000     C                   RETURN                                                 Return
020100000000     C                   ENDDO                                                  If error
020200000000     C* Move fields
020300040224     C                   MOVE      JOBNAM        wfJNAM                         Job name
020400040224     C                   MOVE      USRNAM        wfUSER                         User name
020500040224     C                   MOVE      JOBNUM        wfJNBR                         Job number
020600040224     C                   MOVE      USRDTA        wfUDTA                         User data
020700040224     C                   MOVE      FRMTYP        wfFTYP                         Form type
020800040224     C                   MOVE      FILNAM        wfFILE                         Spl file nam
020900040223     C                   Z-ADD     FILNUM        DEC6              6 0          Spl file nbr
021000040224     C                   MOVE      DEC6          wfFNBR                         Spl file nbr
021100040224     C                   Z-ADD     TOTCPY        wfCPY                          Copies
021200040224     C                   MOVE      CPYLFT        wfCPYL                         Copies left
021300040224     C                   MOVE      OUTPRI        wfPTY                          Pritority
021400040224     C                   MOVEL     FILAVL        wfSCHD                         Schedule
021500040224     C                   MOVEL     HLDF          wfHOLD                         Hold
021600040224     C                   MOVEL     FLDRCD        wfFOLD                         Fold
021700040224     C                   MOVEL     SAVF          wfSAVF                         Save file
021800040224     C                   MOVE      DATFOP        wfDAT                          Date opened
021900040224     C                   MOVEL     DATFOP        wfCEN                          Cent opened
022000040224     C                   MOVE      TIMFOP        wfTIM                          Time opened
022100040224     C                   MOVE      ACCCOD        wfACGC                         Acctg code
022200040224     C                   MOVE      PRTTXT        wfPRTX                         Print text
022300040224     C                   MOVE      DEVFNA        wfDEV                          Device
022400040224     C                   MOVE      DEVFLB        wfDEVL                         Device lib
022500040224     C                   MOVE      PGMOPF        wfPGM                          Pgm that opn
022600040224     C                   MOVE      PGMOPL        wfPGML                           Pgm lib
022700040224     C                   Z-ADD     PAGLEN        wfPAGL                         Page length
022800040224     C                   Z-ADD     PAGWID        wfPAGW                         Page width
022900040224     C                   Z-ADD     TOTPAG        wfNPAG                         Nbr of pages
023000040224     C                   Z-ADD     PAGWRT        wfWRTP                         Page being w
023100040224     C                   Z-ADD     STRPAG        wfSTRP                         Start page
023200040224     C                   Z-ADD     ENDPAG        wfENDP                         End page
023300040224     C                   Z-ADD     LASPAG        wfLSTP                         Last page
023400040224     C                   Z-ADD     RESPRT        wfRESP                         Restart page
023500000000     C                   Z-ADD     LPI           DEC9              9 0          LPI
023600040224     C                   MOVE      DEC9          wfLPI                          LPI
023700000000     C                   Z-ADD     CPI           DEC9                           CPI
023800040224     C                   MOVE      DEC9          wfCPI                          CPI
023900040224     C                   Z-ADD     NUMSEP        wfNSEP                         Nbr of seps
024000040224     C                   Z-ADD     OVRLIN        wfOFLN                         Overflow lin
024100040224     C                   MOVE      PRTFON        wfFONT                         Print font
024200040224     C                   Z-ADD     PAGROT        wfPGRT                         Page rotate
024300040224     C                   MOVE      PRTBOT        wfBOTH                         Prt both sid
024400040224     C                   Z-ADD     JUSTIF        wfJUST                         Justificatn
024500040224     C                   MOVE      ALGFRM        wfALGN                         Alignment
024600040224     C                   MOVE      PRTQUA        wfPQTY                         Prt quality
024700040224     C                   MOVE      PRTFID        wfPFID                         Prt fidelity
024800040224     C                   Z-ADD     TOTRCD        wfNREC                         Nbr of rcds
024900040224     C                   Z-ADD     RCDLEN        wfRLEN                         Rcd length
025000040224     C                   Z-ADD     MAXRCD        wfMAXR                         Max records
025100040224     C                   Z-ADD     SRCDRW        wfSRCD                         Source drawr
025200040224     C                   MOVE      DEVCLS        wfDEVT                         Device type
025300040224     C                   MOVE      PRTTYP        wfPRTT                         Printer type
025400040224     C                   MOVE      DOCNAM        wfDOC                          Doc name
025500040224     C                   MOVE      FLDNAM        wfFLDR                         Folder name
025600040224     C                   MOVE      CODPAG        wfCDEP                         Code page
025700040224     C                   MOVE      GRAPHI        wfGRST                         Graphic set
025800040224     C                   MOVE      CTLCHR        wfCTLC                         Control char
025900040224     C                   MOVE      FLDRCD        wfDUPX                         Duplex
026000040224     C                   MOVEL     USRDDT        wfDAUT                         Duplex
026100000000     C* Special handling cases
026200000000     C*    Status in DS contains values like *READY. Change to 3 char
026300000000     C     STATUS        IFEQ      '*READY  '                                   Rdy
026400040224     C                   MOVE      'RDY'         wfSTS                          Move sts
026500000000     C                   END                                                    Rdy
026600000000     C     STATUS        IFEQ      '*OPEN   '                                   Rdy
026700040224     C                   MOVE      'OPN'         wfSTS                          Move sts
026800000000     C                   END                                                    Rdy
026900000000     C     STATUS        IFEQ      '*CLOSED '                                   Rdy
027000040224     C                   MOVE      'CLO'         wfSTS                          Move sts
027100000000     C                   END                                                    Rdy
027200000000     C     STATUS        IFEQ      '*HELD   '                                   Rdy
027300040224     C                   MOVE      'HLD'         wfSTS                          Move sts
027400000000     C                   END                                                    Rdy
027500000000     C     STATUS        IFEQ      '*SAVED  '                                   Rdy
027600040224     C                   MOVE      'SAV'         wfSTS                          Move sts
027700000000     C                   END                                                    Rdy
027800000000     C     STATUS        IFEQ      '*WRITING'                                   Rdy
027900040224     C                   MOVE      'WTR'         wfSTS                          Move sts
028000000000     C                   END                                                    Rdy
028100000000     C     STATUS        IFEQ      '*PENDING'                                   Rdy
028200040224     C                   MOVE      'PNS'         wfSTS                          Move sts
028300000000     C                   END                                                    Rdy
028400000000     C     STATUS        IFEQ      '*PRINTER'                                   Rdy
028500040224     C                   MOVE      'PRT'         wfSTS                          Move sts
028600000000     C                   END                                                    Rdy
028700000000     C* Write output record
028800040224     C                   WRITE     WFOUTQ00                                     Write rcd
028900000000     C* Bump for next start position
029000000000     C     ENDSPF        TAG                                                    End of splf
029100000000     C                   ADD       ENTLN         STRPS                          Bump
029200000000     C                   END                                                    Do nbr of
029300040224      *
029400040224     C                   Eval      *inLR = *on
029500000000     C                   RETURN                                                 Return
