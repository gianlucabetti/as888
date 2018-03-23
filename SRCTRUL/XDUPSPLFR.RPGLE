000100040218      ********************************************************************************************
000200040218      ***   ATTENZIONE compilare dal profilo QSECOFR ed indicare nel parametro USRPRF *OWNER   ***
000300040218      ********************************************************************************************
000400080408      /COPY QSYSINC/QRPGLESRC,QUSRSPLA
000500080408     D*** SplfAtt         DS
000600080408     D***  AtrData                1   3824
000700080408     D***  WrkExtData          1073   1153
000800000000      *
000900080408     D APIError        DS                  INZ
001000040629     D  EBytesP                1      4B 0 INZ(%SIZE(APIError))
001100000000     D  EBytesA                5      8B 0
001200000000     D  EMsgID                 9     15
001300000000     D  EReserverd            16     16
001400040629     D  EData                 17     99
001500000000      *
001600000000     D                 DS
001700040219     D SplfXb                  1      4B 0
001800040219     D CSplX                   1      4
001900000000      *
002000000000     D SplSpc1         C                   CONST('SPL001US  QTEMP     ')
002100000000      *
002200000000     D SpcPtr          S               *
002300000000     D SpcDes          S             50    INZ('User Space Spool APIs')
002400000000     D SpcName         S             20
002500000000     D SpcAtr          S             10
002600000000     D SpcAut          S             10    INZ('*ALL')
002700000000     D SpcSiz          S              9B 0 INZ(32767)
002800000000     D SpcFormat       S              8
002900000000     D SpcInv          S              1
003000000000      *
003100000000     D JobName         S             26    INZ
003200000000     D JobID           S             26    INZ
003300000000     D SplFID          S             26    INZ
003400000000     D EndOper         S             10    INZ('*WAIT')
003500080408     D RecLen          S              9B 0 INZ(%size(QUSA0200))
003600000000     D SplfIH          S              9B 0
003700000000     D SplfOH          S              9B 0
003800040128     D XBuff           S              9B 0
003900040128     D BufferX         S              9B 0
004000040219     D SplfXd          S              6  0
004100000000      *
004200000000     D File            S             10
004300000000     D Job             S             10
004400000000     D User            S             10
004500040128     D JobX            S              6
004600040219     D SplfXx          S              6
004700040130
004800040130     D* Informazioni sul job
004900040130     D psds           sds
005000040130     D  psuser               254    263
005100040130
005200040130     D* Parametri x modifiche attributi spool
005300040130     D PARAM           S            262    DIM(20)
005400040130
005500040130     D* DS scomposizione parametri x modifiche attributi spool
005600040130     D SPOOLATR        DS
005700040130     D  POSDA                  1      4  0
005800040130     D  LUNG                   5      7  0
005900040130     D  VALORE                 8    262
006000040130
006100000000      ****************************************************************
006200000000     C                   EXSR      $ONE
006300000000      *
006400000000     C                   SETON                                        LR
006500000000      **************************************************************
006600000000      * Copy Spooled File To Duplicate Spooled File                *
006700000000      **************************************************************
006800000000     C     $ONE          BEGSR
006900000000      *
007000000000     C                   EXSR      $RTVSPLFA
007100040202     C*
007200040202     C* Eseguo routine d modifica attributi dello spool secondo valori d default
007300040202     C                   EXSR      CHGSPLFADFT
007400040128     C* Eseguo routine d "personalizzazione" attributi dello spool
007500040130     C                   EXSR      CHGSPLFA
007600040128     C*
007700000000     C                   eval      SpcName = SplSpc1
007800000000     C                   EXSR      $CRTSP
007900000000     C                   EXSR      $CREATESF
008000000000     C                   EXSR      $OPENSF
008100000000     C                   EXSR      $GETSF
008200000000     C                   EXSR      $PUTSF
008300000000     C                   EXSR      $CLOSESF
008400040130     C                   EXSR      $DLTSP
008500000000      *
008600000000     C                   ENDSR
008700040202      *****************************************************************
008800040202      * Modifica attributi dello spool "destinazione" secondo default *
008900040202      *****************************************************************
009000040202     C     CHGSPLFADFT   BEGSR
009100040202      *
009200040202      * D default impongo che l'utente proprietario dello spool "destinazione"
009300040202      * sia lo stesso che effettua la richiesta d copia spool
009400080408     C***                EVAL      %subst(QUSA0200:58+1:10) = psuser
009500080408     C                   EVAL      QUSUN13 = psuser
009600040202      *
009700040202     C                   ENDSR
009800040128      **************************************************************
009900040128      * Personalizzazione attributi dello spool                    *
010000040128      **************************************************************
010100040128     C     CHGSPLFA      BEGSR
010200040130      *
010300040130      * Inizializzo contatore attributi spool da modificare
010400040130     C                   Z-ADD     1             i                 2 0
010500040130      * Ciclo x tutti gli elementi della schiera dei parametri attributi spool da modificare
010600040130     C                   DOW       i <= %elem(PARAM) AND
010700040130     C                             PARAM(i) <> *blanks
010800040130     C                   EVAL      SPOOLATR = PARAM(i)
010900040130      * Verifico che tutti gli elementi necessari x effettuare la modifica dell'attributo siano presenti
011000040130     C                   IF        POSDA > *zeros AND
011100040130     C                             LUNG  > *zeros
011200080408     C                   EVAL      %subst(QUSA0200:POSDA+1:LUNG)=%trimr(VALORE)
011300040130     C                   ENDIF
011400040130     C                   EVAL      i = i + 1
011500040130     C                   ENDDO
011600040128      *
011700040128     C                   ENDSR
011800000000      **************************************************************
011900000000      * Create Spooled File                                        *
012000000000      **************************************************************
012100000000     C     $CREATESF     BEGSR
012200080408      *
012300080408     C                   RESET                   APIError
012400000000      *
012500000000     C                   CALL      'QSPCRTSP'
012600000000     C                   PARM                    SplfOH
012700080408     C                   PARM                    QUSA0200
012800000000     C                   PARM                    APIError
012900000000      *
013000000000     C                   ENDSR
013100000000      **************************************************************
013200000000      * Open Spooled File                                          *
013300000000      **************************************************************
013400000000     C     $OPENSF       BEGSR
013500080408      *
013600080408     C                   RESET                   APIError
013700000000      *
013800040128     C                   eval      XBuff = -1
013900000000      *
014000000000     C                   CALL      'QSPOPNSP'
014100000000     C                   PARM                    SplfIH
014200000000     C                   PARM                    JobName
014300000000     C                   PARM                    JobID
014400000000     C                   PARM                    SplfID
014500000000     C                   PARM                    File
014600040128     C                   PARM                    SplfXb
014700040128     C                   PARM                    XBuff
014800000000     C                   PARM                    APIError
014900000000      *
015000000000     C                   ENDSR
015100000000      **************************************************************
015200000000      * Get Spooled File Data                                      *
015300000000      **************************************************************
015400000000     C     $GETSF        BEGSR
015500080408      *
015600080408     C                   RESET                   APIError
015700000000      *
015800040128     C                   eval      BufferX = -1
015900000000      *
016000000000     C                   CALL      'QSPGETSP'
016100000000     C                   PARM                    SplfIH
016200000000     C                   PARM                    SpcName
016300000000     C                   PARM      'SPFR0200'    SpcFormat
016400040128     C                   PARM                    BufferX
016500000000     C                   PARM                    EndOper
016600000000     C                   PARM                    APIError
016700000000      *
016800000000     C                   ENDSR
016900000000      **************************************************************
017000000000      * Put Spooled File Data                                      *
017100000000      **************************************************************
017200000000     C     $PUTSF        BEGSR
017300080408      *
017400080408     C                   RESET                   APIError
017500000000      *
017600000000     C                   CALL      'QSPPUTSP'
017700000000     C                   PARM                    SplfOH
017800000000     C                   PARM                    SpcName
017900000000     C                   PARM                    APIError
018000000000      *
018100000000     C                   ENDSR
018200000000      **************************************************************
018300000000      * Close Spooled File                                         *
018400000000      **************************************************************
018500000000     C     $CLOSESF      BEGSR
018600080408      *
018700080408     C                   RESET                   APIError
018800000000      *
018900000000     C                   CALL      'QSPCLOSP'
019000000000     C                   PARM                    SplfIH
019100000000     C                   PARM                    APIError
019200080408      *
019300080408     C                   RESET                   APIError
019400000000      *
019500000000     C                   CALL      'QSPCLOSP'
019600000000     C                   PARM                    SplfOH
019700000000     C                   PARM                    APIError
019800000000      *
019900000000     C                   ENDSR
020000000000      **************************************************************
020100000000      * Call Retrieve Spooled File Attributes API                  *
020200000000      **************************************************************
020300000000     C     $RTVSPLFA     BEGSR
020400000000      *
020500040128     C                   eval      JobName = Job + User + JobX
020600040128     C                   MOVE      SplfXx        SplfXd
020700040128     C                   eval      SplfXb = SplfXd
020800000000      *
020900000000     C                   CALL      'QUSRSPLA'
021000080408     C                   PARM                    QUSA0200
021100000000     C                   PARM                    RecLen
021200000000     C                   PARM      'SPLA0200'    SpcFormat
021300000000     C                   PARM                    JobName
021400000000     C                   PARM                    JobID
021500000000     C                   PARM                    SplfID
021600000000     C                   PARM                    File
021700040128     C                   PARM                    SplfXb
021800000000      *
021900000000     C                   ENDSR
022000000000      **************************************************************
022100000000      * Create User Space                                          *
022200000000      **************************************************************
022300000000     C     $CRTSP        BEGSR
022400000000      *
022500000000     C                   CALL      'QUSCRTUS'
022600000000     C                   PARM                    SpcName
022700000000     C                   PARM                    SpcAtr
022800000000     C                   PARM                    SpcSiz
022900000000     C                   PARM                    SpcInv
023000000000     C                   PARM                    SpcAut
023100000000     C                   PARM                    SpcDes
023200000000      *
023300000000     C                   ENDSR
023400040130      **************************************************************
023500000000      * Retrieve Pointer To User Space                             *
023600000000      **************************************************************
023700000000     C     $RTVSP        BEGSR
023800000000      *
023900000000     C                   CALL      'QUSPTRUS'
024000000000     C                   PARM                    SpcName
024100000000     C                   PARM                    SpcPtr
024200000000      *
024300000000     C                   ENDSR
024400000000      **************************************************************
024500000000      * Delete User Space                                          *
024600000000      **************************************************************
024700000000     C     $DLTSP        BEGSR
024800080408      *
024900080408     C                   RESET                   APIError
025000000000      *
025100000000     C                   CALL      'QUSDLTUS'
025200000000     C                   PARM                    SpcName
025300000000     C                   PARM                    APIError
025400000000      *
025500000000     C                   ENDSR
025600000000      ***************************************************************
025700000000      * INITIALIZATION SUBROUTINE                                   *
025800000000      ***************************************************************
025900000000     C     *INZSR        BEGSR
026000000000      *
026100000000     C     *ENTRY        PLIST
026200000000     C                   PARM                    File
026300000000     C                   PARM                    Job
026400000000     C                   PARM                    User
026500040128     C                   PARM                    JobX
026600040128     C                   PARM                    SplfXx
026700040130     C                   PARM                    PARAM
026800000000      *
026900000000     C                   ENDSR
