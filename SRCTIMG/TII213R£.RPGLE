000100990422     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990422     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     F* MPH940R *-MP/38-*--------------------------------------------*
000400000000     F*-----------------*                                            *
000500000000     F*       - LANCIO OPERAZIONI PERIODICHE MAGAZZ.  M.P. -         *
000600000000     F*       ----------------------------------------------         *
000700000000     F*                                                   AZN.: H900 *
000800000000     F*         FILE : MPH940D-ANAMP01L-ANTAB00F-                    *
000900000000     F*                                                              *
001000000000     F*         PGM. : RIC001R   - RICERCA INTESTAZIONE DITTA        *
001100000000     F*                RICDATE   - CONTROLLO INVERSIONE DATA         *
001200000000     F*                ANU153R   - RICERCA CODICI  DI MAGAZZINO      *
001300000000     F*                                                              *
001400000000     F*                AZN.-H942 - STAMPA GIORNALE DI  MAGAZZINO     *
001500000000     F*                AZN.-H943 - ELIMINAZIONE PERIODICA DI MAG.    *
001600851217     F*                AZN.-H944 - APERTURA ESERCIZIO                *
001700000000     F*                AZN.-H945 - GENERAZ.MOVIM.GIACENZA INIZ.      *
001800000000     F*--------------------------------------------------------------*
001900990422     Ftii213D   CF   E             WORKSTN
002000000000     FANTAB00F  IF   E           K DISK
002100860103     D ERR             S             50    DIM(21) CTDATA PERRCD(1)             *ERRORI
002200000000     D SCELTA          DS
002300990422     D  SCE944                 1      1
002400990422     D  SCE945                 2      2
002500000000     D MPHDT1          DS           512
002600000000     D*-----DATA AREA NUMERATORE MOVIM. MAGAZZINO
002700000000     D  NMOV                   1      5
002800000000     D  FIL01                  6      8
002900000000     D  DMOV                   9     14
003000000000     D  FIL02                 15     17
003100000000     D  ACOM                  18     19
003200000000     D  FIL03                 20     50
003300000000     D  DGIO                  51     56
003400000000     D  FIL04                 57    100
003500000000     D  DPER                 101    106
003600000000     D  FIL05                107    150
003700000000     D  AGEN                 151    152
003800000000     D  FIL06                153    200
003900850415     D  AGEL                 201    202
004000850415     D  FIL07                203    250
004100850527     D  NUMDB                251    255
004200850611     D  FIL08                256    300
004300850611     D  GMAX                 301    302
004400851128     D  FIL09                303    350
004500860624     D  MASDTA               351    353
004600860624     D  FIL101               354    378
004700860624     D  ADBDTA               379    379
004800860624     D  FIL102               380    400
004900851217     D  NUMIN                401    405
005000851217     D  FIL11                406    450
005100000000     D  FIL12                451    500
005200000000     D  UTE                  501    512
005300851128     D PAR944          DS
005400990422     D  NAA944                        4  0
005500990422     d  soc944                        3
005600000000     D PAR945          DS
005700990422     D  ANS945                        4  0
005800990422     D  MAG945                        7
005900990422     D  DDC945                        8  0
006000990422     D  NDC945                        7  0
006100990422     D  AMV945                        4  0
006200990422     D  DMV945                        8  0
006300990422     D  NMV945                        7  0
006400990422     D  LIS945                        3
006500990422     D  COL945                        1  0
006600990422     d  soc945                        3
006700860103     D TB153         E DS                  EXTNAME(ANTAB153)
006800000000     D KPJBA         E DS
006900000000     D*----- PASSAGGIO PARAMETRI
007000000000     D  MSGERR               453    502
007100990422     D soc001        E DS                  EXTNAME(xsoc001ds)
007200990422     D xsocds          DS          1000
007300990422     d dataiso         s               d   datfmt(*iso)
007400990422     d dataeur         s               d   datfmt(*eur)
007500000000     C*----------------------------------------------------*
007600000000     C     *ENTRY        PLIST                                                  *
007700000000     C                   PARM                    KPJBA                          *
007800000104     C                   MOVE      *ZEROS        PAR945                         *
007900000000     C*---------- ANAGRAFICO TABELLE :                     *
008000000000     C     KEYTAB        KLIST                                                  *
008100000000     C                   KFLD                    TABGRU                         *
008200000000     C                   KFLD                    TABCOD                         *
008300000000     C*---------- RICERCA DITTA :                          *
008400990422     C                   MOVEL     'SOC001'      TIPXSC
008500990422     C                   MOVEL     *blank        SOCXSC
008600990422     C                   EXSR      REPSOC
008700990422     C     RTNXSC        IFNE      '1'
008800990422     C                   MOVEL     XSOCDS        SOC001
008900990422     C                   MOVEL     xscsoc        soc944
009000990422     c                   move      xscsoc        soc945
009100990422     c                   end
009200000000     C*---------                                           *
009300000000     C     *DTAARA       DEFINE                  MPHDT1                         *
009400000000     C*--------- CONTROLLI SU PROMPT ----------------------*
009500000000     C     INIZIO        TAG                                                    *
009600000000     C                   MOVE      *BLANKS       SCELTA                         *
009700000000     C     EMI940        TAG                                                    *
009800000000     C                   EXFMT     MPH940PT                                     *
009900990422     C   Kc              GOTO      FINE                                         *
010000000000     C                   MOVEA     *ZEROS        *IN(1)                         *
010100000000     C                   MOVE      *BLANKS       ERRORE                         *
010200000000     C     SCELTA        IFEQ      *BLANKS                                      *
010300000000     C                   SETON                                            90    *
010400000000     C                   MOVEL     ERR(2)        ERRORE                         *
010500000000     C                   GOTO      EMI940                                       *
010600000000     C                   END                                                    *
010700000000     C*--------- APERTURA NUOVO ESERCIZIO -----------------*
010800000000     C     SCE944        IFEQ      'X'                                          *
010900000000     C                   EXSR      RICDTA                                       *
011000860624     C                   OUT       MPHDT1                                       *
011100990422     C                   z-add     0             VAA944                         *
011200990422     C                   MOVE      ACOM          acom2             2 0          *
011300990422     C                   MOVE      ACOM2         VAA944                         *
011400990422     c                   if        acom2 > 40
011500990422     c                   add       1900          vaa944
011600990422     c                   else
011700990422     c                   add       2000          vaa944
011800990422     c                   end
011900000000     C     VAA944        ADD       1             NAA944                         *
012000000000     C     EMI944        TAG                                                    *
012100000000     C                   EXFMT     MPH944PT                                     *
012200990422     C   Kl              GOTO      INIZIO                                       *
012300000000     C  NKY              SETON                                            10    *
012400000000     C  NKY              GOTO      EMI944                                       *
012500000000     C   KY              DO                                                     *
012600860624     C                   MOVEL     PAR944        KPJBU                          *
012700990422     C                   MOVE      'I209'        KCOAZ                          *
012800970102     C                   MOVEL     'KSNOTTE'     KQEBI                          *
012900000104     C                   CALL      'BCH10'                                      *
013000860624     C                   PARM                    KPJBA                          *
013100000000     C                   END                                                    *
013200000000     C                   GOTO      INIZIO                                       *
013300000000     C                   END                                                    *
013400000000     C*---------- GENERAZIONE GIACENZA INIZIALE -----------*
013500000000     C     SCE945        IFEQ      'X'                                          *
013600000000     C                   EXSR      RICDTA                                       *
013700000000     C     AGEN          COMP      *BLANKS                                90    *ANNO COMPETENZ
013800000000     C   90              MOVEL     ERR(3)        ERRORE                         *
013900000000     C   90              GOTO      EMI940                                       *
014000990422     C                   MOVE      AGEN          Agen2             2 0          *
014100990422     C                   z-add     0             ANS945                         *
014200990422     C                   MOVE      AGEN2         ANS945                         *
014300990422     c                   if        agen2 > 40
014400990422     c                   add       1900          ans945
014500990422     c                   else
014600990422     c                   add       2000          ans945
014700990422     c                   end
014800990422     c
014900000000     C                   OUT       MPHDT1                                       *
015000000000     C                   MOVE      '?      '     MAG945                         *
015100000000     C     ANS945        ADD       1             DATDOC                         *
015200000000     C                   MOVEL     '0101'        DATDOC                         *
015300000000     C                   Z-ADD     1             NDC945                         *
015400990422     C                   MOVE      *BLANKS       DESLIS                         *
015500860624     C                   MOVE      '?  '         LIS945                         *
015600860103     C                   Z-ADD     0             COL945                         *
015700000000     C     EMI945        TAG                                                    *
015800000000     C                   EXFMT     MPH945PT                                     *
015900000000     C                   MOVEA     *ZEROS        *IN(1)                         *
016000000000     C                   MOVE      *BLANKS       ERRORE                         *
016100860103     C                   MOVE      *BLANKS       DESLIS                         *
016200990422     C   Kl              GOTO      INIZIO                                       *
016300000000     C     MAG945        IFNE      '9999999'                                    *
016400000000     C     MAG945        IFEQ      '?      '                                    *
016500000000     C                   MOVE      *BLANKS       MAG945                         *
016600000000     C                   MOVE      *BLANKS       DAT153                         *
016700000103     C                   CALL      'TII108R'                                    *
016800000000     C                   PARM                    MAG945                         *
016900990422     C                   PARM                    DAT153           75            *
017000000000     C                   GOTO      EMI945                                       *
017100000000     C                   END                                                    *
017200000000     C                   MOVE      '153'         TABGRU                         *
017300000000     C                   MOVE      *ZEROS        TABCOD                         *
017400000000     C                   MOVE      MAG945        TABCOD                         *
017500000000     C     KEYTAB        CHAIN     ANTAB00F                           90        *
017600860624     C  N90              MOVEL     TABDES        TB153                          *
017700860624     C  N90GIA153        COMP      'S'                                9090      *
017800000000     C   90              SETON                                            91    *
017900000000     C   90              MOVEL     ERR(4)        ERRORE                         *
018000000000     C   90              GOTO      EMI945                                       *
018100000000     C                   END                                                    *
018200860103     C*---------- RICERCA   LISTINO X VALORIZZAZIONE ------*
018300860103     C     LIS945        IFEQ      '?  '                                        *
018400860103     C                   MOVE      *BLANKS       LIS945                         *
018500860103     C                   MOVE      *BLANKS       DS161            75            *
018600000103     C                   CALL      'TII116R'                                    *
018700860103     C                   PARM                    LIS945                         *
018800860103     C                   PARM                    DS161                          *
018900860103     C                   MOVEL     DS161         DESLIS                         *
019000860103     C                   GOTO      EMI945                                       *
019100860103     C                   END                                                    *
019200860630     C*---------- GENERAZIONE GIACENZA INIZIALE --------2--*
019300860624     C*---------- CONTROLLO DATA DOCUMENTO ----------------*
019400990422     C                   MOVE      DATDOC        dataeur                        *
019500990422     C                   move      dataeur       dataiso                        *
019600990422     C                   move      dataiso       ddc945                         *
019700990422     C   90              SETON                                            92    *
019800000000     C   90              MOVEL     ERR(5)        ERRORE                         *
019900000000     C   90              GOTO      EMI945                                       *
020000000000     C*---------- CONTROLLO NUMERO DOCUMENTO               *
020100000000     C     NDC945        COMP      0                                      90    *
020200000000     C   90              SETON                                            93    *
020300000000     C   90              MOVEL     ERR(6)        ERRORE                         *
020400000000     C   90              GOTO      EMI945                                       *
020500860103     C*---------- RICERCA   LISTINO X VALORIZZAZIONE ------*
020600860103     C                   MOVE      '161'         TABGRU                         *
020700860103     C                   MOVE      *ZEROS        TABCOD                         *
020800860103     C                   MOVE      LIS945        TABCOD                         *
020900860103     C     KEYTAB        CHAIN     ANTAB00F                           90        *
021000860103     C  N90              MOVEL     TABDES        DESLIS                         *
021100860103     C  N90COL945        COMP      0                                    9090    *
021200860624     C   90              SETON                                        94        *
021300860103     C   90              MOVEL     ERR(21)       ERRORE                         *
021400860103     C   90              GOTO      EMI945                                       *
021500000000     C*---------- VERIFICA ANNO DI COMPETENZA :            *
021600000103     C                   movel     DDC945        AMV945                         *
021700000000     C     ANS945        IFGT      AMV945                                       *
021800000000     C                   SETON                                        9092      *
021900000000     C                   MOVEL     ERR(7)        ERRORE                         *
022000000000     C                   GOTO      EMI945                                       *
022100000000     C                   END                                                    *
022200000000     C*---------- IMPOSTO DATE GENERAZIONE :               *
022300990422     C                   MOVEL     *loval        DATaiso                        *
022400000104     C                   adddur    AMV945:*y     DATaiso                        *
022500000104     C                   subdur    1:*y          DATaiso                        *
022600000000     C   90              SETON                                            92    *
022700000000     C   90              MOVEL     ERR(5)        ERRORE                         *
022800000000     C   90              GOTO      EMI945                                       *
022900990422     C                   move      dataiso       DMV945                         *
023000000000     C                   Z-ADD     1             NMV945                         *
023100860630     C*---------- GENERAZIONE GIACENZA INIZIALE --------3--*
023200000000     C  NKY              SETON                                            10    *
023300000000     C  NKY              GOTO      EMI945                                       *
023400000000     C*---------- LANCIO GENERAZIONE GIACENZA INIZIALE ----*
023500000000     C                   MOVEL     PAR945        KPJBU                          *
023600990422     C                   MOVE      'I214'        KCOAZ                          *
023700970102     C                   MOVEL     'KSNOTTE'     KQEBI                          *
023800000104     C                   CALL      'BCH10'                                      *
023900000000     C                   PARM                    KPJBA                          *
024000000000     C                   GOTO      FINE                                         *
024100000000     C                   END                                                    *
024200000000     C*----------------------------------------------------*
024300000000     C     FINE          TAG                                                    *
024400000000     C                   SETON                                            LR    *
024500000000     C**----------RICERCA DATA AREA MAGAZZINO -----------***
024600000000     C***------------------------------------------------***
024700000000     CSR   RICDTA        BEGSR                                                  *
024800000000     C     SULOCK        TAG                                                    *
024900000000     C     *LOCK         IN        MPHDT1                               90      *
025000000000     C   90              GOTO      SULOCK                                       *
025100000000     C     NMOV          IFEQ      *BLANKS                                      *NUMERO MOVIMEN
025200000000     C                   MOVE      *ZEROS        NMOV                           *
025300000000     C                   END                                                    *
025400000000     C     DMOV          IFEQ      *BLANKS                                      *DATA   MOVIMEN
025500000000     C                   MOVEL     '010100'      DMOV                           *
025600000000     C                   MOVE      UYEAR         DMOV                           *
025700000000     C                   END                                                    *
025800000000     C     ACOM          IFEQ      *BLANKS                                      *ANNO COMPETENZ
025900000000     C                   MOVE      UYEAR         ACOM                           *
026000000000     C                   END                                                    *
026100000000     C     DGIO          IFEQ      *BLANKS                                      *DATA GIORNALE
026200000000     C                   MOVE      *ZEROS        DGIO                           *
026300000000     C                   END                                                    *
026400000000     C     DPER          IFEQ      *BLANKS                                      *DATA ELIMINAZ.
026500000000     C                   MOVE      *ZEROS        DPER                           *
026600000000     C                   END                                                    *
026700000000     C     AGEN          IFEQ      *BLANKS                                      *ANNO GEN.GIACI
026800000000     C                   MOVE      ACOM          AGEN                           *
026900000000     C                   END                                                    *
027000850415     C     AGEL          IFEQ      *BLANKS                                      *ANNO GEN.LIFO
027100850415     C                   MOVE      ACOM          AGEL                           *
027200850415     C                   END                                                    *
027300850527     C     NUMDB         IFEQ      *BLANKS                                      *NUM.RICH.D.BAS
027400850527     C                   MOVE      *ZEROS        NUMDB                          *
027500850527     C                   END                                                    *
027600850611     C     GMAX          IFEQ      *BLANKS                                      *GIORNI TOLLER.
027700850611     C                   MOVE      *ZEROS        GMAX                           *
027800850611     C                   END                                                    *
027900860630     C*----------------------------------------------------*
028000860630     C/EJECT
028100860630     C***------------------------------------------------***
028200860624     C     MASDTA        IFEQ      *BLANKS                                      *MASTER PROCED.
028300860624     C                   MOVE      '***'         MASDTA                         *
028400860624     C                   END                                                    *
028500860624     C     ADBDTA        IFEQ      *BLANKS                                      *S/N  ADB
028600860624     C                   MOVE      'N'           ADBDTA                         *
028700860624     C                   END                                                    *
028800851217     C     NUMIN         IFEQ      *BLANKS                                      *NUMERAT.INVENT
028900851217     C                   MOVE      *ZEROS        NUMIN                          *
029000851217     C                   END                                                    *
029100000000     CSR                 ENDSR                                                  *
029200990422     C*----------------------------------------------------*
029300990422     C* Reperimento dati società
029400990422     C*----------------------------------------------------*
029500990422     C     REPSOC        BEGSR
029600990422     C*
029700990422     C                   CALLB     'XSOC'
029800990422     C                   PARM                    TIPXSC            6
029900990422     C                   PARM                    SOCXSC            3
030000990422     C                   PARM                    CDSXSC            9 0
030100990422     C                   PARM                    MODXSC            3
030200990422     C                   PARM      *blanks       RTNXSC            1
030300990422     C                   PARM                    XSOCDS
030400990422     C                   PARM                    KPJBA
030500990422     C*
030600990422     C                   ENDSR
030700000000     C***------------------------------------------------***
030800860103**    ERR                       ERRORI
030900851217*- Cod. DITTA NON VALIDO !! -*                    ERR,1
031000851217*- Eseguire SCELTA Lavoro   -*                    ERR,2
031100851217*-Eseguire Impost.Numeratori-*                    ERR,3
031200851217*- Cod. Magazzino  ERRATO !!-*                    ERR,4
031300851217*- Controllare DATE !!!!!!!!-*                    ERR,5
031400851217*- Inserire Numero Documento-*                    ERR,6
031500851217*- Anno SALDI > Anno MOVIM. -*                    ERR,7
031600851217*-                          -*                    ERR,8
031700851217*-                          -*                    ERR,9
031800851217*-                          -*                    ERR,10
03190085121700000<->000000<->00<--Anno di Competenza..........ERR,11
032000851217000000<--Data Ultima Stampa Giornale di Magazzino.ERR,12
032100851217000000<--Data Eliminazione  Periodica Movimenti...ERR,13
03220085121700<------Anno Gener.Movim.Giacenza Iniziale.......ERR,14
03230085121700<------Anno Generazione L.I.F.O. ...............ERR,15
03240085121700000<---Num.Progress.Richiami D.BASE x MPH111R...ERR,16
03250085121700<------Max.Giorni Tolleranza Variaz.Data Movim..ERR,17
032600860624<-----Master Procedura<->   0<--Personalizz. ADB..ERR,18
03270085121700000<---Numeratore Fogli Rilevazione InventarialeERR,19
03280085121700000000000000000000000000000000000000000000000000ERR,20
032900860103*- Listino/Col.  ERRATO   !!-*                    ERR,21
