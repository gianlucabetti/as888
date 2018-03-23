000100020510     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200020510
000300020516
000400020516     D TrgInsert       C                   '1'
000500020516     D TrgDelete       C                   '2'
000600020516     D TrgUpdate       C                   '3'
000700020516     D
000800020516     D CodTrgInsert    C                   'I'
000900020516     D CodTrgDelete    C                   'D'
001000020516     D CodTrgUpdate    C                   'U'
001100020516
001200020516     D After           C                   '1'
001300020516     D Before          C                   '2'
001400020607
001500030529     D OriRec          S           1000    VARYING INZ(' ')
001600030529     D NewRec          S           1000    VARYING INZ(' ')
001700020516
001800030529     D TIS799DSI     E DS
001900030529     D DS$KUN        E DS
002000030529     D BDS$KUN       E DS                  EXTNAME(DS$KUN)
002100030529     D                                     PREFIX(B)
002200020516
002300020515     D PAR1            DS
002400020510     D  File                         10
002500020510     D  Library                      10
002600020510     D  Member                       10
002700020510     D  TriggerEvent                  1
002800020510     D  TriggerTime                   1
002900020510     D  CommitLock                    1
003000020513     D  Reserved7                     3
003100020513     D  CCSIDofData                  10I 0
003200020513     D  RRN                          10I 0
003300020513     D  Reserved9                     4
003400020513     D  OriRecOffset                 10I 0
003500020513     D  OriRecLength                 10I 0
003600020513     D  OriRecNBMO                   10I 0
003700020513     D  OriRecNBML                   10I 0
003800020513     D  NewRecOffset                 10I 0
003900020513     D  NewRecLength                 10I 0
004000020513     D  NewRecNBMO                   10I 0
004100020513     D  NewRecNBML                   10I 0
004200020513     D  Reserved16                   16
004300021128     D  BufferDati                 5000    VARYING
004400020510
004500020515     D PAR2            DS
004600020515     D  PAR1Length                   10I 0
004700020515
004800020515     D* Reperimento dati del PGM
004900020515     D PGMSTATUS      SDS
005000020515     D JOB_NAME              244    253
005100020515     D USER                  254    263
005200020515     D JOB_NUM               264    269S 0
005300020515     D JOB_DATE              270    275S 0
005400020515     D RUN_DATE              276    281S 0
005500020515     D RUN_TIME              282    287S 0
005600020515     D PROC_PGM              334    343
005700020618
005800020618
005900020618     D******************************************************************
006000020618     D*Record structure for QUSRJOBI JOBI0200 format
006100020618     D******************************************************************
006200020618     DQUSI020000       DS
006300020618     D*                                             Qwc JOBI0200
006400020618     D QUSBR01                 1      4B 0
006500020618     D*                                             Bytes Return
006600020618     D QUSBA01                 5      8B 0
006700020618     D*                                             Bytes Avail
006800020618     D QUSJN03                 9     18
006900020618     D*                                             Job Name
007000020618     D QUSUN03                19     28
007100020618     D*                                             User Name
007200020618     D QUSJNBR03              29     34
007300020618     D*                                             Job Number
007400020618     D QUSIJID01              35     50
007500020618     D*                                             Int Job ID
007600020618     D QUSJS05                51     60
007700020618     D*                                             Job Status
007800020618     D QUSJT04                61     61
007900020618     D*                                             Job Type
008000020618     D QUSJS06                62     62
008100020618     D*                                             Job Subtype
008200020618     D QUSSN                  63     72
008300020618     D*                                             Subsys Name
008400020618     D QUSRP01                73     76B 0
008500020618     D*                                             Run Priority
008600020618     D QUSSPID00              77     80B 0
008700020618     D*                                             System Pool ID
008800020618     D QUSCPUU00              81     84B 0
008900020618     D*                                             CPU Used
009000020618     D QUSAIOR                85     88B 0
009100020618     D*                                             Aux IO Request
009200020618     D QUSIT                  89     92B 0
009300020618     D*                                             Interact Trans
009400020618     D QUSRT                  93     96B 0
009500020618     D*                                             Response Time
009600020618     D QUSFT                  97     97
009700020618     D*                                             Function Type
009800020618     D QUSFN15                98    107
009900020618     D*                                             Function Name
010000020618     D QUSAJS                108    111
010100020618     D*                                             Active Job Stat
010200020618     D QUSNDBLW              112    115B 0
010300020618     D*                                             Num DBase Lock Wts
010400020618     D QUSNIMLW              116    119B 0
010500020618     D*                                             Num Internal Mch Lck Wts
010600020618     D QUSNDBLW00            120    123B 0
010700020618     D*                                             Num Non DBase Lock Wts
010800020618     D QUSTDBLW              124    127B 0
010900020618     D*                                             Wait Time DBase Lock Wts
011000020618     D QUSTIMLW              128    131B 0
011100020618     D*                                             Wait Time Internal Mch Lck W
011200020618     D QUSNDBLW01            132    135B 0
011300020618     D*                                             Wait Time Non DBase Lock Wts
011400020618     D QUSERVED45            136    136
011500020618     D*                                             Reserved
011600020618     D QUSCSPID              137    140B 0
011700020618     D*                                             Current System Pool ID
011800020618     D QUSTC01               141    144B 0
011900020618     D*                                             Thread Count
012000020515
012100030529     D TipOpe          S              1
012200020618
012300020618     D ReceiveVar      S            200
012400020618     D LenReceiveVar   S              9B 0 INZ(%size(ReceiveVar))
012500020618     D FormatName      S              8
012600020618     D QualJobName     S             26
012700020618     D InternalJobID   S             16
012800020510
012900020510
013000020510     C     *ENTRY        PLIST
013100020515     C                   PARM                    PAR1
013200020515     C                   PARM                    PAR2
013300021125     C*
013400021125     C* Innanzitutto valorizzo le due immagini PRIMA e DOPO
013500021125     C                   EVAL      OriRec =
013600021125     C                             %subst(PAR1:OriRecOffset+1:OriRecLength)
013700021125     C                   EVAL      NewRec =
013800021125     C                             %subst(PAR1:NewRecOffset+1:NewRecLength)
013900020510
014000020614     C*===============================================================
014100020614     C* INNANZI TUTTO VERIFICO SE LE DUE IMMAGINI "PRIMA" E "DOPO"
014200020614     C* SONO DIVERSE ALTRIMENTI E' INUTILE PROCEDERE.
014300030529 IF 1C                   IF        OriRec <> NewRec
014400020521     C*===============================================================
014500020521     C*
014600040902     C* Se sono in UNITRAGRU effettuo altre operazioni specifiche
014700030529 IF 2C                   IF        File = 'TIKUN00F'     AND
014800040902     C                             Library = 'UNITRAGRU'
014900020521     C*
015000020521     C* Elaboro solo se trattasi di chiamata trigger su *AFTER
015100030529 IF 3C                   IF        TriggerTime = After
015200020521     C*
015300020617     C* Trascodifico il tipo di operazione avvenuta sul file
015400020521     C                   IF        TriggerEvent = TrgInsert
015500020521     C                   EVAL      TipOpe = CodTrgInsert
015600020521     C                   ENDIF
015700020521     C                   IF        TriggerEvent = TrgDelete
015800020521     C                   EVAL      TipOpe = CodTrgDelete
015900020521     C                   ENDIF
016000020521     C                   IF        TriggerEvent = TrgUpdate
016100020521     C                   EVAL      TipOpe = CodTrgUpdate
016200020521     C                   ENDIF
016300020610     C*
016400020610     C* A seconda del tipo di operazione sul file gestisco la valorizzaz. della
016500020610     C* DS. Questo xchè il trigger passa nelle immagini vuote nn valori "loval"
016600020610     C* x tipo campo (es.: *blanks x gli ALFA e *zeros x i NUMERICI) ma il
016700020610     C* valore *null.
016800030529     C*
016900030529     C* MODIFICA
017000020610     C                   IF        TipOpe = 'U'
017100030529     C                   MOVEL(P)  OriRec        BDS$KUN
017200030529     C                   MOVEL(P)  NewRec        DS$KUN
017300030529     C     BDK$TLE       IFEQ      'ST'
017400030529     C     DK$TLE        ANDEQ     'ST'
017500030529     C                   ELSE
017600091117     C                   IF        BDK$COP <> DK$COP
017700091117     C                   CLEAR                   TIS799DSI
017800091117     C                   MOVE      BDK$COP       KSUI99
017900091117     C                   MOVE      BDK$COF       CLII99
018000091117     C                   CALL      'TIS7I2C'
018100091117     C                   PARM                    TIS799DSI
018200091117     C                   ENDIF
018300030529     C                   IF        (DK$VAT = BDK$VAT) AND
018400030529     C                             (DK$AZP = BDK$AZP) AND
018500030529     C                             (DK$COP = BDK$COP) AND
018600030529     C                             (DK$AZF = BDK$AZF) AND
018700030529     C                             (DK$COF = BDK$COF) AND
018800030529     C                             (DK$TLE = BDK$TLE) AND
018900030529     C                             (DK$DDE = BDK$DDE) AND
019000030529     C                             (DK$DSC = BDK$DSC)
019100030529     C                   ELSE
019200030529     C                   CLEAR                   TIS799DSI
019300030529     C                   MOVE      DK$COP        KSUI99
019400030529     C                   MOVE      DK$COF        CLII99
019500030529     C                   CALL      'TIS7I2C'
019600030529     C                   PARM                    TIS799DSI
019700030529     C                   ENDIF
019800030529     C                   ENDIF
019900030529     C                   ENDIF
020000030529     C*
020100030529     C* INSERIMENTO
020200030529     C                   IF        TipOpe = 'I'
020300030529     C                   MOVEL(P)  NewRec        DS$KUN
020400030529     C     DK$TLE        IFEQ      'ST'
020500030529     C                   ELSE
020600030529     C                   CLEAR                   TIS799DSI
020700030529     C                   MOVE      DK$COP        KSUI99
020800030529     C                   MOVE      DK$COF        CLII99
020900030529     C                   CALL      'TIS7I2C'
021000030529     C                   PARM                    TIS799DSI
021100020610     C                   ENDIF
021200030529     C                   ENDIF
021300030529     C*
021400030529     C* CANCELLAZIONE
021500020610     C                   IF        TipOpe = 'D'
021600030529     C                   MOVEL(P)  OriRec        BDS$KUN
021700030529     C     BDK$TLE       IFEQ      'ST'
021800030529     C                   ELSE
021900030529     C                   CLEAR                   TIS799DSI
022000030529     C                   MOVE      BDK$COP       KSUI99
022100030529     C                   MOVE      BDK$COF       CLII99
022200030529     C                   CALL      'TIS7I2C'
022300030529     C                   PARM                    TIS799DSI
022400020610     C                   ENDIF
022500030529     C                   ENDIF
022600020521     C*
022700030529 E  3C                   ENDIF
022800030529 E  2C                   ENDIF
022900020617     C*===============================================================
023000020617     C*
023100030529     C* Se sono in PROVA effettuo altre operazioni specifiche
023200030529 IF 3C                   IF        (File = 'TIKUN00F'       AND
023300040902     C                              Library = 'UNITRAGRP')
023400030530     C***  Library       DSPLY
023500030529 E  3C                   ENDIF
023600020614     C*
023700020614     C*===============================================================
023800030529 E  1C                   ENDIF
023900020614     C*===============================================================
024000020607     C*
024100020618     C* Infine x decidere il tipo di chiusura testo il tipo di job: se
024200020618     C* interattivo chiudo in "RT", altrimenti in "LR".
024300020618     C                   IF        QUSJT04 = 'I'
024400020618     C                   EVAL      *INLR = *ON
024500020618     C                   ELSE
024600020618     C                   EVAL      *INRT = *ON
024700020618     C                   ENDIF
024800020607     C*------------------------------------------------------------------------*
024900020607     C* *INZSR - OPERAZIONI INIZIALI
025000020607     C*------------------------------------------------------------------------*
025100020607     C     *INZSR        BEGSR
025200020618     C*
025300020618     C* Chiamo il *PGM x reperimento informazoni sul job
025400020618     C                   CALL      'QUSRJOBI'
025500020618     C                   PARM                    ReceiveVar
025600020618     C                   PARM                    LenReceiveVar
025700020618     C                   PARM      'JOBI0200'    FormatName
025800020618     C                   PARM      '*'           QualJobName
025900020618     C                   PARM                    InternalJobID
026000020607     C*
026100020618     C                   MOVEL(P)  ReceiveVar    QUSI020000
026200020618     C*
026300020607     C                   ENDSR
