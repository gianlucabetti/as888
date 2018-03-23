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
001900030530     D DS$VSS        E DS
002000030708     D DSVSS_ORI     E DS                  EXTNAME(TIVSS00F) PREFIX(Ori_)
002100030708     D DSVSS_NEW     E DS                  EXTNAME(TIVSS00F) PREFIX(New_)
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
013900030708     C*
014000030708     C* Valorizzo le DS x ridefinire le immagini "Prima" e "Dopo"
014100030708     C                   EVAL      DSVSS_ORI = OriRec
014200030708     C                   EVAL      DSVSS_NEW = NewRec
014300030708     C
014400020510
014500020614     C*===============================================================
014600020614     C* INNANZI TUTTO VERIFICO SE LE DUE IMMAGINI "PRIMA" E "DOPO"
014700020614     C* SONO DIVERSE ALTRIMENTI E' INUTILE PROCEDERE.
014800030529 IF 1C                   IF        OriRec <> NewRec
014900030708     C* E VERIFICO IL TIPO SERVIZIO DI ALMENO UNA DELLE DUE IMMAGINI "PRIMA" E "DOPO"
015000030708     C* SIA "TT" => ALTRIMENTI NON BISOGNA PROCEDERE.
015100030708 IF 2C                   IF        Ori_VSSISV = 'TT' OR
015200030708     C                             New_VSSISV = 'TT'
015300020521     C*===============================================================
015400020521     C*
015500020521     C* Se sono in GAITRAGRU effettuo altre operazioni specifiche
015600050207 IF 3C                   IF        (File = 'TIVSS00F'      AND
015700050207     C                              Library = 'GAITRAGRU') OR
015800050207     C                             (File = 'TIVSS00F'      AND
015900050207     C                              Library = 'UNITRAGRU')
016000020521     C*
016100020521     C* Elaboro solo se trattasi di chiamata trigger su *AFTER
016200030708 IF 4C                   IF        TriggerTime = After
016300020521     C*
016400020617     C* Trascodifico il tipo di operazione avvenuta sul file
016500020521     C                   IF        TriggerEvent = TrgInsert
016600020521     C                   EVAL      TipOpe = CodTrgInsert
016700020521     C                   ENDIF
016800020521     C                   IF        TriggerEvent = TrgDelete
016900020521     C                   EVAL      TipOpe = CodTrgDelete
017000020521     C                   ENDIF
017100020521     C                   IF        TriggerEvent = TrgUpdate
017200020521     C                   EVAL      TipOpe = CodTrgUpdate
017300020521     C                   ENDIF
017400020610     C*
017500020610     C* A seconda del tipo di operazione sul file gestisco la valorizzaz. della
017600020610     C* DS. Questo xchè il trigger passa nelle immagini vuote nn valori "loval"
017700020610     C* x tipo campo (es.: *blanks x gli ALFA e *zeros x i NUMERICI) ma il
017800020610     C* valore *null.
017900030529     C*
018000030529     C* MODIFICA
018100020610     C                   IF        TipOpe = 'U'
018101080623     C*
018102080623     C* Se la modifica coinvolge il codice padre => gestisco come D+I in quanto
018103080623     C* occorre PRIMA cancellare i record del vecchio codice
018104080623     C                   IF        Ori_VSSKSU <> New_VSSKSU
018106080623     C                   MOVEL(P)  OriRec        DS$VSS
018107080623     C                   CLEAR                   TIS799DSI
018108080623     C                   MOVEL     DV$SUN        SUNI99
018109080623     C                   MOVEL     DV$KSU        KSUI99
018110080623     C                   CALL      'TIS7I2C'
018111080623     C                   PARM                    TIS799DSI
018114080623     C                   ENDIF
018115080623     C*
018200030530     C                   MOVEL(P)  NewRec        DS$VSS
018300030529     C                   CLEAR                   TIS799DSI
018400030530     C                   MOVEL     DV$SUN        SUNI99
018500030530     C                   MOVEL     DV$KSU        KSUI99
018600030529     C                   CALL      'TIS7I2C'
018700030529     C                   PARM                    TIS799DSI
018800030529     C                   ENDIF
018900030529     C*
019000030529     C* INSERIMENTO
019100030529     C                   IF        TipOpe = 'I'
019200030530     C                   MOVEL(P)  NewRec        DS$VSS
019300030529     C                   CLEAR                   TIS799DSI
019400030530     C                   MOVEL     DV$SUN        SUNI99
019500030530     C                   MOVEL     DV$KSU        KSUI99
019600030529     C                   CALL      'TIS7I2C'
019700030529     C                   PARM                    TIS799DSI
019800030529     C                   ENDIF
019900030529     C*
020000030529     C* CANCELLAZIONE
020100020610     C                   IF        TipOpe = 'D'
020200030530     C                   MOVEL(P)  OriRec        DS$VSS
020300030529     C                   CLEAR                   TIS799DSI
020400030530     C                   MOVEL     DV$SUN        SUNI99
020500030530     C                   MOVEL     DV$KSU        KSUI99
020600030529     C                   CALL      'TIS7I2C'
020700030529     C                   PARM                    TIS799DSI
020800030529     C                   ENDIF
020900020521     C*
021000030708 E  4C                   ENDIF
021100030708 E  3C                   ENDIF
021200020617     C*===============================================================
021300020617     C*
021400030529     C* Se sono in PROVA effettuo altre operazioni specifiche
021500030708 IF 3C                   IF        (File = 'TIVSS00F'       AND
021600021128     C                              Library = 'GAITRAGRPS')
021700030530     C***  Library       DSPLY
021800030708 E  3C                   ENDIF
021900020614     C*
022000030708     C*===============================================================
022100030708 E  2C                   ENDIF
022200030529 E  1C                   ENDIF
022300020614     C*===============================================================
022400020607     C*
022500020618     C* Infine x decidere il tipo di chiusura testo il tipo di job: se
022600020618     C* interattivo chiudo in "RT", altrimenti in "LR".
022700020618     C                   IF        QUSJT04 = 'I'
022800020618     C                   EVAL      *INLR = *ON
022900020618     C                   ELSE
023000020618     C                   EVAL      *INRT = *ON
023100020618     C                   ENDIF
023200020607     C*------------------------------------------------------------------------*
023300020607     C* *INZSR - OPERAZIONI INIZIALI
023400020607     C*------------------------------------------------------------------------*
023500020607     C     *INZSR        BEGSR
023600020618     C*
023700020618     C* Chiamo il *PGM x reperimento informazoni sul job
023800020618     C                   CALL      'QUSRJOBI'
023900020618     C                   PARM                    ReceiveVar
024000020618     C                   PARM                    LenReceiveVar
024100020618     C                   PARM      'JOBI0200'    FormatName
024200020618     C                   PARM      '*'           QualJobName
024300020618     C                   PARM                    InternalJobID
024400020607     C*
024500020618     C                   MOVEL(P)  ReceiveVar    QUSI020000
024600020618     C*
024700020607     C                   ENDSR
