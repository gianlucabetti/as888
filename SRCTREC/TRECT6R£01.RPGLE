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
001500020705     D OURPGM          S             10    DIM(13) CTDATA PERRCD(1)
001600020607     D SKSTACK         S             10    DIM(1000)
001700021128     D OriRec          S           1000    VARYING INZ(' ')
001800021128     D NewRec          S           1000    VARYING INZ(' ')
001900020516
002000020617     D Ptncsb00ds    E DS
002100020617     D                                     EXTNAME(TNCSB00F)
002200020617     D                                     INZ
002300020617     D                                     PREFIX(P_)
002400020617     D Dtncsb00ds    E DS
002500020617     D                                     EXTNAME(TNCSB00F)
002600020617     D                                     INZ
002700020617     D                                     PREFIX(D_)
002800020516
002900020516     D DS£ECO        E DS
003000020516
003100020515     D PAR1            DS
003200020510     D  File                         10
003300020510     D  Library                      10
003400020510     D  Member                       10
003500020510     D  TriggerEvent                  1
003600020510     D  TriggerTime                   1
003700020510     D  CommitLock                    1
003800020513     D  Reserved7                     3
003900020513     D  CCSIDofData                  10I 0
004000020513     D  RRN                          10I 0
004100020513     D  Reserved9                     4
004200020513     D  OriRecOffset                 10I 0
004300020513     D  OriRecLength                 10I 0
004400020513     D  OriRecNBMO                   10I 0
004500020513     D  OriRecNBML                   10I 0
004600020513     D  NewRecOffset                 10I 0
004700020513     D  NewRecLength                 10I 0
004800020513     D  NewRecNBMO                   10I 0
004900020513     D  NewRecNBML                   10I 0
005000020513     D  Reserved16                   16
005100021128     D  BufferDati                 5000    VARYING
005200020510
005300020515     D PAR2            DS
005400020515     D  PAR1Length                   10I 0
005500020515
005600020515     D* Reperimento dati del PGM
005700020515     D PGMSTATUS      SDS
005800020515     D JOB_NAME              244    253
005900020515     D USER                  254    263
006000020515     D JOB_NUM               264    269S 0
006100020515     D JOB_DATE              270    275S 0
006200020515     D RUN_DATE              276    281S 0
006300020515     D RUN_TIME              282    287S 0
006400020515     D PROC_PGM              334    343
006500020618
006600020618
006700020618     D******************************************************************
006800020618     D*Record structure for QUSRJOBI JOBI0200 format
006900020618     D******************************************************************
007000020618     DQUSI020000       DS
007100020618     D*                                             Qwc JOBI0200
007200020618     D QUSBR01                 1      4B 0
007300020618     D*                                             Bytes Return
007400020618     D QUSBA01                 5      8B 0
007500020618     D*                                             Bytes Avail
007600020618     D QUSJN03                 9     18
007700020618     D*                                             Job Name
007800020618     D QUSUN03                19     28
007900020618     D*                                             User Name
008000020618     D QUSJNBR03              29     34
008100020618     D*                                             Job Number
008200020618     D QUSIJID01              35     50
008300020618     D*                                             Int Job ID
008400020618     D QUSJS05                51     60
008500020618     D*                                             Job Status
008600020618     D QUSJT04                61     61
008700020618     D*                                             Job Type
008800020618     D QUSJS06                62     62
008900020618     D*                                             Job Subtype
009000020618     D QUSSN                  63     72
009100020618     D*                                             Subsys Name
009200020618     D QUSRP01                73     76B 0
009300020618     D*                                             Run Priority
009400020618     D QUSSPID00              77     80B 0
009500020618     D*                                             System Pool ID
009600020618     D QUSCPUU00              81     84B 0
009700020618     D*                                             CPU Used
009800020618     D QUSAIOR                85     88B 0
009900020618     D*                                             Aux IO Request
010000020618     D QUSIT                  89     92B 0
010100020618     D*                                             Interact Trans
010200020618     D QUSRT                  93     96B 0
010300020618     D*                                             Response Time
010400020618     D QUSFT                  97     97
010500020618     D*                                             Function Type
010600020618     D QUSFN15                98    107
010700020618     D*                                             Function Name
010800020618     D QUSAJS                108    111
010900020618     D*                                             Active Job Stat
011000020618     D QUSNDBLW              112    115B 0
011100020618     D*                                             Num DBase Lock Wts
011200020618     D QUSNIMLW              116    119B 0
011300020618     D*                                             Num Internal Mch Lck Wts
011400020618     D QUSNDBLW00            120    123B 0
011500020618     D*                                             Num Non DBase Lock Wts
011600020618     D QUSTDBLW              124    127B 0
011700020618     D*                                             Wait Time DBase Lock Wts
011800020618     D QUSTIMLW              128    131B 0
011900020618     D*                                             Wait Time Internal Mch Lck W
012000020618     D QUSNDBLW01            132    135B 0
012100020618     D*                                             Wait Time Non DBase Lock Wts
012200020618     D QUSERVED45            136    136
012300020618     D*                                             Reserved
012400020618     D QUSCSPID              137    140B 0
012500020618     D*                                             Current System Pool ID
012600020618     D QUSTC01               141    144B 0
012700020618     D*                                             Thread Count
012800020515
012900020605     D TipOPe          S              1
013000020618
013100020618     D ReceiveVar      S            200
013200020618     D LenReceiveVar   S              9B 0 INZ(%size(ReceiveVar))
013300020618     D FormatName      S              8
013400020618     D QualJobName     S             26
013500020618     D InternalJobID   S             16
013600020510
013700020510
013800020510     C     *ENTRY        PLIST
013900020515     C                   PARM                    PAR1
014000020515     C                   PARM                    PAR2
014100021125     C*
014200021125     C* Innanzitutto valorizzo le due immagini PRIMA e DOPO
014300021125     C                   EVAL      OriRec =
014400021125     C                             %subst(PAR1:OriRecOffset+1:OriRecLength)
014500021125     C                   EVAL      NewRec =
014600021125     C                             %subst(PAR1:NewRecOffset+1:NewRecLength)
014700020510
014800020614     C*===============================================================
014900020614     C* INNANZI TUTTO VERIFICO SE LE DUE IMMAGINI "PRIMA" E "DOPO"
015000020614     C* SONO DIVERSE ALTRIMENTI E' INUTILE PROCEDERE.
015100020617     C                   IF        OriRec <> NewRec
015200020521     C*===============================================================
015300020521     C*
015400020521     C* Se sono in GAITRAGRU effettuo altre operazioni specifiche
015500020617     C                   IF        File = 'TNCSB00F'     AND
015600020617     C                             Library = 'GAITRAGRU'
015700020521     C*
015800020521     C* Elaboro solo se trattasi di chiamata trigger su *AFTER
015900020617     C                   IF        TriggerTime = After
016000020521     C*
016100020617     C* Trascodifico il tipo di operazione avvenuta sul file
016200020521     C                   IF        TriggerEvent = TrgInsert
016300020521     C                   EVAL      TipOpe = CodTrgInsert
016400020521     C                   ENDIF
016500020521     C                   IF        TriggerEvent = TrgDelete
016600020521     C                   EVAL      TipOpe = CodTrgDelete
016700020521     C                   ENDIF
016800020521     C                   IF        TriggerEvent = TrgUpdate
016900020521     C                   EVAL      TipOpe = CodTrgUpdate
017000020521     C                   ENDIF
017100020521     C*
017200020521     C* Compongo la DS DS£ECO da scrivere nella apposita DTAQ
017300020521     C                   CLEAR                   DS£ECO
017400020731     C                   EVAL      D£GTJB = QUSJT04
017500020521     C                   EVAL      D£GSTS = TipOpe
017600020610     C*
017700020610     C* Giro la data da formato MMGGAA a formato AAMMGG
017800020610     C                   MOVEL(P)  RUN_DATE      WRKDATE           6
017900020610     C                   EVAL      WRKDATE = %subst(WRKDATE:5:2) +
018000020610     C                                       %subst(WRKDATE:1:2) +
018100020610     C                                       %subst(WRKDATE:3:2)
018200020610     C                   MOVEL     WRKDATE       D£GDAT
018300020521     C                   EVAL      D£GTIM = RUN_TIME
018400020521     C                   EVAL      D£GUSR = USER
018500020521     C                   EVAL      D£GPGF = depStack
018600020521     C                   EVAL      D£GFIL = File
018700020521     C                   EVAL      D£GMBR = Member
018800020521     C                   EVAL      D£GLIB = Library
018900020610     C*
019000020610     C* A seconda del tipo di operazione sul file gestisco la valorizzaz. della
019100020610     C* DS. Questo xchè il trigger passa nelle immagini vuote nn valori "loval"
019200020610     C* x tipo campo (es.: *blanks x gli ALFA e *zeros x i NUMERICI) ma il
019300020610     C* valore *null.
019400020610     C                   IF        TipOpe = 'U'
019500020617     C                   EVAL      D£PRE1 = %subst(OriRec:001:214)
019600020617     C                   EVAL      D£DRE1 = %subst(NewRec:001:214)
019700020610     C                   ENDIF
019800020610     C                   IF        TipOpe = 'I'
019900020617     C                   EVAL      D£DRE1 = %subst(NewRec:001:214)
020000020610     C                   ENDIF
020100020610     C                   IF        TipOpe = 'D'
020200020617     C                   EVAL      D£PRE1 = %subst(OriRec:001:214)
020300020610     C                   ENDIF
020400020521     C*
020500020612     C                   CALL      'TRECD2R'
020600020521     C                   PARM                    DS£ECO
020700020521     C                   ENDIF
020800020521     C*
020900020521     C                   ENDIF
021000020617     C*
021100020617     C*===============================================================
021200020617     C*
021300021125     C* Se sono in EDPFG effettuo altre operazioni specifiche
021400021128     C                   IF        (File = 'TESTRG0F'       AND
021500021128     C                              Library = 'EDPFG')      OR
021600021128     C                             (File = 'TNCSB00F'       AND
021700021128     C                              Library = 'GAITRAGRPS')
021800021125     C*
021900021125     C* Valorizzo la DS del file
022000021125     C                   EXSR      VALDS
022100020617     C*
022200020617     C* Elaboro solo se trattasi di chiamata trigger su *AFTER
022300020617     C                   IF        TriggerTime = After
022400020617     C*
022500020617     C* Trascodifico il tipo di operazione avvenuta sul file
022600020617     C                   IF        TriggerEvent = TrgInsert
022700020617     C                   EVAL      TipOpe = CodTrgInsert
022800020617     C                   ENDIF
022900020617     C                   IF        TriggerEvent = TrgDelete
023000020617     C                   EVAL      TipOpe = CodTrgDelete
023100020617     C                   ENDIF
023200020617     C                   IF        TriggerEvent = TrgUpdate
023300020617     C                   EVAL      TipOpe = CodTrgUpdate
023400020617     C                   ENDIF
023500020617     C*
023600020617     C* Compongo la DS DS£ECO da scrivere nella apposita DTAQ
023700020617     C                   CLEAR                   DS£ECO
023800020731     C                   EVAL      D£GTJB = QUSJT04
023900020617     C                   EVAL      D£GSTS = TipOpe
024000020617     C*
024100020617     C* Giro la data da formato MMGGAA a formato AAMMGG
024200020617     C                   MOVEL(P)  RUN_DATE      WRKDATE           6
024300020617     C                   EVAL      WRKDATE = %subst(WRKDATE:5:2) +
024400020617     C                                       %subst(WRKDATE:1:2) +
024500020617     C                                       %subst(WRKDATE:3:2)
024600020617     C                   MOVEL     WRKDATE       D£GDAT
024700020617     C                   EVAL      D£GTIM = RUN_TIME
024800020617     C                   EVAL      D£GUSR = USER
024900020617     C                   EVAL      D£GPGF = depStack
025000020617     C                   EVAL      D£GFIL = File
025100020617     C                   EVAL      D£GMBR = Member
025200020617     C                   EVAL      D£GLIB = Library
025300020617     C*
025400020617     C* A seconda del tipo di operazione sul file gestisco la valorizzaz. della
025500020617     C* DS. Questo xchè il trigger passa nelle immagini vuote nn valori "loval"
025600020617     C* x tipo campo (es.: *blanks x gli ALFA e *zeros x i NUMERICI) ma il
025700020617     C* valore *null.
025800020617     C                   IF        TipOpe = 'U'
025900020617     C                   EVAL      D£PRE1 = %subst(OriRec:001:214)
026000020617     C                   EVAL      D£DRE1 = %subst(NewRec:001:214)
026100020617     C                   ENDIF
026200020617     C                   IF        TipOpe = 'I'
026300020617     C                   EVAL      D£DRE1 = %subst(NewRec:001:214)
026400020617     C                   ENDIF
026500020617     C                   IF        TipOpe = 'D'
026600020617     C                   EVAL      D£PRE1 = %subst(OriRec:001:214)
026700020617     C                   ENDIF
026800020617     C*
026900020617     C                   CALL      'TRECD2R'
027000020617     C                   PARM                    DS£ECO
027100020617     C                   ENDIF
027200020617     C*
027300020617     C                   ENDIF
027400020614     C*
027500020614     C*===============================================================
027600020617     C                   ENDIF
027700020614     C*===============================================================
027800020607     C*
027900020618     C* Infine x decidere il tipo di chiusura testo il tipo di job: se
028000020618     C* interattivo chiudo in "RT", altrimenti in "LR".
028100020618     C                   IF        QUSJT04 = 'I'
028200020618     C                   EVAL      *INLR = *ON
028300020618     C                   ELSE
028400020618     C                   EVAL      *INRT = *ON
028500020618     C                   ENDIF
028600020617     C*------------------------------------------------------------------------*
028700020617     C* VALDS - IMPOSTA LE DS A SECONDA DEL FILE A CUI SI RIFERISCONO
028800020617     C*------------------------------------------------------------------------*
028900020617     C     VALDS         BEGSR
029000020617     C*
029100020617     C                   EVAL      Ptncsb00ds = OriRec
029200020617     C                   EVAL      Dtncsb00ds = NewRec
029300020617     C*
029400020617     C                   ENDSR
029500020607     C*------------------------------------------------------------------------*
029600020607     C* *INZSR - OPERAZIONI INIZIALI
029700020607     C*------------------------------------------------------------------------*
029800020607     C     *INZSR        BEGSR
029900020607     C*
030000020607     C* Chiamo il *PGM x reperimento stack di richiami del job
030100020612     C                   CALL      'XRTVSTKR'
030200020607     C                   PARM                    PStack         1000
030300020607     C                   PARM                    PStartPoint       3 0
030400020607     C                   PARM                    PEntries          3 0
030500020607     C*
030600020607     C* Cerco nello stack di richiami i nostroi pgm da monitorare
030700020607     C                   MOVEL     *blanks       depStack         10
030800020607     C                   MOVEL     *blanks       wk1Stack         10
030900020607     C                   MOVEL     *blanks       wk2Stack         10
031000020607     C                   MOVEA     PStack        SKSTACK
031100020607     C                   Z-ADD     1             i                 2 0
031200020607     C* Ciclo x tutti i nostri pgm da monitorare
031300020607     C                   DOW       i <= %elem(OURPGM)
031400020607     C* Ciclo x 1000 pgm dello stack richiami
031500020607     C     1             DO        1000          j                 4 0
031600020607     C* Effettuo considerazioni solo se il pgm dello stack è valido
031700020613     C                   IF        SKSTACK(j) <> *blanks
031800020613     C*
031900020613     C                   IF        %subst(SKSTACK(j):1:1) <> 'Q'   AND
032000020613     C                             %subst(SKSTACK(j):1:3) <> 'BAK'
032100020607     C* Imposto momentaneamente il pgm corrente dello stack
032200020607     C                   EVAL      wk1Stack   =  SKSTACK(j)
032300020607     C* Se ho trovato un nostro pgm lo imposto e continuo la ricerca
032400020607     C                   IF        SKSTACK(J) =  OURPGM(i)
032500020607     C                   EVAL      wk2Stack   =  SKSTACK(j)
032600020607     C                   ENDIF
032700020613     C                   ENDIF
032800020607     C                   ELSE
032900020607     C                   LEAVE
033000020607     C                   ENDIF
033100020607     C                   ENDDO
033200020607     C                   ADD       1             i
033300020607     C                   ENDDO
033400020607     C*
033500020607     C* Al termine della scansione dello stack effettuo considerazioni finali
033600020607     C                   IF        wk2Stack <> *blanks
033700020607     C                   EVAL      depStack =  wk2Stack
033800020607     C                   ELSE
033900020607     C                   EVAL      depStack =  wk1Stack
034000020607     C                   ENDIF
034100020618     C*
034200020618     C* Chiamo il *PGM x reperimento informazoni sul job
034300020618     C                   CALL      'QUSRJOBI'
034400020618     C                   PARM                    ReceiveVar
034500020618     C                   PARM                    LenReceiveVar
034600020618     C                   PARM      'JOBI0200'    FormatName
034700020618     C                   PARM      '*'           QualJobName
034800020618     C                   PARM                    InternalJobID
034900020607     C*
035000020618     C                   MOVEL(P)  ReceiveVar    QUSI020000
035100020618     C*
035200020607     C                   ENDSR
035300020607
035400020607
035500020607** OURPGM - NOSTRI PGM DA MONITORARE
035600020607TNSB23R
035700020607TRECD6R
035800020607TNSB36R
035900020607TRECE9R
036000020607TREC78R
036100020607TREC79R
036200020607TNTE50R
036300020607TRECT0R
036400020607TREC11R
036500020607TREC87R
036600020607TREC88R
036700020705TNSF02C
036800020705TNSF04C
