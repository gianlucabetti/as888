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
001500040607     D OURPGM          S             10    DIM(14) CTDATA PERRCD(1)
001600040607     D PGMNOTHING      S             10    DIM(1)  CTDATA PERRCD(1)
001700020607     D SKSTACK         S             10    DIM(1000)
001800021128     D OriRec          S           1000    VARYING INZ(' ')
001900021128     D NewRec          S           1000    VARYING INZ(' ')
002000020516
002100020617     D Ptncsb00ds    E DS
002200020617     D                                     EXTNAME(TNCSB00F)
002300020617     D                                     INZ
002400020617     D                                     PREFIX(P_)
002500020617     D Dtncsb00ds    E DS
002600020617     D                                     EXTNAME(TNCSB00F)
002700020617     D                                     INZ
002800020617     D                                     PREFIX(D_)
002900020516
003000020516     D DS£ECO        E DS
003100020516
003200020515     D PAR1            DS
003300020510     D  File                         10
003400020510     D  Library                      10
003500020510     D  Member                       10
003600020510     D  TriggerEvent                  1
003700020510     D  TriggerTime                   1
003800020510     D  CommitLock                    1
003900020513     D  Reserved7                     3
004000020513     D  CCSIDofData                  10I 0
004100020513     D  RRN                          10I 0
004200020513     D  Reserved9                     4
004300020513     D  OriRecOffset                 10I 0
004400020513     D  OriRecLength                 10I 0
004500020513     D  OriRecNBMO                   10I 0
004600020513     D  OriRecNBML                   10I 0
004700020513     D  NewRecOffset                 10I 0
004800020513     D  NewRecLength                 10I 0
004900020513     D  NewRecNBMO                   10I 0
005000020513     D  NewRecNBML                   10I 0
005100020513     D  Reserved16                   16
005200021128     D  BufferDati                 5000    VARYING
005300020510
005400020515     D PAR2            DS
005500020515     D  PAR1Length                   10I 0
005600020515
005700020515     D* Reperimento dati del PGM
005800020515     D PGMSTATUS      SDS
005900020515     D JOB_NAME              244    253
006000020515     D USER                  254    263
006100020515     D JOB_NUM               264    269S 0
006200020515     D JOB_DATE              270    275S 0
006300020515     D RUN_DATE              276    281S 0
006400020515     D RUN_TIME              282    287S 0
006500020515     D PROC_PGM              334    343
006600020618
006700020618
006800020618     D******************************************************************
006900020618     D*Record structure for QUSRJOBI JOBI0200 format
007000020618     D******************************************************************
007100020618     DQUSI020000       DS
007200020618     D*                                             Qwc JOBI0200
007300020618     D QUSBR01                 1      4B 0
007400020618     D*                                             Bytes Return
007500020618     D QUSBA01                 5      8B 0
007600020618     D*                                             Bytes Avail
007700020618     D QUSJN03                 9     18
007800020618     D*                                             Job Name
007900020618     D QUSUN03                19     28
008000020618     D*                                             User Name
008100020618     D QUSJNBR03              29     34
008200020618     D*                                             Job Number
008300020618     D QUSIJID01              35     50
008400020618     D*                                             Int Job ID
008500020618     D QUSJS05                51     60
008600020618     D*                                             Job Status
008700020618     D QUSJT04                61     61
008800020618     D*                                             Job Type
008900020618     D QUSJS06                62     62
009000020618     D*                                             Job Subtype
009100020618     D QUSSN                  63     72
009200020618     D*                                             Subsys Name
009300020618     D QUSRP01                73     76B 0
009400020618     D*                                             Run Priority
009500020618     D QUSSPID00              77     80B 0
009600020618     D*                                             System Pool ID
009700020618     D QUSCPUU00              81     84B 0
009800020618     D*                                             CPU Used
009900020618     D QUSAIOR                85     88B 0
010000020618     D*                                             Aux IO Request
010100020618     D QUSIT                  89     92B 0
010200020618     D*                                             Interact Trans
010300020618     D QUSRT                  93     96B 0
010400020618     D*                                             Response Time
010500020618     D QUSFT                  97     97
010600020618     D*                                             Function Type
010700020618     D QUSFN15                98    107
010800020618     D*                                             Function Name
010900020618     D QUSAJS                108    111
011000020618     D*                                             Active Job Stat
011100020618     D QUSNDBLW              112    115B 0
011200020618     D*                                             Num DBase Lock Wts
011300020618     D QUSNIMLW              116    119B 0
011400020618     D*                                             Num Internal Mch Lck Wts
011500020618     D QUSNDBLW00            120    123B 0
011600020618     D*                                             Num Non DBase Lock Wts
011700020618     D QUSTDBLW              124    127B 0
011800020618     D*                                             Wait Time DBase Lock Wts
011900020618     D QUSTIMLW              128    131B 0
012000020618     D*                                             Wait Time Internal Mch Lck W
012100020618     D QUSNDBLW01            132    135B 0
012200020618     D*                                             Wait Time Non DBase Lock Wts
012300020618     D QUSERVED45            136    136
012400020618     D*                                             Reserved
012500020618     D QUSCSPID              137    140B 0
012600020618     D*                                             Current System Pool ID
012700020618     D QUSTC01               141    144B 0
012800020618     D*                                             Thread Count
012900020515
013000020605     D TipOPe          S              1
013100020618
013200020618     D ReceiveVar      S            200
013300020618     D LenReceiveVar   S              9B 0 INZ(%size(ReceiveVar))
013400020618     D FormatName      S              8
013500020618     D QualJobName     S             26
013600020618     D InternalJobID   S             16
013700020510
013800020510
013900020510     C     *ENTRY        PLIST
014000020515     C                   PARM                    PAR1
014100020515     C                   PARM                    PAR2
014200021125     C*
014300021125     C* Innanzitutto valorizzo le due immagini PRIMA e DOPO
014400021125     C                   EVAL      OriRec =
014500021125     C                             %subst(PAR1:OriRecOffset+1:OriRecLength)
014600021125     C                   EVAL      NewRec =
014700021125     C                             %subst(PAR1:NewRecOffset+1:NewRecLength)
014800020510
014900020614     C*===============================================================
015000020614     C* INNANZI TUTTO VERIFICO SE LE DUE IMMAGINI "PRIMA" E "DOPO"
015100020614     C* SONO DIVERSE ALTRIMENTI E' INUTILE PROCEDERE.
015200040607     C                   IF        OriRec <> NewRec and
015300040607     C                             W_OK = 'S'
015400020521     C*===============================================================
015500040119     C                   if        (File = 'TNCSB00F'       AND
015600021128     C                              Library = 'GAITRAGRPS')
015700021125     C*
015800021125     C* Valorizzo la DS del file
015900021125     C                   EXSR      VALDS
016000020617     C*
016100020617     C* Elaboro solo se trattasi di chiamata trigger su *AFTER
016200020617     C                   IF        TriggerTime = After
016300031209     C*
016400031209     C* se in MODIFICA, elabora solo se alcuni campi sono cambiati
016500031209     C                   IF        TriggerEvent = TrgInsert OR
016600031209     C                             TriggerEvent = TrgDelete OR
016700031209     C                             TriggerEvent = TrgUpdate AND
016800031209     c                             (P_CSBTPI <> D_CSBTPI OR
016900031209     c                              P_CSBVCA <> D_CSBVCA OR
017000031209     c                              P_CSBCAS <> D_CSBCAS)
017100020617     C*
017200020617     C* Trascodifico il tipo di operazione avvenuta sul file
017300020617     C                   IF        TriggerEvent = TrgInsert
017400020617     C                   EVAL      TipOpe = CodTrgInsert
017500020617     C                   ENDIF
017600020617     C                   IF        TriggerEvent = TrgDelete
017700020617     C                   EVAL      TipOpe = CodTrgDelete
017800020617     C                   ENDIF
017900020617     C                   IF        TriggerEvent = TrgUpdate
018000020617     C                   EVAL      TipOpe = CodTrgUpdate
018100020617     C                   ENDIF
018200020617     C*
018300020617     C* Compongo la DS DS£ECO da scrivere nella apposita DTAQ
018400020617     C                   CLEAR                   DS£ECO
018500020731     C                   EVAL      D£GTJB = QUSJT04
018600020617     C                   EVAL      D£GSTS = TipOpe
018700020617     C*
018800020617     C* Giro la data da formato MMGGAA a formato AAMMGG
018900020617     C                   MOVEL(P)  RUN_DATE      WRKDATE           6
019000020617     C                   EVAL      WRKDATE = %subst(WRKDATE:5:2) +
019100020617     C                                       %subst(WRKDATE:1:2) +
019200020617     C                                       %subst(WRKDATE:3:2)
019300020617     C                   MOVEL     WRKDATE       D£GDAT
019400020617     C                   EVAL      D£GTIM = RUN_TIME
019500020617     C                   EVAL      D£GUSR = USER
019600020617     C                   EVAL      D£GPGF = depStack
019700020617     C                   EVAL      D£GFIL = File
019800020617     C                   EVAL      D£GMBR = Member
019900020617     C                   EVAL      D£GLIB = Library
020000020617     C*
020100020617     C* A seconda del tipo di operazione sul file gestisco la valorizzaz. della
020200020617     C* DS. Questo xchè il trigger passa nelle immagini vuote nn valori "loval"
020300020617     C* x tipo campo (es.: *blanks x gli ALFA e *zeros x i NUMERICI) ma il
020400020617     C* valore *null.
020500020617     C                   IF        TipOpe = 'U'
020600020617     C                   EVAL      D£PRE1 = %subst(OriRec:001:214)
020700020617     C                   EVAL      D£DRE1 = %subst(NewRec:001:214)
020800020617     C                   ENDIF
020900020617     C                   IF        TipOpe = 'I'
021000020617     C                   EVAL      D£DRE1 = %subst(NewRec:001:214)
021100020617     C                   ENDIF
021200020617     C                   IF        TipOpe = 'D'
021300020617     C                   EVAL      D£PRE1 = %subst(OriRec:001:214)
021400020617     C                   ENDIF
021500020617     C*
021600040119     C                   CALL      'TRECD4PRV'
021700020617     C                   PARM                    DS£ECO
021800040128     C                   PARM                    QUSJT04
021900031209     C                   ENDIF
022000031209     C*
022100020617     C                   ENDIF
022200020617     C*
022300020617     C                   ENDIF
022400020614     C*
022500020614     C*===============================================================
022600020617     C                   ENDIF
022700020614     C*===============================================================
022800040128     C* Infine x decidere il tipo di chiusura testo il tipo di job: se
022900040128     C* interattivo chiudo in "RT", altrimenti in "LR".
023000040128     C                   IF        QUSJT04 = 'I'
023100040128     C                   EVAL      *INLR = *ON
023200040128     C                   ELSE
023300040128     C                   EVAL      *INRT = *ON
023400040128     C                   ENDIF
023500020617     C*------------------------------------------------------------------------*
023600020617     C* VALDS - IMPOSTA LE DS A SECONDA DEL FILE A CUI SI RIFERISCONO
023700020617     C*------------------------------------------------------------------------*
023800020617     C     VALDS         BEGSR
023900020617     C*
024000020617     C                   EVAL      Ptncsb00ds = OriRec
024100020617     C                   EVAL      Dtncsb00ds = NewRec
024200020617     C*
024300020617     C                   ENDSR
024400020607     C*------------------------------------------------------------------------*
024500020607     C* *INZSR - OPERAZIONI INIZIALI
024600020607     C*------------------------------------------------------------------------*
024700020607     C     *INZSR        BEGSR
024800020607     C*
024900040607     C* Inizializzo la viariabile che condizionerà l'intera procedura
025000040607     C                   MOVEL     'S'           W_OK              1
025100020607     C* Chiamo il *PGM x reperimento stack di richiami del job
025200020612     C                   CALL      'XRTVSTKR'
025300020607     C                   PARM                    PStack         1000
025400020607     C                   PARM                    PStartPoint       3 0
025500020607     C                   PARM                    PEntries          3 0
025600020607     C*
025700020607     C* Cerco nello stack di richiami i nostroi pgm da monitorare
025800020607     C                   MOVEL     *blanks       depStack         10
025900020607     C                   MOVEL     *blanks       wk1Stack         10
026000020607     C                   MOVEL     *blanks       wk2Stack         10
026100020607     C                   MOVEA     PStack        SKSTACK
026200020607     C                   Z-ADD     1             i                 2 0
026300020607     C* Ciclo x tutti i nostri pgm da monitorare
026400020607     C                   DOW       i <= %elem(OURPGM)
026500020607     C* Ciclo x 1000 pgm dello stack richiami
026600020607     C     1             DO        1000          j                 4 0
026700020607     C* Effettuo considerazioni solo se il pgm dello stack è valido
026800020613     C                   IF        SKSTACK(j) <> *blanks
026900020613     C*
027000020613     C                   IF        %subst(SKSTACK(j):1:1) <> 'Q'   AND
027100020613     C                             %subst(SKSTACK(j):1:3) <> 'BAK'
027200020607     C* Imposto momentaneamente il pgm corrente dello stack
027300020607     C                   EVAL      wk1Stack   =  SKSTACK(j)
027400020607     C* Se ho trovato un nostro pgm lo imposto e continuo la ricerca
027500020607     C                   IF        SKSTACK(J) =  OURPGM(i)
027600020607     C                   EVAL      wk2Stack   =  SKSTACK(j)
027700020607     C                   ENDIF
027800020613     C                   ENDIF
027900020607     C                   ELSE
028000020607     C                   LEAVE
028100020607     C                   ENDIF
028200020607     C                   ENDDO
028300020607     C                   ADD       1             i
028400020607     C                   ENDDO
028500020607     C*
028600020607     C* Al termine della scansione dello stack effettuo considerazioni finali
028700020607     C                   IF        wk2Stack <> *blanks
028800020607     C                   EVAL      depStack =  wk2Stack
028900020607     C                   ELSE
029000020607     C                   EVAL      depStack =  wk1Stack
029100020607     C                   ENDIF
029200040607     C* A questo punto determino se il pgm eletto come "chiamante" è 1 d quelli
029300040607     C* x i quali nn si deve fare nulla (nella fattispecie nn scrivere TITASQUE)
029400040607     C     depStack      LOOKUP    PGMNOTHING                             20
029500040607     C                   IF        *IN20 = *on
029600040607     C                   MOVEL     'N'           W_OK
029700040607     C                   ENDIF
029800020618     C*
029900020618     C* Chiamo il *PGM x reperimento informazoni sul job
030000020618     C                   CALL      'QUSRJOBI'
030100020618     C                   PARM                    ReceiveVar
030200020618     C                   PARM                    LenReceiveVar
030300020618     C                   PARM      'JOBI0200'    FormatName
030400020618     C                   PARM      '*'           QualJobName
030500020618     C                   PARM                    InternalJobID
030600020607     C*
030700020618     C                   MOVEL(P)  ReceiveVar    QUSI020000
030800020618     C*
030900020607     C                   ENDSR
031000020607
031100020607
031200020607** OURPGM - NOSTRI PGM DA MONITORARE
031300020607TNSB23R
031400020607TRECD6R
031500020607TNSB36R
031600020607TRECE9R
031700020607TREC78R
031800020607TREC79R
031900020607TNTE50R
032000020607TRECT0R
032100020607TREC11R
032200020607TREC87R
032300020607TREC88R
032400020705TNSF02C
032500020705TNSF04C
032600040607TNSC61C
032700040607** PGMNOTHING - PGM X I QUALI NN SI DEVE FARE ASSOLUTAMENTE NULLA (NO TITASQUE)
032800040607TNSC61C
