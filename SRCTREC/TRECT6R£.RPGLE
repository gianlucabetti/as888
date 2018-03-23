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
015500020521     C*
015600020521     C* Se sono in GAITRAGRU effettuo altre operazioni specifiche
015700020617     C                   IF        File = 'TNCSB00F'     AND
015800020617     C                             Library = 'GAITRAGRU'
015900031209     C*
016000031209     C* Valorizzo la DS del file
016100031209     C                   EXSR      VALDS
016200020521     C*
016300020521     C* Elaboro solo se trattasi di chiamata trigger su *AFTER
016400020617     C                   IF        TriggerTime = After
016500031209     C*
016600031209     C* se in MODIFICA, elabora solo se alcuni campi sono cambiati
016700031209     C                   IF        TriggerEvent = TrgInsert OR
016800031209     C                             TriggerEvent = TrgDelete OR
016900031209     C                             TriggerEvent = TrgUpdate AND
017000031209     c                             (P_CSBTPI <> D_CSBTPI OR
017100031209     c                              P_CSBVCA <> D_CSBVCA OR
017200031209     c                              P_CSBCAS <> D_CSBCAS)
017300020521     C*
017400020617     C* Trascodifico il tipo di operazione avvenuta sul file
017500020521     C                   IF        TriggerEvent = TrgInsert
017600020521     C                   EVAL      TipOpe = CodTrgInsert
017700020521     C                   ENDIF
017800020521     C                   IF        TriggerEvent = TrgDelete
017900020521     C                   EVAL      TipOpe = CodTrgDelete
018000020521     C                   ENDIF
018100020521     C                   IF        TriggerEvent = TrgUpdate
018200020521     C                   EVAL      TipOpe = CodTrgUpdate
018300020521     C                   ENDIF
018400020521     C*
018500020521     C* Compongo la DS DS£ECO da scrivere nella apposita DTAQ
018600020521     C                   CLEAR                   DS£ECO
018700020731     C                   EVAL      D£GTJB = QUSJT04
018800020521     C                   EVAL      D£GSTS = TipOpe
018900020610     C*
019000020610     C* Giro la data da formato MMGGAA a formato AAMMGG
019100020610     C                   MOVEL(P)  RUN_DATE      WRKDATE           6
019200020610     C                   EVAL      WRKDATE = %subst(WRKDATE:5:2) +
019300020610     C                                       %subst(WRKDATE:1:2) +
019400020610     C                                       %subst(WRKDATE:3:2)
019500020610     C                   MOVEL     WRKDATE       D£GDAT
019600020521     C                   EVAL      D£GTIM = RUN_TIME
019700020521     C                   EVAL      D£GUSR = USER
019800020521     C                   EVAL      D£GPGF = depStack
019900020521     C                   EVAL      D£GFIL = File
020000020521     C                   EVAL      D£GMBR = Member
020100020521     C                   EVAL      D£GLIB = Library
020200020610     C*
020300020610     C* A seconda del tipo di operazione sul file gestisco la valorizzaz. della
020400020610     C* DS. Questo xchè il trigger passa nelle immagini vuote nn valori "loval"
020500020610     C* x tipo campo (es.: *blanks x gli ALFA e *zeros x i NUMERICI) ma il
020600020610     C* valore *null.
020700020610     C                   IF        TipOpe = 'U'
020800020617     C                   EVAL      D£PRE1 = %subst(OriRec:001:214)
020900020617     C                   EVAL      D£DRE1 = %subst(NewRec:001:214)
021000020610     C                   ENDIF
021100020610     C                   IF        TipOpe = 'I'
021200020617     C                   EVAL      D£DRE1 = %subst(NewRec:001:214)
021300020610     C                   ENDIF
021400020610     C                   IF        TipOpe = 'D'
021500020617     C                   EVAL      D£PRE1 = %subst(OriRec:001:214)
021600020610     C                   ENDIF
021700020521     C*
021800020612     C                   CALL      'TRECD2R'
021900020521     C                   PARM                    DS£ECO
022000031209     C                   ENDIF
022100031209     C*
022200020521     C                   ENDIF
022300020521     C*
022400020521     C                   ENDIF
022500020617     C*
022600020617     C*===============================================================
022700020617     C*
022800021125     C* Se sono in EDPFG effettuo altre operazioni specifiche
022900021128     C                   IF        (File = 'TESTRG0F'       AND
023000021128     C                              Library = 'EDPFG')      OR
023100021128     C                             (File = 'TNCSB00F'       AND
023200021128     C                              Library = 'GAITRAGRPS')
023300021125     C*
023400021125     C* Valorizzo la DS del file
023500021125     C                   EXSR      VALDS
023600020617     C*
023700020617     C* Elaboro solo se trattasi di chiamata trigger su *AFTER
023800020617     C                   IF        TriggerTime = After
023900031209     C*
024000031209     C* se in MODIFICA, elabora solo se alcuni campi sono cambiati
024100031209     C                   IF        TriggerEvent = TrgInsert OR
024200031209     C                             TriggerEvent = TrgDelete OR
024300031209     C                             TriggerEvent = TrgUpdate AND
024400031209     c                             (P_CSBTPI <> D_CSBTPI OR
024500031209     c                              P_CSBVCA <> D_CSBVCA OR
024600031209     c                              P_CSBCAS <> D_CSBCAS)
024700020617     C*
024800020617     C* Trascodifico il tipo di operazione avvenuta sul file
024900020617     C                   IF        TriggerEvent = TrgInsert
025000020617     C                   EVAL      TipOpe = CodTrgInsert
025100020617     C                   ENDIF
025200020617     C                   IF        TriggerEvent = TrgDelete
025300020617     C                   EVAL      TipOpe = CodTrgDelete
025400020617     C                   ENDIF
025500020617     C                   IF        TriggerEvent = TrgUpdate
025600020617     C                   EVAL      TipOpe = CodTrgUpdate
025700020617     C                   ENDIF
025800020617     C*
025900020617     C* Compongo la DS DS£ECO da scrivere nella apposita DTAQ
026000020617     C                   CLEAR                   DS£ECO
026100020731     C                   EVAL      D£GTJB = QUSJT04
026200020617     C                   EVAL      D£GSTS = TipOpe
026300020617     C*
026400020617     C* Giro la data da formato MMGGAA a formato AAMMGG
026500020617     C                   MOVEL(P)  RUN_DATE      WRKDATE           6
026600020617     C                   EVAL      WRKDATE = %subst(WRKDATE:5:2) +
026700020617     C                                       %subst(WRKDATE:1:2) +
026800020617     C                                       %subst(WRKDATE:3:2)
026900020617     C                   MOVEL     WRKDATE       D£GDAT
027000020617     C                   EVAL      D£GTIM = RUN_TIME
027100020617     C                   EVAL      D£GUSR = USER
027200020617     C                   EVAL      D£GPGF = depStack
027300020617     C                   EVAL      D£GFIL = File
027400020617     C                   EVAL      D£GMBR = Member
027500020617     C                   EVAL      D£GLIB = Library
027600020617     C*
027700020617     C* A seconda del tipo di operazione sul file gestisco la valorizzaz. della
027800020617     C* DS. Questo xchè il trigger passa nelle immagini vuote nn valori "loval"
027900020617     C* x tipo campo (es.: *blanks x gli ALFA e *zeros x i NUMERICI) ma il
028000020617     C* valore *null.
028100020617     C                   IF        TipOpe = 'U'
028200020617     C                   EVAL      D£PRE1 = %subst(OriRec:001:214)
028300020617     C                   EVAL      D£DRE1 = %subst(NewRec:001:214)
028400020617     C                   ENDIF
028500020617     C                   IF        TipOpe = 'I'
028600020617     C                   EVAL      D£DRE1 = %subst(NewRec:001:214)
028700020617     C                   ENDIF
028800020617     C                   IF        TipOpe = 'D'
028900020617     C                   EVAL      D£PRE1 = %subst(OriRec:001:214)
029000020617     C                   ENDIF
029100020617     C*
029200020617     C                   CALL      'TRECD2R'
029300020617     C                   PARM                    DS£ECO
029400031209     C                   ENDIF
029500031209     C*
029600020617     C                   ENDIF
029700020617     C*
029800020617     C                   ENDIF
029900020614     C*
030000020614     C*===============================================================
030100020617     C                   ENDIF
030200020614     C*===============================================================
030300020607     C*
030400020618     C* Infine x decidere il tipo di chiusura testo il tipo di job: se
030500020618     C* interattivo chiudo in "RT", altrimenti in "LR".
030600020618     C                   IF        QUSJT04 = 'I'
030700020618     C                   EVAL      *INLR = *ON
030800020618     C                   ELSE
030900020618     C                   EVAL      *INRT = *ON
031000020618     C                   ENDIF
031100020617     C*------------------------------------------------------------------------*
031200020617     C* VALDS - IMPOSTA LE DS A SECONDA DEL FILE A CUI SI RIFERISCONO
031300020617     C*------------------------------------------------------------------------*
031400020617     C     VALDS         BEGSR
031500020617     C*
031600020617     C                   EVAL      Ptncsb00ds = OriRec
031700020617     C                   EVAL      Dtncsb00ds = NewRec
031800020617     C*
031900020617     C                   ENDSR
032000020607     C*------------------------------------------------------------------------*
032100020607     C* *INZSR - OPERAZIONI INIZIALI
032200020607     C*------------------------------------------------------------------------*
032300020607     C     *INZSR        BEGSR
032400020607     C*
032500040607     C* Inizializzo la viariabile che condizionerà l'intera procedura
032600040607     C                   MOVEL     'S'           W_OK              1
032700020607     C* Chiamo il *PGM x reperimento stack di richiami del job
032800020612     C                   CALL      'XRTVSTKR'
032900020607     C                   PARM                    PStack         1000
033000020607     C                   PARM                    PStartPoint       3 0
033100020607     C                   PARM                    PEntries          3 0
033200020607     C*
033300020607     C* Cerco nello stack di richiami i nostroi pgm da monitorare
033400020607     C                   MOVEL     *blanks       depStack         10
033500020607     C                   MOVEL     *blanks       wk1Stack         10
033600020607     C                   MOVEL     *blanks       wk2Stack         10
033700020607     C                   MOVEA     PStack        SKSTACK
033800020607     C                   Z-ADD     1             i                 2 0
033900020607     C* Ciclo x tutti i nostri pgm da monitorare
034000020607     C                   DOW       i <= %elem(OURPGM)
034100020607     C* Ciclo x 1000 pgm dello stack richiami
034200020607     C     1             DO        1000          j                 4 0
034300020607     C* Effettuo considerazioni solo se il pgm dello stack è valido
034400020613     C                   IF        SKSTACK(j) <> *blanks
034500020613     C*
034600020613     C                   IF        %subst(SKSTACK(j):1:1) <> 'Q'   AND
034700020613     C                             %subst(SKSTACK(j):1:3) <> 'BAK'
034800020607     C* Imposto momentaneamente il pgm corrente dello stack
034900020607     C                   EVAL      wk1Stack   =  SKSTACK(j)
035000020607     C* Se ho trovato un nostro pgm lo imposto e continuo la ricerca
035100020607     C                   IF        SKSTACK(J) =  OURPGM(i)
035200020607     C                   EVAL      wk2Stack   =  SKSTACK(j)
035300020607     C                   ENDIF
035400020613     C                   ENDIF
035500020607     C                   ELSE
035600020607     C                   LEAVE
035700020607     C                   ENDIF
035800020607     C                   ENDDO
035900020607     C                   ADD       1             i
036000020607     C                   ENDDO
036100020607     C*
036200020607     C* Al termine della scansione dello stack effettuo considerazioni finali
036300020607     C                   IF        wk2Stack <> *blanks
036400020607     C                   EVAL      depStack =  wk2Stack
036500020607     C                   ELSE
036600020607     C                   EVAL      depStack =  wk1Stack
036700020607     C                   ENDIF
036800020618     C*
036900040607     C* A questo punto determino se il pgm eletto come "chiamante" è 1 d quelli
037000040607     C* x i quali nn si deve fare nulla (nella fattispecie nn scrivere TITASQUE)
037100040607     C     depStack      LOOKUP    PGMNOTHING                             20
037200040607     C                   IF        *IN20 = *on
037300040607     C                   MOVEL     'N'           W_OK
037400040607     C                   ENDIF
037500040607     C*
037600020618     C* Chiamo il *PGM x reperimento informazoni sul job
037700020618     C                   CALL      'QUSRJOBI'
037800020618     C                   PARM                    ReceiveVar
037900020618     C                   PARM                    LenReceiveVar
038000020618     C                   PARM      'JOBI0200'    FormatName
038100020618     C                   PARM      '*'           QualJobName
038200020618     C                   PARM                    InternalJobID
038300020607     C*
038400020618     C                   MOVEL(P)  ReceiveVar    QUSI020000
038500020618     C*
038600020607     C                   ENDSR
038700020607
038800020607
038900020607** OURPGM - NOSTRI PGM DA MONITORARE
039000020607TNSB23R
039100020607TRECD6R
039200020607TNSB36R
039300020607TRECE9R
039400020607TREC78R
039500020607TREC79R
039600020607TNTE50R
039700020607TRECT0R
039800020607TREC11R
039900020607TREC87R
040000020607TREC88R
040100020705TNSF02C
040200020705TNSF04C
040300040607TNSC61C
040400040607** PGMNOTHING - PGM X I QUALI NN SI DEVE FARE ASSOLUTAMENTE NULLA (NO TITASQUE)
040500040607TNSC61C
