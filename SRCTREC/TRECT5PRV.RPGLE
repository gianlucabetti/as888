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
001500160427     D OURPGM          S             10    DIM(14) CTDATA PERRCD(1)
001600160427     D PGMNOTHING      S             10    DIM(3)  CTDATA PERRCD(1)
001700020607     D SKSTACK         S             10    DIM(1000)
001800021128     D OriRec          S           1000    VARYING INZ(' ')
001900021128     D NewRec          S           1000    VARYING INZ(' ')
002000020516
002100020617     D Ptitas00ds    E DS
002200020617     D                                     EXTNAME(TITAS00F)
002300020617     D                                     INZ
002400020617     D                                     PREFIX(P_)
002500020617     D Dtitas00ds    E DS
002600020617     D                                     EXTNAME(TITAS00F)
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
004400021125     D  OriRecLength                 10I 0
004500020513     D  OriRecNBMO                   10I 0
004600020513     D  OriRecNBML                   10I 0
004700020513     D  NewRecOffset                 10I 0
004800021125     D  NewRecLength                 10I 0
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
012900020618
013000020618     D TipOPe          S              1
013100020618
013200020618     D ReceiveVar      S            200
013300020618     D LenReceiveVar   S              9B 0 INZ(%size(ReceiveVar))
013400020618     D FormatName      S              8
013500020618     D QualJobName     S             26
013600020618     D InternalJobID   S             16
013700020515
013800020510
013900020926     C*
014000020926     C     *ENTRY        PLIST
014100020926     C                   PARM                    PAR1
014200020926     C                   PARM                    PAR2
014300021125     C*
014400021125     C* Innanzitutto valorizzo le due immagini PRIMA e DOPO
014500021125     C                   EVAL      OriRec =
014600021125     C                             %subst(PAR1:OriRecOffset+1:OriRecLength)
014700021125     C                   EVAL      NewRec =
014800021125     C                             %subst(PAR1:NewRecOffset+1:NewRecLength)
014900020510
015000020926     C*===============================================================
015100020926     C* INNANZI TUTTO VERIFICO SE LE DUE IMMAGINI "PRIMA" E "DOPO"
015200020926     C* SONO DIVERSE ALTRIMENTI E' INUTILE PROCEDERE...
015300020926     C* ... E SE OCCORRE => PROCEDO
015400021125     C                   IF        OriRec <> NewRec AND
015500020926     C                             W_OK = 'S'
015600040119     c*
015700020617     C                   IF        (File = 'TITAS00F'     OR
015800020617     C                              File = 'TITAS10F')    AND
015900020617     C                             Library = 'GAITRAGRPS'
016000021128     C*
016100021128     C* Valorizzo la DS del file
016200021128     C                   EXSR      VALDS
016300020617     C*
016400020617     C* Elaboro solo se trattasi di chiamata trigger su *AFTER
016500020617     C                   IF        TriggerTime = After
016600031210     C*
016700031210     C* se in MODIFICA, elabora solo se alcuni campi sono cambiati
016800031210     C                   IF        TriggerEvent = TrgInsert OR
016900031210     C                             TriggerEvent = TrgDelete OR
017000031210     C                             TriggerEvent = TrgUpdate AND
017100031210     c                             (P_TASAAS <> D_TASAAS OR
017200031210     c                              P_TASLNP <> D_TASLNP OR
017300031210     c                              P_TASNRS <> D_TASNRS OR
017400031210     c                              P_TASNSP <> D_TASNSP OR
017500031210     c                              P_TASTBL <> D_TASTBL OR
017600031210     c                              P_TASCBO <> D_TASCBO OR
017700031210     c                              P_TASMGS <> D_TASMGS OR
017800031210     c                              P_TASKSC <> D_TASKSC OR
017900031210     c                              P_TASLNA <> D_TASLNA OR
018000031210     c                              P_TASNCL <> D_TASNCL OR
018100031210     c                              P_TASPKB <> D_TASPKB OR
018200031210     c                              P_TASPKC <> D_TASPKC OR
018300031210     c                              P_TASNCP <> D_TASNCP OR
018400031210     c                              P_TASPKF <> D_TASPKF OR
018500031210     c                              P_TASPVL <> D_TASPVL OR
018600031210     c                              P_TASVLB <> D_TASVLB OR
018700031210     c                              P_TASVLC <> D_TASVLC OR
018800031210     c                              P_TASNCR <> D_TASNCR OR
018900031210     c                              P_TASVLF <> D_TASVLF OR
019000031210     c                              P_TASFVF <> D_TASFVF OR
019100031210     c                              P_TASIAS <> D_TASIAS OR
019200031210     c                              P_TASVAS <> D_TASVAS OR
019300031210     c                              P_TASCTR <> D_TASCTR OR
019400031210     c                              P_TASTSP <> D_TASTSP OR
019500031210     c                              P_TASRSD <> D_TASRSD OR
019600031210     c                              P_TASCAD <> D_TASCAD OR
019700031210     c                              P_TASLOD <> D_TASLOD OR
019800031210     c                              P_TASPRD <> D_TASPRD OR
019900031210     c                              P_TASNZD <> D_TASNZD OR
020000031210     c                              P_TASCTS <> D_TASCTS OR
020100031210     c                              P_TASFIN <> D_TASFIN OR
020200031210     c                              P_TASCCM <> D_TASCCM OR
020300031210     c                              P_TASRMN <> D_TASRMN OR
020400031210     c                              P_TASMCT <> D_TASMCT OR
020500031210     c                              P_TASFAP <> D_TASFAP OR
020600031210     c                              P_TASFLP <> D_TASFLP OR
020700031210     c                              P_TASFL2 <> D_TASFL2 OR
020800031210     c                              P_TASTFA <> D_TASTFA OR
020900031210     c                              P_TASTFP <> D_TASTFP OR
021000031210     c                              P_TASSOP <> D_TASSOP OR
021100031210     c                              P_TASPDR <> D_TASPDR OR
021200031210     c                              P_TASDRT <> D_TASDRT OR
021300031210     c                              P_TASNRT <> D_TASNRT OR
021400031210     c                              P_TASSTR <> D_TASSTR OR
021500031210     c                              P_TASCTM <> D_TASCTM OR
021600031210     c                              P_TASDUC <> D_TASDUC OR
021700031210     c                              P_TASDTI <> D_TASDTI OR
021800031210     c                              P_TASPDC <> D_TASPDC OR
021900031210     c                              P_TASNDC <> D_TASNDC OR
022000031210     c                              P_TASDDC <> D_TASDDC OR
022100031210     c                              P_TASSTP <> D_TASSTP OR
022200031210     c                              P_TASDCP <> D_TASDCP OR
022300031210     c                              P_TASDCM <> D_TASDCM OR
022400031210     c                              P_TASCCA <> D_TASCCA OR
022500031210     c                              P_TASNRC <> D_TASNRC OR
022600031210     c                              P_TASFDN <> D_TASFDN OR
022700031210     c                              P_TASFFD <> D_TASFFD OR
022800031210     c                              P_TASFTC <> D_TASFTC OR
022900031210     c                              P_TASTC2 <> D_TASTC2 OR
023000031210     c                              P_TASFBR <> D_TASFBR OR
023100031210     c                              P_TASLL1 <> D_TASLL1 OR
023200031210     c                              P_TASLL2 <> D_TASLL2 OR
023300031210     c                              P_TASFA2 <> D_TASFA2 OR
023400031210     c                              P_TASFCA <> D_TASFCA OR
023500031210     c                              P_TASFA1 <> D_TASFA1 OR
023600031210     c                              P_TASFGC <> D_TASFGC OR
023700031210     c                              P_TASFLO <> D_TASFLO OR
023800031210     c                              P_TASGGS <> D_TASGGS OR
023900031210     c                              P_TASGMA <> D_TASGMA OR
024000031210     c                              P_TASGVA <> D_TASGVA OR
024100031210     c                              P_TASDIV <> D_TASDIV OR
024200031210     c                              P_TASPOR <> D_TASPOR OR
024300031210     c                              P_TASSV1 <> D_TASSV1 OR
024400031210     c                              P_TASVA1 <> D_TASVA1 OR
024500031210     c                              P_TASSV2 <> D_TASSV2 OR
024600031210     c                              P_TASVA2 <> D_TASVA2 OR
024700031210     c                              P_TASSV3 <> D_TASSV3 OR
024800031210     c                              P_TASVA3 <> D_TASVA3 OR
024900031210     c                              P_TASDFT <> D_TASDFT OR
025000031210     c                              P_TASFIV <> D_TASFIV OR
025100031210     c                              P_TASSCL <> D_TASSCL)
025200020617     C*
025300020617     C* Trascodifico il tipo di operazione avvenuta sul file
025400020617     C                   IF        TriggerEvent = TrgInsert
025500020617     C                   EVAL      TipOpe = CodTrgInsert
025600020617     C                   ENDIF
025700020617     C                   IF        TriggerEvent = TrgDelete
025800020617     C                   EVAL      TipOpe = CodTrgDelete
025900020617     C                   ENDIF
026000020617     C                   IF        TriggerEvent = TrgUpdate
026100020617     C                   EVAL      TipOpe = CodTrgUpdate
026200020617     C                   ENDIF
026300020617     C*
026400040119     C* Compongo la DS DS£ECO da passare al pgm di calcolo
026500020617     C                   CLEAR                   DS£ECO
026600020731     C                   EVAL      D£GTJB = QUSJT04
026700020617     C                   EVAL      D£GSTS = TipOpe
026800020617     C*
026900020617     C* Giro la data da formato MMGGAA a formato AAMMGG
027000020617     C                   MOVEL(P)  RUN_DATE      WRKDATE           6
027100020617     C                   EVAL      WRKDATE = %subst(WRKDATE:5:2) +
027200020617     C                                       %subst(WRKDATE:1:2) +
027300020617     C                                       %subst(WRKDATE:3:2)
027400020617     C                   MOVEL     WRKDATE       D£GDAT
027500020617     C                   EVAL      D£GTIM = RUN_TIME
027600020617     C                   EVAL      D£GUSR = USER
027700020617     C                   EVAL      D£GPGF = depStack
027800020617     C                   EVAL      D£GFIL = File
027900020617     C                   EVAL      D£GMBR = Member
028000020617     C                   EVAL      D£GLIB = Library
028100020617     C*
028200020617     C* A seconda del tipo di operazione sul file gestisco la valorizzaz. della
028300020617     C* DS. Questo xchè il trigger passa nelle immagini vuote nn valori "loval"
028400020617     C* x tipo campo (es.: *blanks x gli ALFA e *zeros x i NUMERICI) ma il
028500020617     C* valore *null.
028600020617     C                   IF        TipOpe = 'U'
028700020617     C                   EVAL      D£PRE1 = %subst(OriRec:001:250)
028800020617     C                   EVAL      D£PRE2 = %subst(OriRec:251:220)
028900020617     C                   EVAL      D£DRE1 = %subst(NewRec:001:250)
029000020617     C                   EVAL      D£DRE2 = %subst(NewRec:251:220)
029100020617     C                   ENDIF
029200020617     C                   IF        TipOpe = 'I'
029300020617     C                   EVAL      D£DRE1 = %subst(NewRec:001:250)
029400020617     C                   EVAL      D£DRE2 = %subst(NewRec:251:220)
029500020617     C                   ENDIF
029600020617     C                   IF        TipOpe = 'D'
029700020617     C                   EVAL      D£PRE1 = %subst(OriRec:001:250)
029800020617     C                   EVAL      D£PRE2 = %subst(OriRec:251:220)
029900020617     C                   ENDIF
030000020617     C*
030100040119     C                   CALL      'TRECD4PRV'
030200020617     C                   PARM                    DS£ECO
030300040128     C                   PARM                    QUSJT04
030400020617     C                   ENDIF
030500031210     C*
030600031210     C                   ENDIF
030700020617     C*
030800020617     C                   ENDIF
030900040119     C*
031000040119     C                   ENDIF
031100020617     C*
031200040128     C* Infine x decidere il tipo di chiusura testo il tipo di job: se
031300040128     C* interattivo chiudo in "RT", altrimenti in "LR".
031400040203     C                   IF        QUSJT04 = 'I'
031500040203     C                   EVAL      *INLR = *ON
031600040203     C                   ELSE
031700040128     C                   EVAL      *INRT = *ON
031800040203     C                   ENDIF
031900020617     C*------------------------------------------------------------------------*
032000020617     C* VALDS - IMPOSTA LE DS A SECONDA DEL FILE A CUI SI RIFERISCONO
032100020617     C*------------------------------------------------------------------------*
032200020617     C     VALDS         BEGSR
032300020617     C*
032400020617     C                   EVAL      Ptitas00ds = OriRec
032500020617     C                   EVAL      Dtitas00ds = NewRec
032600020617     C*
032700020617     C                   ENDSR
032800020607     C*------------------------------------------------------------------------*
032900020607     C* *INZSR - OPERAZIONI INIZIALI
033000020607     C*------------------------------------------------------------------------*
033100020607     C     *INZSR        BEGSR
033200020926     C*
033300020926     C* Inizializzo la viariabile che condizionerà l'intera procedura
033400020926     C                   MOVEL     'S'           W_OK              1
033500020607     C*
033600020607     C* Chiamo il *PGM x reperimento stack di richiami del job
033700020612     C                   CALL      'XRTVSTKR'
033800020607     C                   PARM                    PStack         1000
033900020607     C                   PARM                    PStartPoint       3 0
034000020607     C                   PARM                    PEntries          3 0
034100020607     C*
034200020607     C* Cerco nello stack di richiami i nostroi pgm da monitorare
034300020607     C                   MOVEL     *blanks       depStack         10
034400020607     C                   MOVEL     *blanks       wk1Stack         10
034500020607     C                   MOVEL     *blanks       wk2Stack         10
034600020607     C                   MOVEA     PStack        SKSTACK
034700020607     C                   Z-ADD     1             i                 2 0
034800020607     C* Ciclo x tutti i nostri pgm da monitorare
034900020607     C                   DOW       i <= %elem(OURPGM)
035000020607     C* Ciclo x 1000 pgm dello stack richiami
035100020607     C     1             DO        1000          j                 4 0
035200020607     C* Effettuo considerazioni solo se il pgm dello stack è valido
035300020613     C                   IF        SKSTACK(j) <> *blanks
035400020613     C*
035500020613     C                   IF        %subst(SKSTACK(j):1:1) <> 'Q'   AND
035600020613     C                             %subst(SKSTACK(j):1:3) <> 'BAK'
035700020607     C* Imposto momentaneamente il pgm corrente dello stack
035800020607     C                   EVAL      wk1Stack   =  SKSTACK(j)
035900020607     C* Se ho trovato un nostro pgm lo imposto e continuo la ricerca
036000020607     C                   IF        SKSTACK(J) =  OURPGM(i)
036100020607     C                   EVAL      wk2Stack   =  SKSTACK(j)
036200020607     C                   ENDIF
036300020613     C                   ENDIF
036400020607     C                   ELSE
036500020607     C                   LEAVE
036600020607     C                   ENDIF
036700020607     C                   ENDDO
036800020607     C                   ADD       1             i
036900020607     C                   ENDDO
037000020607     C*
037100020607     C* Al termine della scansione dello stack effettuo considerazioni finali
037200020607     C                   IF        wk2Stack <> *blanks
037300020607     C                   EVAL      depStack =  wk2Stack
037400020607     C                   ELSE
037500020607     C                   EVAL      depStack =  wk1Stack
037600020607     C                   ENDIF
037700020926     C*
037800020926     C* A questo punto determino se il pgm eletto come "chiamante" è 1 d quelli
037900020926     C* x i quali nn si deve fare nulla (nella fattispecie nn scrivere TITASQUE)
038000020926     C     depStack      LOOKUP    PGMNOTHING                             20
038100020926     C                   IF        *IN20 = *on
038200020926     C                   MOVEL     'N'           W_OK
038300020926     C                   ENDIF
038400020618     C*
038500020618     C* Chiamo il *PGM x reperimento informazoni sul job
038600020618     C                   CALL      'QUSRJOBI'
038700020618     C                   PARM                    ReceiveVar
038800020618     C                   PARM                    LenReceiveVar
038900020618     C                   PARM      'JOBI0200'    FormatName
039000020618     C                   PARM      '*'           QualJobName
039100020618     C                   PARM                    InternalJobID
039200020618     C*
039300020618     C                   MOVEL(P)  ReceiveVar    QUSI020000
039400020607     C*
039500020607     C                   ENDSR
039600020607
039700020607
039800130516** OURPGM - sono i pgm che il c/e TRECD* "tratta" diversamente
039900020607TNSB23R
040000020607TRECD6R
040100020607TNSB36R
040200020607TRECE9R
040300020607TREC78R
040400020607TREC79R
040500020607TNTE50R
040600020607TRECT0R
040700020607TREC11R
040800020607TREC87R
040900020607TREC88R
041000020702TNSF02C
041100020702TNSF04C
041101160427TNVRT5R
041200020926** PGMNOTHING - PGM X I QUALI NN SI DEVE FARE ASSOLUTAMENTE NULLA (NO TITASQUE)
041300021223TNSB36R
041400021105TNSF02C
041500160427TNVRT5R
