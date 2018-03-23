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
001600040607     D PGMNOTHING      S             10    DIM(3)  CTDATA PERRCD(1)
001700020607     D SKSTACK         S             10    DIM(1000)
001800021128     D OriRec          S           1000    VARYING INZ(' ')
001900021128     D NewRec          S           1000    VARYING INZ(' ')
002000020516
002100021209     D Pfiar500ds    E DS
002200021209     D                                     EXTNAME(FIAR500F)
002300020617     D                                     INZ
002400020617     D                                     PREFIX(P_)
002500021209     D Dfiar500ds    E DS
002600021209     D                                     EXTNAME(FIAR500F)
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
015600020521     C*===============================================================
015700020521     C*
015800020521     C* Se sono in GAITRAGRU effettuo altre operazioni specifiche
015900021209     C                   IF        File = 'FIAR500F'      AND
016000020617     C                             Library = 'GAITRAGRU'
016100021206     C*
016200021206     C* Valorizzo la DS del file
016300021206     C                   EXSR      VALDS
016400021206     C*
016500030729     C* Elaboro solo se trattasi di operazione per certi tipi record
016600021209     C                   IF        P_AR5TRD = 'BAN' OR
016700040514     C                             P_AR5TRD = 'TRS' OR
016800030729     C                             P_AR5TRD = 'BNB' OR
016900021209     C                             D_AR5TRD = 'BAN' OR
017000040514     C                             D_AR5TRD = 'TRS' OR
017100030729     C                             D_AR5TRD = 'BNB'
017200020521     C*
017300020521     C* Elaboro solo se trattasi di chiamata trigger su *AFTER
017400020617     C                   IF        TriggerTime = After
017500020617     C*
017600020617     C* Trascodifico il tipo di operazione avvenuta sul file
017700020521     C                   IF        TriggerEvent = TrgInsert
017800020521     C                   EVAL      TipOpe = CodTrgInsert
017900020521     C                   ENDIF
018000020521     C                   IF        TriggerEvent = TrgDelete
018100020521     C                   EVAL      TipOpe = CodTrgDelete
018200020521     C                   ENDIF
018300020521     C                   IF        TriggerEvent = TrgUpdate
018400020521     C                   EVAL      TipOpe = CodTrgUpdate
018500020521     C                   ENDIF
018600020521     C*
018700020521     C* Compongo la DS DS£ECO da scrivere nella apposita DTAQ
018800020521     C                   CLEAR                   DS£ECO
018900020731     C                   EVAL      D£GTJB = QUSJT04
019000020521     C                   EVAL      D£GSTS = TipOpe
019100020610     C*
019200020610     C* Giro la data da formato MMGGAA a formato AAMMGG
019300020610     C                   MOVEL(P)  RUN_DATE      WRKDATE           6
019400020610     C                   EVAL      WRKDATE = %subst(WRKDATE:5:2) +
019500020610     C                                       %subst(WRKDATE:1:2) +
019600020610     C                                       %subst(WRKDATE:3:2)
019700020610     C                   MOVEL     WRKDATE       D£GDAT
019800020521     C                   EVAL      D£GTIM = RUN_TIME
019900020521     C                   EVAL      D£GUSR = USER
020000020521     C                   EVAL      D£GPGF = depStack
020100020521     C                   EVAL      D£GFIL = File
020200020521     C                   EVAL      D£GMBR = Member
020300020521     C                   EVAL      D£GLIB = Library
020400020610     C*
020500020610     C* A seconda del tipo di operazione sul file gestisco la valorizzaz. della
020600020610     C* DS. Questo xchè il trigger passa nelle immagini vuote nn valori "loval"
020700020610     C* x tipo campo (es.: *blanks x gli ALFA e *zeros x i NUMERICI) ma il
020800020610     C* valore *null.
020900020610     C                   IF        TipOpe = 'U'
021000021209     C                   EVAL      D£PRE1 = %subst(OriRec:001:244)
021100021209     C                   EVAL      D£DRE1 = %subst(NewRec:001:244)
021200030123     C                   EVAL      D£GTRD = D_AR5TRD
021300020610     C                   ENDIF
021400020610     C                   IF        TipOpe = 'I'
021500021209     C                   EVAL      D£DRE1 = %subst(NewRec:001:244)
021600030123     C                   EVAL      D£GTRD = D_AR5TRD
021700020610     C                   ENDIF
021800020610     C                   IF        TipOpe = 'D'
021900021209     C                   EVAL      D£PRE1 = %subst(OriRec:001:244)
022000030123     C                   EVAL      D£GTRD = P_AR5TRD
022100020610     C                   ENDIF
022200020521     C*
022300020612     C                   CALL      'TRECD2R'
022400020521     C                   PARM                    DS£ECO
022500020521     C                   ENDIF
022600021206     C                   ENDIF
022700020521     C*
022800020521     C                   ENDIF
022900020521     C*
023000020617     C*===============================================================
023100020617     C*
023200020617     C* Se sono in GAITRAGRPS effettuo altre operazioni specifiche
023300021209     C                   IF        File = 'FIAR500F'      AND
023400020617     C                             Library = 'GAITRAGRPS'
023500021128     C*
023600021128     C* Valorizzo la DS del file
023700021128     C                   EXSR      VALDS
023800021206     C*
023900030729     C* Elaboro solo se trattasi di operazione per certi tipi record
024000021209     C                   IF        P_AR5TRD = 'BAN' OR
024100040513     C*                            P_AR5TRD = 'TRS' OR
024200030729     C                             P_AR5TRD = 'BNB' OR
024300021209     C                             D_AR5TRD = 'BAN' OR
024400040513     C*                            D_AR5TRD = 'TRS' OR
024500030729     C                             D_AR5TRD = 'BNB'
024600020617     C*
024700020617     C* Elaboro solo se trattasi di chiamata trigger su *AFTER
024800020617     C                   IF        TriggerTime = After
024900020617     C*
025000020617     C* Trascodifico il tipo di operazione avvenuta sul file
025100020617     C                   IF        TriggerEvent = TrgInsert
025200020617     C                   EVAL      TipOpe = CodTrgInsert
025300020617     C                   ENDIF
025400020617     C                   IF        TriggerEvent = TrgDelete
025500020617     C                   EVAL      TipOpe = CodTrgDelete
025600020617     C                   ENDIF
025700020617     C                   IF        TriggerEvent = TrgUpdate
025800020617     C                   EVAL      TipOpe = CodTrgUpdate
025900020617     C                   ENDIF
026000020617     C*
026100020617     C* Compongo la DS DS£ECO da scrivere nella apposita DTAQ
026200020617     C                   CLEAR                   DS£ECO
026300020731     C                   EVAL      D£GTJB = QUSJT04
026400020617     C                   EVAL      D£GSTS = TipOpe
026500020617     C*
026600020617     C* Giro la data da formato MMGGAA a formato AAMMGG
026700020617     C                   MOVEL(P)  RUN_DATE      WRKDATE           6
026800020617     C                   EVAL      WRKDATE = %subst(WRKDATE:5:2) +
026900020617     C                                       %subst(WRKDATE:1:2) +
027000020617     C                                       %subst(WRKDATE:3:2)
027100020617     C                   MOVEL     WRKDATE       D£GDAT
027200020617     C                   EVAL      D£GTIM = RUN_TIME
027300020617     C                   EVAL      D£GUSR = USER
027400020617     C                   EVAL      D£GPGF = depStack
027500020617     C                   EVAL      D£GFIL = File
027600020617     C                   EVAL      D£GMBR = Member
027700020617     C                   EVAL      D£GLIB = Library
027800020617     C*
027900020617     C* A seconda del tipo di operazione sul file gestisco la valorizzaz. della
028000020617     C* DS. Questo xchè il trigger passa nelle immagini vuote nn valori "loval"
028100020617     C* x tipo campo (es.: *blanks x gli ALFA e *zeros x i NUMERICI) ma il
028200020617     C* valore *null.
028300020617     C                   IF        TipOpe = 'U'
028400021209     C                   EVAL      D£PRE1 = %subst(OriRec:001:244)
028500021209     C                   EVAL      D£DRE1 = %subst(NewRec:001:244)
028600030123     C                   EVAL      D£GTRD = D_AR5TRD
028700020617     C                   ENDIF
028800020617     C                   IF        TipOpe = 'I'
028900021209     C                   EVAL      D£DRE1 = %subst(NewRec:001:244)
029000030123     C                   EVAL      D£GTRD = D_AR5TRD
029100020617     C                   ENDIF
029200020617     C                   IF        TipOpe = 'D'
029300021209     C                   EVAL      D£PRE1 = %subst(OriRec:001:244)
029400030123     C                   EVAL      D£GTRD = P_AR5TRD
029500020617     C                   ENDIF
029600020617     C*
029700020617     C                   CALL      'TRECD2R'
029800020617     C                   PARM                    DS£ECO
029900020617     C                   ENDIF
030000021206     C                   ENDIF
030100020617     C*
030200020617     C                   ENDIF
030300020617     C*
030400020607     C*===============================================================
030500020607     C*
030600020612     C* Se sto modificando il file EDPFG/TESTRG0F
030700020612     C                   IF        File = 'TESTRG0F'   AND
030800020607     C                             Library = 'EDPFG'
030900021125     C*
031000021125     C* Valorizzo la DS del file
031100021125     C                   EXSR      VALDS
031200021206     C*
031300030729     C* Elaboro solo se trattasi di operazione per certi tipi record
031400021209     C                   IF        P_AR5TRD = 'BAN' OR
031500040513     C*                            P_AR5TRD = 'TRS' OR
031600030729     C                             P_AR5TRD = 'BNB' OR
031700021209     C                             D_AR5TRD = 'BAN' OR
031800040513     C*                            D_AR5TRD = 'TRS' OR
031900030729     C                             D_AR5TRD = 'BNB'
032000020612     C*
032100020612     C* Elaboro solo se trattasi di chiamata trigger su *AFTER
032200020612     C                   IF        TriggerTime = After
032300020612     C*
032400020612     C                   IF        TriggerEvent = TrgInsert
032500020612     C                   EVAL      TipOpe = CodTrgInsert
032600020612     C                   ENDIF
032700020612     C                   IF        TriggerEvent = TrgDelete
032800020612     C                   EVAL      TipOpe = CodTrgDelete
032900020612     C                   ENDIF
033000020612     C                   IF        TriggerEvent = TrgUpdate
033100020612     C                   EVAL      TipOpe = CodTrgUpdate
033200020612     C                   ENDIF
033300020612     C*
033400020612     C* Compongo la DS DS£ECO da scrivere nella apposita DTAQ
033500020612     C                   CLEAR                   DS£ECO
033600020731     C                   EVAL      D£GTJB = QUSJT04
033700020612     C                   EVAL      D£GSTS = TipOpe
033800020612     C*
033900020612     C* Giro la data da formato MMGGAA a formato AAMMGG
034000020612     C                   MOVEL(P)  RUN_DATE      WRKDATE           6
034100020612     C                   EVAL      WRKDATE = %subst(WRKDATE:5:2) +
034200020612     C                                       %subst(WRKDATE:1:2) +
034300020612     C                                       %subst(WRKDATE:3:2)
034400020612     C                   MOVEL     WRKDATE       D£GDAT
034500020612     C                   EVAL      D£GTIM = RUN_TIME
034600020612     C                   EVAL      D£GUSR = USER
034700020612     C                   EVAL      D£GPGF = depStack
034800020612     C                   EVAL      D£GFIL = File
034900020612     C                   EVAL      D£GMBR = Member
035000020612     C                   EVAL      D£GLIB = Library
035100020612     C*
035200020612     C* A seconda del tipo di operazione sul file gestisco la valorizzaz. della
035300020612     C* DS. Questo xchè il trigger passa nelle immagini vuote nn valori "loval"
035400020612     C* x tipo campo (es.: *blanks x gli ALFA e *zeros x i NUMERICI) ma il
035500020612     C* valore *null.
035600020617     C                   IF        TipOpe = 'U'
035700021209     C                   EVAL      D£PRE1 = %subst(OriRec:001:244)
035800021209     C                   EVAL      D£DRE1 = %subst(NewRec:001:244)
035900030123     C                   EVAL      D£GTRD = D_AR5TRD
036000020617     C                   ENDIF
036100020617     C                   IF        TipOpe = 'I'
036200021209     C                   EVAL      D£DRE1 = %subst(NewRec:001:244)
036300030123     C                   EVAL      D£GTRD = D_AR5TRD
036400020617     C                   ENDIF
036500020617     C                   IF        TipOpe = 'D'
036600021209     C                   EVAL      D£PRE1 = %subst(OriRec:001:244)
036700030123     C                   EVAL      D£GTRD = P_AR5TRD
036800020617     C                   ENDIF
036900020612     C*
037000020617     C                   CALL      'TRECD2R'
037100020617     C                   PARM                    DS£ECO
037200020612     C                   ENDIF
037300021206     C                   ENDIF
037400020612     C*
037500020607     C                   ENDIF
037600020614     C*
037700020614     C*===============================================================
037800020617     C                   ENDIF
037900020614     C*===============================================================
038000020618     C*
038100020618     C* Infine x decidere il tipo di chiusura testo il tipo di job: se
038200020618     C* interattivo chiudo in "RT", altrimenti in "LR".
038300020618     C                   IF        QUSJT04 = 'I'
038400020618     C                   EVAL      *INLR = *ON
038500020618     C                   ELSE
038600020618     C                   EVAL      *INRT = *ON
038700020618     C                   ENDIF
038800020617     C*------------------------------------------------------------------------*
038900020617     C* VALDS - IMPOSTA LE DS A SECONDA DEL FILE A CUI SI RIFERISCONO
039000020617     C*------------------------------------------------------------------------*
039100020617     C     VALDS         BEGSR
039200020617     C*
039300021209     C                   EVAL      Pfiar500ds = OriRec
039400021209     C                   EVAL      Dfiar500ds = NewRec
039500020617     C*
039600020617     C                   ENDSR
039700020607     C*------------------------------------------------------------------------*
039800020607     C* *INZSR - OPERAZIONI INIZIALI
039900020607     C*------------------------------------------------------------------------*
040000020607     C     *INZSR        BEGSR
040100020926     C*
040200020926     C* Inizializzo la viariabile che condizionerà l'intera procedura
040300020926     C                   MOVEL     'S'           W_OK              1
040400020607     C*
040500020607     C* Chiamo il *PGM x reperimento stack di richiami del job
040600020612     C                   CALL      'XRTVSTKR'
040700020607     C                   PARM                    PStack         1000
040800020607     C                   PARM                    PStartPoint       3 0
040900020607     C                   PARM                    PEntries          3 0
041000020607     C*
041100020607     C* Cerco nello stack di richiami i nostroi pgm da monitorare
041200020607     C                   MOVEL     *blanks       depStack         10
041300020607     C                   MOVEL     *blanks       wk1Stack         10
041400020607     C                   MOVEL     *blanks       wk2Stack         10
041500020607     C                   MOVEA     PStack        SKSTACK
041600020607     C                   Z-ADD     1             i                 2 0
041700020607     C* Ciclo x tutti i nostri pgm da monitorare
041800020607     C                   DOW       i <= %elem(OURPGM)
041900020607     C* Ciclo x 1000 pgm dello stack richiami
042000020607     C     1             DO        1000          j                 4 0
042100020607     C* Effettuo considerazioni solo se il pgm dello stack è valido
042200020613     C                   IF        SKSTACK(j) <> *blanks
042300020613     C*
042400020613     C                   IF        %subst(SKSTACK(j):1:1) <> 'Q'   AND
042500020613     C                             %subst(SKSTACK(j):1:3) <> 'BAK'
042600020607     C* Imposto momentaneamente il pgm corrente dello stack
042700020607     C                   EVAL      wk1Stack   =  SKSTACK(j)
042800020607     C* Se ho trovato un nostro pgm lo imposto e continuo la ricerca
042900020607     C                   IF        SKSTACK(J) =  OURPGM(i)
043000020607     C                   EVAL      wk2Stack   =  SKSTACK(j)
043100020607     C                   ENDIF
043200020613     C                   ENDIF
043300020607     C                   ELSE
043400020607     C                   LEAVE
043500020607     C                   ENDIF
043600020607     C                   ENDDO
043700020607     C                   ADD       1             i
043800020607     C                   ENDDO
043900020607     C*
044000020607     C* Al termine della scansione dello stack effettuo considerazioni finali
044100020607     C                   IF        wk2Stack <> *blanks
044200020607     C                   EVAL      depStack =  wk2Stack
044300020607     C                   ELSE
044400020607     C                   EVAL      depStack =  wk1Stack
044500020607     C                   ENDIF
044600020926     C*
044700020926     C* A questo punto determino se il pgm eletto come "chiamante" è 1 d quelli
044800020926     C* x i quali nn si deve fare nulla (nella fattispecie nn scrivere TITASQUE)
044900020926     C     depStack      LOOKUP    PGMNOTHING                             20
045000020926     C                   IF        *IN20 = *on
045100020926     C                   MOVEL     'N'           W_OK
045200020926     C                   ENDIF
045300020618     C*
045400020618     C* Chiamo il *PGM x reperimento informazoni sul job
045500020618     C                   CALL      'QUSRJOBI'
045600020618     C                   PARM                    ReceiveVar
045700020618     C                   PARM                    LenReceiveVar
045800020618     C                   PARM      'JOBI0200'    FormatName
045900020618     C                   PARM      '*'           QualJobName
046000020618     C                   PARM                    InternalJobID
046100020618     C*
046200020618     C                   MOVEL(P)  ReceiveVar    QUSI020000
046300020607     C*
046400020607     C                   ENDSR
046500020607
046600020607
046700020607** OURPGM - NOSTRI PGM DA MONITORARE
046800020607TNSB23R
046900020607TRECD6R
047000020607TNSB36R
047100020607TRECE9R
047200020607TREC78R
047300020607TREC79R
047400020607TNTE50R
047500020607TRECT0R
047600020607TREC11R
047700020607TREC87R
047800020607TREC88R
047900020702TNSF02C
048000020702TNSF04C
048100040607FNLRN4C
048200020926** PGMNOTHING - PGM X I QUALI NN SI DEVE FARE ASSOLUTAMENTE NULLA (NO TITASQUE)
048300030123TNSB36R
048400021105TNSF02C
048500040607FNLRN4C
