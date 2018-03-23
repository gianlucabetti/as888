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
001500040520     D OURPGM          S             10    DIM(13) CTDATA PERRCD(1)
001600040520     D PGMNOTHING      S             10    DIM(2)  CTDATA PERRCD(1)
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
015600020521     C*===============================================================
015700020521     C*
015800020521     C* Se sono in GAITRAGRU effettuo altre operazioni specifiche
015900020617     C                   IF        (File = 'TITAS00F'     OR
016000020617     C                              File = 'TITAS10F')    AND
016100020617     C                             Library = 'GAITRAGRU'
016200020521     C*
016300020521     C* Elaboro solo se trattasi di chiamata trigger su *AFTER
016400020617     C                   IF        TriggerTime = After
016500020617     C*
016600020617     C* Trascodifico il tipo di operazione avvenuta sul file
016700020521     C                   IF        TriggerEvent = TrgInsert
016800020521     C                   EVAL      TipOpe = CodTrgInsert
016900020521     C                   ENDIF
017000020521     C                   IF        TriggerEvent = TrgDelete
017100020521     C                   EVAL      TipOpe = CodTrgDelete
017200020521     C                   ENDIF
017300020521     C                   IF        TriggerEvent = TrgUpdate
017400020521     C                   EVAL      TipOpe = CodTrgUpdate
017500020521     C                   ENDIF
017600020521     C*
017700020521     C* Compongo la DS DS£ECO da scrivere nella apposita DTAQ
017800020521     C                   CLEAR                   DS£ECO
017900020731     C                   EVAL      D£GTJB = QUSJT04
018000020521     C                   EVAL      D£GSTS = TipOpe
018100020610     C*
018200020610     C* Giro la data da formato MMGGAA a formato AAMMGG
018300020610     C                   MOVEL(P)  RUN_DATE      WRKDATE           6
018400020610     C                   EVAL      WRKDATE = %subst(WRKDATE:5:2) +
018500020610     C                                       %subst(WRKDATE:1:2) +
018600020610     C                                       %subst(WRKDATE:3:2)
018700020610     C                   MOVEL     WRKDATE       D£GDAT
018800020521     C                   EVAL      D£GTIM = RUN_TIME
018900020521     C                   EVAL      D£GUSR = USER
019000020521     C                   EVAL      D£GPGF = depStack
019100020521     C                   EVAL      D£GFIL = File
019200020521     C                   EVAL      D£GMBR = Member
019300020521     C                   EVAL      D£GLIB = Library
019400020610     C*
019500020610     C* A seconda del tipo di operazione sul file gestisco la valorizzaz. della
019600020610     C* DS. Questo xchè il trigger passa nelle immagini vuote nn valori "loval"
019700020610     C* x tipo campo (es.: *blanks x gli ALFA e *zeros x i NUMERICI) ma il
019800020610     C* valore *null.
019900020610     C                   IF        TipOpe = 'U'
020000020617     C                   EVAL      D£PRE1 = %subst(OriRec:001:250)
020100020617     C                   EVAL      D£PRE2 = %subst(OriRec:251:220)
020200020617     C                   EVAL      D£DRE1 = %subst(NewRec:001:250)
020300020617     C                   EVAL      D£DRE2 = %subst(NewRec:251:220)
020400020610     C                   ENDIF
020500020610     C                   IF        TipOpe = 'I'
020600020617     C                   EVAL      D£DRE1 = %subst(NewRec:001:250)
020700020617     C                   EVAL      D£DRE2 = %subst(NewRec:251:220)
020800020610     C                   ENDIF
020900020610     C                   IF        TipOpe = 'D'
021000020617     C                   EVAL      D£PRE1 = %subst(OriRec:001:250)
021100020617     C                   EVAL      D£PRE2 = %subst(OriRec:251:220)
021200020610     C                   ENDIF
021300020521     C*
021400020612     C                   CALL      'TRECD2R'
021500020521     C                   PARM                    DS£ECO
021600020521     C                   ENDIF
021700020521     C*
021800020521     C                   ENDIF
021900020521     C*
022000020617     C*===============================================================
022100020617     C*
022200020617     C* Se sono in GAITRAGRPS effettuo altre operazioni specifiche
022300020617     C                   IF        (File = 'TITAS00F'     OR
022400020617     C                              File = 'TITAS10F')    AND
022500020617     C                             Library = 'GAITRAGRPS'
022600021128     C*
022700021128     C* Valorizzo la DS del file
022800021128     C                   EXSR      VALDS
022900020617     C*
023000020617     C* Elaboro solo se trattasi di chiamata trigger su *AFTER
023100020617     C                   IF        TriggerTime = After
023200031210     C*
023300031210     C* se in MODIFICA, elabora solo se alcuni campi sono cambiati
023400031210     C                   IF        TriggerEvent = TrgInsert OR
023500031210     C                             TriggerEvent = TrgDelete OR
023600031210     C                             TriggerEvent = TrgUpdate AND
023700031210     c                             (P_TASAAS <> D_TASAAS OR
023800031210     c                              P_TASLNP <> D_TASLNP OR
023900031210     c                              P_TASNRS <> D_TASNRS OR
024000031210     c                              P_TASNSP <> D_TASNSP OR
024100031210     c                              P_TASTBL <> D_TASTBL OR
024200031210     c                              P_TASCBO <> D_TASCBO OR
024300031210     c                              P_TASMGS <> D_TASMGS OR
024400031210     c                              P_TASKSC <> D_TASKSC OR
024500031210     c                              P_TASLNA <> D_TASLNA OR
024600031210     c                              P_TASNCL <> D_TASNCL OR
024700031210     c                              P_TASPKB <> D_TASPKB OR
024800031210     c                              P_TASPKC <> D_TASPKC OR
024900031210     c                              P_TASNCP <> D_TASNCP OR
025000031210     c                              P_TASPKF <> D_TASPKF OR
025100031210     c                              P_TASPVL <> D_TASPVL OR
025200031210     c                              P_TASVLB <> D_TASVLB OR
025300031210     c                              P_TASVLC <> D_TASVLC OR
025400031210     c                              P_TASNCR <> D_TASNCR OR
025500031210     c                              P_TASVLF <> D_TASVLF OR
025600031210     c                              P_TASFVF <> D_TASFVF OR
025700031210     c                              P_TASIAS <> D_TASIAS OR
025800031210     c                              P_TASVAS <> D_TASVAS OR
025900031210     c                              P_TASCTR <> D_TASCTR OR
026000031210     c                              P_TASTSP <> D_TASTSP OR
026100031210     c                              P_TASRSD <> D_TASRSD OR
026200031210     c                              P_TASCAD <> D_TASCAD OR
026300031210     c                              P_TASLOD <> D_TASLOD OR
026400031210     c                              P_TASPRD <> D_TASPRD OR
026500031210     c                              P_TASNZD <> D_TASNZD OR
026600031210     c                              P_TASCTS <> D_TASCTS OR
026700031210     c                              P_TASFIN <> D_TASFIN OR
026800031210     c                              P_TASCCM <> D_TASCCM OR
026900031210     c                              P_TASRMN <> D_TASRMN OR
027000031210     c                              P_TASMCT <> D_TASMCT OR
027100031210     c                              P_TASFAP <> D_TASFAP OR
027200031210     c                              P_TASFLP <> D_TASFLP OR
027300031210     c                              P_TASFL2 <> D_TASFL2 OR
027400031210     c                              P_TASTFA <> D_TASTFA OR
027500031210     c                              P_TASTFP <> D_TASTFP OR
027600031210     c                              P_TASSOP <> D_TASSOP OR
027700031210     c                              P_TASPDR <> D_TASPDR OR
027800031210     c                              P_TASDRT <> D_TASDRT OR
027900031210     c                              P_TASNRT <> D_TASNRT OR
028000031210     c                              P_TASSTR <> D_TASSTR OR
028100031210     c                              P_TASCTM <> D_TASCTM OR
028200031210     c                              P_TASDUC <> D_TASDUC OR
028300031210     c                              P_TASDTI <> D_TASDTI OR
028400031210     c                              P_TASPDC <> D_TASPDC OR
028500031210     c                              P_TASNDC <> D_TASNDC OR
028600031210     c                              P_TASDDC <> D_TASDDC OR
028700031210     c                              P_TASSTP <> D_TASSTP OR
028800031210     c                              P_TASDCP <> D_TASDCP OR
028900031210     c                              P_TASDCM <> D_TASDCM OR
029000031210     c                              P_TASCCA <> D_TASCCA OR
029100031210     c                              P_TASNRC <> D_TASNRC OR
029200031210     c                              P_TASFDN <> D_TASFDN OR
029300031210     c                              P_TASFFD <> D_TASFFD OR
029400031210     c                              P_TASFTC <> D_TASFTC OR
029500031210     c                              P_TASTC2 <> D_TASTC2 OR
029600031210     c                              P_TASFBR <> D_TASFBR OR
029700031210     c                              P_TASLL1 <> D_TASLL1 OR
029800031210     c                              P_TASLL2 <> D_TASLL2 OR
029900031210     c                              P_TASFA2 <> D_TASFA2 OR
030000031210     c                              P_TASFCA <> D_TASFCA OR
030100031210     c                              P_TASFA1 <> D_TASFA1 OR
030200031210     c                              P_TASFGC <> D_TASFGC OR
030300031210     c                              P_TASFLO <> D_TASFLO OR
030400031210     c                              P_TASGGS <> D_TASGGS OR
030500031210     c                              P_TASGMA <> D_TASGMA OR
030600031210     c                              P_TASGVA <> D_TASGVA OR
030700031210     c                              P_TASDIV <> D_TASDIV OR
030800031210     c                              P_TASPOR <> D_TASPOR OR
030900031210     c                              P_TASSV1 <> D_TASSV1 OR
031000031210     c                              P_TASVA1 <> D_TASVA1 OR
031100031210     c                              P_TASSV2 <> D_TASSV2 OR
031200031210     c                              P_TASVA2 <> D_TASVA2 OR
031300031210     c                              P_TASSV3 <> D_TASSV3 OR
031400031210     c                              P_TASVA3 <> D_TASVA3 OR
031500031210     c                              P_TASDFT <> D_TASDFT OR
031600031210     c                              P_TASFIV <> D_TASFIV OR
031700031210     c                              P_TASSCL <> D_TASSCL)
031800020617     C*
031900020617     C* Trascodifico il tipo di operazione avvenuta sul file
032000020617     C                   IF        TriggerEvent = TrgInsert
032100020617     C                   EVAL      TipOpe = CodTrgInsert
032200020617     C                   ENDIF
032300020617     C                   IF        TriggerEvent = TrgDelete
032400020617     C                   EVAL      TipOpe = CodTrgDelete
032500020617     C                   ENDIF
032600020617     C                   IF        TriggerEvent = TrgUpdate
032700020617     C                   EVAL      TipOpe = CodTrgUpdate
032800020617     C                   ENDIF
032900020617     C*
033000020617     C* Compongo la DS DS£ECO da scrivere nella apposita DTAQ
033100020617     C                   CLEAR                   DS£ECO
033200020731     C                   EVAL      D£GTJB = QUSJT04
033300020617     C                   EVAL      D£GSTS = TipOpe
033400020617     C*
033500020617     C* Giro la data da formato MMGGAA a formato AAMMGG
033600020617     C                   MOVEL(P)  RUN_DATE      WRKDATE           6
033700020617     C                   EVAL      WRKDATE = %subst(WRKDATE:5:2) +
033800020617     C                                       %subst(WRKDATE:1:2) +
033900020617     C                                       %subst(WRKDATE:3:2)
034000020617     C                   MOVEL     WRKDATE       D£GDAT
034100020617     C                   EVAL      D£GTIM = RUN_TIME
034200020617     C                   EVAL      D£GUSR = USER
034300020617     C                   EVAL      D£GPGF = depStack
034400020617     C                   EVAL      D£GFIL = File
034500020617     C                   EVAL      D£GMBR = Member
034600020617     C                   EVAL      D£GLIB = Library
034700020617     C*
034800020617     C* A seconda del tipo di operazione sul file gestisco la valorizzaz. della
034900020617     C* DS. Questo xchè il trigger passa nelle immagini vuote nn valori "loval"
035000020617     C* x tipo campo (es.: *blanks x gli ALFA e *zeros x i NUMERICI) ma il
035100020617     C* valore *null.
035200020617     C                   IF        TipOpe = 'U'
035300020617     C                   EVAL      D£PRE1 = %subst(OriRec:001:250)
035400020617     C                   EVAL      D£PRE2 = %subst(OriRec:251:220)
035500020617     C                   EVAL      D£DRE1 = %subst(NewRec:001:250)
035600020617     C                   EVAL      D£DRE2 = %subst(NewRec:251:220)
035700020617     C                   ENDIF
035800020617     C                   IF        TipOpe = 'I'
035900020617     C                   EVAL      D£DRE1 = %subst(NewRec:001:250)
036000020617     C                   EVAL      D£DRE2 = %subst(NewRec:251:220)
036100020617     C                   ENDIF
036200020617     C                   IF        TipOpe = 'D'
036300020617     C                   EVAL      D£PRE1 = %subst(OriRec:001:250)
036400020617     C                   EVAL      D£PRE2 = %subst(OriRec:251:220)
036500020617     C                   ENDIF
036600020617     C*
036700020617     C                   CALL      'TRECD2R'
036800020617     C                   PARM                    DS£ECO
036900020617     C                   ENDIF
037000031210     C*
037100031210     C                   ENDIF
037200020617     C*
037300020617     C                   ENDIF
037400020617     C*
037500020607     C*===============================================================
037600020607     C*
037700020612     C* Se sto modificando il file EDPFG/TESTRG0F
037800020612     C                   IF        File = 'TESTRG0F'   AND
037900020607     C                             Library = 'EDPFG'
038000021125     C*
038100021125     C* Valorizzo la DS del file
038200021125     C                   EXSR      VALDS
038300020612     C*
038400020612     C* Elaboro solo se trattasi di chiamata trigger su *AFTER
038500020612     C                   IF        TriggerTime = After
038600031210     C*
038700031210     C* se in MODIFICA, elabora solo se alcuni campi sono cambiati
038800031210     C                   IF        TriggerEvent = TrgInsert OR
038900031210     C                             TriggerEvent = TrgDelete OR
039000031210     C                             TriggerEvent = TrgUpdate AND
039100031210     c                             (P_TASAAS <> D_TASAAS OR
039200031210     c                              P_TASLNP <> D_TASLNP OR
039300031210     c                              P_TASNRS <> D_TASNRS OR
039400031210     c                              P_TASNSP <> D_TASNSP OR
039500031210     c                              P_TASTBL <> D_TASTBL OR
039600031210     c                              P_TASCBO <> D_TASCBO OR
039700031210     c                              P_TASMGS <> D_TASMGS OR
039800031210     c                              P_TASKSC <> D_TASKSC OR
039900031210     c                              P_TASLNA <> D_TASLNA OR
040000031210     c                              P_TASNCL <> D_TASNCL OR
040100031210     c                              P_TASPKB <> D_TASPKB OR
040200031210     c                              P_TASPKC <> D_TASPKC OR
040300031210     c                              P_TASNCP <> D_TASNCP OR
040400031210     c                              P_TASPKF <> D_TASPKF OR
040500031210     c                              P_TASPVL <> D_TASPVL OR
040600031210     c                              P_TASVLB <> D_TASVLB OR
040700031210     c                              P_TASVLC <> D_TASVLC OR
040800031210     c                              P_TASNCR <> D_TASNCR OR
040900031210     c                              P_TASVLF <> D_TASVLF OR
041000031210     c                              P_TASFVF <> D_TASFVF OR
041100031210     c                              P_TASIAS <> D_TASIAS OR
041200031210     c                              P_TASVAS <> D_TASVAS OR
041300031210     c                              P_TASCTR <> D_TASCTR OR
041400031210     c                              P_TASTSP <> D_TASTSP OR
041500031210     c                              P_TASRSD <> D_TASRSD OR
041600031210     c                              P_TASCAD <> D_TASCAD OR
041700031210     c                              P_TASLOD <> D_TASLOD OR
041800031210     c                              P_TASPRD <> D_TASPRD OR
041900031210     c                              P_TASNZD <> D_TASNZD OR
042000031210     c                              P_TASCTS <> D_TASCTS OR
042100031210     c                              P_TASFIN <> D_TASFIN OR
042200031210     c                              P_TASCCM <> D_TASCCM OR
042300031210     c                              P_TASRMN <> D_TASRMN OR
042400031210     c                              P_TASMCT <> D_TASMCT OR
042500031210     c                              P_TASFAP <> D_TASFAP OR
042600031210     c                              P_TASFLP <> D_TASFLP OR
042700031210     c                              P_TASFL2 <> D_TASFL2 OR
042800031210     c                              P_TASTFA <> D_TASTFA OR
042900031210     c                              P_TASTFP <> D_TASTFP OR
043000031210     c                              P_TASSOP <> D_TASSOP OR
043100031210     c                              P_TASPDR <> D_TASPDR OR
043200031210     c                              P_TASDRT <> D_TASDRT OR
043300031210     c                              P_TASNRT <> D_TASNRT OR
043400031210     c                              P_TASSTR <> D_TASSTR OR
043500031210     c                              P_TASCTM <> D_TASCTM OR
043600031210     c                              P_TASDUC <> D_TASDUC OR
043700031210     c                              P_TASDTI <> D_TASDTI OR
043800031210     c                              P_TASPDC <> D_TASPDC OR
043900031210     c                              P_TASNDC <> D_TASNDC OR
044000031210     c                              P_TASDDC <> D_TASDDC OR
044100031210     c                              P_TASSTP <> D_TASSTP OR
044200031210     c                              P_TASDCP <> D_TASDCP OR
044300031210     c                              P_TASDCM <> D_TASDCM OR
044400031210     c                              P_TASCCA <> D_TASCCA OR
044500031210     c                              P_TASNRC <> D_TASNRC OR
044600031210     c                              P_TASFDN <> D_TASFDN OR
044700031210     c                              P_TASFFD <> D_TASFFD OR
044800031210     c                              P_TASFTC <> D_TASFTC OR
044900031210     c                              P_TASTC2 <> D_TASTC2 OR
045000031210     c                              P_TASFBR <> D_TASFBR OR
045100031210     c                              P_TASLL1 <> D_TASLL1 OR
045200031210     c                              P_TASLL2 <> D_TASLL2 OR
045300031210     c                              P_TASFA2 <> D_TASFA2 OR
045400031210     c                              P_TASFCA <> D_TASFCA OR
045500031210     c                              P_TASFA1 <> D_TASFA1 OR
045600031210     c                              P_TASFGC <> D_TASFGC OR
045700031210     c                              P_TASFLO <> D_TASFLO OR
045800031210     c                              P_TASGGS <> D_TASGGS OR
045900031210     c                              P_TASGMA <> D_TASGMA OR
046000031210     c                              P_TASGVA <> D_TASGVA OR
046100031210     c                              P_TASDIV <> D_TASDIV OR
046200031210     c                              P_TASPOR <> D_TASPOR OR
046300031210     c                              P_TASSV1 <> D_TASSV1 OR
046400031210     c                              P_TASVA1 <> D_TASVA1 OR
046500031210     c                              P_TASSV2 <> D_TASSV2 OR
046600031210     c                              P_TASVA2 <> D_TASVA2 OR
046700031210     c                              P_TASSV3 <> D_TASSV3 OR
046800031210     c                              P_TASVA3 <> D_TASVA3 OR
046900031210     c                              P_TASDFT <> D_TASDFT OR
047000031210     c                              P_TASFIV <> D_TASFIV OR
047100031210     c                              P_TASSCL <> D_TASSCL)
047200020612     C*
047300020612     C                   IF        TriggerEvent = TrgInsert
047400020612     C                   EVAL      TipOpe = CodTrgInsert
047500020612     C                   ENDIF
047600020612     C                   IF        TriggerEvent = TrgDelete
047700020612     C                   EVAL      TipOpe = CodTrgDelete
047800020612     C                   ENDIF
047900020612     C                   IF        TriggerEvent = TrgUpdate
048000020612     C                   EVAL      TipOpe = CodTrgUpdate
048100020612     C                   ENDIF
048200020612     C*
048300020612     C* Compongo la DS DS£ECO da scrivere nella apposita DTAQ
048400020612     C                   CLEAR                   DS£ECO
048500020731     C                   EVAL      D£GTJB = QUSJT04
048600020612     C                   EVAL      D£GSTS = TipOpe
048700020612     C*
048800020612     C* Giro la data da formato MMGGAA a formato AAMMGG
048900020612     C                   MOVEL(P)  RUN_DATE      WRKDATE           6
049000020612     C                   EVAL      WRKDATE = %subst(WRKDATE:5:2) +
049100020612     C                                       %subst(WRKDATE:1:2) +
049200020612     C                                       %subst(WRKDATE:3:2)
049300020612     C                   MOVEL     WRKDATE       D£GDAT
049400020612     C                   EVAL      D£GTIM = RUN_TIME
049500020612     C                   EVAL      D£GUSR = USER
049600020612     C                   EVAL      D£GPGF = depStack
049700020612     C                   EVAL      D£GFIL = File
049800020612     C                   EVAL      D£GMBR = Member
049900020612     C                   EVAL      D£GLIB = Library
050000020612     C*
050100020612     C* A seconda del tipo di operazione sul file gestisco la valorizzaz. della
050200020612     C* DS. Questo xchè il trigger passa nelle immagini vuote nn valori "loval"
050300020612     C* x tipo campo (es.: *blanks x gli ALFA e *zeros x i NUMERICI) ma il
050400020612     C* valore *null.
050500020617     C                   IF        TipOpe = 'U'
050600020617     C                   EVAL      D£PRE1 = %subst(OriRec:001:250)
050700020617     C                   EVAL      D£PRE2 = %subst(OriRec:251:220)
050800020617     C                   EVAL      D£DRE1 = %subst(NewRec:001:250)
050900020617     C                   EVAL      D£DRE2 = %subst(NewRec:251:220)
051000020617     C                   ENDIF
051100020617     C                   IF        TipOpe = 'I'
051200020617     C                   EVAL      D£DRE1 = %subst(NewRec:001:250)
051300020617     C                   EVAL      D£DRE2 = %subst(NewRec:251:220)
051400020617     C                   ENDIF
051500020617     C                   IF        TipOpe = 'D'
051600020617     C                   EVAL      D£PRE1 = %subst(OriRec:001:250)
051700020617     C                   EVAL      D£PRE2 = %subst(OriRec:251:220)
051800020617     C                   ENDIF
051900020612     C*
052000020617     C                   CALL      'TRECD2R'
052100020617     C                   PARM                    DS£ECO
052200031210     C                   ENDIF
052300020607     C*
052400020612     C                   ENDIF
052500020612     C*
052600020607     C                   ENDIF
052700020614     C*
052800020614     C*===============================================================
052900020617     C                   ENDIF
053000020614     C*===============================================================
053100020618     C*
053200020618     C* Infine x decidere il tipo di chiusura testo il tipo di job: se
053300020618     C* interattivo chiudo in "RT", altrimenti in "LR".
053400020618     C                   IF        QUSJT04 = 'I'
053500020618     C                   EVAL      *INLR = *ON
053600020618     C                   ELSE
053700020618     C                   EVAL      *INRT = *ON
053800020618     C                   ENDIF
053900020617     C*------------------------------------------------------------------------*
054000020617     C* VALDS - IMPOSTA LE DS A SECONDA DEL FILE A CUI SI RIFERISCONO
054100020617     C*------------------------------------------------------------------------*
054200020617     C     VALDS         BEGSR
054300020617     C*
054400020617     C                   EVAL      Ptitas00ds = OriRec
054500020617     C                   EVAL      Dtitas00ds = NewRec
054600020617     C*
054700020617     C                   ENDSR
054800020607     C*------------------------------------------------------------------------*
054900020607     C* *INZSR - OPERAZIONI INIZIALI
055000020607     C*------------------------------------------------------------------------*
055100020607     C     *INZSR        BEGSR
055200020926     C*
055300020926     C* Inizializzo la viariabile che condizionerà l'intera procedura
055400020926     C                   MOVEL     'S'           W_OK              1
055500020607     C*
055600020607     C* Chiamo il *PGM x reperimento stack di richiami del job
055700020612     C                   CALL      'XRTVSTKR'
055800020607     C                   PARM                    PStack         1000
055900020607     C                   PARM                    PStartPoint       3 0
056000020607     C                   PARM                    PEntries          3 0
056100020607     C*
056200020607     C* Cerco nello stack di richiami i nostroi pgm da monitorare
056300020607     C                   MOVEL     *blanks       depStack         10
056400020607     C                   MOVEL     *blanks       wk1Stack         10
056500020607     C                   MOVEL     *blanks       wk2Stack         10
056600020607     C                   MOVEA     PStack        SKSTACK
056700020607     C                   Z-ADD     1             i                 2 0
056800020607     C* Ciclo x tutti i nostri pgm da monitorare
056900020607     C                   DOW       i <= %elem(OURPGM)
057000020607     C* Ciclo x 1000 pgm dello stack richiami
057100020607     C     1             DO        1000          j                 4 0
057200020607     C* Effettuo considerazioni solo se il pgm dello stack è valido
057300020613     C                   IF        SKSTACK(j) <> *blanks
057400020613     C*
057500020613     C                   IF        %subst(SKSTACK(j):1:1) <> 'Q'   AND
057600020613     C                             %subst(SKSTACK(j):1:3) <> 'BAK'
057700020607     C* Imposto momentaneamente il pgm corrente dello stack
057800020607     C                   EVAL      wk1Stack   =  SKSTACK(j)
057900020607     C* Se ho trovato un nostro pgm lo imposto e continuo la ricerca
058000020607     C                   IF        SKSTACK(J) =  OURPGM(i)
058100020607     C                   EVAL      wk2Stack   =  SKSTACK(j)
058200020607     C                   ENDIF
058300020613     C                   ENDIF
058400020607     C                   ELSE
058500020607     C                   LEAVE
058600020607     C                   ENDIF
058700020607     C                   ENDDO
058800020607     C                   ADD       1             i
058900020607     C                   ENDDO
059000020607     C*
059100020607     C* Al termine della scansione dello stack effettuo considerazioni finali
059200020607     C                   IF        wk2Stack <> *blanks
059300020607     C                   EVAL      depStack =  wk2Stack
059400020607     C                   ELSE
059500020607     C                   EVAL      depStack =  wk1Stack
059600020607     C                   ENDIF
059700020926     C*
059800020926     C* A questo punto determino se il pgm eletto come "chiamante" è 1 d quelli
059900020926     C* x i quali nn si deve fare nulla (nella fattispecie nn scrivere TITASQUE)
060000020926     C     depStack      LOOKUP    PGMNOTHING                             20
060100020926     C                   IF        *IN20 = *on
060200020926     C                   MOVEL     'N'           W_OK
060300020926     C                   ENDIF
060400020618     C*
060500020618     C* Chiamo il *PGM x reperimento informazoni sul job
060600020618     C                   CALL      'QUSRJOBI'
060700020618     C                   PARM                    ReceiveVar
060800020618     C                   PARM                    LenReceiveVar
060900020618     C                   PARM      'JOBI0200'    FormatName
061000020618     C                   PARM      '*'           QualJobName
061100020618     C                   PARM                    InternalJobID
061200020618     C*
061300020618     C                   MOVEL(P)  ReceiveVar    QUSI020000
061400020607     C*
061500020607     C                   ENDSR
061600020607
061700020607
061800130516** OURPGM - sono i pgm che il c/e TRECD* "tratta" diversamente
061900020607TNSB23R
062000020607TRECD6R
062100020607TNSB36R
062200020607TRECE9R
062300020607TREC78R
062400020607TREC79R
062500020607TNTE50R
062600020607TRECT0R
062700020607TREC11R
062800020607TREC87R
062900020607TREC88R
063000020702TNSF02C
063100020702TNSF04C
063200020926** PGMNOTHING - PGM X I QUALI NN SI DEVE FARE ASSOLUTAMENTE NULLA (NO TITASQUE)
063300021223TNSB36R
063400021105TNSF02C
