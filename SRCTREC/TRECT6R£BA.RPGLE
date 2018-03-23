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
015700031209     C*
015800031209     C* Valorizzo la DS del file
015900031209     C                   EXSR      VALDS
016000020521     C*
016100020521     C* Elaboro solo se trattasi di chiamata trigger su *AFTER
016200020617     C                   IF        TriggerTime = After
016300031209     C*
016400031209     C* se in MODIFICA, elabora solo se alcuni campi sono cambiati
016500031209     C                   IF        TriggerEvent = TrgInsert OR
016600031209     C                             TriggerEvent = TrgDelete OR
016700031209     C                             TriggerEvent = TrgUpdate AND
016800031209     c                             (P_CSBTPI <> D_CSBTPI OR
016900031209     c                              P_CSBVCA <> D_CSBVCA OR
017000031209     c                              P_CSBCAS <> D_CSBCAS)
017100020521     C*
017200020617     C* Trascodifico il tipo di operazione avvenuta sul file
017300020521     C                   IF        TriggerEvent = TrgInsert
017400020521     C                   EVAL      TipOpe = CodTrgInsert
017500020521     C                   ENDIF
017600020521     C                   IF        TriggerEvent = TrgDelete
017700020521     C                   EVAL      TipOpe = CodTrgDelete
017800020521     C                   ENDIF
017900020521     C                   IF        TriggerEvent = TrgUpdate
018000020521     C                   EVAL      TipOpe = CodTrgUpdate
018100020521     C                   ENDIF
018200020521     C*
018300020521     C* Compongo la DS DS£ECO da scrivere nella apposita DTAQ
018400020521     C                   CLEAR                   DS£ECO
018500020731     C                   EVAL      D£GTJB = QUSJT04
018600020521     C                   EVAL      D£GSTS = TipOpe
018700020610     C*
018800020610     C* Giro la data da formato MMGGAA a formato AAMMGG
018900020610     C                   MOVEL(P)  RUN_DATE      WRKDATE           6
019000020610     C                   EVAL      WRKDATE = %subst(WRKDATE:5:2) +
019100020610     C                                       %subst(WRKDATE:1:2) +
019200020610     C                                       %subst(WRKDATE:3:2)
019300020610     C                   MOVEL     WRKDATE       D£GDAT
019400020521     C                   EVAL      D£GTIM = RUN_TIME
019500020521     C                   EVAL      D£GUSR = USER
019600020521     C                   EVAL      D£GPGF = depStack
019700020521     C                   EVAL      D£GFIL = File
019800020521     C                   EVAL      D£GMBR = Member
019900020521     C                   EVAL      D£GLIB = Library
020000020610     C*
020100020610     C* A seconda del tipo di operazione sul file gestisco la valorizzaz. della
020200020610     C* DS. Questo xchè il trigger passa nelle immagini vuote nn valori "loval"
020300020610     C* x tipo campo (es.: *blanks x gli ALFA e *zeros x i NUMERICI) ma il
020400020610     C* valore *null.
020500020610     C                   IF        TipOpe = 'U'
020600020617     C                   EVAL      D£PRE1 = %subst(OriRec:001:214)
020700020617     C                   EVAL      D£DRE1 = %subst(NewRec:001:214)
020800020610     C                   ENDIF
020900020610     C                   IF        TipOpe = 'I'
021000020617     C                   EVAL      D£DRE1 = %subst(NewRec:001:214)
021100020610     C                   ENDIF
021200020610     C                   IF        TipOpe = 'D'
021300020617     C                   EVAL      D£PRE1 = %subst(OriRec:001:214)
021400020610     C                   ENDIF
021500020521     C*
021600020612     C                   CALL      'TRECD2R'
021700020521     C                   PARM                    DS£ECO
021800031209     C                   ENDIF
021900031209     C*
022000020521     C                   ENDIF
022100020521     C*
022200020521     C                   ENDIF
022300020617     C*
022400020617     C*===============================================================
022500020617     C*
022600021125     C* Se sono in EDPFG effettuo altre operazioni specifiche
022700021128     C                   IF        (File = 'TESTRG0F'       AND
022800021128     C                              Library = 'EDPFG')      OR
022900021128     C                             (File = 'TNCSB00F'       AND
023000021128     C                              Library = 'GAITRAGRPS')
023100021125     C*
023200021125     C* Valorizzo la DS del file
023300021125     C                   EXSR      VALDS
023400020617     C*
023500020617     C* Elaboro solo se trattasi di chiamata trigger su *AFTER
023600020617     C                   IF        TriggerTime = After
023700031209     C*
023800031209     C* se in MODIFICA, elabora solo se alcuni campi sono cambiati
023900031209     C                   IF        TriggerEvent = TrgInsert OR
024000031209     C                             TriggerEvent = TrgDelete OR
024100031209     C                             TriggerEvent = TrgUpdate AND
024200031209     c                             (P_CSBTPI <> D_CSBTPI OR
024300031209     c                              P_CSBVCA <> D_CSBVCA OR
024400031209     c                              P_CSBCAS <> D_CSBCAS)
024500020617     C*
024600020617     C* Trascodifico il tipo di operazione avvenuta sul file
024700020617     C                   IF        TriggerEvent = TrgInsert
024800020617     C                   EVAL      TipOpe = CodTrgInsert
024900020617     C                   ENDIF
025000020617     C                   IF        TriggerEvent = TrgDelete
025100020617     C                   EVAL      TipOpe = CodTrgDelete
025200020617     C                   ENDIF
025300020617     C                   IF        TriggerEvent = TrgUpdate
025400020617     C                   EVAL      TipOpe = CodTrgUpdate
025500020617     C                   ENDIF
025600020617     C*
025700020617     C* Compongo la DS DS£ECO da scrivere nella apposita DTAQ
025800020617     C                   CLEAR                   DS£ECO
025900020731     C                   EVAL      D£GTJB = QUSJT04
026000020617     C                   EVAL      D£GSTS = TipOpe
026100020617     C*
026200020617     C* Giro la data da formato MMGGAA a formato AAMMGG
026300020617     C                   MOVEL(P)  RUN_DATE      WRKDATE           6
026400020617     C                   EVAL      WRKDATE = %subst(WRKDATE:5:2) +
026500020617     C                                       %subst(WRKDATE:1:2) +
026600020617     C                                       %subst(WRKDATE:3:2)
026700020617     C                   MOVEL     WRKDATE       D£GDAT
026800020617     C                   EVAL      D£GTIM = RUN_TIME
026900020617     C                   EVAL      D£GUSR = USER
027000020617     C                   EVAL      D£GPGF = depStack
027100020617     C                   EVAL      D£GFIL = File
027200020617     C                   EVAL      D£GMBR = Member
027300020617     C                   EVAL      D£GLIB = Library
027400020617     C*
027500020617     C* A seconda del tipo di operazione sul file gestisco la valorizzaz. della
027600020617     C* DS. Questo xchè il trigger passa nelle immagini vuote nn valori "loval"
027700020617     C* x tipo campo (es.: *blanks x gli ALFA e *zeros x i NUMERICI) ma il
027800020617     C* valore *null.
027900020617     C                   IF        TipOpe = 'U'
028000020617     C                   EVAL      D£PRE1 = %subst(OriRec:001:214)
028100020617     C                   EVAL      D£DRE1 = %subst(NewRec:001:214)
028200020617     C                   ENDIF
028300020617     C                   IF        TipOpe = 'I'
028400020617     C                   EVAL      D£DRE1 = %subst(NewRec:001:214)
028500020617     C                   ENDIF
028600020617     C                   IF        TipOpe = 'D'
028700020617     C                   EVAL      D£PRE1 = %subst(OriRec:001:214)
028800020617     C                   ENDIF
028900020617     C*
029000020617     C                   CALL      'TRECD2R'
029100020617     C                   PARM                    DS£ECO
029200031209     C                   ENDIF
029300031209     C*
029400020617     C                   ENDIF
029500020617     C*
029600020617     C                   ENDIF
029700020614     C*
029800020614     C*===============================================================
029900020617     C                   ENDIF
030000020614     C*===============================================================
030100020607     C*
030200020618     C* Infine x decidere il tipo di chiusura testo il tipo di job: se
030300020618     C* interattivo chiudo in "RT", altrimenti in "LR".
030400020618     C                   IF        QUSJT04 = 'I'
030500020618     C                   EVAL      *INLR = *ON
030600020618     C                   ELSE
030700020618     C                   EVAL      *INRT = *ON
030800020618     C                   ENDIF
030900020617     C*------------------------------------------------------------------------*
031000020617     C* VALDS - IMPOSTA LE DS A SECONDA DEL FILE A CUI SI RIFERISCONO
031100020617     C*------------------------------------------------------------------------*
031200020617     C     VALDS         BEGSR
031300020617     C*
031400020617     C                   EVAL      Ptncsb00ds = OriRec
031500020617     C                   EVAL      Dtncsb00ds = NewRec
031600020617     C*
031700020617     C                   ENDSR
031800020607     C*------------------------------------------------------------------------*
031900020607     C* *INZSR - OPERAZIONI INIZIALI
032000020607     C*------------------------------------------------------------------------*
032100020607     C     *INZSR        BEGSR
032200020607     C*
032300020607     C* Chiamo il *PGM x reperimento stack di richiami del job
032400020612     C                   CALL      'XRTVSTKR'
032500020607     C                   PARM                    PStack         1000
032600020607     C                   PARM                    PStartPoint       3 0
032700020607     C                   PARM                    PEntries          3 0
032800020607     C*
032900020607     C* Cerco nello stack di richiami i nostroi pgm da monitorare
033000020607     C                   MOVEL     *blanks       depStack         10
033100020607     C                   MOVEL     *blanks       wk1Stack         10
033200020607     C                   MOVEL     *blanks       wk2Stack         10
033300020607     C                   MOVEA     PStack        SKSTACK
033400020607     C                   Z-ADD     1             i                 2 0
033500020607     C* Ciclo x tutti i nostri pgm da monitorare
033600020607     C                   DOW       i <= %elem(OURPGM)
033700020607     C* Ciclo x 1000 pgm dello stack richiami
033800020607     C     1             DO        1000          j                 4 0
033900020607     C* Effettuo considerazioni solo se il pgm dello stack è valido
034000020613     C                   IF        SKSTACK(j) <> *blanks
034100020613     C*
034200020613     C                   IF        %subst(SKSTACK(j):1:1) <> 'Q'   AND
034300020613     C                             %subst(SKSTACK(j):1:3) <> 'BAK'
034400020607     C* Imposto momentaneamente il pgm corrente dello stack
034500020607     C                   EVAL      wk1Stack   =  SKSTACK(j)
034600020607     C* Se ho trovato un nostro pgm lo imposto e continuo la ricerca
034700020607     C                   IF        SKSTACK(J) =  OURPGM(i)
034800020607     C                   EVAL      wk2Stack   =  SKSTACK(j)
034900020607     C                   ENDIF
035000020613     C                   ENDIF
035100020607     C                   ELSE
035200020607     C                   LEAVE
035300020607     C                   ENDIF
035400020607     C                   ENDDO
035500020607     C                   ADD       1             i
035600020607     C                   ENDDO
035700020607     C*
035800020607     C* Al termine della scansione dello stack effettuo considerazioni finali
035900020607     C                   IF        wk2Stack <> *blanks
036000020607     C                   EVAL      depStack =  wk2Stack
036100020607     C                   ELSE
036200020607     C                   EVAL      depStack =  wk1Stack
036300020607     C                   ENDIF
036400020618     C*
036500020618     C* Chiamo il *PGM x reperimento informazoni sul job
036600020618     C                   CALL      'QUSRJOBI'
036700020618     C                   PARM                    ReceiveVar
036800020618     C                   PARM                    LenReceiveVar
036900020618     C                   PARM      'JOBI0200'    FormatName
037000020618     C                   PARM      '*'           QualJobName
037100020618     C                   PARM                    InternalJobID
037200020607     C*
037300020618     C                   MOVEL(P)  ReceiveVar    QUSI020000
037400020618     C*
037500020607     C                   ENDSR
037600020607
037700020607
037800020607** OURPGM - NOSTRI PGM DA MONITORARE
037900020607TNSB23R
038000020607TRECD6R
038100020607TNSB36R
038200020607TRECE9R
038300020607TREC78R
038400020607TREC79R
038500020607TNTE50R
038600020607TRECT0R
038700020607TREC11R
038800020607TREC87R
038900020607TREC88R
039000020705TNSF02C
039100020705TNSF04C
