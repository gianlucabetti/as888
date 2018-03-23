000100020510     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200020510
000300031013     FTRUL82D   CF   E             WORKSTN USROPN
000400020515
000500020618
000600031002     D********************************************************************
000700031002     D*Prototypes for calling Retrieve Record Lock API QDBRRCDL - Header
000800031002     D********************************************************************
000900031002     DQDB0100H02       DS
001000031002     D*                                             Qdb RRcd0100 Hdr
001100031002     D QDBJAVL00               1      4B 0
001200031002     D*                                             Jobs Available
001300031002     D QDBJRTN00               5      8B 0
001400031002     D*                                             Jobs Returned
001500031002     D QDBJIO                  9     12B 0
001600031002     D*                                             Job Information Offset
001700031002     D QDBJIS                 13     16B 0
001800031002     D*                                             Job Information Size
001900040315     D******************************************************************
002000031002
002100031002     D********************************************************************
002200031002     D*Prototypes for calling Retrieve Record Lock API QDBRRCDL - Job information
002300031002     D********************************************************************
002400031002     DQDBR010000       DS
002500031002     D*                                             Qdb RRcd0100
002600031002     D QDBJN00                 1     10
002700031002     D*                                             Job Name
002800031002     D QDBJUN00               11     20
002900031002     D*                                             Job User Name
003000031002     D QDBJNBR00              21     26
003100031002     D*                                             Job Number
003200031002     D QDBLS02                27     27
003300031002     D*                                0=Held; 1=Wait
003400031002     D QDBLT00                28     28
003500031002     D*                                0=Read; 1=Update
003600031002     D QDBRNBR01              29     32U 0
003700031002     D*                                             Record Number
003800040315     D******************************************************************
003900020618
004000020618     D******************************************************************
004100020618     D*Record structure for QUSRJOBI JOBI0200 format
004200020618     D******************************************************************
004300020618     DQUSI020000       DS
004400020618     D*                                             Qwc JOBI0200
004500020618     D QUSBR01                 1      4B 0
004600020618     D*                                             Bytes Return
004700020618     D QUSBA01                 5      8B 0
004800020618     D*                                             Bytes Avail
004900020618     D QUSJN03                 9     18
005000020618     D*                                             Job Name
005100020618     D QUSUN03                19     28
005200020618     D*                                             User Name
005300020618     D QUSJNBR03              29     34
005400020618     D*                                             Job Number
005500020618     D QUSIJID01              35     50
005600020618     D*                                             Int Job ID
005700020618     D QUSJS05                51     60
005800020618     D*                                             Job Status
005900020618     D QUSJT04                61     61
006000020618     D*                                             Job Type
006100020618     D QUSJS06                62     62
006200020618     D*                                             Job Subtype
006300020618     D QUSSN                  63     72
006400020618     D*                                             Subsys Name
006500020618     D QUSRP01                73     76B 0
006600020618     D*                                             Run Priority
006700020618     D QUSSPID00              77     80B 0
006800020618     D*                                             System Pool ID
006900020618     D QUSCPUU00              81     84B 0
007000020618     D*                                             CPU Used
007100020618     D QUSAIOR                85     88B 0
007200020618     D*                                             Aux IO Request
007300020618     D QUSIT                  89     92B 0
007400020618     D*                                             Interact Trans
007500020618     D QUSRT                  93     96B 0
007600020618     D*                                             Response Time
007700020618     D QUSFT                  97     97
007800020618     D*                                             Function Type
007900020618     D QUSFN15                98    107
008000020618     D*                                             Function Name
008100020618     D QUSAJS                108    111
008200020618     D*                                             Active Job Stat
008300020618     D QUSNDBLW              112    115B 0
008400020618     D*                                             Num DBase Lock Wts
008500020618     D QUSNIMLW              116    119B 0
008600020618     D*                                             Num Internal Mch Lck Wts
008700020618     D QUSNDBLW00            120    123B 0
008800020618     D*                                             Num Non DBase Lock Wts
008900020618     D QUSTDBLW              124    127B 0
009000020618     D*                                             Wait Time DBase Lock Wts
009100020618     D QUSTIMLW              128    131B 0
009200020618     D*                                             Wait Time Internal Mch Lck W
009300020618     D QUSNDBLW01            132    135B 0
009400020618     D*                                             Wait Time Non DBase Lock Wts
009500020618     D QUSERVED45            136    136
009600020618     D*                                             Reserved
009700020618     D QUSCSPID              137    140B 0
009800020618     D*                                             Current System Pool ID
009900020618     D QUSTC01               141    144B 0
010000020618     D*                                             Thread Count
010100040315     D******************************************************************
010200040315
010300031002
010400031002     D*****************************************************************
010500031002     D* Definizione formato DS QSEC x ritorno esito API
010600031002     D*****************************************************************
010700031002     DQUSEC            DS
010800031002     D*                                             Qus EC
010900031002     D QUSBPRV                 1      4B 0
011000031002     D*                                             Bytes Provided
011100031002     D QUSBAVL                 5      8B 0
011200031002     D*                                             Bytes Available
011300031002     D QUSEI                   9     15
011400031002     D*                                             Exception Id
011500031002     D QUSERVED               16     16
011600031002     D*                                             Reserved
011700031002     D*QUSED01                17     17
011800031002     D*
011900031002     D*                                      Varying length
012000031002
012100031002     D*****************************************************************
012200031002     D* Definizione formato DS x API QEZSNDMG
012300031002     D*****************************************************************
012400031002     D MsgType         S             10                                         Message type
012500031002     D                                     INZ('*INFO')
012600031002     D DeliveryMode    S             10                                         Deluvery mode
012700031002     D                                     INZ('*BREAK')
012800031009     D MessageText     S            120                                         Message text
012900031002     D LenMsgText      S             10I 0                                      Length of text
013000031002     D                                     INZ(%SIZE(MessageText))
013100031002     D UserList        S             10                                         User profile
013200031002     D                                     DIM(299)
013300031002     D NumberUsr       S             10I 0                                      Number of user
013400031002     D MsgIndicator    S             10I 0                                      Message sent indic.
013500031002     D FunctionReq     S             10I 0                                      Function requested
013600031002     D SendOK          S              1                                         Show display
013700031002     D                                     INZ('N')
013800031002     D MsgQueue        S             20                                         Qualified MSGQ name
013900031002     D TypeIndicator   S              4                                         Name type indicator
014000040315
014100031002
014200031002     D***
014300031002     D* Definizione parametri x API QUSRJOBI e QDBRRCDL
014400031002     D***
014500031002     D ReceiveVarL     S             48
014600040315     D LenReceiveVarL  S             10i 0 INZ(%size(ReceiveVarL))
014700031002     D ReceiveVarJ     S            200
014800040315     D LenReceiveVarJ  S             10i 0 INZ(%size(ReceiveVarJ))
014900031002     D FormatName      S              8
015000031002     D QualJobName     S             26
015100031002     D InternalJobID   S             16
015200031002     D FileLibName     S             20
015300031002     D MemberName      S             10
015400040315     D RelRecordNum    S             10i 0
015500031020
015600031020     D***
015700031020     D* Definizione parametri x API QMHSNDM
015800031020     D***
015900031021     D MSGID           S              7    INZ('GAI0001')
016000031021     D MSGFIL          S             20    INZ('MSGFGAI   GAITRAOBJ ')
016100031021     D MSGDTA          S            512
016200031021     D MSGDLN          S             10I 0 INZ(%SIZE(MSGDTA))
016300031020     D MSGTYP          S             10    INZ('*INFO     ')
016400031020     D MSGQ            S             20    INZ('*SYSOPR             ')
016500031021     D MSGQn           S             10I 0 INZ(1)
016600031020     D RPYMQ           S             20
016700031020     D MSGKEY          S              4
016800031010
016900031010     D***
017000031010     D* Definizione prototipazione x API (in C) Sleep()
017100031010     D***
017200031010     D sleep           PR            10U 0 extproc('sleep')
017300031010     D   seconds                     10U 0 value
017400020618
017500031002     D***
017600031002     D* DS ridefinizione nome file e libreria file di cui verificare le allocazioni
017700031002     D***
017800031002     D DSFileLib       DS
017900031002     D  FileName                     10
018000031002     D  LibName                      10
018100031002
018200031002     D***
018300031002     D* DS ridefinizione qualificatore job x reperimento informazioni sullo stesso
018400031002     D***
018500031002     D DSIDJob         DS
018600031002     D  JobName                      10
018700031002     D  JobUsrName                   10
018800031002     D  JobNumber                     6
018900031002
019000031002     D***
019100031002     D* DS di procedura
019200031002     D***
019300031013     D TRUL82DS      E DS
019400031002
019500031002     D***
019600031002     D* Variabili di wrk
019700031002     D***
019800031002     D wTipoJobCorr    S              1
019900031002     D wTipoJobAlck    S              1
020000031002     D wNumRecAlloc    S             10
020100031009     D wMsgDft         S            120
020200031006     D wTentativi      S              2  0 INZ(*zeros)
020300020515
020400020510
020500031002     C*------------------------------------------------------------------------*
020600031002     C* MAIN
020700031002     C*------------------------------------------------------------------------*
020800031002     C*
020900031002     C     *ENTRY        PLIST
021000031013     C                   PARM                    TRUL82DS
021100060303     C*
021200060303     C* Innanzitutto inizializzo il numero d tentativi correnti
021300060303     C                   CLEAR                   wTentativi
021400031006     C*
021500060303     C* Ciclo almeno 1 volta e x tutti i tentativi richiesti nei parametri d ingresso
021600031013     C                   DOW       wTentativi <= UL82§NUM
021700031006     C*
021800031006     C* Incremento il numero d tentativi effettuati
021900031006     C                   EVAL      wTentativi = wTentativi + 1
022000031002     C*
022100031002     C* Eseguo routine di inizializzazione tutte DS di procedura
022200031002     C                   EXSR      INZWDS
022300031002     C*
022400031002     C* Se nn specificato il file vado direttamente a fine *PGM
022500031013     C                   IF        UL82§FIL <> *blanks
022600031002     C*
022700031002     C* Se nn specificata libreria assumo x default *LIBL
022800031013     C                   IF        UL82§LIB = *blanks
022900031013     C                   EVAL      UL82§LIB = '*LIBL'
023000031002     C                   ENDIF
023100031002     C*
023200031002     C* Se nn specificato membro assumo x default *FIRST
023300031013     C                   IF        UL82§MBR = *blanks
023400031013     C                   EVAL      UL82§MBR = '*FIRST'
023500031002     C                   ENDIF
023600031002     C*
023700031002     C* Imposto i parametri x la chiamata all'API QDBRRCDL
023800031013     C                   EVAL      FileName = UL82§FIL
023900031013     C                   EVAL      LibName  = UL82§LIB
024000031002     C*
024100031002     C* Inizializzo e imposto la DS x la gestione esiti chiamate API (QUSEC)
024200031002     C                   CLEAR                   QUSEC
024300031002     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
024400031002     C*
024500031002     C* Chiamo l'API x il reperimento dei job che allocano record del file/libreria specificato
024600031020     C   55              EVAL      FormatName = 'RRCD0100'
024700031020     C  N55              EVAL      FormatName = 'RCDL0100'
024800031002     C                   CALL      'QDBRRCDL'
024900031002     C                   PARM                    ReceiveVarL
025000031002     C                   PARM                    LenReceiveVarL
025100031020     C                   PARM                    FormatName
025200031002     C                   PARM      DSFileLib     FileLibName
025300031013     C                   PARM      UL82§MBR      MemberName
025400040315     C                   PARM      UL82§RRN      RelRecordNum
025500031002     C                   PARM                    QUSEC
025600031002     C*
025700031002     C* Valorizza Header di ritorno API
025800031002     C                   EVAL      QDB0100H02=ReceiveVarL
025900031002     C*
026000031002     C* Verifico se errore d impostazione parametri
026100031002     C                   IF        ReceiveVarL = *blanks
026200040315     C                   EVAL      UL82§STS = *blanks
026300031013     C                   EVAL      UL82§ERR = '1'
026400031013     C                   EVAL      UL82§ERD = %trim(UL82§LIB)+'/'+
026500031013     C                                        %trim(UL82§FIL)+'('+
026600031013     C                                        %trim(UL82§MBR)+')'+
026700031002     C                             ' non trovato oppure è una vista logica'
026800031002     C                   ELSE
026900031006     C*
027000031006     C* Se effettivamente c'è qualche job che alloca record nel file richiesto, proseguo
027100031006     C                   IF        QDBJAVL00 > 0
027200031020     C   55              EVAL      QDBR010000=
027300031002     C                             %subst(ReceiveVarL:QDBJIO+1:QDBJIS)
027400031020     C  N55              EVAL      QDBR010000=
027500031020     C                             %subst(ReceiveVarL:8+1:28)
027600031002     C*
027700031006     C* Imposto i parametri x la chiamata all'API QUSRJOBI
027800031002     C                   EVAL      JobName    = QDBJN00
027900031002     C                   EVAL      JobUsrName = QDBJUN00
028000031002     C                   EVAL      JobNumber  = QDBJNBR00
028100031002     C*
028200031002     C* Chiamo l'API x reperimento informazoni sul job allocante il libreria/file/membro
028300031002     C                   CALL(E)   'QUSRJOBI'
028400031002     C                   PARM                    ReceiveVarJ
028500031002     C                   PARM                    LenReceiveVarJ
028600031002     C                   PARM      'JOBI0200'    FormatName
028700031002     C                   PARM      DSIDJob       QualJobName
028800031002     C                   PARM                    InternalJobID
028900031002     C*
029000031002     C* Gestisco l'errore in fase d reperimento degli attributi del job "allocante"
029100031002     C                   IF        %error
029200040315     C                   EVAL      UL82§STS = *blanks
029300031013     C                   EVAL      UL82§ERR = '0'
029400031013     C                   EVAL      UL82§ERD = 'Il lavoro '+
029500031002     C                                        %trim(JobNumber)+'/'+
029600031002     C                                        %trim(JobUsrName)+'/'+
029700031002     C                                        %trim(JobName)+
029800031002     C                             ' non è più nel sistema.'
029900031006     C* Se nn esiste + il vincolo d allocazione, esco anche dal ciclo principale dei tentativi
030000031006     C                   LEAVE
030100031002     C                   ELSE
030200031006     C* A questo punto ho tutte le informazini necessarie x decidere come procedere
030300031006     C                   MOVEL(P)  ReceiveVarJ   QUSI020000
030400031002     C                   EVAL      wTipoJobAlck = QUSJT04
030500031006     C* Valorizzo i parametri di OUT relativi al 'ID del job e al *PGM "allocante"
030600031013     C                   EVAL      UL82§JNAM  = QUSJN03
030700031013     C                   EVAL      UL82§JUSR  = QUSUN03
030800031013     C                   EVAL      UL82§JNUM  = QUSJNBR03
030900031013     C                   EVAL      UL82§PGMN  = QUSFN15
031000031006     C* Eseguo routine di gestione allocazione
031100031002     C                   EXSR      GESTALCK
031200031006     C                   ENDIF
031300031002     C                   ELSE
031400040315     C                   EVAL      UL82§STS = *blanks
031500031013     C                   EVAL      UL82§ERR = '0'
031600031013     C                   EVAL      UL82§ERD = 'Nessun record allocato '+
031700031013     C                                        'nel file '+%trim(UL82§LIB)+'/'+
031800031013     C                                        %trim(UL82§FIL)+'('+
031900031013     C                                        %trim(UL82§MBR)+')'
032000031006     C* Se nn esiste + il vincolo d allocazione, esco anche dal ciclo principale dei tentativi
032100031006     C                   LEAVE
032200031002     C                   ENDIF
032300031002     C                   ENDIF
032400031002     C                   ENDIF
032500031006     C*
032600031006     C* Al termine, prima d proseguire con il ciclo d tentativi richiesti, effetto la pausa
032700031006     C* richiesta nei parametri d lancio
032800031013     C                   CALLP     sleep(UL82§ATT)
032900031006     C*
033000031006     C                   ENDDO
033100020618     C*
033200031002     C* Infine x decidere il tipo di chiusura testo il tipo del job corrente:
033300060217     C* se interattivo chiudo in "LR", altrimenti in "RT".
033400031002     C                   IF        wTipoJobCorr = 'I'
033500031002     C                   EVAL      *INLR = *ON
033600031002     C                   ELSE
033700031002     C                   EVAL      *INRT = *ON
033800031002     C                   ENDIF
033900031002     C*
034000031002     C*------------------------------------------------------------------------*
034100031002     C* INZWDS - INIZIALIZZAZIONE DS DI PROCEDURA
034200031002     C*------------------------------------------------------------------------*
034300031002     C     INZWDS        BEGSR
034400031002     C*
034500031002     C                   CLEAR                   DSFileLib
034600031002     C                   CLEAR                   DSIDJob
034700031002     C                   CLEAR                   QDB0100H02
034800031002     C                   CLEAR                   QDBR010000
034900031002     C                   CLEAR                   QUSI020000
035000031002     C*
035100031002     C                   ENDSR
035200031002     C*------------------------------------------------------------------------*
035300031002     C* GESTALCK - GESTIONE PROCEDURA
035400031002     C*------------------------------------------------------------------------*
035500031002     C     GESTALCK      BEGSR
035600031013     C*
035700031013     C* Imposto subito il messagio d default
035800031020     C                   CLEAR                   wNumRecAlloc
035900031020     C   55              MOVEL     QDBRNBR01     wNumRecAlloc
036000031020     C                   EVAL      wMsgDft = 'Record '+%trim(wNumRecAlloc)+
036100031020     C                             ' del file '+%trim(FileName)           +
036200031013     C                             ' è impegnato dal lavoro '             +
036300031013     C                             %trim(QDBJNBR00)+'/'+%trim(QDBJUN00)   +
036400031013     C                             '/'+%trim(QDBJN00)
036500031002     C*
036600031002     C* Inizializzo gli indicatori che condizionano la window
036700031002     C                   SETOFF                                       2527
036800031002     C*
036900031002     C* Se richiesto emissione videata e trattasi d job (corrente) INTERATTIVO apro il file video
037000031002     C                   IF        wTipoJobCorr = 'I' AND
037100031013     C                             UL82§WIN = 'S'
037200031013     C                   IF        not %open(TRUL82D)
037300031013     C                   OPEN      TRUL82D
037400031002     C                   ENDIF
037500031002     C*
037600031002     C* Se richiesta emissione messaggio in window personalizzato lo imposto altrimenti lascio
037700031002     C* il messaggio di default
037800050404     C                   EVAL      STSMS1 = %subst(wMsgDft:1:40)
037900050404     C                   EVAL      STSMS2 = %subst(wMsgDft:41:40)
038000050404     C                   EVAL      STSMS3 = %subst(wMsgDft:81:40)
038100031013     C                   IF        UL82§MSW <> *blanks
038200050404     C                   EVAL      STSMS4 = %subst(UL82§MSW:1:40)
038300050404     C                   EVAL      STSMS5 = %subst(UL82§MSW:41:40)
038400050404     C                   ENDIF
038500031002     C*
038600060217     C* Se richiesto nei parametri abilito il tasto F5 per MSG a QSYSOPR
038700031013     C                   IF        UL82§F5 = 'S'
038800031002     C                   SETON                                        25
038900031002     C                   ENDIF
039000031002     C*
039100031002     C* Se richiesto nei parametri abilito il tasto F7 per MSG al JOB
039200031013     C                   IF        UL82§F7 = 'S'
039300031002     C                   SETON                                        27
039400031002     C                   ENDIF
039500031002     C*
039600031002     C* Fino a che nn si preme il tasto funzione F12 ciclo
039700031002     C     *INKL         DOWEQ     '0'
039800031006     C*
039900031013     C  NKL              EXFMT     UL82W1
040000031002     C*
040100031002     C* Gestione tasto funzione F5 (invio messaggio alla coda dell'operatore)
040200031002     C     *INKE         IFEQ      *ON
040300031002     C                   EXSR      EXEF5
040400031002     C                   ENDIF
040500031002     C*
040600031002     C* Gestione tasto funzione F7 (invio messaggio al job allocante)
040700031002     C     *INKG         IFEQ      *ON
040800031002     C                   EXSR      EXEF7
040900031002     C                   ENDIF
041000031013     C*
041100031013     C* Eseguo routine d verifica vincolo d'allocazione ancora attivo
041200031013     C                   EXSR      CHKLCK
041300031002     C*
041400031002     C                   ENDDO
041500031002     C*
041600031002     C* Chiudo il display-file
041700031013     C                   IF        %open(TRUL82D)
041800031013     C                   CLOSE     TRUL82D
041900031002     C                   ENDIF
042000031002     C*
042100031002     C* Se invece trattasi O d job nn INTERATTIVO O nn è richiesta l'emissione della windows
042200031002     C* gestisco solo i flag d richiesta invii messaggi
042300031002     C                   ELSE
042400031006     C*
042500031013     C                   IF        UL82§F5 = 'S'
042600031002     C                   EXSR      EXEF5
042700031002     C                   ENDIF
042800031013     C                   IF        UL82§F7 = 'S'
042900031002     C                   EXSR      EXEF7
043000031002     C                   ENDIF
043100031013     C*
043200031013     C* Eseguo routine d verifica vincolo d'allocazione ancora attivo
043300031013     C                   EXSR      CHKLCK
043400031002     C                   ENDIF
043500031002     C*
043600031002     C                   ENDSR
043700031002     C*------------------------------------------------------------------------*
043800031002     C* EXEF5 - GESTIONE TASTO FUNZIONE "F5" (invio msg alla coda operatore)
043900031002     C*------------------------------------------------------------------------*
044000031002     C     EXEF5         BEGSR
044100031002     C*
044200060217     C* Imposto il parametro di ritorno indicante che l'utente ha premuto l'F5
044300031013     C                   EVAL      UL82§OF5 = 'S'
044400031002     C*
044500031020     C*                  CALL      'QEZSNDMG'
044600031020     C*                  PARM                    MsgType
044700031020     C*                  PARM                    DeliveryMode
044800031020     C*                  PARM      wMsgDft       MessageText
044900031020     C*                  PARM      120           LenMsgText
045000031020     C*                  PARM      '*SYSOPR'     UserList
045100031020     C*                  PARM      1             NumberUsr
045200031020     C*                  PARM                    MsgIndicator
045300031020     C*                  PARM                    FunctionReq
045400031020     C*                  PARM                    Qusec
045500031020     C*                  PARM                    SendOK
045600031020     C*                  PARM                    MsgQueue
045700031020     C*                  PARM      '*USR'        TypeIndicator
045800031020     C*
045900031020     C* Inizializzo e imposto la DS x la gestione esiti chiamate API (QUSEC)
046000031020     C                   CLEAR                   QUSEC
046100031020     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
046200031020     C*
046300031021     C                   CALL      'QMHSNDM'
046400031021     C                   PARM                    MSGID
046500031020     C                   PARM                    MSGFIL
046600031020     C                   PARM      wMsgDft       MSGDTA
046700031020     C                   PARM                    MSGDLN
046800031020     C                   PARM                    MSGTYP
046900031020     C                   PARM                    MSGQ
047000031020     C                   PARM                    MSGQn
047100031020     C                   PARM      *BLANKS       RPYMQ
047200031020     C                   PARM                    MSGKEY
047300031020     C                   PARM                    QUSEC
047400031002     C*
047500031002     C                   ENDSR
047600031002     C*------------------------------------------------------------------------*
047700031002     C* EXEF7 - GESTIONE TASTO FUNZIONE "F7" (invio msg al job allocante)
047800031002     C*------------------------------------------------------------------------*
047900031002     C     EXEF7         BEGSR
048000031002     C*
048100060217     C* Imposto il parametro di ritorno indicante che l'utente ha premuto l'F7
048200031013     C                   EVAL      UL82§OF7 = 'S'
048300031009     C*
048400031009     C* Imposto il messaggio parametrizzato richiesto oppure quello standard
048500031013     C                   IF        UL82§MSS <> *blanks
048600031013     C                   EVAL      MessageText = UL82§MSS
048700031009     C                   ELSE
048800031009     C                   EVAL      MessageText = wMsgDft
048900031009     C                   ENDIF
049000031002     C*
049100031002     C                   CALL      'QEZSNDMG'
049200031002     C                   PARM                    MsgType
049300031002     C                   PARM                    DeliveryMode
049400031009     C                   PARM                    MessageText
049500031009     C                   PARM      120           LenMsgText
049600031002     C                   PARM      QDBJN00       UserList
049700031002     C                   PARM      1             NumberUsr
049800031002     C                   PARM                    MsgIndicator
049900031002     C                   PARM                    FunctionReq
050000031002     C                   PARM                    Qusec
050100031002     C                   PARM                    SendOK
050200031002     C                   PARM                    MsgQueue
050300031002     C                   PARM      '*DSP'        TypeIndicator
050400031002     C*
050500031002     C                   ENDSR
050600031006     C*------------------------------------------------------------------------*
050700031006     C* CHKLCK - VERIFICA ALLOCAZIONE LIB/FILE(MBR) RICHIESTO
050800031006     C*------------------------------------------------------------------------*
050900031006     C     CHKLCK        BEGSR
051000031006     C*
051100031006     C* Inizializzo e imposto la DS x la gestione esiti chiamate API (QUSEC)
051200031006     C                   CLEAR                   QUSEC
051300031006     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
051400031006     C*
051500031006     C* Chiamo l'API x il reperimento dei job che allocano record del file/libreria specificato
051600031020     C   55              EVAL      FormatName = 'RRCD0100'
051700031020     C  N55              EVAL      FormatName = 'RCDL0100'
051800031006     C                   CALL      'QDBRRCDL'
051900031006     C                   PARM                    ReceiveVarL
052000031006     C                   PARM                    LenReceiveVarL
052100031020     C                   PARM                    FormatName
052200031006     C                   PARM      DSFileLib     FileLibName
052300031013     C                   PARM      UL82§MBR      MemberName
052400040315     C                   PARM      UL82§RRN      RelRecordNum
052500031006     C                   PARM                    QUSEC
052600031006     C*
052700031006     C* Valorizza Header di ritorno API
052800031006     C                   EVAL      QDB0100H02=ReceiveVarL
052900031006     C*
053000031006     C* Se nn c sono + allocazioni simulo la pressione del tasto F12 ed esco anche dal ciclo
053100031006     C* principale della gestione dei tentativi
053200031006     C                   IF        QDBJAVL00 = 0
053300031006     C                   EVAL      *INKL = *ON
053400031013     C                   EVAL      wTentativi = UL82§NUM + 1
053500031006     C*
053600031006     C* Inoltre ritorno l'esito nella DS d ritorno parametri
053700040315     C                   EVAL      UL82§STS = *blanks
053800031013     C                   EVAL      UL82§ERR = '0'
053900031013     C                   EVAL      UL82§ERD = 'Nessun record allocato '+
054000031013     C                                        'nel file '+%trim(UL82§LIB)+'/'+
054100031013     C                                        %trim(UL82§FIL)+'('+
054200031013     C                                        %trim(UL82§MBR)+')'
054300040315     C                   ELSE
054400040315     C                   EVAL      UL82§STS = 'A'
054500040315     C                   EVAL      UL82§ERR = '0'
054600040315     C                   EVAL      UL82§ERD = 'Record ancora allocato '+
054700040315     C                                        'nel file '+%trim(UL82§LIB)+'/'+
054800040315     C                                        %trim(UL82§FIL)+'('+
054900040315     C                                        %trim(UL82§MBR)+')'
055000031006     C                   ENDIF
055100031006     C*
055200031006     C                   ENDSR
055300020607     C*------------------------------------------------------------------------*
055400020607     C* *INZSR - OPERAZIONI INIZIALI
055500020607     C*------------------------------------------------------------------------*
055600020607     C     *INZSR        BEGSR
055700020618     C*
055800031002     C* Chiamo l'API x reperimento informazoni sul job corrente, in questo caso mi serve sapere
055900031002     C* se il job corrente è INTERATTIVO o BATCH
056000031002     C                   CALL      'QUSRJOBI'
056100031002     C                   PARM                    ReceiveVarJ
056200031002     C                   PARM                    LenReceiveVarJ
056300031002     C                   PARM      'JOBI0200'    FormatName
056400031002     C                   PARM      '*'           QualJobName
056500031002     C                   PARM                    InternalJobID
056600031002     C*
056700031002     C                   MOVEL(P)  ReceiveVarJ   QUSI020000
056800031002     C                   EVAL      wTipoJobCorr = QUSJT04
056900031020     C*
057000031020     C* Reperisco la DTAARA QSS1MRI che indica la versione d OS/400 presente sull'AS/400 corrente
057100031020     C     *DTAARA       DEFINE    QSS1MRI       OSVersion       750
057200031020     C                   IN(E)     *DTAARA
057300031020     C                   IF        %error
057400031020     C* Se reperimento DTAARA termina in errore assumo d essere in SEDE (in virtù dell'AS UNICO)
057500031020     C                   SETON                                        55
057600031020     C                   ELSE
057700031020     C* Se reperimento DTAARA ok => verifico la versione d OS/400 corrente
057800031020     C                   IF        %subst(OSVersion:1:4) = 'V4R3'
057900031020     C                   SETOFF                                       55
058000031020     C                   ELSE
058100031020     C                   SETON                                        55
058200031020     C                   ENDIF
058300031020     C                   ENDIF
058400020607     C*
058500020607     C                   ENDSR
