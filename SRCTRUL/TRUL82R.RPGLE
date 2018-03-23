000100020510     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200020510
000300031013     FTRUL82D   CF   E             WORKSTN USROPN
000400020618
000500031002     D********************************************************************
000600031002     D*Prototypes for calling Retrieve Record Lock API QDBRRCDL - Header
000700031002     D********************************************************************
000800031002     DQDB0100H02       DS
000900031002     D*                                             Qdb RRcd0100 Hdr
001000031002     D QDBJAVL00               1      4B 0
001100031002     D*                                             Jobs Available
001200031002     D QDBJRTN00               5      8B 0
001300031002     D*                                             Jobs Returned
001400031002     D QDBJIO                  9     12B 0
001500031002     D*                                             Job Information Offset
001600031002     D QDBJIS                 13     16B 0
001700031002     D*                                             Job Information Size
001800040315     D******************************************************************
001900031002
002000031002     D********************************************************************
002100031002     D*Prototypes for calling Retrieve Record Lock API QDBRRCDL - Job information
002200031002     D********************************************************************
002300031002     DQDBR010000       DS
002400031002     D*                                             Qdb RRcd0100
002500031002     D QDBJN00                 1     10
002600031002     D*                                             Job Name
002700031002     D QDBJUN00               11     20
002800031002     D*                                             Job User Name
002900031002     D QDBJNBR00              21     26
003000031002     D*                                             Job Number
003100031002     D QDBLS02                27     27
003200031002     D*                                0=Held; 1=Wait
003300031002     D QDBLT00                28     28
003400031002     D*                                0=Read; 1=Update
003500031002     D QDBRNBR01              29     32U 0
003600031002     D*                                             Record Number
003700040315     D******************************************************************
003800020618
003900020618     D******************************************************************
004000020618     D*Record structure for QUSRJOBI JOBI0200 format
004100020618     D******************************************************************
004200020618     DQUSI020000       DS
004300020618     D*                                             Qwc JOBI0200
004400020618     D QUSBR01                 1      4B 0
004500020618     D*                                             Bytes Return
004600020618     D QUSBA01                 5      8B 0
004700020618     D*                                             Bytes Avail
004800020618     D QUSJN03                 9     18
004900020618     D*                                             Job Name
005000020618     D QUSUN03                19     28
005100020618     D*                                             User Name
005200020618     D QUSJNBR03              29     34
005300020618     D*                                             Job Number
005400020618     D QUSIJID01              35     50
005500020618     D*                                             Int Job ID
005600020618     D QUSJS05                51     60
005700020618     D*                                             Job Status
005800020618     D QUSJT04                61     61
005900020618     D*                                             Job Type
006000020618     D QUSJS06                62     62
006100020618     D*                                             Job Subtype
006200020618     D QUSSN                  63     72
006300020618     D*                                             Subsys Name
006400020618     D QUSRP01                73     76B 0
006500020618     D*                                             Run Priority
006600020618     D QUSSPID00              77     80B 0
006700020618     D*                                             System Pool ID
006800020618     D QUSCPUU00              81     84B 0
006900020618     D*                                             CPU Used
007000020618     D QUSAIOR                85     88B 0
007100020618     D*                                             Aux IO Request
007200020618     D QUSIT                  89     92B 0
007300020618     D*                                             Interact Trans
007400020618     D QUSRT                  93     96B 0
007500020618     D*                                             Response Time
007600020618     D QUSFT                  97     97
007700020618     D*                                             Function Type
007800020618     D QUSFN15                98    107
007900020618     D*                                             Function Name
008000020618     D QUSAJS                108    111
008100020618     D*                                             Active Job Stat
008200020618     D QUSNDBLW              112    115B 0
008300020618     D*                                             Num DBase Lock Wts
008400020618     D QUSNIMLW              116    119B 0
008500020618     D*                                             Num Internal Mch Lck Wts
008600020618     D QUSNDBLW00            120    123B 0
008700020618     D*                                             Num Non DBase Lock Wts
008800020618     D QUSTDBLW              124    127B 0
008900020618     D*                                             Wait Time DBase Lock Wts
009000020618     D QUSTIMLW              128    131B 0
009100020618     D*                                             Wait Time Internal Mch Lck W
009200020618     D QUSNDBLW01            132    135B 0
009300020618     D*                                             Wait Time Non DBase Lock Wts
009400020618     D QUSERVED45            136    136
009500020618     D*                                             Reserved
009600020618     D QUSCSPID              137    140B 0
009700020618     D*                                             Current System Pool ID
009800020618     D QUSTC01               141    144B 0
009900020618     D*                                             Thread Count
010000040315     D******************************************************************
010100040315
010200031002
010300031002     D*****************************************************************
010400031002     D* Definizione formato DS QSEC x ritorno esito API
010500031002     D*****************************************************************
010600031002     DQUSEC            DS
010700031002     D*                                             Qus EC
010800031002     D QUSBPRV                 1      4B 0
010900031002     D*                                             Bytes Provided
011000031002     D QUSBAVL                 5      8B 0
011100031002     D*                                             Bytes Available
011200031002     D QUSEI                   9     15
011300031002     D*                                             Exception Id
011400031002     D QUSERVED               16     16
011500031002     D*                                             Reserved
011600031002     D*QUSED01                17     17
011700031002     D*
011800031002     D*                                      Varying length
011900031002
012000031002     D*****************************************************************
012100031002     D* Definizione formato DS x API QEZSNDMG
012200031002     D*****************************************************************
012300031002     D MsgType         S             10                                         Message type
012400031002     D                                     INZ('*INFO')
012500031002     D DeliveryMode    S             10                                         Deluvery mode
012600031002     D                                     INZ('*BREAK')
012700070830     D MessageText     S            256                                         Message text
012800031002     D LenMsgText      S             10I 0                                      Length of text
012900031002     D                                     INZ(%SIZE(MessageText))
013000031002     D UserList        S             10                                         User profile
013100031002     D                                     DIM(299)
013200031002     D NumberUsr       S             10I 0                                      Number of user
013300031002     D MsgIndicator    S             10I 0                                      Message sent indic.
013400031002     D FunctionReq     S             10I 0                                      Function requested
013500031002     D SendOK          S              1                                         Show display
013600031002     D                                     INZ('N')
013700031002     D MsgQueue        S             20                                         Qualified MSGQ name
013800031002     D TypeIndicator   S              4                                         Name type indicator
013900040315
014000031002
014100031002     D***
014200031002     D* Definizione parametri x API QUSRJOBI e QDBRRCDL
014300031002     D***
014400031002     D ReceiveVarL     S             48
014500040315     D LenReceiveVarL  S             10i 0 INZ(%size(ReceiveVarL))
014600031002     D ReceiveVarJ     S            200
014700040315     D LenReceiveVarJ  S             10i 0 INZ(%size(ReceiveVarJ))
014800031002     D FormatName      S              8
014900031002     D QualJobName     S             26
015000031002     D InternalJobID   S             16
015100031002     D FileLibName     S             20
015200031002     D MemberName      S             10
015300040315     D RelRecordNum    S             10i 0
015400031020
015500031020     D***
015600031020     D* Definizione parametri x API QMHSNDM
015700031020     D***
015800031021     D MSGID           S              7    INZ('GAI0001')
015900031021     D MSGFIL          S             20    INZ('MSGFGAI   GAITRAOBJ ')
016000031021     D MSGDTA          S            512
016100031021     D MSGDLN          S             10I 0 INZ(%SIZE(MSGDTA))
016200031020     D MSGTYP          S             10    INZ('*INFO     ')
016300031020     D MSGQ            S             20    INZ('*SYSOPR             ')
016400031021     D MSGQn           S             10I 0 INZ(1)
016500031020     D RPYMQ           S             20
016600031020     D MSGKEY          S              4
016700031010
016800031010     D***
016900031010     D* Definizione prototipazione x API (in C) Sleep()
017000031010     D***
017100031010     D sleep           PR            10U 0 extproc('sleep')
017200031010     D   seconds                     10U 0 value
017300020618
017400031002     D***
017500031002     D* DS ridefinizione nome file e libreria file di cui verificare le allocazioni
017600031002     D***
017700031002     D DSFileLib       DS
017800031002     D  FileName                     10
017900031002     D  LibName                      10
018000031002
018100031002     D***
018200031002     D* DS ridefinizione qualificatore job x reperimento informazioni sullo stesso
018300031002     D***
018400031002     D DSIDJob         DS
018500031002     D  JobName                      10
018600031002     D  JobUsrName                   10
018700031002     D  JobNumber                     6
018800070829
018900070829      ***
019000070829      * Status DS
019100070829      ***
019200070829     d Status         sds
019300070830     d  SDSprm           *parms
019400070830     d  SDSjnm               244    253
019500070830     d  SDSusr               254    263
019600070830     d  SDSjnr               264    269s 0
019700031002
019800031002     D***
019900031002     D* DS di procedura
020000031002     D***
020100031013     D TRUL82DS      E DS
020200031002
020300031002     D***
020400031002     D* Variabili di wrk
020500031002     D***
020600031002     D wTipoJobCorr    S              1
020700031002     D wTipoJobAlck    S              1
020800031002     D wNumRecAlloc    S             10
020900031009     D wMsgDft         S            120
021000031006     D wTentativi      S              2  0 INZ(*zeros)
021100020515
021200020510
021300031002     C*------------------------------------------------------------------------*
021400031002     C* MAIN
021500031002     C*------------------------------------------------------------------------*
021600031002     C*
021700031002     C     *ENTRY        PLIST
021800031013     C                   PARM                    TRUL82DS
021900070829     c                   parm                    §UL82txt2       120
022000060303     C*
022100060303     C* Innanzitutto inizializzo il numero d tentativi correnti
022200060303     C                   CLEAR                   wTentativi
022300031006     C*
022400060303     C* Ciclo almeno 1 volta e x tutti i tentativi richiesti nei parametri d ingresso
022500070829do  1C                   DOW       wTentativi <= UL82§NUM
022600031006     C*
022700031006     C* Incremento il numero d tentativi effettuati
022800031006     C                   EVAL      wTentativi = wTentativi + 1
022900031002     C*
023000031002     C* Eseguo routine di inizializzazione tutte DS di procedura
023100031002     C                   EXSR      INZWDS
023200031002     C*
023300031002     C* Se nn specificato il file vado direttamente a fine *PGM
023400070829if  2C                   IF        UL82§FIL <> *blanks
023500031002     C*
023600031002     C* Se nn specificata libreria assumo x default *LIBL
023700031013     C                   IF        UL82§LIB = *blanks
023800031013     C                   EVAL      UL82§LIB = '*LIBL'
023900031002     C                   ENDIF
024000031002     C*
024100031002     C* Se nn specificato membro assumo x default *FIRST
024200031013     C                   IF        UL82§MBR = *blanks
024300031013     C                   EVAL      UL82§MBR = '*FIRST'
024400031002     C                   ENDIF
024500031002     C*
024600031002     C* Imposto i parametri x la chiamata all'API QDBRRCDL
024700031013     C                   EVAL      FileName = UL82§FIL
024800031013     C                   EVAL      LibName  = UL82§LIB
024900031002     C*
025000031002     C* Inizializzo e imposto la DS x la gestione esiti chiamate API (QUSEC)
025100031002     C                   CLEAR                   QUSEC
025200031002     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
025300031002     C*
025400031002     C* Chiamo l'API x il reperimento dei job che allocano record del file/libreria specificato
025500031020     C   55              EVAL      FormatName = 'RRCD0100'
025600031020     C  N55              EVAL      FormatName = 'RCDL0100'
025700031002     C                   CALL      'QDBRRCDL'
025800031002     C                   PARM                    ReceiveVarL
025900031002     C                   PARM                    LenReceiveVarL
026000031020     C                   PARM                    FormatName
026100031002     C                   PARM      DSFileLib     FileLibName
026200031013     C                   PARM      UL82§MBR      MemberName
026300040315     C                   PARM      UL82§RRN      RelRecordNum
026400031002     C                   PARM                    QUSEC
026500031002     C*
026600031002     C* Valorizza Header di ritorno API
026700031002     C                   EVAL      QDB0100H02=ReceiveVarL
026800031002     C*
026900031002     C* Verifico se errore d impostazione parametri
027000070829if  3C                   IF        ReceiveVarL = *blanks
027100040315     C                   EVAL      UL82§STS = *blanks
027200031013     C                   EVAL      UL82§ERR = '1'
027300031013     C                   EVAL      UL82§ERD = %trim(UL82§LIB)+'/'+
027400031013     C                                        %trim(UL82§FIL)+'('+
027500031013     C                                        %trim(UL82§MBR)+')'+
027600031002     C                             ' non trovato oppure è una vista logica'
027700070829x   3C                   ELSE
027800031006     C*
027900031006     C* Se effettivamente c'è qualche job che alloca record nel file richiesto, proseguo
028000070829if  4C                   IF        QDBJAVL00 > 0
028100031020     C   55              EVAL      QDBR010000=
028200031002     C                             %subst(ReceiveVarL:QDBJIO+1:QDBJIS)
028300031020     C  N55              EVAL      QDBR010000=
028400031020     C                             %subst(ReceiveVarL:8+1:28)
028500031002     C*
028600031006     C* Imposto i parametri x la chiamata all'API QUSRJOBI
028700031002     C                   EVAL      JobName    = QDBJN00
028800031002     C                   EVAL      JobUsrName = QDBJUN00
028900031002     C                   EVAL      JobNumber  = QDBJNBR00
029000031002     C*
029100031002     C* Chiamo l'API x reperimento informazoni sul job allocante il libreria/file/membro
029200031002     C                   CALL(E)   'QUSRJOBI'
029300031002     C                   PARM                    ReceiveVarJ
029400031002     C                   PARM                    LenReceiveVarJ
029500031002     C                   PARM      'JOBI0200'    FormatName
029600031002     C                   PARM      DSIDJob       QualJobName
029700031002     C                   PARM                    InternalJobID
029800031002     C*
029900031002     C* Gestisco l'errore in fase d reperimento degli attributi del job "allocante"
030000070829if  5C                   IF        %error
030100040315     C                   EVAL      UL82§STS = *blanks
030200031013     C                   EVAL      UL82§ERR = '0'
030300031013     C                   EVAL      UL82§ERD = 'Il lavoro '+
030400031002     C                                        %trim(JobNumber)+'/'+
030500031002     C                                        %trim(JobUsrName)+'/'+
030600031002     C                                        %trim(JobName)+
030700031002     C                             ' non è più nel sistema.'
030800031006     C* Se nn esiste + il vincolo d allocazione, esco anche dal ciclo principale dei tentativi
030900031006     C                   LEAVE
031000070829x   5C                   ELSE
031100031006     C* A questo punto ho tutte le informazini necessarie x decidere come procedere
031200031006     C                   MOVEL(P)  ReceiveVarJ   QUSI020000
031300031002     C                   EVAL      wTipoJobAlck = QUSJT04
031400031006     C* Valorizzo i parametri di OUT relativi al 'ID del job e al *PGM "allocante"
031500031013     C                   EVAL      UL82§JNAM  = QUSJN03
031600031013     C                   EVAL      UL82§JUSR  = QUSUN03
031700031013     C                   EVAL      UL82§JNUM  = QUSJNBR03
031800031013     C                   EVAL      UL82§PGMN  = QUSFN15
031900140127      *---
032000031006     C* Eseguo routine di gestione allocazione
032100031002     C                   EXSR      GESTALCK
032200140127      *---
032300070829e   5C                   ENDIF
032400070829x   4C                   ELSE
032500040315     C                   EVAL      UL82§STS = *blanks
032600031013     C                   EVAL      UL82§ERR = '0'
032700031013     C                   EVAL      UL82§ERD = 'Nessun record allocato '+
032800031013     C                                        'nel file '+%trim(UL82§LIB)+'/'+
032900031013     C                                        %trim(UL82§FIL)+'('+
033000031013     C                                        %trim(UL82§MBR)+')'
033100031006     C* Se nn esiste + il vincolo d allocazione, esco anche dal ciclo principale dei tentativi
033200031006     C                   LEAVE
033300070829e   4C                   ENDIF
033400070829e   3C                   ENDIF
033500070829e   2C                   ENDIF
033600031006     C*
033700031006     C* Al termine, prima d proseguire con il ciclo d tentativi richiesti, effetto la pausa
033800031006     C* richiesta nei parametri d lancio
033900031013     C                   CALLP     sleep(UL82§ATT)
034000031006     C*
034100070829e   1C                   ENDDO
034200020618     C*
034300031002     C* Infine x decidere il tipo di chiusura testo il tipo del job corrente:
034400060217     C* se interattivo chiudo in "LR", altrimenti in "RT".
034500031002     C                   IF        wTipoJobCorr = 'I'
034600031002     C                   EVAL      *INLR = *ON
034700031002     C                   ELSE
034800031002     C                   EVAL      *INRT = *ON
034900031002     C                   ENDIF
035000031002     C*
035100031002     C*------------------------------------------------------------------------*
035200031002     C* INZWDS - INIZIALIZZAZIONE DS DI PROCEDURA
035300031002     C*------------------------------------------------------------------------*
035400031002     C     INZWDS        BEGSR
035500031002     C*
035600031002     C                   CLEAR                   DSFileLib
035700031002     C                   CLEAR                   DSIDJob
035800031002     C                   CLEAR                   QDB0100H02
035900031002     C                   CLEAR                   QDBR010000
036000031002     C                   CLEAR                   QUSI020000
036100031002     C*
036200031002     C                   ENDSR
036300031002     C*------------------------------------------------------------------------*
036400031002     C* GESTALCK - GESTIONE PROCEDURA
036500031002     C*------------------------------------------------------------------------*
036600031002     C     GESTALCK      BEGSR
036700031013     C*
036800031013     C* Imposto subito il messagio d default
036900031020     C                   CLEAR                   wNumRecAlloc
037000031020     C   55              MOVEL     QDBRNBR01     wNumRecAlloc
037100031020     C                   EVAL      wMsgDft = 'Record '+%trim(wNumRecAlloc)+
037200031020     C                             ' del file '+%trim(FileName)           +
037300031013     C                             ' è impegnato dal lavoro '             +
037400031013     C                             %trim(QDBJNBR00)+'/'+%trim(QDBJUN00)   +
037500031013     C                             '/'+%trim(QDBJN00)
037600031002     C*
037700031002     C* Inizializzo gli indicatori che condizionano la window
037800031002     C                   SETOFF                                       2527
037900031002     C*
038000031002     C* Se richiesto emissione videata e trattasi d job (corrente) INTERATTIVO apro il file video
038100070829if  1C                   IF        wTipoJobCorr = 'I' AND
038200031013     C                             UL82§WIN = 'S'
038300031013     C                   IF        not %open(TRUL82D)
038400031013     C                   OPEN      TRUL82D
038500031002     C                   ENDIF
038600031002     C*
038700031002     C* Se richiesta emissione messaggio in window personalizzato lo imposto altrimenti lascio
038800031002     C* il messaggio di default
038900050404     C                   EVAL      STSMS1 = %subst(wMsgDft:1:40)
039000050404     C                   EVAL      STSMS2 = %subst(wMsgDft:41:40)
039100050404     C                   EVAL      STSMS3 = %subst(wMsgDft:81:40)
039200031013     C                   IF        UL82§MSW <> *blanks
039300050404     C                   EVAL      STSMS4 = %subst(UL82§MSW:1:40)
039400050404     C                   EVAL      STSMS5 = %subst(UL82§MSW:41:40)
039500050404     C                   ENDIF
039600031002     C*
039700060217     C* Se richiesto nei parametri abilito il tasto F5 per MSG a QSYSOPR
039800031013     C                   IF        UL82§F5 = 'S'
039900031002     C                   SETON                                        25
040000031002     C                   ENDIF
040100031002     C*
040200031002     C* Se richiesto nei parametri abilito il tasto F7 per MSG al JOB
040300031013     C                   IF        UL82§F7 = 'S'
040400031002     C                   SETON                                        27
040500031002     C                   ENDIF
040600031002     C*
040700140127     C*------
040800140127     C*  invio,una volta,automaticamente all'utente che sta allocando
040900140127     C*   il msg di avviso.... poi posso inviarlo nuovamente se abilitato F7 a video
041000140127     c                   if          fare_una_volta = *blank
041100140127     C                   EXSR      EXEF7
041200140127     c                   eval        fare_una_volta = 'S'
041300140127     c                   end
041400140127     C*------
041500140127     C*
041600031002     C* Fino a che nn si preme il tasto funzione F12 ciclo
041700070829do  2C     *INKL         DOWEQ     '0'
041800031006     C*
041900031013     C  NKL              EXFMT     UL82W1
042000031002     C*
042100031002     C* Gestione tasto funzione F5 (invio messaggio alla coda dell'operatore)
042200031002     C     *INKE         IFEQ      *ON
042300031002     C                   EXSR      EXEF5
042400031002     C                   ENDIF
042500031002     C*
042600031002     C* Gestione tasto funzione F7 (invio messaggio al job allocante)
042700031002     C     *INKG         IFEQ      *ON
042800031002     C                   EXSR      EXEF7
042900031002     C                   ENDIF
043000031013     C*
043100031013     C* Eseguo routine d verifica vincolo d'allocazione ancora attivo
043200031013     C                   EXSR      CHKLCK
043300031002     C*
043400070829e   2C                   ENDDO
043500031002     C*
043600031002     C* Chiudo il display-file
043700031013     C                   IF        %open(TRUL82D)
043800031013     C                   CLOSE     TRUL82D
043900031002     C                   ENDIF
044000031002     C*
044100031002     C* Se invece trattasi O d job nn INTERATTIVO O nn è richiesta l'emissione della windows
044200031002     C* gestisco solo i flag d richiesta invii messaggi
044300070829x   1C                   ELSE
044400031006     C*
044500031013     C                   IF        UL82§F5 = 'S'
044600031002     C                   EXSR      EXEF5
044700031002     C                   ENDIF
044800031013     C                   IF        UL82§F7 = 'S'
044900031002     C                   EXSR      EXEF7
045000031002     C                   ENDIF
045100031013     C*
045200031013     C* Eseguo routine d verifica vincolo d'allocazione ancora attivo
045300031013     C                   EXSR      CHKLCK
045400070829e   1C                   ENDIF
045500031002     C*
045600031002     C                   ENDSR
045700031002     C*------------------------------------------------------------------------*
045800031002     C* EXEF5 - GESTIONE TASTO FUNZIONE "F5" (invio msg alla coda operatore)
045900031002     C*------------------------------------------------------------------------*
046000031002     C     EXEF5         BEGSR
046100031002     C*
046200060217     C* Imposto il parametro di ritorno indicante che l'utente ha premuto l'F5
046300031013     C                   EVAL      UL82§OF5 = 'S'
046400031002     C*
046500031020     C*                  CALL      'QEZSNDMG'
046600031020     C*                  PARM                    MsgType
046700031020     C*                  PARM                    DeliveryMode
046800031020     C*                  PARM      wMsgDft       MessageText
046900070830     C*                  PARM      120           LenMsgText
047000031020     C*                  PARM      '*SYSOPR'     UserList
047100031020     C*                  PARM      1             NumberUsr
047200031020     C*                  PARM                    MsgIndicator
047300031020     C*                  PARM                    FunctionReq
047400031020     C*                  PARM                    Qusec
047500031020     C*                  PARM                    SendOK
047600031020     C*                  PARM                    MsgQueue
047700031020     C*                  PARM      '*USR'        TypeIndicator
047800031020     C*
047900031020     C* Inizializzo e imposto la DS x la gestione esiti chiamate API (QUSEC)
048000031020     C                   CLEAR                   QUSEC
048100031020     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
048200031020     C*
048300031021     C                   CALL      'QMHSNDM'
048400031021     C                   PARM                    MSGID
048500031020     C                   PARM                    MSGFIL
048600031020     C                   PARM      wMsgDft       MSGDTA
048700031020     C                   PARM                    MSGDLN
048800031020     C                   PARM                    MSGTYP
048900031020     C                   PARM                    MSGQ
049000031020     C                   PARM                    MSGQn
049100031020     C                   PARM      *BLANKS       RPYMQ
049200031020     C                   PARM                    MSGKEY
049300031020     C                   PARM                    QUSEC
049400031002     C*
049500031002     C                   ENDSR
049600031002     C*------------------------------------------------------------------------*
049700031002     C* EXEF7 - GESTIONE TASTO FUNZIONE "F7" (invio msg al job allocante)
049800031002     C*------------------------------------------------------------------------*
049900031002     C     EXEF7         BEGSR
050000031002     C*
050100060217     C* Imposto il parametro di ritorno indicante che l'utente ha premuto l'F7
050200031013     C                   EVAL      UL82§OF7 = 'S'
050300031009     C*
050400031009     C* Imposto il messaggio parametrizzato richiesto oppure quello standard
050500031013     C                   IF        UL82§MSS <> *blanks
050600031013     C                   EVAL      MessageText = UL82§MSS
050700031009     C                   ELSE
050800031009     C                   EVAL      MessageText = wMsgDft
050900031009     C                   ENDIF
051000031002     C*
051100031002     C                   CALL      'QEZSNDMG'
051200031002     C                   PARM                    MsgType
051300031002     C                   PARM                    DeliveryMode
051400031009     C                   PARM                    MessageText
051500070830     C                   PARM      120           LenMsgText
051600031002     C                   PARM      QDBJN00       UserList
051700031002     C                   PARM      1             NumberUsr
051800031002     C                   PARM                    MsgIndicator
051900031002     C                   PARM                    FunctionReq
052000031002     C                   PARM                    Qusec
052100031002     C                   PARM                    SendOK
052200031002     C                   PARM                    MsgQueue
052300031002     C                   PARM      '*DSP'        TypeIndicator
052400070830      *
052500070830      * Invia messaggio di avviso all'utente "chiamante"
052600070830      *   (se richiesto con UL82§win = "X")
052700070830if  1c                   IF             UL82§win  = 'X'
052800070830     c                   exsr      SndMsgUsrCall
052900070830e   1c                   ENDIF
053000031002     C*
053100031002     C                   ENDSR
053200031006     C*------------------------------------------------------------------------*
053300031006     C* CHKLCK - VERIFICA ALLOCAZIONE LIB/FILE(MBR) RICHIESTO
053400031006     C*------------------------------------------------------------------------*
053500031006     C     CHKLCK        BEGSR
053600031006     C*
053700031006     C* Inizializzo e imposto la DS x la gestione esiti chiamate API (QUSEC)
053800031006     C                   CLEAR                   QUSEC
053900031006     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
054000031006     C*
054100031006     C* Chiamo l'API x il reperimento dei job che allocano record del file/libreria specificato
054200031020     C   55              EVAL      FormatName = 'RRCD0100'
054300031020     C  N55              EVAL      FormatName = 'RCDL0100'
054400031006     C                   CALL      'QDBRRCDL'
054500031006     C                   PARM                    ReceiveVarL
054600031006     C                   PARM                    LenReceiveVarL
054700031020     C                   PARM                    FormatName
054800031006     C                   PARM      DSFileLib     FileLibName
054900031013     C                   PARM      UL82§MBR      MemberName
055000040315     C                   PARM      UL82§RRN      RelRecordNum
055100031006     C                   PARM                    QUSEC
055200031006     C*
055300031006     C* Valorizza Header di ritorno API
055400031006     C                   EVAL      QDB0100H02=ReceiveVarL
055500031006     C*
055600031006     C* Se nn c sono + allocazioni simulo la pressione del tasto F12 ed esco anche dal ciclo
055700031006     C* principale della gestione dei tentativi
055800031006     C                   IF        QDBJAVL00 = 0
055900031006     C                   EVAL      *INKL = *ON
056000031013     C                   EVAL      wTentativi = UL82§NUM + 1
056100031006     C*
056200031006     C* Inoltre ritorno l'esito nella DS d ritorno parametri
056300040315     C                   EVAL      UL82§STS = *blanks
056400031013     C                   EVAL      UL82§ERR = '0'
056500031013     C                   EVAL      UL82§ERD = 'Nessun record allocato '+
056600031013     C                                        'nel file '+%trim(UL82§LIB)+'/'+
056700031013     C                                        %trim(UL82§FIL)+'('+
056800031013     C                                        %trim(UL82§MBR)+')'
056900040315     C                   ELSE
057000040315     C                   EVAL      UL82§STS = 'A'
057100040315     C                   EVAL      UL82§ERR = '0'
057200040315     C                   EVAL      UL82§ERD = 'Record ancora allocato '+
057300040315     C                                        'nel file '+%trim(UL82§LIB)+'/'+
057400040315     C                                        %trim(UL82§FIL)+'('+
057500040315     C                                        %trim(UL82§MBR)+')'
057600031006     C                   ENDIF
057700031006     C*
057800031006     C                   ENDSR
057900070830      *------------------------------------------------------------------------*
058000070830      * SNDMSG_CALLER - Manda un messaggio di avviso all'utente del
058100070830      *                 programma chiamante
058200070830      *------------------------------------------------------------------------*
058300070830     c     SndMsgUsrCall BEGSR
058400070830      *
058500070830sel 1c                   select
058600070830w   1c                   when      SDSprm      > 1
058700070830     c                   eval      MessageText = 'Lavoro '
058800070830     c                                         + %trim(UL82§jnam)
058900070830     c                                         + ' - Utente '
059000070830     c                                         + %trim(UL82§jusr)
059100070830     c                                         + ' => '
059200070830     c                                         + §UL82txt2
059300070830w   1c                   when      UL82§msw   <> *blanks
059400070830     c                   eval      MessageText = UL82§msw
059500070830x   1c                   other
059600070830     c                   eval      MessageText = 'Il lavoro '
059700070830     c                                         + %trim(UL82§jnam)
059800070830     c                                         + ' dell''utente '
059900070830     c                                         + %trim(UL82§jusr)
060000070830     c                                         + ' sta bloccando il +
060100070830     c                                           tuo lavoro '
060200070830     c                                         + SDSjnm
060300070830e   1c                   endsl
060400070830      *
060500070830     c                   clear                   Qusec
060600070830      *
060700070830     c                   call      'QEZSNDMG'
060800070830     c                   parm      '*INFO'       MsgType
060900070830     c                   parm      '*BREAK'      DeliveryMode
061000070830     c                   parm                    MessageText
061100070830     c                   parm      256           LenMsgText
061200070830     c                   parm      SDSusr        UserList
061300070830     c                   parm      1             NumberUsr
061400070830     c                   parm                    MsgIndicator
061500070830     c                   parm                    FunctionReq
061600070830     c                   parm                    Qusec
061700070830     c                   parm      'N'           SendOK
061800070830     c                   parm                    MsgQueue
061900070830     c                   parm      '*USR'        TypeIndicator
062000070830      *
062100070830     c                   ENDSR
062200020607     C*------------------------------------------------------------------------*
062300020607     C* *INZSR - OPERAZIONI INIZIALI
062400020607     C*------------------------------------------------------------------------*
062500020607     C     *INZSR        BEGSR
062600020618     C*
062700031002     C* Chiamo l'API x reperimento informazoni sul job corrente, in questo caso mi serve sapere
062800031002     C* se il job corrente è INTERATTIVO o BATCH
062900031002     C                   CALL      'QUSRJOBI'
063000031002     C                   PARM                    ReceiveVarJ
063100031002     C                   PARM                    LenReceiveVarJ
063200031002     C                   PARM      'JOBI0200'    FormatName
063300031002     C                   PARM      '*'           QualJobName
063400031002     C                   PARM                    InternalJobID
063500031002     C*
063600031002     C                   MOVEL(P)  ReceiveVarJ   QUSI020000
063700031002     C                   EVAL      wTipoJobCorr = QUSJT04
063800031020     C*
063900031020     C* Reperisco la DTAARA QSS1MRI che indica la versione d OS/400 presente sull'AS/400 corrente
064000031020     C     *DTAARA       DEFINE    QSS1MRI       OSVersion       750
064100031020     C                   IN(E)     *DTAARA
064200031020     C                   IF        %error
064300031020     C* Se reperimento DTAARA termina in errore assumo d essere in SEDE (in virtù dell'AS UNICO)
064400031020     C                   SETON                                        55
064500031020     C                   ELSE
064600031020     C* Se reperimento DTAARA ok => verifico la versione d OS/400 corrente
064700031020     C                   IF        %subst(OSVersion:1:4) = 'V4R3'
064800031020     C                   SETOFF                                       55
064900031020     C                   ELSE
065000031020     C                   SETON                                        55
065100031020     C                   ENDIF
065200031020     C                   ENDIF
065300020607     C*
065400140127     C*  Inizializza qui il flag di FARE ALMENO UNA VOLTA (la prima volta)
065500140127     c                   clear                   fare_una_volta    1
065600140127     C*
065700020607     C                   ENDSR
