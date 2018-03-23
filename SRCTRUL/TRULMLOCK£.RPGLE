000100030610      /TITLE Invio messaggio a utenti che allocano oggetto.
000200030610     ***********************************************************************
000300030610     **
000400030610     ** Questo programma reperisce i lavori che stanno allocando un oggetto,
000500030610     ** invia un messaggio interruzione ad un massimo di 299 utenti
000600030610     ** e restituisce il numero di allocazioni.
000700030610     **
000800030610     ** Lo compilo in un proprio gruppo di attivazione per evitare che un
000900030610     ** RCLRSC mi faccia perdere il valore delle variabili.
001000030610     **
001100030610     ** --------------------------------------------------------------------
001200030610     **
001300030610     ** PARAMETRI DI INPUT
001400030610     **
001500030610     ** PrmFase     Fase del controllo.
001600030610     **             '1' = stop;
001700030610     **             '5' = sollecito stop;
001800030610     **             '9' = via libera.
001900030610     ** PrmObj      Nome dell'oggetto da controllare.
002000030610     ** PrmLib      Libreria dell'oggetto.
002100030610     ** PrmObjType  Tipo oggetto.
002200030610     ** PrmMbr      Nome membro.
002300030610     **             Le regole di impostazione dei parametri oggetto,
002400030610     **             libreria, tipo oggetto e membro sono le stesse del
002500030610     **             comando WRKOBJLCK.
002600030610     ** PrmMsg      Testo del messaggio da inviare.
002700030610     **             Per la fase 1 e 5 il testo deve chiedere agli
002800030610     **             utenti di sospendere una certa attività.
002900030610     **             Per la fase 9 il testo deve dire agli utenti che
003000030610     **             l'attività sospesa può riprendere.
003100030610     **
003200030610     ** PARAMETRI DI OUTPUT
003300030610     **
003400030610     ** PrmNbrLock  Numero di allocazioni rilevate.
003500030610     **             Quando è 0 l'oggetto non è usato.
003600030610     ** PrmEsito    Esito elaborazione.
003700030610     **             '0' = eseguito;
003800030610     **             '1' = errori.
003900030610     **
004000030610     ** --------------------------------------------------------------------
004100030610     **
004200030610     ** Fase 1 (Stop)
004300030610     ** Impostare questa fase alla prima chiamata
004400030610     ** per inviare agli utenti che usano l'oggetto un messaggio di stop.
004500030610     ** L'elenco degli utenti è salvato in sostituzione dell'elenco
004600030610     ** precedente.
004700030610     ** Il programma termina con RETURN.
004800030610     **
004900030610     ** Fase 5 (Sollecito stop)
005000030610     ** Impostare questa fase dalla seconda chiamata in poi
005100030610     ** per inviare agli utenti che usano l'oggetto un messaggio di stop.
005200030610     ** L'elenco degli utenti è salvato in aggiunta all'elenco precedente.
005300030610     ** Il programma termina con RETURN.
005400030610     ** Non cambiare l'oggetto tra la fase 1 e 5.
005500030610     **
005600030610     ** Fase 9 (Via libera).
005700030610     ** Impostare questa fase per inviare agli utenti precedentemente
005800030610     ** salvati un messaggio di via libera.
005900030610     ** Il programma termina con *INLR.
006000030610     **
006100030610     ** Fase  Utenti
006200030610     ** ---- ---------
006300030610     **   1  A B
006400030610     **   5    B C D
006500030610     **   5          E
006600030610     **   9  A B C D E
006700030610     ** ---- ---------
006800030610     **
006900030610     ***********************************************************************
007000030606
007100030610     H DFTACTGRP(*NO) ACTGRP('TRULMLOCK')
007200030606
007300030610     D UserSpaceName   C                   'TRULMLOCK QTEMP'
007400030610     D Eseguito        C                   '0'
007500030610     D Errore          C                   '1'
007600030610     D Stop            C                   '1'
007700030610     D SollecitoStop   C                   '5'
007800030610     D ViaLibera       C                   '9'
007900030606
008000030606      /COPY QSYSINC/QRPGLESRC,QUSGEN
008100030606      /COPY QSYSINC/QRPGLESRC,QWCLOBJL
008200030606      /COPY QSYSINC/QRPGLESRC,QUSEC
008300030606
008400030606     ** Parametri API QUSCRTUS
008500030606     D CrtUS01         S             20                                         Qualified user space
008600030606     D                                     INZ(UserSpaceName)
008700030606     D CrtUS02         S             10                                         Extended attribute
008800030606     D CrtUS03         S             10I 0                                      Initial size
008900030606     D                                     INZ(4096)
009000030606     D CrtUS04         S              1                                         Initial value
009100030606     D                                     INZ(X'00')
009200030606     D CrtUS05         S             10                                         Public authority
009300030606     D                                     INZ('*EXCLUDE')
009400030606     D CrtUS06         S             50                                         Text description
009500030610     D                                     INZ('List Object Locks')
009600030606     D CrtUS07         S             10                                         Replace
009700030606     D                                     INZ('*YES')
009800030606
009900030606     ** Parametri API QUSRTVUS
010000030606     D RtvUS01         S             20                                         Qualified user space
010100030606     D                                     INZ(UserSpaceName)
010200030606     D RtvUS02         S             10I 0                                      Starting position
010300030606     D RtvUS03         S             10I 0                                      Length of data
010400030606
010500030606     ** Parametri API QWCLOBJL
010600030606     D LObjL01         S             20                                         Qualified user space
010700030606     D                                     INZ(UserSpaceName)
010800030606     D LObjL02         S              8                                         Format name
010900030606     D                                     INZ('OBJL0100')
011000030606     D LObjL03         S             20                                         Qualified object nam
011100030606     D LObjL04         S             10                                         Object type
011200030606     D LObjL05         S             10                                         Member name
011300030606
011400030606     ** Parametri API QEZSNDMG
011500030606     D SndMg01         S             10                                         Message type
011600030606     D                                     INZ('*INFO')
011700030606     D SndMg02         S             10                                         Deluvery mode
011800030606     D                                     INZ('*BREAK')
011900030610     D SndMg03         S                                                        Message text
012000030610     D                                     LIKE(PrmMsg)
012100030606     D SndMg04         S             10I 0                                      Length of text
012200030606     D                                     INZ(%SIZE(SndMg03))
012300030606     D SndMg05         S             10                                         User profile
012400030606     D                                     DIM(299)
012500030606     D SndMg06         S             10I 0                                      Number of user
012600030606     D SndMg07         S             10I 0                                      Message sent indic.
012700030606     D SndMg08         S             10I 0                                      Function requested
012800030606     D SndMg10         S              1                                         Show display
012900030606     D                                     INZ('N')
013000030606     D SndMg11         S             20                                         Qualified MSGQ name
013100030606     D SndMg12         S              4                                         Name type indicator
013200030606     D                                     INZ('*USR')
013300030606
013400030606     D U               S              5I 0
013500030610     D W               S              5I 0
013600030606     D E               S
013700030606     D                                     LIKE(QUSNBRLE)
013800030610     D SavMg05         S                                                        User profile
013900030610     D                                     LIKE(SndMg05)
014000030610     D                                     DIM(%ELEM(SndMg05))
014100030610
014200030610     D PrmFase         S              1
014300030610     D PrmObj          S             10
014400030610     D PrmLib          S             10
014500030610     D PrmObjType      S             10
014600030610     D PrmMbr          S             10
014700030610     D PrmMsg          S            256
014800030610     D PrmNbrLock      S             10I 0
014900030610     D PrmEsito        S              1
015000030606
015100030610     ***********************************************************************
015200030610     **
015300030610     ** Main
015400030610     **
015500030610     ***********************************************************************
015600030606     C     *ENTRY        PLIST
015700030610     C                   PARM                    PrmFase
015800030610     C                   PARM                    PrmObj
015900030610     C                   PARM                    PrmLib
016000030610     C                   PARM                    PrmObjType
016100030610     C                   PARM                    PrmMbr
016200030610     C                   PARM                    PrmMsg
016300030610     C                   PARM                    PrmNbrLock
016400030610     C                   PARM                    PrmEsito
016500030606
016600030611     C                   EXSR      ChkParm
016700030610
016800030610     C                   SELECT
016900030610     C                   WHEN      PrmFase = Stop
017000030610     C                   EXSR      Fase1
017100030610     C                   WHEN      PrmFase = SollecitoStop
017200030610     C                   EXSR      Fase5
017300030610     C                   WHEN      PrmFase = ViaLibera
017400030610     C                   EXSR      Fase9
017500030610     C                   ENDSL
017600030606
017700030610     C                   EXSR      Uscita
017800030611
017900030611     ***********************************************************************
018000030611     **
018100030611     ** Controllo parametri ricevuti.
018200030611     **
018300030611     ***********************************************************************
018400030611     C     ChkParm       BEGSR
018500030611
018600030611     C                   CLEAR                   PrmNbrLock
018700030611     C                   CLEAR                   PrmEsito
018800030611
018900030611     C                   SELECT
019000030611     C                   WHEN      PrmFase = *BLANK
019100030611     C                   EVAL      PrmFase = Stop
019200030611     C                   WHEN      PrmFase <> Stop
019300030611     C                             AND
019400030611     C                             PrmFase <> SollecitoStop
019500030611     C                             AND
019600030611     C                             PrmFase <> ViaLibera
019700030611     C                   EVAL      PrmEsito = Errore
019800030611     C                   EXSR      Uscita
019900030611     C                   ENDSL
020000030611
020100030611     C                   IF        PrmObj = *BLANK
020200030611     C                             OR
020300030611     C                             PrmObjType = *BLANK
020400030611     C                   EVAL      PrmEsito = Errore
020500030611     C                   EXSR      Uscita
020600030611     C                   ENDIF
020700030611
020800030611     C                   IF        PrmLib = *BLANK
020900030611     C                   EVAL      PrmLib = '*LIBL'
021000030611     C                   ENDIF
021100030611
021200030611     C                   IF        PrmMbr = *BLANK
021300030611     C                   EVAL      PrmMbr = '*NONE'
021400030611     C                   ENDIF
021500030611
021600030611     C                   ENDSR
021700030610
021800030610     ***********************************************************************
021900030610     **
022000030610     ** Stop
022100030610     **
022200030610     ***********************************************************************
022300030610     C     Fase1         BEGSR
022400030610
022500030610     C                   CLEAR                   SavMg05
022600030610     C                   EXSR      ChkObjLck
022700030610
022800030610     C                   ENDSR
022900030610
023000030610     ***********************************************************************
023100030610     **
023200030610     ** Sollecito stop.
023300030610     **
023400030610     ***********************************************************************
023500030610     C     Fase5         BEGSR
023600030610
023700030610     C                   EXSR      ChkObjLck
023800030610
023900030610     C                   ENDSR
024000030610
024100030610     ***********************************************************************
024200030610     **
024300030610     ** Via Libera.
024400030610     **
024500030610     ***********************************************************************
024600030610     C     Fase9         BEGSR
024700030610
024800030611     C                   IF        SavMg05(1) <> *BLANK
024900030611
025000030610     ** Ripristino tutti gli utenti a cui ho inviato il messaggio.
025100030610     C                   EVAL      SndMg05 = SavMg05
025200030610
025300030610     ** Calcolo il numero degli utenti.
025400030610     C                   EVAL      U = 1
025500030610     C     *BLANK        LOOKUP    SndMg05(U)                             01
025600030610     C                   IF        %EQUAL
025700030610     C                   EVAL      SndMg06 = U
025800030610     C                   ELSE
025900030610     C                   EVAL      SndMg06 = %ELEM(SndMg05)
026000030610     C                   ENDIF
026100030610
026200030610     ** Invio il messaggio di via libera.
026300030610     C                   EXSR      CalQEZSNDMG
026400030611
026500030611     C                   ENDIF
026600030610
026700030610     C                   ENDSR
026800030610
026900030610     ***********************************************************************
027000030610     **
027100030610     ** Controllo allocazione oggetto.
027200030610     **
027300030610     ***********************************************************************
027400030610     C     ChkObjLck     BEGSR
027500030610
027600030606     ** Creo lo user space.
027700030606     C                   CLEAR                   QUSEC
027800030606     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
027900030606
028000030606     C                   CALL      'QUSCRTUS'
028100030606     C                   PARM                    CrtUS01
028200030606     C                   PARM                    CrtUS02
028300030606     C                   PARM                    CrtUS03
028400030606     C                   PARM                    CrtUS04
028500030606     C                   PARM                    CrtUS05
028600030606     C                   PARM                    CrtUS06
028700030606     C                   PARM                    CrtUS07
028800030606     C                   PARM                    QUSEC
028900030606
029000030606     C                   IF        QUSEI <> *BLANK
029100030610     C                   EVAL      PrmEsito = Errore
029200030606     C                   EXSR      Uscita
029300030606     C                   ENDIF
029400030606
029500030606     ** Riempio lo user space con i lavori che allocano l'oggetto.
029600030610     C                   EVAL      LObjL03 = PrmObj + PrmLib
029700030610     C                   EVAL      LObjL04 = PrmObjType
029800030610     C                   EVAL      LObjL05 = PrmMbr
029900030606     C                   CLEAR                   QUSEC
030000030606     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
030100030606
030200030606     C                   CALL      'QWCLOBJL'
030300030606     C                   PARM                    LObjL01
030400030606     C                   PARM                    LObjL02
030500030606     C                   PARM                    LObjL03
030600030606     C                   PARM                    LObjL04
030700030606     C                   PARM                    LObjL05
030800030606     C                   PARM                    QUSEC
030900030606
031000030606     C                   IF        QUSEI <> *BLANK
031100030610     C                   EVAL      PrmEsito = Errore
031200030606     C                   EXSR      Uscita
031300030606     C                   ENDIF
031400030606
031500030606     ** Retrieve User Space - Generic Header
031600030606     C                   EVAL      RtvUS02 = 1
031700030606     C                   EVAL      RtvUS03 = %LEN(QUSH0100)
031800030606     C                   CLEAR                   QUSEC
031900030606     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
032000030606
032100030606     C                   CALL      'QUSRTVUS'
032200030606     C                   PARM                    RtvUS01
032300030606     C                   PARM                    RtvUS02
032400030606     C                   PARM                    RtvUS03
032500030606     C                   PARM                    QUSH0100
032600030606     C                   PARM                    QUSEC
032700030606
032800030606     C                   IF        QUSEI <> *BLANK
032900030610     C                   EVAL      PrmEsito = Errore
033000030606     C                   EXSR      Uscita
033100030606     C                   ENDIF
033200030610
033300030610     C                   CLEAR                   SndMg05
033400030610     C                   CLEAR                   SndMg06
033500030610
033600030610     ** Restituisco il numero di allocazioni.
033700030610     C                   EVAL      PrmNbrLock = QUSNBRLE
033800030610
033900030606     ** Imposto la posizione iniziale della lista
034000030606     ** e la lunghezza di ogni elemento.
034100030606     C                   EVAL      RtvUS02 = QUSOLD + 1
034200030606     C                   EVAL      RtvUS03 = QUSSEE
034300030606
034400030606     ** Loop lettura elementi.
034500030606     C                   EVAL      E = 1
034600030606     C                   DOW       E <= QUSNBRLE
034700030606
034800030606     ** Retrieve User Space - OBJL0100 Format
034900030606     C                   CLEAR                   QUSEC
035000030606     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
035100030606
035200030606     C                   CALL      'QUSRTVUS'
035300030606     C                   PARM                    RtvUS01
035400030606     C                   PARM                    RtvUS02
035500030606     C                   PARM                    RtvUS03
035600030606     C                   PARM                    QWC0100L
035700030606     C                   PARM                    QUSEC
035800030606
035900030606     C                   IF        QUSEI <> *BLANK
036000030610     C                   EVAL      PrmEsito = Errore
036100030606     C                   EXSR      Uscita
036200030606     C                   ENDIF
036300030606
036400030606     ** Memorizzo l'utente nella schiera dei destinatari del messaggio.
036500030610     C                   IF        PrmMsg <> *BLANK
036600030606     C                   EVAL      U = 1
036700030606     C     QWCJUN        LOOKUP    SndMg05(U)                             01
036800030606     C                   IF        NOT %EQUAL
036900030606     C                   EVAL      U = 1
037000030606     C     *BLANK        LOOKUP    SndMg05(U)                             01
037100030606     C                   IF        %EQUAL
037200030606     C                   EVAL      SndMg05(U) = QWCJUN
037300030606     C                   EVAL      SndMg06 = U
037400030606     C                   ENDIF
037500030606     C                   ENDIF
037600030610     C                   ENDIF
037700030606
037800030606     ** Determino la posizione iniziale del prossimo elemento.
037900030606     C                   EVAL      RtvUS02 = QUSOLD + 1 + (QUSSEE * E)
038000030606     C                   EVAL      E = E + 1
038100030606
038200030606     C                   ENDDO
038300030606
038400030606     C                   EXSR      CalQEZSNDMG
038500030610     C                   EXSR      SavUser
038600030610
038700030610     C                   ENDSR
038800030606
038900030606     ***********************************************************************
039000030606     **
039100030606     ** Operazioni finali.
039200030606     **
039300030606     ***********************************************************************
039400030606     C     CalQEZSNDMG   BEGSR
039500030606
039600030610     ** Invio il messaggio se c'è almeno un utente
039700030606     C                   IF        SndMg06 > 0
039800030606
039900030606     C                   CALL      'QEZSNDMG'
040000030606     C                   PARM                    SndMg01
040100030606     C                   PARM                    SndMg02
040200030610     C                   PARM      PrmMsg        SndMg03
040300030606     C                   PARM                    SndMg04
040400030606     C                   PARM                    SndMg05
040500030606     C                   PARM                    SndMg06
040600030606     C                   PARM                    SndMg07
040700030606     C                   PARM                    SndMg08
040800030606     C                   PARM                    Qusec
040900030606     C                   PARM                    SndMg10
041000030606     C                   PARM                    SndMg11
041100030606     C                   PARM                    SndMg12
041200030606
041300030606     C                   ENDIF
041400030606
041500030606     C                   ENDSR
041600030610
041700030610     ***********************************************************************
041800030610     **
041900030610     ** Salvataggio utenti
042000030610     **
042100030610     ***********************************************************************
042200030610     C     SavUser       BEGSR
042300030610
042400030610     C                   EVAL      U = 1
042500030610     C                   DOW       SndMg05(U) <> *BLANK
042600030610     C                             AND
042700030610     C                             U <= %ELEM(SndMg05)
042800030610     C                   EVAL      W = 1
042900030610     C     SndMg05(U)    LOOKUP    SavMg05(W)                             01
043000030610     C                   IF        NOT %EQUAL
043100030610     C                   EVAL      W = 1
043200030610     C     *BLANK        LOOKUP    SavMg05(W)                             01
043300030610     C                   IF        %EQUAL
043400030610     C                   EVAL      SavMg05(W) = SndMg05(U)
043500030610     C                   ENDIF
043600030610     C                   ENDIF
043700030610     C                   EVAL      U = U + 1
043800030610     C                   ENDDO
043900030610
044000030610     C                   ENDSR
044100030606
044200030606     ***********************************************************************
044300030606     **
044400030606     ** Operazioni finali.
044500030606     **
044600030606     ***********************************************************************
044700030606     C     Uscita        BEGSR
044800030606
044900030610     C                   IF        PrmEsito = *BLANK
045000030612     C                   EVAL      PrmEsito = Eseguito
045100030610     C                   ENDIF
045200030610
045300030610     C                   EVAL      *INLR = (PrmFase = ViaLibera)
045400030606     C                   RETURN
045500030606
045600030606     C                   ENDSR
