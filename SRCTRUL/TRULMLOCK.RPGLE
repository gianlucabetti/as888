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
007900121130
008000121130     D EXCL            C                   '1'
008100121130     D EXCLRD          C                   '2'
008200121130     D SHRUPD          C                   '3'
008300121130     D SHRNUPD         C                   '4'
008400121130     D SHRRD           C                   '5'
008500030606
008600030606      /COPY QSYSINC/QRPGLESRC,QUSGEN
008700030606      /COPY QSYSINC/QRPGLESRC,QWCLOBJL
008800030606      /COPY QSYSINC/QRPGLESRC,QUSEC
008900030606
009000030606     ** Parametri API QUSCRTUS
009100030606     D CrtUS01         S             20                                         Qualified user space
009200030606     D                                     INZ(UserSpaceName)
009300030606     D CrtUS02         S             10                                         Extended attribute
009400030606     D CrtUS03         S             10I 0                                      Initial size
009500030606     D                                     INZ(4096)
009600030606     D CrtUS04         S              1                                         Initial value
009700030606     D                                     INZ(X'00')
009800030606     D CrtUS05         S             10                                         Public authority
009900030606     D                                     INZ('*EXCLUDE')
010000030606     D CrtUS06         S             50                                         Text description
010100030610     D                                     INZ('List Object Locks')
010200030606     D CrtUS07         S             10                                         Replace
010300030606     D                                     INZ('*YES')
010400030606
010500030606     ** Parametri API QUSRTVUS
010600030606     D RtvUS01         S             20                                         Qualified user space
010700030606     D                                     INZ(UserSpaceName)
010800030606     D RtvUS02         S             10I 0                                      Starting position
010900030606     D RtvUS03         S             10I 0                                      Length of data
011000030606
011100030606     ** Parametri API QWCLOBJL
011200030606     D LObjL01         S             20                                         Qualified user space
011300030606     D                                     INZ(UserSpaceName)
011400030606     D LObjL02         S              8                                         Format name
011500030606     D                                     INZ('OBJL0100')
011600030606     D LObjL03         S             20                                         Qualified object nam
011700030606     D LObjL04         S             10                                         Object type
011800030606     D LObjL05         S             10                                         Member name
011900030606
012000030606     ** Parametri API QEZSNDMG
012100030606     D SndMg01         S             10                                         Message type
012200030606     D                                     INZ('*INFO')
012300030606     D SndMg02         S             10                                         Deluvery mode
012400030606     D                                     INZ('*BREAK')
012500030610     D SndMg03         S                                                        Message text
012600030610     D                                     LIKE(PrmMsg)
012700030606     D SndMg04         S             10I 0                                      Length of text
012800030606     D                                     INZ(%SIZE(SndMg03))
012900030606     D SndMg05         S             10                                         User profile
013000030606     D                                     DIM(299)
013100030606     D SndMg06         S             10I 0                                      Number of user
013200030606     D SndMg07         S             10I 0                                      Message sent indic.
013300030606     D SndMg08         S             10I 0                                      Function requested
013400030606     D SndMg10         S              1                                         Show display
013500030606     D                                     INZ('N')
013600030606     D SndMg11         S             20                                         Qualified MSGQ name
013700030606     D SndMg12         S              4                                         Name type indicator
013800030606     D                                     INZ('*USR')
013900030606
014000030606     D U               S              5I 0
014100030610     D W               S              5I 0
014200030606     D E               S
014300030606     D                                     LIKE(QUSNBRLE)
014400030610     D SavMg05         S                                                        User profile
014500030610     D                                     LIKE(SndMg05)
014600030610     D                                     DIM(%ELEM(SndMg05))
014700030610
014800030610     D PrmFase         S              1
014900030610     D PrmObj          S             10
015000030610     D PrmLib          S             10
015100030610     D PrmObjType      S             10
015200030610     D PrmMbr          S             10
015300121130     D PrmTipoAlc      S              1
015400030610     D PrmMsg          S            256
015500030610     D PrmNbrLock      S             10I 0
015600030610     D PrmEsito        S              1
015700030606
015800030610     ***********************************************************************
015900030610     **
016000030610     ** Main
016100030610     **
016200030610     ***********************************************************************
016300030606     C     *ENTRY        PLIST
016400030610     C                   PARM                    PrmFase
016500030610     C                   PARM                    PrmObj
016600030610     C                   PARM                    PrmLib
016700030610     C                   PARM                    PrmObjType
016800030610     C                   PARM                    PrmMbr
016900121130     C                   PARM                    PrmTipoAlc
017000030610     C                   PARM                    PrmMsg
017100030610     C                   PARM                    PrmNbrLock
017200030610     C                   PARM                    PrmEsito
017300121130      *
017400121130      *   il controllo effettuato dal TRULMLOCK sull'allocazione
017500121130      *   è determinato dal tipo di controllo allocazione richiesto
017600121130      *   con questo parametro: TIPOALC che può avere i seguenti valori
017700121130      *
017800121130      *     (1) - *EXCL              (4) - *SHRNUPD
017900121130      *     (2) - *EXCLRD            (5) - *SHRRD
018000121130      *     (3) - *SHRUPD
018100121130      *
018200030611     C                   EXSR      ChkParm
018300030610
018400030610     C                   SELECT
018500030610     C                   WHEN      PrmFase = Stop
018600030610     C                   EXSR      Fase1
018700030610     C                   WHEN      PrmFase = SollecitoStop
018800030610     C                   EXSR      Fase5
018900030610     C                   WHEN      PrmFase = ViaLibera
019000030610     C                   EXSR      Fase9
019100030610     C                   ENDSL
019200030606
019300030610     C                   EXSR      Uscita
019400030611
019500030611     ***********************************************************************
019600030611     **
019700030611     ** Controllo parametri ricevuti.
019800030611     **
019900030611     ***********************************************************************
020000030611     C     ChkParm       BEGSR
020100030611
020200030611     C                   CLEAR                   PrmNbrLock
020300030611     C                   CLEAR                   PrmEsito
020400030611
020500030611     C                   SELECT
020600030611     C                   WHEN      PrmFase = *BLANK
020700030611     C                   EVAL      PrmFase = Stop
020800030611     C                   WHEN      PrmFase <> Stop
020900030611     C                             AND
021000030611     C                             PrmFase <> SollecitoStop
021100030611     C                             AND
021200030611     C                             PrmFase <> ViaLibera
021300030611     C                   EVAL      PrmEsito = Errore
021400030611     C                   EXSR      Uscita
021500030611     C                   ENDSL
021600030611
021700030611     C                   IF        PrmObj = *BLANK
021800030611     C                             OR
021900030611     C                             PrmObjType = *BLANK
022000030611     C                   EVAL      PrmEsito = Errore
022100030611     C                   EXSR      Uscita
022200030611     C                   ENDIF
022300030611
022400030611     C                   IF        PrmLib = *BLANK
022500030611     C                   EVAL      PrmLib = '*LIBL'
022600030611     C                   ENDIF
022700030611
022800030611     C                   IF        PrmMbr = *BLANK
022900030611     C                   EVAL      PrmMbr = '*NONE'
023000030611     C                   ENDIF
023100030611
023200030611     C                   ENDSR
023300030610
023400030610     ***********************************************************************
023500030610     **
023600030610     ** Stop
023700030610     **
023800030610     ***********************************************************************
023900030610     C     Fase1         BEGSR
024000030610
024100030610     C                   CLEAR                   SavMg05
024200030610     C                   EXSR      ChkObjLck
024300030610
024400030610     C                   ENDSR
024500030610
024600030610     ***********************************************************************
024700030610     **
024800030610     ** Sollecito stop.
024900030610     **
025000030610     ***********************************************************************
025100030610     C     Fase5         BEGSR
025200030610
025300030610     C                   EXSR      ChkObjLck
025400030610
025500030610     C                   ENDSR
025600030610
025700030610     ***********************************************************************
025800030610     **
025900030610     ** Via Libera.
026000030610     **
026100030610     ***********************************************************************
026200030610     C     Fase9         BEGSR
026300030610
026400030611     C                   IF        SavMg05(1) <> *BLANK
026500030611
026600030610     ** Ripristino tutti gli utenti a cui ho inviato il messaggio.
026700030610     C                   EVAL      SndMg05 = SavMg05
026800030610
026900030610     ** Calcolo il numero degli utenti.
027000030610     C                   EVAL      U = 1
027100030610     C     *BLANK        LOOKUP    SndMg05(U)                             01
027200030610     C                   IF        %EQUAL
027300030610     C                   EVAL      SndMg06 = U
027400030610     C                   ELSE
027500030610     C                   EVAL      SndMg06 = %ELEM(SndMg05)
027600030610     C                   ENDIF
027700030610
027800030610     ** Invio il messaggio di via libera.
027900030610     C                   EXSR      CalQEZSNDMG
028000030611
028100030611     C                   ENDIF
028200030610
028300030610     C                   ENDSR
028400030610
028500030610     ***********************************************************************
028600030610     **
028700030610     ** Controllo allocazione oggetto.
028800030610     **
028900030610     ***********************************************************************
029000030610     C     ChkObjLck     BEGSR
029100030610
029200030606     ** Creo lo user space.
029300030606     C                   CLEAR                   QUSEC
029400030606     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
029500030606
029600030606     C                   CALL      'QUSCRTUS'
029700030606     C                   PARM                    CrtUS01
029800030606     C                   PARM                    CrtUS02
029900030606     C                   PARM                    CrtUS03
030000030606     C                   PARM                    CrtUS04
030100030606     C                   PARM                    CrtUS05
030200030606     C                   PARM                    CrtUS06
030300030606     C                   PARM                    CrtUS07
030400030606     C                   PARM                    QUSEC
030500030606
030600030606     C                   IF        QUSEI <> *BLANK
030700030610     C                   EVAL      PrmEsito = Errore
030800030606     C                   EXSR      Uscita
030900030606     C                   ENDIF
031000030606
031100030606     ** Riempio lo user space con i lavori che allocano l'oggetto.
031200030610     C                   EVAL      LObjL03 = PrmObj + PrmLib
031300030610     C                   EVAL      LObjL04 = PrmObjType
031400030610     C                   EVAL      LObjL05 = PrmMbr
031500030606     C                   CLEAR                   QUSEC
031600030606     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
031700030606
031800030606     C                   CALL      'QWCLOBJL'
031900030606     C                   PARM                    LObjL01
032000030606     C                   PARM                    LObjL02
032100030606     C                   PARM                    LObjL03
032200030606     C                   PARM                    LObjL04
032300030606     C                   PARM                    LObjL05
032400030606     C                   PARM                    QUSEC
032500030606
032600030606     C                   IF        QUSEI <> *BLANK
032700030610     C                   EVAL      PrmEsito = Errore
032800030606     C                   EXSR      Uscita
032900030606     C                   ENDIF
033000030606
033100030606     ** Retrieve User Space - Generic Header
033200030606     C                   EVAL      RtvUS02 = 1
033300030606     C                   EVAL      RtvUS03 = %LEN(QUSH0100)
033400030606     C                   CLEAR                   QUSEC
033500030606     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
033600030606
033700030606     C                   CALL      'QUSRTVUS'
033800030606     C                   PARM                    RtvUS01
033900030606     C                   PARM                    RtvUS02
034000030606     C                   PARM                    RtvUS03
034100030606     C                   PARM                    QUSH0100
034200030606     C                   PARM                    QUSEC
034300030606
034400030606     C                   IF        QUSEI <> *BLANK
034500030610     C                   EVAL      PrmEsito = Errore
034600030606     C                   EXSR      Uscita
034700030606     C                   ENDIF
034800030610
034900030610     C                   CLEAR                   SndMg05
035000030610     C                   CLEAR                   SndMg06
035100030610
035200030610     ** Restituisco il numero di allocazioni.
035300030610     C                   EVAL      PrmNbrLock = QUSNBRLE
035400030610
035500030606     ** Imposto la posizione iniziale della lista
035600030606     ** e la lunghezza di ogni elemento.
035700030606     C                   EVAL      RtvUS02 = QUSOLD + 1
035800030606     C                   EVAL      RtvUS03 = QUSSEE
035900030606
036000030606     ** Loop lettura elementi.
036100030606     C                   EVAL      E = 1
036200030606     C                   DOW       E <= QUSNBRLE
036300030606
036400030606     ** Retrieve User Space - OBJL0100 Format
036500030606     C                   CLEAR                   QUSEC
036600030606     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
036700030606
036800030606     C                   CALL      'QUSRTVUS'
036900030606     C                   PARM                    RtvUS01
037000030606     C                   PARM                    RtvUS02
037100030606     C                   PARM                    RtvUS03
037200030606     C                   PARM                    QWC0100L
037300030606     C                   PARM                    QUSEC
037400030606
037500030606     C                   IF        QUSEI <> *BLANK
037600030610     C                   EVAL      PrmEsito = Errore
037700030606     C                   EXSR      Uscita
037800030606     C                   ENDIF
037900030606
038000030606     ** Memorizzo l'utente nella schiera dei destinatari del messaggio.
038100030610     C                   IF        PrmMsg <> *BLANK
038200121130      **
038300121130      **  Testa x l'allocazione nel CL cosa è o non è permesso dal WRKOBJLCK.
038400121130      **    tabella di riepilogo in base all'allocazione
038500121130      **  Se 1-*EXCL   --> qualsiasi altro deve essere AVVISATO
038600121130      **  Se 2-*EXCLRD --> Devono essere AVVISATI tutti quelli che non sono in *SHRRD
038700121130      **  Se 3-*SHRUPD --> Devono essere AVVISATI tutti quelli che non sono in *SHRRD e *SHRUPD
038800121130      **  Se 4-*SHRNUPD--> Devono essere AVVISATI tutti quelli che non sono in *SHRRD e *SHRNUPD
038900121130      **  Se 5-*SHRRD  --> Devono essere AVVISATI tutti quelli che sono in *EXCL
039000121130      **
039100121130     c                   if        prmTipoALC = EXCL
039200121130     c                              or
039300121130     c                             prmTipoALC = EXCLRD     and
039400121130     c                              QWCLS <> '*SHRRD    '
039500121130     c                              or
039600121130     c                             prmTipoALC = SHRUPD     and
039700121130     c                              QWCLS <> '*SHRRD    '  and
039800121130     c                              QWCLS <> '*SHRUPD   '
039900121130     c                              or
040000121130     c                             prmTipoALC = SHRNUPD    and
040100121130     c                              QWCLS <> '*SHRRD    '  and
040200121130     c                              QWCLS <> '*SHRNUPD  '
040300121130     c                              or
040400121130     c                             prmTipoALC = SHRRD      and
040500121130     c                              QWCLS  = '*EXCL     '
040600121130      **
040700030606     C                   EVAL      U = 1
040800030606     C     QWCJUN        LOOKUP    SndMg05(U)                             01
040900030606     C                   IF        NOT %EQUAL
041000030606     C                   EVAL      U = 1
041100030606     C     *BLANK        LOOKUP    SndMg05(U)                             01
041200030606     C                   IF        %EQUAL
041300030606     C                   EVAL      SndMg05(U) = QWCJUN
041400030606     C                   EVAL      SndMg06 = U
041500030606     C                   ENDIF
041600030606     C                   ENDIF
041700121130      **
041800121130      **  avvisa solo quelli che NON devono essere in allocazione
041900121130      **   in base all'allocazione utilizzata
042000121130     c                   end
042100121130      **
042200030610     C                   ENDIF
042300030606
042400030606     ** Determino la posizione iniziale del prossimo elemento.
042500030606     C                   EVAL      RtvUS02 = QUSOLD + 1 + (QUSSEE * E)
042600030606     C                   EVAL      E = E + 1
042700030606
042800030606     C                   ENDDO
042900030606
043000030606     C                   EXSR      CalQEZSNDMG
043100030610     C                   EXSR      SavUser
043200030610
043300030610     C                   ENDSR
043400030606
043500030606     ***********************************************************************
043600030606     **
043700030606     ** Operazioni finali.
043800030606     **
043900030606     ***********************************************************************
044000030606     C     CalQEZSNDMG   BEGSR
044100030606
044200030610     ** Invio il messaggio se c'è almeno un utente
044300030606     C                   IF        SndMg06 > 0
044400030606
044500030606     C                   CALL      'QEZSNDMG'
044600030606     C                   PARM                    SndMg01
044700030606     C                   PARM                    SndMg02
044800030610     C                   PARM      PrmMsg        SndMg03
044900030606     C                   PARM                    SndMg04
045000030606     C                   PARM                    SndMg05
045100030606     C                   PARM                    SndMg06
045200030606     C                   PARM                    SndMg07
045300030606     C                   PARM                    SndMg08
045400030606     C                   PARM                    Qusec
045500030606     C                   PARM                    SndMg10
045600030606     C                   PARM                    SndMg11
045700030606     C                   PARM                    SndMg12
045800030606
045900030606     C                   ENDIF
046000030606
046100030606     C                   ENDSR
046200030610
046300030610     ***********************************************************************
046400030610     **
046500030610     ** Salvataggio utenti
046600030610     **
046700030610     ***********************************************************************
046800030610     C     SavUser       BEGSR
046900030610
047000030610     C                   EVAL      U = 1
047100030610     C                   DOW       SndMg05(U) <> *BLANK
047200030610     C                             AND
047300030610     C                             U <= %ELEM(SndMg05)
047400030610     C                   EVAL      W = 1
047500030610     C     SndMg05(U)    LOOKUP    SavMg05(W)                             01
047600030610     C                   IF        NOT %EQUAL
047700030610     C                   EVAL      W = 1
047800030610     C     *BLANK        LOOKUP    SavMg05(W)                             01
047900030610     C                   IF        %EQUAL
048000030610     C                   EVAL      SavMg05(W) = SndMg05(U)
048100030610     C                   ENDIF
048200030610     C                   ENDIF
048300030610     C                   EVAL      U = U + 1
048400030610     C                   ENDDO
048500030610
048600030610     C                   ENDSR
048700030606
048800030606     ***********************************************************************
048900030606     **
049000030606     ** Operazioni finali.
049100030606     **
049200030606     ***********************************************************************
049300030606     C     Uscita        BEGSR
049400030606
049500030610     C                   IF        PrmEsito = *BLANK
049600030612     C                   EVAL      PrmEsito = Eseguito
049700030610     C                   ENDIF
049800030610
049900030610     C                   EVAL      *INLR = (PrmFase = ViaLibera)
050000030606     C                   RETURN
050100030606
050200030606     C                   ENDSR
