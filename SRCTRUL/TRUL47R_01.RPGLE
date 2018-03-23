000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000200050906     Ftivgs00f  O    E             DISK
000300050906     Ftivgs01l  UF   E           K DISK    rename(tivgs000:tivgs01)
000400050907     Ftivgs02l  UF   E           K DISK    rename(tivgs000:tivgs02)
000500940922     F*--------------------------------------------------------------*
000600050906     D trul47ds      E DS
000700050906     D
000800050906     D pgmstatus      SDS
000900050906     D  JOB_NAME             244    253                                         * Job name
001000050906     D  USER                 254    263                                         * User name
001100050906     D  JOB_NUM              264    269                                         * Job number
001200050907     D*------------------
001300050907     D* VARIABILI D WRK
001400050907     D*------------------
001500050907     D  kvgsLCK        S                   LIKE(vgsLCK)
001600050907     D  ExistTipF      S              1
001700050907     D  AllocTipF      S              1
001800050907     D  BloccTipF      S              1
001900050907     D  wMessErrore    s           1000    inz(*blanks)
002000050906     D*------------------
002100050906     D* DS "XSRDA8" - CONTROLLA DATA (8)
002200050906     D*------------------
002300050906     D WLBDA8          DS                  INZ
002400050906     D  G08DAT                 1      8  0
002500050906     D  G08INV                 9     16  0
002600050906     D  G08ERR                17     17
002700050906     D  G08TGI                18     22  0
002800050906     D*--------------------------------------------------------------*
002900050907     C*
003000050909     C* Innanzitutto ad ogni chiamata resetto i campi d output
003100050909     C                   eval      d47sts  = *blanks
003200050909     C                   eval      d47jsts = *blanks
003300050906     C*
003400050907     C********
003500050906     C* Operazioni di inizio
003600050907     C********
003700050907     C                   if        d47opz = 'I'
003800050907     C*
003900050907     C* Verifico subito se x il tipo file richiesto è presente qualche forzatura d allocazione
004000050907     C                   eval      kvgsLCK = 'S'
004100050907     C                   exsr      ChkTipF
004200050907     C*
004300050907     C* Se tipo file richiesto NN già allocato proseguo altrimenti esco subito e restituisco stato attuale
004400050907     C                   if        AllocTipF = 'N'
004500050907     C*
004600050906     C* Inserisco il record d blocco x il tipo file/job corrente
004700050906     C                   clear                   tivgs000
004800050906     C                   eval      VGSTIP  = d47tip
004900050906     C                   eval      VGSDAT  = datcor
005000050906     C                   eval      VGSORA  = oracor
005100050906     C                   eval      VGSJNAM = JOB_NAME
005200050906     C                   eval      VGSUNAM = USER
005300050906     C                   eval      VGSJNUM = JOB_NUM
005400050906     C                   eval      VGSPGM  = d47pgm
005500050907     C                   eval      VGSLCK  = d47lck
005600050907     C                   eval      VGSCHKJ = d47chkj
005700050906     C                   write     tivgs000
005800050907     C*
005900050907     C                   else
006000050907     C                   eval      d47sts = 'A'
006100050907     C                   endif
006200050906     C                   endif
006300050907     C*
006400050907     C********
006500050907     C* Operazioni di verifica disponibilità elaborazione tipo file
006600050907     C********
006700050907     C                   if        d47opz = 'C'
006800050907     C*
006900050907     C* Verifico subito se x il tipo file richiesto è presente qualche forzatura d allocazione
007000050907     C                   eval      kvgsLCK = 'S'
007100050907     C                   exsr      ChkTipF
007200050907     C*
007300050907     C* Se dopo il 1° giro x i record semaforo CON forzatura allocazione esistono ancora record effettuo 2° giro
007400050907     C* x quelli SENZA forzatura allocazione
007500050907     C                   if        ExistTipF = 'S' AND
007600050907     C                             AllocTipF = 'N' AND
007700050907     C                             BloccTipF = 'N'
007800050907     C                   eval      kvgsLCK = *blanks
007900050907     C                   exsr      ChkTipF
008000050907     C                   endif
008100050907     C*
008200050907     C* Se a questo punto il tipo file è presente considero bloccata la sua elaborazione
008300050907     C                   if        ExistTipF = 'S'
008400050907     C                   eval      d47sts = 'B'
008500050907     C                   endif
008600050907     C                   endif
008700050907     C*
008800050907     C********
008900050906     C* Operazioni di fine
009000050907     C********
009100050906     C                   if        d47opz = 'F'
009200050907     C*
009300050907     C* Elimino il record d blocco x il tipo file/job corrente
009400050906     C     KEYVGS_01C    chain     tivgs01l
009500050906     C                   if        %found(tivgs01l)
009600050907     C                   delete    tivgs01l
009700050906     C                   else
009800050906     C                   eval      d47sts = '1'
009900050906     C                   endif
010000050906     C                   endif
010100050909     C*
010200050909     C********
010300050909     C* Operazioni di reperimento stato job specifico ricevuto in input
010400050909     C********
010500050909     C                   if        d47opz = 'J'
010600050909     C*
010700050909     C* Effettuo chiamata reprire lo stato attuale del job
010800050909     C                   call(e)   'TRUL47C'
010900050909     C                   parm                    d47jnam
011000050909     C                   parm                    d47unam
011100050909     C                   parm                    d47jnum
011200050909     C                   parm      *blanks       d47jsts
011300050909     C                   if        %error
011400050909     C                   eval      d47sts = '1'
011500050909     C                   endif
011600050909     C                   endif
011700050906     C*
011800050907     C********
011900050906     C* Operazioni di chiusura
012000050907     C********
012100050907     C                   if        d47opz = 'Z'
012200050906     C                   endif
012300050909     C*
012400050909     C                   seton                                        lr
012500050906     C*
012600050906     C******************************************************
012700050906     C     *PSSR         BEGSR
012800050906     C*
012900050906     C                   eval      d47sts = '1'
013000050906     C*
013100050906     C                   ENDSR     '*CANCL'
013200050907     C******************************************************
013300050907     C     ChkTipF       BEGSR
013400050907     C*
013500050907     C* D default considero il tipo file richiesto come NN allocato e NN bloccato
013600050907     C                   eval      ExistTipF = 'N'
013700050907     C                   eval      AllocTipF = 'N'
013800050907     C                   eval      BloccTipF = 'N'
013900050907     C*
014000050907     C* Verifico se x il tipo file corrente è già presente 1 record d allocazione forzata
014100050907     C     KEYVGS_02C    setll     tivgs02l
014200050907     C                   if        %equal(tivgs02l)
014300050907     C     KEYVGS_02C    reade     tivgs02l
014400050907     C                   dow       not %eof(tivgs02l)
014500050907     C*
014600050907     C* Effettuo chiamata x verificare se il job è ancora valido
014700050907     C                   call(e)   'TRUL47C'
014800050907     C                   parm                    JOB_NAME
014900050907     C                   parm                    USER
015000050907     C                   parm                    JOB_NUM
015100050907     C                   parm      *blanks       JOB_STATUS       10
015200050907     C*
015300050907     C* Se il job sul record semaforo corrente NN è attivo ed è richiesta la gestione in automatico elimino il
015400050907     C* record semaforo corrente
015500050907     C                   if        %error OR JOB_STATUS <> '*ACTIVE'
015600050907     C                   if        vgsCHKJ = 'S'
015700050907     C                   delete    tivgs02l
015800050907     C                   else
015900050907     C*
016000050907     C* Se il job sul record semaforo corrente NN è attivo ma nn è stata espressamente richiesta sullo stesso la
016100050907     C* gestione in automatico vuol dire che c'è qualcosa che si è interrotto in maniera anomala x cui mando
016200050907     C* segnalazione via mail agli operatori
016300050907     C                   eval      BloccTipF = 'S'
016400050907     C                   eval      wMessErrore = 'VERIFICARE IL FILE DOWNLOAD '+
016500050907     C                             'GENERICO TIVGD00F'                   +':/N'+
016600050907     C                             ' '                                   +':/N'+
016700050907     C                             'RIFERIMENTI'                         +':/N'+
016800050907     C                             'Tipo file: ' + d47tip                +':/N'+
016900050907     C                             'Programma: ' + d47pgm                +':/N'+
017000050907     C                             'Job: '+JOB_NAME+'/'+USER+'/'+JOB_NUM +':/N'+
017100050907     C                             ' '                                   +':/N'+
017200050907     C                             'MESSAGGIO'                           +':/N'+
017300050907     C                             'Il tipo file nel TIVGD00F è bloccato!!!'
017400050907     C                   exsr      sndeml
017500050907     C                   endif
017600050907     C                   else
017700050907     C*
017800050907     C* Se invece il job è già/ancora il tipo file è cmq in stato d blocco elaborazione
017900050907     C                   eval      BloccTipF = 'S'
018000050907     C*
018100050907     C* Se invece il job è già/ancora attivo ed ha il flag d forzatura allocazione valorizzato
018200050907     C                   if        vgsLCK = 'S'
018300050907     C                   eval      AllocTipF = 'S'
018400050907     C                   endif
018500050907     C                   endif
018600050907     C*
018700050907     C* Proseguo cmq la lettura x verificare sempre tutti i record semaforo
018800050907     C     KEYVGS_02C    reade     tivgs02l
018900050907     C                   enddo
019000050907     C                   endif
019100050907     C*
019200050907     C* A questo punto verifico se x il tipo file corrente è ancora presente almeno 1 record semaforo
019300050907     C     d47tip        setll     tivgs02l
019400050907     C                   if        %equal(tivgs02l)
019500050907     C                   eval      ExistTipF = 'S'
019600050907     C                   else
019700050907     C*
019800050907     C* Se NN esiste nessun record semaforo x il tipo file corrente NN considero ovviamente nemmeno gli altri
019900050907     C* vincoli d blocco
020000050907     C                   eval      ExistTipF = 'N'
020100050907     C                   eval      AllocTipF = 'N'
020200050907     C                   eval      BloccTipF = 'N'
020300050907     C                   endif
020400050907     C*
020500050907     C                   ENDSR
020600050907     C******************************************************
020700050907      /TITLE Compongo il testo e spedisco una e-m@ail
020800050907     C     sndeml        begsr
020900050907     C*
021000050907     C* Inizializzo variabili
021100050907     C                   movel     *blanks       wrkEml          253
021200050907     C                   movel     *blanks       wrkEmlMsg      5000
021300050907     C                   movel     *blanks       wrkEmlOgg        44
021400050907     C* Valorizzo i campi della e-m@ail
021500050907     C                   eval      wrkEml='cedalert@bartolini.it'
021600050907     C                   eval      wrkEmlOgg='VAS - DOWNLOAD - ERROR!!!'
021700050907     C                   eval      wrkEmlMsg=wMessErrore
021800050907     C*
021900050907     C                   call(e)   'TIS701C'
022000050907     C                   parm                    wrkEml
022100050907     C                   parm                    wrkEmlOgg
022200050907     C                   parm                    wrkEmlMsg
022300050907     C*
022400050907     C                   endsr
022500041015     C******************************************************
022600041015     C     *INZSR        BEGSR
022700931217     C*
022800931217     C     *ENTRY        PLIST
022900050906     C                   PARM                    trul47ds
023000050906     C*
023100050906     C     KEYVGS_01C    KLIST
023200050906     C                   KFLD                    d47tip
023300050906     C                   KFLD                    JOB_NAME
023400050906     C                   KFLD                    USER
023500050906     C                   KFLD                    JOB_NUM
023600050907     C*
023700050907     C     KEYVGS_02C    KLIST
023800050907     C                   KFLD                    d47tip
023900050907     C                   KFLD                    kvgsLCK
024000050906     C*
024100050906     C* CALCOLA LA DATA CORRENTE
024200050907     C                   clear                   wlbda8
024300050906     C                   time                    wn14             14 0
024400050906     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
024500050906     C                   movel     wn14          oracor            6 0
024600050906     C                   z-add     wn8           g08dat
024700050906     C                   z-add     *zeros        g08inv
024800050906     C                   movel     '0'           g08err
024900050906     C                   call      'XSRDA8'
025000050906     C                   parm                    wlbda8
025100050906     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
025200050906     C*
025300931217     C                   ENDSR
