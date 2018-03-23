000100120306     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200900418     H DECEDIT('0,') DATEDIT(*DMY.)
000300050906     Ftivgs00f  O    E             DISK
000400050906     Ftivgs01l  UF   E           K DISK    rename(tivgs000:tivgs01)
000500050907     Ftivgs02l  UF   E           K DISK    rename(tivgs000:tivgs02)
000600940922     F*--------------------------------------------------------------*
000700050906     D trul47ds      E DS
000800050906     D
000900050906     D pgmstatus      SDS
001000050906     D  JOB_NAME             244    253                                         * Job name
001100050906     D  USER                 254    263                                         * User name
001200050906     D  JOB_NUM              264    269                                         * Job number
001300050907     D*------------------
001400050907     D* VARIABILI D WRK
001500050907     D*------------------
001600050907     D  kvgsLCK        S                   LIKE(vgsLCK)
001700050907     D  ExistTipF      S              1
001800050907     D  AllocTipF      S              1
001900050907     D  BloccTipF      S              1
002000050907     D  wMessErrore    s           1000    inz(*blanks)
002100050906     D*------------------
002200050906     D* DS "XSRDA8" - CONTROLLA DATA (8)
002300050906     D*------------------
002400050906     D WLBDA8          DS                  INZ
002500050906     D  G08DAT                 1      8  0
002600050906     D  G08INV                 9     16  0
002700050906     D  G08ERR                17     17
002800050906     D  G08TGI                18     22  0
002900050906     D*--------------------------------------------------------------*
003000050907     C*
003100050909     C* Innanzitutto ad ogni chiamata resetto i campi d output
003200050909     C                   eval      d47sts  = *blanks
003300050909     C                   eval      d47jsts = *blanks
003400050906     C*
003500050907     C********
003600050906     C* Operazioni di inizio
003700050907     C********
003800050907     C                   if        d47opz = 'I'
003900050907     C*
004000050907     C* Verifico subito se x il tipo file richiesto è presente qualche forzatura d allocazione
004100050907     C                   eval      kvgsLCK = 'S'
004200050907     C                   exsr      ChkTipF
004300050907     C*
004400050907     C* Se tipo file richiesto NN già allocato proseguo altrimenti esco subito e restituisco stato attuale
004500050907     C                   if        AllocTipF = 'N'
004600050907     C*
004700050906     C* Inserisco il record d blocco x il tipo file/job corrente
004800050906     C                   clear                   tivgs000
004900050906     C                   eval      VGSTIP  = d47tip
005000050906     C                   eval      VGSDAT  = datcor
005100050906     C                   eval      VGSORA  = oracor
005200050906     C                   eval      VGSJNAM = JOB_NAME
005300050906     C                   eval      VGSUNAM = USER
005400050906     C                   eval      VGSJNUM = JOB_NUM
005500050906     C                   eval      VGSPGM  = d47pgm
005600050907     C                   eval      VGSLCK  = d47lck
005700050907     C                   eval      VGSCHKJ = d47chkj
005800091001     C                   write(e)  tivgs000
005900050907     C*
006000050907     C                   else
006100050907     C                   eval      d47sts = 'A'
006200050907     C                   endif
006300050906     C                   endif
006400050907     C*
006500050907     C********
006600050907     C* Operazioni di verifica disponibilità elaborazione tipo file
006700050907     C********
006800050907     C                   if        d47opz = 'C'
006900050907     C*
007000050907     C* Verifico subito se x il tipo file richiesto è presente qualche forzatura d allocazione
007100050907     C                   eval      kvgsLCK = 'S'
007200050907     C                   exsr      ChkTipF
007300050907     C*
007400050907     C* Se dopo il 1° giro x i record semaforo CON forzatura allocazione esistono ancora record effettuo 2° giro
007500050907     C* x quelli SENZA forzatura allocazione
007600050907     C                   if        ExistTipF = 'S' AND
007700050907     C                             AllocTipF = 'N' AND
007800050907     C                             BloccTipF = 'N'
007900050907     C                   eval      kvgsLCK = *blanks
008000050907     C                   exsr      ChkTipF
008100050907     C                   endif
008200050907     C*
008300050907     C* Se a questo punto il tipo file è presente considero bloccata la sua elaborazione
008400050907     C                   if        ExistTipF = 'S'
008500050907     C                   eval      d47sts = 'B'
008600050907     C                   endif
008700050907     C                   endif
008800050907     C*
008900050907     C********
009000050906     C* Operazioni di fine
009100050907     C********
009200050906     C                   if        d47opz = 'F'
009300050907     C*
009400050907     C* Elimino il record d blocco x il tipo file/job corrente
009500050906     C     KEYVGS_01C    chain     tivgs01l
009600050906     C                   if        %found(tivgs01l)
009700050907     C                   delete    tivgs01l
009800050906     C                   else
009900050906     C                   eval      d47sts = '1'
010000050906     C                   endif
010100050906     C                   endif
010200050909     C*
010300050909     C********
010400050909     C* Operazioni di reperimento stato job specifico ricevuto in input
010500050909     C********
010600050909     C                   if        d47opz = 'J'
010700050909     C*
010800050909     C* Effettuo chiamata reprire lo stato attuale del job
010900050909     C                   call(e)   'TRUL47C'
011000050909     C                   parm                    d47jnam
011100050909     C                   parm                    d47unam
011200050909     C                   parm                    d47jnum
011300050909     C                   parm      *blanks       d47jsts
011400050909     C                   if        %error
011500050909     C                   eval      d47sts = '1'
011600050909     C                   endif
011700050909     C                   endif
011800050906     C*
011900050907     C********
012000050906     C* Operazioni di chiusura
012100050907     C********
012200050907     C                   if        d47opz = 'Z'
012300050906     C                   endif
012400050909     C*
012500050909     C                   seton                                        lr
012600050906     C*
012700050906     C******************************************************
012800050906     C     *PSSR         BEGSR
012900050906     C*
013000050906     C                   eval      d47sts = '1'
013100050906     C*
013200050906     C                   ENDSR     '*CANCL'
013300050907     C******************************************************
013400050907     C     ChkTipF       BEGSR
013500050907     C*
013600050907     C* D default considero il tipo file richiesto come NN allocato e NN bloccato
013700050907     C                   eval      ExistTipF = 'N'
013800050907     C                   eval      AllocTipF = 'N'
013900050907     C                   eval      BloccTipF = 'N'
014000050907     C*
014100050907     C* Verifico se x il tipo file corrente è già presente 1 record d allocazione forzata
014200081006     C     *loval        setll     tivgs02l
014300050907     C     KEYVGS_02C    setll     tivgs02l
014400050907     C                   if        %equal(tivgs02l)
014500050907     C     KEYVGS_02C    reade     tivgs02l
014600050907     C                   dow       not %eof(tivgs02l)
014700050907     C*
014800050907     C* Effettuo chiamata x verificare se il job è ancora valido
014900050907     C                   call(e)   'TRUL47C'
015000081006     C                   parm      vgsJNAM       d47jnam
015100081006     C                   parm      vgsUNAM       d47unam
015200081006     C                   parm      vgsJNUM       d47jnum
015300081006     C                   parm      *blanks       JOB_STATUS       10
015400050907     C*
015500050907     C* Se il job sul record semaforo corrente NN è attivo ed è richiesta la gestione in automatico elimino il
015600050907     C* record semaforo corrente
015700090916     C                   if        %error OR JOB_STATUS = '*OUTQ'
015800090916     C                                    OR JOB_STATUS = '*ERROR'
015900090916     C                   if        vgsCHKJ = 'N'
016000050907     C                   delete    tivgs02l
016100050907     C                   else
016200050907     C*
016300050907     C* Se il job sul record semaforo corrente NN è attivo ma nn è stata espressamente richiesta sullo stesso la
016400050907     C* gestione in automatico vuol dire che c'è qualcosa che si è interrotto in maniera anomala x cui mando
016500050907     C* segnalazione via mail agli operatori
016600050907     C                   eval      BloccTipF = 'S'
016700050907     C                   eval      wMessErrore = 'VERIFICARE IL FILE DOWNLOAD '+
016800050907     C                             'GENERICO TIVGD00F'                   +':/N'+
016900050907     C                             ' '                                   +':/N'+
017000050907     C                             'RIFERIMENTI'                         +':/N'+
017100050907     C                             'Tipo file: ' + d47tip                +':/N'+
017200050907     C                             'Programma: ' + d47pgm                +':/N'+
017300050907     C                             'Job: '+JOB_NAME+'/'+USER+'/'+JOB_NUM +':/N'+
017400050907     C                             ' '                                   +':/N'+
017500050907     C                             'MESSAGGIO'                           +':/N'+
017600050907     C                             'Il tipo file nel TIVGD00F è bloccato!!!'
017700050907     C                   exsr      sndeml
017800050907     C                   endif
017900050907     C                   else
018000050907     C*
018100050907     C* Se invece il job è già/ancora il tipo file è cmq in stato d blocco elaborazione
018200050907     C                   eval      BloccTipF = 'S'
018300050907     C*
018400050907     C* Se invece il job è già/ancora attivo ed ha il flag d forzatura allocazione valorizzato
018500050907     C                   if        vgsLCK = 'S'
018600050907     C                   eval      AllocTipF = 'S'
018700050907     C                   endif
018800050907     C                   endif
018900050907     C*
019000050907     C* Proseguo cmq la lettura x verificare sempre tutti i record semaforo
019100050907     C     KEYVGS_02C    reade     tivgs02l
019200050907     C                   enddo
019300050907     C                   endif
019400050907     C*
019500050907     C* A questo punto verifico se x il tipo file corrente è ancora presente almeno 1 record semaforo
019600081006     C     *loval        setll     tivgs02l
019700050907     C     d47tip        setll     tivgs02l
019800050907     C                   if        %equal(tivgs02l)
019900050907     C                   eval      ExistTipF = 'S'
020000050907     C                   else
020100050907     C*
020200050907     C* Se NN esiste nessun record semaforo x il tipo file corrente NN considero ovviamente nemmeno gli altri
020300050907     C* vincoli d blocco
020400050907     C                   eval      ExistTipF = 'N'
020500050907     C                   eval      AllocTipF = 'N'
020600050907     C                   eval      BloccTipF = 'N'
020700050907     C                   endif
020800050907     C*
020900050907     C                   ENDSR
021000050907     C******************************************************
021100050907      /TITLE Compongo il testo e spedisco una e-m@ail
021200050907     C     sndeml        begsr
021300050907     C*
021400050907     C* Inizializzo variabili
021500050907     C                   movel     *blanks       wrkEml          253
021600050907     C                   movel     *blanks       wrkEmlMsg      5000
021700050907     C                   movel     *blanks       wrkEmlOgg        44
021800050907     C* Valorizzo i campi della e-m@ail
021900120306     C                   eval      wrkEml='cedalert@brt.it'
022000050907     C                   eval      wrkEmlOgg='VAS - DOWNLOAD - ERROR!!!'
022100050907     C                   eval      wrkEmlMsg=wMessErrore
022200050907     C*
022300050907     C                   call(e)   'TIS701C'
022400050907     C                   parm                    wrkEml
022500050907     C                   parm                    wrkEmlOgg
022600050907     C                   parm                    wrkEmlMsg
022700050907     C*
022800050907     C                   endsr
022900041015     C******************************************************
023000041015     C     *INZSR        BEGSR
023100931217     C*
023200931217     C     *ENTRY        PLIST
023300050906     C                   PARM                    trul47ds
023400050906     C*
023500050906     C     KEYVGS_01C    KLIST
023600050906     C                   KFLD                    d47tip
023700050906     C                   KFLD                    JOB_NAME
023800050906     C                   KFLD                    USER
023900050906     C                   KFLD                    JOB_NUM
024000050907     C*
024100050907     C     KEYVGS_02C    KLIST
024200050907     C                   KFLD                    d47tip
024300050907     C                   KFLD                    kvgsLCK
024400050906     C*
024500050906     C* CALCOLA LA DATA CORRENTE
024600050907     C                   clear                   wlbda8
024700050906     C                   time                    wn14             14 0
024800050906     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
024900050906     C                   movel     wn14          oracor            6 0
025000050906     C                   z-add     wn8           g08dat
025100050906     C                   z-add     *zeros        g08inv
025200050906     C                   movel     '0'           g08err
025300050906     C                   call      'XSRDA8'
025400050906     C                   parm                    wlbda8
025500050906     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
025600050906     C*
025700931217     C                   ENDSR
