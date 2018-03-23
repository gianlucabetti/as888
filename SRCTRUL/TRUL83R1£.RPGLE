000100040928      **********************************************************
000200051102      *           Compilare da: QSECOFR                        *
000300051102      *           Compilare con USRPRF: *OWNER                 *
000400040928      **********************************************************
000500040607     FTRUL83D1  CF   E             WORKSTN
000600040217     FTIMAS01L  IF   E           K DISK
000700041013     FTNTBE01L  IF   E           K DISK
000800041008     FPRTEMAIL  O    F  132        PRINTER OFLIND(*INOF) USROPN
000900040928      **********************************************************
001000040130     D*
001100051025     D*------------------
001200041006     D* DS X RIDEFINIZIONE PASSAGGIO SCHIERA SPOOL SELEZIONATI A VIDEO
001300051025     D*------------------
001400041006     D DSSPOOL         DS
001500041006     D  File                         10
001600041006     D  Job                          10
001700041006     D  User                         10
001800041006     D  JobX                          6
001900041006     D  SplfXx                        6
002000041006     D  UsrDta                       10
002100041006     D  SplRig                        9  0
002200041006     D  SplCol                        9  0
002300041006     D  SplCPI                        9  1
002400041006     D  OutQue                       10
002500041006     D  DataOra                      17
002600041006     D  TotPagine                     6  0
002700041006     D  StatoSpool                    6
002800051025     D*------------------
002900051025     D* DS REPERIMENTO LISTA SPOOL - INTERNAL SPOOL & JOB ID
003000051025     D*------------------
003100051025     D XLSTSPLDS2    E DS
003200051025     D*------------------
003300051025     D* DS REPERIMENTO ATTRIBUTI SPOOL
003400051025     D*------------------
003500051025     D XSPLATRDS     E DS
003600051025     D XSPLATRDS1    E DS
003700051025     D*------------------
003800051025     D DSSPOOLID       DS
003900051025     D  Sp_Job_ID                          like(XLSTSPLDS2)
004000051025     D  Sp_Job_Codr                   3
004100051025     D*------------------
004200041006     D* SCHIERE SPOOL SELEZIONATI A VIDEO
004300051025     D*------------------
004400051025     D SKSPOOL         S                   like(DSSPOOL)   dim(99)
004500051102     D*
004600051102     D SKSPOOLIDDS     DS
004700051102     D  SKSPOOLID                          like(DSSPOOLID) dim(99)
004800041006     D*------------------
004900041006     D* DS X REPERIMENTO NUMERATORE
005000041006     D*------------------
005100041006     D TRUL33DS      E DS                  INZ
005200041006     D*------------------
005300041006     D* ARCHITETTURA
005400041006     D*------------------
005500041006     D KPJBA         E DS
005600041013     D*------------------
005700041013     D* TABELLEA 'MRA'
005800041013     D*------------------
005900041013     D DMRA          E DS
006000040130
006100040130     D* Parametri x modifiche attributi spool
006200040130     D PARAM           S            262    DIM(20)
006300040130
006400040130     D* DS scomposizione parametri x modifiche attributi spool
006500040130     D SPOOLATR        DS
006600040130     D  POSDA                  1      4  0
006700040130     D  LUNG                   5      7  0
006800040130     D  VALORE                 8    262
006900040217
007000040217     D* DS ridefinizione dati utente estesi spool x mailing
007100040217     D TRTCM1DS      E DS
007200040217
007300040217     D* DS REPERIMENTO DATI UTENTE
007400040217     D TIBS34DS      E DS                                                       *Profili utente
007500040217     D DDATIUTE      E DS                                                       *Dati utente
007600040217     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007700040521
007800040521     D* DS x richiamo *pgm utilità d controllo indirizzi e-mail
007900040521     D DSEMAIL       E DS
008000040130
008100040218     D* Variabili riferite al data-base
008200040218     D KmasSPOOL       S                   like(masSPOOL)
008300040224     D KmasUSDTA       S                   like(masUSDTA)
008400040218     D KmasUTE         S                   like(masUTE)
008500040218
008600040218     D* Informazioni sul job
008700040218     D psds           sds
008800040218     D  psuser               254    263
008900041006
009000041006     D* Variabili d wrk
009100041006     D FlgAbil         S              1    inz('S')
009200041006     D wNumSpool       S              2  0
009300041006     D wNumSpoolCor    S              2  0
009400041006     D wNumSpoolTot    S              2  0
009500040224
009600040217
009700040217     C* /MAIN
009800051025     C*
009900051025     C* Se tutto ok in operazioni iniziali procedo
010000051025     C                   IF        *IN70 = *OFF
010100041013     C*
010200041013     C* Reperisco dati tabellati (Tabella 'MRA')
010300041013     C                   EXSR      CARTAB
010400041006     C*
010500041006     C* Verifico subito le abilitazioni e le regole x lo spool indicato dall'utente
010600051017     C* (effettuo questo controllo solo x le chiamate x singolo spool)
010700041006     C                   IF        wNumSpool = 1
010800041006     C                   EVAL      DSSPOOL = SKSPOOL(1)
010900041006     C                   EXSR      CHKSPOOL
011000041006     C                   ENDIF
011100040218     C*
011200040218     C* Se lo spool è specificatamente negato x l'utente corrente => NN elaboro e restituisco messaggio
011300041006     C                   IF        FlgAbil = 'N'
011400040218     C                   EVAL      Esito = '2'
011500040218     C                   EVAL      Messaggio = 'Non si è autorizzati ad ' +
011600040218     C                                         'inviare lo spool indicato.'
011700040218     C                   ELSE
011800041006     C*
011900041006     C* Nel nei parametri d ingresso è stato richiesto anche l'invio (oltre al controllo abilitazione allo spool)
012000041006     C* procedo all'elaborazione ad all'invio della mail
012100041006     C                   IF        FlgSnd = 'S'
012200040217     C*
012300040217     C* Ciclo finchè nn scelta opzione valida
012400040217     C                   DOW       *INKL = *OFF
012500041006     C*
012600041006     C* Effettuo inizializzazione videata
012700041006     C                   EXSR      INZVID
012800040217     C*
012900040217     C* Emetto la window x ricevere dall'utente gli indirizzi e-mail mittente e destinatari
013000051025     C                   IF        FlgDspVid <> 'N'
013100040520     C                   EXFMT     UL831DW01
013200040224     C*
013300040224     C* Effettuo controllo dell'input
013400040224     C                   EXSR      CHKINPUT
013500051025     C                   ELSE
013600051025     C* Se chiamata con parametri forzati in input do x assunto che sia tutto ok => quindi bypasso i controlli
013700051025     C                   EVAL      FlgOK = 'S'
013800051025     C                   ENDIF
013900040217     C*
014000040217     C* Se F06 elaboro
014100040217     C     *INKF         IFEQ      *ON
014200051024     C     FlgDspVid     OREQ      'N'
014300051024     C*
014400040224     C* Se tutto ok => procedo
014500040224     C                   IF        FlgOK = 'S'
014600041006     C*
014700041006     C* Se si è abilitati come prima cosa stacco 1 numeratore
014800041006     C                   EXSR      REPNUM
014900041006     C*
015000041006     C* Se no errori in reperimento numeratore proseguo con l'elaborazione, altrimenti esco e restituisco messaggio
015100041006     C                   IF        Esito <> '2'
015200041006     C*
015300041006     C* Ciclo x tutti gli spool scelti
015400041006     C                   EVAL      wNumSpoolCor = 1
015500051104     C*
015600051104     C* Se richiesta gestione del "tappo" forzato in ingresso NN modifico il numero spool totali
015700051104     C                   IF        FlgTappoLast <> 'S'
015800041006     C                   EVAL      wNumSpoolTot = wNumSpool + 1
015900051104     C                   ELSE
016000051104     C                   EVAL      wNumSpoolTot = wNumSpool
016100051104     C                   ENDIF
016200041006     C                   DOW       wNumSpoolCor <= wNumSpool
016300041006     C                   EVAL      DSSPOOL = SKSPOOL(wNumSpoolCor)
016400041006     C                   EXSR      CHKSPOOL
016500040224     C                   EXSR      PROCEDI
016600041006     C                   ADD       1             wNumSpoolCor
016700051102     C                   ENDDO
016800051102     C*
016900051102     C* Se nn espressamente richiesta gestione "nn standard" =>
017000051102     C                   IF        FlgTappoLast <> 'S'
017100041006     C*
017200041006     C* Sottometto quindi l'elaborazione dello spool d chiusura contenete il testo della mail
017300041006     C* ...quindi lascio la DS d procedura (TRTCM1DS) intatta come i precedenti lanci e
017400041006     C* ...sostituisco solo la regola appropriata
017500041006     C*
017600041006     C* Imposto il numero d spool corrente = al numero d spool totali che fanno parte dello stesso invio
017700041006     C                   MOVE(P)   wNumSpoolTot  §CM1ATT
017800041006     C                   MOVE(P)   wNumSpoolTot  §CM1TOTATT
017900041006     C*
018000041013     C* Imposto la regola dello spool e gli altri dati fondamentali (processo, stato, etc...)
018100041013     C                   EVAL      §CM1TIPS = §MRAREG
018200041013     C                   EVAL      §CM1IDP  = §MRAIDPRO
018300041013     C   60              EVAL      §CM1STS  = 'H'
018400041006     C*
018500041006     C* Eseguo override x generare uno spool ad "hoc"
018600041006     C                   CALL      'TRUL83C1'
018700041006     C                   PARM      'O'           OPZIONE           1
018800060810     C                   PARM      'PRTEMAIL'    PRTFILE          10
018900041006     C                   PARM                    TRTCM1DS
019000041013     C                   PARM                    §MRAOUTQI
019100041013     C                   PARM                    Esito
019200041006     C* Se nn c sono errori d override proseguo con l'elaborazione, altrimenti mando messaggio all'utente
019300041006     C                   IF        Esito = '2'
019400041006     C                   EVAL      Messaggio = 'Errori nel''invio della mail'
019500041006     C                   ELSE
019600041006     C                   OPEN      PRTEMAIL
019700041006     C                   EXCEPT    DET
019800041006     C                   CLOSE     PRTEMAIL
019900041006     C* Eseguo delete override precedente
020000041006     C                   CALL      'TRUL83C1'
020100060810     C                   PARM      'D'           OPZIONE
020200060810     C                   PARM                    PRTFILE
020300041006     C                   PARM                    TRTCM1DS
020400041013     C                   PARM                    §MRAOUTQI
020500041006     C                   PARM                    Esito
020600041006     C                   IF        Esito = '2'
020700041006     C                   EVAL      Messaggio = 'Errori nel''invio della mail'
020800041006     C                   ENDIF
020900041006     C                   ENDIF
021000051102     C*
021100051102     C                   ENDIF
021200051104     C*
021300051104     C* Dopo l'invio della mail forzo l'uscita dal programma
021400051104     C                   EVAL      *INKL = *ON
021500051102     C*
021600040224     C                   ENDIF
021700040217     C                   ENDIF
021800041006     C                   ENDIF
021900040224     C*
022000040520     C* Se F05 visualizzo lo spool
022100041006     C  N51*INKE         IFEQ      *ON
022200041006     C                   CALL      'TRUL83C'
022300040224     C                   PARM      '5'           Opzione           1
022400040224     C                   PARM                    File
022500040224     C                   PARM                    Job
022600040224     C                   PARM                    User
022700040224     C                   PARM                    JobX
022800040224     C                   PARM                    SplfXx
022900040224     C                   PARM                    Esito
023000040224     C                   IF        Esito = '2'
023100040224     C                   EVAL      Messaggio = 'Errori nella visualizzazione ' +
023200040224     C                                         'dello spool'
023300040224     C                   ENDIF
023400040224     C                   ENDIF
023500040217     C*
023600040217     C* Se F12 vado a fine
023700040217     C     *INKL         IFEQ      *ON
023800040218     C                   EVAL      *INKL = *ON
023900040217     C                   ENDIF
024000040217     C*
024100040217     C                   ENDDO
024200041006     C*
024300041006     C                   ENDIF
024400040218     C                   ENDIF
024500051025     C                   ENDIF
024600040217     C*
024700051025     C* Al termine delle operazioni se richiesto nei parametri in input e se TUTTO è andato bene
024800051025     C* elimino gli spool originali
024900051025     C                   IF        FlgDltSplIn = 'S' AND
025000051025     C                             Esito <> '2'
025100051104     C                   Z-ADD     1             x                 5 0
025200051104     C                   DOW       x <= %elem(SKSPOOL)
025300051025     C                   IF        SKSPOOL(x) <> *blanks
025400051104     C                   EVAL      DSSPOOL = SKSPOOL(x)
025500051025     C                   CALL      'TRUL83C'
025600051025     C                   PARM      '4'           Opzione           1
025700051025     C                   PARM                    File
025800051025     C                   PARM                    Job
025900051025     C                   PARM                    User
026000051025     C                   PARM                    JobX
026100051025     C                   PARM                    SplfXx
026200051025     C                   PARM                    Esito
026300051104     C                   ELSE
026400051104     C                   LEAVE
026500051025     C                   ENDIF
026600051104     C                   ADD       1             x
026700051025     C                   ENDDO
026800051025     C                   ENDIF
026900051025     C*
027000040217     C                   SETON                                        LR
027100040217     C****************************************************************
027200040524
027300040524
027400040524
027500040524     C****************************************************************
027600040524     C* INIZIALIZZA LA VIDEATA CON I VALORI D DEFAULT
027700040524     C****************************************************************
027800040524     C     INZVID        BEGSR
027900041006     C*
028000041006     C* Gestisco subito indicatori "particolari"
028100041006     C                   SETOFF                                       50
028200040524     C*
028300040524     C* Inizializzo la videata
028400040524     C                   IF        MITT = *blanks
028500040524     C                   SETOFF                                       30
028600040524     C* Se specificato indirizzo e-mail mittente in chiamata imposto quello
028700040607     C                   IF        EmailMitt <> *blanks
028800040607     C                   EVAL      MITT = EmailMitt
028900040524     C                   SETON                                        20
029000040524     C                   ELSE
029100040524     C* Altrimenti verifico se valorizzato campo UTEEMA (email utente) dell'utente corrente su AZUTE00F
029200040524     C                   IF        UTEEMA <> *blanks
029300040524     C* Se presenti + indirizzi e-mail abbinati all'utente come default considero il primo
029400040524     C                   IF        %scan(';':UTEEMA) > *zeros
029500040524     C                   EVAL      MITT = %subst(%trim(UTEEMA):1:
029600040524     C                                    %scan(';':UTEEMA)-1)
029700040524     C                   ELSE
029800040524     C                   EVAL      MITT = %trim(UTEEMA)
029900040524     C                   ENDIF
030000040524     C                   ELSE
030100040524     C* Se nn presente indirizzo e-mail abbinato all'utente corrente su AZUTE00F forzo sempre l'"info" del P.O.
030200040524     C* utente corrente
030300041021     C                   EVAL      MITT = %trim(§MRAMIT1L) +
030400041021     C                                    %trim(%editw(DUTPOU:'0   '))
030500040524     C                   ENDIF
030600040524     C                   ENDIF
030700040524     C                   ENDIF
030800040524     C*
030900041006     C* Se richiesto singolo spool imposto i dati a video altrimenti forzo dicitura
031000041006     C                   IF        *IN51 = *OFF
031100051017     C                   EVAL      SPOOL = 'Spool   : ' +
031200051017     C                                     %trim(File)+'/'+%trim(User)+'/'+
031300040524     C                                     %trim(OutQue)+'/'+%trim(StatoSpool)+
031400040524     C                                     '/'+%trim(UsrDta)
031500040524     C                   EVAL      DATORA = DataOra
031600040524     C                   EVAL      NPAG = TotPagine
031700041006     C                   ELSE
031800041006     C                   EVAL      SPOOL = 'Selezione multipla'
031900041006     C                   ENDIF
032000051025     C*
032100051025     C*************
032200051025     C* FORZATURE DA PARAMETRI IN INPUT
032300051025     C*************
032400051025     C*
032500051025     C* Imposto e-mail mittente se passato
032600051025     C                   if        EmailMitt <> *blanks
032700051025     C                   eval      MITT = EmailMitt
032800051025     C                   endif
032900051024     C*
033000051024     C* Imposto e-mail destinatario se passato
033100051025     C                   if        EmailDest <> *blanks
033200051025     C                   eval      DEST1 = %subst(EmailDest:1:55)
033300051025     C                   eval      DEST2 = %subst(EmailDest:56:55)
033400051024     C                   endif
033500051024     C*
033600051024     C* Imposto oggetto se passato
033700051024     C                   if        EmailObj <> *blanks
033800051025     C                   eval      OGGETTO = EmailObj
033900051024     C                   endif
034000051024     C*
034100051024     C* Imposto testo se passato
034200051024     C                   if        EmailTxt1 + EmailTxt2 + EmailTxt3 +
034300051024     C                             EmailTxt4 + EmailTxt5 + EmailTxt6 +
034400051024     C                             EmailTxt7 + EmailTxt8 <> *blanks
034500051025     C                   eval      TESTO1 = EmailTxt1
034600051025     C                   eval      TESTO2 = EmailTxt2
034700051025     C                   eval      TESTO3 = EmailTxt3
034800051025     C                   eval      TESTO4 = EmailTxt4
034900051025     C                   eval      TESTO5 = EmailTxt5
035000051025     C                   eval      TESTO6 = EmailTxt6
035100051025     C                   eval      TESTO7 = EmailTxt7
035200051025     C                   eval      TESTO8 = EmailTxt8
035300051024     C                   endif
035400040524     C*
035500040524     C                   ENDSR
035600040524     C***************************************************************
035700040218
035800040218
035900040218     C****************************************************************
036000040218     C     CHKSPOOL      BEGSR
036100040218     C****************************************************************
036200040218     C*
036300040218     C* Reperimento "codice regola mailing" x spool corrente
036400050127     C                   MOVEL     'S'           FlgAbil
036500040224     C*
036600040224     C* Effettuo 1° verifica x nome file spool + dati utente + utente
036700040224     C* x verificare eventuali abilitazioni/negazioni specifiche
036800040224     C                   EVAL      KmasSPOOL = File
036900040224     C                   EVAL      KmasUSDTA = UsrDta
037000040224     C                   EVAL      KmasUTE   = psuser
037100040224     C     KEYMAS01C     CHAIN     TIMAS01L
037200040224     C                   IF        %found(TIMAS01L)
037300040224     C                   MOVEL     MASABIL       FlgAbil
037400040224     C                   ELSE
037500040224     C*
037600040224     C* Effettuo 2° verifica x nome file spool + dati utente
037700040224     C* x verificare eventuali abilitazioni/negazioni d gruppo sull'utente
037800040224     C     KEYMAS01P     SETLL     TIMAS01L
037900040218     C*
038000040224     C* Se trovato verifico se l'utente o il gruppo è presente e le relative abilitazioni/negazioni
038100040218     C                   IF        %equal(TIMAS01L)
038200040224     C     KEYMAS01P     READE     TIMAS01L
038300040218     C                   DOW       not %eof(TIMAS01L)
038400040218     C                   IF        %subst(psuser:1:(%len(%trim(masUTE)))) =
038500041013     C                             %trim(masUTE) OR masUTE = '*ALL'
038600040224     C                   MOVEL     MASABIL       FlgAbil
038700040218     C                   LEAVE
038800040218     C                   ENDIF
038900040224     C     KEYMAS01P     READE     TIMAS01L
039000040218     C                   ENDDO
039100040224     C                   ELSE
039200040224     C*
039300040224     C* Effettuo 3° verifica x nome file spool
039400040224     C* x verificare eventuali abilitazioni/negazioni d gruppo su dati utente e utente
039500040224     C     File          SETLL     TIMAS01L
039600040224     C*
039700040224     C* Se trovato verifico se l'utente o il gruppo è presente e le relative abilitazioni/negazioni
039800040224     C                   IF        %equal(TIMAS01L)
039900040224     C     File          READE     TIMAS01L
040000040224     C                   DOW       not %eof(TIMAS01L)
040100040224     C                   IF        masUSDTA = UsrDta  OR
040200041013     C                             masUSDTA = '*ALL'
040300040224     C                   IF        %subst(psuser:1:(%len(%trim(masUTE)))) =
040400041013     C                             %trim(masUTE) OR masUTE = '*ALL'
040500040224     C                   MOVEL     MASABIL       FlgAbil
040600040224     C                   LEAVE
040700040224     C                   ENDIF
040800040224     C                   ENDIF
040900040224     C     File          READE     TIMAS01L
041000040224     C                   ENDDO
041100040607     C                   ELSE
041200040607     C*
041300040607     C* Effettuo 4° verifica x utente e nome spool "*ALL"
041400040607     C* x verificare eventuali abilitazioni/negazioni d gruppo su dati utente e utente
041500040607     C                   EVAL      KmasSPOOL = '*ALL'
041600040607     C                   EVAL      KmasUSDTA = '*ALL'
041700041013     C     KEYMAS01P     SETLL     TIMAS01L
041800041013     C*
041900041013     C* Se trovato verifico se l'utente o il gruppo è presente e le relative abilitazioni/negazioni
042000041013     C                   IF        %equal(TIMAS01L)
042100041013     C     KEYMAS01P     READE     TIMAS01L
042200041013     C                   DOW       not %eof(TIMAS01L)
042300041013     C                   IF        %subst(psuser:1:(%len(%trim(masUTE)))) =
042400041013     C                             %trim(masUTE) OR masUTE = '*ALL'
042500041013     C                   MOVEL     MASABIL       FlgAbil
042600041013     C                   LEAVE
042700041013     C                   ENDIF
042800041013     C     KEYMAS01P     READE     TIMAS01L
042900041013     C                   ENDDO
043000041013     C                   ENDIF
043100041013     C*                  EVAL      KmasUTE   = psuser
043200041013     C*    KEYMAS01C     CHAIN     TIMAS01L
043300041013     C*                  IF        %found(TIMAS01L)
043400041013     C*                  MOVEL     MASABIL       FlgAbil
043500041013     C*                  ENDIF
043600040607     C                   ENDIF
043700040224     C                   ENDIF
043800040224     C                   ENDIF
043900041006     C*
044000041006     C                   IF        MASCODR = *blanks
044100041006     C***                SETON                                        50
044200041006     C                   IF        FORMATO = *blanks
044300041006     C                   EVAL      FORMATO = 'T'
044400041006     C                   ENDIF
044500041006     C                   ENDIF
044600051025     C*
044700051025     C* Se richiesto di bypassare il controllo del file di spool imposto
044800051025     C* il flag di abilitazione a "S"
044900051025     C                   if        FlgBypChSp = 'S'
045000051017     C                   MOVEL     'S'           FlgAbil
045100051025     C                   endif
045200040218     C*
045300040218     C                   ENDSR
045400040218     C****************************************************************
045500040217
045600040217
045700040217     C****************************************************************
045800040217     C     CHKINPUT      BEGSR
045900040217     C****************************************************************
046000040224     C*
046100040224     C                   CLEAR                   MSGERR
046200040217     C*
046300040217     C* Inizializzo il flag d controllo input
046400040217     C                   MOVEL     'S'           FlgOK             1
046500041013     C*
046600041013     C* Spengo gli indicatori d errore a video
046700041013     C                   SETOFF                                       303132
046800041013     C                   SETOFF                                       33
046900041013     C*
047000041013     C* Effettuo controllo su reperimento con successo record 'TRUL83R' in tabella 'MRA'
047100041013     C                   IF        *IN60 = *on
047200041013     C                   SETON                                        303132
047300041013     C                   SETON                                        33
047400041013     C                   MOVEL     'N'           FlgOK
047500041013     C                   EVAL      MSGERR = 'Record TRUL83R non trovato in ' +
047600041013     C                                      'tabella MRA, avvisare il CED.'
047700041013     C                   ENDIF
047800040217     C*
047900040217     C* Effettuo controlli obbligatorietà parametri a video
048000040224     C                   IF        FlgOK = 'S'
048100040217     C                   IF        MITT = *blanks
048200040217     C                   SETON                                        30
048300040217     C                   MOVEL     'N'           FlgOK
048400040521     C                   EVAL      MSGERR = 'Campo obbligatorio'
048500040217     C                   ENDIF
048600041005     C                   ENDIF
048700041005     C                   IF        FlgOK = 'S'
048800040224     C                   IF        DEST1 + DEST2 = *blanks
048900040217     C                   SETON                                        31
049000040217     C                   MOVEL     'N'           FlgOK
049100040521     C                   EVAL      MSGERR = 'Campo obbligatorio'
049200040217     C                   ENDIF
049300040224     C                   ENDIF
049400041005     C                   IF        FlgOK = 'S'
049500041005     C                   IF        OGGETTO = *blanks
049600041005     C                   SETON                                        32
049700041005     C                   MOVEL     'N'           FlgOK
049800041005     C                   EVAL      MSGERR = 'Campo obbligatorio'
049900041005     C                   ENDIF
050000041005     C                   ENDIF
050100041005     C                   IF        FlgOK = 'S'
050200041005     C                   IF        TESTO1+TESTO2+TESTO3+TESTO4+TESTO5+TESTO6+
050300041005     C                             TESTO7+TESTO8 = *blanks
050400041005     C                   SETON                                        33
050500041005     C                   MOVEL     'N'           FlgOK
050600041005     C                   EVAL      MSGERR = 'Campo obbligatorio'
050700041005     C                   ENDIF
050800041005     C                   ENDIF
050900040524     C*
051000040524     C* Effettuo controllo su abilitazione indirizzo e-mail mittente
051100040524     C                   IF        FlgOK = 'S'
051200041021     C                   IF        MITT = %trim(§MRAMIT1L) +
051300041021     C                                    %trim(%editw(DUTPOU:'0   '))
051400040607     C                             OR MITT = EmailMitt OR
051500040524     C                             %scan(%trim(MITT):UTEEMA) > *zeros
051600040524     C                   ELSE
051700040524     C                   SETON                                        30
051800040524     C                   MOVEL     'N'           FlgOK
051900040524     C                   EVAL      MSGERR = 'Indirizzo e-mail non consentito'
052000040524     C                   ENDIF
052100040524     C                   ENDIF
052200041005     C*
052300041005     C* Effettuo controlli su correttezza formale indirizzo e-mail mitente
052400041005     C                   IF        FlgOK = 'S'
052500041005     C                   CLEAR                   DSEMAIL
052600120314     C                   EVAL      §EMLINDI = %trim(MITT) + '@brt.it'
052700041005     C* Se c'è 1 indirizzo da controllare chiamo il *pgm d utilità controllo formale indirizzi e-mail
052800041005     C                   IF        §EMLINDI <> *blanks
052900041005     C                   CALL      'XEMAIL'
053000041005     C                   PARM                    DSEMAIL
053100041005     C                   IF        §EMLERRO = '1'
053200041005     C                   MOVEL     'N'           FlgOK
053300041005     C                   SETON                                        30
053400041005     C                   EVAL      MSGERR=§EMLMSGO
053500041005     C                   ENDIF
053600041005     C                   ENDIF
053700041005     C                   ENDIF
053800040224     C*
053900041005     C* Effettuo controlli su correttezza formale singoli indirizzi e-mail destinatari
054000040521     C                   IF        FlgOK = 'S'
054100040521     C                   CLEAR                   DSEMAIL
054200040929     C                   EVAL      §EMLINDI = %trim(DEST1 + DEST2)
054300040521     C* Se c'è 1 indirizzo da controllare chiamo il *pgm d utilità controllo formale indirizzi e-mail
054400040521     C                   IF        §EMLINDI <> *blanks
054500040521     C                   CALL      'XEMAIL'
054600040521     C                   PARM                    DSEMAIL
054700040521     C                   IF        §EMLERRO = '1'
054800040521     C                   MOVEL     'N'           FlgOK
054900040929     C                   SETON                                        31
055000040929     C                   EVAL      MSGERR=§EMLMSGO
055100040629     C                   ELSE
055200090714     C                   EVAL      DEST1 = %subst(§EMLINDO:1:55)
055300090714     C                   EVAL      DEST2 = %subst(§EMLINDO:56:100-55)
055400040521     C                   ENDIF
055500040521     C                   ENDIF
055600040521     C                   ENDIF
055700040217     C*
055800040217     C                   ENDSR
055900040217     C****************************************************************
056000040217
056100040217
056200040217     C****************************************************************
056300040217     C     PROCEDI       BEGSR
056400040217     C****************************************************************
056500040217     C*
056600040218     C                   CLEAR                   TRTCM1DS
056700041006     C*
056800041006     C* Come primissima cosa valorizzo l'ID Progressivo "univoco"
056900041006     C                   MOVE(P)   O33NRF        §CM1PROG
057000041013     C*
057100041013     C* Imposto la regola dello spool e gli altri dati fondamentali (processo, stato, etc...)
057200041013     C                   EVAL      §CM1TIPS = §MRAREG
057300041013     C                   EVAL      §CM1IDP  = §MRAIDPRO
057400041013     C   60              EVAL      §CM1STS  = 'H'
057500041006     C*
057600041006     C* Imposto il numero d spool corrente e il numero d spool totali che fanno parte dello stesso invio
057700041006     C                   MOVE(P)   wNumSpoolCor  §CM1ATT
057800041006     C                   MOVE(P)   wNumSpoolTot  §CM1TOTATT
057900041006     C*
058000041006     C* Imposto l'oggetto della mail
058100041006     C                   EVAL      §CM1VAR = '*OBJM*' + OGGETTO
058200041006     C*
058300041006     C* Imposto la regola dello spool
058400051025     C                   MOVEL     *blanks       wCODR             3
058500051102     C* => prima imposto la regola dello spool da file abilitazioni/forzature invii spool (no x "tappo" in input)
058600051104     C***                IF        FlgTappoLast <> 'S'
058700051104     C                   IF        wNumSpoolCor <> wNumSpoolTot
058800051025     C                   EVAL      wCODR = MASCODR
058900051102     C                   ENDIF
059000051025     C* => poi verifico se passati in input codici regola specifici x ogni spool
059100051025     C                   EVAL      DSSPOOLID = SKSPOOLID(wNumSpoolCor)
059200051025     C                   IF        Sp_Job_Codr <> *blanks
059300051025     C                   EVAL      wCODR = Sp_Job_Codr
059400051025     C                   ENDIF
059500051025     C* => altrimenti procedo con l'attribuzione d 1 regola generica desunta dagli attributi stessi sello spool corrente
059600051025     C                   IF        wCODR   <> *blanks
059700051025     C                   EVAL      §CM1TIPS = wCODR
059800040217     C* Se NN precisata regola x lo spool corrente procedo con l'attribuzione d 1 regola generica
059900040217     C                   ELSE
060000040928     C*
060100040928     C* Verifico quale scelta formato ha indicato l'utente a video
060200040928     C                   IF        FORMATO = 'L'
060300040928     C                   EVAL      §CM1TIPS = 'LET'
060400040928     C                   ENDIF
060500040928     C                   IF        FORMATO = 'T'
060600040217     C* Il numero d colonne MAX in decimi x restare in verticale è 82 x cui procedo a decimalizzare i CPI
060700040929     C                   EVAL      §CM1TIPS = 'TBL'
060800040929     C*                  IF        (SplCol / SplCPI * 10) < 82
060900040929     C*                  EVAL      §CM1TIPS = 'TBV'
061000040929     C*                  ELSE
061100040929     C*                  EVAL      §CM1TIPS = 'TBO'
061200040929     C*                  ENDIF
061300040217     C                   ENDIF
061400040928     C                   ENDIF
061500040524     C*
061600040524     C* Imposto come P.O. quello dell'utente che effettua la richiesta
061700040217     C                   MOVE      DUTPOU        §CM1PO
061800040217     C*
061900040218     C* Modifico gli attributi dello spool scelto x effettuare la copia x l'elaborazione
062000040218     C                   CLEAR                   PARAM
062100040520     C*
062200040520     C* Modifico a *NO il salvataggio del file d spool dopo la scrittura
062300040520     C                   CLEAR                   SPOOLATR
062400040520     C                   EVAL      POSDA = 138
062500040520     C                   EVAL      LUNG  = 10
062600040520     C                   EVAL      VALORE = '*NO'
062700040520     C                   EVAL      PARAM(1) = SPOOLATR
062800040520     C*
062900040520     C* Modifico l'utente proprietario dello spool "destinazione"
063000040520     C                   CLEAR                   SPOOLATR
063100040520     C                   EVAL      POSDA = 58
063200040520     C                   EVAL      LUNG  = 10
063300040520     C                   EVAL      VALORE = 'EDPMAIL'
063400040520     C                   EVAL      PARAM(2) = SPOOLATR
063500040520     C*
063600040520     C* Modifico la OUTQ dello spool "destinazione"
063700040520     C                   CLEAR                   SPOOLATR
063800040520     C                   EVAL      POSDA = 190
063900040520     C                   EVAL      LUNG  = 10
064000041013     C                   EVAL      VALORE = §MRAOUTQI
064100040520     C                   EVAL      PARAM(3) = SPOOLATR
064200040520     C*
064300040520     C* Modifico la libreria della OUTQ dello spool "destinazione"
064400040520     C                   CLEAR                   SPOOLATR
064500040520     C                   EVAL      POSDA = 200
064600040520     C                   EVAL      LUNG  = 10
064700040520     C                   EVAL      VALORE = 'GAITRAOBJ'
064800040520     C                   EVAL      PARAM(4) = SPOOLATR
064900040520     C*
065000040218     C* Modifico il parametro USRDFNDTA (dati utente estesi) con quanto indicato a video dall'utente
065100040218     C                   EVAL      §CM1MITT = MITT
065200040218     C                   EVAL      §CM1DST  = %trim(DEST1) +
065300040224     C                                        %trim(DEST2)
065400040130     C                   CLEAR                   SPOOLATR
065500040130     C                   EVAL      POSDA = 3460
065600040130     C                   EVAL      LUNG  = 255
065700040217     C                   EVAL      VALORE = TRTCM1DS
065800040218     C                   EVAL      PARAM(5) = SPOOLATR
065900040520     C*
066000040520     C* Modifico lo stato dello spool "destinazione" in modo che sia sempre a *RDY
066100040520     C                   CLEAR                   SPOOLATR
066200040520     C                   EVAL      POSDA = 108
066300040520     C                   EVAL      LUNG  = 10
066400040520     C                   EVAL      VALORE = '*READY'
066500040520     C                   EVAL      PARAM(6) = SPOOLATR
066600040520     C*
066700040520     C* Modifico a *NO il precongelamento prima della scrittura
066800040520     C                   CLEAR                   SPOOLATR
066900040520     C                   EVAL      POSDA = 128
067000040520     C                   EVAL      LUNG  = 10
067100040520     C                   EVAL      VALORE = '*NO'
067200040520     C                   EVAL      PARAM(7) = SPOOLATR
067300040130     C*
067400040217     C* Effettuo la copia dello spool con gli attributi così modificati
067500040130     C                   CALL      'XDUPSPLFR'
067600040130     C                   PARM                    File
067700040130     C                   PARM                    Job
067800040130     C                   PARM                    User
067900040130     C                   PARM                    JobX
068000040130     C                   PARM                    SplfXx
068100040130     C                   PARM                    PARAM
068200040217     C*
068300040217     C                   ENDSR
068400040217     C****************************************************************
068500041006
068600041006
068700041006     C****************************************************************
068800041006     C     REPNUM        BEGSR
068900041006     C****************************************************************
069000041006     C*
069100041006     C* INIALIAZIZZO FLAG ERRORE
069200041006     C                   CLEAR                   TRUL33DS
069300041006     C                   EVAL      I33TLA = 'L'
069400041006     C                   EVAL      I33OPE = *zeros
069500041006     C                   EVAL      I33CNU = 024
069600041006     C                   EVAL      I33NUM = 1
069700041006     C                   MOVEL     TRUL33DS      KPJBU
069800041025     C                   CALL      'TRUL33C'
069900041006     C                   PARM                    KPJBA
070000041006     C                   MOVEL     KPJBU         TRUL33DS
070100041006     C                   IF        O33ERR <> *zeros
070200041006     C                   MOVEL     '2'           Esito
070300041006     C                   EVAL      Messaggio = 'TRTCM1R1 - Errore in ' +
070400041025     C                                         'reperimento contatore (TRUL33C)'
070500041006     C                   ENDIF
070600041006     C*
070700041006     C                   ENDSR
070800041006     C****************************************************************
070900040217
071000040217
071100040217     C****************************************************************
071200040217     C* REPERISCE I DATI UTENTE
071300040217     C****************************************************************
071400040217     C     REPDATIUTE    BEGSR
071500040217     C*
071600040217     C* INIZIALIZZA VARIABILI DI WRK
071700040217     C                   CLEAR                   TIBS34DS
071800040217     C                   CLEAR                   AZUTEDS
071900040217     C                   CLEAR                   DDATIUTE
072000040217     C*
072100040217     C     *DTAARA       DEFINE    §azute        azuteds
072200040217     C     *DTAARA       DEFINE    §datiute      ddatiute
072300040217     C                   IN(E)     *DTAARA
072400040217     C                   IF        %Error
072500040217     C                   EVAL      I34Tla = 'L'
072600040217     C                   CALL      'TIBS34R'
072700040217     C                   PARM                    Tibs34Ds
072800040217     C                   IN        *DTAARA
072900040217     C                   ENDIF
073000040217     C*
073100040217     C                   ENDSR
073200040217     C***************************************************************
073300041013
073400041013
073500041013
073600041013     C****************************************************************
073700041013     C* CARICA DATI TABELLATI
073800041013     C****************************************************************
073900041013     C     CARTAB        BEGSR
074000041013     C*
074100041013     C* Reperimento tabella MRA
074200041013     C                   CLEAR                   DMRA
074300041013     C                   MOVE(P)   'MRA'         tbeCOD
074400041013     C                   MOVEL(p)  'TRUL83R'     tbeKE1
074500041013     C                   MOVE      *blanks       tbeKE2
074600041013     C     KEYTBE01P     CHAIN     TNTBE01L
074700041013     C                   IF        %found(TNTBE01L) AND
074800041013     C                             tbeATB <> 'A'
074900041013     C                   SETOFF                                       60
075000041013     C                   MOVEL     tbeUNI        DMRA
075100041013     C                   ELSE
075200041013     C                   SETON                                        60
075300051025     C                   EVAL      Esito = '2'
075400051025     C                   EVAL      Messaggio = 'Record TRUL83R non trovato in '+
075500051025     C                                      'tabella MRA, avvisare il CED.'
075600041013     C                   ENDIF
075700041013     C*
075800041013     C                   ENDSR
075900041013     C***************************************************************
076000051025
076100051025
076200051025     C****************************************************************
076300051025     C* REPERISCE ATTRIBUTI SPOOL X RICONDURRE SCHIERA LISTA SPOOL/JOB DA FORMATO "INTERNAL ID" A "CLASSICO"
076400051025     C****************************************************************
076500051025     C     RTVSKSPOOL    BEGSR
076600051025     C*
076700051025     C                   SETOFF                                       70
076800051025     C*
076900051025     C* Reperisco il numero degli spool ricevuti in input
077000051104     C                   Z-ADD     1             x                 5 0
077100051104     C                   DOW       x <= %elem(SKSPOOLID)
077200051025     C                   IF        SKSPOOLID(x) <> *blanks
077300051104     C                   EVAL      DSSPOOLID = SKSPOOLID(x)
077400051025     C                   EVAL      XLSTSPLDS2 = Sp_Job_ID
077500051025     C*
077600051025     C* Reperisco gli attributi dello spool corrente
077700051025     C                   CLEAR                   XSPLATRDS
077800051025     C                   CLEAR                   XSPLATRDS1
077900051025     C                   EVAL      §SPLA_JID  = §LST2_JOB
078000051025     C                   EVAL      §SPLA_SID  = §LST2_SPL
078100051025     C                   CALL(e)   'XSPLATR'
078200051025     C                   PARM                    XSPLATRDS
078300051025     C*
078400051025     C* Gestisco eventuale errore
078500051025     C                   IF        %error OR §SPLA_ERR = 'E'
078600051025     C                   SETON                                        70
078700051025     C                   EVAL      Esito = '2'
078800051025     C                   EVAL      Messaggio = 'Errori in reperimento ' +
078900051025     C                                         'attributi spool!'
079000051025     C                   LEAVE
079100051025     C                   ELSE
079200051025     C*
079300051025     C* Se tutto ok => valorizzo la schiera lista spool in formato "classico"
079400051025     C                   EVAL      XSPLATRDS1 = §SPLA_ATTR
079500051025     C                   EVAL      File       = §SPLA1_FIL
079600051025     C                   EVAL      Job        = §SPLA1_JNM
079700051025     C                   EVAL      User       = §SPLA1_USN
079800051025     C                   EVAL      JobX       = §SPLA1_JNR
079900051102     C                   MOVE(P)   §SPLA1_FNR    SplfXx
080000051025     C                   EVAL      UsrDta     = §SPLA1_DTA
080100051025     C                   EVAL      SplRig     = §SPLA1_LEN
080200051025     C                   EVAL      SplCol     = §SPLA1_WID
080300051025     C                   EVAL      SplCPI     = §SPLA1_CPI
080400051025     C                   EVAL      OutQue     = §SPLA1_QNM
080500051102     C                   EVAL      DataOra = %subst(§SPLA1_FOP:6:2) + '/' +
080600051102     C                                       %subst(§SPLA1_FOP:4:2) + '/' +
080700051102     C                                       %subst(§SPLA1_FOP:2:2) + ' '+
080800051102     C                                       %subst(§SPLA1_TIM:1:2) + ':'+
080900051102     C                                       %subst(§SPLA1_TIM:3:2) + ':'+
081000051102     C                                       %subst(§SPLA1_TIM:5:2)
081100051025     C                   EVAL      TotPagine  = §SPLA1_TPG
081200051025     C                   EVAL      StatoSpool = §SPLA1_STS
081300051025     C                   EVAL      SKSPOOL(x) = DSSPOOL
081400051025     C                   ENDIF
081500051104     C                   ELSE
081600051104     C                   LEAVE
081700051102     C                   ENDIF
081800051104     C                   ADD       1             x
081900051025     C                   ENDDO
082000051025     C*
082100051025     C                   ENDSR
082200051025     C***************************************************************
082300040217
082400040217
082500040217     C***************************************************************
082600040130     C     *INZSR        BEGSR
082700040217     C***************************************************************
082800040217     C*
082900041006     C     *ENTRY        PLIST
083000041006     C                   PARM                    SKSPOOL
083100051102     C                   PARM                    SKSPOOLIDDS
083200051102     C                   PARM                    FlgSnd            1
083300040607     C                   PARM                    EmailMitt        30
083400051025     C                   PARM                    EmailDest       110
083500051025     C                   PARM                    EmailObj         55
083600051025     C                   PARM                    EmailTxt1        75
083700051025     C                   PARM                    EmailTxt2        75
083800051025     C                   PARM                    EmailTxt3        75
083900051025     C                   PARM                    EmailTxt4        75
084000051025     C                   PARM                    EmailTxt5        75
084100051025     C                   PARM                    EmailTxt6        75
084200051025     C                   PARM                    EmailTxt7        75
084300051025     C                   PARM                    EmailTxt8        75
084400051025     C                   PARM                    FlgTappoLast      1
084500051025     C                   PARM                    FlgBypChSp        1
084600051025     C                   PARM                    FlgDspVid         1
084700051025     C                   PARM                    FlgDltSplIn       1
084800051025     C                   PARM                    Esito             1
084900051025     C                   PARM                    Messaggio        50
085000051025     C*
085100051025     C* Controllo se passati in input schiera spool/job in formato "internal ID"
085200051025     C* se sì => riconduco a DS standard tramite reperimento attributi spool
085300051102     C                   IF        SKSPOOLIDDS <> *blanks
085400051025     C                   CLEAR                   SKSPOOL
085500051025     C                   EXSR      RTVSKSPOOL
085600051025     C                   ENDIF
085700041006     C*
085800041006     C* Reperisco il numero degli spool ricevuti in input
085900051104     C                   Z-ADD     *zeros        wNumSpool
086000051104     C                   Z-ADD     1             x                 5 0
086100051104     C                   DOW       x <= %elem(SKSPOOL)
086200041006     C                   IF        SKSPOOL(x) <> *blanks
086300041006     C                   ADD       1             wNumSpool
086400051104     C                   ELSE
086500051104     C                   LEAVE
086600041006     C                   ENDIF
086700051104     C                   ADD       1             x
086800041006     C                   ENDDO
086900041006     C*
087000051025     C* Se trattasi scelta multipla disabilito il tasto funzine F5 x la visualizzazione dello spool richiesto
087100051025     C                   IF        wNumSpool > 1 or FlgDspVid = 'N'
087200041006     C                   SETON                                        51
087300041006     C                   ENDIF
087400040524     C*
087500040524     C* Reperisco il P.O. dell'utente corrente
087600040524     C                   EXSR      REPDATIUTE
087700040218     C*
087800040218     C* CHIAVI DI LETTURA
087900040224     C     KEYMAS01C     KLIST                                                  *TIMAS01L
088000040218     C                   KFLD                    KmasSPOOL                       -FILE SPOOL
088100040224     C                   KFLD                    KmasUSDTA                       -DATI UTENTE
088200040218     C                   KFLD                    KmasUTE                         -MASCHERA UTENTE
088300040224     C     KEYMAS01P     KLIST                                                  *TIMAS01L
088400040224     C                   KFLD                    KmasSPOOL                       -FILE SPOOL
088500040224     C                   KFLD                    KmasUSDTA                       -DATI UTENTE
088600041013     C     KEYTBE01P     KLIST
088700041013     C                   KFLD                    tbeCOD
088800041013     C                   KFLD                    tbeKE1
088900041013     C                   KFLD                    tbeKE2
089000040217     C*
089100040130     C                   ENDSR
089200041006     O*------------------------------------------------------------------------*
089300041006     O* STAMPA
089400041006     O*------------------------------------------------------------------------*
089500041006     OPRTEMAIL  E            DET         1
089600041006     O                       TESTO1              +1
089700041006     O          E            DET         1
089800041006     O                       TESTO2              +1
089900041006     O          E            DET         1
090000041006     O                       TESTO3              +1
090100041006     O          E            DET         1
090200041006     O                       TESTO4              +1
090300041006     O          E            DET         1
090400041006     O                       TESTO5              +1
090500041006     O          E            DET         1
090600041006     O                       TESTO6              +1
090700041006     O          E            DET         1
090800041006     O                       TESTO7              +1
090900041006     O          E            DET         1
091000041006     O                       TESTO8              +1
