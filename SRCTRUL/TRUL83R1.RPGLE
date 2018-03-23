000100180131     /*PRM usrprf(*owner)
000200180131     /*END
000300040928      **********************************************************
000400051102      *           Compilare da: QSECOFR                        *
000500051102      *           Compilare con USRPRF: *OWNER                 *
000600040928      **********************************************************
000700040607     FTRUL83D1  CF   E             WORKSTN
000800040217     FTIMAS01L  IF   E           K DISK
000900041013     FTNTBE01L  IF   E           K DISK
001000041008     FPRTEMAIL  O    F  132        PRINTER OFLIND(*INOF) USROPN
001100040928      **********************************************************
001200040130     D*
001300051025     D*------------------
001400041006     D* DS X RIDEFINIZIONE PASSAGGIO SCHIERA SPOOL SELEZIONATI A VIDEO
001500051025     D*------------------
001600041006     D DSSPOOL         DS
001700041006     D  File                         10
001800041006     D  Job                          10
001900041006     D  User                         10
002000041006     D  JobX                          6
002100041006     D  SplfXx                        6
002200041006     D  UsrDta                       10
002300041006     D  SplRig                        9  0
002400041006     D  SplCol                        9  0
002500041006     D  SplCPI                        9  1
002600041006     D  OutQue                       10
002700041006     D  DataOra                      17
002800041006     D  TotPagine                     6  0
002900041006     D  StatoSpool                    6
003000051025     D*------------------
003100051025     D* DS REPERIMENTO LISTA SPOOL - INTERNAL SPOOL & JOB ID
003200051025     D*------------------
003300051025     D XLSTSPLDS2    E DS
003400051025     D*------------------
003500051025     D* DS REPERIMENTO ATTRIBUTI SPOOL
003600051025     D*------------------
003700051025     D XSPLATRDS     E DS
003800051025     D XSPLATRDS1    E DS
003900051025     D*------------------
004000051025     D DSSPOOLID       DS
004100051025     D  Sp_Job_ID                          like(XLSTSPLDS2)
004200051025     D  Sp_Job_Codr                   3
004300051025     D*------------------
004400041006     D* SCHIERE SPOOL SELEZIONATI A VIDEO
004500051025     D*------------------
004600051025     D SKSPOOL         S                   like(DSSPOOL)   dim(99)
004700051102     D*
004800051102     D SKSPOOLIDDS     DS
004900051102     D  SKSPOOLID                          like(DSSPOOLID) dim(99)
005000041006     D*------------------
005100041006     D* DS X REPERIMENTO NUMERATORE
005200041006     D*------------------
005300041006     D TRUL33DS      E DS                  INZ
005400041006     D*------------------
005500041006     D* ARCHITETTURA
005600041006     D*------------------
005700041006     D KPJBA         E DS
005800041013     D*------------------
005900041013     D* TABELLEA 'MRA'
006000041013     D*------------------
006100041013     D DMRA          E DS
006200040130
006300040130     D* Parametri x modifiche attributi spool
006400040130     D PARAM           S            262    DIM(20)
006500040130
006600040130     D* DS scomposizione parametri x modifiche attributi spool
006700040130     D SPOOLATR        DS
006800040130     D  POSDA                  1      4  0
006900040130     D  LUNG                   5      7  0
007000040130     D  VALORE                 8    262
007100040217
007200040217     D* DS ridefinizione dati utente estesi spool x mailing
007300040217     D TRTCM1DS      E DS
007400040217
007500040217     D* DS REPERIMENTO DATI UTENTE
007600040217     D TIBS34DS      E DS                                                       *Profili utente
007700040217     D DDATIUTE      E DS                                                       *Dati utente
007800040217     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007900040521
008000040521     D* DS x richiamo *pgm utilità d controllo indirizzi e-mail
008100040521     D DSEMAIL       E DS
008200040130
008300040218     D* Variabili riferite al data-base
008400040218     D KmasSPOOL       S                   like(masSPOOL)
008500040224     D KmasUSDTA       S                   like(masUSDTA)
008600040218     D KmasUTE         S                   like(masUTE)
008700040218
008800040218     D* Informazioni sul job
008900040218     D psds           sds
009000180201     D  pprogr           *proc
009100040218     D  psuser               254    263
009200041006
009300041006     D* Variabili d wrk
009400041006     D FlgAbil         S              1    inz('S')
009500041006     D wNumSpool       S              2  0
009600041006     D wNumSpoolCor    S              2  0
009700041006     D wNumSpoolTot    S              2  0
009800040224
009900040217
010000040217     C* /MAIN
010100051025     C*
010200051025     C* Se tutto ok in operazioni iniziali procedo
010300051025     C                   IF        *IN70 = *OFF
010400041013     C*
010500041013     C* Reperisco dati tabellati (Tabella 'MRA')
010600041013     C                   EXSR      CARTAB
010700041006     C*
010800041006     C* Verifico subito le abilitazioni e le regole x lo spool indicato dall'utente
010900051017     C* (effettuo questo controllo solo x le chiamate x singolo spool)
011000041006     C                   IF        wNumSpool = 1
011100041006     C                   EVAL      DSSPOOL = SKSPOOL(1)
011200041006     C                   EXSR      CHKSPOOL
011300041006     C                   ENDIF
011400040218     C*
011500040218     C* Se lo spool è specificatamente negato x l'utente corrente => NN elaboro e restituisco messaggio
011600041006     C                   IF        FlgAbil = 'N'
011700040218     C                   EVAL      Esito = '2'
011800040218     C                   EVAL      Messaggio = 'Non si è autorizzati ad ' +
011900040218     C                                         'inviare lo spool indicato.'
012000040218     C                   ELSE
012100041006     C*
012200041006     C* Nel nei parametri d ingresso è stato richiesto anche l'invio (oltre al controllo abilitazione allo spool)
012300041006     C* procedo all'elaborazione ad all'invio della mail
012400041006     C                   IF        FlgSnd = 'S'
012500040217     C*
012600040217     C* Ciclo finchè nn scelta opzione valida
012700040217     C                   DOW       *INKL = *OFF
012800041006     C*
012900041006     C* Effettuo inizializzazione videata
013000041006     C                   EXSR      INZVID
013100040217     C*
013200040217     C* Emetto la window x ricevere dall'utente gli indirizzi e-mail mittente e destinatari
013300051025     C                   IF        FlgDspVid <> 'N'
013400040520     C                   EXFMT     UL831DW01
013500040224     C*
013600040224     C* Effettuo controllo dell'input
013700040224     C                   EXSR      CHKINPUT
013800051025     C                   ELSE
013900051025     C* Se chiamata con parametri forzati in input do x assunto che sia tutto ok => quindi bypasso i controlli
014000051025     C                   EVAL      FlgOK = 'S'
014100051025     C                   ENDIF
014200040217     C*
014300040217     C* Se F06 elaboro
014400040217     C     *INKF         IFEQ      *ON
014500051024     C     FlgDspVid     OREQ      'N'
014600051024     C*
014700040224     C* Se tutto ok => procedo
014800040224     C                   IF        FlgOK = 'S'
014900041006     C*
015000041006     C* Se si è abilitati come prima cosa stacco 1 numeratore
015100041006     C                   EXSR      REPNUM
015200041006     C*
015300041006     C* Se no errori in reperimento numeratore proseguo con l'elaborazione, altrimenti esco e restituisco messaggio
015400041006     C                   IF        Esito <> '2'
015500041006     C*
015600041006     C* Ciclo x tutti gli spool scelti
015700041006     C                   EVAL      wNumSpoolCor = 1
015800051104     C*
015900051104     C* Se richiesta gestione del "tappo" forzato in ingresso NN modifico il numero spool totali
016000051104     C                   IF        FlgTappoLast <> 'S'
016100041006     C                   EVAL      wNumSpoolTot = wNumSpool + 1
016200051104     C                   ELSE
016300051104     C                   EVAL      wNumSpoolTot = wNumSpool
016400051104     C                   ENDIF
016500041006     C                   DOW       wNumSpoolCor <= wNumSpool
016600041006     C                   EVAL      DSSPOOL = SKSPOOL(wNumSpoolCor)
016700041006     C                   EXSR      CHKSPOOL
016800040224     C                   EXSR      PROCEDI
016900041006     C                   ADD       1             wNumSpoolCor
017000051102     C                   ENDDO
017100051102     C*
017200051102     C* Se nn espressamente richiesta gestione "nn standard" =>
017300051102     C                   IF        FlgTappoLast <> 'S'
017400041006     C*
017500041006     C* Sottometto quindi l'elaborazione dello spool d chiusura contenete il testo della mail
017600041006     C* ...quindi lascio la DS d procedura (TRTCM1DS) intatta come i precedenti lanci e
017700041006     C* ...sostituisco solo la regola appropriata
017800041006     C*
017900041006     C* Imposto il numero d spool corrente = al numero d spool totali che fanno parte dello stesso invio
018000041006     C                   MOVE(P)   wNumSpoolTot  §CM1ATT
018100041006     C                   MOVE(P)   wNumSpoolTot  §CM1TOTATT
018200041006     C*
018300041013     C* Imposto la regola dello spool e gli altri dati fondamentali (processo, stato, etc...)
018400041013     C                   EVAL      §CM1TIPS = §MRAREG
018500041013     C                   EVAL      §CM1IDP  = §MRAIDPRO
018600041013     C   60              EVAL      §CM1STS  = 'H'
018700041006     C*
018800041006     C* Eseguo override x generare uno spool ad "hoc"
018900041006     C                   CALL      'TRUL83C1'
019000041006     C                   PARM      'O'           OPZIONE           1
019100060810     C                   PARM      'PRTEMAIL'    PRTFILE          10
019200041006     C                   PARM                    TRTCM1DS
019300041013     C                   PARM                    §MRAOUTQI
019400041013     C                   PARM                    Esito
019500041006     C* Se nn c sono errori d override proseguo con l'elaborazione, altrimenti mando messaggio all'utente
019600041006     C                   IF        Esito = '2'
019700041006     C                   EVAL      Messaggio = 'Errori nel''invio della mail'
019800041006     C                   ELSE
019900041006     C                   OPEN      PRTEMAIL
020000041006     C                   EXCEPT    DET
020100041006     C                   CLOSE     PRTEMAIL
020200041006     C* Eseguo delete override precedente
020300041006     C                   CALL      'TRUL83C1'
020400060810     C                   PARM      'D'           OPZIONE
020500060810     C                   PARM                    PRTFILE
020600041006     C                   PARM                    TRTCM1DS
020700041013     C                   PARM                    §MRAOUTQI
020800041006     C                   PARM                    Esito
020900041006     C                   IF        Esito = '2'
021000041006     C                   EVAL      Messaggio = 'Errori nel''invio della mail'
021100041006     C                   ENDIF
021200041006     C                   ENDIF
021300051102     C*
021400051102     C                   ENDIF
021500051104     C*
021600051104     C* Dopo l'invio della mail forzo l'uscita dal programma
021700051104     C                   EVAL      *INKL = *ON
021800051102     C*
021900040224     C                   ENDIF
022000040217     C                   ENDIF
022100041006     C                   ENDIF
022200040224     C*
022300040520     C* Se F05 visualizzo lo spool
022400041006     C  N51*INKE         IFEQ      *ON
022500041006     C                   CALL      'TRUL83C'
022600040224     C                   PARM      '5'           Opzione           1
022700040224     C                   PARM                    File
022800040224     C                   PARM                    Job
022900040224     C                   PARM                    User
023000040224     C                   PARM                    JobX
023100040224     C                   PARM                    SplfXx
023200040224     C                   PARM                    Esito
023300040224     C                   IF        Esito = '2'
023400040224     C                   EVAL      Messaggio = 'Errori nella visualizzazione ' +
023500040224     C                                         'dello spool'
023600040224     C                   ENDIF
023700040224     C                   ENDIF
023800040217     C*
023900040217     C* Se F12 vado a fine
024000040217     C     *INKL         IFEQ      *ON
024100040218     C                   EVAL      *INKL = *ON
024200040217     C                   ENDIF
024300040217     C*
024400040217     C                   ENDDO
024500041006     C*
024600041006     C                   ENDIF
024700040218     C                   ENDIF
024800051025     C                   ENDIF
024900040217     C*
025000051025     C* Al termine delle operazioni se richiesto nei parametri in input e se TUTTO è andato bene
025100051025     C* elimino gli spool originali
025200051025     C                   IF        FlgDltSplIn = 'S' AND
025300051025     C                             Esito <> '2'
025400051104     C                   Z-ADD     1             x                 5 0
025500051104     C                   DOW       x <= %elem(SKSPOOL)
025600051025     C                   IF        SKSPOOL(x) <> *blanks
025700051104     C                   EVAL      DSSPOOL = SKSPOOL(x)
025800051025     C                   CALL      'TRUL83C'
025900051025     C                   PARM      '4'           Opzione           1
026000051025     C                   PARM                    File
026100051025     C                   PARM                    Job
026200051025     C                   PARM                    User
026300051025     C                   PARM                    JobX
026400051025     C                   PARM                    SplfXx
026500051025     C                   PARM                    Esito
026600051104     C                   ELSE
026700051104     C                   LEAVE
026800051025     C                   ENDIF
026900051104     C                   ADD       1             x
027000051025     C                   ENDDO
027100051025     C                   ENDIF
027200051025     C*
027300040217     C                   SETON                                        LR
027400040217     C****************************************************************
027500040524
027600040524
027700040524
027800040524     C****************************************************************
027900040524     C* INIZIALIZZA LA VIDEATA CON I VALORI D DEFAULT
028000040524     C****************************************************************
028100040524     C     INZVID        BEGSR
028200180201      *
028300180201      * Imposto il nome del programma a video
028400180201     c                   eval      Progr = pProgr
028500180201      *
028600180201      * Imposto il codice regola reperito
028700180201     c*//                eval      *in52 = (pUser = 'EDPSM ')
028800180201     c                   eval      *in52 = *on
028900180201     c                   eval      Regola = MAScodR
029000041006     C*
029100041006     C* Gestisco subito indicatori "particolari"
029200041006     C                   SETOFF                                       50
029300040524     C*
029400040524     C* Inizializzo la videata
029500040524     C                   IF        MITT = *blanks
029600040524     C                   SETOFF                                       30
029700040524     C* Se specificato indirizzo e-mail mittente in chiamata imposto quello
029800040607     C                   IF        EmailMitt <> *blanks
029900040607     C                   EVAL      MITT = EmailMitt
030000040524     C                   SETON                                        20
030100040524     C                   ELSE
030200040524     C* Altrimenti verifico se valorizzato campo UTEEMA (email utente) dell'utente corrente su AZUTE00F
030300040524     C                   IF        UTEEMA <> *blanks
030400040524     C* Se presenti + indirizzi e-mail abbinati all'utente come default considero il primo
030500040524     C                   IF        %scan(';':UTEEMA) > *zeros
030600040524     C                   EVAL      MITT = %subst(%trim(UTEEMA):1:
030700040524     C                                    %scan(';':UTEEMA)-1)
030800040524     C                   ELSE
030900040524     C                   EVAL      MITT = %trim(UTEEMA)
031000040524     C                   ENDIF
031100040524     C                   ELSE
031200040524     C* Se nn presente indirizzo e-mail abbinato all'utente corrente su AZUTE00F forzo sempre l'"info" del P.O.
031300040524     C* utente corrente
031400041021     C                   EVAL      MITT = %trim(§MRAMIT1L) +
031500041021     C                                    %trim(%editw(DUTPOU:'0   '))
031600040524     C                   ENDIF
031700040524     C                   ENDIF
031800040524     C                   ENDIF
031900040524     C*
032000041006     C* Se richiesto singolo spool imposto i dati a video altrimenti forzo dicitura
032100041006     C                   IF        *IN51 = *OFF
032200051017     C                   EVAL      SPOOL = 'Spool   : ' +
032300051017     C                                     %trim(File)+'/'+%trim(User)+'/'+
032400040524     C                                     %trim(OutQue)+'/'+%trim(StatoSpool)+
032500040524     C                                     '/'+%trim(UsrDta)
032600040524     C                   EVAL      DATORA = DataOra
032700040524     C                   EVAL      NPAG = TotPagine
032800041006     C                   ELSE
032900041006     C                   EVAL      SPOOL = 'Selezione multipla'
033000041006     C                   ENDIF
033100051025     C*
033200051025     C*************
033300051025     C* FORZATURE DA PARAMETRI IN INPUT
033400051025     C*************
033500051025     C*
033600051025     C* Imposto e-mail mittente se passato
033700051025     C                   if        EmailMitt <> *blanks
033800051025     C                   eval      MITT = EmailMitt
033900051025     C                   endif
034000051024     C*
034100051024     C* Imposto e-mail destinatario se passato
034200051025     C                   if        EmailDest <> *blanks
034300051025     C                   eval      DEST1 = %subst(EmailDest:1:55)
034400051025     C                   eval      DEST2 = %subst(EmailDest:56:55)
034500051024     C                   endif
034600051024     C*
034700051024     C* Imposto oggetto se passato
034800051024     C                   if        EmailObj <> *blanks
034900051025     C                   eval      OGGETTO = EmailObj
035000051024     C                   endif
035100051024     C*
035200051024     C* Imposto testo se passato
035300051024     C                   if        EmailTxt1 + EmailTxt2 + EmailTxt3 +
035400051024     C                             EmailTxt4 + EmailTxt5 + EmailTxt6 +
035500051024     C                             EmailTxt7 + EmailTxt8 <> *blanks
035600051025     C                   eval      TESTO1 = EmailTxt1
035700051025     C                   eval      TESTO2 = EmailTxt2
035800051025     C                   eval      TESTO3 = EmailTxt3
035900051025     C                   eval      TESTO4 = EmailTxt4
036000051025     C                   eval      TESTO5 = EmailTxt5
036100051025     C                   eval      TESTO6 = EmailTxt6
036200051025     C                   eval      TESTO7 = EmailTxt7
036300051025     C                   eval      TESTO8 = EmailTxt8
036400051024     C                   endif
036500040524     C*
036600040524     C                   ENDSR
036700040524     C***************************************************************
036800040218
036900040218
037000040218     C****************************************************************
037100040218     C     CHKSPOOL      BEGSR
037200040218     C****************************************************************
037300040218     C*
037400040218     C* Reperimento "codice regola mailing" x spool corrente
037500180131     C*//                MOVEL     'S'           FlgAbil
037600180201     c                   clear                   FlgAbil
037700040224     C*
037800040224     C* Effettuo 1° verifica x nome file spool + dati utente + utente
037900040224     C* x verificare eventuali abilitazioni/negazioni specifiche
038000040224     C                   EVAL      KmasSPOOL = File
038100040224     C                   EVAL      KmasUSDTA = UsrDta
038200040224     C                   EVAL      KmasUTE   = psuser
038300040224     C     KEYMAS01C     CHAIN     TIMAS01L
038400040224     C                   IF        %found(TIMAS01L)
038500040224     C                   MOVEL     MASABIL       FlgAbil
038600180131     C*//                ELSE
038700180131     C                   ENDIF
038800040224     C*
038900040224     C* Effettuo 2° verifica x nome file spool + dati utente
039000040224     C* x verificare eventuali abilitazioni/negazioni d gruppo sull'utente
039100180201if  1c                   If        FlgAbil = *blanks
039200040224     C     KEYMAS01P     SETLL     TIMAS01L
039300040218     C*
039400040224     C* Se trovato verifico se l'utente o il gruppo è presente e le relative abilitazioni/negazioni
039500040218     C                   IF        %equal(TIMAS01L)
039600040224     C     KEYMAS01P     READE     TIMAS01L
039700040218     C                   DOW       not %eof(TIMAS01L)
039800040218     C                   IF        %subst(psuser:1:(%len(%trim(masUTE)))) =
039900041013     C                             %trim(masUTE) OR masUTE = '*ALL'
040000040224     C                   MOVEL     MASABIL       FlgAbil
040100040218     C                   LEAVE
040200040218     C                   ENDIF
040300040224     C     KEYMAS01P     READE     TIMAS01L
040400040218     C                   ENDDO
040500180131     C*//                ELSE
040600180201     c                   ENDIF
040700180201e   1c                   EndIf
040800040224     C*
040900040224     C* Effettuo 3° verifica x nome file spool
041000040224     C* x verificare eventuali abilitazioni/negazioni d gruppo su dati utente e utente
041100180201if  1c                   If        FlgAbil = *blanks
041200040224     C     File          SETLL     TIMAS01L
041300040224     C*
041400040224     C* Se trovato verifico se l'utente o il gruppo è presente e le relative abilitazioni/negazioni
041500040224     C                   IF        %equal(TIMAS01L)
041600040224     C     File          READE     TIMAS01L
041700040224     C                   DOW       not %eof(TIMAS01L)
041800040224     C                   IF        masUSDTA = UsrDta  OR
041900041013     C                             masUSDTA = '*ALL'
042000040224     C                   IF        %subst(psuser:1:(%len(%trim(masUTE)))) =
042100041013     C                             %trim(masUTE) OR masUTE = '*ALL'
042200040224     C                   MOVEL     MASABIL       FlgAbil
042300040224     C                   LEAVE
042400040224     C                   ENDIF
042500040224     C                   ENDIF
042600040224     C     File          READE     TIMAS01L
042700040224     C                   ENDDO
042800180131     C*//                ELSE
042900180201     c                   ENDIF
043000180131e   1c                   EndIf
043100040607     C*
043200040607     C* Effettuo 4° verifica x utente e nome spool "*ALL"
043300040607     C* x verificare eventuali abilitazioni/negazioni d gruppo su dati utente e utente
043400180201if  1c                   If        FlgAbil = *blanks
043500040607     C                   EVAL      KmasSPOOL = '*ALL'
043600040607     C                   EVAL      KmasUSDTA = '*ALL'
043700041013     C     KEYMAS01P     SETLL     TIMAS01L
043800041013     C*
043900041013     C* Se trovato verifico se l'utente o il gruppo è presente e le relative abilitazioni/negazioni
044000041013     C                   IF        %equal(TIMAS01L)
044100041013     C     KEYMAS01P     READE     TIMAS01L
044200041013     C                   DOW       not %eof(TIMAS01L)
044300041013     C                   IF        %subst(psuser:1:(%len(%trim(masUTE)))) =
044400041013     C                             %trim(masUTE) OR masUTE = '*ALL'
044500041013     C                   MOVEL     MASABIL       FlgAbil
044600041013     C                   LEAVE
044700041013     C                   ENDIF
044800041013     C     KEYMAS01P     READE     TIMAS01L
044900041013     C                   ENDDO
045000041013     C                   ENDIF
045100180131e   1c                   EndIf
045200180131      *
045300041013     C*                  EVAL      KmasUTE   = psuser
045400041013     C*    KEYMAS01C     CHAIN     TIMAS01L
045500041013     C*                  IF        %found(TIMAS01L)
045600041013     C*                  MOVEL     MASABIL       FlgAbil
045700041013     C*                  ENDIF
045800180131     C*//                ENDIF
045900180131     C*//                ENDIF
046000180131     C*//                ENDIF
046100041006     C*
046200041006     C                   IF        MASCODR = *blanks
046300041006     C***                SETON                                        50
046400041006     C                   IF        FORMATO = *blanks
046500041006     C                   EVAL      FORMATO = 'T'
046600041006     C                   ENDIF
046700041006     C                   ENDIF
046800180201      *
046900180201      * - SE non è stata trovata 1 (UNA) abilitazione - neanche quella
047000180201      *   generica (*ALL/*ALL/*ALL), CHE E' IMPOSSIBILE !!! - forzo "N"
047100180201     c                   if        FlgAbil = *blanks
047200180201     c                   eval      FlgAbil = 'N'
047300180201     c                   endif
047400051025     C*
047500051025     C* Se richiesto di bypassare il controllo del file di spool imposto
047600051025     C* il flag di abilitazione a "S"
047700051025     C                   if        FlgBypChSp = 'S'
047800051017     C                   MOVEL     'S'           FlgAbil
047900051025     C                   endif
048000040218     C*
048100040218     C                   ENDSR
048200040218     C****************************************************************
048300040217
048400040217
048500040217     C****************************************************************
048600040217     C     CHKINPUT      BEGSR
048700040217     C****************************************************************
048800040224     C*
048900040224     C                   CLEAR                   MSGERR
049000040217     C*
049100040217     C* Inizializzo il flag d controllo input
049200040217     C                   MOVEL     'S'           FlgOK             1
049300041013     C*
049400041013     C* Spengo gli indicatori d errore a video
049500041013     C                   SETOFF                                       303132
049600041013     C                   SETOFF                                       33
049700041013     C*
049800041013     C* Effettuo controllo su reperimento con successo record 'TRUL83R' in tabella 'MRA'
049900041013     C                   IF        *IN60 = *on
050000041013     C                   SETON                                        303132
050100041013     C                   SETON                                        33
050200041013     C                   MOVEL     'N'           FlgOK
050300041013     C                   EVAL      MSGERR = 'Record TRUL83R non trovato in ' +
050400041013     C                                      'tabella MRA, avvisare il CED.'
050500041013     C                   ENDIF
050600040217     C*
050700040217     C* Effettuo controlli obbligatorietà parametri a video
050800040224     C                   IF        FlgOK = 'S'
050900040217     C                   IF        MITT = *blanks
051000040217     C                   SETON                                        30
051100040217     C                   MOVEL     'N'           FlgOK
051200040521     C                   EVAL      MSGERR = 'Campo obbligatorio'
051300040217     C                   ENDIF
051400041005     C                   ENDIF
051500041005     C                   IF        FlgOK = 'S'
051600040224     C                   IF        DEST1 + DEST2 = *blanks
051700040217     C                   SETON                                        31
051800040217     C                   MOVEL     'N'           FlgOK
051900040521     C                   EVAL      MSGERR = 'Campo obbligatorio'
052000040217     C                   ENDIF
052100040224     C                   ENDIF
052200041005     C                   IF        FlgOK = 'S'
052300041005     C                   IF        OGGETTO = *blanks
052400041005     C                   SETON                                        32
052500041005     C                   MOVEL     'N'           FlgOK
052600041005     C                   EVAL      MSGERR = 'Campo obbligatorio'
052700041005     C                   ENDIF
052800041005     C                   ENDIF
052900041005     C                   IF        FlgOK = 'S'
053000041005     C                   IF        TESTO1+TESTO2+TESTO3+TESTO4+TESTO5+TESTO6+
053100041005     C                             TESTO7+TESTO8 = *blanks
053200041005     C                   SETON                                        33
053300041005     C                   MOVEL     'N'           FlgOK
053400041005     C                   EVAL      MSGERR = 'Campo obbligatorio'
053500041005     C                   ENDIF
053600041005     C                   ENDIF
053700040524     C*
053800040524     C* Effettuo controllo su abilitazione indirizzo e-mail mittente
053900040524     C                   IF        FlgOK = 'S'
054000041021     C                   IF        MITT = %trim(§MRAMIT1L) +
054100041021     C                                    %trim(%editw(DUTPOU:'0   '))
054200040607     C                             OR MITT = EmailMitt OR
054300040524     C                             %scan(%trim(MITT):UTEEMA) > *zeros
054400040524     C                   ELSE
054500040524     C                   SETON                                        30
054600040524     C                   MOVEL     'N'           FlgOK
054700040524     C                   EVAL      MSGERR = 'Indirizzo e-mail non consentito'
054800040524     C                   ENDIF
054900040524     C                   ENDIF
055000041005     C*
055100041005     C* Effettuo controlli su correttezza formale indirizzo e-mail mitente
055200041005     C                   IF        FlgOK = 'S'
055300041005     C                   CLEAR                   DSEMAIL
055400120314     C                   EVAL      §EMLINDI = %trim(MITT) + '@brt.it'
055500041005     C* Se c'è 1 indirizzo da controllare chiamo il *pgm d utilità controllo formale indirizzi e-mail
055600041005     C                   IF        §EMLINDI <> *blanks
055700041005     C                   CALL      'XEMAIL'
055800041005     C                   PARM                    DSEMAIL
055900041005     C                   IF        §EMLERRO = '1'
056000041005     C                   MOVEL     'N'           FlgOK
056100041005     C                   SETON                                        30
056200041005     C                   EVAL      MSGERR=§EMLMSGO
056300041005     C                   ENDIF
056400041005     C                   ENDIF
056500041005     C                   ENDIF
056600040224     C*
056700041005     C* Effettuo controlli su correttezza formale singoli indirizzi e-mail destinatari
056800040521     C                   IF        FlgOK = 'S'
056900040521     C                   CLEAR                   DSEMAIL
057000040929     C                   EVAL      §EMLINDI = %trim(DEST1 + DEST2)
057100040521     C* Se c'è 1 indirizzo da controllare chiamo il *pgm d utilità controllo formale indirizzi e-mail
057200040521     C                   IF        §EMLINDI <> *blanks
057300040521     C                   CALL      'XEMAIL'
057400040521     C                   PARM                    DSEMAIL
057500040521     C                   IF        §EMLERRO = '1'
057600040521     C                   MOVEL     'N'           FlgOK
057700040929     C                   SETON                                        31
057800040929     C                   EVAL      MSGERR=§EMLMSGO
057900040629     C                   ELSE
058000090714     C                   EVAL      DEST1 = %subst(§EMLINDO:1:55)
058100090714     C                   EVAL      DEST2 = %subst(§EMLINDO:56:100-55)
058200040521     C                   ENDIF
058300040521     C                   ENDIF
058400040521     C                   ENDIF
058500040217     C*
058600040217     C                   ENDSR
058700040217     C****************************************************************
058800040217
058900040217
059000040217     C****************************************************************
059100040217     C     PROCEDI       BEGSR
059200040217     C****************************************************************
059300040217     C*
059400040218     C                   CLEAR                   TRTCM1DS
059500041006     C*
059600041006     C* Come primissima cosa valorizzo l'ID Progressivo "univoco"
059700041006     C                   MOVE(P)   O33NRF        §CM1PROG
059800041013     C*
059900041013     C* Imposto la regola dello spool e gli altri dati fondamentali (processo, stato, etc...)
060000041013     C                   EVAL      §CM1TIPS = §MRAREG
060100041013     C                   EVAL      §CM1IDP  = §MRAIDPRO
060200041013     C   60              EVAL      §CM1STS  = 'H'
060300041006     C*
060400041006     C* Imposto il numero d spool corrente e il numero d spool totali che fanno parte dello stesso invio
060500041006     C                   MOVE(P)   wNumSpoolCor  §CM1ATT
060600041006     C                   MOVE(P)   wNumSpoolTot  §CM1TOTATT
060700041006     C*
060800041006     C* Imposto l'oggetto della mail
060900041006     C                   EVAL      §CM1VAR = '*OBJM*' + OGGETTO
061000041006     C*
061100041006     C* Imposto la regola dello spool
061200051025     C                   MOVEL     *blanks       wCODR             3
061300051102     C* => prima imposto la regola dello spool da file abilitazioni/forzature invii spool (no x "tappo" in input)
061400051104     C***                IF        FlgTappoLast <> 'S'
061500051104     C                   IF        wNumSpoolCor <> wNumSpoolTot
061600051025     C                   EVAL      wCODR = MASCODR
061700051102     C                   ENDIF
061800051025     C* => poi verifico se passati in input codici regola specifici x ogni spool
061900051025     C                   EVAL      DSSPOOLID = SKSPOOLID(wNumSpoolCor)
062000051025     C                   IF        Sp_Job_Codr <> *blanks
062100051025     C                   EVAL      wCODR = Sp_Job_Codr
062200051025     C                   ENDIF
062300051025     C* => altrimenti procedo con l'attribuzione d 1 regola generica desunta dagli attributi stessi sello spool corrente
062400051025     C                   IF        wCODR   <> *blanks
062500051025     C                   EVAL      §CM1TIPS = wCODR
062600040217     C* Se NN precisata regola x lo spool corrente procedo con l'attribuzione d 1 regola generica
062700040217     C                   ELSE
062800040928     C*
062900040928     C* Verifico quale scelta formato ha indicato l'utente a video
063000040928     C                   IF        FORMATO = 'L'
063100040928     C                   EVAL      §CM1TIPS = 'LET'
063200040928     C                   ENDIF
063300040928     C                   IF        FORMATO = 'T'
063400040217     C* Il numero d colonne MAX in decimi x restare in verticale è 82 x cui procedo a decimalizzare i CPI
063500040929     C                   EVAL      §CM1TIPS = 'TBL'
063600040929     C*                  IF        (SplCol / SplCPI * 10) < 82
063700040929     C*                  EVAL      §CM1TIPS = 'TBV'
063800040929     C*                  ELSE
063900040929     C*                  EVAL      §CM1TIPS = 'TBO'
064000040929     C*                  ENDIF
064100040217     C                   ENDIF
064200040928     C                   ENDIF
064300040524     C*
064400040524     C* Imposto come P.O. quello dell'utente che effettua la richiesta
064500040217     C                   MOVE      DUTPOU        §CM1PO
064600040217     C*
064700040218     C* Modifico gli attributi dello spool scelto x effettuare la copia x l'elaborazione
064800040218     C                   CLEAR                   PARAM
064900040520     C*
065000040520     C* Modifico a *NO il salvataggio del file d spool dopo la scrittura
065100040520     C                   CLEAR                   SPOOLATR
065200040520     C                   EVAL      POSDA = 138
065300040520     C                   EVAL      LUNG  = 10
065400040520     C                   EVAL      VALORE = '*NO'
065500040520     C                   EVAL      PARAM(1) = SPOOLATR
065600040520     C*
065700040520     C* Modifico l'utente proprietario dello spool "destinazione"
065800040520     C                   CLEAR                   SPOOLATR
065900040520     C                   EVAL      POSDA = 58
066000040520     C                   EVAL      LUNG  = 10
066100040520     C                   EVAL      VALORE = 'EDPMAIL'
066200040520     C                   EVAL      PARAM(2) = SPOOLATR
066300040520     C*
066400040520     C* Modifico la OUTQ dello spool "destinazione"
066500040520     C                   CLEAR                   SPOOLATR
066600040520     C                   EVAL      POSDA = 190
066700040520     C                   EVAL      LUNG  = 10
066800041013     C                   EVAL      VALORE = §MRAOUTQI
066900040520     C                   EVAL      PARAM(3) = SPOOLATR
067000040520     C*
067100040520     C* Modifico la libreria della OUTQ dello spool "destinazione"
067200040520     C                   CLEAR                   SPOOLATR
067300040520     C                   EVAL      POSDA = 200
067400040520     C                   EVAL      LUNG  = 10
067500040520     C                   EVAL      VALORE = 'GAITRAOBJ'
067600040520     C                   EVAL      PARAM(4) = SPOOLATR
067700040520     C*
067800040218     C* Modifico il parametro USRDFNDTA (dati utente estesi) con quanto indicato a video dall'utente
067900040218     C                   EVAL      §CM1MITT = MITT
068000040218     C                   EVAL      §CM1DST  = %trim(DEST1) +
068100040224     C                                        %trim(DEST2)
068200040130     C                   CLEAR                   SPOOLATR
068300040130     C                   EVAL      POSDA = 3460
068400040130     C                   EVAL      LUNG  = 255
068500040217     C                   EVAL      VALORE = TRTCM1DS
068600040218     C                   EVAL      PARAM(5) = SPOOLATR
068700040520     C*
068800040520     C* Modifico lo stato dello spool "destinazione" in modo che sia sempre a *RDY
068900040520     C                   CLEAR                   SPOOLATR
069000040520     C                   EVAL      POSDA = 108
069100040520     C                   EVAL      LUNG  = 10
069200040520     C                   EVAL      VALORE = '*READY'
069300040520     C                   EVAL      PARAM(6) = SPOOLATR
069400040520     C*
069500040520     C* Modifico a *NO il precongelamento prima della scrittura
069600040520     C                   CLEAR                   SPOOLATR
069700040520     C                   EVAL      POSDA = 128
069800040520     C                   EVAL      LUNG  = 10
069900040520     C                   EVAL      VALORE = '*NO'
070000040520     C                   EVAL      PARAM(7) = SPOOLATR
070100040130     C*
070200040217     C* Effettuo la copia dello spool con gli attributi così modificati
070300040130     C                   CALL      'XDUPSPLFR'
070400040130     C                   PARM                    File
070500040130     C                   PARM                    Job
070600040130     C                   PARM                    User
070700040130     C                   PARM                    JobX
070800040130     C                   PARM                    SplfXx
070900040130     C                   PARM                    PARAM
071000040217     C*
071100040217     C                   ENDSR
071200040217     C****************************************************************
071300041006
071400041006
071500041006     C****************************************************************
071600041006     C     REPNUM        BEGSR
071700041006     C****************************************************************
071800041006     C*
071900041006     C* INIALIAZIZZO FLAG ERRORE
072000041006     C                   CLEAR                   TRUL33DS
072100041006     C                   EVAL      I33TLA = 'L'
072200041006     C                   EVAL      I33OPE = *zeros
072300041006     C                   EVAL      I33CNU = 024
072400041006     C                   EVAL      I33NUM = 1
072500041006     C                   MOVEL     TRUL33DS      KPJBU
072600041025     C                   CALL      'TRUL33C'
072700041006     C                   PARM                    KPJBA
072800041006     C                   MOVEL     KPJBU         TRUL33DS
072900041006     C                   IF        O33ERR <> *zeros
073000041006     C                   MOVEL     '2'           Esito
073100041006     C                   EVAL      Messaggio = 'TRTCM1R1 - Errore in ' +
073200041025     C                                         'reperimento contatore (TRUL33C)'
073300041006     C                   ENDIF
073400041006     C*
073500041006     C                   ENDSR
073600041006     C****************************************************************
073700040217
073800040217
073900040217     C****************************************************************
074000040217     C* REPERISCE I DATI UTENTE
074100040217     C****************************************************************
074200040217     C     REPDATIUTE    BEGSR
074300040217     C*
074400040217     C* INIZIALIZZA VARIABILI DI WRK
074500040217     C                   CLEAR                   TIBS34DS
074600040217     C                   CLEAR                   AZUTEDS
074700040217     C                   CLEAR                   DDATIUTE
074800040217     C*
074900040217     C     *DTAARA       DEFINE    §azute        azuteds
075000040217     C     *DTAARA       DEFINE    §datiute      ddatiute
075100040217     C                   IN(E)     *DTAARA
075200040217     C                   IF        %Error
075300040217     C                   EVAL      I34Tla = 'L'
075400040217     C                   CALL      'TIBS34R'
075500040217     C                   PARM                    Tibs34Ds
075600040217     C                   IN        *DTAARA
075700040217     C                   ENDIF
075800040217     C*
075900040217     C                   ENDSR
076000040217     C***************************************************************
076100041013
076200041013
076300041013
076400041013     C****************************************************************
076500041013     C* CARICA DATI TABELLATI
076600041013     C****************************************************************
076700041013     C     CARTAB        BEGSR
076800041013     C*
076900041013     C* Reperimento tabella MRA
077000041013     C                   CLEAR                   DMRA
077100041013     C                   MOVE(P)   'MRA'         tbeCOD
077200041013     C                   MOVEL(p)  'TRUL83R'     tbeKE1
077300041013     C                   MOVE      *blanks       tbeKE2
077400041013     C     KEYTBE01P     CHAIN     TNTBE01L
077500041013     C                   IF        %found(TNTBE01L) AND
077600041013     C                             tbeATB <> 'A'
077700041013     C                   SETOFF                                       60
077800041013     C                   MOVEL     tbeUNI        DMRA
077900041013     C                   ELSE
078000041013     C                   SETON                                        60
078100051025     C                   EVAL      Esito = '2'
078200051025     C                   EVAL      Messaggio = 'Record TRUL83R non trovato in '+
078300051025     C                                      'tabella MRA, avvisare il CED.'
078400041013     C                   ENDIF
078500041013     C*
078600041013     C                   ENDSR
078700041013     C***************************************************************
078800051025
078900051025
079000051025     C****************************************************************
079100051025     C* REPERISCE ATTRIBUTI SPOOL X RICONDURRE SCHIERA LISTA SPOOL/JOB DA FORMATO "INTERNAL ID" A "CLASSICO"
079200051025     C****************************************************************
079300051025     C     RTVSKSPOOL    BEGSR
079400051025     C*
079500051025     C                   SETOFF                                       70
079600051025     C*
079700051025     C* Reperisco il numero degli spool ricevuti in input
079800051104     C                   Z-ADD     1             x                 5 0
079900051104     C                   DOW       x <= %elem(SKSPOOLID)
080000051025     C                   IF        SKSPOOLID(x) <> *blanks
080100051104     C                   EVAL      DSSPOOLID = SKSPOOLID(x)
080200051025     C                   EVAL      XLSTSPLDS2 = Sp_Job_ID
080300051025     C*
080400051025     C* Reperisco gli attributi dello spool corrente
080500051025     C                   CLEAR                   XSPLATRDS
080600051025     C                   CLEAR                   XSPLATRDS1
080700051025     C                   EVAL      §SPLA_JID  = §LST2_JOB
080800051025     C                   EVAL      §SPLA_SID  = §LST2_SPL
080900051025     C                   CALL(e)   'XSPLATR'
081000051025     C                   PARM                    XSPLATRDS
081100051025     C*
081200051025     C* Gestisco eventuale errore
081300051025     C                   IF        %error OR §SPLA_ERR = 'E'
081400051025     C                   SETON                                        70
081500051025     C                   EVAL      Esito = '2'
081600051025     C                   EVAL      Messaggio = 'Errori in reperimento ' +
081700051025     C                                         'attributi spool!'
081800051025     C                   LEAVE
081900051025     C                   ELSE
082000051025     C*
082100051025     C* Se tutto ok => valorizzo la schiera lista spool in formato "classico"
082200051025     C                   EVAL      XSPLATRDS1 = §SPLA_ATTR
082300051025     C                   EVAL      File       = §SPLA1_FIL
082400051025     C                   EVAL      Job        = §SPLA1_JNM
082500051025     C                   EVAL      User       = §SPLA1_USN
082600051025     C                   EVAL      JobX       = §SPLA1_JNR
082700051102     C                   MOVE(P)   §SPLA1_FNR    SplfXx
082800051025     C                   EVAL      UsrDta     = §SPLA1_DTA
082900051025     C                   EVAL      SplRig     = §SPLA1_LEN
083000051025     C                   EVAL      SplCol     = §SPLA1_WID
083100051025     C                   EVAL      SplCPI     = §SPLA1_CPI
083200051025     C                   EVAL      OutQue     = §SPLA1_QNM
083300051102     C                   EVAL      DataOra = %subst(§SPLA1_FOP:6:2) + '/' +
083400051102     C                                       %subst(§SPLA1_FOP:4:2) + '/' +
083500051102     C                                       %subst(§SPLA1_FOP:2:2) + ' '+
083600051102     C                                       %subst(§SPLA1_TIM:1:2) + ':'+
083700051102     C                                       %subst(§SPLA1_TIM:3:2) + ':'+
083800051102     C                                       %subst(§SPLA1_TIM:5:2)
083900051025     C                   EVAL      TotPagine  = §SPLA1_TPG
084000051025     C                   EVAL      StatoSpool = §SPLA1_STS
084100051025     C                   EVAL      SKSPOOL(x) = DSSPOOL
084200051025     C                   ENDIF
084300051104     C                   ELSE
084400051104     C                   LEAVE
084500051102     C                   ENDIF
084600051104     C                   ADD       1             x
084700051025     C                   ENDDO
084800051025     C*
084900051025     C                   ENDSR
085000051025     C***************************************************************
085100040217
085200040217
085300040217     C***************************************************************
085400040130     C     *INZSR        BEGSR
085500040217     C***************************************************************
085600040217     C*
085700041006     C     *ENTRY        PLIST
085800041006     C                   PARM                    SKSPOOL
085900051102     C                   PARM                    SKSPOOLIDDS
086000051102     C                   PARM                    FlgSnd            1
086100040607     C                   PARM                    EmailMitt        30
086200051025     C                   PARM                    EmailDest       110
086300051025     C                   PARM                    EmailObj         55
086400051025     C                   PARM                    EmailTxt1        75
086500051025     C                   PARM                    EmailTxt2        75
086600051025     C                   PARM                    EmailTxt3        75
086700051025     C                   PARM                    EmailTxt4        75
086800051025     C                   PARM                    EmailTxt5        75
086900051025     C                   PARM                    EmailTxt6        75
087000051025     C                   PARM                    EmailTxt7        75
087100051025     C                   PARM                    EmailTxt8        75
087200051025     C                   PARM                    FlgTappoLast      1
087300051025     C                   PARM                    FlgBypChSp        1
087400051025     C                   PARM                    FlgDspVid         1
087500051025     C                   PARM                    FlgDltSplIn       1
087600051025     C                   PARM                    Esito             1
087700051025     C                   PARM                    Messaggio        50
087800051025     C*
087900051025     C* Controllo se passati in input schiera spool/job in formato "internal ID"
088000051025     C* se sì => riconduco a DS standard tramite reperimento attributi spool
088100051102     C                   IF        SKSPOOLIDDS <> *blanks
088200051025     C                   CLEAR                   SKSPOOL
088300051025     C                   EXSR      RTVSKSPOOL
088400051025     C                   ENDIF
088500041006     C*
088600041006     C* Reperisco il numero degli spool ricevuti in input
088700051104     C                   Z-ADD     *zeros        wNumSpool
088800051104     C                   Z-ADD     1             x                 5 0
088900051104     C                   DOW       x <= %elem(SKSPOOL)
089000041006     C                   IF        SKSPOOL(x) <> *blanks
089100041006     C                   ADD       1             wNumSpool
089200051104     C                   ELSE
089300051104     C                   LEAVE
089400041006     C                   ENDIF
089500051104     C                   ADD       1             x
089600041006     C                   ENDDO
089700041006     C*
089800051025     C* Se trattasi scelta multipla disabilito il tasto funzine F5 x la visualizzazione dello spool richiesto
089900051025     C                   IF        wNumSpool > 1 or FlgDspVid = 'N'
090000041006     C                   SETON                                        51
090100041006     C                   ENDIF
090200040524     C*
090300040524     C* Reperisco il P.O. dell'utente corrente
090400040524     C                   EXSR      REPDATIUTE
090500040218     C*
090600040218     C* CHIAVI DI LETTURA
090700040224     C     KEYMAS01C     KLIST                                                  *TIMAS01L
090800040218     C                   KFLD                    KmasSPOOL                       -FILE SPOOL
090900040224     C                   KFLD                    KmasUSDTA                       -DATI UTENTE
091000040218     C                   KFLD                    KmasUTE                         -MASCHERA UTENTE
091100040224     C     KEYMAS01P     KLIST                                                  *TIMAS01L
091200040224     C                   KFLD                    KmasSPOOL                       -FILE SPOOL
091300040224     C                   KFLD                    KmasUSDTA                       -DATI UTENTE
091400041013     C     KEYTBE01P     KLIST
091500041013     C                   KFLD                    tbeCOD
091600041013     C                   KFLD                    tbeKE1
091700041013     C                   KFLD                    tbeKE2
091800040217     C*
091900040130     C                   ENDSR
092000041006     O*------------------------------------------------------------------------*
092100041006     O* STAMPA
092200041006     O*------------------------------------------------------------------------*
092300041006     OPRTEMAIL  E            DET         1
092400041006     O                       TESTO1              +1
092500041006     O          E            DET         1
092600041006     O                       TESTO2              +1
092700041006     O          E            DET         1
092800041006     O                       TESTO3              +1
092900041006     O          E            DET         1
093000041006     O                       TESTO4              +1
093100041006     O          E            DET         1
093200041006     O                       TESTO5              +1
093300041006     O          E            DET         1
093400041006     O                       TESTO6              +1
093500041006     O          E            DET         1
093600041006     O                       TESTO7              +1
093700041006     O          E            DET         1
093800041006     O                       TESTO8              +1
