000113160624      //************************************************************************
000114160624      //
000115160624      // Questo modulo sostituisce quello di Proj per Laguna
000116160624      // PJAI1S3/SRCMOD(PJDR1701R)
000117160624      //
000118160624      // Nel campo ANLGN00F.LGNIDLGN ricevo il numero assoluto registrazione,
000119160624      // col quale accedo a NDREG00F per analizzare la registrazione.
000120160624      //
000121160624      // Mi interessano:
000122160624      // - le fatture di vendita;
000123160624      // - le note di accredito a clienti;
000124160624      // - le auto fatture di acquisto.
000125160624      //
000128160624      //************************************************************************
000129160624
000130160624      /COPY GAITRASRC/SRCCONST,BRTLEHSPEC
000131160713       CTL-OPT BNDDIR('PJXBND':'PJCBND':'YCO');
000142160624
000143160624      //************************************************************************
000144160624      //
000145160624      // Strutture.
000146160624      //
000147160624      //************************************************************************
000161160624
000162160913       DCL-DS andiz00f EXT QUALIFIED TEMPLATE END-DS;
000163160913       DCL-DS myAnlgn00f LIKEDS(anlgn00f);
000169160713
000170160913       DCL-DS this QUALIFIED INZ;
000172160913         societa LIKE(andiz00f.societa);
000173160913         ctb LIKE(andiz00f.ctb);
000174160913         kcc LIKE(andiz00f.kcc);
000175160913         ksc LIKE(andiz00f.ksc);
000176160913         dtPar LIKE(andiz00f.data);
000177160913         nrPar LIKE(andiz00f.nr);
000178160913         serPar LIKE(andiz00f.serieNm);
000179160913         dare LIKE(andiz00f.dareAvere);
000180160913         nrAsReg LIKE(andiz00f.nr);
000181160713       END-DS;
000182160712
000183160712      //************************************************************************
000184160712      //
000185160712      // Prototipi.
000186160712      //
000187160712      //************************************************************************
000188160712
000190160713       /COPY GAITRASRC/SRCPROTOPR,YDOCFLOW
000192160624
000193160624      //************************************************************************
000194160624      //
000195160624      // Parametri.
000196160624      //
000197160624      //************************************************************************
000198160624
000199160624       DCL-PI Pjdr1701r ;
000200160624         piOpzione CHAR(2) CONST;
000201160624         piErrore CHAR(1);
000202160624         piVista ZONED(2) CONST OPTIONS(*NOPASS:*OMIT);
000203160624         piDati CHAR(4000) OPTIONS(*VARSIZE:*NOPASS:*OMIT);
000204160624         piLunghezza BINDEC(9) CONST OPTIONS(*NOPASS:*OMIT);
000205160624         piCommit CHAR(1) CONST OPTIONS(*NOPASS:*OMIT);
000206160624         piNrChiavi ZONED(2) CONST OPTIONS(*NOPASS:*OMIT);
000207160624         piAlloca CHAR(1) CONST OPTIONS(*NOPASS:*OMIT);
000208160624         piRefresh CHAR(1) CONST OPTIONS(*NOPASS:*OMIT);
000209160624       END-PI ;
000210160624
000211160624      //************************************************************************
000212160624      //
000213160624      // Opzioni SQL.
000214160624      //
000215160624      //************************************************************************
000216160712
000217160624       EXEC SQL SET OPTION SQLCA = *NO;
000218160624
000219160624      //************************************************************************
000220160624      //
000221160624      // Main.
000222160624      //
000223160624      //************************************************************************
000224160624
000225160624       piErrore = '0';
000226160624
000227160624       IF piOpzione <> '01';
000228160624         RETURN;
000229160624       ENDIF;
000230160624
000231160913       RESET this;
000232160913       myAnlgn00f = %SUBST(piDati : 1 : piLunghezza);
000233160624
000234160913       // Non ho ricevuto ID registrazione, provo con ID partita.
000235160913
000236160913       IF myAnlgn00f.lgnIdLgn = *ZERO AND piNrChiavi = 7 AND
000237160913          myAnlgn00f.lgnClassLg = 'DOC';
000238160913
000239160913         SELECT;
000240160913           WHEN myAnlgn00f.lgnClassPj = '020'; // Fornitore.
000241160913             this.dare = 0;
000242160913           WHEN myAnlgn00f.lgnClassPj = '030'; // Cliente.
000243160913             this.dare = 1;
000244160913           OTHER;
000245160913             RETURN;
000246160913         ENDSL;
000247160913
000248160913         // Cerco il numero assoluto di registrazione della fattura.
000249160913
000250160913         this.societa = myAnlgn00f.lgnKey101;
000251160913         this.ctb = myAnlgn00f.lgnKey102;
000252160913         this.kcc = myAnlgn00f.lgnKey103;
000253160913         this.ksc = myAnlgn00f.lgnKey104;
000254160913         this.dtPar = %DATE(myAnlgn00f.lgnKey105);
000255160913         this.nrPar = %DEC(myAnlgn00f.lgnKey106 : 9 : 0);
000256160913         this.serPar = myAnlgn00f.lgnKey107;
000257160913
000258160913         EXEC SQL
000259160913           SELECT pas.pasNrAsReg
000260160913             INTO :this.nrAsReg
000261160913             FROM ndpas00f AS pas
000262160913             JOIN anope00f AS ope ON pas.pasSocieta = ope.opeSocieta
000263160913                                 AND pas.pasCausRig = ope.opeCausale
000264160913            WHERE pas.pasSocieta = :this.societa
000265160913              AND pas.pasCtb = :this.ctb
000266160913              AND pas.pasKcc = :this.kcc
000267160913              AND pas.pasKsc = :this.ksc
000268160913              AND pas.pasDtPar = :this.dtPar
000269160913              AND pas.pasNrPar = :this.nrPar
000270160913              AND pas.pasSerPar = :this.serPar
000271160913              AND pas.pasDare = :this.dare
000272160913              AND ope.opeTpc = '1'
000273160913            ORDER BY pas.pasSocieta
000274160913                   , pas.pasCtb
000275160913                   , pas.pasKcc
000276160913                   , pas.pasKsc
000277160913                   , pas.pasDtPar
000278160913                   , pas.pasNrPar
000279160913                   , pas.pasSerPar
000280160913                   , pas.pasDtReg
000281160913            FETCH FIRST ROW ONLY
000282160913         ;
000283160913
000284160913         SELECT;
000285160913           WHEN sqlCode < *ZERO;
000286160913             DUMP(A);
000287160913             RETURN;
000288160913         ENDSL;
000289160913
000290160913         myAnlgn00f.lgnIdLgn = this.nrAsReg;
000291160913
000292160913       ENDIF;
000293160913
000294160913       IF myAnlgn00f.lgnIdLgn = *ZERO;
000295160624         RETURN;
000296160624       ENDIF;
000297160624
000313160913       IF NOT DocFlow_IsRegistrazioneInDocFlow( 0 : myAnlgn00f.lgnIdLgn
000314160913                                              : myAnlgn00f );
000315160713         piErrore = '1';
000316160713       ENDIF;
000318160713
000319160913       %SUBST(piDati : 1 : piLunghezza) = myAnlgn00f;
000320160713
000321160624       RETURN;
000322160712
