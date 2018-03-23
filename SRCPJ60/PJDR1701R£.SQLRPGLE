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
000162160624       DCL-DS psds QUALIFIED PSDS;
000163160624         jobUser CHAR(10) POS(254);
000164160624       END-DS;
000165160713
000166160713       DCL-DS this QUALIFIED;
000167160713         anlgn00f LIKEDS(anlgn00f);
000168160713       END-DS;
000169160712
000170160712      //************************************************************************
000171160712      //
000172160712      // Prototipi.
000173160712      //
000174160712      //************************************************************************
000175160712
000176160713       /COPY GAITRASRC/SRCPROTOPR,YDOCFLOW
000177160624
000178160624      //************************************************************************
000179160624      //
000180160624      // Parametri.
000181160624      //
000182160624      //************************************************************************
000183160624
000184160624       DCL-PI Pjdr1701r ;
000185160624         piOpzione CHAR(2) CONST;
000186160624         piErrore CHAR(1);
000187160624         piVista ZONED(2) CONST OPTIONS(*NOPASS:*OMIT);
000188160624         piDati CHAR(4000) OPTIONS(*VARSIZE:*NOPASS:*OMIT);
000189160624         piLunghezza BINDEC(9) CONST OPTIONS(*NOPASS:*OMIT);
000190160624         piCommit CHAR(1) CONST OPTIONS(*NOPASS:*OMIT);
000191160624         piNrChiavi ZONED(2) CONST OPTIONS(*NOPASS:*OMIT);
000192160624         piAlloca CHAR(1) CONST OPTIONS(*NOPASS:*OMIT);
000193160624         piRefresh CHAR(1) CONST OPTIONS(*NOPASS:*OMIT);
000194160624       END-PI ;
000195160624
000196160624      //************************************************************************
000197160624      //
000198160624      // Opzioni SQL.
000199160624      //
000200160624      //************************************************************************
000207160712
000208160624       EXEC SQL SET OPTION SQLCA = *NO;
000209160624
000210160624      //************************************************************************
000211160624      //
000212160624      // Main.
000213160624      //
000214160624      //************************************************************************
000215160624
000216160624       piErrore = '0';
000217160624
000218160624       IF piOpzione <> '01';
000219160624         RETURN;
000220160624       ENDIF;
000221160624
000222160713       this.anlgn00f = %SUBST(piDati : 1 : piLunghezza);
000223160624
000224160713       IF this.anlgn00f.lgnIdLgn = *ZERO;
000225160624         RETURN;
000226160624       ENDIF;
000229160624
000313160713       IF NOT DocFlow_IsRegistrazioneInDocFlow( 0 : this.anlgn00f.lgnIdLgn
000314160713                                              : this.anlgn00f );
000315160713         piErrore = '1';
000316160713       ENDIF;
000317160713
000318160713       %SUBST(piDati : 1 : piLunghezza) = this.anlgn00f;
000319160713
000320160624       RETURN;
000321160712
