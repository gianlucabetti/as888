000100160624      //************************************************************************
000101160624      //
000102160624      // Questo programma sostituisce quello di Proj per Laguna
000103160624      // PJAI1S3/SRCPGM(PJXX038R)
000107160624      //
000108160624      // Mi interessano:
000109160624      // - le fatture di vendita;
000110160624      // - le note di accredito a clienti;
000111160624      // - le auto fatture di acquisto.
000112160624      //
000113160624      //************************************************************************
000114160624
000115160624      /COPY GAITRASRC/SRCCONST,BRTLEHSPEC
000116160624       CTL-OPT ACTGRP(*CALLER);
000117160624
000118160624      //************************************************************************
000119160624      //
000120160624      // Costanti.
000121160624      //
000122160624      //************************************************************************
000123160624
000124160624       DCL-C SOCIETA_BRT '201';
000125160624       DCL-C NFT0 'NFT0';
000126160624       DCL-C NFTP 'NFTP';
000127160624       DCL-C NFT9 'NFT9';
000128160624       DCL-C FATTURA_AUTOMATICA 'FATA';
000129160624       DCL-C FATTURA_CLIENTI_VARI 'FAT9';
000131160624       DCL-C FATTURA_MANUALE_BRT 'FTMB';
000132160624       DCL-C FATTURA_MANUALE_ALTRE 'FTMX';
000133160624       DCL-C NOTA_ACCREDITO_BRT 'NACB';
000134160624       DCL-C NOTA_ACCREDITO_ALTRE 'NACX';
000135160624
000136160624      //************************************************************************
000137160624      //
000138160624      // Strutture.
000139160624      //
000140160624      //************************************************************************
000141160624
000142160624       DCL-DS this QUALIFIED;
000143160624         registrazioneNumeroAssoluto PACKED(9);
000144160624         registrazioneData DATE;
000145160624         registrazioneNumero PACKED(9);
000146160624         registrazioneSerie CHAR(4);
000147160624         registrazioneCausale CHAR(4);
000148160624         registrazioneIva CHAR(1);
000149160624         tipoDocumentoIva CHAR(1);
000150160624         idLaguna ZONED(10);
000151160624         classeLaguna CHAR(3);
000152160624         tipoSpool CHAR(4);
000153160624         docFlowUrl VARCHAR(128)
000154160624                    INZ('https://ecm.brt.it/b/b.asp?Params=SYN_INVO_UNIQUEID=');
000156160624       END-DS;
000157160624
000158160624      //************************************************************************
000159160624      //
000160160624      // Prototipi.
000161160624      //
000162160624      //************************************************************************
000163160624
000164160624       /COPY GAITRASRC/SRCPROTOPR,SYSTEM
000165160624
000166160624      //************************************************************************
000167160624      //
000168160624      // Parametri.
000169160624      //
000170160624      //************************************************************************
000171160624
000172160624       DCL-PI Pjxx038r ;
000173160624         piSocieta CHAR(3) CONST;
000174160624         piComando CHAR(15) CONST;
000175160624         piParametri CHAR(44) DIM(70) CONST;
000176160624         piErrore CHAR(1);
000177160624         piRet CHAR(32);
000178160624       END-PI ;
000179160624
000180160624      //************************************************************************
000181160624      //
000182160624      // Opzioni SQL.
000183160624      //
000184160624      //************************************************************************
000185160624
000186160624       EXEC SQL SET OPTION SQLCA = *NO;
000187160624
000188160624      //************************************************************************
000189160624      //
000190160624      // Main.
000191160624      //
000192160624      //************************************************************************
000193160624
000194160624       piErrore = '0';
000195160624
000196160624       IF piComando <> 'VIEW';
000197160624         RETURN;
000198160624       ENDIF;
000199160624
000200160624       RESET this;
000201160624       this.classeLaguna = piParametri(1);
000202160624       this.idLaguna = %DEC(piParametri(2):10:0);
000203160624       this.registrazioneNumeroAssoluto = this.idLaguna;
000204160624
000205160624       EXEC SQL
000206160624         SELECT DATE(anlgn00f.lgnKey102)
000207160624              , DEC(anlgn00f.lgnKey103, 9, 0)
000208160624              , LEFT(anlgn00f.lgnKey104, 4)
000209160624           INTO :this.registrazioneData
000210160624              , :this.registrazioneNumero
000211160624              , :this.registrazioneSerie
000212160624           FROM anlgn00f
000213160624          WHERE anlgn00f.lgnIstanza = ''
000214160624            AND anlgn00f.lgnClassLg = :this.classeLaguna
000215160624            AND anlgn00f.lgnIdLgn = :this.idLaguna
000216160624            AND anlgn00f.lgnIdCrt = 0
000217160624       ;
000218160624
000219160624       SELECT;
000220160624         WHEN sqlCode < *ZERO;
000221160624           DUMP(A);
000222160624           piErrore = '1';
000223160624           RETURN;
000224160624         WHEN sqlCode = 100;
000225160624           piErrore = '1';
000226160624           RETURN;
000227160624       ENDSL;
000228160624
000229160624       EXEC SQL
000230160624         SELECT ndreg00f.regCausTes
000231160624              , ndreg00f.regDocIva
000232160624              , anope00f.opeTpDocI
000233160624           INTO :this.registrazioneCausale
000234160624              , :this.registrazioneIva
000235160624              , :this.tipoDocumentoIva
000236160624           FROM ndreg00f
000237160624           JOIN anope00f
000238160624             ON ndreg00f.regSocieta = anope00f.opeSocieta
000239160624            AND ndreg00f.regCausTes = anope00f.opeCausale
000240160624          WHERE ndreg00f.regSys = 0
000241160624            AND ndreg00f.regNrAsReg = :this.registrazioneNumeroAssoluto
000242160624       ;
000243160624
000244160624       SELECT;
000245160624         WHEN sqlCode < *ZERO;
000246160624           DUMP(A);
000247160624           piErrore = '1';
000248160624           RETURN;
000249160624         WHEN sqlCode = 100 OR this.registrazioneIva = *OFF;
000250160624           piErrore = '1';
000251160624           RETURN;
000252160624       ENDSL;
000253160624
000254160624       // Determino il tipo di spool.
000255160624
000256160624       SELECT;
000257160624         WHEN %SUBST(this.registrazioneSerie : 1 : 1) = '1'; // Acquisti
000258160624         WHEN %SUBST(this.registrazioneSerie : 1 : 1) = '2'; // Vendite
000259160624           SELECT;
000260160624             WHEN this.tipoDocumentoIva = '1'  // Nota accredito nazionale
000261160624               OR this.tipoDocumentoIva = '4'  // Nota accredito estera
000262160624               OR this.tipoDocumentoIva = '7'; // Nota accredito solo IVA
000263160624               IF piSocieta = SOCIETA_BRT;
000264160624                 this.tipoSpool = NOTA_ACCREDITO_BRT;
000265160624               ELSE;
000266160624                 this.tipoSpool = NOTA_ACCREDITO_ALTRE;
000267160624               ENDIF;
000268160624             OTHER;
000269160624               SELECT;
000270160624                 WHEN this.registrazioneCausale = NFT0;
000271160624                   this.tipoSpool = FATTURA_AUTOMATICA;
000272160624                 WHEN this.registrazioneCausale = NFTP
000274160624                   OR this.registrazioneCausale = NFT9;
000275160624                   this.tipoSpool = FATTURA_CLIENTI_VARI;
000276160624                 OTHER;
000277160624                   IF piSocieta = SOCIETA_BRT;
000278160624                     this.tipoSpool = FATTURA_MANUALE_BRT;
000279160624                   ELSE;
000280160624                     this.tipoSpool = FATTURA_MANUALE_ALTRE;
000281160624                   ENDIF;
000282160624               ENDSL;
000283160624           ENDSL;
000284160624       ENDSL;
000285160624
000286160624       this.docFlowUrl += this.tipoSpool + piSocieta
000287160624                        + %CHAR(%SUBDT(this.registrazioneData:*YEARS))
000288160624                        + this.registrazioneSerie
000289160624                        + %EDITC(this.registrazioneNumero:'X');
000290160624
000291160624       ExecuteCommand('STRPCCMD PAUSE(*NO) PCCMD(''"start '
000292160624                     + this.docFlowUrl + '"'')');
000293160624
000294160624       RETURN;
000295160624
000296160624      //************************************************************************
000297160624      //
000298160624      // Inizializzazioni.
000299160624      //
000300160624      //************************************************************************
000301160624
000302160624       BEGSR *INZSR;
000303160624
000304160624         ExecuteCommand('STRPCO');
000305160624
000306160624       ENDSR;
