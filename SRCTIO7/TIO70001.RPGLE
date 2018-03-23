000100141222     ***************************************************************************
000101140207     **
000102140304     ** ORM 2.0
000103140321     **
000104140321     ** Questo programma è chiamato da AS777 via SQL e nel paradigma MVC ricopre
000105140321     ** il ruolo di controller tra la view (browser) e il model (programma FIORA00R).
000106140207     **
000107140207     ***************************************************************************
000108160321
000109140207     H DFTACTGRP(*NO)
000110160526     H/COPY GAITRASRC/SRCCONST,BRTLEHSPEC
000111140207
000112140207     ***************************************************************************
000113140207     **
000114140207     ** Costanti.
000115140207     **
000116140207     ***************************************************************************
000117160321
000118140207      /COPY GAITRASRC/SRCCONST,FIORA00R
000119140311      /COPY GAITRASRC/SRCCONST,FIORB00R
000120160321
000121151104     D A...
000122151104     D                 C                   'A'
000123140404     D GAITRATMP...
000124140404     D                 C                   'GAITRATMP'
000125140212     D RPYOPC_DONE...
000126140212     D                 C                   'DONE'
000127140307     D RPYOPC_ERRORE...
000128140307     D                 C                   'ERRORE'
000129140212     D RPYOPC_OTHERERROR...
000130140212     D                 C                   'OTHERERROR'
000131140311     D RQSOPC_CARCLIRIT...
000132140311     D                 C                   'CARCLIRIT'
000133140404     D RQSOPC_CARCLIRITE...
000134140404     D                 C                   'CARCLIRITE'
000135140212     D RQSOPC_CHKPARTE1...
000136140212     D                 C                   'CHKPARTE1'
000137140212     D RQSOPC_CHKPARTE2...
000138140212     D                 C                   'CHKPARTE2'
000139140212     D RQSOPC_CONFERMA...
000140140212     D                 C                   'CONFERMA'
000141140220     D RQSOPC_CHECKIMM...
000142140220     D                 C                   'CHECKIMM'
000143140404     D RQSOPC_GETDATERIT...
000144140404     D                 C                   'GETDATERIT'
000145140408     D RQSOPC_GETINFORIT...
000146140408     D                 C                   'GETINFORIT'
000147151104     D S...
000148151104     D                 C                   'S'
000149140213     D THIS_ESITO_ERRORE...
000150140213     D                 C                   -1
000151140213     D THIS_ESITO_OK...
000152140213     D                 C                   0
000153140321     D THIS_INSERIRE_ORM_NO...
000154140321     D                 C                   *OFF
000155140321     D THIS_INSERIRE_ORM_SI...
000156140321     D                 C                   *ON
000157140207
000158140207     ***************************************************************************
000159140207     **
000160140207     ** Strutture dati.
000161140207     **
000162140207     ***************************************************************************
000163160321
000164140404      /COPY QSYSINC/QRPGLESRC,QUSEC
000165160321
000166140404     D fiora0f0      E DS                  QUALIFIED INZ(*EXTDFT)               Forzature errori
000167140210     D fiora0i0      E DS                  QUALIFIED INZ(*EXTDFT)               Input
000168140207     D  idRichiamo   E                     INZ(FIORA00_ID_RICHIAMO_INTERNET)
000169140411     D  orrMin       E                     INZ(0700)
000170140411     D  orrMax       E                     INZ(1845)
000171140210     D fiora0m0      E DS                  QUALIFIED INZ(*EXTDFT)               Errori e messaggi
000172140210     D fiora0o0      E DS                  QUALIFIED INZ(*EXTDFT)               Output
000173140311     D rp1Error        DS                  QUALIFIED
000174140311     D  errCode                       7
000175140311     D  errSpace                      1
000176140311     D  errText                     200
000177140210     D this            DS                  QUALIFIED INZ
000178140210     D  rpyOpCode...
000179140210     D                               10I 0
000180160217     D  rqsOpCode...
000181160217     D                               10I 0
000182140210     D  rpyIdMsg...
000183140210     D                               10I 0
000184140210     D  e...
000185140210     D                                3U 0
000186140210     D  m...
000187140210     D                                3U 0
000188140210     D  idCampo...
000189140210     D                               10A
000190140213     D  idMsg...
000191140213     D                                7A
000192140210     D  textMsg...
000193140210     D                              255A
000194140210     D  errWarn...
000195140210     D                                1A
000196160318     D tio110ids     E DS                  QUALIFIED PREFIX('':4) INZ
000198170619     D                                     EXTNAME('STRATEDEV/TIO110IDS')
000199140213     D  o110por      E                     INZ(*ZERO)
000200140213     D  o110poe      E                     INZ(*ZERO)
000201140225     D  pkg1                          6A   OVERLAY(pkg)
000202140225     D  pkg2                          1A   OVERLAY(pkg:7)
000203140225     D  vlm1                          2A   OVERLAY(vlm)
000204140225     D  vlm2                          3A   OVERLAY(vlm:3)
000205140213     D tio110ods     E DS                  QUALIFIED PREFIX('':4) INZ
000206170619     D                                     EXTNAME('STRATEOBJ/TIO110ODS')
000207140207
000208140207     ***************************************************************************
000209140207     **
000210140207     ** Campi.
000211140207     **
000212140207     ***************************************************************************
000213160321
000214140321     D rqsOpc          S             10A                                        REQUEST OPCODE
000215140321     D rqsDta          S           9999A                                        REQUEST DATA
000216160321     D rqsLen          S              4P 0                                      REQUEST DATA LENGTH
000217160321     D rqsCty          S             10A                                        REQUEST CLIENT TYPE
000218140321     D rqsCid          S             10A                                        REQUEST CLIENT
000219140321     D rpyOpc          S             10A                                        REPLY OPCODE
000220140321     D rpyDta          S           9999A                                        REPLY DATA
000221140311     D rpyLen          S              4P 0                                      REPLY DATA LENGTH
000229140408
000230140207     ***************************************************************************
000231140207     **
000232140207     ** Prototipi.
000233140207     **
000234140207     ***************************************************************************
000235160321
000236140212      /COPY GAITRASRC/SRCPROTOPR,FIORA00R
000237140311      /COPY GAITRASRC/SRCPROTOPR,FIORB00R
000238140404      /COPY GAITRASRC/SRCPROTOPR,QUSCHGUS
000239140404      /COPY GAITRASRC/SRCPROTOPR,QUSCRTUS
000240140404      /COPY GAITRASRC/SRCPROTOPR,QUSDLTUS
000241140404      /COPY GAITRASRC/SRCPROTOPR,QUSRTVUS
000242140404      /COPY GAITRASRC/SRCPROTOPR,TRULTMPN
000243140210
000244140207     ***************************************************************************
000245140207     **
000246140207     ** Parametri.
000247140207     **
000248140207     ***************************************************************************
000249160321
000250140207     C     *ENTRY        PLIST
000251140312     C                   PARM                    rqsOpc
000252140312     C                   PARM                    rqsDta
000253140312     C                   PARM                    rqsLen
000254140312     C                   PARM                    rqsCty
000255140312     C                   PARM                    rqsCid
000256140312     C                   PARM                    rpyOpc
000257140312     C                   PARM                    rpyDta
000258140312     C                   PARM                    rpyLen
000259140207
000260140207     ***************************************************************************
000261140207     **
000262140207     ** Main.
000263140207     **
000264140207     ***************************************************************************
000265140207
000266140207      /FREE
000267140207
000268140212       rpyOpc = RPYOPC_DONE;
000269140210       CLEAR rpyDta;
000270140210       CLEAR rpyLen;
000271140207       RESET rp1Error;
000272140210       RESET this;
000273140210       RESET fiora0i0;
000274140212       RESET fiora0m0;
000275140212       RESET fiora0f0;
000276140212       RESET fiora0o0;
000277140207
000278140207       SELECT;
000279140212         WHEN rqsOpc = RQSOPC_CHKPARTE1;
000280140210           ChkParte1();
000281140212         WHEN rqsOpc = RQSOPC_CHKPARTE2;
000282140211           ChkParte2();
000283140220         WHEN rqsOpc = RQSOPC_CHECKIMM;
000284140220           CheckImm();
000285140212         WHEN rqsOpc = RQSOPC_CONFERMA;
000286140321           Conferma(THIS_INSERIRE_ORM_SI);
000294140311         WHEN rqsOpc = RQSOPC_CARCLIRIT;
000295140311           CarCliRit();
000296140404         WHEN rqsOpc = RQSOPC_CARCLIRITE;
000297140404           CarCliRitEstesa();
000298140407         WHEN rqsOpc = RQSOPC_GETDATERIT;
000299140407           GetDateRitiro();
000300140408         WHEN rqsOpc = RQSOPC_GETINFORIT;
000301140408           GetInfoRitiro();
000302140207       ENDSL;
000408140207
000409140207       RETURN;
000410140207
000411140207      /END-FREE
000412140210
000413140210     P*--------------------------------------------------
000414140210     P* Procedure name: ChkParte1
000415140210     P* Purpose:        Controllo 1 di 2.
000416140210     P* Returns:        Esito.
000417140210     P*--------------------------------------------------
000418140210     P ChkParte1       B
000419140210     D ChkParte1       PI
000424140210
000425140401     D fiorb0i1      E DS                  QUALIFIED STATIC INZ(*EXTDFT)
000428140401     D fiorb0o1      E DS                  QUALIFIED STATIC INZ(*EXTDFT)
000433140213     D tio102ids     E DS                  QUALIFIED PREFIX('':4) STATIC
000434170619     D                                     EXTNAME('STRATEOBJ/TIO102IDS')
000435140213     D tio102ods     E DS                  QUALIFIED PREFIX('':4) STATIC INZ
000436170619     D                                     EXTNAME('STRATEOBJ/TIO102ODS')
000437140213     D  errAry                        1A   DIM(10) OVERLAY(err) INZ(*OFF)
000438140213     D  msgAry                       80A   DIM(10) OVERLAY(msg)
000440140210
000441140210      /FREE
000442140210
000443140210       tio102ids = %SUBST(rqsDta : 1 : rqsLen);
000444140210       RESET tio102ods;
000445140401       EVAL-CORR tio102ods = tio102ids;
000446140213       RESET tio110ids;
000533140213       EVAL-CORR tio110ids = tio102ids;
000534140401
000535140401       // Recupero alcuni valori anagrafici del codice di ritiro.
000536140401
000537140401       IF tio102ids.cro <> *BLANK;
000538140401
000540140401         RESET fiorb0i1;
000541140401         RESET fiorb0o1;
000542140401
000543140401         MONITOR;
000544140401           fiorb0i1.codice = %DEC(tio102ids.cro : 10 : 0);
000545140401           Fiorb00_Anagrafica( FIORB00_RQSOPCODE_GETDATI
000546160318                             : this.rpyOpCode : this.rpyIdMsg
000548160318                             : fiorb0i1.formato : fiorb0i1 : %SIZE(fiorb0i1)
000551160318                             : fiorb0o1.formato : fiorb0o1 : %SIZE(fiorb0o1) );
000555140401         ON-ERROR;
000556140401           this.rpyOpCode = FIORB00_RPYOPCODE_ERROR;
000564140401         ENDMON;
000565140213
000566140401         IF this.rpyOpCode < FIORB00_RPYOPCODE_DONE;
000569140401           rpyOpc = RPYOPC_OTHERERROR;
000570140401           rp1error.errCode = 'COR';
000571140402           rp1error.errText = 'Il codice cliente ritiro +
000572140401                               contiene dati non validi.';
000573140401           rpyDta = rp1error;
000574140401           rpyLen = %SIZE(rp1error);
000575140401           RETURN;
000576140401         ENDIF;
000577140401
000579140402         tio102ods.rsr = fiorb0o1.nome;
000580140402         tio102ods.inr = fiorb0o1.indirizzo;
000581140402         tio102ods.car = fiorb0o1.cap;
000582140402         tio102ods.lor = fiorb0o1.localita;
000583140402         tio102ods.prr = fiorb0o1.provincia;
000584140402         tio102ods.nar = fiorb0o1.nazione;
000586140401         tio102ods.per = fiorb0o1.referente;
000587140401         tio102ods.tel = fiorb0o1.telefono;
000588140401         tio102ods.ntm = fiorb0o1.natMerce;
000589140401         tio102ods.no1 = fiorb0o1.nota1;
000590151104         tio102ods.no2 = fiorb0o1.nota2;
000591140401         tio102ods.fcl = fiorb0o1.flagFncl;
000592140401         tio102ods.fpk = fiorb0o1.flagFpkg;
000593140401         tio102ods.fmc = fiorb0o1.flagFvlm;
000594140401         tio102ods.fbn = fiorb0o1.flagFbnc;
000595140401         tio102ods.fbl = fiorb0o1.flagFblc;
000596140401         tio102ods.fmt = fiorb0o1.flagFmtc;
000597140401         tio102ods.fat = fiorb0o1.flagFatt;
000598140401         tio102ods.por = %EDITC(fiorb0o1.filRit : 'X');
000599140401         tio102ods.poe = %EDITC(fiorb0o1.filEmi : 'X');
000604140403         tio102ods.ad1 = GetOraAggiustata(fiorb0o1.dalleAM);
000609140403         tio102ods.aa1 = GetOraAggiustata(fiorb0o1.alleAM);
000614140403         tio102ods.ad2 = GetOraAggiustata(fiorb0o1.dallePM);
000619140403         tio102ods.aa2 = GetOraAggiustata(fiorb0o1.allePM);
000620140403
000621140403         tio102ods.orr = GetOraAggiustata(fiorb0o1.oraRitiro);
000622140403
000623140403         IF tio102ods.orr <> *BLANK;
000626140403           SELECT;
000627140403             WHEN tio102ods.orr < '0700';
000628140403               tio102ods.orr = '0700';
000629140403             WHEN tio102ods.orr > '1845';
000630140403               tio102ods.orr = '1845';
000631140403           ENDSL;
000632140403         ENDIF;
000633140401
000634140401       ENDIF;
000635140401
000636140401       // Controllo i dati.
000637140401
000638140321       IF Conferma(THIS_INSERIRE_ORM_NO) < THIS_ESITO_OK;
000639140213         RETURN;
000640140213       ENDIF;
000641140306
000642140402       tio102ods.por = %EDITC(fiora0o0.vaoPor : 'X');
000643140402       tio102ods.poe = %EDITC(fiora0o0.vaoPoe : 'X');
000644140307       tio102ods.fre = fiora0o0.ritiroEst; // Ritiro all'estero
000645140307       tio102ods.ose = fiora0o0.forzaNtw; // Forzatura network estero
000646140217       tio102ods.ppg = fiora0o0.prepagato; // Prepagato
000647140508       tio102ods.avrEma = fiora0o0.alertMail;
000648140508       tio102ods.avrSms = fiora0o0.alertSms;
000656140213
000657140210       // Ci sono degli errori?
000658140210
000659140210       IF fiora0m0.nrMsg > *ZERO;
000660140210
000661140210         FOR this.m = 1 TO fiora0m0.nrMsg;
000662140210
000663140210           this.errWarn = %SUBST(fiora0m0.errWarn : this.m);
000664140312           this.idCampo = %SUBST(fiora0m0.idCampo : this.m * 10 - 9);
000665140210
000666140310           // Mi interessano solo gli errori relativi ai campi del primo form.
000667140310
000668140310           IF this.errWarn = FIORA00_MSG_ERRORE AND
000669140310           (   this.idCampo = 'IDLINGUA' OR this.idCampo = 'VAOCOR'
000670140310            OR this.idCampo = 'VAOCRA' OR this.idCampo = 'VAORSR'
000671140310            OR this.idCampo = 'VAOINR' OR this.idCampo = 'VAOCAR'
000672140310            OR this.idCampo = 'VAOLOR' OR this.idCampo = 'VAOPRR'
000673140310            OR this.idCampo = 'VAONAR' OR this.idCampo = 'VAOPAG'
000674140424            OR this.idCampo = 'CODFISCALE' OR this.idCampo = 'PARTITAIVA'
000675140424            OR this.idCampo = 'CONTATTOT' OR this.idCampo = 'MAIL'
000676140424            OR this.idCampo = 'SMS'
000677140424           );
000678140310             this.textMsg = %SUBST(fiora0m0.textMsg : this.m * 255 - 254);
000679140310             this.idMsg = %SUBST(fiora0m0.idMsg : this.m * 7 - 6);
000680140310             this.e += 1;
000681140310             tio102ods.errAry(this.e) = *ON;
000682140310             tio102ods.msgAry(this.e) = this.textMsg;
000683140310             IF tio102ods.msgAry(this.e) = *BLANK;
000684140310               tio102ods.msgAry(this.e) = this.idCampo + ' ' + this.idMsg;
000685140310             ENDIF;
000686140310             IF this.e = %ELEM(tio102ods.errAry);
000687140310               LEAVE;
000688140310             ENDIF;
000689140310           ENDIF;
000690140210
000691140210         ENDFOR;
000692140210
000693140210       ENDIF;
000694140307
000695140307       IF this.e > *ZERO;
000696140403
000697140307         rpyOpc = RPYOPC_ERRORE;
000698140403
000699140403         // Se ho ricevuto un indirizzo codificato, pulisco i dati perché il
000700140403         // form per il ritiro non codificato deve essere vuoto.
000701140403
000702140403         IF tio102ids.cro <> *BLANK;
000703140403           CLEAR tio102ods.rsr;
000704140403           CLEAR tio102ods.inr;
000705140403           CLEAR tio102ods.car;
000706140403           CLEAR tio102ods.lor;
000707140403           CLEAR tio102ods.prr;
000708140403           CLEAR tio102ods.nar;
000709140403         ENDIF;
000710140403
000711140307       ELSE;
000712140403
000713140402         rpyOpc = RPYOPC_DONE;
000714140403
000715140403         // Restituisco l'indirizzo di ritiro normalizzato.
000716140403
000717140403         IF fiora0o0.vaoRsr <> *BLANK;
000718140403           tio102ods.rsr = fiora0o0.vaoRsr;
000719140403           tio102ods.inr = fiora0o0.vaoInr;
000720140403           tio102ods.car = fiora0o0.vaoCar;
000721140403           tio102ods.lor = fiora0o0.vaoLor;
000722140403           tio102ods.prr = fiora0o0.vaoPrr;
000723140403           tio102ods.nar = fiora0o0.vaoNar;
000724140403         ENDIF;
000725140403
000726140307       ENDIF;
000727140210
000728140210       // Se arrivo qui vuol dire che è andato tutto bene ... forse ...
000729140210
000730140210       rpyDta = tio102ods;
000731140210       rpyLen = %SIZE(tio102ods);
000732140210
000733140210       RETURN;
000734140210
000735140210      /END-FREE
000736140210     P ChkParte1       E
000737140211
000738140211     P*--------------------------------------------------
000739140211     P* Procedure name: ChkParte2
000740140211     P* Purpose:        Controllo 2 di 2.
000741140211     P* Returns:        Esito.
000742140211     P*--------------------------------------------------
000743140211     P ChkParte2       B
000744140211     D ChkParte2       PI
000745140211
000746140213     D tio107ids     E DS                  QUALIFIED PREFIX('':4) STATIC
000747170619     D                                     EXTNAME('STRATEDEV/TIO107IDS')
000748140402     D  pkg1                          6A   OVERLAY(pkg)
000749140402     D  pkg2                          1A   OVERLAY(pkg:7)
000750140402     D  vlm1                          2A   OVERLAY(vlm)
000751140402     D  vlm2                          3A   OVERLAY(vlm:3)
000752140213     D tio107ods     E DS                  QUALIFIED PREFIX('':4) STATIC INZ
000753170619     D                                     EXTNAME('STRATEOBJ/TIO107ODS')
000754140213     D  errAry                        1A   DIM(10) OVERLAY(err) INZ(*OFF)
000755140213     D  msgAry                       80A   DIM(10) OVERLAY(msg)
000756140211
000757140211      /FREE
000758140211
000759140211       tio107ids = %SUBST(rqsDta : 1 : rqsLen);
000760140211       RESET tio107ods;
000899140213       RESET tio110ids;
000900140213       EVAL-CORR tio110ids = tio107ids;
000923140213
000924140321       IF Conferma(THIS_INSERIRE_ORM_NO) < THIS_ESITO_OK;
000925140213         RETURN;
000926140213       ENDIF;
000927140402
000928140402       // Controllo ORM eseguito.
000929140402
000930140415       tio107ods.no1 = fiora0o0.vaoNo1;
000931151104       tio107ods.no2 = fiora0o0.vaoNo2;
000932140402
000933140402       // Peso numerico
000934140402
000935140402       IF tio107ids.pkg <> *BLANK;
000936140402         IF tio107ids.pkg1 = *BLANK;
000937140402           tio107ids.pkg1 = *ZERO;
000938140402         ENDIF;
000939140402         IF tio107ids.pkg2 = *BLANK;
000940140402           tio107ids.pkg2 = *ZERO;
000941140402         ENDIF;
000942140402         MONITOR;
000943140402           tio107ids.pkg1 = %EDITC(%DEC(tio107ids.pkg1 : 6 : 0) : 'X');
000944140402           tio107ids.pkg2 = %EDITC(%DEC(tio107ids.pkg2 : 1 : 0) : 'X');
000945140402           tio107ods.pk2 = %DEC(tio107ids.pkg : 7 : 0) / 10;
000946140402         ON-ERROR;
000947140402         ENDMON;
000948140402       ENDIF;
000949140402
000950140402       // Volume numerico
000951140402
000952140402       IF tio107ids.vlm <> *BLANK;
000953140402         IF tio107ids.vlm1 = *BLANK;
000954140402           tio107ids.vlm1 = *ZERO;
000955140402         ENDIF;
000959140505         tio107ids.vlm2 = %TRIML(tio107ids.vlm2);
000960140505         tio107ids.vlm2 = %XLATE(' ' : '0' : tio107ids.vlm2);
000961140402         MONITOR;
000962140402           tio107ids.vlm1 = %EDITC(%DEC(tio107ids.vlm1 : 2 : 0) : 'X');
000963140402           tio107ids.vlm2 = %EDITC(%DEC(tio107ids.vlm2 : 3 : 0) : 'X');
000964140402           tio107ods.vl2 = %DEC(tio107ids.vlm : 5 : 0) / 1000;
000965140402         ON-ERROR;
000966140402         ENDMON;
000967140402       ENDIF;
000968140213
000969140213       // Filiali coinvolte.
000970140213
000971140402       tio107ods.pr2 = %EDITC(fiora0o0.vaoPor : 'X');
000972140402       tio107ods.pe2 = %EDITC(fiora0o0.vaoPoe : 'X');
000973140217
000974140211       // Ci sono degli errori?
000975140211
000976140211       IF fiora0m0.nrMsg > *ZERO;
000977140211
000978140211         FOR this.m = 1 TO fiora0m0.nrMsg;
000979140211
000980140211           this.errWarn = %SUBST(fiora0m0.errWarn : this.m);
000981140211
000982140307           IF this.errWarn = FIORA00_MSG_ERRORE;
000983140310             this.textMsg = %SUBST(fiora0m0.textMsg : this.m * 255 - 254);
000984140310             this.idCampo = %SUBST(fiora0m0.idCampo : this.m * 10 - 9);
000985140310             this.idMsg = %SUBST(fiora0m0.idMsg : this.m * 7 - 6);
000986140212             this.e += 1;
000987140213             tio107ods.errAry(this.e) = *ON;
000988140213             tio107ods.msgAry(this.e) = this.textMsg;
000989140213             IF tio107ods.msgAry(this.e) = *BLANK;
000990140213               tio107ods.msgAry(this.e) = this.idCampo + ' ' + this.idMsg;
000991140213             ENDIF;
000992140213             IF this.e = %ELEM(tio107ods.errAry);
000993140212               LEAVE;
000994140212             ENDIF;
000995140211           ENDIF;
000996140211
000997140211         ENDFOR;
000998140211
000999140211       ENDIF;
001000140307
001001140307       IF this.e > *ZERO;
001002140307         rpyOpc = RPYOPC_ERRORE;
001003140402       ELSE;
001004140402         rpyOpc = RPYOPC_DONE;
001005140307       ENDIF;
001006140211
001007140211       // Se arrivo qui vuol dire che è andato tutto bene ... forse ...
001008140211
001009140211       rpyDta = tio107ods;
001010140211       rpyLen = %SIZE(tio107ods);
001011140212
001012140211       RETURN;
001013140211
001014140211      /END-FREE
001015140211     P ChkParte2       E
001371140220
001372140220
001373140220     P*--------------------------------------------------
001374140220     P* Procedure name: CheckImm
001375140220     P* Purpose: Inserimento ORM veloce.
001376140220     P* Returns:
001377140220     P*--------------------------------------------------
001378140220     P CheckImm        B
001379140220     D CheckImm        PI
001380140220
001381140220     D tio121ids     E DS                  QUALIFIED PREFIX('':4) STATIC
001382170619     D                                     EXTNAME('STRATEOBJ/TIO121IDS')
001383140220     D tio121ods     E DS                  QUALIFIED PREFIX('':4) STATIC INZ
001384170619     D                                     EXTNAME('GAITRAOBJ/TIO121ODS')
001385140220     D  errAry                        1A   DIM(10) OVERLAY(err) INZ(*OFF)
001386140220     D  msgAry                       80A   DIM(10) OVERLAY(msg)
001388140220
001389140220      /FREE
001390140220
001391140220       tio121ids = %SUBST(rqsDta : 1 : rqsLen);
001392140220       RESET tio121ods;
001393140401       EVAL-CORR tio121ods = tio121ids;
001394140220       RESET tio110ids;
001395140220       EVAL-CORR tio110ids = tio121ids;
001396140220       tio110ids.crc = tio121ids.cde;
001397140220
001398140321       IF Conferma(THIS_INSERIRE_ORM_SI) < THIS_ESITO_OK;
001399140220         RETURN;
001400140220       ENDIF;
001401140220
001402140220       // Controllo ORM eseguito.
001403140220
001404140220       EVAL-CORR tio121ods = tio110ods;
001405140220       tio121ods.errAry(1) = tio110ods.err;
001406140220       tio121ods.msgAry(1) = tio110ods.msg;
001407140220       tio121ods.poe = %EDITC(tio110ods.pe2 : 'X');
001408140220       tio121ods.por = %EDITC(tio110ods.pr2 : 'X');
001410140220       tio121ods.no1 = fiora0o0.vaoNo1;
001411151104       tio121ods.no2 = fiora0o0.vaoNo2;
001413160405       tio121ods.porDes = tio110ods.loc;
001414140220       tio121ods.porFon = tio110ods.tel;
001415160405       tio121ods.porEma = %TRIMR(tio110ods.emaFil) + '@brt.it';
001416160405       tio121ods.urlFil = tio110ods.urlFil;
001417160316       tio121ods.npr = tio110ods.npr;
001418160405       tio121ods.scOra1 = tio110ods.scOra1;
001419160405       tio121ods.scOra2 = tio110ods.scOra2;
001420160405       tio121ods.scOra3 = tio110ods.scOra3;
001421160405       tio121ods.scOra4 = tio110ods.scOra4;
001422160405       tio121ods.oraRiI = tio110ods.oraRiI;
001423160405       tio121ods.oraRiF = tio110ods.oraRiF;
001424140220
001425140220       // Ci sono degli errori?
001426140220
001427140220       IF fiora0m0.nrMsg > *ZERO;
001428140220
001429140220         FOR this.m = 1 TO fiora0m0.nrMsg;
001430140220
001432140220           this.errWarn = %SUBST(fiora0m0.errWarn : this.m);
001433140220
001434140307           IF this.errWarn = FIORA00_MSG_ERRORE;
001435140310             this.textMsg = %SUBST(fiora0m0.textMsg : this.m * 255 - 254);
001436140310             this.idCampo = %SUBST(fiora0m0.idCampo : this.m * 10 - 9);
001437140310             this.idMsg = %SUBST(fiora0m0.idMsg : this.m * 7 - 6);
001438140220             this.e += 1;
001439140220             tio121ods.errAry(this.e) = *ON;
001440140220             tio121ods.msgAry(this.e) = this.textMsg;
001441140220             IF tio121ods.msgAry(this.e) = *BLANK;
001442140220               tio121ods.msgAry(this.e) = this.idCampo + ' ' + this.idMsg;
001443140220             ENDIF;
001444140220             IF this.e = %ELEM(tio121ods.errAry);
001445140220               LEAVE;
001446140220             ENDIF;
001447140220           ENDIF;
001448140220
001449140220         ENDFOR;
001450140220
001451140220       ENDIF;
001452140307
001453140307       IF this.e > *ZERO;
001454140307         rpyOpc = RPYOPC_ERRORE;
001455140402       ELSE;
001456140402         rpyOpc = RPYOPC_DONE;
001457140307       ENDIF;
001458140220
001459140220       // Se arrivo qui vuol dire che è andato tutto bene ... forse ...
001460140220
001461140220       rpyDta = tio121ods;
001462140220       rpyLen = %SIZE(tio121ods);
001463140220
001464140220       RETURN;
001731140220
001732140220      /END-FREE
001733140220     P CheckImm        E
001734140220
001735140220
001736140220     P*--------------------------------------------------
001737140220     P* Procedure name: Conferma
001738140220     P* Purpose:
001739140220     P* Returns:
001740140220     P*--------------------------------------------------
001741140220     P Conferma        B
001742140220     D Conferma        PI            10I 0
001743140220     D  prmInserireOrm...
001744140220     D                                 N   CONST
001745140220
001746140515     D Tio712r         PR                  EXTPGM('TIO712R')
001747140220     D  tio110ids                          LIKEDS(tio110ids)
001748140220     D  tio110ods                          LIKEDS(tio110ods)
001749170621     D  oraEstesa                     1A   OPTIONS(*NOPASS)
001750140220
001751140220      /FREE
001752140220
001753140220       IF rqsOpc = RQSOPC_CONFERMA;
001754151106         RESET tio110ids;
001755151106         tio110ids = %SUBST(rqsDta : 1 : rqsLen);
001756140220       ENDIF;
001757140220
001758140220       RESET tio110ods;
001759140401       EVAL-CORR tio110ods = tio110ids;
001760140220
001761140220       fiora0i0.idLingua = tio110ids.lang;
001762140220       fiora0i0.idUtente = tio110ids.rqsCid;
001763140220       fiora0i0.vaoDao = %DEC(%DATE() : *ISO);
001764140220       fiora0i0.vaoOao = %DEC(%TIME() : *ISO);
001765140220
001766140220       IF tio110ids.ksu <> *BLANK;
001767140220         MONITOR;
001768140220           fiora0i0.vaoCor =
001769140220                           %DEC(%SUBST(tio110ids.ksu : 2) + '000' : 10 : 0);
001770140220         ON-ERROR;
001771140220           rpyOpc = RPYOPC_OTHERERROR;
001772140220           rp1error.errCode = 'COR';
001773140220           rp1error.errText = 'Il codice cliente ordinante +
001774140220                               contiene dati non validi.';
001775140220           rpyDta = rp1error;
001776140220           rpyLen = %SIZE(rp1error);
001777140220           RETURN THIS_ESITO_ERRORE;
001778140220         ENDMON;
001779140220       ENDIF;
001780140317
001781140317       IF tio110ids.cro = *BLANK;
001782140317         tio110ids.cro = *ZERO;
001783140317       ENDIF;
001784140220
001785140317       MONITOR;
001786140317         fiora0i0.vaoCra = %DEC(tio110ids.cro : 10 : 0);
001787140317       ON-ERROR;
001788140317         rpyOpc = RPYOPC_OTHERERROR;
001789140317         rp1error.errCode = 'CRO';
001790140317         rp1error.errText = 'Il codice cliente ritiro +
001791140317                             contiene dati non validi.';
001792140317         rpyDta = rp1error;
001793140317         rpyLen = %SIZE(rp1error);
001794140317         RETURN THIS_ESITO_ERRORE;
001795140317       ENDMON;
001796140220
001797140220       // Mittente.
001798140220
001799140220       fiora0i0.vaoRsr = tio110ids.rsr;
001800140220       fiora0i0.vaoInr = tio110ids.inr;
001801140220       fiora0i0.vaoCar = tio110ids.car;
001802140220       fiora0i0.vaoLor = tio110ids.lor;
001803140220       fiora0i0.vaoPrr = tio110ids.prr;
001804140220       fiora0i0.vaoNar = tio110ids.nar;
001805140220
001806140220       // Ordinante anonimo.
001807140220
001808140220       fiora0i0.codFiscale = tio110ids.cfo;
001809140220       fiora0i0.partitaIva = tio110ids.pio;
001810140220
001811140220       // Destinatario.
001812140220
001813140220       IF tio110ids.crc <> *BLANK;
001814140220         MONITOR;
001815140220           fiora0i0.vaoCrc = %DEC(tio110ids.crc : 10 : 0);
001816140220         ON-ERROR;
001817140220           rpyOpc = RPYOPC_OTHERERROR;
001818140220           rp1error.errCode = 'CRC';
001819140220           rp1error.errText = 'Il codice cliente destinatario +
001820140220                               contiene dati non validi.';
001821140220           rpyDta = rp1error;
001822140220           rpyLen = %SIZE(rp1error);
001823140220           RETURN THIS_ESITO_ERRORE;
001824140220         ENDMON;
001825140220       ENDIF;
001826140220
001827140220       fiora0i0.vaoRsc = tio110ids.rsc;
001828140220       fiora0i0.vaoInc = tio110ids.inc;
001829140220       fiora0i0.vaoCac = tio110ids.cac;
001830140220       fiora0i0.vaoLoc = tio110ids.loc;
001831140220       fiora0i0.vaoPrc = tio110ids.prc;
001832140220       fiora0i0.vaoNac = tio110ids.nac;
001833140220
001834140220       // Data ritiro
001835140220
001836140220       IF tio110ids.dar <> *BLANK;
001837140220         MONITOR;
001838140220           fiora0i0.vaoDar = %DEC(tio110ids.dar : 8 : 0);
001839140220           fiora0i0.vaoDar = %DEC(%DATE(fiora0i0.vaoDar : *EUR) : *ISO);
001840140220         ON-ERROR;
001841140220           rpyOpc = RPYOPC_OTHERERROR;
001842140220           rp1error.errCode = 'DAR';
001843160318           rp1error.errText = 'La data ritiro "' + tio110ids.dar
001844160318                            + '" contiene dati non validi.';
001845140220           rpyDta = rp1error;
001846140220           rpyLen = %SIZE(rp1error);
001847140220           RETURN THIS_ESITO_ERRORE;
001848140220         ENDMON;
001849140220       ENDIF;
001850140220
001851140220       // Ora ritiro
001852140220
001853140220       IF tio110ids.orr <> *BLANK;
001854140220         MONITOR;
001855140220           fiora0i0.vaoOrr = %DEC(tio110ids.orr : 4 : 0);
001856140220         ON-ERROR;
001857140220           rpyOpc = RPYOPC_OTHERERROR;
001858140220           rp1error.errCode = 'ORR';
001859160318           rp1error.errText = 'La ora ritiro "' + tio110ids.orr
001860160318                            + '" contiene dati non validi.';
001861140220           rpyDta = rp1error;
001862140220           rpyLen = %SIZE(rp1error);
001863140220           RETURN THIS_ESITO_ERRORE;
001864140220         ENDMON;
001865140220       ENDIF;
001866140220
001867140220       // Numero colli
001868140220
001869140220       IF tio110ids.ncl <> *BLANK;
001870140220         MONITOR;
001871140220           fiora0i0.vaoNcl = %DEC(tio110ids.ncl : 5 : 0);
001872140220         ON-ERROR;
001873140220         ENDMON;
001874140220       ENDIF;
001875140220
001876140220       // Peso
001877140220
001878140225       IF tio110ids.pkg <> *BLANK;
001879140225         IF tio110ids.pkg1 = *BLANK;
001880140225           tio110ids.pkg1 = *ZERO;
001881140225         ENDIF;
001882140225         IF tio110ids.pkg2 = *BLANK;
001883140225           tio110ids.pkg2 = *ZERO;
001884140225         ENDIF;
001885140225         MONITOR;
001886140225           tio110ids.pkg1 = %EDITC(%DEC(tio110ids.pkg1 : 6 : 0) : 'X');
001887140225           tio110ids.pkg2 = %EDITC(%DEC(tio110ids.pkg2 : 1 : 0) : 'X');
001888140225           fiora0i0.vaoPkg = %DEC(tio110ids.pkg : 7 : 0) / 10;
001889140220         ON-ERROR;
001890140220         ENDMON;
001891140220       ENDIF;
001892140220
001893140220       // Volume
001894140220
001895140220       IF tio110ids.vlm <> *BLANK;
001896140225         IF tio110ids.vlm1 = *BLANK;
001897140225           tio110ids.vlm1 = *ZERO;
001898140225         ENDIF;
001901140506         tio110ids.vlm2 = %TRIML(tio110ids.vlm2);
001902140506         tio110ids.vlm2 = %XLATE(' ' : '0' : tio110ids.vlm2);
001903140220         MONITOR;
001904140225           tio110ids.vlm1 = %EDITC(%DEC(tio110ids.vlm1 : 2 : 0) : 'X');
001905140225           tio110ids.vlm2 = %EDITC(%DEC(tio110ids.vlm2 : 3 : 0) : 'X');
001906140225           fiora0i0.vaoVlm = %DEC(tio110ids.vlm : 5 : 0) / 1000;
001907140220         ON-ERROR;
001908140220         ENDMON;
001909140220       ENDIF;
001910140220
001911140220       // Bancali
001912140220
001913140220       IF tio110ids.bnc <> *BLANK;
001914140220         MONITOR;
001915140220           fiora0i0.vaoBnc = %DEC(tio110ids.bnc : 5 : 0);
001916140220         ON-ERROR;
001917140220         ENDMON;
001918140220       ENDIF;
001919140220
001920140220       // Orari apertura: mattino dalle
001921140220
001922140220       IF tio110ids.ad1 <> *BLANK;
001923140220         MONITOR;
001924140220           fiora0i0.dalleAM = %DEC(tio110ids.ad1 : 4 : 0);
001925140220         ON-ERROR;
001926140220           rpyOpc = RPYOPC_OTHERERROR;
001927140220           rp1error.errCode = 'AD1';
001928160318           rp1error.errText = 'Apertura mattino dalle "' + tio110ids.ad1
001929160318                            + '" contiene dati non validi.';
001930140220           rpyDta = rp1error;
001931140220           rpyLen = %SIZE(rp1error);
001932140220           RETURN THIS_ESITO_ERRORE;
001933140220         ENDMON;
001934140220       ENDIF;
001935140220
001936140220       // Orari apertura: mattino alle
001937140220
001938140220       IF tio110ids.aa1 <> *BLANK;
001939140220         MONITOR;
001940140220           fiora0i0.alleAM = %DEC(tio110ids.aa1 : 4 : 0);
001941140220         ON-ERROR;
001942140220           rpyOpc = RPYOPC_OTHERERROR;
001943140220           rp1error.errCode = 'AA1';
001944160318           rp1error.errText = 'Apertura mattino alle "' + tio110ids.aa1
001945160318                            + '" contiene dati non validi.';
001946140220           rpyDta = rp1error;
001947140220           rpyLen = %SIZE(rp1error);
001948140220           RETURN THIS_ESITO_ERRORE;
001949140220         ENDMON;
001950140220       ENDIF;
001951140220
001952140220       // Orari apertura: pomeriggio dalle
001953140220
001954140220       IF tio110ids.ad2 <> *BLANK;
001955140220         MONITOR;
001956140220           fiora0i0.dallePM = %DEC(tio110ids.ad2 : 4 : 0);
001957140220         ON-ERROR;
001958140220           rpyOpc = RPYOPC_OTHERERROR;
001959140220           rp1error.errCode = 'AD2';
001960160318           rp1error.errText = 'Apertura pomeriggio dalle "' + tio110ids.ad2
001961160318                            + '" contiene dati non validi.';
001962140220           rpyDta = rp1error;
001963140220           rpyLen = %SIZE(rp1error);
001964140220           RETURN THIS_ESITO_ERRORE;
001965140220         ENDMON;
001966140220       ENDIF;
001967140220
001968140220       // Orari apertura: pomeriggio alle
001969140220
001970140220       IF tio110ids.aa2 <> *BLANK;
001971140220         MONITOR;
001972140220           fiora0i0.allePM = %DEC(tio110ids.aa2 : 4 : 0);
001973140220         ON-ERROR;
001974140220           rpyOpc = RPYOPC_OTHERERROR;
001975140220           rp1error.errCode = 'AA2';
001976160318           rp1error.errText = 'Apertura pomeriggio alle "' + tio110ids.aa2
001977160318                            + '" contiene dati non validi.';
001978140220           rpyDta = rp1error;
001979140220           rpyLen = %SIZE(rp1error);
001980140220           RETURN THIS_ESITO_ERRORE;
001981140220         ENDMON;
001982140220       ENDIF;
002007140220
002008140220       // Varie
002009140220
002011140220       fiora0i0.vaoNam = tio110ids.nam; // Natura merce
002012140220       fiora0i0.vaoRfa = tio110ids.rfa; // Riferimento ORM
002013140507       fiora0i0.vaoNo1 = tio110ids.no1; // Note 1
002014140220       fiora0i0.vaoNo2 = tio110ids.no2; // Note 2
002015140220       fiora0i0.vaoPag = tio110ids.pag; // Chi paga?
002016140220       fiora0i0.vaoFfd = tio110ids.ffd; // Fermo deposito
002017140220       fiora0i0.vaoRer = tio110ids.rer; // Referente ritiro
002018140220       fiora0i0.vaoTer = tio110ids.ter; // Telefono ritiro
002019140305       fiora0i0.contattoT = tio110ids.cttTlf; // Contatto telefonico
002020140317       fiora0i0.sceltaNtw = tio110ids.sse; // Network estero
002021140220       fiora0i0.unoPiuDest = tio110ids.upd; // 1 o più destinatari.
002022140416       fiora0i0.mail = tio110ids.emaddr;
002024140416       fiora0i0.sms = tio110ids.ntsmsr;
002025140507       fiora0i0.alertMail = tio110ids.avrEma;
002026140507       fiora0i0.alertSms = tio110ids.avrSms;
002027151020       fiora0i0.alertConf = tio110ids.caoInv;
002028151020       fiora0i0.mailConf = tio110ids.caoEma;
002029160318       fiora0i0.smsConf = tio110ids.caoSms;
002030160727       fiora0i0.memDatiAcr = tio110ids.memAcr;
002031140220
002032140418       // Filiale ritiro
002033140418
002034140418       IF tio110ids.por = *BLANK;
002035140418         tio110ids.por = *ZERO;
002036140418       ENDIF;
002037140418
002038140418       MONITOR;
002039140418         fiora0i0.vaoPor = %DEC(tio110ids.por : 3 : 0);
002040140418       ON-ERROR;
002041140418       ENDMON;
002042140418
002043140418       // Filiale emissione
002044140418
002045140418       IF tio110ids.poe = *BLANK;
002046140418         tio110ids.poe = *ZERO;
002047140418       ENDIF;
002048140418
002049140418       MONITOR;
002050140418         fiora0i0.vaoPoe = %DEC(tio110ids.poe : 3 : 0);
002051140418       ON-ERROR;
002052140418       ENDMON;
002053140418
002054140220       // Eseguo controllo dei dati.
002055160217       // Se dopo aver superato i controlli devo inserire l'ORM,
002056160217       // allora chiedo anche la restituzione del numero prenotazione ritiro.
002057140220
002058160217       IF prmInserireOrm;
002059160217         this.rqsOpCode = FIORA00_RQSOPCODE_WRITENPR;
002060160217       ELSE;
002061160217         this.rqsOpCode = FIORA00_RQSOPCODE_CONTROL;
002062160217       ENDIF;
002063160217
002064140220       MONITOR;
002065160318         Fiora00_Immetti( this.rqsOpCode : this.rpyOpCode : this.rpyIdMsg
002067160318                        : fiora0i0.formato : fiora0i0 : %SIZE(fiora0i0)
002070160318                        : fiora0o0.formato : fiora0o0 : %SIZE(fiora0O0)
002073160318                        : fiora0m0.formato : fiora0m0 : %SIZE(fiora0m0)
002076160318                        : fiora0f0.formato : fiora0f0 : %SIZE(fiora0f0) );
002080140220       ON-ERROR;
002081140220         this.rpyOpCode = FIORA00_RPYOPCODE_ERROR;
002082140220       ENDMON;
002083140220
002084140220       // Controllo ORM non eseguito.
002085140220
002086140220       IF this.rpyOpCode < FIORA00_RPYOPCODE_DONE;
002087140220         rpyOpc = RPYOPC_OTHERERROR;
002088140220         rp1error.errCode = %CHAR(this.rpyIdMsg);
002089140220         rp1error.errText = 'Si sono verificati degli errori gravi nella +
002090140220                             chiamata a FIORA00R (errore '
002091140220                             + %CHAR(this.rpyIdMsg) + ').';
002092140220         rpyDta = rp1error;
002093140220         rpyLen = %SIZE(rp1error);
002094140220         RETURN THIS_ESITO_ERRORE;
002095140220       ENDIF;
002096140402
002097140220       // Ci sono degli errori?
002098140220
002099140220       IF fiora0m0.nrMsg > *ZERO;
002100140220         FOR this.m = 1 TO fiora0m0.nrMsg;
002101140220           this.errWarn = %SUBST(fiora0m0.errWarn : this.m);
002102140307           IF this.errWarn = FIORA00_MSG_ERRORE;
002103140310             this.textMsg = %SUBST(fiora0m0.textMsg : this.m * 255 - 254);
002104140310             this.idCampo = %SUBST(fiora0m0.idCampo : this.m * 10 - 9);
002105140310             this.idMsg = %SUBST(fiora0m0.idMsg : this.m * 7 - 6);
002106140220             tio110ods.err = *ON;
002107140220             tio110ods.msg = this.textMsg;
002108140220             IF tio110ods.msg = *BLANK;
002109140220               tio110ods.msg = this.idCampo + ' ' + this.idMsg;
002110140220             ENDIF;
002111140307             rpyOpc = RPYOPC_ERRORE;
002112140220             LEAVE;
002113140220           ENDIF;
002114140220         ENDFOR;
002115141223       ENDIF;
002116141223
002117141223       IF rpyOpc <> RPYOPC_ERRORE; // Non ci sono errori, inserisco l'ORM.
002118140317
002119140317         tio110ids.poe = %EDITC(fiora0o0.vaoPoe : 'X');
002120140317         tio110ids.rsr = fiora0o0.vaoRsr;
002121140317         tio110ids.inr = fiora0o0.vaoInr;
002122140317         tio110ids.car = fiora0o0.vaoCar;
002123140317         tio110ids.lor = fiora0o0.vaoLor;
002124140317         tio110ids.prr = fiora0o0.vaoPrr;
002125140317         tio110ids.nar = fiora0o0.vaoNar;
002126140317         tio110ids.rsc = fiora0o0.vaoRsc;
002127140317         tio110ids.inc = fiora0o0.vaoInc;
002128140317         tio110ids.cac = fiora0o0.vaoCac;
002129140317         tio110ids.loc = fiora0o0.vaoLoc;
002130140317         tio110ids.prc = fiora0o0.vaoPrc;
002131140402         tio110ids.nac = fiora0o0.vaoNac;
002132140317         tio110ids.rer = fiora0o0.vaoRer;
002133140317         tio110ids.ter = fiora0o0.vaoTer;
002134140317         tio110ids.dar = %CHAR(%DATE(fiora0o0.vaoDar : *ISO) : *EUR0);
002135140317         tio110ids.orr = %EDITC(fiora0o0.vaoOrr : 'X');
002136140317         tio110ids.nam = fiora0o0.vaoNam;
002137140317         tio110ids.rfa = fiora0o0.vaoRfa;
002138140317         tio110ids.no1 = fiora0o0.vaoNo1;
002139140317         tio110ids.no2 = fiora0o0.vaoNo2;
002140140317         tio110ids.por = %EDITC(fiora0o0.vaoPor : 'X');
002141140317         tio110ids.cfo = fiora0o0.codFiscale;
002142140317         tio110ids.pio = fiora0o0.partitaIva;
002143140317         tio110ids.sse = fiora0o0.sceltaNtw;
002144140317         tio110ids.ose = fiora0o0.forzaNtw;
002145140317         tio110ids.fre = fiora0o0.ritiroEst;
002146140317         tio110ids.upd = fiora0o0.unoPiuDest;
002147140317         tio110ids.cttTlf = fiora0o0.contattoT;
002148140402         tio110ids.emaddr = fiora0o0.mail;
002149140402         tio110ids.ntsmsr = fiora0o0.sms;
002150140507         tio110ids.avrEma = fiora0o0.alertMail;
002151140507         tio110ids.avrSms = fiora0o0.alertSms;
002152160503         tio110ids.caoInv = fiora0o0.alertConf;
002154160503         tio110ids.caoInS = fiora0o0.alertConfS;
002155140402         tio110ids.pr2 = %EDITC(fiora0o0.vaoPor : 'X');
002156140402         tio110ids.pe2 = %EDITC(fiora0o0.vaoPoe : 'X');
002157160420         tio110ids.ppg = fiora0o0.prePagato;
002158160509         tio110ids.oraRiI = %TIME(%EDITC(fiora0o0.oraRitI:'X') + '00' : *EUR0);
002160160509         tio110ids.oraRiF = %TIME(%EDITC(fiora0o0.oraRitF:'X') + '00' : *EUR0);
002161170619
002162170619         MONITOR;
002163170619           tio110ids.drc = %EDITC(fiora0o0.dataRitiro : 'X');
002164170619         ON-ERROR;
002165170619         ENDMON;
002166170619
002167170619         tio110ids.posd = fiora0o0.posticipo;
002168170619         tio110ids.ant = fiora0o0.anticipa;
002169140220
002170140220         IF prmInserireOrm;
002171140305
002172160217           tio110ids.npr = fiora0o0.npr;
002173160217
002174140220           MONITOR;
002175170621             Tio712r( tio110ids : tio110ods : fiora0O0.oraEstesa );
002176140220           ON-ERROR;
002177140220             tio110ods.err = *ON;
002178140220           ENDMON;
002179140220
002180140220           // Inserimento ORM non riuscito (soccia che sfiga).
002181140220
002182141219           IF tio110ods.err = *ON OR tio110ods.msg = *BLANK;
002183140220             rpyOpc = RPYOPC_OTHERERROR;
002184140220             rp1error.errText = 'Si sono verificati degli errori gravi nella +
002185140515                                 chiamata a TIO712R. ORM non inserito.';
002186140220             rpyDta = rp1error;
002187140220             rpyLen = %SIZE(rp1error);
002188141219             DUMP(A);
002189140220             RETURN THIS_ESITO_ERRORE;
002190140220           ENDIF;
002191160420
002192160509           tio110ods.oraRiI = %TIME(%EDITC(fiora0o0.oraRitI:'X') + '00'
002193160509                                     : *EUR0);
002194160509           tio110ods.oraRiF = %TIME(%EDITC(fiora0o0.oraRitF:'X') + '00'
002195160509                                     : *EUR0);
002196160509
002197160504           MONITOR;
002198160504             tio110ods.scOra1 = %TIME(fiora0o0.oraAmDaPor + '00' : *EUR0);
002199160504             tio110ods.scOra2 = %TIME(fiora0o0.oraAmAPor + '00' : *EUR0);
002200160504             tio110ods.scOra3 = %TIME(fiora0o0.oraPmDaPor + '00' : *EUR0);
002201160504             tio110ods.scOra4 = %TIME(fiora0o0.oraPmAPor + '00' : *EUR0);
002202160504           ON-ERROR;
002203160504             DUMP(A);
002204160504           ENDMON;
002205160504
002206160420           tio110ods.urlFil = fiora0o0.urlPor;
002207160420           tio110ods.emaFil = fiora0o0.mailPor;
002208160420           tio110ods.des = fiora0o0.descrPor;
002209160420           tio110ods.tel = %SCANRPL('/' : '' : fiora0o0.telPor);
002210160422           tio110ods.tel = %SCANRPL('.' : '' : tio110ods.tel);
002211160422           tio110ods.tel = %SCANRPL('-' : '' : tio110ods.tel);
002212140220
002213140220         ENDIF;
002214160420
002215141223       ENDIF;
002216151020
002217140220       // Se arrivo qui vuol dire che è andato tutto bene ... forse ...
002218140220
002219160315       IF rqsOpc = RQSOPC_CONFERMA OR rqsOpc = RQSOPC_CHECKIMM;
002220140220         rpyDta = tio110ods;
002221140220         rpyLen = %SIZE(tio110ods);
002222141222         IF tio110ods.err = *BLANK AND tio110ods.msg = *BLANK;
002223141222           DUMP(A);
002224141222         ENDIF;
002225140220       ENDIF;
002226140220
002227140220       RETURN THIS_ESITO_OK;
002228140220
002229140220      /END-FREE
002230140220     P Conferma        E
002231140212
002232140311
002233140311     P*--------------------------------------------------
002234140311     P* Procedure name: CarCliRit
002235140311     P* Purpose:        Carica i codici clienti ritiro.
002236140311     P* Returns:        Esito.
002237140311     P*--------------------------------------------------
002238140311     P CarCliRit       B
002239140311     D CarCliRit       PI            10I 0
002240140311
002241140404     D retField        S             10I 0 STATIC
002242140311     D fiorb0i0      E DS                  QUALIFIED STATIC INZ(*EXTDFT)
002243140311     D  ordina       E                     INZ(FIORB00_ORDINA_LOC)
002244140312     D  escludiTcr   E                     INZ(FIORB00_ESCLUDI_MAIINTERNET)
002245140313     D fiorb0o0      E DS                  QUALIFIED STATIC INZ(*EXTDFT)
002246140311     D  codiceAry                    10A   DIM(10) OVERLAY(codice)
002247140311     D  nomeAry                      48A   DIM(10) OVERLAY(nome)
002248140311     D  indirizzoAry                 35A   DIM(10) OVERLAY(indirizzo)
002249140311     D  localitaAry                  35A   DIM(10) OVERLAY(localita)
002250140311     D tio103ids     E DS                  QUALIFIED PREFIX('':4) STATIC
002251170619     D                                     EXTNAME('STRATEOBJ/TIO103IDS')
002252140311     D tio103ods     E DS                  QUALIFIED PREFIX('':4) STATIC INZ
002253170619     D                                     EXTNAME('STRATEOBJ/TIO103ODS')
002254140311     D  croAry                       10A   DIM(140) OVERLAY(cro)
002255140311     D  reiAry                       60A   DIM(140) OVERLAY(rei)
002256140311     D
002257140312     D local           DS                  QUALIFIED STATIC INZ
002258140311     D  i                            10I 0
002259140313     D  rqsOpCode                    10I 0 INZ(FIORB00_RQSOPCODE_LEGGI)
002260140311
002261140311      /FREE
002262140311
002263140311       RESET retField;
002264140311       RESET local;
002265140311       RESET tio103ods;
002266140311       RESET fiorb0i0;
002267140313       RESET fiorb0o0;
002268140311
002269140311       tio103ids = %SUBST(rqsDta : 1 : rqsLen);
002270140311       fiorb0i0.codiceKsu = tio103ids.ksu;
002271140311
002272140404       // La prima chiamata la faccio col codice operativo LEGGI che popola la
002273140404       // lista e mi restituisce i primi 10 elementi, poi proseguo con un ciclo
002274140404       // di chiamate col codice operativo CARICA che mi restituisce gruppi di
002275140404       // 10 elementi.
002276140404
002277140313       DOU tio103ods.nrc = fiorb0o0.clientiTot
002278140313        OR fiorb0o0.nrRec = *ZERO
002279140313        OR tio103ods.nrc = %ELEM(tio103ods.croAry);
002280140311
002281140313         MONITOR;
002282140313           Fiorb00_Anagrafica( local.rqsOpCode
002283140313                             : this.rpyOpCode
002284140313                             : this.rpyIdMsg
002285140313                             : fiorb0i0.formato
002286140313                             : fiorb0i0
002287140313                             : %SIZE(fiorb0i0)
002288140313                             : fiorb0o0.formato
002289140313                             : fiorb0o0
002290140313                             : %SIZE(fiorb0o0)
002291140313                             );
002292140313         ON-ERROR;
002293140313           this.rpyOpCode = FIORB00_RPYOPCODE_ERROR;
002294140313         ENDMON;
002295140311
002296140313         IF this.rpyOpCode < FIORB00_RPYOPCODE_DONE;
002297140313           tio103ods.err = *ON;
002298140313           LEAVE;
002299140313         ENDIF;
002300140313
002301140313         IF fiorb0o0.clientiTot = *ZERO OR fiorb0o0.nrRec = *ZERO;
002302140313           LEAVE;
002303140313         ENDIF;
002304140317
002305140317         tio103ods.cttTlf = fiorb0o0.contatto;
002306140311
002307140313         FOR local.i = 1 TO fiorb0o0.nrRec;
002308140313           tio103ods.nrc += 1;
002309140313           tio103ods.croAry(tio103ods.nrc) = fiorb0o0.codiceAry(local.i);
002310140313           tio103ods.reiAry(tio103ods.nrc) =
002311140313                                          %TRIMR(fiorb0o0.localitaAry(local.i))
002312140313                                        + ' '
002313140313                                        + %TRIMR(fiorb0o0.indirizzoAry(local.i))
002314140313                                        + ' '
002315140313                                        + %TRIMR(fiorb0o0.nomeAry(local.i))
002316140313                                        ;
002317140313           IF tio103ods.nrc = %ELEM(tio103ods.croAry);
002318140313             LEAVE;
002319140313           ENDIF;
002320140313         ENDFOR;
002321140311
002322140313         IF tio103ods.nrc = %ELEM(tio103ods.croAry)
002323140313         OR tio103ods.nrc = fiorb0o0.clientiTot;
002324140313           LEAVE;
002325140313         ENDIF;
002326140313
002327140313         local.rqsOpCode = FIORB00_RQSOPCODE_CARICA;
002328140313
002329140313       ENDDO;
002330140312
002331140312       MONITOR;
002332140312         Fiorb00_Anagrafica( FIORB00_RQSOPCODE_CLOSE
002333140312                           : this.rpyOpCode
002334140312                           : this.rpyIdMsg
002335140312                           );
002336140312       ON-ERROR;
002337140312       ENDMON;
002338140311
002339140311       IF tio103ods.err = *ON;
002340140311         rpyOpc = RPYOPC_OTHERERROR;
002341140311         rp1error.errText = 'Si sono verificati degli errori gravi nella +
002342140311                             chiamata a FIORB00R.';
002343140311         rpyDta = rp1error;
002344140311         rpyLen = %SIZE(rp1error);
002345140311         RETURN THIS_ESITO_ERRORE;
002346140311       ENDIF;
002347140317
002348140311       // Se arrivo qui vuol dire che è andato tutto bene ... forse ...
002349140311
002350140311       rpyDta = tio103ods;
002351140311       rpyLen = %SIZE(tio103ods);
002352140311
002353140311       RETURN retField;
002354140311
002355140311      /END-FREE
002356140311     P CarCliRit       E
002357140311
002358140402
002359140402     P*--------------------------------------------------
002360140403     P* Procedure name: GetOraAggiustata
002361140403     P* Purpose:        Restituisce l'ora aggiustata ai 15 minuti.
002362140402     P* Returns:        Ora e minuto aggiustati.
002363140402     P* Parameter:      hhmm => Ora e minuto
002364140402     P*--------------------------------------------------
002365140403     P GetOraAggiustata...
002366140402     P                 B
002367140403     D GetOraAggiustata...
002368140402     D                 PI             4A
002369140402     D  hhmm                          4S 0 CONST
002370140402     D retField        DS             4    QUALIFIED STATIC
002371140402     D  hhmm                   1      4S 0
002372140402     D  hh                     1      2S 0
002373140402     D  mm                     3      4S 0
002374140402
002375140402      /FREE
002376140402
002377140402       IF hhmm = *ZERO;
002378140402         RETURN *BLANK;
002379140402       ENDIF;
002380140402
002381140402       retField.hhmm = hhmm;
002382140402
002383140402       SELECT;
002384140402         WHEN retField.mm = 00 OR retField.mm = 15
002385140402           OR retField.mm = 30 OR retField.mm = 45;
002386140402           // Non è da aggiustare.
002387140402         WHEN retField.mm < 15;
002388140402           retField.mm = 15;
002389140402         WHEN retField.mm < 30;
002390140402           retField.mm = 30;
002391140402         WHEN retField.mm < 45;
002392140402           retField.mm = 45;
002393140402         OTHER;
002394140402           retField.hh += 1;
002395140402           retField.mm = 00;
002396140402       ENDSL;
002397140402
002398140402       RETURN retField;
002399140402
002400140402      /END-FREE
002401140403     P GetOraAggiustata...
002402140402     P                 E
002403140402
002404140404
002405140404     P*--------------------------------------------------
002406140404     P* Procedure name: CarCliRitEstesa
002407140404     P* Purpose:        Restituisce più di 140 anagrafiche di ritiro.
002408140404     P* Returns:        Esito.
002409140404     P*--------------------------------------------------
002410140404     PCarCliRitEstesa  B
002411140404     DCarCliRitEstesa  PI            10I 0
002412140404
002413140404     D retField        S             10I 0 STATIC
002414140404     D fiorb0i0      E DS                  QUALIFIED STATIC INZ(*EXTDFT)
002415140404     D  ordina       E                     INZ(FIORB00_ORDINA_LOC)
002416140404     D  escludiTcr   E                     INZ(FIORB00_ESCLUDI_MAIINTERNET)
002417140404     D fiorb0o0      E DS                  QUALIFIED STATIC INZ(*EXTDFT)
002418140404     D  codiceAry                    10A   DIM(10) OVERLAY(codice)
002419140404     D  nomeAry                      48A   DIM(10) OVERLAY(nome)
002420140404     D  indirizzoAry                 35A   DIM(10) OVERLAY(indirizzo)
002421140404     D  localitaAry                  35A   DIM(10) OVERLAY(localita)
002422140404     D tio103ids     E DS                  QUALIFIED PREFIX('':4) STATIC
002423170619     D                                     EXTNAME('STRATEOBJ/TIO103IDS')
002424160318     D tio103ods     E DS                  QUALIFIED PREFIX('':4) STATIC INZ
002425170619     D                                     EXTNAME('STRATEOBJ/TIO103ODS')
002426140404     D  croAry                       10A   DIM(140) OVERLAY(cro)
002427140404     D  reiAry                       60A   DIM(140) OVERLAY(rei)
002428140404     D local           DS                  QUALIFIED STATIC INZ
002429140404     D  i                            10I 0
002430140404     D  l                            10I 0
002431140404     D  f                            10I 0
002432140404     D  rqsOpCode                    10I 0 INZ(FIORB00_RQSOPCODE_LEGGI)
002433140404     D luoghi          DS                  QUALIFIED STATIC
002434140404     D  cro                          10A   DIM(980)
002435140404     D  rei                          60A   DIM(980)
002436140404     D usrSpc          DS                  QUALIFIED STATIC
002437140404     D  name                         10A
002438140404     D  library                      10A   INZ(GAITRATMP)
002439140404
002440140404      /FREE
002441140404
002442140404       RESET retField;
002443140404       RESET local;
002444140404       RESET tio103ods;
002445140404       RESET luoghi;
002446140404       RESET usrSpc;
002447140404
002448140404       tio103ids = %SUBST(rqsDta : 1 : rqsLen);
002449140404
002450140404       IF tio103ids.usrSpc = *BLANK;
002451140404
002452140404         RESET fiorb0i0;
002453140404         RESET fiorb0o0;
002454140404         fiorb0i0.codiceKsu = tio103ids.ksu;
002455140404
002456140404         // La prima chiamata la faccio col codice operativo LEGGI che popola la
002457140404         // lista e mi restituisce i primi 10 elementi, poi proseguo con un ciclo
002458140404         // di chiamate col codice operativo CARICA che mi restituisce gruppi di
002459140404         // 10 elementi.
002460140404
002461140404         DOU local.l = fiorb0o0.clientiTot
002462140404          OR fiorb0o0.nrRec = *ZERO
002463140404          OR local.l = %ELEM(luoghi.cro);
002464140404
002465140404           MONITOR;
002466140404             Fiorb00_Anagrafica( local.rqsOpCode
002467140404                               : this.rpyOpCode
002468140404                               : this.rpyIdMsg
002469140404                               : fiorb0i0.formato
002470140404                               : fiorb0i0
002471140404                               : %SIZE(fiorb0i0)
002472140404                               : fiorb0o0.formato
002473140404                               : fiorb0o0
002474140404                               : %SIZE(fiorb0o0)
002475140404                               );
002476140404           ON-ERROR;
002477140404             this.rpyOpCode = FIORB00_RPYOPCODE_ERROR;
002478140404           ENDMON;
002479140404
002480140404           IF this.rpyOpCode < FIORB00_RPYOPCODE_DONE;
002481140404             tio103ods.err = *ON;
002482140404             LEAVE;
002483140404           ENDIF;
002484140404
002485140404           IF fiorb0o0.clientiTot = *ZERO OR fiorb0o0.nrRec = *ZERO;
002486140404             LEAVE;
002487140404           ENDIF;
002488140404
002489140408           IF local.rqsOpCode = FIORB00_RQSOPCODE_LEGGI;
002490140408             tio103ods.cttTlf = fiorb0o0.contatto;
002491151019             tio103ods.caoEma = fiorb0o0.mailConf;
002492160318             tio103ods.caoSms = fiorb0o0.smsConf;
002493151019             IF fiorb0o0.alertConf = *BLANK;
002494151019               tio103ods.caoInv = 'N';
002495151019             ELSE;
002496151019               tio103ods.caoInv = fiorb0o0.alertConf;
002497151019             ENDIF;
002498151019           ENDIF;
002499140404
002500140404           FOR local.i = 1 TO fiorb0o0.nrRec;
002501140404             local.l += 1;
002502140404             luoghi.cro(local.l) = fiorb0o0.codiceAry(local.i);
002503140404             luoghi.rei(local.l) = %TRIMR(fiorb0o0.localitaAry(local.i))
002504140404                                 + ' '
002505140404                                 + %TRIMR(fiorb0o0.indirizzoAry(local.i))
002506140404                                 + ' '
002507140404                                 + %TRIMR(fiorb0o0.nomeAry(local.i))
002508140404                                 ;
002509140404             IF local.l = %ELEM(luoghi.cro);
002510140404               LEAVE;
002511140404             ENDIF;
002512140404           ENDFOR;
002513140404
002514140404           IF local.l = %ELEM(luoghi.cro)
002515140404           OR local.l = fiorb0o0.clientiTot;
002516140404             LEAVE;
002517140404           ENDIF;
002518140404
002519140404           local.rqsOpCode = FIORB00_RQSOPCODE_CARICA;
002520140404
002521140404         ENDDO;
002522140404
002523140404         MONITOR;
002524140404           Fiorb00_Anagrafica( FIORB00_RQSOPCODE_CLOSE
002525140404                             : this.rpyOpCode
002526140404                             : this.rpyIdMsg
002527140404                             );
002528140404         ON-ERROR;
002529140404         ENDMON;
002530140404
002531140404         IF tio103ods.err = *ON;
002532140404           rpyOpc = RPYOPC_OTHERERROR;
002533140404           rp1error.errText = 'Si sono verificati degli errori gravi nella +
002534140404                               chiamata a FIORB00R.';
002535140404           rpyDta = rp1error;
002536140404           rpyLen = %SIZE(rp1error);
002537140404           RETURN THIS_ESITO_ERRORE;
002538140404         ENDIF;
002539140404
002540140404         // Ho più di 140 elementi, posteggio tutto in uno user space.
002541140404
002542140404         IF local.l > %ELEM(tio103ods.croAry);
002543140404
002544140404           TrulTmpN( '*USRSPC' : *BLANK : usrSpc.library : usrSpc.name );
002545140404
002546140404           IF %SUBST(usrSpc.name : 1 : 1) = '*';
002547140404             DUMP(A);
002548140404             RETURN THIS_ESITO_ERRORE;
002549140404           ENDIF;
002550140404
002551140404           CLEAR QUSEC;
002552140404           QUSBPRV = %SIZE(QUSEC);
002553140404
002554140404           CreateUserSpace( usrSpc
002555140404                          : 'LUOGHI'
002556140404                          : %SIZE(luoghi)
002557140404                          : X'00'
002558140404                          : '*ALL'
002559140404                          : %SUBST(rqsDta : 1 : 50)
002560140404                          : '*NO'
002561140404                          : QUSEC
002562140404                          );
002563140404
002564140404           IF QUSEI <> *BLANK;
002565140404             DUMP(A);
002566140404             RETURN THIS_ESITO_ERRORE;
002567140404           ENDIF;
002568140404
002569140404           CLEAR QUSEC;
002570140404           QUSBPRV = %SIZE(QUSEC);
002571140404
002572140404           ChangeUserSpace( usrSpc
002573140404                          : 1
002574140404                          : %SIZE(luoghi)
002575140404                          : luoghi
002576140404                          : '0'
002577140404                          : QUSEC
002578140404                          );
002579140404
002580140404           IF QUSEI <> *BLANK;
002581140404             DUMP(A);
002582140404             RETURN THIS_ESITO_ERRORE;
002583140404           ENDIF;
002584140404
002585140404           tio103ods.usrSpc = usrSpc.name;
002586140404
002587140404         ENDIF;
002588140404
002589140404       ELSE; // Ho ricevuto il nome dello user space.
002590140404
002591140408         tio103ods.eodata = *OFF;
002592140404         tio103ods.usrSpc = tio103ids.usrSpc;
002593140404
002594140404         CLEAR QUSEC;
002595140404         QUSBPRV = %SIZE(QUSEC);
002596140404
002597140404         RetrieveUserSpace( tio103ids.usrSpc + usrSpc.library
002598140404                          : 1
002599140404                          : %SIZE(luoghi)
002600140404                          : luoghi
002601140404                          : QUSEC
002602140404                          );
002603140404
002604140404         IF QUSEI <> *BLANK;
002605140404           DUMP(A);
002606140404           RETURN THIS_ESITO_ERRORE;
002607140404         ENDIF;
002608140404
002609140404       ENDIF;
002610140404
002611140404       // Cerco l'ultimo elemento pieno.
002612140404
002613140404       local.f = %LOOKUP(*BLANK : luoghi.cro);
002614140404
002615140404       IF local.f = *ZERO;
002616140407         local.f = %ELEM(luoghi.cro);
002617140407       ELSE;
002618140407         local.f -= 1;
002619140404       ENDIF;
002620140404
002621140404       RESET local.i;
002622140404
002623140404       // Restituisco 140 elementi per volta.
002624140404
002625140404       FOR local.l = (tio103ids.prgEle + 1) TO local.f;
002626140404         local.i += 1;
002627140404         tio103ods.croAry(local.i) = luoghi.cro(local.l);
002628140404         tio103ods.reiAry(local.i) = luoghi.rei(local.l);
002629140404         IF local.l = local.f;
002630140404           tio103ods.eodata = *ON;
002631140404           LEAVE;
002632140404         ENDIF;
002633140404         IF local.i = %ELEM(tio103ods.croAry);
002634140404           LEAVE;
002635140404         ENDIF;
002636140404       ENDFOR;
002637140404
002638140404       tio103ods.nrc = local.i;
002639140404       tio103ods.prgEle = local.l;
002640140404
002641140407       IF tio103ods.eodata = *ON AND tio103ids.usrSpc <> *BLANK;
002642140404         CLEAR QUSEC;
002643140404         QUSBPRV = %SIZE(QUSEC);
002644140407         DeleteUserSpace(tio103ids.usrSpc + usrSpc.library : QUSEC);
002645140404         CLEAR tio103ods.usrSpc;
002646140404       ENDIF;
002647140404
002648140404       // Se arrivo qui vuol dire che è andato tutto bene ... forse ...
002649140404
002650140404       rpyDta = tio103ods;
002651140404       rpyLen = %SIZE(tio103ods);
002652140404
002653140404       RETURN retField;
002654140404
002655140404      /END-FREE
002656140404     PCarCliRitEstesa  E
002657140404
002658140407
002659140407     P*--------------------------------------------------
002660140407     P* Procedure name: GetDateRitiro
002661140407     P* Purpose:        Restituisce le date di ritiro ammesse.
002662140407     P* Returns:        Esito.
002663140407     P*--------------------------------------------------
002664140407     P GetDateRitiro   B
002665140407     D GetDateRitiro   PI            10I 0
002666140407
002667140407     D retField        S             10I 0 STATIC
002668140407     D tio10800i     E DS                  QUALIFIED INZ(*EXTDFT) STATIC
002669170619     D                                     EXTNAME('STRATEOBJ/TIO10800I')
002670140407     D tio10800o     E DS                  QUALIFIED INZ(*EXTDFT) STATIC
002671170619     D                                     EXTNAME('STRATEOBJ/TIO10800O')
002672140515     D Tio70802r       PR                  EXTPGM('TIO70802R')
002673140407     D  rqsDataFormat                10A
002674140407     D  rqsDataSize                  10I 0 CONST
002675140407     D  rqsData                    9999A   OPTIONS(*VARSIZE)
002676140407     D  rpyEsito                     10I 0
002677140407     D  rpyDataFormat                10A
002678140407     D  rpyDataSize                  10I 0 CONST
002679140407     D  rpyData                    9999A   OPTIONS(*VARSIZE)
002680140407
002681140407      /FREE
002682140407
002683140407       RESET retField;
002684140407
002685140407       rpyLen = %SIZE(tio10800o);
002686140407
002687140407       MONITOR;
002688140515         tio70802r ( tio10800i.rcdFmt
002689140407                   : rqsLen
002690140407                   : rqsDta
002691140407                   : retField
002692140407                   : tio10800o.rcdFmt
002693140407                   : rpyLen
002694140407                   : rpyDta
002695140407                   );
002696140407       ON-ERROR;
002697140407         retField = THIS_ESITO_ERRORE;
002698140407       ENDMON;
002699140407
002700140407       IF retField = THIS_ESITO_ERRORE;
002701140407         rpyOpc = RPYOPC_OTHERERROR;
002702140407         rp1error.errText = 'Si sono verificati degli errori gravi nella +
002703140515                             chiamata a TIO70802R.';
002704140407         rpyDta = rp1error;
002705140407         rpyLen = %SIZE(rp1error);
002706140407       ENDIF;
002707140407
002708140407       RETURN retField;
002709140407
002710140407      /END-FREE
002711140407     P GetDateRitiro   E
002712140407
002713140408
002714140408     P*--------------------------------------------------
002715140408     P* Procedure name: GetInfoRitiro
002716140408     P* Purpose:        Restituisce le informazioni per il ritiro.
002717140408     P* Returns:        Esito.
002718140408     P*--------------------------------------------------
002719140408     P GetInfoRitiro   B
002720140408     D GetInfoRitiro   PI            10I 0
002721140408
002722140408     D retField        S             10I 0 STATIC
002723140408     D fiora0i1      E DS                  QUALIFIED INZ(*EXTDFT) STATIC
002724140408     D fiora0o1      E DS                  QUALIFIED INZ(*EXTDFT) STATIC
002725140408     D tio10900i     E DS                  QUALIFIED STATIC
002726170619     D                                     EXTNAME('STRATEOBJ/TIO10900I')
002727140408     D tio10900o     E DS                  QUALIFIED STATIC
002728170619     D                                     EXTNAME('STRATEOBJ/TIO10900O')
002729140408
002730140408      /FREE
002731140408
002732140408       RESET retField;
002733140408
002734140408       tio10900i = %SUBST(rqsDta : 1 : rqsLen);
002735140408       RESET fiora0i1;
002736140408       RESET fiora0o1;
002737140408       fiora0i1.idLingua = tio10900i.lingua;
002738140408       fiora0i1.filRitiro = tio10900i.por;
002739140408       fiora0i1.cap = tio10900i.car;
002740140408       fiora0i1.localita = tio10900i.lor;
002741160526       fiora0i1.nazione = tio10900i.nar;
002742140408       fiora0i1.contattoT = tio10900i.cttTlf;
002743151104
002744151104       IF tio10900i.versione > A;
002745151104         IF tio10900i.sun = *ZERO;
002746151104           fiora0i1.anonimo = S;
002747151104         ENDIF;
002748151104         fiora0i1.chiPaga = tio10900i.pagamento;
002749151104       ENDIF;
002750140408
002751140408       MONITOR;
002752140409         Fiora00_GetInfoOrm( FIORA00_RQSOPCODE_GET_FRASE1
002753140409                           : this.rpyOpCode
002754140409                           : this.rpyIdMsg
002755140409                           : fiora0i1.formato
002756140409                           : fiora0i1
002757140409                           : %SIZE(fiora0i1)
002758140409                           : fiora0o1.formato
002759140409                           : fiora0o1
002760140409                           : %SIZE(fiora0o1)
002761140409                           );
002762140408       ON-ERROR;
002763140408         this.rpyOpCode = FIORA00_RPYOPCODE_ERROR;
002764140408       ENDMON;
002765140408
002766140408       // Controllo ORM non eseguito.
002767140408
002768140408       IF this.rpyOpCode < FIORA00_RPYOPCODE_DONE;
002769140408         rpyOpc = RPYOPC_OTHERERROR;
002770140408         rp1error.errCode = %CHAR(this.rpyIdMsg);
002771140408         rp1error.errText = 'Si sono verificati degli errori gravi nella +
002772140408                             chiamata a FIORA00R (errore '
002773140408                             + %CHAR(this.rpyIdMsg) + ').';
002774140408         rpyDta = rp1error;
002775140408         rpyLen = %SIZE(rp1error);
002776140408         RETURN THIS_ESITO_ERRORE;
002777140408       ENDIF;
002778140408
002779140408       tio10900o.infoOrm = fiora0o1.frase;
002780140408
002781140408       // Se arrivo qui vuol dire che è andato tutto bene ... forse ...
002782140408
002783140408       rpyDta = tio10900o;
002784140408       rpyLen = %SIZE(tio10900o);
002785140408
002786140408       RETURN retField;
002787140408
002788140408      /END-FREE
002789140408     P GetInfoRitiro   E
002790140408
