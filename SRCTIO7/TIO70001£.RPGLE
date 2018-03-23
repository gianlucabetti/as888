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
000197160318     D
000198140213     D  o110por      E                     INZ(*ZERO)
000199140213     D  o110poe      E                     INZ(*ZERO)
000200140225     D  pkg1                          6A   OVERLAY(pkg)
000201140225     D  pkg2                          1A   OVERLAY(pkg:7)
000202140225     D  vlm1                          2A   OVERLAY(vlm)
000203140225     D  vlm2                          3A   OVERLAY(vlm:3)
000204140213     D tio110ods     E DS                  QUALIFIED PREFIX('':4) INZ
000205140207
000206140207     ***************************************************************************
000207140207     **
000208140207     ** Campi.
000209140207     **
000210140207     ***************************************************************************
000211160321
000212140321     D rqsOpc          S             10A                                        REQUEST OPCODE
000213140321     D rqsDta          S           9999A                                        REQUEST DATA
000214160321     D rqsLen          S              4P 0                                      REQUEST DATA LENGTH
000215160321     D rqsCty          S             10A                                        REQUEST CLIENT TYPE
000216140321     D rqsCid          S             10A                                        REQUEST CLIENT
000217140321     D rpyOpc          S             10A                                        REPLY OPCODE
000218140321     D rpyDta          S           9999A                                        REPLY DATA
000219140311     D rpyLen          S              4P 0                                      REPLY DATA LENGTH
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
000434140213     D tio102ods     E DS                  QUALIFIED PREFIX('':4) STATIC INZ
000435140213     D  errAry                        1A   DIM(10) OVERLAY(err) INZ(*OFF)
000436140213     D  msgAry                       80A   DIM(10) OVERLAY(msg)
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
000747140402     D  pkg1                          6A   OVERLAY(pkg)
000748140402     D  pkg2                          1A   OVERLAY(pkg:7)
000749140402     D  vlm1                          2A   OVERLAY(vlm)
000750140402     D  vlm2                          3A   OVERLAY(vlm:3)
000751140213     D tio107ods     E DS                  QUALIFIED PREFIX('':4) STATIC INZ
000752140213     D  errAry                        1A   DIM(10) OVERLAY(err) INZ(*OFF)
000753140213     D  msgAry                       80A   DIM(10) OVERLAY(msg)
000754140211
000755140211      /FREE
000756140211
000757140211       tio107ids = %SUBST(rqsDta : 1 : rqsLen);
000758140211       RESET tio107ods;
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
001382140220     D tio121ods     E DS                  QUALIFIED PREFIX('':4) STATIC INZ
001383140220     D  errAry                        1A   DIM(10) OVERLAY(err) INZ(*OFF)
001384140220     D  msgAry                       80A   DIM(10) OVERLAY(msg)
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
001749140220
001750140220      /FREE
001751140220
001752140220       IF rqsOpc = RQSOPC_CONFERMA;
001753151106         RESET tio110ids;
001754151106         tio110ids = %SUBST(rqsDta : 1 : rqsLen);
001755140220       ENDIF;
001756140220
001757140220       RESET tio110ods;
001758140401       EVAL-CORR tio110ods = tio110ids;
001759140220
001760140220       fiora0i0.idLingua = tio110ids.lang;
001761140220       fiora0i0.idUtente = tio110ids.rqsCid;
001762140220       fiora0i0.vaoDao = %DEC(%DATE() : *ISO);
001763140220       fiora0i0.vaoOao = %DEC(%TIME() : *ISO);
001764140220
001765140220       IF tio110ids.ksu <> *BLANK;
001766140220         MONITOR;
001767140220           fiora0i0.vaoCor =
001768140220                           %DEC(%SUBST(tio110ids.ksu : 2) + '000' : 10 : 0);
001769140220         ON-ERROR;
001770140220           rpyOpc = RPYOPC_OTHERERROR;
001771140220           rp1error.errCode = 'COR';
001772140220           rp1error.errText = 'Il codice cliente ordinante +
001773140220                               contiene dati non validi.';
001774140220           rpyDta = rp1error;
001775140220           rpyLen = %SIZE(rp1error);
001776140220           RETURN THIS_ESITO_ERRORE;
001777140220         ENDMON;
001778140220       ENDIF;
001779140317
001780140317       IF tio110ids.cro = *BLANK;
001781140317         tio110ids.cro = *ZERO;
001782140317       ENDIF;
001783140220
001784140317       MONITOR;
001785140317         fiora0i0.vaoCra = %DEC(tio110ids.cro : 10 : 0);
001786140317       ON-ERROR;
001787140317         rpyOpc = RPYOPC_OTHERERROR;
001788140317         rp1error.errCode = 'CRO';
001789140317         rp1error.errText = 'Il codice cliente ritiro +
001790140317                             contiene dati non validi.';
001791140317         rpyDta = rp1error;
001792140317         rpyLen = %SIZE(rp1error);
001793140317         RETURN THIS_ESITO_ERRORE;
001794140317       ENDMON;
001795140220
001796140220       // Mittente.
001797140220
001798140220       fiora0i0.vaoRsr = tio110ids.rsr;
001799140220       fiora0i0.vaoInr = tio110ids.inr;
001800140220       fiora0i0.vaoCar = tio110ids.car;
001801140220       fiora0i0.vaoLor = tio110ids.lor;
001802140220       fiora0i0.vaoPrr = tio110ids.prr;
001803140220       fiora0i0.vaoNar = tio110ids.nar;
001804140220
001805140220       // Ordinante anonimo.
001806140220
001807140220       fiora0i0.codFiscale = tio110ids.cfo;
001808140220       fiora0i0.partitaIva = tio110ids.pio;
001809140220
001810140220       // Destinatario.
001811140220
001812140220       IF tio110ids.crc <> *BLANK;
001813140220         MONITOR;
001814140220           fiora0i0.vaoCrc = %DEC(tio110ids.crc : 10 : 0);
001815140220         ON-ERROR;
001816140220           rpyOpc = RPYOPC_OTHERERROR;
001817140220           rp1error.errCode = 'CRC';
001818140220           rp1error.errText = 'Il codice cliente destinatario +
001819140220                               contiene dati non validi.';
001820140220           rpyDta = rp1error;
001821140220           rpyLen = %SIZE(rp1error);
001822140220           RETURN THIS_ESITO_ERRORE;
001823140220         ENDMON;
001824140220       ENDIF;
001825140220
001826140220       fiora0i0.vaoRsc = tio110ids.rsc;
001827140220       fiora0i0.vaoInc = tio110ids.inc;
001828140220       fiora0i0.vaoCac = tio110ids.cac;
001829140220       fiora0i0.vaoLoc = tio110ids.loc;
001830140220       fiora0i0.vaoPrc = tio110ids.prc;
001831140220       fiora0i0.vaoNac = tio110ids.nac;
001832140220
001833140220       // Data ritiro
001834140220
001835140220       IF tio110ids.dar <> *BLANK;
001836140220         MONITOR;
001837140220           fiora0i0.vaoDar = %DEC(tio110ids.dar : 8 : 0);
001838140220           fiora0i0.vaoDar = %DEC(%DATE(fiora0i0.vaoDar : *EUR) : *ISO);
001839140220         ON-ERROR;
001840140220           rpyOpc = RPYOPC_OTHERERROR;
001841140220           rp1error.errCode = 'DAR';
001842160318           rp1error.errText = 'La data ritiro "' + tio110ids.dar
001843160318                            + '" contiene dati non validi.';
001844140220           rpyDta = rp1error;
001845140220           rpyLen = %SIZE(rp1error);
001846140220           RETURN THIS_ESITO_ERRORE;
001847140220         ENDMON;
001848140220       ENDIF;
001849140220
001850140220       // Ora ritiro
001851140220
001852140220       IF tio110ids.orr <> *BLANK;
001853140220         MONITOR;
001854140220           fiora0i0.vaoOrr = %DEC(tio110ids.orr : 4 : 0);
001855140220         ON-ERROR;
001856140220           rpyOpc = RPYOPC_OTHERERROR;
001857140220           rp1error.errCode = 'ORR';
001858160318           rp1error.errText = 'La ora ritiro "' + tio110ids.orr
001859160318                            + '" contiene dati non validi.';
001860140220           rpyDta = rp1error;
001861140220           rpyLen = %SIZE(rp1error);
001862140220           RETURN THIS_ESITO_ERRORE;
001863140220         ENDMON;
001864140220       ENDIF;
001865140220
001866140220       // Numero colli
001867140220
001868140220       IF tio110ids.ncl <> *BLANK;
001869140220         MONITOR;
001870140220           fiora0i0.vaoNcl = %DEC(tio110ids.ncl : 5 : 0);
001871140220         ON-ERROR;
001872140220         ENDMON;
001873140220       ENDIF;
001874140220
001875140220       // Peso
001876140220
001877140225       IF tio110ids.pkg <> *BLANK;
001878140225         IF tio110ids.pkg1 = *BLANK;
001879140225           tio110ids.pkg1 = *ZERO;
001880140225         ENDIF;
001881140225         IF tio110ids.pkg2 = *BLANK;
001882140225           tio110ids.pkg2 = *ZERO;
001883140225         ENDIF;
001884140225         MONITOR;
001885140225           tio110ids.pkg1 = %EDITC(%DEC(tio110ids.pkg1 : 6 : 0) : 'X');
001886140225           tio110ids.pkg2 = %EDITC(%DEC(tio110ids.pkg2 : 1 : 0) : 'X');
001887140225           fiora0i0.vaoPkg = %DEC(tio110ids.pkg : 7 : 0) / 10;
001888140220         ON-ERROR;
001889140220         ENDMON;
001890140220       ENDIF;
001891140220
001892140220       // Volume
001893140220
001894140220       IF tio110ids.vlm <> *BLANK;
001895140225         IF tio110ids.vlm1 = *BLANK;
001896140225           tio110ids.vlm1 = *ZERO;
001897140225         ENDIF;
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
002162140220
002163140220         IF prmInserireOrm;
002164140305
002165160217           tio110ids.npr = fiora0o0.npr;
002166160217
002167140220           MONITOR;
002168140515             Tio712r( tio110ids : tio110ods );
002169140220           ON-ERROR;
002170140220             tio110ods.err = *ON;
002171140220           ENDMON;
002172140220
002173140220           // Inserimento ORM non riuscito (soccia che sfiga).
002174140220
002175141219           IF tio110ods.err = *ON OR tio110ods.msg = *BLANK;
002176140220             rpyOpc = RPYOPC_OTHERERROR;
002177140220             rp1error.errText = 'Si sono verificati degli errori gravi nella +
002178140515                                 chiamata a TIO712R. ORM non inserito.';
002179140220             rpyDta = rp1error;
002180140220             rpyLen = %SIZE(rp1error);
002181141219             DUMP(A);
002182140220             RETURN THIS_ESITO_ERRORE;
002183140220           ENDIF;
002184160420
002185160509           tio110ods.oraRiI = %TIME(%EDITC(fiora0o0.oraRitI:'X') + '00'
002186160509                                     : *EUR0);
002187160509           tio110ods.oraRiF = %TIME(%EDITC(fiora0o0.oraRitF:'X') + '00'
002188160509                                     : *EUR0);
002189160509
002190160504           MONITOR;
002192160504             tio110ods.scOra1 = %TIME(fiora0o0.oraAmDaPor + '00' : *EUR0);
002193160504             tio110ods.scOra2 = %TIME(fiora0o0.oraAmAPor + '00' : *EUR0);
002194160504             tio110ods.scOra3 = %TIME(fiora0o0.oraPmDaPor + '00' : *EUR0);
002195160504             tio110ods.scOra4 = %TIME(fiora0o0.oraPmAPor + '00' : *EUR0);
002196160504           ON-ERROR;
002197160504             DUMP(A);
002198160504           ENDMON;
002199160504
002200160420           tio110ods.urlFil = fiora0o0.urlPor;
002201160420           tio110ods.emaFil = fiora0o0.mailPor;
002202160420           tio110ods.des = fiora0o0.descrPor;
002203160420           tio110ods.tel = %SCANRPL('/' : '' : fiora0o0.telPor);
002204160422           tio110ods.tel = %SCANRPL('.' : '' : tio110ods.tel);
002205160422           tio110ods.tel = %SCANRPL('-' : '' : tio110ods.tel);
002206140220
002207140220         ENDIF;
002208160420
002209141223       ENDIF;
002210151020
002211140220       // Se arrivo qui vuol dire che è andato tutto bene ... forse ...
002212140220
002213160315       IF rqsOpc = RQSOPC_CONFERMA OR rqsOpc = RQSOPC_CHECKIMM;
002214140220         rpyDta = tio110ods;
002215140220         rpyLen = %SIZE(tio110ods);
002216141222         IF tio110ods.err = *BLANK AND tio110ods.msg = *BLANK;
002217141222           DUMP(A);
002218141222         ENDIF;
002219140220       ENDIF;
002220140220
002221140220       RETURN THIS_ESITO_OK;
002222140220
002223140220      /END-FREE
002224140220     P Conferma        E
002225140212
002226140311
002227140311     P*--------------------------------------------------
002228140311     P* Procedure name: CarCliRit
002229140311     P* Purpose:        Carica i codici clienti ritiro.
002230140311     P* Returns:        Esito.
002231140311     P*--------------------------------------------------
002232140311     P CarCliRit       B
002233140311     D CarCliRit       PI            10I 0
002234140311
002235140404     D retField        S             10I 0 STATIC
002236140311     D fiorb0i0      E DS                  QUALIFIED STATIC INZ(*EXTDFT)
002237140311     D  ordina       E                     INZ(FIORB00_ORDINA_LOC)
002238140312     D  escludiTcr   E                     INZ(FIORB00_ESCLUDI_MAIINTERNET)
002239140313     D fiorb0o0      E DS                  QUALIFIED STATIC INZ(*EXTDFT)
002240140311     D  codiceAry                    10A   DIM(10) OVERLAY(codice)
002241140311     D  nomeAry                      48A   DIM(10) OVERLAY(nome)
002242140311     D  indirizzoAry                 35A   DIM(10) OVERLAY(indirizzo)
002243140311     D  localitaAry                  35A   DIM(10) OVERLAY(localita)
002244140311     D tio103ids     E DS                  QUALIFIED PREFIX('':4) STATIC
002245140311     D tio103ods     E DS                  QUALIFIED PREFIX('':4) STATIC INZ
002246140311     D  croAry                       10A   DIM(140) OVERLAY(cro)
002247140311     D  reiAry                       60A   DIM(140) OVERLAY(rei)
002248140311     D
002249140312     D local           DS                  QUALIFIED STATIC INZ
002250140311     D  i                            10I 0
002251140313     D  rqsOpCode                    10I 0 INZ(FIORB00_RQSOPCODE_LEGGI)
002252140311
002253140311      /FREE
002254140311
002255140311       RESET retField;
002256140311       RESET local;
002257140311       RESET tio103ods;
002258140311       RESET fiorb0i0;
002259140313       RESET fiorb0o0;
002260140311
002261140311       tio103ids = %SUBST(rqsDta : 1 : rqsLen);
002262140311       fiorb0i0.codiceKsu = tio103ids.ksu;
002263140311
002264140404       // La prima chiamata la faccio col codice operativo LEGGI che popola la
002265140404       // lista e mi restituisce i primi 10 elementi, poi proseguo con un ciclo
002266140404       // di chiamate col codice operativo CARICA che mi restituisce gruppi di
002267140404       // 10 elementi.
002268140404
002269140313       DOU tio103ods.nrc = fiorb0o0.clientiTot
002270140313        OR fiorb0o0.nrRec = *ZERO
002271140313        OR tio103ods.nrc = %ELEM(tio103ods.croAry);
002272140311
002273140313         MONITOR;
002274140313           Fiorb00_Anagrafica( local.rqsOpCode
002275140313                             : this.rpyOpCode
002276140313                             : this.rpyIdMsg
002277140313                             : fiorb0i0.formato
002278140313                             : fiorb0i0
002279140313                             : %SIZE(fiorb0i0)
002280140313                             : fiorb0o0.formato
002281140313                             : fiorb0o0
002282140313                             : %SIZE(fiorb0o0)
002283140313                             );
002284140313         ON-ERROR;
002285140313           this.rpyOpCode = FIORB00_RPYOPCODE_ERROR;
002286140313         ENDMON;
002287140311
002288140313         IF this.rpyOpCode < FIORB00_RPYOPCODE_DONE;
002289140313           tio103ods.err = *ON;
002290140313           LEAVE;
002291140313         ENDIF;
002292140313
002293140313         IF fiorb0o0.clientiTot = *ZERO OR fiorb0o0.nrRec = *ZERO;
002294140313           LEAVE;
002295140313         ENDIF;
002296140317
002297140317         tio103ods.cttTlf = fiorb0o0.contatto;
002298140311
002299140313         FOR local.i = 1 TO fiorb0o0.nrRec;
002300140313           tio103ods.nrc += 1;
002301140313           tio103ods.croAry(tio103ods.nrc) = fiorb0o0.codiceAry(local.i);
002302140313           tio103ods.reiAry(tio103ods.nrc) =
002303140313                                          %TRIMR(fiorb0o0.localitaAry(local.i))
002304140313                                        + ' '
002305140313                                        + %TRIMR(fiorb0o0.indirizzoAry(local.i))
002306140313                                        + ' '
002307140313                                        + %TRIMR(fiorb0o0.nomeAry(local.i))
002308140313                                        ;
002309140313           IF tio103ods.nrc = %ELEM(tio103ods.croAry);
002310140313             LEAVE;
002311140313           ENDIF;
002312140313         ENDFOR;
002313140311
002314140313         IF tio103ods.nrc = %ELEM(tio103ods.croAry)
002315140313         OR tio103ods.nrc = fiorb0o0.clientiTot;
002316140313           LEAVE;
002317140313         ENDIF;
002318140313
002319140313         local.rqsOpCode = FIORB00_RQSOPCODE_CARICA;
002320140313
002321140313       ENDDO;
002322140312
002323140312       MONITOR;
002324140312         Fiorb00_Anagrafica( FIORB00_RQSOPCODE_CLOSE
002325140312                           : this.rpyOpCode
002326140312                           : this.rpyIdMsg
002327140312                           );
002328140312       ON-ERROR;
002329140312       ENDMON;
002330140311
002331140311       IF tio103ods.err = *ON;
002332140311         rpyOpc = RPYOPC_OTHERERROR;
002333140311         rp1error.errText = 'Si sono verificati degli errori gravi nella +
002334140311                             chiamata a FIORB00R.';
002335140311         rpyDta = rp1error;
002336140311         rpyLen = %SIZE(rp1error);
002337140311         RETURN THIS_ESITO_ERRORE;
002338140311       ENDIF;
002339140317
002340140311       // Se arrivo qui vuol dire che è andato tutto bene ... forse ...
002341140311
002342140311       rpyDta = tio103ods;
002343140311       rpyLen = %SIZE(tio103ods);
002344140311
002345140311       RETURN retField;
002346140311
002347140311      /END-FREE
002348140311     P CarCliRit       E
002349140311
002350140402
002351140402     P*--------------------------------------------------
002352140403     P* Procedure name: GetOraAggiustata
002353140403     P* Purpose:        Restituisce l'ora aggiustata ai 15 minuti.
002354140402     P* Returns:        Ora e minuto aggiustati.
002355140402     P* Parameter:      hhmm => Ora e minuto
002356140402     P*--------------------------------------------------
002357140403     P GetOraAggiustata...
002358140402     P                 B
002359140403     D GetOraAggiustata...
002360140402     D                 PI             4A
002361140402     D  hhmm                          4S 0 CONST
002362140402     D retField        DS             4    QUALIFIED STATIC
002363140402     D  hhmm                   1      4S 0
002364140402     D  hh                     1      2S 0
002365140402     D  mm                     3      4S 0
002366140402
002367140402      /FREE
002368140402
002369140402       IF hhmm = *ZERO;
002370140402         RETURN *BLANK;
002371140402       ENDIF;
002372140402
002373140402       retField.hhmm = hhmm;
002374140402
002375140402       SELECT;
002376140402         WHEN retField.mm = 00 OR retField.mm = 15
002377140402           OR retField.mm = 30 OR retField.mm = 45;
002378140402           // Non è da aggiustare.
002379140402         WHEN retField.mm < 15;
002380140402           retField.mm = 15;
002381140402         WHEN retField.mm < 30;
002382140402           retField.mm = 30;
002383140402         WHEN retField.mm < 45;
002384140402           retField.mm = 45;
002385140402         OTHER;
002386140402           retField.hh += 1;
002387140402           retField.mm = 00;
002388140402       ENDSL;
002389140402
002390140402       RETURN retField;
002391140402
002392140402      /END-FREE
002393140403     P GetOraAggiustata...
002394140402     P                 E
002395140402
002396140404
002397140404     P*--------------------------------------------------
002398140404     P* Procedure name: CarCliRitEstesa
002399140404     P* Purpose:        Restituisce più di 140 anagrafiche di ritiro.
002400140404     P* Returns:        Esito.
002401140404     P*--------------------------------------------------
002402140404     PCarCliRitEstesa  B
002403140404     DCarCliRitEstesa  PI            10I 0
002404140404
002405140404     D retField        S             10I 0 STATIC
002406140404     D fiorb0i0      E DS                  QUALIFIED STATIC INZ(*EXTDFT)
002407140404     D  ordina       E                     INZ(FIORB00_ORDINA_LOC)
002408140404     D  escludiTcr   E                     INZ(FIORB00_ESCLUDI_MAIINTERNET)
002409140404     D fiorb0o0      E DS                  QUALIFIED STATIC INZ(*EXTDFT)
002410140404     D  codiceAry                    10A   DIM(10) OVERLAY(codice)
002411140404     D  nomeAry                      48A   DIM(10) OVERLAY(nome)
002412140404     D  indirizzoAry                 35A   DIM(10) OVERLAY(indirizzo)
002413140404     D  localitaAry                  35A   DIM(10) OVERLAY(localita)
002414140404     D tio103ids     E DS                  QUALIFIED PREFIX('':4) STATIC
002415160318     D tio103ods     E DS                  QUALIFIED PREFIX('':4) STATIC INZ
002416140404     D  croAry                       10A   DIM(140) OVERLAY(cro)
002417140404     D  reiAry                       60A   DIM(140) OVERLAY(rei)
002418140404     D local           DS                  QUALIFIED STATIC INZ
002419140404     D  i                            10I 0
002420140404     D  l                            10I 0
002421140404     D  f                            10I 0
002422140404     D  rqsOpCode                    10I 0 INZ(FIORB00_RQSOPCODE_LEGGI)
002423140404     D luoghi          DS                  QUALIFIED STATIC
002424140404     D  cro                          10A   DIM(980)
002425140404     D  rei                          60A   DIM(980)
002426140404     D usrSpc          DS                  QUALIFIED STATIC
002427140404     D  name                         10A
002428140404     D  library                      10A   INZ(GAITRATMP)
002429140404
002430140404      /FREE
002431140404
002432140404       RESET retField;
002433140404       RESET local;
002434140404       RESET tio103ods;
002435140404       RESET luoghi;
002436140404       RESET usrSpc;
002437140404
002438140404       tio103ids = %SUBST(rqsDta : 1 : rqsLen);
002439140404
002440140404       IF tio103ids.usrSpc = *BLANK;
002441140404
002442140404         RESET fiorb0i0;
002443140404         RESET fiorb0o0;
002444140404         fiorb0i0.codiceKsu = tio103ids.ksu;
002445140404
002446140404         // La prima chiamata la faccio col codice operativo LEGGI che popola la
002447140404         // lista e mi restituisce i primi 10 elementi, poi proseguo con un ciclo
002448140404         // di chiamate col codice operativo CARICA che mi restituisce gruppi di
002449140404         // 10 elementi.
002450140404
002451140404         DOU local.l = fiorb0o0.clientiTot
002452140404          OR fiorb0o0.nrRec = *ZERO
002453140404          OR local.l = %ELEM(luoghi.cro);
002454140404
002455140404           MONITOR;
002456140404             Fiorb00_Anagrafica( local.rqsOpCode
002457140404                               : this.rpyOpCode
002458140404                               : this.rpyIdMsg
002459140404                               : fiorb0i0.formato
002460140404                               : fiorb0i0
002461140404                               : %SIZE(fiorb0i0)
002462140404                               : fiorb0o0.formato
002463140404                               : fiorb0o0
002464140404                               : %SIZE(fiorb0o0)
002465140404                               );
002466140404           ON-ERROR;
002467140404             this.rpyOpCode = FIORB00_RPYOPCODE_ERROR;
002468140404           ENDMON;
002469140404
002470140404           IF this.rpyOpCode < FIORB00_RPYOPCODE_DONE;
002471140404             tio103ods.err = *ON;
002472140404             LEAVE;
002473140404           ENDIF;
002474140404
002475140404           IF fiorb0o0.clientiTot = *ZERO OR fiorb0o0.nrRec = *ZERO;
002476140404             LEAVE;
002477140404           ENDIF;
002478140404
002479140408           IF local.rqsOpCode = FIORB00_RQSOPCODE_LEGGI;
002480140408             tio103ods.cttTlf = fiorb0o0.contatto;
002481151019             tio103ods.caoEma = fiorb0o0.mailConf;
002482160318             tio103ods.caoSms = fiorb0o0.smsConf;
002483151019             IF fiorb0o0.alertConf = *BLANK;
002484151019               tio103ods.caoInv = 'N';
002485151019             ELSE;
002486151019               tio103ods.caoInv = fiorb0o0.alertConf;
002487151019             ENDIF;
002488151019           ENDIF;
002489140404
002490140404           FOR local.i = 1 TO fiorb0o0.nrRec;
002491140404             local.l += 1;
002492140404             luoghi.cro(local.l) = fiorb0o0.codiceAry(local.i);
002493140404             luoghi.rei(local.l) = %TRIMR(fiorb0o0.localitaAry(local.i))
002494140404                                 + ' '
002495140404                                 + %TRIMR(fiorb0o0.indirizzoAry(local.i))
002496140404                                 + ' '
002497140404                                 + %TRIMR(fiorb0o0.nomeAry(local.i))
002498140404                                 ;
002499140404             IF local.l = %ELEM(luoghi.cro);
002500140404               LEAVE;
002501140404             ENDIF;
002502140404           ENDFOR;
002503140404
002504140404           IF local.l = %ELEM(luoghi.cro)
002505140404           OR local.l = fiorb0o0.clientiTot;
002506140404             LEAVE;
002507140404           ENDIF;
002508140404
002509140404           local.rqsOpCode = FIORB00_RQSOPCODE_CARICA;
002510140404
002511140404         ENDDO;
002512140404
002513140404         MONITOR;
002514140404           Fiorb00_Anagrafica( FIORB00_RQSOPCODE_CLOSE
002515140404                             : this.rpyOpCode
002516140404                             : this.rpyIdMsg
002517140404                             );
002518140404         ON-ERROR;
002519140404         ENDMON;
002520140404
002521140404         IF tio103ods.err = *ON;
002522140404           rpyOpc = RPYOPC_OTHERERROR;
002523140404           rp1error.errText = 'Si sono verificati degli errori gravi nella +
002524140404                               chiamata a FIORB00R.';
002525140404           rpyDta = rp1error;
002526140404           rpyLen = %SIZE(rp1error);
002527140404           RETURN THIS_ESITO_ERRORE;
002528140404         ENDIF;
002529140404
002530140404         // Ho più di 140 elementi, posteggio tutto in uno user space.
002531140404
002532140404         IF local.l > %ELEM(tio103ods.croAry);
002533140404
002534140404           TrulTmpN( '*USRSPC' : *BLANK : usrSpc.library : usrSpc.name );
002535140404
002536140404           IF %SUBST(usrSpc.name : 1 : 1) = '*';
002537140404             DUMP(A);
002538140404             RETURN THIS_ESITO_ERRORE;
002539140404           ENDIF;
002540140404
002541140404           CLEAR QUSEC;
002542140404           QUSBPRV = %SIZE(QUSEC);
002543140404
002544140404           CreateUserSpace( usrSpc
002545140404                          : 'LUOGHI'
002546140404                          : %SIZE(luoghi)
002547140404                          : X'00'
002548140404                          : '*ALL'
002549140404                          : %SUBST(rqsDta : 1 : 50)
002550140404                          : '*NO'
002551140404                          : QUSEC
002552140404                          );
002553140404
002554140404           IF QUSEI <> *BLANK;
002555140404             DUMP(A);
002556140404             RETURN THIS_ESITO_ERRORE;
002557140404           ENDIF;
002558140404
002559140404           CLEAR QUSEC;
002560140404           QUSBPRV = %SIZE(QUSEC);
002561140404
002562140404           ChangeUserSpace( usrSpc
002563140404                          : 1
002564140404                          : %SIZE(luoghi)
002565140404                          : luoghi
002566140404                          : '0'
002567140404                          : QUSEC
002568140404                          );
002569140404
002570140404           IF QUSEI <> *BLANK;
002571140404             DUMP(A);
002572140404             RETURN THIS_ESITO_ERRORE;
002573140404           ENDIF;
002574140404
002575140404           tio103ods.usrSpc = usrSpc.name;
002576140404
002577140404         ENDIF;
002578140404
002579140404       ELSE; // Ho ricevuto il nome dello user space.
002580140404
002581140408         tio103ods.eodata = *OFF;
002582140404         tio103ods.usrSpc = tio103ids.usrSpc;
002583140404
002584140404         CLEAR QUSEC;
002585140404         QUSBPRV = %SIZE(QUSEC);
002586140404
002587140404         RetrieveUserSpace( tio103ids.usrSpc + usrSpc.library
002588140404                          : 1
002589140404                          : %SIZE(luoghi)
002590140404                          : luoghi
002591140404                          : QUSEC
002592140404                          );
002593140404
002594140404         IF QUSEI <> *BLANK;
002595140404           DUMP(A);
002596140404           RETURN THIS_ESITO_ERRORE;
002597140404         ENDIF;
002598140404
002599140404       ENDIF;
002600140404
002601140404       // Cerco l'ultimo elemento pieno.
002602140404
002603140404       local.f = %LOOKUP(*BLANK : luoghi.cro);
002604140404
002605140404       IF local.f = *ZERO;
002606140407         local.f = %ELEM(luoghi.cro);
002607140407       ELSE;
002608140407         local.f -= 1;
002609140404       ENDIF;
002610140404
002611140404       RESET local.i;
002612140404
002613140404       // Restituisco 140 elementi per volta.
002614140404
002615140404       FOR local.l = (tio103ids.prgEle + 1) TO local.f;
002616140404         local.i += 1;
002617140404         tio103ods.croAry(local.i) = luoghi.cro(local.l);
002618140404         tio103ods.reiAry(local.i) = luoghi.rei(local.l);
002619140404         IF local.l = local.f;
002620140404           tio103ods.eodata = *ON;
002621140404           LEAVE;
002622140404         ENDIF;
002623140404         IF local.i = %ELEM(tio103ods.croAry);
002624140404           LEAVE;
002625140404         ENDIF;
002626140404       ENDFOR;
002627140404
002628140404       tio103ods.nrc = local.i;
002629140404       tio103ods.prgEle = local.l;
002630140404
002631140407       IF tio103ods.eodata = *ON AND tio103ids.usrSpc <> *BLANK;
002632140404         CLEAR QUSEC;
002633140404         QUSBPRV = %SIZE(QUSEC);
002634140407         DeleteUserSpace(tio103ids.usrSpc + usrSpc.library : QUSEC);
002635140404         CLEAR tio103ods.usrSpc;
002636140404       ENDIF;
002637140404
002638140404       // Se arrivo qui vuol dire che è andato tutto bene ... forse ...
002639140404
002640140404       rpyDta = tio103ods;
002641140404       rpyLen = %SIZE(tio103ods);
002642140404
002643140404       RETURN retField;
002644140404
002645140404      /END-FREE
002646140404     PCarCliRitEstesa  E
002647140404
002648140407
002649140407     P*--------------------------------------------------
002650140407     P* Procedure name: GetDateRitiro
002651140407     P* Purpose:        Restituisce le date di ritiro ammesse.
002652140407     P* Returns:        Esito.
002653140407     P*--------------------------------------------------
002654140407     P GetDateRitiro   B
002655140407     D GetDateRitiro   PI            10I 0
002656140407
002657140407     D retField        S             10I 0 STATIC
002658140407     D tio10800i     E DS                  QUALIFIED INZ(*EXTDFT) STATIC
002659140407     D tio10800o     E DS                  QUALIFIED INZ(*EXTDFT) STATIC
002660140515     D Tio70802r       PR                  EXTPGM('TIO70802R')
002661140407     D  rqsDataFormat                10A
002662140407     D  rqsDataSize                  10I 0 CONST
002663140407     D  rqsData                    9999A   OPTIONS(*VARSIZE)
002664140407     D  rpyEsito                     10I 0
002665140407     D  rpyDataFormat                10A
002666140407     D  rpyDataSize                  10I 0 CONST
002667140407     D  rpyData                    9999A   OPTIONS(*VARSIZE)
002668140407
002669140407      /FREE
002670140407
002671140407       RESET retField;
002672140407
002673140407       rpyLen = %SIZE(tio10800o);
002674140407
002675140407       MONITOR;
002676140515         tio70802r ( tio10800i.rcdFmt
002677140407                   : rqsLen
002678140407                   : rqsDta
002679140407                   : retField
002680140407                   : tio10800o.rcdFmt
002681140407                   : rpyLen
002682140407                   : rpyDta
002683140407                   );
002684140407       ON-ERROR;
002685140407         retField = THIS_ESITO_ERRORE;
002686140407       ENDMON;
002687140407
002688140407       IF retField = THIS_ESITO_ERRORE;
002689140407         rpyOpc = RPYOPC_OTHERERROR;
002690140407         rp1error.errText = 'Si sono verificati degli errori gravi nella +
002691140515                             chiamata a TIO70802R.';
002692140407         rpyDta = rp1error;
002693140407         rpyLen = %SIZE(rp1error);
002694140407       ENDIF;
002695140407
002696140407       RETURN retField;
002697140407
002698140407      /END-FREE
002699140407     P GetDateRitiro   E
002700140407
002701140408
002702140408     P*--------------------------------------------------
002703140408     P* Procedure name: GetInfoRitiro
002704140408     P* Purpose:        Restituisce le informazioni per il ritiro.
002705140408     P* Returns:        Esito.
002706140408     P*--------------------------------------------------
002707140408     P GetInfoRitiro   B
002708140408     D GetInfoRitiro   PI            10I 0
002709140408
002710140408     D retField        S             10I 0 STATIC
002711140408     D fiora0i1      E DS                  QUALIFIED INZ(*EXTDFT) STATIC
002712140408     D fiora0o1      E DS                  QUALIFIED INZ(*EXTDFT) STATIC
002713140408     D tio10900i     E DS                  QUALIFIED STATIC
002714140408     D tio10900o     E DS                  QUALIFIED STATIC
002715140408
002716140408      /FREE
002717140408
002718140408       RESET retField;
002719140408
002720140408       tio10900i = %SUBST(rqsDta : 1 : rqsLen);
002721140408       RESET fiora0i1;
002722140408       RESET fiora0o1;
002723140408       fiora0i1.idLingua = tio10900i.lingua;
002724140408       fiora0i1.filRitiro = tio10900i.por;
002725140408       fiora0i1.cap = tio10900i.car;
002726140408       fiora0i1.localita = tio10900i.lor;
002727160526       fiora0i1.nazione = tio10900i.nar;
002728140408       fiora0i1.contattoT = tio10900i.cttTlf;
002729151104
002730151104       IF tio10900i.versione > A;
002731151104         IF tio10900i.sun = *ZERO;
002732151104           fiora0i1.anonimo = S;
002733151104         ENDIF;
002734151104         fiora0i1.chiPaga = tio10900i.pagamento;
002735151104       ENDIF;
002736140408
002737140408       MONITOR;
002738140409         Fiora00_GetInfoOrm( FIORA00_RQSOPCODE_GET_FRASE1
002739140409                           : this.rpyOpCode
002740140409                           : this.rpyIdMsg
002741140409                           : fiora0i1.formato
002742140409                           : fiora0i1
002743140409                           : %SIZE(fiora0i1)
002744140409                           : fiora0o1.formato
002745140409                           : fiora0o1
002746140409                           : %SIZE(fiora0o1)
002747140409                           );
002748140408       ON-ERROR;
002749140408         this.rpyOpCode = FIORA00_RPYOPCODE_ERROR;
002750140408       ENDMON;
002751140408
002752140408       // Controllo ORM non eseguito.
002753140408
002754140408       IF this.rpyOpCode < FIORA00_RPYOPCODE_DONE;
002755140408         rpyOpc = RPYOPC_OTHERERROR;
002756140408         rp1error.errCode = %CHAR(this.rpyIdMsg);
002757140408         rp1error.errText = 'Si sono verificati degli errori gravi nella +
002758140408                             chiamata a FIORA00R (errore '
002759140408                             + %CHAR(this.rpyIdMsg) + ').';
002760140408         rpyDta = rp1error;
002761140408         rpyLen = %SIZE(rp1error);
002762140408         RETURN THIS_ESITO_ERRORE;
002763140408       ENDIF;
002764140408
002765140408       tio10900o.infoOrm = fiora0o1.frase;
002766140408
002767140408       // Se arrivo qui vuol dire che è andato tutto bene ... forse ...
002768140408
002769140408       rpyDta = tio10900o;
002770140408       rpyLen = %SIZE(tio10900o);
002771140408
002772140408       RETURN retField;
002773140408
002774140408      /END-FREE
002775140408     P GetInfoRitiro   E
002776140408
