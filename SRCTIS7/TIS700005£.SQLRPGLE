000100140606     ***************************************************************************
000101140526     **
000102140526     ** Questo programma è chiamato da AS777 via DRDA e gestisce le istruzioni
000103140526     ** di consegna inserite dal sito web.
000104140526     **
000105140526     ***************************************************************************
000106140605     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('TIBS':'TIS')
000124140526
000125140526     ***************************************************************************
000126140526     **
000127140526     ** Strutture dati.
000128140526     **
000129140526     ***************************************************************************
000130140605     D dataRitiro...
000131140605     D                 DS                  QUALIFIED
000132140605     D  data                           D   DIM(9) DATFMT(*EUR)
000133140605     D  descrizione                  30A   DIM(9)
000134140605     D  i                             3U 0
000135140605     D dayOfWeek       DS                  QUALIFIED
000136140605     D                                7A   INZ(MSGID_LUNEDI)
000137140605     D                                7A   INZ(MSGID_MARTEDI)
000138140605     D                                7A   INZ(MSGID_MERCOLEDI)
000139140605     D                                7A   INZ(MSGID_GIOVEDI)
000140140605     D                                7A   INZ(MSGID_VENERDI)
000141140605     D                                7A   INZ(MSGID_SABATO)
000142140605     D                                7A   INZ(MSGID_DOMENICA)
000143140605     D  msgId                         7A   DIM(7) OVERLAY(dayOfWeek)
000144140605     D fnlry00m0     E DS                  QUALIFIED INZ(*EXTDFT)
000145140527     D  idMsgAry                      7A   DIM(99) OVERLAY(idMsg)
000146140527     D  idCampoAry                   10A   DIM(99) OVERLAY(idCampo)
000147140527     D  errWarnAry                    1A   DIM(99) OVERLAY(errWarn)
000148140527     D  textMsgAry                  255A   DIM(99) OVERLAY(textMsg)
000149140527     D rp1error        DS                  QUALIFIED
000150140527     D  errCode                       7
000151140527     D  errSpace                      1
000152140527     D  errText                     200
000153140527     D tis700008     E DS                  QUALIFIED INZ(*EXTDFT)
000154140526     D tis700009     E DS                  QUALIFIED INZ(*EXTDFT)
000155140527     D tis700010     E DS                  QUALIFIED INZ(*EXTDFT)
000156140527     D  idMsgAry                      7A   DIM(9) OVERLAY(idMsg)
000157140527     D  idCampoAry                   10A   DIM(9) OVERLAY(idCampo)
000158140527     D  errWarnAry                    1A   DIM(9) OVERLAY(errWarn)
000159140527     D  textMsgAry                  255A   DIM(9) OVERLAY(textMsg)
000160140526     D psds           SDS                  QUALIFIED
000161140526     D  exceptionID           40     46A
000162140526     D this            DS                  QUALIFIED INZ
000163140527     D  i                            10I 0
000164140527     D  rpyOpCode                    10I 0
000165140526     D  rpyIdMsg                     10I 0
000166140526
000167140526     ***************************************************************************
000168140526     **
000169140526     ** Campi.
000170140526     **
000171140526     ***************************************************************************
000172140526     D rqsOpc          S             10A                                        * REQUEST OPCODE
000173140526     D rqsDta          S           9999A                                        * REQUEST DATA
000174140526     D rqsLen          S              4P 0                                      * REQUEST DATA LENGT
000175140526     D rqsCty          S             10A                                        * REQUEST CLIENT TYP
000176140526     D rqsCid          S             10A                                        * REQUEST CLIENT
000177140526     D rpyOpc          S             10A                                        * REPLY OPCODE
000178140526     D rpyDta          S           9999A                                        * REPLY DATA
000179140526     D rpyLen          S              4P 0                                      * REPLY DATA LENGTH
000180140526
000181140526     ***************************************************************************
000182140526     **
000183140526     ** Costanti.
000184140526     **
000185140526     ***************************************************************************
000186140526      /COPY GAITRASRC/SRCCONST,FNLRY00R
000187140930      /COPY GAITRASRC/SRCCONST,TRULORIR
000188140526     D DONE...
000189140526     D                 C                   'DONE'
000190140527     D ERROR...
000191140527     D                 C                   'ERROR'
000192140529     D HSMOPCERR...
000193140529     D                 C                   'HSMOPCERR'
000194140605     D MSGID_OGGI...
000195140605     D                 C                   'TIS0698'
000196140605     D MSGID_LUNEDI...
000197140605     D                 C                   'TIS0351'
000198140605     D MSGID_MARTEDI...
000199140605     D                 C                   'TIS0354'
000200140605     D MSGID_MERCOLEDI...
000201140605     D                 C                   'TIS0356'
000202140605     D MSGID_GIOVEDI...
000203140605     D                 C                   'TIS0203'
000204140605     D MSGID_VENERDI...
000205140605     D                 C                   'TIS0607'
000206140605     D MSGID_SABATO...
000207140605     D                 C                   'TIS0533'
000208140605     D MSGID_DOMENICA...
000209140605     D                 C                   'TIS0159'
000210140619     D MSGID_BRTCODE_NON_VALIDO...
000211140619     D                 C                   'TIS0737'
000212140605     D OTHERERROR...
000213140526     D                 C                   'OTHERERROR'
000214140930     D GETFILIALE...
000215140930     D                 C                   'GETFILIALE'
000216140526     D GETISTRUCO...
000217140526     D                 C                   'GETISTRUCO'
000218140526     D PUTISTRUCO...
000219140526     D                 C                   'PUTISTRUCO'
000233140930
000234140526     ***************************************************************************
000235140526     **
000236140526     ** Prototipi.
000237140526     **
000238140526     ***************************************************************************
000239140526      /COPY GAITRASRC/SRCPROTOPR,FNLRY00R
000240140605      /COPY GAITRASRC/SRCPROTOPR,RTVMSGLANG
000241140930      /COPY GAITRASRC/SRCPROTOPR,TRULORIR
000242140605
000243140526     ***************************************************************************
000244140526     **
000245140526     ** Parametri.
000246140526     **
000247140526     ***************************************************************************
000248140526     C     *ENTRY        PLIST
000249140526     C                   PARM                    rqsOpc
000250140526     C                   PARM                    rqsDta
000251140526     C                   PARM                    rqsLen
000252140526     C                   PARM                    rqsCty
000253140526     C                   PARM                    rqsCid
000254140526     C                   PARM                    rpyOpc
000255140526     C                   PARM                    rpyDta
000256140526     C                   PARM                    rpyLen
000257140526
000258140526     ***************************************************************************
000259140526     **
000260140526     ** Main.
000261140526     **
000262140526     ***************************************************************************
000263140526
000264140526      /FREE
000265140526
000266140526       CLEAR rpyOpc;
000267140526       CLEAR rpyDta;
000268140526       CLEAR rpyLen;
000269140526       CLEAR this;
000270140526
000271140526       SELECT;
000272140526         WHEN rqsOpc = GETISTRUCO;
000273140526           GetIstruzioniConsegna();
000274140930         WHEN rqsOpc = GETFILIALE;
000275140930           GetAnagraficaFiliale();
000276140526         WHEN rqsOpc = PUTISTRUCO;
000277140610           PutIstruzioniConsegna();
000278140529         OTHER;
000279140529           rpyOpc = HSMOPCERR;
000280140526       ENDSL;
000281140526
000282140526       RETURN;
000283140526
000284140526      /END-FREE
000285140526
000286140526     P*--------------------------------------------------
000287140526     P* Procedure name: GetIstruzioniConsegna
000288140526     P* Purpose:        Restituisce informazioni sulla consegna.
000289140526     P* Returns:        Esito.
000290140526     P*--------------------------------------------------
000291140526     P GetIstruzioniConsegna...
000292140526     P                 B
000293140526     D GetIstruzioniConsegna...
000294140526     D                 PI            10I 0
000295140526
000296140526     D retField        S             10I 0 STATIC
000297140606     D local           DS                  STATIC QUALIFIED INZ
000298140606     D  oraP                           T   TIMFMT(*EUR)                         Ora progressiva
000299140606     D  oraF                           T   TIMFMT(*EUR)                         Ora fine
000300140606     D  minuti                        3U 0                                      Minuti periodo
000301140526     D fnlry00i0     E DS                  QUALIFIED INZ(*EXTDFT) STATIC
000302140526     D fnlry00o0     E DS                  QUALIFIED INZ(*EXTDFT) STATIC
000303140526     D tis700006     E DS                  QUALIFIED INZ(*EXTDFT) STATIC
000304140526     D tis700007     E DS                  QUALIFIED INZ(*EXTDFT) STATIC
000305140526
000306140526      /FREE
000307140526
000308140526       RESET retField;
000309140606       RESET local;
000310140526       RESET tis700007;
000311140526       RESET fnlry00i0;
000312140526       RESET fnlry00o0;
000313140526       RESET fnlry00m0;
000314140526
000315140526       tis700006 = %SUBST(rqsDta : 1 : rqsLen);
000316141009
000317141009       // Per gestire il lasciato avviso senza BRTcode occorre il login;
000318141009       // senza login pulisco KSU, così avrò certamente un errore.
000319141009
000320141009       IF tis700006.brtCode = *BLANK AND tis700006.sun = *ZERO;
000321141009         CLEAR tis700006.ksu;
000322141009       ENDIF;
000323141009
000324140526       fnlry00i0.linIso2 = tis700006.lingua;
000325140526       fnlry00i0.ksu = tis700006.ksu;
000326140526       fnlry00i0.idBolla = %SUBST(tis700006.annoBolla : 3 : 2)
000327140526                         + tis700006.nBolla;
000328140526       fnlry00i0.brtCode = tis700006.brtCode;
000329140526
000330140526       MONITOR;
000331140526         Fnlry00_Immetti( FNLRY00_RQSOPCODE_GET_ISTRUCO
000332140526                        : this.rpyOpCode
000333140526                        : this.rpyIdMsg
000334140526                        : fnlry00i0.formato
000335140526                        : fnlry00i0
000336140526                        : %SIZE(fnlry00i0)
000337140526                        : fnlry00o0.formato
000338140526                        : fnlry00o0
000339140526                        : %SIZE(fnlry00o0)
000340140526                        : fnlry00m0.formato
000341140526                        : fnlry00m0
000342140526                        : %SIZE(fnlry00m0)
000343140526                        );
000344140526         ON-ERROR;
000345140618           this.rpyOpCode = -1;
000346140526           this.rpyIdMsg = %STATUS * -1;
000347140526       ENDMON;
000348140526
000349140527       IF this.rpyOpCode < FNLRY00_ESITO_OK OR fnlry00m0.nrMsg > *ZERO;
000350140527         IF fnlry00m0.nrMsg = *ZERO;
000351141120           rp1error.errCode = %CHAR(this.rpyIdMsg);
000352140527           rp1error.errText = 'CALL a FNLRY00R terminata in modo anomalo.';
000353140527           rpyOpc = OTHERERROR;
000354140527           rpyDta = rp1Error;
000355140527           rpyLen = %SIZE(rp1Error);
000356141119           DUMP(A);
000357140527         ELSE;
000358140527           RESET tis700010;
000359140905           IF tis700006.brtCode <> *BLANK;
000360140905             tis700010.annoBolla = '20' + %SUBST(tis700006.brtCode : 1 : 2);
000361140905             tis700010.nBolla = %SUBST(tis700006.brtCode : 3);
000362140905           ELSE;
000363140905             tis700010.annoBolla = tis700006.annoBolla;
000364140905             tis700010.nBolla = tis700006.nBolla;
000365140905           ENDIF;
000366140527           FOR this.i = 1 TO fnlry00m0.nrMsg;
000367140527             IF fnlry00m0.errWarnAry(this.i) = FNLRY00_MSG_ERRORE;
000368140527               tis700010.nrMsg += 1;
000369140619               IF fnlry00m0.idMsgAry(this.i) = MSGID_BRTCODE_NON_VALIDO;
000370140619                 rpyOpc = MSGID_BRTCODE_NON_VALIDO;
000371140619                 rpyDta = fnlry00m0.textMsgAry(this.i);
000372140619                 rpyLen = %SIZE(fnlry00m0.textMsgAry);
000373140619                 RETURN this.rpyOpCode;
000374140619               ENDIF;
000375140619               tis700010.idMsgAry(tis700010.nrMsg) = fnlry00m0.idMsgAry(this.i);
000376140527               tis700010.idCampoAry(tis700010.nrMsg) =
000377140527                                                   fnlry00m0.idCampoAry(this.i);
000378140527               tis700010.errWarnAry(tis700010.nrMsg) =
000379140527                                                   fnlry00m0.errWarnAry(this.i);
000380140527               tis700010.textMsgAry(tis700010.nrMsg) =
000381140527                                                   fnlry00m0.textMsgAry(this.i);
000382140527               IF tis700010.nrMsg = *HIVAL;
000383140527                 LEAVE;
000384140527               ENDIF;
000385140527             ENDIF;
000386140527           ENDFOR;
000387140527           rpyOpc = tis700010.formato;
000388140527           rpyDta = tis700010;
000389140527           rpyLen = %SIZE(tis700010);
000390140527         ENDIF;
000391140526         RETURN this.rpyOpCode;
000392140526       ENDIF;
000393140526
000394140526       tis700007.annoBolla = tis700006.annoBolla;
000395140526       tis700007.nBolla = tis700006.nBolla;
000396140526       tis700007.brtCode = tis700006.brtCode;
000397140526
000398140526       IF tis700007.annoBolla = *BLANK;
000399140526         tis700007.annoBolla = '20' + %SUBST(tis700007.brtCode : 1 : 2);
000400140526       ENDIF;
000401140526
000402140526       IF tis700007.nBolla = *BLANK;
000403140526         tis700007.nBolla = %SUBST(tis700007.brtCode : 3 : 12);
000404140526       ENDIF;
000405140526
000406140526       tis700007.isIstruCo = fnlry00o0.butIstrCon;
000407140526       tis700007.isAppuntam = fnlry00o0.butAppunt;
000408140526       tis700007.isFermoDep = fnlry00o0.butFerDep;
000409140526       tis700007.isAltroInd = fnlry00o0.butIndiriz;
000410140526       tis700007.isAltre = fnlry00o0.butAltro;
000411140526       tis700007.destEmail = fnlry00o0.destMail;
000412140526       tis700007.destMobile = fnlry00o0.destCell;
000413140526       tis700007.dtConsegna = fnlry00o0.dtConsRic;
000414140606
000415140606       IF fnlry00o0.orConsRic <> *BLANK;
000416140606         tis700007.orConsegna =
000417140606         %SUBST(%CHAR(%TIME(fnlry00o0.orConsRic + '00' : *EUR0)) : 1 : 5);
000418140606       ENDIF;
000419140606
000420140526       tis700007.msgNoFD = fnlry00o0.msgFdMitt;
000421140617       tis700007.msgFdCond = fnlry00o0.msgFdCond;
000422140929       tis700007.msgPrfOra = fnlry00o0.msgPrfOra;
000423141007       tis700007.msgIndiriz = fnlry00o0.msgAutInd;
000424140605
000425140606       // Elenco date consegna possibili.
000426140605
000427140606       tis700007.dtConsRitV = fnlry00o0.dateConsT;
000428140606
000429140606       FOR tis700007.dtConsRitN = 1 TO fnlry00o0.nrDate;
000430140606         %SUBST(tis700007.dtConsRitZ : (tis700007.dtConsRitN * 30) - 29 : 30) =
000431140606         GetDescrizioneGiorno( %DATE(%SUBST( tis700007.dtConsRitV
000432140606         : (tis700007.dtConsRitN * 10) - 9 : 10) : *EUR) : tis700006.lingua );
000433140606       ENDFOR;
000434140922
000435140922       tis700007.dtConsRitN = fnlry00o0.nrDate;
000436140922
000437140922       // Elenco date consegna possibili per nuovo indirizzo.
000438140922
000439140922       tis700007.dtConiRitV = fnlry00o0.dateConsT;
000440140922
000441140922       FOR tis700007.dtConiRitN = 1 TO fnlry00o0.nrDate;
000442140922         %SUBST(tis700007.dtConiRitZ : (tis700007.dtConiRitN * 30) - 29 : 30) =
000443140922         GetDescrizioneGiorno( %DATE(%SUBST( tis700007.dtConiRitV
000444140922         : (tis700007.dtConiRitN * 10) - 9 : 10) : *EUR) : tis700006.lingua );
000445140922       ENDFOR;
000446140605
000447140922       tis700007.dtConiRitN = fnlry00o0.nrDate;
000448140606
000449140606       // Elenco ore consegna possibili.
000450140606
000451140606       local.oraP = %TIME(fnlry00o0.oraRiTndI + '00' : *EUR0);
000452140606       local.oraF = %TIME(fnlry00o0.oraRiTndF + '00' : *EUR0);
000453140606       local.minuti = %UNS(fnlry00o0.mmIntervC);
000454140606       %SUBST(tis700007.orConsRitV : 1 : 8) = %SUBST(%CHAR(local.oraP) : 1 : 5);
000455140606       tis700007.orConsRitN = 1;
000456140606
000457140606       FOR this.i = 2 TO 99;
000458140606         local.oraP += %MINUTES(local.minuti);
000459140606         %SUBST(tis700007.orConsRitV : (this.i * 8) - 7 : 8) =
000460140606                                              %SUBST(%CHAR(local.oraP) : 1 : 5);
000461140606         tis700007.orConsRitN += 1;
000462140606         IF local.oraP >= local.oraF;
000463140606           LEAVE;
000464140606         ENDIF;
000465140606       ENDFOR;
000466140922
000467140922       // Elenco ore consegna possibili per nuovo indirizzo.
000468140922
000469140922       local.oraP = %TIME(fnlry00o0.oraRiTndI + '00' : *EUR0);
000470140922       local.oraF = %TIME(fnlry00o0.oraRiTndF + '00' : *EUR0);
000471140922       local.minuti = %UNS(fnlry00o0.mmIntervC);
000472140922       %SUBST(tis700007.orConiRitV : 1 : 8) = %SUBST(%CHAR(local.oraP) : 1 : 5);
000473140922       tis700007.orConiRitN = 1;
000474140922
000475140922       FOR this.i = 2 TO 99;
000476140922         local.oraP += %MINUTES(local.minuti);
000477140922         %SUBST(tis700007.orConiRitV : (this.i * 8) - 7 : 8) =
000478140922                                              %SUBST(%CHAR(local.oraP) : 1 : 5);
000479140922         tis700007.orConiRitN += 1;
000480140922         IF local.oraP >= local.oraF;
000481140922           LEAVE;
000482140922         ENDIF;
000483140922       ENDFOR;
000484141009
000485170126       IF fnlry00o0.versione > 'A';
000486170126         tis700007.desIstrCon = fnlry00o0.desIstrCon;
000487170126       ENDIF;
000489170126
000490140606       // Ho finito.
000491140606
000492140527       rpyOpc = tis700007.formato;
000493140526       rpyDta = tis700007;
000494140526       rpyLen = %SIZE(tis700007);
000495140526
000496140526       RETURN retField;
000497140526
000498140526      /END-FREE
000499140526     P GetIstruzioniConsegna...
000500140526     P                 E
000501140526
000502140605     P*--------------------------------------------------
000503140605     P* Procedure name: GetDescrizioneGiorno
000504140605     P* Purpose:        Restituisce la descrizione del giorno.
000505140605     P* Returns:        Descrizione del giorno.
000506140605     P* Parameter:      priData
000507140605     P*--------------------------------------------------
000508140605     P GetDescrizioneGiorno...
000509140605     P                 B
000510140605     D GetDescrizioneGiorno...
000511140605     D                 PI           255A   VARYING
000512140605     D  priData                        D   CONST
000513140605     D  priLingua                     2A
000514140605
000515140605     D dw              S             10I 0 STATIC
000516140605
000517140605      /FREE
000518140605
000519140605       IF priData = %DATE();
000520140605         RETURN %TRIMR(rtvMsgLang(MSGID_OGGI : priLingua));
000521140605       ENDIF;
000522140605
000523140605       EXEC SQL SET :dw = DAYOFWEEK_ISO(:priData);
000524140605
000525140605       RETURN %CHAR(priData : *EUR) + ' '
000526140605              + %TRIMR(rtvMsgLang(dayOfWeek.msgId(dw) : priLingua));
000527140605
000528140605      /END-FREE
000529140605     P GetDescrizioneGiorno...
000530140605     P                 E
000531140605
000532140610
000533140610     P*--------------------------------------------------
000534140610     P* Procedure name: PutIstruzioniConsegna
000535140610     P* Purpose:        Controlla e imposta le istruzioni di consegna.
000536140610     P* Returns:        Esito.
000537140610     P*--------------------------------------------------
000538140610     P PutIstruzioniConsegna...
000539140610     P                 B
000540140610     D PutIstruzioniConsegna...
000541140610     D                 PI            10I 0
000542140610
000543140610     D retField        S             10I 0 STATIC
000544140610     D fnlry00i1     E DS                  QUALIFIED INZ(*EXTDFT) STATIC
000545140610     D fnlry00o1     E DS                  QUALIFIED INZ(*EXTDFT) STATIC
000546140610     D tis700008     E DS                  QUALIFIED INZ(*EXTDFT) STATIC
000547140610     D tis700009     E DS                  QUALIFIED INZ(*EXTDFT) STATIC
000548140616     D EPOCA1970       C                   Z'1970-01-01-00.00.00.000000'
000549140922     D FNLRY00_MATTINO...
000550140922     D                 C                   'M'
000551140922     D FNLRY00_POMERIGGIO...
000552140922     D                 C                   'P'
000553140616     D tcsSecondi      S             20U 0 STATIC
000554140616     D tcsMicroS       S             10U 0 STATIC
000555140616
000556140610      /FREE
000557140610
000558140610       RESET retField;
000559140610       RESET tis700009;
000560140610       RESET fnlry00i1;
000561140610       RESET fnlry00o1;
000562140610       RESET fnlry00m0;
000563140610
000564140610       tis700008 = %SUBST(rqsDta : 1 : rqsLen);
000565141009
000566141009       // Per gestire il lasciato avviso senza BRTcode occorre il login;
000567141009       // senza login pulisco KSU, così avrò certamente un errore.
000568141009
000569141009       IF tis700008.brtCode = *BLANK AND tis700008.sun = *ZERO;
000570141009         CLEAR tis700008.ksu;
000571141009       ENDIF;
000572141009
000573140610       fnlry00i1.linIso2 = tis700008.lingua;
000574141009       fnlry00i1.cltIpAddr = tis700008.clIpAddr;
000575140610       fnlry00i1.ksu = tis700008.ksu;
000576140903       fnlry00i1.sun = tis700008.sun;
000577141009       fnlry00i1.ksuTimeStN = tis700008.cts;
000578140616       tcsSecondi = %DIV(tis700008.cts : 1000);
000579140616       tcsMicroS = %REM(tis700008.cts : 1000) * 1000 ;
000580140616       fnlry00i1.ksuTimest = EPOCA1970 + %SECONDS(tcsSecondi)
000581140616                                       + %MSECONDS(tcsMicroS);
000582140610       fnlry00i1.idBolla = %SUBST(tis700008.annoBolla : 3 : 2)
000583140610                         + tis700008.nBolla;
000584140610       fnlry00i1.brtCode = tis700008.brtCode;
000585140610       fnlry00i1.tipoEla = tis700008.tpElabor;
000586140610       fnlry00i1.tipoDispo = tis700008.tpIstruz;
000587140610       fnlry00i1.oldRefCons = tis700008.oldDestRef;
000588140610       fnlry00i1.oldTelCons = tis700008.oldDestTlf;
000589140610       fnlry00i1.oldDesMail = tis700008.oldDestEma;
000590140610       fnlry00i1.oldDesCell = tis700008.oldDestMob;
000591140610       fnlry00i1.oldDtCRich = tis700008.oldDtConse;
000592140610       fnlry00i1.oldOrCRich = %SUBST(tis700008.oldOrConse : 1 : 2)
000593140610                            + %SUBST(tis700008.oldOrConse : 4 : 2);
000594140610       fnlry00i1.oldRsD = tis700008.oldDestNom;
000595140610       fnlry00i1.oldInD = tis700008.oldDestInd;
000596140610       fnlry00i1.oldCaD = tis700008.oldDestCap;
000597140610       fnlry00i1.oldLoD = tis700008.oldDestLoc;
000598140610       fnlry00i1.oldPrD = tis700008.oldDestPro;
000599140610       fnlry00i1.newRefCons = tis700008.newDestRef;
000600140610       fnlry00i1.newTelCons = tis700008.newDestTlf;
000601140610       fnlry00i1.newDesMail = tis700008.newDestEma;
000602140610       fnlry00i1.newDesCell = tis700008.newDestMob;
000603140610       fnlry00i1.newDtCRich = tis700008.newDtConse;
000604140610       fnlry00i1.newOrCRich = %SUBST(tis700008.newOrConse : 1 : 2)
000605140610                            + %SUBST(tis700008.newOrConse : 4 : 2);
000606140922
000607140922       SELECT;
000608141007         WHEN tis700008.newMattino = *ON AND tis700008.newPomerig <> *ON;
000609141003           fnlry00i1.prConsRic = FNLRY00_MATTINO;
000610141007         WHEN tis700008.newMattino <> *ON AND tis700008.newPomerig = *ON;
000611141003           fnlry00i1.prConsRic = FNLRY00_POMERIGGIO;
000612141003         OTHER;
000613140922           fnlry00i1.prConsRic = *BLANK;
000614140922       ENDSL;
000615140922
000616140610       fnlry00i1.newRsD = tis700008.newDestNom;
000617140610       fnlry00i1.newInD = tis700008.newDestInd;
000618140610       fnlry00i1.newCaD = tis700008.newDestCap;
000619140610       fnlry00i1.newLoD = tis700008.newDestLoc;
000620140610       fnlry00i1.newPrD = tis700008.newDestPro;
000621141001       fnlry00i1.newDtCRicI = tis700008.newDtConsI;
000622141001       fnlry00i1.newOrCRicI = %SUBST(tis700008.newOrConsI : 1 : 2)
000623141001                            + %SUBST(tis700008.newOrConsI : 4 : 2);
000624141001
000625141001       SELECT;
000626141007         WHEN tis700008.newMattinI = *ON AND tis700008.newPomeriI <> *ON;
000627141003           fnlry00i1.prConsRicI = FNLRY00_MATTINO;
000628141007         WHEN tis700008.newMattinI <> *ON AND tis700008.newPomeriI = *ON;
000629141003           fnlry00i1.prConsRicI = FNLRY00_POMERIGGIO;
000630141003         OTHER;
000631141001           fnlry00i1.prConsRicI = *BLANK;
000632141001       ENDSL;
000633141001
000634141001       fnlry00i1.altro = tis700008.altreIstr;
000635140610
000636140610       MONITOR;
000637140610         Fnlry00_Immetti( FNLRY00_RQSOPCODE_PUT_ISTRUCO
000638140610                        : this.rpyOpCode
000639140610                        : this.rpyIdMsg
000640140610                        : fnlry00i1.formato
000641140610                        : fnlry00i1
000642140610                        : %SIZE(fnlry00i1)
000643140610                        : fnlry00o1.formato
000644140610                        : fnlry00o1
000645140610                        : %SIZE(fnlry00o1)
000646140610                        : fnlry00m0.formato
000647140610                        : fnlry00m0
000648140610                        : %SIZE(fnlry00m0)
000649140610                        );
000650140610         ON-ERROR;
000651140618           this.rpyOpCode = -1;
000652140610           this.rpyIdMsg = %STATUS * -1;
000653140610       ENDMON;
000654140610
000655140610       IF this.rpyOpCode < FNLRY00_ESITO_OK OR fnlry00m0.nrMsg > *ZERO;
000656140610         IF fnlry00m0.nrMsg = *ZERO;
000657141120           rp1error.errCode = %CHAR(this.rpyIdMsg);
000658140610           rp1error.errText = 'CALL a FNLRY00R terminata in modo anomalo.';
000659140610           rpyOpc = OTHERERROR;
000660140610           rpyDta = rp1Error;
000661140610           rpyLen = %SIZE(rp1Error);
000662141119           DUMP(A);
000663140610         ELSE;
000664140610           RESET tis700010;
000665141003           IF tis700008.brtCode <> *BLANK;
000666141003             tis700010.annoBolla = '20' + %SUBST(tis700008.brtCode : 1 : 2);
000667141003             tis700010.nBolla = %SUBST(tis700008.brtCode : 3);
000668141003           ELSE;
000669141003             tis700010.annoBolla = tis700008.annoBolla;
000670141003             tis700010.nBolla = tis700008.nBolla;
000671141003           ENDIF;
000672140610           FOR this.i = 1 TO fnlry00m0.nrMsg;
000673140610             IF fnlry00m0.errWarnAry(this.i) = FNLRY00_MSG_ERRORE;
000674140610               tis700010.nrMsg += 1;
000675140610               tis700010.idMsgAry(tis700010.nrMsg) = fnlry00m0.idMsgAry(this.i);
000676140610               tis700010.idCampoAry(tis700010.nrMsg) =
000677140610                                                   fnlry00m0.idCampoAry(this.i);
000678140610               tis700010.errWarnAry(tis700010.nrMsg) =
000679140610                                                   fnlry00m0.errWarnAry(this.i);
000680140610               tis700010.textMsgAry(tis700010.nrMsg) =
000681140610                                                   fnlry00m0.textMsgAry(this.i);
000682140610               IF tis700010.nrMsg = *HIVAL;
000683140610                 LEAVE;
000684140610               ENDIF;
000685140610             ENDIF;
000686140610           ENDFOR;
000687140610           rpyOpc = tis700010.formato;
000688140610           rpyDta = tis700010;
000689140610           rpyLen = %SIZE(tis700010);
000690140610         ENDIF;
000691140610         RETURN this.rpyOpCode;
000692140610       ENDIF;
000693140610
000694140617       tis700009.msgConfRic = fnlry00o1.msgConferm;
000695141001       tis700009.msgConfSpe = fnlry00o1.msgConferS;
000696140617       tis700009.idFilFD = %EDITC(fnlry00o1.fdIdFil : 'X');
000697140617
000698140610       // Ho finito.
000699140610
000700140610       rpyOpc = tis700009.formato;
000701140610       rpyDta = tis700009;
000702140610       rpyLen = %SIZE(tis700009);
000703140610
000704140610       RETURN retField;
000705140610
000706140610      /END-FREE
000707140610     P PutIstruzioniConsegna...
000708140610     P                 E
000709140610
000710140930
000711140930     P*--------------------------------------------------
000712140930     P* Procedure name: GetAnagraficaFiliale
000713140930     P* Purpose:        Reperisce l'anagrafica della filiale.
000714140930     P* Returns:        Esito.
000715140930     P*--------------------------------------------------
000716140930     P GetAnagraficaFiliale...
000717140930     P                 B
000718140930     D GetAnagraficaFiliale...
000719140930     D                 PI            10I 0
000720140930
000721140930     D retField        S             10I 0 STATIC
000722140930     D tis700011     E DS                  QUALIFIED INZ(*EXTDFT) STATIC
000723140930     D tis700012     E DS                  QUALIFIED INZ(*EXTDFT) STATIC
000724140930     D trulOriI      E DS                  QUALIFIED INZ(*EXTDFT) STATIC
000725140930     D trulOriM      E DS                  QUALIFIED INZ(*EXTDFT) STATIC
000726150109     D  idMsgAry                      7A   DIM(9) OVERLAY(idMsg)
000727150109     D  idCampoAry                   10A   DIM(9) OVERLAY(idCampo)
000728150109     D  errWarnAry                    1A   DIM(9) OVERLAY(errWarn)
000729150109     D  textMsgAry                  255A   DIM(9) OVERLAY(textMsg)
000730140930     D trulOriO      E DS                  QUALIFIED INZ(*EXTDFT) STATIC
000731140930
000732140930      /FREE
000733140930
000734140930       CLEAR retField;
000735140930
000736140930       tis700011 = %SUBST(rqsDta : 1 : rqsLen);
000737140930
000738140930       RESET trulOriI;
000739140930       RESET trulOriM;
000740140930       RESET trulOriO;
000741140930
000742140930       trulOriI.linIso2 = tis700011.lingua;
000743140930       trulOriI.filiale = %EDITC(tis700011.idFiliale : 'X');
000744140930
000745140930       MONITOR;
000746140930         GET_Filiale( TRULORI_RQSOPCODE_GET_FILIALE
000747140930                    : this.rpyOpCode
000748140930                    : this.rpyIdMsg
000749140930                    : trulOriI.formato
000750140930                    : trulOriI
000751140930                    : %SIZE(trulOriI)
000752140930                    : trulOriO.formato
000753140930                    : trulOriO
000754140930                    : %SIZE(trulOriO)
000755140930                    : trulOriM.formato
000756140930                    : trulOriM
000757140930                    : %SIZE(trulOriM)
000758140930                    );
000759140930         ON-ERROR;
000760140930           this.rpyOpCode = -1;
000761140930           this.rpyIdMsg = %STATUS * -1;
000762140930       ENDMON;
000763140930
000764140930       IF this.rpyOpCode < TRULORI_ESITO_OK OR trulOriM.nrMsg > *ZERO;
000765140930         IF trulOriM.nrMsg = *ZERO;
000766141120           rp1error.errCode = %CHAR(this.rpyIdMsg);
000767140930           rp1error.errText = 'CALL a TRULORIR terminata in modo anomalo.';
000768140930           rpyOpc = OTHERERROR;
000769140930           rpyDta = rp1Error;
000770140930           rpyLen = %SIZE(rp1Error);
000771141119           DUMP(A);
000772150109           RETURN this.rpyOpCode;
000773150109         ELSE;
000774150109           RESET tis700010;
000775150109           FOR this.i = 1 TO trulOriM.nrMsg;
000776150109             IF trulOriM.errWarnAry(this.i) = FNLRY00_MSG_ERRORE;
000777150109               tis700010.nrMsg += 1;
000778150109               tis700010.idMsgAry(tis700010.nrMsg) = trulOriM.idMsgAry(this.i);
000779150109               tis700010.idCampoAry(tis700010.nrMsg) =
000780150109                                                   trulOriM.idCampoAry(this.i);
000781150109               tis700010.errWarnAry(tis700010.nrMsg) =
000782150109                                                   trulOriM.errWarnAry(this.i);
000783150109               tis700010.textMsgAry(tis700010.nrMsg) =
000784150109                                                   trulOriM.textMsgAry(this.i);
000785150109               IF tis700010.nrMsg = *HIVAL;
000786150109                 LEAVE;
000787150109               ENDIF;
000788150109             ENDIF;
000789150109           ENDFOR;
000790150109           IF tis700010.nrMsg > *ZERO;
000791150109             rpyOpc = tis700010.formato;
000792150109             rpyDta = tis700010;
000793150109             rpyLen = %SIZE(tis700010);
000794150109             RETURN this.rpyOpCode;
000795150109           ENDIF;
000796140930         ENDIF;
000797140930       ENDIF;
000798140930
000799140930       tis700012.descrizion = trulOriO.oorfDescr;
000800140930       tis700012.indirizzo = trulOriO.oorfInd;
000801140930       tis700012.localita = trulOriO.oorfLoc;
000802140930       tis700012.siglaProv = trulOriO.oorfPro;
000803140930       tis700012.cap = trulOriO.oorfCap;
000804140930       tis700012.telefono = trulOriO.oorfTel;
000805140930       tis700012.emailAddr = trulOriO.oorfMail;
000806140930       tis700012.oraFilMatD = trulOriO.oorfFilM_d + '.00';
000807140930       tis700012.oraFilMatA = trulOriO.oorfFilM_a + '.00';
000808140930       tis700012.oraFilPomD = trulOriO.oorfFilP_d + '.00';
000809140930       tis700012.oraFilPomA = trulOriO.oorfFilP_a + '.00';
000810140930       tis700012.oraFilDesc = trulOriO.oorfOreF_d;
000811140930       tis700012.oraSClMatD = trulOriO.oorfSClM_d + '.00';
000812140930       tis700012.oraSClMatA = trulOriO.oorfSClM_a + '.00';
000813140930       tis700012.oraSClPomD = trulOriO.oorfSClP_d + '.00';
000814140930       tis700012.oraSClPomA = trulOriO.oorfSClP_a + '.00';
000815140930       tis700012.oraSClDesc = trulOriO.oorfOrSC_d;
000816140930       tis700012.urlFiliale = trulOriO.oorfUrl;
000817140930
000818140930       // Ho finito.
000819140930
000820140930       rpyOpc = tis700012.formato;
000821140930       rpyDta = tis700012;
000822140930       rpyLen = %SIZE(tis700012);
000823140930
000824140930       RETURN retField;
000825140930
000826140930      /END-FREE
000827140930     P GetAnagraficaFiliale...
000828140930     P                 E
000829140930
