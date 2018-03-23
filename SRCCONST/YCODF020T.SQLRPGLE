000107161207       CTL-OPT DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('YCO');
000108161205
000135161205       /COPY GAITRASRC/SRCCONST,YCODF
000136161205       /COPY GAITRASRC/SRCCONST,YDOCFLOW
000137161207       /COPY GAITRASRC/SRCCONST,YMSG
000142161205
000143161205       // **********************************************************************
000144161205       //
000145161205       // Strutture.
000146161205       //
000147161205       // **********************************************************************
000148161205
000149161205       DCL-DS kpjba EXT QUALIFIED INZ END-DS;
000150161209       DCL-DS ndmov00f EXT QUALIFIED INZ END-DS;
000151161209       DCL-DS ndreg00f EXT QUALIFIED INZ END-DS;
000152161205       DCL-DS ycodf020i EXT QUALIFIED INZ(*EXTDFT) END-DS;
000158161207       DCL-DS ycodf020o EXT QUALIFIED INZ(*EXTDFT) END-DS;
000159161209       DCL-DS yMsg LIKEDS(yMsg99) INZ(*LIKEDS);
000160161209       DCL-DS docFlowMetadati LIKEDS(docFlowMetadatiT) INZ(*LIKEDS);
000161161207       DCL-DS ydrmdf00f EXT QUALIFIED INZ(*EXTDFT) END-DS;
000163161205
000164161205       DCL-DS this QUALIFIED INZ;
000165161207         rqsOpCode INT(10);
000166161207         rpyOpCode INT(10);
000167161205       END-DS;
000168161205
000169161205       // **********************************************************************
000170161205       //
000171161205       // Prototipi.
000172161205       //
000173161205       // **********************************************************************
000174161205
000175161205       /COPY GAITRASRC/SRCPROTOPR,YCODF020R
000177161205       /COPY GAITRASRC/SRCPROTOPR,YDOCFLOW
000178161209
000179161209       // **********************************************************************
000180161209       //
000181161209       // Campi.
000182161209       //
000183161209       // **********************************************************************
000184161209
000185161209       DCL-S docFlow_esito INT(10) IMPORT;
000186161209
000187161209       // **********************************************************************
000188161209       //
000189161209       // Parametri.
000190161209       //
000191161209       // **********************************************************************
000192161209
000193161209     C     *ENTRY        PLIST
000194161214     C                   PARM                    prmSys            3 0
000195161214     C                   PARM                    prmNrAsReg        9 0
000196161209
000197161205       // **********************************************************************
000198161205       //
000199161205       // Main.
000200161205       //
000201161205       // **********************************************************************
000207161205
000208161205       Ycodf020r( YCODF_RQSOPCODE_KPJBA
000209161205                : this.rpyOpCode
000210161205                : 'KPJBA'
000211161205                : kpjba
000212161205                : %SIZE(kpjba) );
000213161213
000214161213       DOU *INLR;
000233161209
000234161213         // Ho ricevuto il numero assoluto registrazione di Proj;
000235161213         // reperisco un po' di dati dalla registrazione contabile.
000236161213
000237161214         IF %PARMS() > 1 AND prmNrAsReg <> *ZERO;
000238161209
000239161213           EXEC SQL
000240161213             SELECT ndreg00f.regSocieta, ndreg00f.regUnita
000241161213                  , ndreg00f.regDtReg, ndreg00f.regNrReg, ndreg00f.regSerReg
000242161213                  , ndreg00f.regDtDoc, ndreg00f.regNrDoc, ndreg00f.regImporto
000243161213                  , ndmov00f.movKsc
000244161213               INTO :ndreg00f.regSocieta, :ndreg00f.regUnita
000245161213                  , :ndreg00f.regDtReg, :ndreg00f.regNrReg
000246161213                  , :ndreg00f.regSerReg
000247161213                  , :ndreg00f.regDtDoc, :ndreg00f.regNrDoc
000248161213                  , :ndreg00f.regImporto
000249161213                  , :ndmov00f.movKsc
000250161213               FROM ndreg00f
000251161213               JOIN ndmov00f ON ndreg00f.regSys = ndmov00f.movSys
000252161213                            AND ndreg00f.regNrAsReg = ndmov00f.movNrAsReg
000253161213                            AND 1 = ndmov00f.movNrRigaM
000254161214              WHERE ndreg00f.regSys = :prmSys
000255161214                AND ndreg00f.regNrAsReg = :prmNrAsReg
000256161214                AND ndreg00f.regDocIva = '1'
000257161214                AND LEFT(ndreg00f.regSerReg, 1) = '1'
000258161213           ;
000259161209
000260161213           SELECT;
000261161213             WHEN sqlCode < *ZERO;
000262161213               DUMP(A);
000263161214               *INLR = *ON;
000264161214               LEAVE;
000265161213             WHEN sqlCode = 100;
000266161214               *INLR = *ON;
000267161214               LEAVE;
000268161213             OTHER;
000269161213               ycodf020i.eIdDocFlow = DocFlow_GetIdDocFlowByProjNumeroAssoluto
000270161213                                      ( *ZERO : prmNrAsReg );
000298161213               ydrmdf00f.id_docflow = ycodf020i.eIdDocFlow;
000299161213               ydrmdf00f.pjSys = *ZERO;
000300161213               ydrmdf00f.pjNrAsReg = prmNrAsReg;
000301161213               ydrmdf00f.societa = ndreg00f.regSocieta;
000302161213               ydrmdf00f.unita = ndreg00f.regUnita;
000303161213               ydrmdf00f.idFor = ndmov00f.movKsc;
000304161213               ydrmdf00f.dtDoc = ndreg00f.regDtDoc;
000305161213               ydrmdf00f.idDoc = %CHAR(ndreg00f.regNrDoc);
000306161213               ydrmdf00f.impDoc = ndreg00f.regImporto;
000307161213               ydrmdf00f.dtReg = ndreg00f.regDtReg;
000308161213               ydrmdf00f.nrReg = ndreg00f.regNrReg;
000309161213               ydrmdf00f.serReg = ndreg00f.regSerReg;
000310161213               DocFlow_InsMetadati( YDOCFLOW_RQSOPCODE_SOLO_CONTROLLO
000311161213                                  : this.rpyOpCode
000312161213                                  : 'YDRMDF00F'
000313161213                                  : ydrmdf00f
000314161213                                  : %SIZE(ydrmdf00f)
000315161213                                  : yMsg.formato
000316161213                                  : yMsg
000317161213                                  : %SIZE(yMsg)
000318161213                                  : 'YDRMDF00F'
000319161213                                  : ydrmdf00f
000320161213                                  : %SIZE(ydrmdf00f)
000321161213                                  );
000322161213               EVAL-CORR ycodf020i = ydrmdf00f;
000323161213               RESET ycodf020i.formato;
000324161213               RESET ycodf020i.versione;
000325161213               IF ycodf020i.eIdDocFlow <> *BLANK;
000326161213                 docFlowMetadati =
000327161213                 DocFlow_GetMetadatiEsterniByIdDocFlow(ycodf020i.eIdDocFlow);
000328161213                 IF docFlow_esito >= *ZERO AND docFlow_esito <> 100;
000329161213                   IF docFlowMetadati.societa <> *BLANK;
000330161213                     yMsg.msgCount += 1;
000331161213                     yMsg.ary_msgField(yMsg.msgCount) = 'ID_DOCFLOW';
000332161213                     yMsg.ary_msgId(yMsg.msgCount) = *BLANK;
000333161213                     yMsg.ary_msgType(yMsg.msgCount) = YMSG_DSPATR;
000334161213                     yMsg.ary_msgText(yMsg.msgCount) = 'PR';
000338161213                   ENDIF;
000339161213                   ycodf020i.eSocieta = docFlowMetadati.societa;
000340161213                   ycodf020i.eUnita = docFlowMetadati.unita;
000341161213                   ycodf020i.eIdFor = docFlowMetadati.idFor;
000342161213                   ycodf020i.ePiFor = docFlowMetadati.piFor;
000343161213                   ycodf020i.eCfFor = docFlowMetadati.cfFor;
000344161213                   ycodf020i.eRagFor = docFlowMetadati.ragFor;
000345161213                   ycodf020i.eIndFor = docFlowMetadati.indFor;
000346161213                   ycodf020i.eCapFor = docFlowMetadati.capFor;
000347161213                   ycodf020i.eLocFor = docFlowMetadati.locFor;
000348161213                   ycodf020i.eProFor = docFlowMetadati.proFor;
000349161213                   ycodf020i.eNazFor = docFlowMetadati.nazFor;
000350161213                   ycodf020i.eDtDoc = docFlowMetadati.dtDoc;
000351161213                   ycodf020i.eIdDoc = docFlowMetadati.idDoc;
000352161213                   ycodf020i.eImpDoc = docFlowMetadati.impDoc;
000353161213                   ycodf020i.eDtReg = docFlowMetadati.dtReg;
000354161213                   ycodf020i.eNrReg = docFlowMetadati.nrReg;
000355161213                   ycodf020i.eSerReg = docFlowMetadati.serReg;
000356161213                   ycodf020i.eUid = docFlowMetadati.uid;
000357161213                   IF docFlowMetadati.esportato;
000358161213                     ycodf020i.eDatiVari = 'Esportato';
000359161213                   ELSE;
000360161213                     ycodf020i.eDatiVari = 'In attesa di esportazione';
000361161213                   ENDIF;
000362161213                   IF docFlowMetadati.tipoEsp =
000363161213                                               YDOCFLOW_TIPO_ESPORTAZIONE_CLEAR;
000364161213                     ycodf020i.eDatiVari = %TRIMR(ycodf020i.eDatiVari)
000365161213                                         + ' (pulizia)';
000366161213                   ENDIF;
000367161213                 ENDIF;
000368161213               ENDIF;
000369161213           ENDSL;
000370161213         ENDIF;
000371161213
000372161213         DOU this.rpyOpCode = YCODF_RPYOPCODE_F3 OR
000373161213             this.rpyOpCode = YCODF_RPYOPCODE_F5 OR
000374161213             this.rpyOpCode = YCODF_RPYOPCODE_F8 OR
000375161213             this.rpyOpCode = YCODF_RPYOPCODE_F12;
000377161207
000378161213           Ycodf020r( YCODF_RQSOPCODE_GESTIRE_NO_MODIFICARE
000379161213                    : this.rpyOpCode
000380161213                    : ycodf020i.formato
000381161213                    : ycodf020i
000382161213                    : %SIZE(ycodf020i)
000383161213                    : yMsg.formato
000384161213                    : yMsg
000385161213                    : %SIZE(yMsg)
000386161213                    : ycodf020o.formato
000387161213                    : ycodf020o
000388161213                    : %SIZE(ycodf020o) );
000389161207
000390161213           EVAL-CORR ydrmdf00f = ycodf020o;
000391161207
000392161213           SELECT;
000393161213             WHEN this.rpyOpCode = YCODF_RPYOPCODE_INVIO;
000394161213               this.rqsOpCode = YDOCFLOW_RQSOPCODE_SOLO_CONTROLLO;
000395161213             WHEN this.rpyOpCode = YCODF_RPYOPCODE_F3;
000396161213               *INLR = *ON;
000397161213               LEAVE;
000398161213             WHEN this.rpyOpCode = YCODF_RPYOPCODE_F5;
000399161213               RESET ycodf020i;
000400161213               LEAVE;
000401161213             WHEN this.rpyOpCode = YCODF_RPYOPCODE_F8;
000402161213               *INLR = *ON;
000403161213               LEAVE;
000404161213             WHEN this.rpyOpCode = YCODF_RPYOPCODE_F12;
000405161213               *INLR = *ON;
000406161213               LEAVE;
000407161213             WHEN this.rpyOpCode = YCODF_RPYOPCODE_F6;
000408161213               this.rqsOpCode = YDOCFLOW_RQSOPCODE_ESECUZIONE;
000409161213               RESET ydrmdf00f.tipoEsp;
000410161213             WHEN this.rpyOpCode = YCODF_RPYOPCODE_F16;
000411161213               this.rqsOpCode = YDOCFLOW_RQSOPCODE_ESECUZIONE;
000412161213               ydrmdf00f.tipoEsp = YDOCFLOW_TIPO_ESPORTAZIONE_CLEAR;
000413161213           ENDSL;
000414161207
000415161213           IF DocFlow_InsMetadati( this.rqsOpCode
000416161213                                 : this.rpyOpCode
000417161213                                 : 'YDRMDF00F'
000418161213                                 : ydrmdf00f
000419161213                                 : %SIZE(ydrmdf00f)
000420161213                                 : yMsg.formato
000421161213                                 : yMsg
000422161213                                 : %SIZE(yMsg)
000423161213                                 ) < *ZERO;
000424161213           ELSE;
000425161213             EXEC SQL COMMIT;
000426161213           ENDIF;
000427161213
000428161213           docFlowMetadati =
000429161213           DocFlow_GetMetadatiEsterniByIdDocFlow(ydrmdf00f.id_docflow);
000430161213           IF docFlow_esito >= *ZERO AND docFlow_esito <> 100;
000431161213             IF docFlowMetadati.societa <> *BLANK;
000432161213               yMsg.msgCount += 1;
000433161213               yMsg.ary_msgField(yMsg.msgCount) = 'ID_DOCFLOW';
000434161213               yMsg.ary_msgId(yMsg.msgCount) = *BLANK;
000435161213               yMsg.ary_msgType(yMsg.msgCount) = YMSG_DSPATR;
000436161213               yMsg.ary_msgText(yMsg.msgCount) = 'PR';
000437161213             ENDIF;
000438161213             ycodf020i.eIdDocFlow = ydrmdf00f.id_docflow;
000439161213             ycodf020i.eSocieta = docFlowMetadati.societa;
000440161213             ycodf020i.eUnita = docFlowMetadati.unita;
000441161213             ycodf020i.eIdFor = docFlowMetadati.idFor;
000442161213             ycodf020i.ePiFor = docFlowMetadati.piFor;
000443161213             ycodf020i.eCfFor = docFlowMetadati.cfFor;
000444161213             ycodf020i.eRagFor = docFlowMetadati.ragFor;
000445161213             ycodf020i.eIndFor = docFlowMetadati.indFor;
000446161213             ycodf020i.eCapFor = docFlowMetadati.capFor;
000447161213             ycodf020i.eLocFor = docFlowMetadati.locFor;
000448161213             ycodf020i.eProFor = docFlowMetadati.proFor;
000449161213             ycodf020i.eNazFor = docFlowMetadati.nazFor;
000450161213             ycodf020i.eDtDoc = docFlowMetadati.dtDoc;
000451161213             ycodf020i.eIdDoc = docFlowMetadati.idDoc;
000452161213             ycodf020i.eImpDoc = docFlowMetadati.impDoc;
000453161213             ycodf020i.eDtReg = docFlowMetadati.dtReg;
000454161213             ycodf020i.eNrReg = docFlowMetadati.nrReg;
000455161213             ycodf020i.eSerReg = docFlowMetadati.serReg;
000456161213             ycodf020i.eUid = docFlowMetadati.uid;
000457161213             IF docFlowMetadati.esportato;
000458161213               ycodf020i.eDatiVari = 'Esportato';
000459161213             ELSE;
000460161213               ycodf020i.eDatiVari = 'In attesa di esportazione';
000461161213             ENDIF;
000462161213             IF docFlowMetadati.tipoEsp =
000463161213                                         YDOCFLOW_TIPO_ESPORTAZIONE_CLEAR;
000464161213               ycodf020i.eDatiVari = %TRIMR(ycodf020i.eDatiVari)
000465161213                                   + ' (pulizia)';
000466161213             ENDIF;
000467161213           ENDIF;
000468161207
000469161213           EVAL-CORR ycodf020i = ycodf020o;
000470161213           RESET ycodf020i.formato;
000471161213           RESET ycodf020i.versione;
000472161207
000473161213         ENDDO;
000474161213
000475161213       ENDDO;
000476161207
000477161205       RETURN;
