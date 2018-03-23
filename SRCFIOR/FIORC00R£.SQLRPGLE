000100160211       //==============================================================
000200160211       //
000300160211       //?FIORC00R - T&T ORM per Numero Prenotazione Ritiro
000400160211       //
000500160211       //==============================================================
000600160211
000700160211       //--------------------------------------------------------------
000800160211       //?Specifiche di controllo.                                     ?
000900160211       //--------------------------------------------------------------
001000160211     h DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('TIS':'TIBS':'UBBNDDIR')
001100160211
001200160211       //--------------------------------------------------------------
001300160211       //?Dichiarazione file.                                          ?
001400160211       //--------------------------------------------------------------
001500160211
001600160211       //--------------------------------------------------------------
001700160211       //?Definizione costanti.                                        ?
001800160211       //--------------------------------------------------------------
001900160211      /copy gaitrasrc/srcconst,FIORC00R
002000160216
002100160216     d Lettere         c                   'QAZXSWEDCVFRTGBNHYUJMKIOLP'
002200160216     d Numeri          c                   '0123456789'
002300160211
002400160211       //--------------------------------------------------------------
002500160211       //?Definizione strutture dati.                                  ?
002600160211       //--------------------------------------------------------------
002700160211       // -?Dati INPUT
002800160211     d FIORC0I0      e ds                  QUALIFIED INZ(*EXTDFT)
002900160211
003000160211       // -?Dati OUTPUT
003100160211     d FIORC0O0      e ds                  QUALIFIED INZ(*EXTDFT)
003200160216
003300160216       // -?Messaggi
003400160216     d FIORC0M0      e ds                  QUALIFIED INZ(*EXTDFT)
003500160216     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
003600160216     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
003700160216     d  skErrWarn                     1a   dim(99) overlay(ErrWarn)
003800160216     d  skTextMsg                   255a   dim(99) overlay(TextMsg)
003900160211
004000160211       // -?File Estensione ORM
004100160211     d FNORE00F      e ds                  extname(FNORE00F)
004200160219
004300160219       // -?File ORM da VAS
004400160219     d TIORI00F      e ds                  extname(TIORI00F)
004500160211
004600160211       //--------------------------------------------------------------
004700160211       //?Definizione schiere.
004800160211       //--------------------------------------------------------------
004900160211
005000160211       //--------------------------------------------------------------
005100160211       //?Definizione campi.
005200160211       //--------------------------------------------------------------
005300160211     d kpjba           s            502a
005400160216     d wIdMsg          s              7a
005500160216     d wIdCampo        s             10a
005600160216     d wParm           s            512a   inz
005700160216     d xx              s              3s 0
005800160211
005900160211       //--------------------------------------------------------------
006000160211       //?Definizione procedure.
006100160211       //--------------------------------------------------------------
006200160211
006300160211       //--------------------------------------------------------------
006400160211       //?Definizione prototipi.
006500160211       //--------------------------------------------------------------
006600160211      /copy gaitrasrc/srcprotopr,FIORC00R
006700160216      /copy gaitrasrc/srcprotopr,RTVMSGLANG
006800160216      /copy gaitrasrc/srcprotopr,TIBS0800R
006900160211
007000160211       //--------------------------------------------------------------
007100160211       //?Definizione parametri programma.
007200160211       //--------------------------------------------------------------
007300160211     d Fiorc00_GetOrm...
007400160211     d                 PI
007500160211     d prmRqsOpCode...
007600160211     d                               10I 0 CONST
007700160211     d prmRpyOpCode...
007800160211     d                               10I 0
007900160211     d prmRpyIdMsg...
008000160211     d                               10I 0
008100160211     d prmRqsFormato...
008200160211     d                               10A   CONST
008300160211     d prmRqsData...
008400160211     d                            32767A   OPTIONS(*VARSIZE)
008500160216     d prmRqsSize...
008600160211     d                               10I 0 CONST
008700160211     d prmRpyFormato...
008800160211     d                               10A   CONST
008900160211     d prmRpyData...
009000160211     d                            32767A   OPTIONS(*VARSIZE)
009100160216     d prmRpySize...
009200160211     d                               10I 0 CONST
009300160216     d prmRpyMsgFormato...
009400160216     d                               10A   CONST OPTIONS(*NOPASS)
009500160216     d prmRpyMsgData...
009600160216     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
009700160216     d prmRpyMsgSize...
009800160216     d                               10I 0 CONST OPTIONS(*NOPASS)
009900160211
010000160211       //--------------------------------------------------------------
010100160211       //?MAIN.
010200160211       //--------------------------------------------------------------
010300160211
010400160211      /free
010500160211
010600160211       //?Operazioni iniziali
010700160211       exsr RoutInz;
010800160211
010900160211       //?Controllo i dati passati
011000160211       exsr Controlla;
011100160216
011200160216       //?Imposto alcuni campi di DFT se non passati
011300160216       exsr DftInput;
011400160211
011500160211       //?Cerco l'ORM legato al NPR ricevuto
011600160211       exsr Elabora;
011700160211
011800160211       //?Operazioni finali
011900160211       exsr RoutEnd;
012000160211
012100160211       //--------------------------------------------------------------
012200160211       //?Operazioni iniziali.                                         ?
012300160211       //--------------------------------------------------------------
012400160211       BEGSR  RoutInz;
012500160211
012600160211         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
012700160211
012800160211         prmRpyOpCode = FIORC00_RPYOPCODE_DONE;
012900160211         prmRpyIdMsg  = FIORC00_ESITO_OK;
013000160211
013100160211       //?Pulisco ds di output
013200160211         reset FIORC0O0;
013300160216         reset FIORC0M0;
013400160211
013500160211         *inLR = *on;
013600160211
013700160211       ENDSR;
013800160211
013900160211       //--------------------------------------------------------------
014000160211       //?Controllo formale dei dati passati.
014100160211       //--------------------------------------------------------------
014200160211       BEGSR  Controlla;
014300160211
014400160211       //?OpCode
014500160211         IF  prmRqsOpCode <> FIORC00_RQSOPCODE_GET;
014600160211           prmRpyOpCode = FIORC00_RPYOPCODE_ERROR;
014700160211           prmRpyIdMsg  = FIORC00_ESITO_RQSOPCODE_SCONOSCIUTO;
014800160211           exsr RoutEnd;
014900160211         ENDIF;
015000160211
015100160211       //?Formato e size RQS
015200160211         IF  prmRqsFormato = fiorc0I0.formato;
015300160216           fiorc0I0 = %SUBST(prmRqsData:1:prmRqsSize);
015400160211         ELSE;
015500160211           prmRpyOpCode = FIORC00_RPYOPCODE_ERROR;
015600160211           prmRpyIdMsg = FIORC00_ESITO_NOME_FORMATO_SCONOSCIUTO;
015700160211           exsr RoutEnd;
015800160211         ENDIF;
015900160216         IF  prmRqsSize > 0;
016000160211         ELSE;
016100160211           prmRpyOpCode = FIORC00_RPYOPCODE_ERROR;
016200160211           prmRpyIdMsg = FIORC00_ESITO_SIZE_DATA_ERRATO;
016300160211           exsr RoutEnd;
016400160211         ENDIF;
016500160211
016600160211       //?Formato e size RPY
016700160211         IF  prmRpyFormato = fiorc0O0.formato;
016800160211         ELSE;
016900160211           prmRpyOpCode = FIORC00_RPYOPCODE_ERROR;
017000160211           prmRpyIdMsg = FIORC00_ESITO_NOME_FORMATO_SCONOSCIUTO;
017100160211           exsr RoutEnd;
017200160211         ENDIF;
017300160216         IF  prmRpySize > 0;
017400160211         ELSE;
017500160211           prmRpyOpCode = FIORC00_RPYOPCODE_ERROR;
017600160211           prmRpyIdMsg = FIORC00_ESITO_SIZE_DATA_ERRATO;
017700160211           exsr RoutEnd;
017800160211         ENDIF;
017900160216
018000160216       //?Formato e size MSG
018100160216         IF  prmRpyMsgFormato = fiorc0M0.formato;
018200160216         ELSE;
018300160216           prmRpyOpCode = FIORC00_RPYOPCODE_ERROR;
018400160216           prmRpyIdMsg = FIORC00_ESITO_NOME_FORMATO_SCONOSCIUTO;
018500160216           exsr RoutEnd;
018600160216         ENDIF;
018700160216         IF  prmRpyMsgSize > 0;
018800160216         ELSE;
018900160216           prmRpyOpCode = FIORC00_RPYOPCODE_ERROR;
019000160216           prmRpyIdMsg = FIORC00_ESITO_SIZE_DATA_ERRATO;
019100160216           exsr RoutEnd;
019200160216         ENDIF;
019300160211
019400160211       ENDSR;
019500160216
019600160216       //--------------------------------------------------------------
019700160216       //?Imposta i DFT se non passati alcuni campi.
019800160216       //--------------------------------------------------------------
019900160216       BEGSR  DftInput;
020000160216
020100160216       //?Se ID lingua non valido o non passato
020200160216       //?assumo italiano
020300160216         IF  fiorc0I0.idlingua = *blanks or
020400160216            (fiorc0I0.idlingua <> FIORC00_ID_LINGUA_IT and
020500160216             fiorc0I0.idlingua <> FIORC00_ID_LINGUA_FR and
020600160216             fiorc0I0.idlingua <> FIORC00_ID_LINGUA_EN and
020700160216             fiorc0I0.idlingua <> FIORC00_ID_LINGUA_DE);
020800160216            fiorc0I0.idlingua = FIORC00_ID_LINGUA_IT;
020900160216            fiorc0M0.nrmsg += 1;
021000160216            fiorc0M0.skIdMsg(fiorc0M0.nrmsg) = '*parm';
021100160216            fiorc0M0.skIdCampo(fiorc0M0.nrmsg) = 'IDLINGUA';
021200160216            fiorc0M0.skErrWarn(fiorc0M0.nrmsg) = FIORC00_MSG_WARNING;
021300160216            fiorc0M0.skTextMsg(fiorc0M0.nrmsg) =
021400160216            'Id lingua non previsto, assunto Italiano';
021500160216         ENDIF;
021600160216
021700160216       ENDSR;
021800160211
021900160211       //--------------------------------------------------------------
022000160211       //?Cerco l'ORM con NPR passato.
022100160211       //--------------------------------------------------------------
022200160211       BEGSR  Elabora;
022300160216
022400160216         exec sql
022500160216         set :fiorc0I0.npr = ucase (:fiorc0I0.npr);
022600160216
022700160216       //?Deve essere lungo dieci caratteri
022800160216         IF  %subst(fiorc0I0.npr:10:1) = *blanks;
022900160216           wIdMsg   = 'TIS0829';
023000160216           wIdCampo = 'NPR';
023100160216           clear wParm;
023200160216           exsr Messaggi;
023300160216           leavesr;
023400160216         ENDIF;
023500160216
023600160216       //?Non deve avere spazi
023700160216         xx = 1;
023800160216         FOR  xx by 1 to %len(fiorc0I0.npr);
023900160216           IF  %subst(fiorc0I0.npr:xx:1) = *blanks;
024000160216             wIdMsg   = 'TIS0830';
024100160216             wIdCampo = 'NPR';
024200160216             clear wParm;
024300160216             exsr Messaggi;
024400160216             leavesr;
024500160216           ENDIF;
024600160216         ENDFOR;
024700160216
024800160216       //?I primi 3 caratteri devono essere delle lettere
024900160216         xx = 1;
025000160216         FOR  xx by 1 to 3;
025100160216           IF  %check(Lettere:%subst(fiorc0I0.npr:xx:1)) > *zeros;
025200160216             wIdMsg   = 'TIS0830';
025300160216             wIdCampo = 'NPR';
025400160216             clear wParm;
025500160216             exsr Messaggi;
025600160216             leavesr;
025700160216           ENDIF;
025800160216         ENDFOR;
025900160216
026000160216       //?Gli ultimi 7 caratteri devono essere dei numeri
026100160216         xx = 4;
026200160216         FOR  xx by 1 to 7;
026300160216           IF  %check(Numeri:%subst(fiorc0I0.npr:xx:1)) > *zeros;
026400160216             wIdMsg   = 'TIS0830';
026500160216             wIdCampo = 'NPR';
026600160216             clear wParm;
026700160216             exsr Messaggi;
026800160216             leavesr;
026900160216           ENDIF;
027000160216         ENDFOR;
027100160211
027200160211         exec sql SELECT OREpoe, OREnsr, OREnor, OREnrv
027300160211                         into :OREpoe, :OREnsr,
027400160211                              :OREnor, :OREnrv
027500160211                  FROM FNORE02L
027600160211                  WHERE ORED10 = :fiorc0i0.NPR;
027700160211
027800160211       //?Non trovato ORM
027900160216         IF  sqlcod <> 0;
028000160216           wIdMsg   = 'TIS0530';
028100160216           wIdCampo = 'NPR';
028200160216           clear wParm;
028300160216           exsr Messaggi;
028400160216           leavesr;
028500160216         ENDIF;
028600160219
028700160219       //?Se utente LOGGATO
028800160219       //?controllo che l'ORM trovato sia veramente suo
028900160219         IF  fiorc0I0.unificante <> *blanks and
029000160219             fiorc0I0.unificante <> *zeros;
029100160219
029200160219         exec sql SELECT ORIidc into :ORIidc
029300160219                  FROM TIORI05L
029400160219                  WHERE ORIpoe = :OREpoe and ORInsr = :OREnsr and
029500160219                        ORInor = :OREnor and ORInrv = :OREnrv;
029600160219         //?Non trovato ORM
029700160219           IF  sqlcod <> 0;
029800160219             wIdMsg   = 'TIS0530';
029900160219             wIdCampo = 'NPR';
030000160219             clear wParm;
030100160219             exsr Messaggi;
030200160219             leavesr;
030300160219           ENDIF;
030400160219         //?Deve essere dello stesso cliente
030500160219           IF  ORIidc <> fiorc0I0.unificante;
030600160219             wIdMsg   = 'TIS0530';
030700160219             wIdCampo = 'NPR';
030800160219             clear wParm;
030900160219             exsr Messaggi;
031000160219             leavesr;
031100160219           ENDIF;
031200160219         ENDIF;
031300160211
031400160211       //?Imposto i dati di output
031500160211         fiorc0O0.poe = OREpoe;
031600160211         fiorc0O0.nsr = OREnsr;
031700160211         fiorc0O0.nor = OREnor;
031800160211         fiorc0O0.nrv = OREnrv;
031900160211
032000160211       ENDSR;
032100160216
032200160216       //--------------------------------------------------------------
032300160216       //?Routine per schierare i messaggi nella ds FIORC0M0.
032400160216       //--------------------------------------------------------------
032500160216       BEGSR  Messaggi;
032600160216
032700160216       //?Se ho già caricato 99 messaggi esco
032800160216         IF  fiorc0M0.nrmsg = 99;
032900160216           exsr RoutEnd;
033000160216           clear fiorc0M0.nrmsg;
033100160216         ENDIF;
033200160216
033300160216         fiorc0M0.nrmsg += 1;
033400160216         fiorc0M0.skIdMsg(fiorc0M0.nrmsg) = wIdMsg;
033500160216         fiorc0M0.skIdCampo(fiorc0M0.nrmsg) = wIdCampo;
033600160216         fiorc0M0.skErrWarn(fiorc0M0.nrmsg) = FIORC00_MSG_ERRORE;
033700160216         IF  wParm <> *blanks;
033800160216           fiorc0M0.skTextMsg (fiorc0M0.nrmsg) =
033900160216           rtvMsgLang(wIdMsg:fiorc0I0.idlingua:wParm);
034000160216         ELSE;
034100160216           fiorc0M0.skTextMsg (fiorc0M0.nrmsg)=
034200160216           rtvMsgLang(wIdMsg:fiorc0I0.idlingua);
034300160216         ENDIF;
034400160216
034500160216       ENDSR;
034600160211
034700160211       //--------------------------------------------------------------
034800160211       //?Operazioni finali.
034900160211       //--------------------------------------------------------------
035000160211       BEGSR  RoutEnd;
035100160216
035200160216         fiorc0O0.idlingua = fiorc0I0.idlingua;
035300160211
035400160216         %subst(prmRpyData:1:prmRpySize) = fiorc0O0;
035500160216         %subst(prmRpyMsgData:1:prmRpyMsgSize) = fiorc0M0;
035600160211
035700160211         return;
035800160211
035900160211       ENDSR;
036000160211
036100160211      /end-free
