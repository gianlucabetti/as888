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
004200160211
004300160211       //--------------------------------------------------------------
004400160211       //?Definizione schiere.
004500160211       //--------------------------------------------------------------
004600160211
004700160211       //--------------------------------------------------------------
004800160211       //?Definizione campi.
004900160211       //--------------------------------------------------------------
005000160211     d kpjba           s            502a
005100160216     d wIdMsg          s              7a
005200160216     d wIdCampo        s             10a
005300160216     d wParm           s            512a   inz
005400160216     d xx              s              3s 0
005500160211
005600160211       //--------------------------------------------------------------
005700160211       //?Definizione procedure.
005800160211       //--------------------------------------------------------------
005900160211
006000160211       //--------------------------------------------------------------
006100160211       //?Definizione prototipi.
006200160211       //--------------------------------------------------------------
006300160211      /copy gaitrasrc/srcprotopr,FIORC00R
006400160216      /copy gaitrasrc/srcprotopr,RTVMSGLANG
006500160216      /copy gaitrasrc/srcprotopr,TIBS0800R
006600160211
006700160211       //--------------------------------------------------------------
006800160211       //?Definizione parametri programma.
006900160211       //--------------------------------------------------------------
007000160211     d Fiorc00_GetOrm...
007100160211     d                 PI
007200160211     d prmRqsOpCode...
007300160211     d                               10I 0 CONST
007400160211     d prmRpyOpCode...
007500160211     d                               10I 0
007600160211     d prmRpyIdMsg...
007700160211     d                               10I 0
007800160211     d prmRqsFormato...
007900160211     d                               10A   CONST
008000160211     d prmRqsData...
008100160211     d                            32767A   OPTIONS(*VARSIZE)
008200160216     d prmRqsSize...
008300160211     d                               10I 0 CONST
008400160211     d prmRpyFormato...
008500160211     d                               10A   CONST
008600160211     d prmRpyData...
008700160211     d                            32767A   OPTIONS(*VARSIZE)
008800160216     d prmRpySize...
008900160211     d                               10I 0 CONST
009000160216     d prmRpyMsgFormato...
009100160216     d                               10A   CONST OPTIONS(*NOPASS)
009200160216     d prmRpyMsgData...
009300160216     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
009400160216     d prmRpyMsgSize...
009500160216     d                               10I 0 CONST OPTIONS(*NOPASS)
009600160211
009700160211       //--------------------------------------------------------------
009800160211       //?MAIN.
009900160211       //--------------------------------------------------------------
010000160211
010100160211      /free
010200160211
010300160211       //?Operazioni iniziali
010400160211       exsr RoutInz;
010500160211
010600160211       //?Controllo i dati passati
010700160211       exsr Controlla;
010800160216
010900160216       //?Imposto alcuni campi di DFT se non passati
011000160216       exsr DftInput;
011100160211
011200160211       //?Cerco l'ORM legato al NPR ricevuto
011300160211       exsr Elabora;
011400160211
011500160211       //?Operazioni finali
011600160211       exsr RoutEnd;
011700160211
011800160211       //--------------------------------------------------------------
011900160211       //?Operazioni iniziali.                                         ?
012000160211       //--------------------------------------------------------------
012100160211       BEGSR  RoutInz;
012200160211
012300160211         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
012400160211
012500160211         prmRpyOpCode = FIORC00_RPYOPCODE_DONE;
012600160211         prmRpyIdMsg  = FIORC00_ESITO_OK;
012700160211
012800160211       //?Pulisco ds di output
012900160211         reset FIORC0O0;
013000160216         reset FIORC0M0;
013100160211
013200160211         *inLR = *on;
013300160211
013400160211       ENDSR;
013500160211
013600160211       //--------------------------------------------------------------
013700160211       //?Controllo formale dei dati passati.
013800160211       //--------------------------------------------------------------
013900160211       BEGSR  Controlla;
014000160211
014100160211       //?OpCode
014200160211         IF  prmRqsOpCode <> FIORC00_RQSOPCODE_GET;
014300160211           prmRpyOpCode = FIORC00_RPYOPCODE_ERROR;
014400160211           prmRpyIdMsg  = FIORC00_ESITO_RQSOPCODE_SCONOSCIUTO;
014500160211           exsr RoutEnd;
014600160211         ENDIF;
014700160211
014800160211       //?Formato e size RQS
014900160211         IF  prmRqsFormato = fiorc0I0.formato;
015000160216           fiorc0I0 = %SUBST(prmRqsData:1:prmRqsSize);
015100160211         ELSE;
015200160211           prmRpyOpCode = FIORC00_RPYOPCODE_ERROR;
015300160211           prmRpyIdMsg = FIORC00_ESITO_NOME_FORMATO_SCONOSCIUTO;
015400160211           exsr RoutEnd;
015500160211         ENDIF;
015600160216         IF  prmRqsSize > 0;
015700160211         ELSE;
015800160211           prmRpyOpCode = FIORC00_RPYOPCODE_ERROR;
015900160211           prmRpyIdMsg = FIORC00_ESITO_SIZE_DATA_ERRATO;
016000160211           exsr RoutEnd;
016100160211         ENDIF;
016200160211
016300160211       //?Formato e size RPY
016400160211         IF  prmRpyFormato = fiorc0O0.formato;
016500160211         ELSE;
016600160211           prmRpyOpCode = FIORC00_RPYOPCODE_ERROR;
016700160211           prmRpyIdMsg = FIORC00_ESITO_NOME_FORMATO_SCONOSCIUTO;
016800160211           exsr RoutEnd;
016900160211         ENDIF;
017000160216         IF  prmRpySize > 0;
017100160211         ELSE;
017200160211           prmRpyOpCode = FIORC00_RPYOPCODE_ERROR;
017300160211           prmRpyIdMsg = FIORC00_ESITO_SIZE_DATA_ERRATO;
017400160211           exsr RoutEnd;
017500160211         ENDIF;
017600160216
017700160216       //?Formato e size MSG
017800160216         IF  prmRpyMsgFormato = fiorc0M0.formato;
017900160216         ELSE;
018000160216           prmRpyOpCode = FIORC00_RPYOPCODE_ERROR;
018100160216           prmRpyIdMsg = FIORC00_ESITO_NOME_FORMATO_SCONOSCIUTO;
018200160216           exsr RoutEnd;
018300160216         ENDIF;
018400160216         IF  prmRpyMsgSize > 0;
018500160216         ELSE;
018600160216           prmRpyOpCode = FIORC00_RPYOPCODE_ERROR;
018700160216           prmRpyIdMsg = FIORC00_ESITO_SIZE_DATA_ERRATO;
018800160216           exsr RoutEnd;
018900160216         ENDIF;
019000160211
019100160211       ENDSR;
019200160216
019300160216       //--------------------------------------------------------------
019400160216       //?Imposta i DFT se non passati alcuni campi.
019500160216       //--------------------------------------------------------------
019600160216       BEGSR  DftInput;
019700160216
019800160216       //?Se ID lingua non valido o non passato
019900160216       //?assumo italiano
020000160216         IF  fiorc0I0.idlingua = *blanks or
020100160216            (fiorc0I0.idlingua <> FIORC00_ID_LINGUA_IT and
020200160216             fiorc0I0.idlingua <> FIORC00_ID_LINGUA_FR and
020300160216             fiorc0I0.idlingua <> FIORC00_ID_LINGUA_EN and
020400160216             fiorc0I0.idlingua <> FIORC00_ID_LINGUA_DE);
020500160216            fiorc0I0.idlingua = FIORC00_ID_LINGUA_IT;
020600160216            fiorc0M0.nrmsg += 1;
020700160216            fiorc0M0.skIdMsg(fiorc0M0.nrmsg) = '*parm';
020800160216            fiorc0M0.skIdCampo(fiorc0M0.nrmsg) = 'IDLINGUA';
020900160216            fiorc0M0.skErrWarn(fiorc0M0.nrmsg) = FIORC00_MSG_WARNING;
021000160216            fiorc0M0.skTextMsg(fiorc0M0.nrmsg) =
021100160216            'Id lingua non previsto, assunto Italiano';
021200160216         ENDIF;
021300160216
021400160216       ENDSR;
021500160211
021600160211       //--------------------------------------------------------------
021700160211       //?Cerco l'ORM con NPR passato.
021800160211       //--------------------------------------------------------------
021900160211       BEGSR  Elabora;
022000160216
022100160216         exec sql
022200160216         set :fiorc0I0.npr = ucase (:fiorc0I0.npr);
022300160216
022400160216       //?Deve essere lungo dieci caratteri
022500160216         IF  %subst(fiorc0I0.npr:10:1) = *blanks;
022600160216           wIdMsg   = 'TIS0829';
022700160216           wIdCampo = 'NPR';
022800160216           clear wParm;
022900160216           exsr Messaggi;
023000160216           leavesr;
023100160216         ENDIF;
023200160216
023300160216       //?Non deve avere spazi
023400160216         xx = 1;
023500160216         FOR  xx by 1 to %len(fiorc0I0.npr);
023600160216           IF  %subst(fiorc0I0.npr:xx:1) = *blanks;
023700160216             wIdMsg   = 'TIS0830';
023800160216             wIdCampo = 'NPR';
023900160216             clear wParm;
024000160216             exsr Messaggi;
024100160216             leavesr;
024200160216           ENDIF;
024300160216         ENDFOR;
024400160216
024500160216       //?I primi 3 caratteri devono essere delle lettere
024600160216         xx = 1;
024700160216         FOR  xx by 1 to 3;
024800160216           IF  %check(Lettere:%subst(fiorc0I0.npr:xx:1)) > *zeros;
024900160216             wIdMsg   = 'TIS0830';
025000160216             wIdCampo = 'NPR';
025100160216             clear wParm;
025200160216             exsr Messaggi;
025300160216             leavesr;
025400160216           ENDIF;
025500160216         ENDFOR;
025600160216
025700160216       //?Gli ultimi 7 caratteri devono essere dei numeri
025800160216         xx = 4;
025900160216         FOR  xx by 1 to 7;
026000160216           IF  %check(Numeri:%subst(fiorc0I0.npr:xx:1)) > *zeros;
026100160216             wIdMsg   = 'TIS0830';
026200160216             wIdCampo = 'NPR';
026300160216             clear wParm;
026400160216             exsr Messaggi;
026500160216             leavesr;
026600160216           ENDIF;
026700160216         ENDFOR;
026800160211
026900160211         exec sql SELECT OREpoe, OREnsr, OREnor, OREnrv
027000160211                         into :OREpoe, :OREnsr,
027100160211                              :OREnor, :OREnrv
027200160211                  FROM FNORE02L
027300160211                  WHERE ORED10 = :fiorc0i0.NPR;
027400160211
027500160211       //?Non trovato ORM
027600160216         IF  sqlcod <> 0;
027700160216           wIdMsg   = 'TIS0530';
027800160216           wIdCampo = 'NPR';
027900160216           clear wParm;
028000160216           exsr Messaggi;
028100160216           leavesr;
028200160216         ENDIF;
028300160211
028400160211       //?Imposto i dati di output
028500160211         fiorc0O0.poe = OREpoe;
028600160211         fiorc0O0.nsr = OREnsr;
028700160211         fiorc0O0.nor = OREnor;
028800160211         fiorc0O0.nrv = OREnrv;
028900160211
029000160211       ENDSR;
029100160216
029200160216       //--------------------------------------------------------------
029300160216       //?Routine per schierare i messaggi nella ds FIORC0M0.
029400160216       //--------------------------------------------------------------
029500160216       BEGSR  Messaggi;
029600160216
029700160216       //?Se ho già caricato 99 messaggi esco
029800160216         IF  fiorc0M0.nrmsg = 99;
029900160216           exsr RoutEnd;
030000160216           clear fiorc0M0.nrmsg;
030100160216         ENDIF;
030200160216
030300160216         fiorc0M0.nrmsg += 1;
030400160216         fiorc0M0.skIdMsg(fiorc0M0.nrmsg) = wIdMsg;
030500160216         fiorc0M0.skIdCampo(fiorc0M0.nrmsg) = wIdCampo;
030600160216         fiorc0M0.skErrWarn(fiorc0M0.nrmsg) = FIORC00_MSG_ERRORE;
030700160216         IF  wParm <> *blanks;
030800160216           fiorc0M0.skTextMsg (fiorc0M0.nrmsg) =
030900160216           rtvMsgLang(wIdMsg:fiorc0I0.idlingua:wParm);
031000160216         ELSE;
031100160216           fiorc0M0.skTextMsg (fiorc0M0.nrmsg)=
031200160216           rtvMsgLang(wIdMsg:fiorc0I0.idlingua);
031300160216         ENDIF;
031400160216
031500160216       ENDSR;
031600160211
031700160211       //--------------------------------------------------------------
031800160211       //?Operazioni finali.
031900160211       //--------------------------------------------------------------
032000160211       BEGSR  RoutEnd;
032100160216
032200160216         fiorc0O0.idlingua = fiorc0I0.idlingua;
032300160211
032400160216         %subst(prmRpyData:1:prmRpySize) = fiorc0O0;
032500160216         %subst(prmRpyMsgData:1:prmRpyMsgSize) = fiorc0M0;
032600160211
032700160211         return;
032800160211
032900160211       ENDSR;
033000160211
033100160211      /end-free
