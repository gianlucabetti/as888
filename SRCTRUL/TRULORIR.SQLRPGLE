000100131007       //==============================================================
000200131007       //
000300140925       //?TRULORIR - VISUALIZZA ORARI DI FILIALE DA INTERNET
000400140519       //
000500131007       //
000600131007       //==============================================================
000700131009
000800131009       //--------------------------------------------------------------
000900131009       //?Specifiche di controllo.                                     ?
001000131009       //--------------------------------------------------------------
001100140528     H dftactgrp(*no) actgrp(*caller) BNDDIR('TIS':'UBBNDDIR')
001200131009
001300131009       //--------------------------------------------------------------
001400131009       //?Dichiarazione file.                                          ?
001500131009       //--------------------------------------------------------------
001600131010
001700131010       // -?File organigramma
001800131010     fAZORG01L  if   e           k disk
001900131008
002000131007       //--------------------------------------------------------------
002100131007       //?Definizione costanti.                                        ?
002200131007       //--------------------------------------------------------------
002300140929     D digits          c                   '0123456789'
002400141002
002500141002     D MSGID_LUNEDI...
002600141002     D                 C                   'TIS0351'
002700141002     D MSGID_MARTEDI...
002800141002     D                 C                   'TIS0354'
002900141002     D MSGID_MERCOLEDI...
003000141002     D                 C                   'TIS0356'
003100141002     D MSGID_GIOVEDI...
003200141002     D                 C                   'TIS0203'
003300141002     D MSGID_VENERDI...
003400141002     D                 C                   'TIS0607'
003500141002     D MSGID_SABATO...
003600141002     D                 C                   'TIS0533'
003700141002     D MSGID_DOMENICA...
003800141002     D                 C                   'TIS0159'
003900141002
004000140925      /copy gaitrasrc/srcconst,TRULORIR
004100140114
004200131007       //--------------------------------------------------------------
004300131007       //?Definizione strutture dati.                                  ?
004400131007       //--------------------------------------------------------------
004500140929
004600131007       // -?Dati INPUT
004700140925     d TRULORIi      e ds                  QUALIFIED INZ(*EXTDFT)
004800140925       // -?Dati OUTPUT
004900140925     d TRULORIo      e ds                  QUALIFIED INZ(*EXTDFT)
005000131007
005100131007       // -?Messaggi
005200140925     d TRULORIm      e ds                  QUALIFIED INZ(*EXTDFT)
005300140522     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
005400140522     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
005500140522     d  skErrWarn                     1a   dim(99) overlay(ErrWarn)
005600140522     d  skTextMsg                   255a   dim(99) overlay(TextMsg)
005700140521
005800140925     d TRULORFDS     e ds
005900140929     d TRULORFDSO    e ds
006000140604     d KPJBA         e ds
006100140312
006200131009       //--------------------------------------------------------------
006300131009       //?Definizione schiere.
006400131009       //--------------------------------------------------------------
006500140210
006600140210     d skFIdMsg        s              7a   dim(99)
006700140210     d skFIdCampo      s             10a   dim(99)
006800131007
006900131007       //--------------------------------------------------------------
007000131007       //?Definizione campi.
007100131007       //--------------------------------------------------------------
007200140527     d wIdMsg          s              7a
007300140527     d wIdCampo        s             10a
007400140527     d wcampo          s             10a
007500140926     d wfil            s                   like(TRULORIi.filiale)
007600140926     d wIdlingua       s                   like(TRULORIi.liniso2)
007700140521     D Wlibfil         S             21
007800140521     D Wlibfilprd      S             21    inz('FILTRAPRD/        ')
007900140521     D Wlibfil201      S             21    INZ('FILTRA201/        ')
008000140521     d dataeur         s               d   datfmt(*eur)
008100140528     d dataiso         s               d   datfmt(*iso)
008200140611     d wTextMsg        s            255
008300140929     d wParm           s            512a   inz
008400141002     d wdesLUN         s             20a
008500141002     d wdesVEN         s             20a
008600131008
008700131008       //--------------------------------------------------------------
008800131008       //?Definizione prototipi.
008900131008       //--------------------------------------------------------------
009000140926      /copy gaitrasrc/srcprotopr,trulorir
009100140926      /copy gaitrasrc/srcprotopr,trulorfr
009200131009      /copy gaitrasrc/srcprotopr,RTVMSGLANG
009300131219
009400131219       //--------------------------------------------------------------
009500131219       //?Definizione parametri programma.
009600131219       //--------------------------------------------------------------
009700140926     D GET_Filiale...
009800140519     D                 PI
009900140519     D prmRqsOpCode...
010000140519     D                               10I 0 CONST
010100140519     D prmRpyOpCode...
010200140519     D                               10I 0
010300140519     D prmRpyIdMsg...
010400140519     D                               10I 0
010500140519     D prmRqsFormato...
010600140519     D                               10A   CONST
010700140519     D prmRqsData...
010800140519     D                            32767A   OPTIONS(*VARSIZE)
010900140519     D prmRqsDataSize...
011000140519     D                               10I 0 CONST
011100140519     D prmRpyFormato...
011200140520     D                               10A   CONST
011300140519     D prmRpyData...
011400140519     D                            32767A   OPTIONS(*VARSIZE)
011500140519     D prmRpyDataSize...
011600140519     D                               10I 0 CONST
011700140519     D prmRpyFormMsg...
011800140519     D                               10A   CONST OPTIONS(*NOPASS)
011900140519     D prmRpyMessage...
012000140519     D                            32767A   OPTIONS(*VARSIZE : *NOPASS)
012100140519     D prmRpyMsgSize...
012200140519     D                               10I 0 CONST OPTIONS(*NOPASS)
012300131007       //--------------------------------------------------------------
012400131007       //?MAIN.
012500131007       //--------------------------------------------------------------
012600131008
012700131007      /free
012800131008
012900131008       //?Operazioni iniziali
013000131008       exsr RoutInz;
013100131008
013200140523       //?Controllo formale dei dati di accesso
013300131008       exsr Controlla;
013400140529
013500140926       //?REcupero dati di filiale
013600140926       exsr sr_chainFiliale ;
013700140926
013800140926       //?Scrivo DS di output
013900140926       exsr sr_getDatiFil   ;
014000131010
014100131010       //?Operazioni finali
014200131010       exsr RoutEnd;
014300131008
014400131008       //--------------------------------------------------------------
014500131008       //?Operazioni iniziali.                                         ?
014600131008       //--------------------------------------------------------------
014700131008       BEGSR  RoutInz;
014800140211
014900140211         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
015000131008
015100140926         prmRpyOpCode = TRULORI_RPYOPCODE_DONE;
015200140926         prmRpyIdMsg  = TRULORI_ESITO_OK;
015300131009
015400131009       //?Pulisco ds di output
015500140926         reset trulorio ;
015600140926         reset trulorim ;
015700140528
015800131008         *inLR = *on;
015900131008
016000131008       ENDSR;
016100131008
016200131008       //--------------------------------------------------------------
016300131008       //?Controllo formale dei dati passati.
016400131008       //--------------------------------------------------------------
016500131008       BEGSR  Controlla;
016600131008
016700140527         clear widlingua;
016800140926         clear wfil     ;
016900140519
017000131008       //?OpCode
017100140926         IF  prmRqsOpCode <> TRULORI_RQSOPCODE_GET_FILIALE ;
017200140926           prmRpyOpCode = TRULORI_RPYOPCODE_ERROR;
017300140926           prmRpyIdMsg  = TRULORI_ESITO_RQSOPCODE_SCONOSCIUTO;
017400140210           exsr RoutEnd;
017500131008         ENDIF;
017600140408
017700140926       //?per GET_FILIALE
017800140926         IF  prmRqsOpCode = TRULORI_RQSOPCODE_GET_FILIALE;
017900140408       //?Formato e size RQS
018000140926           IF  prmRqsFormato = trulorii.formato;
018100140926             trulorii = %SUBST(prmRqsData:1:prmRqsDataSize);
018200140926             widlingua=trulorii.liniso2;
018300140926             wfil     =trulorii.filiale;
018400140408           ELSE;
018500140926             prmRpyOpCode = TRULORI_RPYOPCODE_ERROR;
018600140926             prmRpyIdMsg = TRULORI_ESITO_NOME_FORMATO_SCONOSCIUTO;
018700140408             exsr RoutEnd;
018800140408           ENDIF;
018900140408           IF  prmRqsDataSize > 0;
019000140408           ELSE;
019100140926             prmRpyOpCode = TRULORI_RPYOPCODE_ERROR;
019200140926             prmRpyIdMsg = TRULORI_ESITO_SIZE_DATA_ERRATO;
019300140408             exsr RoutEnd;
019400140408           ENDIF;
019500140408         //?Formato e size RPY
019600140926           IF  prmRpyFormato = trulorio.formato;
019700140408           ELSE;
019800140926             prmRpyOpCode = TRULORI_RPYOPCODE_ERROR;
019900140926             prmRpyIdMsg = TRULORI_ESITO_NOME_FORMATO_SCONOSCIUTO;
020000140408             exsr RoutEnd;
020100140408           ENDIF;
020200140408           IF  prmRpyDataSize > 0;
020300140408           ELSE;
020400140926             prmRpyOpCode = TRULORI_RPYOPCODE_ERROR;
020500140926             prmRpyIdMsg = TRULORI_ESITO_SIZE_DATA_ERRATO;
020600140408             exsr RoutEnd;
020700140408           ENDIF;
020800140408         ENDIF;
020900140210
021000140522       //?Formato e size MSG
021100140926         IF  prmRpyFormMsg = TRULORIM.formato;
021200140522         ELSE;
021300140926           prmRpyOpCode = TRULORI_RPYOPCODE_ERROR;
021400140926           prmRpyIdMsg = TRULORI_ESITO_NOME_FORMATO_SCONOSCIUTO;
021500140522           exsr RoutEnd;
021600140522         ENDIF;
021700140522         IF  prmRpyMsgSize > 0;
021800140522         ELSE;
021900140926           prmRpyOpCode = TRULORI_RPYOPCODE_ERROR;
022000140926           prmRpyIdMsg = TRULORI_ESITO_SIZE_DATA_ERRATO;
022100140522           exsr RoutEnd;
022200140522         ENDIF;
022300140519
022400140926       //?devo ricevere la filiale
022500140926         if wfil   =*blanks or wfil =*zeros ;
022600140926           prmRpyOpCode = TRULORI_RPYOPCODE_ERROR;
022700140926           prmRpyIdMsg = TRULORI_ESITO_ID_RICHIAMO_ERRATO;
022800140520           exsr RoutEnd;
022900140520         endif;
023000140926
023100140926       //?Controllo la filiale
023200140926         if wfil <>*blanks;
023300140926
023400140520       // Deve contenere solo numeri
023500140926         if %check(digits:wfil)>0;
023600140926              prmRpyOpCode = TRULORI_RPYOPCODE_ERROR;
023700140926              wIdMsg   = 'TIS0783';
023800140926              wIdCampo = 'FILIALE   ';
023900140527              clear wParm;
024000140527              exsr Messaggi;
024100140520              exsr RoutEnd;
024200140520         endif;
024300140929         endif;
024400131008       ENDSR;
024500140926       //--------------------------------------------------------------
024600140926       //?Richiama TRULORF per recupere i dati di filiale
024700140926       //--------------------------------------------------------------
024800140926       BEGSR  sr_chainFiliale;
024900140926
025000140926       clear TRULORFDS ;
025100140926       clear TRULORFDSO;
025200140926       iorffil=%int(wfil)  ;
025300140929       iorflin=widlingua  ;
025400140929       callp TRULORFR    (kpjba:trulorfds:trulorfdso) ;
025500140926
025600140926       if oorferr<>' '  ;
025700140926              prmRpyOpCode = TRULORI_RPYOPCODE_ERROR;
025800140926              wIdMsg   = 'TIS0783';
025900140926              wIdCampo = 'FILIALE   ';
026000140926              clear wParm;
026100140926              exsr Messaggi;
026200140926              exsr RoutEnd;
026300140926         endif;
026400140926       ENDSR ;
026500140522       //--------------------------------------------------------------
026600140926       //?Recupera dati filiale da passare a chiamante
026700140522       //--------------------------------------------------------------
026800140926       BEGSR  sr_GetDatiFil  ;
026900140926
027000140929             TRULORIo.OORFDESCR=OORFDES   ;
027100140929             TRULORIo.OORFind  =OORFind   ;
027200140929             TRULORIo.OORFloc  =OORFloc   ;
027300140929             TRULORIo.OORFpro  =OORFpro   ;
027400140929             TRULORIo.OORFcap  =OORFcap   ;
027500141001             TRULORIo.OORFtel  =OORFtel   ;
027600140929             TRULORIo.OORFfax  =OORFfax   ;
027700140929             TRULORIo.OORFmail =OORFmail  ;
027800140929             TRULORIo.OORFFILM_D=%editw(OORFFMD:'  :  ');
027900140929             TRULORIo.OORFFILM_A=%editw(OORFFMA:'  :  ');
028000140929             TRULORIo.OORFFILp_D=%editw(OORFFpD:'  :  ');
028100140929             TRULORIo.OORFFILp_A=%editw(OORFFpA:'  :  ');
028200141002             // oltre agli orari devo mettere anche la scritta dal lun al ven
028300141002               wdesLUN=rtvMsgLang(MSGID_LUNEDI : widlingua);
028400141002               wdesVEN=rtvMsgLang(MSGID_VENERDI : widlingua);
028500141002             wparm=wdesLUN+wdesVEN ;
028600141002             TRULORIo.OORFOREF_D=rtvMsgLang('TIS0793' : widlingua:wparm);
028700141002             TRULORIo.OORFOREF_D=%trimr(TRULORIo.OORFOREF_D)+' '+OORFOREF_D;
028800140929             TRULORIo.OORFsclM_D=%editw(OORFaMD:'  :  ');
028900140929             TRULORIo.OORFsclM_A=%editw(OORFaMA:'  :  ');
029000140929             TRULORIo.OORFsclp_D=%editw(OORFapD:'  :  ');
029100140929             TRULORIo.OORFsclp_A=%editw(OORFapA:'  :  ');
029200141002             TRULORIo.OORFORsc_D=rtvMsgLang('TIS0793' : widlingua:wparm);
029300141002             TRULORIo.OORFORsc_D=%trimr(TRULORIo.OORFORsc_D)+' '+OORFORsc_D;
029400140929             TRULORIo.OORFurl   =OORFurl   ;
029500140522       endsr;
029600140604
029700140527       //--------------------------------------------------------------
029800140926       //?Routine per schierare i messaggi nella ds TRULORIM0
029900140527       //--------------------------------------------------------------
030000140527       BEGSR  Messaggi;
030100140527
030200140527       //?Se ho già caricato 99 messaggi esco
030300140926         IF  TRULORIM.nrmsg = 99;
030400140527           exsr RoutEnd;
030500140926           clear TRULORIM.nrmsg;
030600140527         ENDIF;
030700140527
030800140926         TRULORIM.nrmsg += 1;
030900140926         TRULORIM.skIdMsg(TRULORIM.nrmsg) = wIdMsg;
031000140926         TRULORIM.skIdCampo(TRULORIM.nrmsg) = wIdCampo;
031100140926         TRULORIM.skErrWarn(TRULORIM.nrmsg) = TRULORI_MSG_ERRORE;
031200140527         IF  wParm <> *blanks;
031300140926           TRULORIM.skTextMsg (TRULORIM.nrmsg) =
031400140527           rtvMsgLang(wIdMsg:wIdlingua:wParm);
031500140527         ELSE;
031600140611       // Se non ho ID messaggio significa che ho già il testo del messaggio
031700140611           if wIdMsg=*blanks;
031800140926             TRULORIM.skTextMsg (TRULORIM.nrmsg)=wTextMsg;
031900140611           else;
032000140926             TRULORIM.skTextMsg (TRULORIM.nrmsg)=
032100140611             rtvMsgLang(wIdMsg:wIdlingua);
032200140611           endif;
032300140527         ENDIF;
032400140527
032500140527       ENDSR;
032600140527
032700140521       //--------------------------------------------------------------
032800140408       //?Carico tabelle.
032900140408       //--------------------------------------------------------------
033000140408       BEGSR  CaricaTab;
033100140408
033200140408
033300140408       ENDSR;
033400131010
033500131010       //--------------------------------------------------------------
033600131010       //?Operazioni finali.
033700131010       //--------------------------------------------------------------
033800131010       BEGSR  RoutEnd;
033900140408
034000140519       //?Se richiamato per GET_ISTRUCO
034100140926         IF  prmRqsOpCode = TRULORI_RQSOPCODE_GET_filiale;
034200140926           %subst(prmRpyData:1:prmRpyDataSize) = TRULORIo;
034300140408         ENDIF;
034400140530
034500140926          %subst(prmRpyMessage:1:prmRpyMsgSize) = TRULORIM;
034600131010
034700131010         return;
034800131010
034900131010       ENDSR;
035000131009
035100131007      /end-free
