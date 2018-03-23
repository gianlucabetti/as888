000100131007       //==============================================================
000200131007       //
000300180111       //?Tis7dp01r - Restituzione id bolla da Numero Parcel DPD
000400140519       //
000500131007       //
000600131007       //==============================================================
000700131009
000800131009       //--------------------------------------------------------------
000900131009       //?Specifiche di controllo.                                     ?
001000131009       //--------------------------------------------------------------
001100180111     H dftactgrp(*no) actgrp(*caller) BNDDIR('TIS')
001200131009
001300131009       //--------------------------------------------------------------
001400131009       //?Dichiarazione file.                                          ?
001500131009       //--------------------------------------------------------------
001600131010
001700180111       // -?File riferimenti DPD
001800180111     ffipnd02l  if   e           k disk
001801180111     ffipnd01l  if   e           k disk    rename(fipnd000:fipnd01)
001803180111       // -?File Bolle di sede
001804180111     ftitas30c  if   e           k disk
001900131009
002000180111       // -?File legami bolla
002100180111     ffnlbl01l  if   e           k disk
002101180112       // -?File Tablle
002102180112     ftabel00f  if   e           k disk
004000131008
004100131007       //--------------------------------------------------------------
004200131007       //?Definizione costanti.                                        ?
004300131007       //--------------------------------------------------------------
004400180111      /copy gaitrasrc/srcconst,tis7dp01r
004500140520
006200140114
006300131007       //--------------------------------------------------------------
006400131007       //?Definizione strutture dati.                                  ?
006500131007       //--------------------------------------------------------------
006600131007       // -?Dati INPUT
006700180111     d tis7dp0i0     e ds                  QUALIFIED INZ(*EXTDFT)
007000131007
007100131007       // -?Dati OUTPUT
007200180111     d tis7dp0o0     e ds                  QUALIFIED INZ(*EXTDFT)
007600131007
007700131007       // -?Messaggi
007800180111     d tis7dp0m0     e ds                  QUALIFIED INZ(*EXTDFT)
007900180112     d  skIdMsg                       7a   dim(9) overlay(IdMsg)
008000180112     d  skIdCampo                    10a   dim(9) overlay(IdCampo)
008100180112     d  skErrWarn                     1a   dim(9) overlay(ErrWarn)
008200180112     d  skTextMsg                   255a   dim(9) overlay(TextMsg)
008300131007
009101180112
009102180112     d ds3a          e ds
013700140606
015600131009       //--------------------------------------------------------------
015700131009       //?Definizione schiere.
015800131009       //--------------------------------------------------------------
015900180112     d rec             S              2    DIM(100) inz
016300131007
016400131007       //--------------------------------------------------------------
016500131007       //?Definizione campi.
016600131007       //--------------------------------------------------------------
016601180111     d w_aas           s                   like(tasaas)
016602180111     d w_lnp           s                   like(taslnp)
016603180111     d w_nrs           s                   like(tasnrs)
016604180111     d w_nsp           s                   like(tasnsp)
016605180111     d w_mgs           s                   like(tasmgs)
016606180112     d w_aasO          s                   like(tasaas)
016607180112     d w_lnpO          s                   like(taslnp)
016608180112     d w_nrsO          s                   like(tasnrs)
016609180112     d w_nspO          s                   like(tasnsp)
016700140527     d wIdMsg          s              7a
016800140527     d wIdCampo        s             10a
016900140527     d wcampo          s             10a
017300180111     d wparcel         s                   like(tis7dp0i0.parceldpd)
017400180111     d wIdlingua       s                   like(tis7dp0i0.liniso2)
020300140521     d wcliente        s             11
020900140528     d wParm           s            512a   inz
021400140604     D Esito           s              1a
022300140611     d wTextMsg        s            255
024200141007     d wtxt            S           2048
024201180112     d i               s              3  0
029000140611      *
029400131008
029500131008       //--------------------------------------------------------------
029600131008       //?Definizione procedure.
029700131008       //--------------------------------------------------------------
033800131008
033900131008       //--------------------------------------------------------------
034000131008       //?Definizione prototipi.
034100131008       //--------------------------------------------------------------
034200180111      /copy gaitrasrc/srcprotopr,tis7dp01r
034700131009      /copy gaitrasrc/srcprotopr,RTVMSGLANG
035400131219
035500131219       //--------------------------------------------------------------
035600131219       //?Definizione parametri programma.
035700131219       //--------------------------------------------------------------
035800180111     D Tis7dp01_GetIdB...
035900140519     D                 PI
036000140519     D prmRqsOpCode...
036100140519     D                               10I 0 CONST
036200140519     D prmRpyOpCode...
036300140519     D                               10I 0
036400140519     D prmRpyIdMsg...
036500140519     D                               10I 0
036600140519     D prmRqsFormato...
036700140519     D                               10A   CONST
036800140519     D prmRqsData...
036900140519     D                            32767A   OPTIONS(*VARSIZE)
037000140519     D prmRqsDataSize...
037100140519     D                               10I 0 CONST
037200140519     D prmRpyFormato...
037300140520     D                               10A   CONST
037400140519     D prmRpyData...
037500140519     D                            32767A   OPTIONS(*VARSIZE)
037600140519     D prmRpyDataSize...
037700140519     D                               10I 0 CONST
037800140519     D prmRpyFormMsg...
037900140519     D                               10A   CONST OPTIONS(*NOPASS)
038000140519     D prmRpyMessage...
038100140519     D                            32767A   OPTIONS(*VARSIZE : *NOPASS)
038200140519     D prmRpyMsgSize...
038300140519     D                               10I 0 CONST OPTIONS(*NOPASS)
038400140519     D prmRpyFormForz...
038500140519     D                               10A   CONST OPTIONS(*NOPASS)
038600140519     D prmRpyForzatu...
038700140519     D                            32767A   OPTIONS(*VARSIZE : *NOPASS)
038800140519     D prmRpyForSize...
038900140519     D                               10I 0 CONST OPTIONS(*NOPASS)
039000131007
039100131007       //--------------------------------------------------------------
039200131007       //?MAIN.
039300131007       //--------------------------------------------------------------
039600131008
039700131008       //?Operazioni iniziali
039800131008       exsr RoutInz;
039900131008
040000140523       //?Controllo formale dei dati di accesso
040100131008       exsr Controlla;
041200180111       //?Reperimento ID Bolla dal numero parcel ricevuto
041300180111       exsr sr_elabora      ;
042300131010
042400131010       //?Operazioni finali
042500131010       exsr RoutEnd;
042600131008
055800131008       //--------------------------------------------------------------
055900131008       //?Operazioni iniziali.                                         ?
056000131008       //--------------------------------------------------------------
056100131008       BEGSR  RoutInz;
056200140211
056400131008
056500180111         prmRpyOpCode = TIS7DP01_RPYOPCODE_DONE;
056600180111         prmRpyIdMsg  = TIS7DP01_ESITO_OK;
056700131009
057200131009       //?Pulisco ds di output
057300180111         reset tis7dp0o0;
057500180111         reset tis7dp0m0;
057501180112       //?Carico schiera dei codici bolla di recupero
057502180112         i=0;
057503180112         setll (1:'3A') tabel00f   ;
057504180112         reade (1:'3A') tabel00f   ;
057505180112         dow not %eof(tabel00f);
057506180112            ds3a=tbluni;
057507180112            if §3arbl='R';
057508180112               i+=1;
057509180112               rec(i)=tblkey;
057510180112            endif;
057511180112            reade (1:'3A') tabel00f   ;
057512180112         enddo                 ;
072900131008
073000131008       ENDSR;
073100131008
073200131008       //--------------------------------------------------------------
073300131008       //?Controllo formale dei dati passati.
073400131008       //--------------------------------------------------------------
073500131008       BEGSR  Controlla;
073600131008
073700180111         clear wparcel;
073701180111         clear widlingua;
074300140519
074400131008       //?OpCode
074500180111         IF  prmRqsOpCode <> TIS7DP01_RQSOPCODE_GET_IDBOLLA   ;
074800180111           prmRpyOpCode = TIS7DP01_RPYOPCODE_ERROR;
074900180111           prmRpyIdMsg  = TIS7DP01_ESITO_RQSOPCODE_SCONOSCIUTO;
075000140210           exsr RoutEnd;
075100131008         ENDIF;
075200140408
075500140408       //?Formato e size RQS
075600180111           IF  prmRqsFormato = tis7dp0I0.formato;
075700180111             tis7dp0I0 = %SUBST(prmRqsData:1:prmRqsDataSize);
075800180111             wparcel=tis7dp0i0.parceldpd;
075801180111             widlingua=tis7dp0i0.liniso2;
076200140408           ELSE;
076300180111             prmRpyOpCode = TIS7DP01_RPYOPCODE_ERROR;
076400180111             prmRpyIdMsg = TIS7DP01_ESITO_NOME_FORMATO_SCONOSCIUTO;
076500140408             exsr RoutEnd;
076600140408           ENDIF;
076700140408           IF  prmRqsDataSize > 0;
076800140408           ELSE;
076900180111             prmRpyOpCode = tis7dp01_RPYOPCODE_ERROR;
077000180111             prmRpyIdMsg = tis7dp01_ESITO_SIZE_DATA_ERRATO;
077100140408             exsr RoutEnd;
077200140408           ENDIF;
077300140408         //?Formato e size RPY
077400180111           IF  prmRpyFormato = tis7dp0O0.formato;
077500140408           ELSE;
077600180111             prmRpyOpCode = tis7dp01_RPYOPCODE_ERROR;
077700180111             prmRpyIdMsg = tis7dp01_ESITO_NOME_FORMATO_SCONOSCIUTO;
077800140408             exsr RoutEnd;
077900140408           ENDIF;
078000140408           IF  prmRpyDataSize > 0;
078100140408           ELSE;
078200180111             prmRpyOpCode = tis7dp01_RPYOPCODE_ERROR;
078300180111             prmRpyIdMsg = tis7dp01_ESITO_SIZE_DATA_ERRATO;
078400140408             exsr RoutEnd;
078500140408           ENDIF;
094800140519
094900180112       //?Numero parcel dpd
095000180111         if wparcel=*blanks;
095100180111           prmRpyOpCode = tis7dp01_RPYOPCODE_ERROR;
095301180130               wIdMsg   = 'TIS0604';
095302180126               wIdCampo = '*PARCEL   ';
095303180126               clear wParm  ;
095304180126               exsr Messaggi;
095305180126               exsr routend;
095400140520         endif;
107600131008       ENDSR;
107601180111       //--------------------------------------------------------------
107602180111       //?Ricerca spedizione dal Numero Parcel dpd ricevuto
107603180111       //--------------------------------------------------------------
107604180111       BEGSR  Sr_elabora;
107605180111
107606180111          clear w_aas;
107607180111          clear w_lnp;
107608180111          clear w_nrs;
107609180111          clear w_nsp;
107610180112          clear w_aasO;
107611180112          clear w_lnpO;
107612180112          clear w_nrsO;
107613180112          clear w_nspO;
107614180111          clear w_mgs;
107616180111
107617180111          setll wparcel fipnd02l;
107618180111          reade wparcel fipnd02l;
107619180111          dow not %eof(fipnd02l);
107620180111        // scarto record di spedizione non ancora su titas
107621180112        // Scarto se spedizione di recupero
107622180111             chain (pndaas:pndlnp:pndnrs:pndnsp) titas30c;
107623180112             if %found(titas30c) and %lookup(tascbo:rec)=0  ;
107626180112        // memorizzo key sped., data sped e se bolla figlia
107627180111        // della bolla più recente
107628180111              if w_aas<tasaas or (W_aas=tasaas and w_mgs<tasmgs);
107629180111                w_aas=tasaas;
107630180111                w_lnp=taslnp;
107631180111                w_nrs=tasnrs;
107632180111                w_nsp=tasnsp;
107633180111                w_mgs=tasmgs;
107634180111        // Verifico se bolla figlia
107635180115                chain (tasaas:taslnp:tasnrs:tasnsp) fnlbl01l;
107636180115                if %found(fnlbl01l);
107637180112                   w_aasO=LBLAAO ;
107638180112                   w_lnpO=LBLLPO ;
107639180112                   w_nrsO=LBLNRO ;
107640180112                   w_nspO=LBLNSO ;
107641180111                endif;
107642180111              endif;
107643180111
107644180111             endif;
107645180111             reade wparcel fipnd02l;
107646180111          enddo;
107647180111        //?Non trovate bolle col numero parcel richiesto
107648180111        if w_nsp=0;
107649180111               prmRpyOpCode = tis7dp01_RPYOPCODE_ERROR;
107650180111               wIdMsg   = 'TIS0868';
107651180111               wIdCampo = '*PARCEL   ';
107652180111               wParm = wparcel;
107653180111               exsr Messaggi;
107654180111               exsr routend;
107655180111        else   ;
107656180111        //?Bolla trovata
107657180112        //? Bolla trovata è una figlia
107658180112           if w_nspO>0  ;
107660180112                 chain (w_aasO:w_lnpO:w_nrsO:w_nspO) fipnd01l;
107661180111                 if %found(fipnd01l);
107662180112        //?         il parcel dell'originale è uguale al parcel della figlia -->restituisco l'origin
107663180111                    if pndipn=wparcel ;
107664180111                      tis7dp0o0.IDB_AAS = pndaas;
107665180111                      tis7dp0o0.IDB_LNP = pndlnp;
107666180111                      tis7dp0o0.IDB_NRS = pndnrs;
107667180111                      tis7dp0o0.IDB_NSP = pndnsp;
107668180111                    else;
107669180111        //?         il parcel della mamma è diverso da quello della figlia --> restituisco la figlia
107670180111                      tis7dp0o0.IDB_AAS = w_aas;
107671180111                      tis7dp0o0.IDB_LNP = w_lnp;
107672180111                      tis7dp0o0.IDB_NRS = w_nrs;
107673180111                      tis7dp0o0.IDB_NSP = w_nsp;
107674180111                    endif;
107675180111                 else;
107677180112        //?      Non trovato il parcel dell'originale --> restituisco la figlia
107678180112                      tis7dp0o0.IDB_AAS = w_aas;
107679180112                      tis7dp0o0.IDB_LNP = w_lnp;
107680180112                      tis7dp0o0.IDB_NRS = w_nrs;
107681180112                      tis7dp0o0.IDB_NSP = w_nsp;
107682180111                 endif;
107684180111           else;
107685180111        //? Se non è una figlia la restituisco senza fare ulteriori ricerche
107686180111            tis7dp0o0.IDB_AAS = w_aas;
107687180111            tis7dp0o0.IDB_LNP = w_lnp;
107688180111            tis7dp0o0.IDB_NRS = w_nrs;
107689180111            tis7dp0o0.IDB_NSP = w_nsp;
107690180111           endif ;
107691180111        endif;
107692180111       ENDSR;
308800140527       //--------------------------------------------------------------
308900180111       //?Routine per schierare i messaggi nella ds TIS7DP0M0
309000140527       //--------------------------------------------------------------
309100140527       BEGSR  Messaggi;
309200140527
309300180112       //?Se ho già caricato 9 messaggi esco
309400180112         IF  tis7dp0M0.nrmsg = 9;
309500140527           exsr RoutEnd;
309600180111           clear tis7dp0M0.nrmsg;
309700140527         ENDIF;
309800140527
309900180111         tis7dp0M0.nrmsg += 1;
310000180111         tis7dp0M0.skIdMsg(tis7dp0M0.nrmsg) = wIdMsg;
310100180111         tis7dp0M0.skIdCampo(tis7dp0M0.nrmsg) = wIdCampo;
310200180111         tis7dp0M0.skErrWarn(tis7dp0M0.nrmsg) = tis7dp01_MSG_ERRORE;
310300140527         IF  wParm <> *blanks;
310400180111           tis7dp0M0.skTextMsg (tis7dp0M0.nrmsg) =
310500140527           rtvMsgLang(wIdMsg:wIdlingua:wParm);
310600140527         ELSE;
310700140611       // Se non ho ID messaggio significa che ho già il testo del messaggio
310800140611           if wIdMsg=*blanks;
310900180111             tis7dp0M0.skTextMsg (tis7dp0M0.nrmsg)=wTextMsg;
311000140611           else;
311100180111             tis7dp0M0.skTextMsg (tis7dp0M0.nrmsg)=
311200140611             rtvMsgLang(wIdMsg:wIdlingua);
311300140611           endif;
311400140527         ENDIF;
311500140527
311600140527       ENDSR;
312500131010
312600131010       //--------------------------------------------------------------
312700131010       //?Operazioni finali.
312800131010       //--------------------------------------------------------------
312900131010       BEGSR  RoutEnd;
313000140408
313300180111          %subst(prmRpyData:1:prmRpyDataSize) = tis7dp0o0;
314200180111          %subst(prmRpyMessage:1:prmRpyMsgSize) = tis7dp0M0;
314300131010
314400131010         return;
314500131010
314600131010       ENDSR;
314700131009
