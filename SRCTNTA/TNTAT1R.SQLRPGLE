000100160211       //==============================================================
000200160211       //?TNTAT1R - Controllo tipologia Tariffa tra tariffe/offerte    ?
000300160211       //?          dello stesso cliente.                              ?
000400160211       //==============================================================
000500160211
000600160211       //--------------------------------------------------------------
000700160211       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800160211       //--------------------------------------------------------------
000900160211
001000160211     /*PRM  dbgview(*source)
001100160211     /*END
001200160211
001300160211       //--------------------------------------------------------------
001400160211       //?Specifiche di controllo.                                     ?
001500160211       //--------------------------------------------------------------
001600160211
001700160211     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800160211     h dftactgrp(*no) actgrp(*caller)
001900160211     h alwnull(*inputonly)
002000160211
002100160211       //--------------------------------------------------------------
002200160211       //?Dichiarazione file.                                          ?
002300160211       //--------------------------------------------------------------
002400160211
002500160211      *// // -?Lista Errori eventualmente rilevati?
002600160211     f*//QSYSPRT   o    f  132        printer usropn
002700160211     f*//                                     oflind(*inOF)
002800160211
002900160211       //--------------------------------------------------------------
003000160211       //?Definizione costanti.                                        ?
003100160211       //--------------------------------------------------------------
003200160211
003300160211
003400160211       //--------------------------------------------------------------
003500160211       //?Definizione schiere.                                         ?
003600160211       //--------------------------------------------------------------
003700160211
003800160211       // -?Messaggi di errore?
003900160212     d sk_Msg          s             78    dim( 9)  ctdata  perrcd( 1)
004000160211
004100160211       //--------------------------------------------------------------
004200160211       //?Definizione aree dati.                                       ?
004300160211       //--------------------------------------------------------------
004400160211
004500160211       // -?Dati utente?
004600160211     d §AzUte        e ds                  extname(AZUTE00F)
004700160211     d                                     dtaara
004800160211     d §DatiUte      e ds                  extname(dDatiUte)
004900160211     d                                     dtaara
005000160211
005100160211       //--------------------------------------------------------------
005200160211       //?Definizione strutture dati.                                  ?
005300160211       //--------------------------------------------------------------
005400160211
005500160211       // -?Status ds?
005600160211     d Status         sds
005700160211     d   SDSpgm          *proc
005800160211     d*//SDSprm          *parms
005900160211     d*//SDSdta              191    198
006000160211     d*//SDSjob              244    253
006100160211     d   SDSusr              254    263
006200160211
006300160211       // -?Parametri ricevuti
006400160211     d KPJBA         e ds
006500160211     d TNTAT1ds      e ds                  inz
006600160301
006700160301       // -?DS esterna x flag operativi di TNTAM: TAMFLO?
006800160301     d dSTA01        e ds                  inz  qualified
006900160211
007000160211       // -?Dati elaborati via SQL?
007100160211     d TNTAM_ds      e ds                  extname(TNTAM00F)
007200160211     d                                     inz  qualified
007300160211     d wFile           s              1a   inz
007400160211
007500160211       //--------------------------------------------------------------
007600160211       //?Definizione variabili globali.                               ?
007700160211       //--------------------------------------------------------------
007800160211
007900160211       // -?Flags booleani?
008000160211     d $EoF            s               n   inz
008100160211
008200160211       // -?Stringa SQL da eseguire?
008300160211     d wSQL            s           5000    inz  varying
008400160211
008500160211       // -?Data *ISO?
008600160211     d wDate_Iso       s               d   datfmt(*ISO)    inz(*loval)
008700160211       // -?Data *EUR?
008800160211     d wDate_Eur       s               d   datfmt(*EUR)    inz(*loval)
008900160211
009000160211       //--------------------------------------------------------------
009100160211       //?Definizione prototipi procedure e relativi parametri.        ?
009200160211       //--------------------------------------------------------------
009300160211
009400160211       // -?Reperimento dati utente?
009500160211     d TIBS34ds      e ds                  inz
009600160211      /copy gaitrasrc/srcProtoPR,TIBS34R
009700160211
009800160211       // -?Parametri API QCAPCMD (Process Commands)?
009900160211     d Qcmd            s           2048    inz  varying
010000160211      /copy qSysInc/qRpgleSrc,QCAPCMD
010100160211       // -?API QCAPCMD (Process Commands)?
010200160211      /copy gaitrasrc/srcProtoPR,QCAPCMD
010300160211
010400160211       // -?Parametri gestione errori API.?
010500160211      /copy qsysinc/qrpglesrc,QUSEC
010600160211
010700160211       //--------------------------------------------------------------
010800160211       //?Definizione key-list.                                        ?
010900160211       //--------------------------------------------------------------
011000160211
011100160211
011200160211       //--------------------------------------------------------------
011300160211       //?Riepilogo indicatori utilizzati.                             ?
011400160211       //--------------------------------------------------------------
011500160211       //--------------------------------------------------------------
011600160211
011700160211       //--------------------------------------------------------------
011800160211       //?M A I N - L I N E                                            ?
011900160211       //--------------------------------------------------------------
012000160211
012100160211     c     *Entry        plist
012200160211     c                   parm                    KPJBA
012300160211
012400160211      /free
012500160211
012600160211       // -?Operazioni iniziali?
012700160211       exsr  sr_RoutInz;
012800160211
012900160211       // -?Verifica SE esistono già tariffe/offerte con tipologia?
013000160211       //  ?diversa da quella ricevuta?
013100160211       exsr  sr_Ctrl_Tipol_Tar;
013200160211
013300160211       // -?Operazioni finali?
013400160211       exsr  sr_RoutEnd;
013500160211
013600160211       //--------------------------------------------------------------
013700160211       //?Operazioni iniziali.                                         ?
013800160211       //--------------------------------------------------------------
013900160211       BEGSR  sr_RoutInz;
014000160211
014100160211         // -?Impostazione opzioni per SQL?
014200160211         exec sql   set  option  DynUsrPrf = *Owner,
014300160211                                 CloSqlCsr = *EndMod;
014400160211
014500160211         // -?Impostazione chiusura?
014600160211         *inLR = *on;
014700160211
014800160211         // -?Pulizia parametri di output?
014900160211         TNTAT1ds = KPJBU;
015000160211         clear  oTAT1tipo;
015100160211         clear  oTAT1ctr;
015200160211         clear  oTAT1prt;
015300160211         clear  oTAT1ntw;
015400160211         clear  oTAT1msg;
015500160211         oTAT1err = *off;
015600160211
015700160211         // -?Reperimento dati job?
015800160211         exsr  sr_DatiJob;
015900160211
016000160211         // -?Controllo dei parametri ricevuti?
016100160211         select;
016200160212           when  iTAT1ntc  =  *zero  and
016300160212                 iTAT1ksc  =  *zero;
016400160212             oTAT1err = *on;
016500160212             oTAT1msg = sk_Msg(01);
016600160212           when  iTAT1ntc  <  *zero;
016700160212             oTAT1err = *on;
016800160212             oTAT1msg = sk_Msg(02);
016900160217           when  iTAT1tipo =  'O'   and
017000160217                 iTAT1ntc <=  *zero;
017100160217             oTAT1err = *on;
017200160217             oTAT1msg = sk_Msg(02);
017300160212           when  iTAT1ksc  <  *zero;
017400160211             oTAT1err = *on;
017500160212             oTAT1msg = sk_Msg(03);
017600160217           when  iTAT1tipo =  'T'   and
017700160217                 iTAT1ksc <=  *zero;
017800160217             oTAT1err = *on;
017900160217             oTAT1msg = sk_Msg(03);
018000160211           when  iTAT1tipo <> *blank  and
018100160211                 iTAT1tipo <> 'T'     and  iTAT1tipo <> 'O';
018200160211             oTAT1err = *on;
018300160212             oTAT1msg = sk_Msg(04);
018400160211           when  iTAT1ctr  <  *zero;
018500160211             oTAT1err = *on;
018600160212             oTAT1msg = sk_Msg(05);
018700160211             if  iTAT1tipo =  'O';
018800160211               oTAT1msg  = %replace( 'offerta'
018900160211                                   : oTAT1msg
019000160211                                   : %scan( 'tariffa' : oTAT1msg ) );
019100160211             endif;
019200160211           when  iTAT1ntw  <> *blank  and
019300160211                 iTAT1ntw  <> 'EEX'   and
019400160211                 iTAT1ntw  <> 'DPD'   and
019500160212                 iTAT1ntw  <> 'FED'   and
019600160212                 iTAT1ntw  <> 'I  '   and
019700160212                 iTAT1ntw  <> 'E  ';
019800160211             oTAT1err = *on;
019900160212             oTAT1msg = sk_Msg(06);
020000160211             if  iTAT1tipo =  'O';
020100160211               oTAT1msg  = %replace( 'offerta'
020200160211                                   : oTAT1msg
020300160211                                   : %scan( 'tariffa' : oTAT1msg ) );
020400160211             endif;
020500160211         endsl;
020600160211         if  oTAT1err = *on;
020700160211           exsr  sr_RoutEnd;
020800160211         endif;
020900160217
021000160301         // -?Impostazione automatica dei dati mancanti?
021100160217         select;
021200160217           when  iTAT1tipo = *blank  and
021300160217                 iTAT1ntc <= *zero;
021400160217             iTAT1tipo = 'T';
021500160217           when  iTAT1tipo = *blank  and
021600160217                 iTAT1ksc <= *zero;
021700160217             iTAT1tipo = 'O';
021800160217         endsl;
021900160301
022000160301         // -?Sulle tariffe del cliente 8888830 NON si fanno controlli?
022100160301         if  iTAT1ntw = '***';
022200160301           exsr  sr_RoutEnd;
022300160301         endif;
022400160211
022500160211       ENDSR;
022600160211
022700160211       //--------------------------------------------------------------
022800160211       //?Reperimento Dati del job (Utente/Operativi).                 ?
022900160211       //--------------------------------------------------------------
023000160211       BEGSR  sr_DatiJob;
023100160211
023200160211         in(E) §AzUte;
023300160211         if NOT %error;
023400160211           in(E) §DatiUte;
023500160211         endif;
023600160211         if %error or RSut = *blanks;
023700160211           clear TIBS34ds;
023800160211           tibs34r ( tibs34ds );
023900160211           in §AzUte;
024000160211           in §DatiUte;
024100160211         endif;
024200160211
024300160211       ENDSR;
024400160211
024500160211       //--------------------------------------------------------------
024600160211       //?Verifica SE esistono già tariffe/offerte con tipologia
024700160211       //?diversa da quella ricevuta.
024800160211       //--------------------------------------------------------------
024900160211       BEGSR  sr_Ctrl_Tipol_Tar;
025000160211
025100160211         clear  $EoF;
025200160211
025300160211         // -?Preparazione SQL per estrazione dati?
025400160211         exsr  sr_PrepSQL;
025500160211
025600160211         // -?Estrazione dati?
025700160211         exsr  sr_OpenCursor;
025800160211
025900160211         DoW  Not $EoF;
026000160211           exsr  sr_ReadCursor;
026100160211         EndDo;
026200160211
026300160211         exsr  sr_CloseCursor;
026400160211
026500160211       ENDSR;
026600160211
026700160211       //--------------------------------------------------------------
026800160211       //?Preparazione SQL.                                            ?
026900160211       //--------------------------------------------------------------
027000160211       BEGSR  sr_PrepSQL;
027100160211
027200160211         // -?Preparazione SQL?
027300160211         clear  wSQL;
027400160212         clear  wFile;
027500160211
027600160211         if  iTAT1tipo = *blank  or
027700160211             iTAT1tipo = 'T';
027800160212           wFile = 'T';
027900160211           wSQL  = 'select ''T'', TNTAM00F.* +
028000160211                      from TNTAM00F';
028100160211         else;
028200160212           wFile = 'O';
028300160211           wSQL  = 'select ''O'', TNOFM00F.* +
028400160211                      from TNOFM00F';
028500160211         endif;
028600160211
028700160211         exsr  sr_Selezioni;
028800160211
028900160211
029000160211         If  iTAT1tipo = *blank;
029100160211
029200160212           wFile = 'O';
029300160211           wSQL += ' UNION +
029400160211                    select ''O'', TNOFM00F.* +
029500160211                      from TNOFM00F';
029600160211
029700160211           exsr  sr_Selezioni;
029800160211
029900160211         EndIf;
030000160211
030100160211
030200160301         wSQL +=   ' order by TAMksc, TAMctr, TAMprg desc';
030300160301
030400160211         wSQL +=     ' for fetch only';
030500160211
030600160211
030700160211         exec sql   prepare S1   from :wSQL;
030800160211
030900160211
031000160211         // -?Dichiarazione cursore?
031100160211         exec sql   declare C1   cursor for S1;
031200160211
031300160211         if  SQLcode < *zero;
031400160211           exsr  sr_PrintErr;
031500160211         endif;
031600160211
031700160211       ENDSR;
031800160211
031900160211       //--------------------------------------------------------------
032000160211       //?Inserimento selezioni nella stringa SQL.                     ?
032100160211       //--------------------------------------------------------------
032200160211       BEGSR  sr_Selezioni;
032300160211
032400160211         // -?Selezione per?
032500160211         //  ?· Flag Annullamento,?
032600160211         //  ?· Codice Cliente,?
032700160211         //  ?· Codice Tariffa?
032800160212         wSQL += ' where TAMatb = '' ''';
032900160301
033000160212         if  wFile = 'O';
033100160212           wSQL += ' and TAMksc = ' +
033200160212                         %trim( %editc( iTAT1ntc : '3' ) );
033300160212         else;
033400160212           wSQL += ' and TAMksc = ' +
033500160212                         %trim( %editc( iTAT1ksc : '3' ) );
033600160212         endif;
033700160301
033800160212         wSQL +=   ' and TAMctr = ' +
033900160211                         %trim( %editc( iTAT1ctr : '3' ) );
034000160211
034100160211       ENDSR;
034200160211
034300160211       //--------------------------------------------------------------
034400160211       //?Apertura cursore.                                            ?
034500160211       //--------------------------------------------------------------
034600160211       BEGSR  sr_OpenCursor;
034700160211
034800160211         // -?Apertura del cursore?
034900160211         exec sql   open C1;
035000160211
035100160211         if  SQLcode < *zero;
035200160211           exsr  sr_PrintErr;
035300160211         endif;
035400160211
035500160211       ENDSR;
035600160211
035700160211       //--------------------------------------------------------------
035800160211       //?Lettura cursore.                                             ?
035900160211       //--------------------------------------------------------------
036000160211       BEGSR  sr_ReadCursor;
036100160211
036200160212         clear  wFile;
036300160211         clear  TNTAM_ds;
036400160211
036500160211         exec sql   fetch next   from C1   into :wFile, :TNTAM_ds;
036600160211
036700160211         Select;
036800160211
036900160211           // -?Fine File?
037000160211           When  SQLcode = 100;
037100160211             $EoF = *on;
037200160211             leavesr;
037300160211
037400160211           // -?Errore SQL?
037500160211           When  SQLcode < *zero;
037600160211             exsr  sr_PrintErr;
037700160211
037800160211           // -?Elaborazione singola Tariffa/Offerta?
037900160211           Other;
038000160211             exsr  sr_Elab_TarOff;
038100160211             exsr  sr_RoutEnd;
038200160211
038300160211         EndSl;
038400160211
038500160211       ENDSR;
038600160211
038700160211       //--------------------------------------------------------------
038800160211       //?Chiusura cursore.                                            ?
038900160211       //--------------------------------------------------------------
039000160211       BEGSR  sr_CloseCursor;
039100160211
039200160211         // -?Chiusura del cursore?
039300160211         exec sql   close C1;
039400160211
039500160211       ENDSR;
039600160211
039700160211       //--------------------------------------------------------------
039800160211       //?Elaborazione singola Tariffa o singola Offerta reperita.     ?
039900160211       //--------------------------------------------------------------
040000160211       BEGSR  sr_Elab_TarOff;
040100160211
040200160301         dSTA01 = TNTAM_ds.TAMflo;
040300160301
040400160301         // -?Verifica SE la Tariffa/Offerta reperita (la più recente)?
040500160301         //  ?ha network diverso da quella che si sta inserendo?
040600160301         Select;
040700160301           // -?...Fanno eccezione le Tariffe di Cartello?
040800160301           //  ?- solo del "cliente" 8888830 -?
040900160301           //  ?(che vanno sempre bene)?
041000160301           When  TNTAM_ds.TAMfie = *blank;
041100160301               leavesr;
041200160301           // -?Inserimento Tariffa Italia - BRT?
041300160301           //  ?Se l'ultima reperita è una tariffa BRT => OK?
041400160301           When  iTAT1ntw = *blank;
041500160301             if  TNTAM_ds.TAMfie = 'I'  and  dSTA01.§TAdpd = *blank
041600160301                                        and  dSTA01.§TAfed = *blank;
041700160301               leavesr;
041800160301             endif;
041900160301           // -?Inserimento Tariffa Estero - EuroExpress?
042000160301           //  ?Se l'ultima reperita è una tariffa EEX => OK?
042100160301           When  iTAT1ntw = 'EEX';
042200160301             if  TNTAM_ds.TAMfie = 'E'  and  dSTA01.§TAdpd = *blank
042300160301                                        and  dSTA01.§TAfed = *blank;
042400160301               leavesr;
042500160301             endif;
042600160301           // -?Inserimento Tariffa Italia - DPD?
042700160301           //  ?Se l'ultima reperita è una tariffa DPD => OK?
042800160301           When  iTAT1ntw = 'DPD';
042900160301             if  TNTAM_ds.TAMfie = 'I'  and  dSTA01.§TAdpd = 'S'
043000160301                                        and  dSTA01.§TAfed = *blank;
043100160301               leavesr;
043200160301             endif;
043300160301           // -?Inserimento Tariffa Estero - FedEx?
043400160301           //  ?Se l'ultima reperita è una tariffa FED => OK?
043500160301           When  iTAT1ntw = 'FED';
043600160301             if  TNTAM_ds.TAMfie = 'E'  and  dSTA01.§TAdpd = *blank
043700160301                                        and  dSTA01.§TAfed = 'S';
043800160301               leavesr;
043900160301             endif;
044000160301           // -?Inserimento Tariffa Italia (BRT o DPD)?
044100160301           //  ?Se l'ultima reperita è una tariffa Italia => OK?
044200160301           When  iTAT1ntw = 'I  ';
044300160301             if  TNTAM_ds.TAMfie = 'I';
044400160301               leavesr;
044500160301             endif;
044600160301           // -?Inserimento Tariffa Estero (EuroExpress o FedEx)?
044700160301           //  ?Se l'ultima reperita è una tariffa Estera => OK?
044800160301           When  iTAT1ntw = 'E  ';
044900160301             if  TNTAM_ds.TAMfie = 'E';
045000160301               leavesr;
045100160301             endif;
045200160301         EndSl;
045300160301
045400160301
045500160301         // -?Impostazione in Output dei dati reperiti?
045600160301
045700160211         oTAT1tipo = wFile;
045800160211         oTAT1ctr  = TNTAM_ds.TAMctr;
045900160211         oTAT1prt  = TNTAM_ds.TAMprg;
046000160211         Select;
046100160211           // -?Trovata Tariffa Italia - BRT?
046200160211           When  TNTAM_ds.TAMfie  = 'I'  and
046300160211                 %subst( TNTAM_ds.TAMflo : 6 : 2 ) =  *blank;
046400160211             oTAT1ntw  = *blank;
046500160211           // -?Trovata Tariffa Estero - EuroExpress?
046600160211           When  TNTAM_ds.TAMfie  = 'E'  and
046700160211                 %subst( TNTAM_ds.TAMflo : 6 : 2 ) =  *blank;
046800160211             oTAT1ntw  = 'EEX';
046900160211           // -?Trovata Tariffa Italia - DPD?
047000160211           When  TNTAM_ds.TAMfie  = 'I'  and
047100160211                 %subst( TNTAM_ds.TAMflo : 6 : 1 ) =  'S';
047200160211             oTAT1ntw  = 'DPD';
047300160211           // -?Trovata Tariffa Estero - FedEx?
047400160211           When  TNTAM_ds.TAMfie  = 'E'  and
047500160211                 %subst( TNTAM_ds.TAMflo : 7 : 1 ) =  'S';
047600160211             oTAT1ntw  = 'FED';
047700160211         EndSl;
047800160211         oTAT1err  = *on;
047900160212         oTAT1msg  = sk_Msg(07);
048000160224         if  iTAT1pgm = 'TNTA56R   ';
048100160224           oTAT1msg = sk_Msg(08);
048200160224         endif;
048300160301         oTAT1msg  = %replace( %editc( TNTAM_ds.TAMctr : 'X' )
048400160301                             : oTAT1msg
048500160301                             : %scan( '___' : oTAT1msg ) );
048600160212         if  wFile = 'O';
048700160212           oTAT1msg   = %replace( 'offerta'
048800160212                                : oTAT1msg
048900160212                                : %scan( 'tariffa' : oTAT1msg ) );
049000160211         endif;
049100160224         if  iTAT1tipo <> 'T';
049200160224           oTAT1msg   = %replace( 'Offerta'
049300160224                                : oTAT1msg
049400160224                                : %scan( 'Tariffa' : oTAT1msg ) );
049500160224         endif;
049600160211         //oTAT1msg = 'Reperita tipologia diversa per la stessa tariffa+
049700160211         //           : progressivo ___ di tipo ###';
049800160211         //oTAT1msg  = %replace( %editc( TNTAM_ds.TAMprg : 'X' )
049900160211         //                    : oTAT1msg
050000160211         //                    : %scan( '___' : oTAT1msg ) );
050100160211         //oTAT1msg  = %replace( oTAT1ntw
050200160211         //                    : oTAT1msg
050300160211         //                    : %scan( '###' : oTAT1msg ) );
050400160211
050500160211       ENDSR;
050600160211
050700160211       //--------------------------------------------------------------
050800160211       //?Stampa segnalazione dell'errore rilevato via SQL.            ?
050900160211       //--------------------------------------------------------------
051000160211       BEGSR  sr_PrintErr;
051100160211
051200160211         oTAT1err = *on;
051300160212         oTAT1msg = sk_Msg(09);
051400160211
051500160211         // -?Stampa del Dump?
051600160211         Dump(A);
051700160211
051800160211         // -?Stampa del Job-Log?
051900160211         Qcmd = 'DSPJOBLOG job(*) output(*print)';
052000160211         exsr  sr_ExecCmd;
052100160211
052200160211         // -?Chiusura del programma?
052300160211         exsr  sr_RoutEnd;
052400160211
052500160211       ENDSR;
052600160211
052700160211       //--------------------------------------------------------------
052800160211       //?Esecuzione del comando (già impostato).                      ?
052900160211       //--------------------------------------------------------------
053000160211       BEGSR  sr_ExecCmd;
053100160211
053200160211         clear Qcap0100;
053300160211         Qcabcsdh = *off;
053400160211         Qcapa    = *off;
053500160211         Qcacmdss = *off;
053600160211         Qcaerved = *allX'00';
053700160211
053800160211         clear Qusec;
053900160211         Qusbprv  = %size(Qusec);
054000160211
054100160211         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
054200160211                           %size(Qcap0100) : 'CPOP0100' : *omit :
054300160211                           0 : 0 : Qusec);
054400160211
054500160211         //if  Qusei <> *blank;
054600160211         //  ...;
054700160211         //endif;
054800160211
054900160211       ENDSR;
055000160211
055100160211       //--------------------------------------------------------------
055200160211       //?Operazioni finali.                                           ?
055300160211       //--------------------------------------------------------------
055400160211       BEGSR  sr_RoutEnd;
055500160211
055600160211         KPJBU = tntaT1ds;
055700160211
055800160211         return;
055900160211
056000160211       ENDSR;
056100160211
056200160211      /end-free
056300160211
056400160211       //--------------------------------------------------------------
056500160211       //?Definizione schiere a tempo di compilazione                  ?
056600160211       //--------------------------------------------------------------
056700160211
056800160211** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
056900160212Numero trattativa e/o codice cliente obbligatori                               1
057000160212Numero trattativa errato                                                       2
057100160212Codice cliente errato                                                          3
057200160212Tipo tariffa/offerta errato                                                    4
057300160212Codice tariffa errato                                                          5
057400160212Tipologia tariffa (Italia/Estero/DPD/FedEx) errata                             6
057500160301Presente altra tariffa ___ con Network diverso: cambiare il codice Tariffa     7
057600160301Presente altra tariffa ___ con Network diverso: creare nuovo codice offerta    8
057700160212Rilevato errore: consultare la stampa ed avvisare il CED                       9
