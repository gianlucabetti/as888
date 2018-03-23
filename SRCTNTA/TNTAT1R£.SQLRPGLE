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
003900160211     d sk_Msg          s             78    dim( 6)  ctdata  perrcd( 1)
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
006300160211       // -?Data/Ora attuali?
006400160211     d wTime_ds        ds                  inz
006500160211     d   wDate                        8s 0 inz
006600160211     d   wTime                        6s 0 inz
006700160211
006800160211       // -?Parametri ricevuti
006900160211     d KPJBA         e ds
007000160211     d TNTAT1ds      e ds                  inz
007100160211
007200160211       // -?Dati elaborati via SQL?
007300160211     d TNTAM_ds      e ds                  extname(TNTAM00F)
007400160211     d                                     inz  qualified
007500160211     d wFile           s              1a   inz
007600160211
007700160211       //--------------------------------------------------------------
007800160211       //?Definizione variabili globali.                               ?
007900160211       //--------------------------------------------------------------
008000160211
008100160211       // -?Flags booleani?
008200160211     d $EoF            s               n   inz
008300160211
008400160211       // -?Stringa SQL da eseguire?
008500160211     d wSQL            s           5000    inz  varying
008600160211
008700160211       // -?Data *ISO?
008800160211     d wDate_Iso       s               d   datfmt(*ISO)    inz(*loval)
008900160211       // -?Data *EUR?
009000160211     d wDate_Eur       s               d   datfmt(*EUR)    inz(*loval)
009100160211
009200160211       //--------------------------------------------------------------
009300160211       //?Definizione prototipi procedure e relativi parametri.        ?
009400160211       //--------------------------------------------------------------
009500160211
009600160211       // -?Reperimento dati utente?
009700160211     d TIBS34ds      e ds                  inz
009800160211      /copy gaitrasrc/srcProtoPR,TIBS34R
009900160211
010000160211       // -?Parametri API QCAPCMD (Process Commands)?
010100160211     d Qcmd            s           2048    inz  varying
010200160211      /copy qSysInc/qRpgleSrc,QCAPCMD
010300160211       // -?API QCAPCMD (Process Commands)?
010400160211      /copy gaitrasrc/srcProtoPR,QCAPCMD
010500160211
010600160211       // -?Parametri gestione errori API.?
010700160211      /copy qsysinc/qrpglesrc,QUSEC
010800160211
010900160211       //--------------------------------------------------------------
011000160211       //?Definizione key-list.                                        ?
011100160211       //--------------------------------------------------------------
011200160211
011300160211
011400160211       //--------------------------------------------------------------
011500160211       //?Riepilogo indicatori utilizzati.                             ?
011600160211       //--------------------------------------------------------------
011700160211       //--------------------------------------------------------------
011800160211
011900160211       //--------------------------------------------------------------
012000160211       //?M A I N - L I N E                                            ?
012100160211       //--------------------------------------------------------------
012200160211
012300160211     c     *Entry        plist
012400160211     c                   parm                    KPJBA
012500160211
012600160211      /free
012700160211
012800160211       // -?Operazioni iniziali?
012900160211       exsr  sr_RoutInz;
013000160211
013100160211       // -?Verifica SE esistono già tariffe/offerte con tipologia?
013200160211       //  ?diversa da quella ricevuta?
013300160211       exsr  sr_Ctrl_Tipol_Tar;
013400160211
013500160211       // -?Operazioni finali?
013600160211       exsr  sr_RoutEnd;
013700160211
013800160211       //--------------------------------------------------------------
013900160211       //?Operazioni iniziali.                                         ?
014000160211       //--------------------------------------------------------------
014100160211       BEGSR  sr_RoutInz;
014200160211
014300160211         // -?Impostazione opzioni per SQL?
014400160211         exec sql   set  option  DynUsrPrf = *Owner,
014500160211                                 CloSqlCsr = *EndMod;
014600160211
014700160211         // -?Impostazione chiusura?
014800160211         *inLR = *on;
014900160211
015000160211         // -?Pulizia parametri di output?
015100160211         TNTAT1ds = KPJBU;
015200160211         clear  oTAT1tipo;
015300160211         clear  oTAT1ctr;
015400160211         clear  oTAT1prt;
015500160211         clear  oTAT1ntw;
015600160211         clear  oTAT1msg;
015700160211         oTAT1err = *off;
015800160211
015900160211         // -?Reperimento dati job?
016000160211         exsr  sr_DatiJob;
016100160211
016200160211         // -?Controllo dei parametri ricevuti?
016300160211         select;
016400160211           when  iTAT1ksc  <= *zero;
016500160211             oTAT1err = *on;
016600160211             oTAT1msg = sk_Msg(01);
016700160211           when  iTAT1tipo <> *blank  and
016800160211                 iTAT1tipo <> 'T'     and  iTAT1tipo <> 'O';
016900160211             oTAT1err = *on;
017000160211             oTAT1msg = sk_Msg(02);
017100160211           when  iTAT1ctr  <  *zero;
017200160211             oTAT1err = *on;
017300160211             oTAT1msg = sk_Msg(03);
017400160211             if  iTAT1tipo =  'O';
017500160211               oTAT1msg  = %replace( 'offerta'
017600160211                                   : oTAT1msg
017700160211                                   : %scan( 'tariffa' : oTAT1msg ) );
017800160211             endif;
017900160211           when  iTAT1ntw  <> *blank  and
018000160211                 iTAT1ntw  <> 'EEX'   and
018100160211                 iTAT1ntw  <> 'DPD'   and
018200160211                 iTAT1ntw  <> 'FED';
018300160211             oTAT1err = *on;
018400160211             oTAT1msg = sk_Msg(04);
018500160211             if  iTAT1tipo =  'O';
018600160211               oTAT1msg  = %replace( 'offerta'
018700160211                                   : oTAT1msg
018800160211                                   : %scan( 'tariffa' : oTAT1msg ) );
018900160211             endif;
019000160211         endsl;
019100160211         if  oTAT1err = *on;
019200160211           exsr  sr_RoutEnd;
019300160211         endif;
019400160211
019500160211         // -?Reperimento data e ora attuali?
019600160211         wTime_ds  = %editc( %dec( %timestamp() ) : 'X' );
019700160211         wDate_Iso = %date( wDate : *Iso );
019800160211         wDate_Eur = wDate_Iso;
019900160211         wDate     = %dec( wDate_Eur );
020000160211
020100160211       ENDSR;
020200160211
020300160211       //--------------------------------------------------------------
020400160211       //?Reperimento Dati del job (Utente/Operativi).                 ?
020500160211       //--------------------------------------------------------------
020600160211       BEGSR  sr_DatiJob;
020700160211
020800160211         in(E) §AzUte;
020900160211         if NOT %error;
021000160211           in(E) §DatiUte;
021100160211         endif;
021200160211         if %error or RSut = *blanks;
021300160211           clear TIBS34ds;
021400160211           tibs34r ( tibs34ds );
021500160211           in §AzUte;
021600160211           in §DatiUte;
021700160211         endif;
021800160211
021900160211       ENDSR;
022000160211
022100160211       //--------------------------------------------------------------
022200160211       //?Verifica SE esistono già tariffe/offerte con tipologia
022300160211       //?diversa da quella ricevuta.
022400160211       //--------------------------------------------------------------
022500160211       BEGSR  sr_Ctrl_Tipol_Tar;
022600160211
022700160211         clear  $EoF;
022800160211
022900160211         // -?Preparazione SQL per estrazione dati?
023000160211         exsr  sr_PrepSQL;
023100160211
023200160211         // -?Estrazione dati?
023300160211         exsr  sr_OpenCursor;
023400160211
023500160211         DoW  Not $EoF;
023600160211           exsr  sr_ReadCursor;
023700160211         EndDo;
023800160211
023900160211         exsr  sr_CloseCursor;
024000160211
024100160211       ENDSR;
024200160211
024300160211       //--------------------------------------------------------------
024400160211       //?Preparazione SQL.                                            ?
024500160211       //--------------------------------------------------------------
024600160211       BEGSR  sr_PrepSQL;
024700160211
024800160211         // -?Preparazione SQL?
024900160211         clear  wSQL;
025000160211
025100160211         if  iTAT1tipo = *blank  or
025200160211             iTAT1tipo = 'T';
025300160211           wSQL  = 'select ''T'', TNTAM00F.* +
025400160211                      from TNTAM00F';
025500160211         else;
025600160211           wSQL  = 'select ''O'', TNOFM00F.* +
025700160211                      from TNOFM00F';
025800160211         endif;
025900160211
026000160211         exsr  sr_Selezioni;
026100160211
026200160211
026300160211         If  iTAT1tipo = *blank;
026400160211
026500160211           wSQL += ' UNION +
026600160211                    select ''O'', TNOFM00F.* +
026700160211                      from TNOFM00F';
026800160211
026900160211           exsr  sr_Selezioni;
027000160211
027100160211         EndIf;
027200160211
027300160211
027400160211         wSQL +=     ' for fetch only';
027500160211
027600160211
027700160211         exec sql   prepare S1   from :wSQL;
027800160211
027900160211
028000160211         // -?Dichiarazione cursore?
028100160211         exec sql   declare C1   cursor for S1;
028200160211
028300160211         if  SQLcode < *zero;
028400160211           exsr  sr_PrintErr;
028500160211         endif;
028600160211
028700160211       ENDSR;
028800160211
028900160211       //--------------------------------------------------------------
029000160211       //?Inserimento selezioni nella stringa SQL.                     ?
029100160211       //--------------------------------------------------------------
029200160211       BEGSR  sr_Selezioni;
029300160211
029400160211         // -?Selezione per?
029500160211         //  ?· Flag Annullamento,?
029600160211         //  ?· Codice Cliente,?
029700160211         //  ?· Codice Tariffa?
029800160211         wSQL += ' where TAMatb = '' '' +
029900160211                     and TAMksc = ' +
030000160211                         %trim( %editc( iTAT1ksc : '3' ) ) +
030100160211                   ' and TAMctr = ' +
030200160211                         %trim( %editc( iTAT1ctr : '3' ) );
030300160211
030400160211         // -?Selezione per Tariffa Italia/Estero e Network?
030500160211         Select;
030600160211           // -?Tariffa Italia - BRT?
030700160211           When  iTAT1ntw = *blank;
030800160211             wSQL += ' and ( TAMfie = ''E'' or +
030900160211                             substr( TAMflo, 6, 2 ) <> ''  '' )';
031000160211           // -?Tariffa Estero - EuroExpress?
031100160211           When  iTAT1ntw = 'EEX';
031200160211             wSQL += ' and ( TAMfie = ''I'' or +
031300160211                             substr( TAMflo, 6, 2 ) <> ''  '' )';
031400160211           // -?Tariffa Italia - DPD?
031500160211           When  iTAT1ntw = 'DPD';
031600160211             wSQL += ' and ( TAMfie = ''E'' or +
031700160211                             substr( TAMflo, 7, 1 ) =  ''S'' )';
031800160211           // -?Tariffa Estero - FedEx?
031900160211           When  iTAT1ntw = 'FED';
032000160211             wSQL += ' and ( TAMfie = ''I'' or +
032100160211                             substr( TAMflo, 6, 1 ) =  ''S'' )';
032200160211         EndSl;
032300160211
032400160211       ENDSR;
032500160211
032600160211       //--------------------------------------------------------------
032700160211       //?Apertura cursore.                                            ?
032800160211       //--------------------------------------------------------------
032900160211       BEGSR  sr_OpenCursor;
033000160211
033100160211         // -?Apertura del cursore?
033200160211         exec sql   open C1;
033300160211
033400160211         if  SQLcode < *zero;
033500160211           exsr  sr_PrintErr;
033600160211         endif;
033700160211
033800160211       ENDSR;
033900160211
034000160211       //--------------------------------------------------------------
034100160211       //?Lettura cursore.                                             ?
034200160211       //--------------------------------------------------------------
034300160211       BEGSR  sr_ReadCursor;
034400160211
034500160211         clear  TNTAM_ds;
034600160211
034700160211         exec sql   fetch next   from C1   into :wFile, :TNTAM_ds;
034800160211
034900160211         Select;
035000160211
035100160211           // -?Fine File?
035200160211           When  SQLcode = 100;
035300160211             $EoF = *on;
035400160211             leavesr;
035500160211
035600160211           // -?Errore SQL?
035700160211           When  SQLcode < *zero;
035800160211             exsr  sr_PrintErr;
035900160211
036000160211           // -?Elaborazione singola Tariffa/Offerta?
036100160211           Other;
036200160211             exsr  sr_Elab_TarOff;
036300160211             exsr  sr_RoutEnd;
036400160211
036500160211         EndSl;
036600160211
036700160211       ENDSR;
036800160211
036900160211       //--------------------------------------------------------------
037000160211       //?Chiusura cursore.                                            ?
037100160211       //--------------------------------------------------------------
037200160211       BEGSR  sr_CloseCursor;
037300160211
037400160211         // -?Chiusura del cursore?
037500160211         exec sql   close C1;
037600160211
037700160211       ENDSR;
037800160211
037900160211       //--------------------------------------------------------------
038000160211       //?Elaborazione singola Tariffa o singola Offerta reperita.     ?
038100160211       //--------------------------------------------------------------
038200160211       BEGSR  sr_Elab_TarOff;
038300160211
038400160211         oTAT1tipo = wFile;
038500160211         oTAT1ctr  = TNTAM_ds.TAMctr;
038600160211         oTAT1prt  = TNTAM_ds.TAMprg;
038700160211         Select;
038800160211           // -?Trovata Tariffa Italia - BRT?
038900160211           When  TNTAM_ds.TAMfie  = 'I'  and
039000160211                 %subst( TNTAM_ds.TAMflo : 6 : 2 ) =  *blank;
039100160211             oTAT1ntw  = *blank;
039200160211           // -?Trovata Tariffa Estero - EuroExpress?
039300160211           When  TNTAM_ds.TAMfie  = 'E'  and
039400160211                 %subst( TNTAM_ds.TAMflo : 6 : 2 ) =  *blank;
039500160211             oTAT1ntw  = 'EEX';
039600160211           // -?Trovata Tariffa Italia - DPD?
039700160211           When  TNTAM_ds.TAMfie  = 'I'  and
039800160211                 %subst( TNTAM_ds.TAMflo : 6 : 1 ) =  'S';
039900160211             oTAT1ntw  = 'DPD';
040000160211           // -?Trovata Tariffa Estero - FedEx?
040100160211           When  TNTAM_ds.TAMfie  = 'E'  and
040200160211                 %subst( TNTAM_ds.TAMflo : 7 : 1 ) =  'S';
040300160211             oTAT1ntw  = 'FED';
040400160211         EndSl;
040500160211         oTAT1err  = *on;
040600160211         oTAT1msg  = sk_Msg(05);
040700160211         if  oTAT1tipo =  'O';
040800160211           oTAT1msg  = %replace( 'offerta'
040900160211                               : oTAT1msg
041000160211                               : %scan( 'tariffa' : oTAT1msg ) );
041100160211         endif;
041200160211         //oTAT1msg = 'Reperita tipologia diversa per la stessa tariffa+
041300160211         //           : progressivo ___ di tipo ###';
041400160211         //oTAT1msg  = %replace( %editc( TNTAM_ds.TAMprg : 'X' )
041500160211         //                    : oTAT1msg
041600160211         //                    : %scan( '___' : oTAT1msg ) );
041700160211         //oTAT1msg  = %replace( oTAT1ntw
041800160211         //                    : oTAT1msg
041900160211         //                    : %scan( '###' : oTAT1msg ) );
042000160211
042100160211       ENDSR;
042200160211
042300160211       //--------------------------------------------------------------
042400160211       //?Stampa segnalazione dell'errore rilevato via SQL.            ?
042500160211       //--------------------------------------------------------------
042600160211       BEGSR  sr_PrintErr;
042700160211
042800160211         oTAT1err = *on;
042900160211         oTAT1msg = sk_Msg(06);
043000160211
043100160211         // -?Stampa del Dump?
043200160211         Dump(A);
043300160211
043400160211         // -?Stampa del Job-Log?
043500160211         Qcmd = 'DSPJOBLOG job(*) output(*print)';
043600160211         exsr  sr_ExecCmd;
043700160211
043800160211         // -?Chiusura del programma?
043900160211         exsr  sr_RoutEnd;
044000160211
044100160211       ENDSR;
044200160211
044300160211       //--------------------------------------------------------------
044400160211       //?Esecuzione del comando (già impostato).                      ?
044500160211       //--------------------------------------------------------------
044600160211       BEGSR  sr_ExecCmd;
044700160211
044800160211         clear Qcap0100;
044900160211         Qcabcsdh = *off;
045000160211         Qcapa    = *off;
045100160211         Qcacmdss = *off;
045200160211         Qcaerved = *allX'00';
045300160211
045400160211         clear Qusec;
045500160211         Qusbprv  = %size(Qusec);
045600160211
045700160211         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
045800160211                           %size(Qcap0100) : 'CPOP0100' : *omit :
045900160211                           0 : 0 : Qusec);
046000160211
046100160211         //if  Qusei <> *blank;
046200160211         //  ...;
046300160211         //endif;
046400160211
046500160211       ENDSR;
046600160211
046700160211       //--------------------------------------------------------------
046800160211       //?Operazioni finali.                                           ?
046900160211       //--------------------------------------------------------------
047000160211       BEGSR  sr_RoutEnd;
047100160211
047200160211         KPJBU = tntaT1ds;
047300160211
047400160211         return;
047500160211
047600160211       ENDSR;
047700160211
047800160211      /end-free
047900160211
048000160211       //--------------------------------------------------------------
048100160211       //?Definizione schiere a tempo di compilazione                  ?
048200160211       //--------------------------------------------------------------
048300160211
048400160211** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
048500160211Codice cliente obbligatorio                                                    1
048600160211Tipo tariffa/offerta errato                                                    2
048700160211Codice tariffa errato                                                          3
048800160211Tipologia tariffa (Italia/Estero/DPD/FedEx) errata                             4
048900160211Presente altra tariffa con Tipo Tariffa o Network diverso                      5
049000160211Rilevato errore: consultare la stampa ed avvisare il CED                       6
