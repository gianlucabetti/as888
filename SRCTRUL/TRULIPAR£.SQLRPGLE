000100150413       //==============================================================
000200150413       //?TRULIPA * Creaz. file AZIPA* (Indice Pubbliche Amministraz.) ?
000300150413       //==============================================================
000400150413
000500150413       //--------------------------------------------------------------
000600150413       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700150413       //--------------------------------------------------------------
000800150413
000900150413     /*PRM dbgview(*source)
001000150413     /*END
001100150413
001200150413       //--------------------------------------------------------------
001300150413       //?Specifiche di controllo.                                     ?
001400150413       //--------------------------------------------------------------
001500150413
001600150413     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700150413     h dftactgrp(*no)
001800150415     h bnddir('UBBNDDIR')
001900150413     h alwnull(*inputonly)
002000150413
002100150413       //--------------------------------------------------------------
002200150413       //?Dichiarazione file.                                          ?
002300150413       //--------------------------------------------------------------
002400150413
002500150413       // -?Indice delle Pubbliche Amministrazioni: Amministrazioni?
002600150414       //  ?(dati scaricati da internet)?
002700150415     fAZIP100F  Uf   e             disk    usropn
002800150413
002900150413       // -?Indice delle Pubbliche Amministrazioni: Unità Organizzative?
003000150414       //  ?(dati scaricati da internet)?
003100150415     fAZIP200F  Uf   e             disk    usropn
003200150414
003300150414       // -?Indice delle Pubbliche Amministrazioni (*file BRT)?
003400150415     fAZIPA00F  O    e             disk    usropn
003500150413
003600150413       //--------------------------------------------------------------
003700150413       //?Definizione costanti.                                        ?
003800150413       //--------------------------------------------------------------
003900150413
004000150413       // -?Caratteri maiuscoli?
004100150413     d c_CharMai       c                   const('ABCDEFGHIJKLM+
004200150413     d                                            NOPQRSTUVWXYZ+
004300150413     d                                            ÀÁÂÃÄÅ+
004400150413     d                                            ÆÇ+
004500150413     d                                            ÈÉÊË+
004600150413     d                                            ÌÍÎÏ+
004700150413     d                                            ÒÓÔÕÖ+
004800150413     d                                            ÙÚÛÜ')
004900150413       // -?Caratteri minuscoli?
005000150413     d c_CharMin       c                   const('abcdefghijklm+
005100150413     d                                            nopqrstuvwxyz+
005200150413     d                                            àáâãäå+
005300150413     d                                            æç+
005400150413     d                                            èéêë+
005500150413     d                                            ìíîï+
005600150413     d                                            òóôõö+
005700150413     d                                            ùúûü')
005800150413
005900150413       //--------------------------------------------------------------
006000150413       //?Definizione schiere.                                         ?
006100150413       //--------------------------------------------------------------
006200150413
006300150512       // -?Messaggi di errore?
006400151015     d sk_Msg          s             78    dim( 9) ctdata perrcd(1)
006500150413
006600150413       //--------------------------------------------------------------
006700150413       //?Definizione aree dati.                                       ?
006800150413       //--------------------------------------------------------------
006900150413
007000150413       // -?Dati utente?
007100150413     d §AzUte        e ds                  extname(AZUTE00F)
007200150413     d                                     dtaara
007300150413     d §DatiUte      e ds                  extname(dDatiUte)
007400150413     d                                     dtaara
007500150413
007600150413       //--------------------------------------------------------------
007700150413       //?Definizione strutture dati.                                  ?
007800150413       //--------------------------------------------------------------
007900150413
008000150413       // -?Status ds?
008100150413     d Status         sds
008200150413     d   SDSpgm          *proc
008300150413     d*//SDSprm          *parms
008400150413     d*//SDSdta              191    198
008500150413     d*//SDSjob              244    253
008600150413     d   SDSusr              254    263
008700150413
008800150413       // -?Parametri ricevuti?
008900151015     d*// KPJBA         e ds
009000151015     d TRULIPAds     e ds
009100150415
009200150415       // -?Dati elaborati via SQL?
009300150415     d AZIP1_ds      e ds                  extname(AZIP100F)  inz
009400150415     d AZIP2_ds      e ds                  extname(AZIP200F)  inz
009500150413
009600150413       //--------------------------------------------------------------
009700150413       //?Definizione variabili globali.                               ?
009800150413       //--------------------------------------------------------------
009900150415
010000150415       // -?Flags booleani?
010100150415     d $EoF            s               n   inz
010200150413
010300150413       // -?File di Flusso?
010400150413     d wSTMF           s             50    inz('/temp/iPA/')
010500150413
010600150413       // -?File su AS/400?
010700150415     d wLibr           s             11    inz
010800150415     d wFile           s             10    inz
010900150414
011000150414       // -?Campi di comodo?
011100150414     d wChar           s                   like(IP2camm)  inz
011200150414
011300150414       // -?Stringa SQL da eseguire?
011400150414     d wSQL            s           5000    inz  varying
011500150413
011600150413       //--------------------------------------------------------------
011700150413       //?Definizione prototipi procedure usate.                       ?
011800150413       //--------------------------------------------------------------
011900150413
012000150413       // -?Reperimento dati utente?
012100150413     d TIBS34ds      e ds                  inz
012200150413      /copy gaitrasrc/srcProtoPR,TIBS34R
012300150415
012400150415       // -?Reperimento NETA sistema AS/400 corrente?
012500150415     d currSysNeta     s              8a   inz
012600150415      /copy gaitrasrc/srcProtoPR,UBRTVNETA
012700150413
012800150413       // -?Parametri API QCAPCMD (Process Commands)?
012900150413     d Qcmd            s           2048    inz  varying
013000150413      /copy qSysInc/qRpgleSrc,QCAPCMD
013100150413       // -?API QCAPCMD (Process Commands)?
013200150413      /copy gaitrasrc/srcProtoPR,QCAPCMD
013300150413
013400150413       // -?Parametri gestione errori API.?
013500150413      /copy qsysinc/qrpglesrc,QUSEC
013600150413
013700150413       //--------------------------------------------------------------
013800150413       //?Definizione key-list.                                        ?
013900150413       //--------------------------------------------------------------
014000150413
014100150413
014200150413       //--------------------------------------------------------------
014300150413       //?Riepilogo indicatori utilizzati.                             ?
014400150413       //--------------------------------------------------------------
014500150413
014600150413
014700150413       //--------------------------------------------------------------
014800150413       //?M A I N - L I N E                                            ?
014900150413       //--------------------------------------------------------------
015000150413
015100150413     c     *Entry        plist
015200151015     c*//                parm                    KPJBA
015300151015     c                   parm                    trulIPAds
015400150413
015500150413      /free
015600150413
015700150413       // -?Operazioni iniziali?
015800150413       exsr  sr_RoutInz;
015900150512
016000151015       // -?Esecuzione delle operazioni richieste?
016100151015       exsr  sr_Execute;
016200150413
016300150413       // -?Operazioni finali?
016400150413       exsr  sr_RoutEnd;
016500150413
016600150413       //--------------------------------------------------------------
016700150413       //?Operazioni iniziali.                                         ?
016800150413       //--------------------------------------------------------------
016900150413       BEGSR  sr_RoutInz;
017000150413
017100150413         // -?Impostazione opzioni per SQL?
017200150413         exec sql   set  option  DynUsrPrf = *Owner,
017300150413                                 CloSqlCsr = *EndMod;
017400150413
017500150413         // -?Impostazione chiusura?
017600150413         *inLR = *on;
017700151015
017800151015         // -?Pulizia parametri di output?
017900151015         clear  ulIPAerr;
018000151015         clear  ulIPAmsg;
018100151015
018200151015         // -?Controllo dei parametri ricevuti?
018300151015         // ·?Verifica ricezione indirizzario (SE richiesta importazione)?
018400151015         If  ulIPAcrtF = 'S';
018500151015           select;
018600151015             // -?Indirizzario "Amministrazioni" obbligatorio?
018700151015             when  ulIPAip1 = *blank;
018800151015               ulIPAerr = *on;
018900151015               ulIPAmsg = sk_Msg(01);
019000151015               exsr  sr_RoutEnd;
019100151015             // -?Indirizzario "Unità Organizzative" obbligatorio?
019200151015             when  ulIPAip2 = *blank;
019300151015               ulIPAerr = *on;
019400151015               ulIPAmsg = sk_Msg(02);
019500151015               exsr  sr_RoutEnd;
019600151015           endsl;
019700151015         EndIf;
019800151015         Select;
019900151015           // ·?Richiesta import dei dati su AS/400?
020000151015           When  ulIPAcrtF <> *blank  and  ulIPAcrtF <> 'S';
020100151015             ulIPAerr = *on;
020200151015             ulIPAmsg = sk_Msg(03);
020300151015             exsr  sr_RoutEnd;
020400151015           // -?Richiesto aggiornamento dei dati in AZIPA00F?
020500151015           When  ulIPAipa  <> *blank  and  ulIPAipa  <> 'S';
020600151015             ulIPAerr = *on;
020700151015             ulIPAmsg = sk_Msg(04);
020800151015             exsr  sr_RoutEnd;
020900151015           // -?Richiesta copia di salvataggio?
021000151015           When  ulIPAcpyF <> *blank  and  ulIPAcpyF <> 'S';
021100151015             ulIPAerr = *on;
021200151015             ulIPAmsg = sk_Msg(05);
021300151015             exsr  sr_RoutEnd;
021400151015           // -?Richiesta cancellazione nuovi dati su AS/400?
021500151015           When  ulIPAdltX <> *blank  and  ulIPAdltX <> 'S';
021600151015             ulIPAerr = *on;
021700151015             ulIPAmsg = sk_Msg(06);
021800151015             exsr  sr_RoutEnd;
021900151015           // -?Richiesta copia senza aggiornamento?
022000151015           When  ulIPAipa  = *blank   and  ulIPAcpyF =  'S';
022100151015             ulIPAerr = *on;
022200151015             ulIPAmsg = sk_Msg(07);
022300151015             exsr  sr_RoutEnd;
022400151015         EndSl;
022500151015         // -?NON richiesta alcuna attività?
022600151015         if  ulIPAcrtF = *blank  and  ulIPAipa = *blank;
022700151015           ulIPAerr = *on;
022800151015           ulIPAmsg = sk_Msg(08);
022900151015           exsr  sr_RoutEnd;
023000151015         endif;
023100150415
023200150415         // -?Verifica del sistema AS/400 corrente?
023300150415         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
023400150415           exsr  sr_RoutEnd;
023500150415         endif;
023600150413
023700150413         // -?Reperimento dati job?
023800150413         exsr  sr_DatiJob;
023900150413
024000150413       ENDSR;
024100150413
024200150413       //--------------------------------------------------------------
024300150413       //?Reperimento Dati del job (Utente/Operativi).                 ?
024400150413       //--------------------------------------------------------------
024500150413       BEGSR  sr_DatiJob;
024600150413
024700150413         in(e) §AzUte;
024800150413         if NOT %error;
024900150413           in(e) §DatiUte;
025000150413         endif;
025100150413         if %error or RSut = *blank;
025200150413           tibs34r ( tibs34ds );
025300150413           in §AzUte;
025400150413           in §DatiUte;
025500150413         endif;
025600150413
025700150413       ENDSR;
025800150512
025900150512       //--------------------------------------------------------------
026000150513       //?Esecuzione delle operazioni richieste.                       ?
026100150512       //--------------------------------------------------------------
026200150512       BEGSR  sr_Execute;
026300150512
026400150512         // -?Import dei nuovi dati delle Pubbliche Amministrazioni?
026500150512         if  %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
026600150512           wLibr = 'UNITRAGRU/';
026700150512         else;
026800150512           wLibr = 'UNITRAGRP/';
026900150512         endif;
027000150512
027100151015         If  ulIPAcrtF = 'S';
027200150512
027300150513           // -?Creazione nuovo file AZIP100F?
027400151015           wSTMF = ulIPAip1;
027500150512           wFile = 'AZIP100F';
027600150512           exsr  sr_Import;
027700151015           if  ulIPAerr = *on;
027800150512             leavesr;
027900150512           endif;
028000150512           // -?Aggiornamento AZIP100F.IP1COD (tutto maiuscolo)?
028100150512           exsr  sr_UpdIP1cod;
028200151015           if  ulIPAerr = *on;
028300150512             leavesr;
028400150512           endif;
028500150512
028600150513           // -?Creazione nuovo file AZIP200F?
028700151015           wSTMF = ulIPAip2;
028800150512           wFile = 'AZIP200F';
028900150512           exsr  sr_Import;
029000151015           if  ulIPAerr = *on;
029100150512             leavesr;
029200150512           endif;
029300150512           // -?Aggiornamento AZIP200F.IP2CAMM (tutto maiuscolo)?
029400150512           exsr  sr_UpdIP2camm;
029500151015           if  ulIPAerr = *on;
029600150512             leavesr;
029700150512           endif;
029800150512
029900150512         EndIf;
030000150512
030100150512         // -?Scrittura del file AZIPA00F?
030200151015         If  ulIPAipa = 'S';
030300150512
030400150513           wFile = 'AZIPA00F';
030500150513
030600150513           // -?Copia dell'attuale file AZIPA00F?
030700151015           If  ulIPAcpyF = 'S';
030800150512             exsr  sr_CpyF;
030900151015             if  ulIPAerr = *on;
031000150512               leavesr;
031100150512             endif;
031200150512           EndIf;
031300150513
031400150513           // -?Pulizia dell'attuale file AZIPA00F?
031500150513           Qcmd = 'CLRPFM file(' + %trimR( wlibr ) + %trimR( wFile ) + ')';
031600150513           exsr  sr_ExecCmd;
031700150513           if  Qusei <> *blank;
031800150513             exsr  sr_PrintErr;
031900150513             leavesr;
032000150513           endif;
032100150513
032200150513           // -?Scrittura rec. nel file AZIPA00F?
032300150512           exsr  sr_WrtAZIPA;
032400151015           if  ulIPAerr = *on;
032500150512             leavesr;
032600150512           endif;
032700150512
032800150512         EndIf;
032900151015
033000151015         // -?Cancellazione dati nei nuovi file AZIPx00F?
033100151015         If  ulIPAdltX = 'S';
033200151015
033300151015           // -?Cancellazione dati nel nuovo file AZIP100F?
033400151015           wFile = 'AZIP100F ';
033500151015           Qcmd  = 'CLRPFM file(' + %trimR(wLibr) + %trimR(wFile) + ') +
033600151015                           mbr(*first)';
033700151015           exsr  sr_ExecCmd;
033800151015           if  Qusei <> *blank;
033900151015             exsr  sr_PrintErr;
034000151015             leavesr;
034100151015           endif;
034200151015           // -?Cancellazione dati nel nuovo file AZIP200F?
034300151015           wFile = 'AZIP200F ';
034400151015           Qcmd  = 'CLRPFM file(' + %trimR(wLibr) + %trimR(wFile) + ') +
034500151015                           mbr(*first)';
034600151015           exsr  sr_ExecCmd;
034700151015           if  Qusei <> *blank;
034800151015             exsr  sr_PrintErr;
034900151015             leavesr;
035000151015           endif;
035100151015
035200151015         EndIf;
035300150513
035400150513         // -?Cancellazione del vecchio file AZIP100F?
035500150513         wFile = 'AZIP100Fx';
035600150513         Qcmd  = 'DLTF file(' + %trimR(wLibr) + %trimR(wFile) + ')';
035700150513         exsr  sr_ExecCmd;
035800150513         // -?Cancellazione del vecchio file AZIP200F?
035900150513         wFile = 'AZIP200Fx';
036000150513         Qcmd  = 'DLTF file(' + %trimR(wLibr) + %trimR(wFile) + ')';
036100150513         exsr  sr_ExecCmd;
036200150512
036300150512       ENDSR;
036400150413
036500150413       //--------------------------------------------------------------
036600150413       //?Import dei nuovi dati delle Pubbliche Amministrazioni.       ?
036700150413       //--------------------------------------------------------------
036800150413       BEGSR  sr_Import;
036900150415
037000150415         Qcmd = 'DLTF file(' + %trimR(wLibr) + %trimR(wFile) + 'x)';
037100150415         exsr  sr_ExecCmd;
037200150415
037300150415         Qcmd = 'CPYF fromfile(' + %trimR(wLibr) + %trimR(wFile) + ') +
037400150415                      tofile(' + %trimR(wLibr) + %trimR(wFile) + 'x) +
037500150415                      mbropt(*add) +
037600150415                      crtfile(*yes)';
037700150415         exsr  sr_ExecCmd;
037800150415         if  Qusei <> *blank;
037900150415           exsr  sr_PrintErr;
038000150512           leavesr;
038100150415         endif;
038200150415
038300150415         Qcmd = 'CLRPFM file(' + %trimR(wLibr) + %trimR(wFile) + ')';
038400150415         exsr  sr_ExecCmd;
038500150415         if  Qusei <> *blank;
038600150415           exsr  sr_PrintErr;
038700150512           leavesr;
038800150415         endif;
038900150413
039000150415         Qcmd = 'CPYFRMIMPF fromstmf(''' + %trimR( wSTMF ) + ''') +
039100150415                            tofile(' + %trimR(wLibr) + %trimR(wFile) + ') +
039200150415                            mbropt(*add) +
039300150415                            rcddlm(*ALL) +
039400150415                            strdlm(*NONE) +
039500150415                            flddlm(*TAB) +
039600150415                            rplNullVal(*FLDDFT)';
039700150413         exsr  sr_ExecCmd;
039800150415         if  Qusei <> *blank;
039900150415           exsr  sr_PrintErr;
040000150512           leavesr;
040100150415         endif;
040200150413
040300150413       ENDSR;
040400150415
040500150415       //--------------------------------------------------------------
040600150513       //?Copia del file AZIPA00F già esistente.                       ?
040700150415       //--------------------------------------------------------------
040800150415       BEGSR  sr_CpyF;
040900150415
041000150513         Qcmd = 'DLTF file(' + %trimR(wLibr) + %trimR(wFile) + 'x)';
041100150513         exsr  sr_ExecCmd;
041200150513
041300150513         Qcmd = 'CPYF fromfile(' + %trimR(wLibr) + %trimR(wFile) + ') +
041400150513                      tofile(' + %trimR(wLibr) + %trimR(wFile) + 'x) +
041500150513                      mbropt(*add) +
041600150513                      crtfile(*yes)';
041700150513         exsr  sr_ExecCmd;
041800150513         if  Qusei <> *blank;
041900150513           exsr  sr_PrintErr;
042000150513           leavesr;
042100150513         endif;
042200150415
042300150415       ENDSR;
042400150413
042500150413       //--------------------------------------------------------------
042600150414       //?Aggiornamento Codice Ente in AZIP100F.                       ?
042700150413       //--------------------------------------------------------------
042800150414       BEGSR  sr_UpdIP1cod;
042900150413
043000150414         // -?Ciclo di elaborazione file AZIP100F?
043100150415         open  AZIP100F;
043200150414         read  AZIP1000;
043300150414
043400150414         DoW  not %eof(AZIP100F);
043500150414
043600150414           // -?Conversione caratteri del codice da Maiuscoli a Minuscoli?
043700150414           //  ?in AZIP100F?
043800150415           wChar = %xlate( c_CharMin : c_CharMai : IP1cod : 1 );
043900150414
044000150414           If  IP1cod <> wChar;
044100150414             IP1cod = wChar;
044200150414             //_______________
044300150414             update  AZIP1000;
044400150414             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
044500150414           Else;
044600150414             unlock  AZIP100F;
044700150414           EndIf;
044800150414
044900150414           read  AZIP1000;
045000150414
045100150414         EndDo;
045200150415
045300150415         close  AZIP100F;
045400150413
045500150413       ENDSR;
045600150413
045700150413       //--------------------------------------------------------------
045800150414       //?Aggiornamento Codice Ente in AZIP200F.                       ?
045900150413       //--------------------------------------------------------------
046000150414       BEGSR  sr_UpdIP2camm;
046100150413
046200150414         // -?Ciclo di elaborazione file AZIP200F?
046300150415         open  AZIP200F;
046400150414         read  AZIP2000;
046500150414
046600150414         DoW  not %eof(AZIP200F);
046700150414
046800150414           // -?Conversione caratteri del codice da Maiuscoli a Minuscoli?
046900150414           //  ?in AZIP200F?
047000150415           wChar = %xlate( c_CharMin : c_CharMai : IP2camm : 1 );
047100150414
047200150414           If  IP2camm <> wChar;
047300150414             IP2camm = wChar;
047400150414             //_______________
047500150414             update  AZIP2000;
047600150414             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
047700150414           Else;
047800150414             unlock  AZIP200F;
047900150414           EndIf;
048000150414
048100150414           read  AZIP2000;
048200150414
048300150414         EndDo;
048400150415
048500150415         close  AZIP200F;
048600150413
048700150413       ENDSR;
048800150413
048900150413       //--------------------------------------------------------------
049000150414       //?Scrittura del file AZIPA00F.                                 ?
049100150413       //--------------------------------------------------------------
049200150414       BEGSR  sr_WrtAZIPA;
049300150414
049400150414         // -?Preparazione SQL per estrazione dati?
049500150414         exsr  sr_PrepSQL;
049600150414
049700150414         // -?Ciclo di elaborazione?
049800150414         exsr  sr_OpenCursor;
049900150415         open  AZIPA00F;
050000150414
050100150414         DoW  Not $EoF;
050200150414           exsr  sr_ReadCursor;
050300150414         EndDo;
050400150414
050500150415         close AZIPA00F;
050600150414         exsr  sr_CloseCursor;
050700150414
050800150414       ENDSR;
050900150414
051000150414       //--------------------------------------------------------------
051100150414       //?Preparazione SQL.                                            ?
051200150414       //--------------------------------------------------------------
051300150414       BEGSR  sr_PrepSQL;
051400150414
051500150414         // -?Preparazione SQL per AZIP200F?
051600150414         clear  wSQL;
051700150414         wSQL = 'select * +
051800150414                   from AZIP200F inner join AZIP100F +
051900150414                     on AZIP200F.IP2camm = AZIP100F.IP1cod +
052000150414                    for fetch only';
052100150414
052200150414         exec sql   prepare S1   from :wSQL;
052300150414
052400150414         // -?Dichiarazione cursore?
052500150414         exec sql   declare C1   cursor for S1;
052600150414
052700150414         if  SQLcode < *zero;
052800150414           $EoF = *on;
052900150414           exsr  sr_PrintErr;
053000150512           leavesr;
053100150414         endif;
053200150414
053300150414       ENDSR;
053400150414
053500150414       //--------------------------------------------------------------
053600150414       //?Apertura cursore.                                            ?
053700150414       //--------------------------------------------------------------
053800150414       BEGSR  sr_OpenCursor;
053900150414
054000150414         // -?Apertura del cursore?
054100150414         exec sql   open C1;
054200150414
054300150414         if  SQLcode < *zero;
054400150414           $EoF = *on;
054500150414           exsr  sr_PrintErr;
054600150512           leavesr;
054700150414         endif;
054800150414
054900150414       ENDSR;
055000150414
055100150414       //--------------------------------------------------------------
055200150414       //?Lettura cursore.                                             ?
055300150414       //--------------------------------------------------------------
055400150414       BEGSR  sr_ReadCursor;
055500150413
055600150415         clear  AZIP1_ds;
055700150415         clear  AZIP2_ds;
055800150413
055900150415         exec sql   fetch next   from C1   into :AZIP2_ds, :AZIP1_ds;
056000150413
056100150413         Select;
056200150413
056300150413           // -?Fine File?
056400150413           When  SQLcode = 100;
056500150413             $EoF = *on;
056600150413
056700150413           // -?Errore SQL?
056800150413           When  SQLcode < *zero;
056900150413             $EoF = *on;
057000150413             exsr  sr_PrintErr;
057100150512             leavesr;
057200150415
057300150415           // -?Record da scartare x mancanza del C.F.?
057400150415           When  IP1cf  = *blank  or  IP1cf = 'null ';
057500150415             leavesr;
057600150415
057700150415           // -?Record da scartare x mancanza dell'UFFICIO?
057800150415           When  IP2uff = *blank  or  IP2uff = 'null ';
057900150415             leavesr;
058000150413
058100150413           // -?Elaborazione singolo record?
058200150413           Other;
058300150415             exsr  sr_WrtRec;
058400150413
058500150413         EndSl;
058600150413
058700150413       ENDSR;
058800150413
058900150413       //--------------------------------------------------------------
059000150413       //?Chiusura cursore.                                            ?
059100150413       //--------------------------------------------------------------
059200150413       BEGSR  sr_CloseCursor;
059300150413
059400150413         // -?Chiusura del cursore?
059500150413         exec sql   close C1;
059600150413
059700150413       ENDSR;
059800150413
059900150413       //--------------------------------------------------------------
060000150414       //?Elaborazione singolo record da AZIP100F/AZIP200F.            ?
060100150413       //--------------------------------------------------------------
060200150415       BEGSR  sr_WrtRec;
060300150413
060400150415         clear  AZIPA000;
060500150415
060600150415         IPAcod   = IP1cod;
060700150415         IPAdes   = IP1des;
060800150415         IPAloc   = IP1loc;
060900150415         IPAcap   = IP1cap;
061000150415         IPAprv   = IP1prv;
061100150415         IPAind   = IP1ind;
061200150415         IPAcat   = IP1cat;
061300150415         IPAtip   = IP1tip;
061400150415         IPAvcf   = IP1vcf;
061500150415         IPAcf    = IP1cf;
061600150415
061700150415         if  IP2cod <> 'null ';
061800150415           IPAuoCod = IP2cod;
061900150415         endif;
062000150415         if  IP2aoo <> 'null ';
062100150415           IPAaoo   = IP2aoo;
062200150415         endif;
062300150415         IPAuff   = IP2uff;
062400150415         if  IP2des <> 'null ';
062500150415           IPAuoDes = IP2des;
062600150415         endif;
062700150415         if  IP2loc <> 'null ';
062800150415           IPAuoLoc = IP2loc;
062900150415         endif;
063000150415         if  IP2cap <> 'null ';
063100150415           IPAuoCap = IP2cap;
063200150415         endif;
063300150415         if  IP2prv <> 'null';
063400150415           IPAuoPrv = IP2prv;
063500150415         endif;
063600150415         if  IP2ind <> 'null ';
063700150415           IPAuoInd = IP2ind;
063800150415         endif;
063900150415         if  IP2mail1 <> 'null ';
064000150415           IPAuoEml = IP2mail1;
064100150415         endif;
064200150415
064300150513         // -?Scrittura record in AZIPA00F?
064400150415         //______________
064500150415         write  AZIPA000;
064600150415         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
064700150413
064800150413       ENDSR;
064900150413
065000150413       //--------------------------------------------------------------
065100150413       //?Stampa segnalazione dell'errore rilevato via SQL             ?
065200150413       //--------------------------------------------------------------
065300150413       BEGSR  sr_PrintErr;
065400150413
065500151015         ulIPAerr = *on;
065600151015         ulIPAmsg = sk_Msg(09);
065700150512
065800150413         // -?Stampa del Dump?
065900150413         Dump(A);
066000150413
066100150413         // -?Stampa del Job-Log?
066200150413         Qcmd = 'DSPJOBLOG job(*) output(*print)';
066300150413         exsr  sr_ExecCmd;
066400150413
066500150512         //// -?Chiusura del programma?
066600150512         ////*inH1 = *on;
066700150512         //exsr  sr_RoutEnd;
066800150413
066900150413       ENDSR;
067000150413
067100150413       //--------------------------------------------------------------
067200150413       //?Esecuzione del comando (già impostato).                      ?
067300150413       //--------------------------------------------------------------
067400150413       BEGSR  sr_ExecCmd;
067500150413
067600150413         clear Qcap0100;
067700150413         Qcabcsdh = *off;
067800150413         Qcapa    = *off;
067900150413         Qcacmdss = *off;
068000150413         Qcaerved = *allX'00';
068100150413
068200150413         clear Qusec;
068300150413         Qusbprv  = %size(Qusec);
068400150413
068500150413         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
068600150413                           %size(Qcap0100) : 'CPOP0100' : *omit :
068700150413                           0 : 0 : Qusec);
068800150413
068900150415         //if  Qusei <> *blank;
069000150415         //  exsr  sr_PrintErr;
069100150415         //endif;
069200150413
069300150413       ENDSR;
069400150413
069500150413       //--------------------------------------------------------------
069600150413       //?Operazioni finali.                                           ?
069700150413       //--------------------------------------------------------------
069800150413       BEGSR  sr_RoutEnd;
069900150413
070000150413         // -?Uscita dal *pgm?
070100150413         return;
070200150413
070300150413       ENDSR;
070400150413
070500150413      /end-free
070600150512
070700150512       //--------------------------------------------------------------
070800150512       //?Definizione schiere a tempo di compilazione.                 ?
070900150512       //--------------------------------------------------------------
071000150512
071100151015** --?sk_Msg:?Messaggi di Errore?---------------------------------------------*
0712001510151 - Indirizzario "Amministrazioni" obbligatorio?                               1
0713001510152 - Indirizzario "Unità Organizzative" obbligatorio?                           2
071400151015Richiesta "Importare dati su AS/400" con valore errato                         3
071500151015Richiesta "Aggiornare dati nel file AZIPA00F" con valore errato                4
071600151015Richiesta "Farne copia precedente" con valore errato                           5
071700151015Richiesta "Cancellare dati importati su AS/400" con valore errato              6
071800151015La copia dei dati già esistenti richiede l'aggiornamento di AZIPA00F           7
071900151015Impostare almeno una selezione: Importare e/o Aggiornare                       8
072000151015Rilevato errore: controllare la stampa                                         9
