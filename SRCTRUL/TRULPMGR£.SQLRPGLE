000100140218       //==============================================================
000200140218       //?Recupero Data/Scaglione da Prezzo gasolio                    ?
000300140218       //==============================================================
000400140218
000500140218       //--------------------------------------------------------------
000600140218       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700140218       //--------------------------------------------------------------
000800140218
000900140218     /*PRM  dbgview(*source)
001000140218     /*END
001100140218
001200140218       //--------------------------------------------------------------
001300140218       //?Specifiche di controllo.                                     ?
001400140218       //--------------------------------------------------------------
001500140218
001600140218     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700140218     h dftactgrp(*no)
001800140218
001900140218       //--------------------------------------------------------------
002000140218       //?Dichiarazione file.                                          ?
002100140218       //--------------------------------------------------------------
002200140218
002300140218       // -?Tabelle?
002400140218     fTNTBE01L  if   e           k disk
002500140218
002600140218       //--------------------------------------------------------------
002700140218       //?Definizione costanti.                                        ?
002800140218       //--------------------------------------------------------------
002900140218
003000140218       // -?Costanti per stringa SQL?
003100140218     d c_ForFetch      c                   const('FOR FETCH ONLY')
003200140218
003300140218       //--------------------------------------------------------------
003400140218       //?Definizione schiere.                                         ?
003500140218       //--------------------------------------------------------------
003600140218
003700140218       // -?Messaggi di errore?
003800140219     d sk_Msg          s             78    dim( 3)  ctdata  perrcd( 1)
003900140218
004000140218       // -?Scaglioni di Incremento del Prezzo del Gasolio?
004100140218     d sk_IPG          s                   dim(999) like(TIPMGds.PMGsca)
004200140219     d                                              inz(*hival)  ascend
004300140218     d sk_IPGmin       s                   dim(999) like(TIPMGds.PMGpmg)
004400140219     d                                              inz(*hival)  ascend
004500140218     d sk_IPGmax       s                   dim(999) like(TIPMGds.PMGpmg)
004600140219     d                                              inz(*hival)  ascend
004700140218
004800140218       //--------------------------------------------------------------
004900140218       //?Definizione aree dati.                                       ?
005000140218       //--------------------------------------------------------------
005100140218
005200140218       // -?Dati utente?
005300140218     d §AzUte        e ds                  extname(AZUTE00F)
005400140218     d                                     dtaara
005500140218     d §DatiUte      e ds                  extname(dDatiUte)
005600140218     d                                     dtaara
005700140218
005800140218       //--------------------------------------------------------------
005900140218       //?Definizione strutture dati.                                  ?
006000140218       //--------------------------------------------------------------
006100140218
006200140218       // -?Status ds?
006300140218     d Status         sds
006400140218     d   SDSpgm          *proc
006500140218
006600140218       // -?Parametri ricevuti?
006700140218     d KPJBA         e ds
006800140219     d trulPMGds     e ds                  inz   qualified
006900140218
007000140218       // -?Dati estratti via SQL?
007100140218     d TIPMGds       e ds                  extname(TIPMG00F)
007200140218     d                                     inz   qualified
007300140218
007400140218       // -?Tab. "IPG" = Scaglioni di Incremento del Prezzo del Gasolio?
007500140218     d dIPG          e ds                  inz   qualified
007600140218
007700140218       //--------------------------------------------------------------
007800140218       //?Definizione variabili globali.                               ?
007900140218       //--------------------------------------------------------------
008000140218
008100140218       // -?Indici di schiera?
008200140218     d xx              s              3  0 inz
008300140219     d yy              s              3  0 inz
008400140218
008500140218       // -?Stringa SQL da eseguire?
008600140218     d wSQL            s           5000    inz   varying
008700140218
008800140218       // -?Campi di comodo?
008900140218     d*// wDate_ISO       s               d   inz  datfmt(*iso)
009000140218     d*// wDate           s              8  0 inz
009100140218
009200140218       //--------------------------------------------------------------
009300140218       //?Definizione prototipi procedure.                             ?
009400140218       //--------------------------------------------------------------
009500140218
009600140218       // -?Reperimento dati utente?
009700140218     d TIBS34ds      e ds                  inz
009800140218      /copy gaitrasrc/srcProtoPR,TIBS34R
009900140218
010000140218       //--------------------------------------------------------------
010100140218       //?Definizione key-list.                                        ?
010200140218       //--------------------------------------------------------------
010300140218
010400140218
010500140218       //--------------------------------------------------------------
010600140218       //?M A I N - L I N E                                            ?
010700140218       //--------------------------------------------------------------
010800140218
010900140218     c     *Entry        plist
011000140218     c                   parm                    KPJBA
011100140218
011200140218      /free
011300140218
011400140218       // -?Operazioni iniziali?
011500140218       exsr sr_RoutInz;
011600140218
011700140218       // -?Apertura cursore?
011800140218       exsr sr_OpenCursor;
011900140218
012000140218       // -?Lettura cursore?
012100140218       exsr sr_ReadCursor;
012200140218
012300140218       // -?Chiusura cursore?
012400140218       exsr sr_CloseCursor;
012500140218
012600140218       // -?Operazioni finali?
012700140218       exsr sr_RoutEnd;
012800140218
012900140218       //--------------------------------------------------------------
013000140218       //?Operazioni iniziali.                                         ?
013100140218       //--------------------------------------------------------------
013200140218       BEGSR  sr_RoutInz;
013300140218
013400140218         // -?Impostazione opzioni SQL?
013500140218         exec sql  set option  DynUsrPrf = *Owner,
013600140218                               CloSqlCsr = *EndMod;
013700140218
013800140218         *inLR = *on;
013900140218
014000140218         // -?Ricezione parametri?
014100140219         trulPMGds = KPJBU;
014200140218
014300140219         clear  trulPMGds.oPMGdtd;
014400140219         clear  trulPMGds.oPMGsca;
014500140219         clear  trulPMGds.oPMGpmg;
014600140219         trulPMGds.oPMGerr = *off;
014700140219         clear  trulPMGds.oPMGmsg;
014800140218
014900140218         // -?Reperimento dati job?
015000140218         exsr  sr_DatiJob;
015100140218
015200140218         // -?Reperimento data odierna?
015300140218         //wDate_Iso = %date();
015400140218         //wDate = %dec( wDate_Iso );
015500140218
015600140218         // -?Intabellamento tab. "IPG"?
015700140219         //reset  sk_IPG;     ?(già così)?
015800140219         //reset  sk_IPGmin;  ?(già così)?
015900140219         //reset  sk_IPGmax;  ?(già così)?
016000140219         clear  xx;
016100140218         TBEcod = 'IPG';
016200140218         setll  ( TBEcod )  TNTBE000;
016300140218         reade  ( TBEcod )  TNTBE000;
016400140218         DoW  NOT  %eof(TNTBE01L);
016500140218           if  TBEatb  = *blank;
016600140218             dIPG = TBEuni;
016700140218             xx += 1;
016800140218             sk_IPG(xx)    = %int( %trimr( TBEke1 ) );
016900140218             sk_IPGmin(xx) = dIPG.§IPGmin;
017000140218             sk_IPGmax(xx) = dIPG.§IPGmax;
017100140218           endif;
017200140218           reade  ( TBEcod )  TNTBE000;
017300140218         EndDo;
017400140218
017500140218       ENDSR;
017600140218
017700140218       //--------------------------------------------------------------
017800140218       //?Reperimento Dati del job (Utente/Operativi).                 ?
017900140218       //--------------------------------------------------------------
018000140218       BEGSR  sr_DatiJob;
018100140218
018200140218         in(e) §AzUte;
018300140218         if NOT %error;
018400140218           in(e) §DatiUte;
018500140218         endif;
018600140218         if %error or RSut = *blank;
018700140218           tibs34r ( tibs34ds );
018800140218           in §AzUte;
018900140218           in §DatiUte;
019000140218         endif;
019100140218
019200140218       ENDSR;
019300140218
019400140218       //--------------------------------------------------------------
019500140218       //?Preperazione / Dichiarazione / Apertura cursore SQL.         ?
019600140218       //--------------------------------------------------------------
019700140218       BEGSR  sr_OpenCursor;
019800140218
019900140218         // -?Preparazione stringa SQL?
020000140218         //  ?per la ricerca di un prezzo uguale a quello ricevuto?
020100140218         wSQL = 'select * +
020200140218                   from TIPMG00F +
020300140218                  where PMGpmg = ' +
020400140219                          %trim( %editc( trulPMGds.iPMGpmg : '3' ) ) +
020500140218                ' order by PMGpmg, PMGsca desc, PMGdtd desc ' +
020600140218                c_ForFetch;
020700140218
020800140218         // -?Dichiarazione cursore?
020900140218         exec sql   prepare S1   from :wSQL;
021000140218         exec sql   declare C1   cursor for S1;
021100140218
021200140218         if  SQLcode < *zero;
021300140219           trulPMGds.oPMGerr = '9';
021400140219           trulPMGds.oPMGmsg = sk_Msg(01);
021500140218           exsr  sr_RoutEnd;
021600140218         endif;
021700140218
021800140218         // -?Apertura del cursore?
021900140218         exec sql   open C1;
022000140218
022100140218         if  SQLcode < *zero;
022200140219           trulPMGds.oPMGerr = '9';
022300140219           trulPMGds.oPMGmsg = sk_Msg(01);
022400140218           exsr  sr_RoutEnd;
022500140218         endif;
022600140218
022700140218       ENDSR;
022800140218
022900140218       //--------------------------------------------------------------
023000140218       //?Lettura SQL.                                                 ?
023100140218       //--------------------------------------------------------------
023200140218       BEGSR  sr_ReadCursor;
023300140218
023400140218         clear  TIPMGds;
023500140218
023600140218         exec sql   fetch next   from C1   into :TIPMGds;
023700140218
023800140218         select;
023900140218
024000140218           // -?Rilevato errore SQL?
024100140218           when  SQLcode < *zero;
024200140219             trulPMGds.oPMGerr = '9';
024300140219             trulPMGds.oPMGmsg = sk_Msg(01);
024400140218
024500140218           // -?Nessun dato estratto?
024600140218           when  SQLcode = 100;
024700140219             //trulPMGds.oPMGerr = *on;
024800140219             //trulPMGds.oPMGmsg = sk_Msg(02);
024900140218             exsr  sr_Chk_Scagl;
025000140218
025100140218           // -?Prezzo Medio estratto?
025200140218           other;
025300140219             trulPMGds.oPMGdtd = TIPMGds.PMGdtd;
025400140219             trulPMGds.oPMGsca = TIPMGds.PMGsca;
025500140219             //if  trulPMGds.iPMGpmg <> TIPMGds.PMGpmg;
025600140219               trulPMGds.oPMGpmg = TIPMGds.PMGpmg;
025700140219             //endif;
025800140218
025900140218         endsl;
026000140218
026100140218       ENDSR;
026200140218
026300140218       //--------------------------------------------------------------
026400140218       //?Chiusura cursore SQL.                                        ?
026500140218       //--------------------------------------------------------------
026600140218       BEGSR  sr_CloseCursor;
026700140218
026800140218         // -?Chiusura del cursore SQL?
026900140218         exec sql   close C1;
027000140218
027100140218       ENDSR;
027200140218
027300140218       //--------------------------------------------------------------
027400140218       //?Ricerca del prezzo maggiore a parità di scaglione.           ?
027500140218       //--------------------------------------------------------------
027600140218       BEGSR  sr_Chk_Scagl;
027700140218
027800140218         // -?Ricerca scaglione?
027900140219         yy = %lookupGE ( trulPMGds.iPMGpmg : sk_IPGmax );
028000140219
028100140219         select;
028200140219           // -?Prezzo NON rilevato tra gli scaglioni?
028300140219           when  yy = *zero;
028400140219             trulPMGds.oPMGerr = *on;
028500140219             trulPMGds.oPMGmsg = sk_Msg(02);
028600140219             leavesr;
028700140219           // -?Prezzo NON rilevato tra gli scaglioni?
028800140219           when  sk_IPGmin(yy) > trulPMGds.iPMGpmg;
028900140219             trulPMGds.oPMGerr = *on;
029000140219             trulPMGds.oPMGmsg = sk_Msg(02);
029100140219             leavesr;
029200140219         endsl;
029300140219
029400140218         // -?Apertura cursore?
029500140218         exsr sr_OpenCursor2;
029600140218
029700140218         // -?Lettura cursore?
029800140218         exsr sr_ReadCursor2;
029900140218
030000140218         // -?Chiusura cursore?
030100140218         exsr sr_CloseCursor2;
030200140218
030300140218       ENDSR;
030400140218
030500140218       //--------------------------------------------------------------
030600140218       //?Preperazione / Dichiarazione / Apertura cursore SQL-2.       ?
030700140218       //--------------------------------------------------------------
030800140218       BEGSR  sr_OpenCursor2;
030900140218
031000140218         // -?Preparazione stringa SQL?
031100140219         //  ?per la ricerca del prezzo più vicino a quello ricevuto  e?
031200140219         //  ?nello stesso scaglione di quello ricevuto?
031300140218         wSQL = 'select * +
031400140218                   from TIPMG00F +
031500140219                  where PMGsca = ' + %trim( %editc( sk_IPG(yy) : '3' ) ) +
031600140219                  ' and PMGpmg >= ' +
031700140219                           %trim( %editc( trulPMGds.iPMGpmg : '3' ) ) +
031800140219                ' order by PMGpmg, PMGdtd desc ' +
031900140218                c_ForFetch;
032000140218
032100140218         // -?Dichiarazione cursore?
032200140218         exec sql   prepare S2   from :wSQL;
032300140218         exec sql   declare C2   cursor for S2;
032400140218
032500140218         if  SQLcode < *zero;
032600140219           trulPMGds.oPMGerr = '9';
032700140219           trulPMGds.oPMGmsg = sk_Msg(01);
032800140218           exsr  sr_RoutEnd;
032900140218         endif;
033000140218
033100140218         // -?Apertura del cursore?
033200140218         exec sql   open C2;
033300140218
033400140218         if  SQLcode < *zero;
033500140219           trulPMGds.oPMGerr = '9';
033600140219           trulPMGds.oPMGmsg = sk_Msg(01);
033700140218           exsr  sr_RoutEnd;
033800140218         endif;
033900140218
034000140218       ENDSR;
034100140218
034200140218       //--------------------------------------------------------------
034300140218       //?Lettura SQL-2.                                               ?
034400140218       //--------------------------------------------------------------
034500140218       BEGSR  sr_ReadCursor2;
034600140218
034700140218         clear  TIPMGds;
034800140218
034900140218         exec sql   fetch next   from C2   into :TIPMGds;
035000140218
035100140218         select;
035200140218
035300140218           // -?Rilevato errore SQL?
035400140218           when  SQLcode < *zero;
035500140219             trulPMGds.oPMGerr = '9';
035600140219             trulPMGds.oPMGmsg = sk_Msg(01);
035700140218
035800140219           // -?Nessun dato estratto (scaglione mai usato)?
035900140218           when  SQLcode = 100;
036000140219             trulPMGds.oPMGerr = '2';
036100140219             trulPMGds.oPMGmsg = sk_Msg(03);
036200140218
036300140218           // -?Prezzo Medio estratto?
036400140218           other;
036500140219             trulPMGds.oPMGdtd = TIPMGds.PMGdtd;
036600140219             trulPMGds.oPMGsca = TIPMGds.PMGsca;
036700140219             //if  trulPMGds.iPMGpmg <> TIPMGds.PMGpmg;
036800140219               trulPMGds.oPMGpmg = TIPMGds.PMGpmg;
036900140219             //endif;
037000140218
037100140218         endsl;
037200140218
037300140218       ENDSR;
037400140218
037500140218       //--------------------------------------------------------------
037600140218       //?Chiusura cursore SQL-2.                                      ?
037700140218       //--------------------------------------------------------------
037800140218       BEGSR  sr_CloseCursor2;
037900140218
038000140218         // -?Chiusura del cursore SQL?
038100140218         exec sql   close C2;
038200140218
038300140218       ENDSR;
038400140218
038500140218       //--------------------------------------------------------------
038600140218       //?Operazioni finali.                                           ?
038700140218       //--------------------------------------------------------------
038800140218       BEGSR  sr_RoutEnd;
038900140218
039000140218         // -?Restituzione eventuali parametri?
039100140219         KPJBU = trulPMGds;
039200140218
039300140218         // -?Uscita dal *pgm?
039400140218         return;
039500140218
039600140218       ENDSR;
039700140218
039800140218      /end-free
039900140218
040000140218       //--------------------------------------------------------------
040100140218       //?Definizione schiere a tempo di compilazione                  ?
040200140218       //--------------------------------------------------------------
040300140218
040400140218** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
040500140218Rilevato errore SQL: avvisare il CED                                           1
040600140218NON reperito alcuno scaglione per il prezzo indicato                           2
040700140219Prezzo mai rilevato settimanalmente dal Ministero                              3
