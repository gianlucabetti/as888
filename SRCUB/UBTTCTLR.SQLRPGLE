000100121217       //==============================================================
000200151019       //?Verifico Carico orario T&T e Carico totale T&T               ?
000300121217       //==============================================================
000400141008
000500121217       //--------------------------------------------------------------
000600121217       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700121217       //--------------------------------------------------------------
000800121217
000900151102       //?*** COMPILARE COME INDICATO SOTTO!!!  ***
001000151020      *
001100151020      *?           ATTENZIONE!!!!!!!!!       ?
001200151020      *?l'oggetto in OBJ può essere diverso da quello su SETRAS perchè il pgm va ?
001300151020      *?per forza compilato su Setras
001400151020      * passaggi per creare il pgm su SETRAS
001500151102      * 1) NON serve che su SETRAS ci sia il DDM che punta alla DS TIVTL10 in quanto l'SQL punta al
001600151102      *    file con la libreria. Se cambiasse l'SQL, controllare il DDM e se non c'è crearlo:
001700151020      *    DSPDDMF TIVTL10F
001800151020      *    CRTDDMF FILE(TIVTL10F) RMTFILE(STRATEBAR/TIVTL10F) RMTLOCNAME(as777 *IP)
001900151020      * 2) controllare su AS777 che l'utente con cui si accede al DB esista ancora
002000151102      * 3) eseguire su SETRAS il cmd CRTSQLRPGI con questi parametri
002100151020      *    CRTSQLRPGI OBJ(GAITRAOBJ/UBTTCTLR) SRCFILE(GAITRASRC/SRCUB) SRCMBR(UBTTCTLR)
002200151102      *    RDB(AS777) OBJTYPE(*MODULE) USER(VAS777) PASSWORD() CLOSQLCSR(*ENDMOD) DYNUSRPRF(*OWNER)
002300151102      *    in modo da avere il modulo, e poi creare il pgm per dargli l'ACTGRP
002400151102      *    CRTPGM PGM(GAITRAOBJ/UBTTCTLR) BNDDIR(UBBNDDIR) ACTGRP(QILE)
002500151102      *    infine eliminare il modulo
002600151102      *    DLTMOD MODULE(GAITRAOBJ/UBTTCTLR)
002700151020      * 4) il cmd crea su AS777 l'SQLPKG UBTTCTLR per l'esecuzione dell'SQL
002800151020      *
002900121217
003000121217       //--------------------------------------------------------------
003100121217       //?Specifiche di controllo.                                     ?
003200121217       //--------------------------------------------------------------
003300121217
003400121217     h decedit('0,') datedit(*dmy/) option(*nodebugio)
003500141029       // per dire che i calcoli intermedi devono avere lo stesso num. decimali del risultato
003600151020       // h expropts(*resdecpos)
003700121217       //--------------------------------------------------------------
003800121217       //?Dichiarazione file.                                          ?
003900121217       //--------------------------------------------------------------
004000121217
004100121217       //--------------------------------------------------------------
004200121217       //?Definizione costanti.                                        ?
004300121217       //--------------------------------------------------------------
004400121217
004500121217       // -?Codice utente conti?
004600121217     d c_Kut           c                   const(1)
004700121218
004800121218       // -?Costanti per la definizione delle schiere con i nomi?
004900121218       //  ?degli iSeries da elaborare e delle relative librerie?
005000140925     d c_NrSyst        c                   const(2)
005100140925     d c_NrLibr        c                   const(2)
005200121217
005300121217       //--------------------------------------------------------------
005400121217       //?Definizione schiere.                                         ?
005500121217       //--------------------------------------------------------------
005600121217
005700121218       // -?iSeries  &  Librerie con entrambi i file tabelle?
005800121218     d $iSystem        s                   like(currSysNetA)
005900121218     d                                     dim(c_NrSyst)
006000121218     d                                     ctdata   perrcd( 1)
006100121218     d $Libraries      s                   like(ds_Libr)
006200121218     d                                     dim(c_NrSyst)
006300121218     d                                     alt($iSystem)
006400121217
006500121217       //--------------------------------------------------------------
006600121217       //?Definizione aree dati.                                       ?
006700121217       //--------------------------------------------------------------
006800121217
006900121218       // -?Dati utente?
007000121218     d §AzUte        e ds                  extname(AZUTE00F)
007100121218     d                                     dtaara
007200121218     d §DatiUte      e ds                  extname(dDatiUte)
007300121218     d                                     dtaara
007400121217
007500121217       //--------------------------------------------------------------
007600121217       //?Definizione strutture dati.                                  ?
007700121217       //--------------------------------------------------------------
007800121217
007900121217       // -?Parametri in input?
008000121218     d KPJBA         e ds
008100121218
008200121218       // -?Ridefinizione elenco librerie in cui elaborare le tabelle?
008300140925     d ds_Libr         ds                  inz
008400121218     d   $Libr                       10    dim(c_NrLibr) inz
008500121217
008600121217       // -?Status?
008700121217     d Psds           sds
008800121217     d   SDSpgm          *proc
008900121217     d*//JobName             244    253                                         Job name
009000121217     d*//JobUser             254    263                                         User name
009100121217     d*//JobNumber           264    269s 0                                      Job number
009200121217
009300121217       //--------------------------------------------------------------
009400121217       //?Definizione variabili.                                       ?
009500121217       //--------------------------------------------------------------
009600121217
009700121217       // -?Flags booleani?
009800121217     d $EoF            s               n   inz
009900121217
010000121217       // -?Indici di schiera?
010100121217     d xx              s              5  0 inz
010200121217     d yy              s              5  0 inz
010300121217     d ww              s              5  0 inz
010400140925     d wX              s              5  0 inz
010500140925     d wY              s              5  0 inz
010600121218
010700121218       // -?Nome esteso Libreria/File dei 2 file tabelle?
010800121220     d wLibTABEL       s             21a   inz
010900121218     d wLibTNTBE       s             21a   inz
011000121217
011100121217       // -?Campi di comodo?
011200121218     d w_iSystem       s              1  0 inz
011300121218     d w_SisInf        s              3  0 inz
011400141028     d wDataDa         s              8  0 inz
011500141028     d wDataA          s              8  0 inz
011600141028     d wDay            s              8  0 inz
011700141028     d dateISO         s               d
011800121218     d wErr            s             10i 0 inz
011900141021     d dayNbr          s             11p 0
012000151020     D pMsgV           s           5000    inz varying
012100141028     d wVideo          s               n
012200141030     D wn20            s             20s 0
012300141030     D wn14            s             14s 0
012400141030     D wn14_2          s             14s 0
012500121218
012600141027       // -?Dati di  dialogo
012700151019     D UBTTCTLDS       DS
012800151019       //elaboro per oggi: TTDay='0'  oppure ieri: TTDay='-1D'
012900151019     D  TTDay                         3a
013000151021     D  TTRichOra                     5s 0
013100151019     D  IndEMail                    126a
013200141027
013300121217       // -?Stringa SQL da eseguire?
013400121217     d wSQL            s           2048    inz  varying
013500151020     d* risultato fetch oggi
013600151020     d wSQL1_dati      ds                  inz  qualified
013700151020     d  wSQL1_RQSData                  d   inz
013800151020     d  wSQL1_RQSOra                  2s 0 inz
013900151020     d  wSQL1_CltIPAd                15a   inz
014000151020     d  wSQL1_KsU                     8a   inz
014100151020     d  wSQL1_TotRich                 7s 0 inz
014200151020     d* risultato fetch ieri
014300151020     d wSQL2_dati      ds                  inz  qualified
014400151020     d  wSQL2_RQSData                  d   inz
014500151020     d  wSQL2_CltIPAd                15a   inz
014600151020     d  wSQL2_KsU                     8a   inz
014700151020     d  wSQL2_TotRich                 7s 0 inz
014800151020
014900151020     D ConnectionAlreadyExists...
015000151020     D                 C                   -842
015100151020     D TAB             c                   x'05'
015200121217
015300121217       //--------------------------------------------------------------
015400121217       //?Definizione prototipi procedure.                             ?
015500121217       //--------------------------------------------------------------
015600121218
015700121218       // -?Reperimento NETA sistema AS/400 corrente?
015800121218     d currSysNeta     s              8a   inz
015900121218      /copy gaitrasrc/srcProtoPR,UBRTVNETA
016000121218
016100121218       // -?Reperimento dati utente?
016200121218     d TIBS34ds      e ds
016300121218      /copy gaitrasrc/srcProtoPR,TIBS34R
016400121217
016500121217       // -?Parametri API QCAPCMD (Process Commands)?
016600121217     d Qcmd            s           2048    inz  varying
016700121217
016800121217       //--------------------------------------------------------------
016900121217       //?Definizione key-list.                                        ?
017000121217       //--------------------------------------------------------------
017100121217
017200121217
017300121217       //--------------------------------------------------------------
017400121217       //?Definizione parametri procedura.                             ?
017500121217       //--------------------------------------------------------------
017600121217
017700121217     c     *Entry        plist
017800121217     c                   parm                    KPJBA
017900121217
018000121217      /free
018100121217
018200121217       //--------------------------------------------------------------
018300121217       //?M A I N - L I N E                                            ?
018400121217       //--------------------------------------------------------------
018500121217
018600121217       // -?Operazioni iniziali?
018700121217       exsr sr_RoutInz;
018800121217
018900121217       // -?Ciclo di reperimento clienti in tab. "3C"?
019000121218       doW  Not $EoF;
019100121217         exsr  sr_ReadCursor;
019200121218       enddo;
019300140925
019400140925       // -?viene mandata una e-mail a chi è preposto?
019500141028       //  ?se non si deve passare il risultato al chiamante
019600141028       if wVideo = *off;
019700141028         exsr  sr_SendEmail;
019800141028       endif;
019900121217
020000121217       // -?Operazioni finali?
020100121217       exsr sr_RoutEnd;
020200121217
020300121217       //--------------------------------------------------------------
020400121217       //?Operazioni iniziali.                                         ?
020500121217       //--------------------------------------------------------------
020600121217       BEGSR  sr_RoutInz;
020700141217
020800151019         UBTTCTLDS = kpjbu;
020900151019         // se NON è stato passato il "quando" imposto come oggi=0
021000151021         if TTDay = *blank;
021100151019           TTDay = '0';
021200151019         endif;
021300151021         // se NON è stato passato il numero di richieste x ora da controllare imposto 3600
021400151021         if TTRichOra = 0;
021500151021           TTRichOra = 3600;
021600151021         endif;
021700121217
021800121217         exec sql  set option  dynusrprf = *owner,
021900121217                               closqlcsr = *endmod;
022000121218
022100121218         // -?Verifica del sistema AS/400 corrente?
022200121218         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
022300121218           $EoF = *on;
022400121218           leavesr;
022500121218         endif;
022600121219
022700121219         // -?Reperimento dati job?
022800121219         exsr  sr_DatiJob;
022900140925
023000141028         // il risultato va inviato via email
023100141217         wVideo = *off;
023200141217         // se è stato passato un indirizzo email specifico, uso quello altrimenti uso i default
023300141217         if IndEMail <> *blank;
023400141217           peml = %trim(IndEMail);
023500141217         else;
023600151019           // il default destinatario è sempre quello qualunque sistema sia
023700171006           peml = 'ced@brt.it';
023800171006           pcceml = *blank;
023900141217         endif;
024000140925
024100151019         select;
024200151019          // oggi
024300151019          when %trim(TTDay) = '0';
024400151020           pogg = 'VAS T&T - CARICO ORARIO TRACKING ' +
024500151020                  %char(%date() : *dmy/);
024600151019          // ieri
024700151019          when %trim(TTDay) = '-1D';
024800151020           pogg = 'VAS T&T - CARICO TOTALE TRACKING ' +
024900151020                  %char(%date() - %days(1) : *dmy/);
025000151019
025100151019         endsl;
025200151019
025300141217         pMsgV = *blank;
025400151020         pMsgV = %trim(pMsgV) +
025500151020                 ':/N';
025600151020         select;
025700151020          // oggi
025800151020          when %trim(TTDay) = '0';
025900151020           pMsgV = %trim(pMsgV) +
026000151020                 'Richiesta data/ora' + TAB;
026100151020
026200151020          when %trim(TTDay) = '-1D';
026300151020           pMsgV = %trim(pMsgV) +
026400151020                 'Richiesta' + TAB;
026500151020         endsl;
026600151020         pMsgV = %trim(pMsgV) +
026700170720                 'Client IP Add.' + TAB + TAB +
026800151020                 'Unificante' + TAB +
026900151020                 'Tot. richiesta';
027000121217
027100121217         // -?Preparazione stringa SQL?
027200121217         exsr  sr_PrepSQL;
027300151019
027400151019         // mi connetto ad AS777
027500151019         exsr ConnectToAS777;
027600121217
027700121217         // -?Apertura cursore SQL?
027800121217         exsr  sr_OpenCursor;
027900140925
028000140925         clear  wX;
028100140925         clear  wY;
028200130208
028300140925       ENDSR;
028400121219
028500121219       //--------------------------------------------------------------
028600121219       //?Reperimento Dati del job (Utente/Operativi).                 ?
028700121219       //--------------------------------------------------------------
028800121219       BEGSR  sr_DatiJob;
028900141028
029000121219         in(E) §AzUte;
029100121219         if NOT %error;
029200121219           in(E) §DatiUte;
029300121219         endif;
029400121219         if %error or RSut = *blanks;
029500121219           clear TIBS34ds;
029600121219           tibs34r ( tibs34ds );
029700121219           in §AzUte;
029800121219           in §DatiUte;
029900121219         endif;
030000140925
030100121219       ENDSR;
030200141028
030300141028       //--------------------------------------------------------------
030400141028       //?Preparazione stringa SQL                                     ?
030500141028       //--------------------------------------------------------------
030600141028       BEGSR  sr_PrepSQL;
030700141028
030800151019         select;
030900151019          // oggi
031000151019          when %trim(TTDay) = '0';
031100151019           wSQL =
031200151020           'select +
031300151020            date(VTLRQSMMT) as VTLRQSDATA, +
031400151020            hour(VTLRQSMMT) as VTLRQSORA, +
031500151020            VTLCLTIPAD, VTLKSU, +
031600151020            count(*) as TOT_RICHIESTE_ORA +
031700151021            from stratebar/tivtl10f +
031800151020            where date(VTLRQSMMT) = curdate() +
031900151020            group by +
032000151020            date(VTLRQSMMT), hour(VTLRQSMMT), VTLCLTIPAD, VTLKSU +
032100151021            having count(*) > ';
032200151021           wSQL = wSQL + %char(TTRichOra) +
032300151021           ' order by VTLRQSORA desc, TOT_RICHIESTE_ORA desc';
032400151019
032500151019          // ieri
032600151019          when %trim(TTDay) = '-1D';
032700160705           // precedente SQL (lo teniamo per un po')
032800160705           //wSQL =
032900160705           //'select +
033000160705           // date(VTLRQSMMT) as VTLRQSDATA, +
033100160705           // VTLCLTIPAD, VTLKSU, +
033200160705           // count(*) as TOT_RICHIESTE +
033300160705           // from stratebar/tivtl10f +
033400160705           // where date(VTLRQSMMT) = date(days(curdate())- 1) +
033500160705           // group by +
033600160705           // date(VTLRQSMMT), VTLCLTIPAD, VTLKSU +
033700160705           // having count(*) > ';
033800160705           wSQL =
033900160705           'select +
034000160705            VTLRQSDATA, +
034100160705            VTLCLTIPAD, CASE_VTLKSU, +
034200160705            count(*) as TOT_RICHIESTE from +
034300160705            (select date(VTLRQSMMT) as VTLRQSDATA, VTLCLTIPAD, +
034400160705            case when length(trim(VTLKSU))=7 then VTLKSU +
034500160705                 when length(trim(VTLKSU))=8 then substr(VTLKSU, 2, 7)  +
034600160705                 else VTLKSU end as CASE_VTLKSU +
034700160705            from stratebar/tivtl10f +
034800160705            where date(VTLRQSMMT) = date(days(curdate())- 1) ) as LOG_TIVTL10 +
034900160705            group by +
035000160705            VTLRQSDATA, VTLCLTIPAD, CASE_VTLKSU +
035100160705            having count(*) > ';
035200151021           wSQL = wSQL + %char(TTRichOra) +
035300151021           ' order by TOT_RICHIESTE desc';
035400151019
035500151019         endsl;
035600151019
035700141020       ENDSR;
035800121217
035900121217       //--------------------------------------------------------------
036000121217       //?Apertura cursore.                                            ?
036100121217       //--------------------------------------------------------------
036200121217       BEGSR  sr_OpenCursor;
036300121217
036400121217         // -?Dichiarazione cursore?
036500121217         exec sql   prepare S1   from :wSQL;
036600141029         exec sql   declare C1  asensitive   cursor for S1;
036700121217
036800121217         // -?Apertura del cursore?
036900121217         exec sql   open C1;
037000121217
037100121217         if  SQLcode < *zero;
037200121217           $EoF = *on;
037300121217           wErr = 1;
037400151020           pMsgV = %trim(pMsgV) + ':/N' + '*** ERRORE SQL: ' +
037500151020                   %char(SQLCode) + ' ***';
037600121217         endif;
037700121217
037800121217       ENDSR;
037900121217
038000121217       //--------------------------------------------------------------
038100121217       //?Lettura cursore.                                             ?
038200121217       //--------------------------------------------------------------
038300121217       BEGSR  sr_ReadCursor;
038400121217
038500151020         clear  wSQL1_dati;
038600151020         clear  wSQL2_dati;
038700151020
038800151020         select;
038900151020          // oggi
039000151020          when %trim(TTDay) = '0';
039100151020           exec sql   fetch next   from C1   into :wSQL1_dati;
039200151020          // oggi
039300151020          when %trim(TTDay) = '-1D';
039400151020           exec sql   fetch next   from C1   into :wSQL2_dati;
039500151020         endsl;
039600141008
039700151020         //le righe successive servono per facilitare il debug
039800151020         //wY += 1;
039900151020         //if %rem(wY:1000) = 0;
040000151020         //  debug1000 = *on;
040100151020         //endif;
040200151020         //debug1000 = *off;
040300141021
040400151020         select;
040500121217
040600151020          when  SQLcode < *zero;
040700151020           $EoF = *on;
040800151020           wErr = 2;
040900151020           pMsgV = %trim(pMsgV) + ':/N' + '*** ERRORE SQL: ' +
041000151020                   %char(SQLCode) + ' ***';
041100151020
041200151020          when  SQLcode = 100;
041300151020           pMsgV = %trim(pMsgV) +
041400151020           ':/N' +
041500151020           ':/N' +
041600151020           ':/N' +
041700151020           'L''SQL con cui si sono estratti questi dati è:' +
041800151020           ':/N' +
041900151020           wSQL ;
042000151020           $EoF = *on;
042100151020
042200151020          other;
042300151020           // se mando il risultato via email
042400151020           if wVideo = *off;
042500151020             select;
042600151020              // oggi
042700151020              when %trim(TTDay) = '0';
042800151020               // preparo il msg
042900151020               //  wSQL1_RQSData                 d   inz
043000151020               //  wSQL1_RQSOra                 2s 0 inz
043100151020               //  wSQL1_CltIPAd               15a   inz
043200151020               //  wSQL1_KsU                    8a   inz
043300151020               //  wSQL1_TotRich                7s 0 inz
043400151020               pMsgV = %trim(pMsgV) +
043500151020               ':/N' +
043600151020               %char(wSQL1_dati.wSQL1_RQSData:*dmy/) + TAB +
043700151020               %editc(wSQL1_dati.wSQL1_RQSOra  : '1') + TAB +
043800170720               %trim(wSQL1_dati.wSQL1_CltIPAd);
043900170720               if %len(%trim(wSQL1_dati.wSQL1_CltIPAd)) <= 14;
044000170720                 pMsgV = %trim(pMsgV) + TAB +TAB;
044100170720               else;
044200170720                 pMsgV = %trim(pMsgV) + TAB;
044300170720               endif;
044400151020               pMsgV = %trim(pMsgV) +
044500170720               %trim(wSQL1_dati.wSQL1_KSU);
044600170720               if %len(%trim(wSQL1_dati.wSQL1_KSU)) <= 6;
044700170720                 pMsgV = %trim(pMsgV) + TAB + TAB;
044800170720               else;
044900170720                 pMsgV = %trim(pMsgV) + TAB;
045000170720               endif;
045100151020               pMsgV = %trim(pMsgV) +
045200151020               %editc(wSQL1_dati.wSQL1_TotRich  : '1');
045300151020
045400151020              // ieri
045500151020              when %trim(TTDay) = '-1D';
045600151020               // preparo il msg
045700151020               //  wSQL2_RQSData                 d   inz
045800151020               //  wSQL2_CltIPAd               15a   inz
045900151020               //  wSQL2_KsU                    8a   inz
046000151020               //  wSQL2_TotRich                7s 0 inz
046100151020               pMsgV = %trim(pMsgV) +
046200151020               ':/N' +
046300151020               %char(wSQL2_dati.wSQL2_RQSData:*dmy/) + TAB +
046400170720               %trim(wSQL2_dati.wSQL2_CltIPAd);
046500170720               if %len(%trim(wSQL2_dati.wSQL2_CltIPAd)) <= 14;
046600170720                 pMsgV = %trim(pMsgV) + TAB +TAB;
046700170720               else;
046800170720                 pMsgV = %trim(pMsgV) + TAB;
046900170720               endif;
047000151020               pMsgV = %trim(pMsgV) +
047100170720               %trim(wSQL2_dati.wSQL2_KSU);
047200170720               if %len(%trim(wSQL2_dati.wSQL2_KSU)) <= 6;
047300170720                 pMsgV = %trim(pMsgV) + TAB + TAB;
047400170720               else;
047500170720                 pMsgV = %trim(pMsgV) + TAB;
047600170720               endif;
047700151020               pMsgV = %trim(pMsgV) +
047800151020               %editc(wSQL2_dati.wSQL2_TotRich  : '1');
047900151020
048000151020             endsl;
048100141021
048200151020             // lo aggiungo nella email
048300151020             exsr AggEmail;
048400141028
048500151020           // se mando il risultato al chiamante (CASO NON CONTEMPLATO)
048600151020           else;
048700151020           endif;
048800151020
048900151020         endsl;
049000121217
049100121217       ENDSR;
049200121217
049300121217       //--------------------------------------------------------------
049400121217       //?Chiusura cursore.                                            ?
049500121217       //--------------------------------------------------------------
049600121217       BEGSR  sr_CloseCursor;
049700121217
049800121217         // -?Chiusura del cursore?
049900121217         exec sql   close C1;
050000121217
050100121217       ENDSR;
050200121217
050300121217       //--------------------------------------------------------------
050400121217       //?Invio e-mail di avviso errore.                               ?
050500121217       //--------------------------------------------------------------
050600121217       BEGSR  sr_SendEmail;
050700140926
050800141021         pmsg = pMsgV;
050900141021         // se sono qui, qualcosa ho scritto
051000141021         exsr EmailInvia;
051100121217
051200121217       ENDSR;
051300121217
051400121217       //--------------------------------------------------------------
051500121217       //?Operazioni finali.                                           ?
051600121217       //--------------------------------------------------------------
051700121217       BEGSR  sr_RoutEnd;
051800121217
051900121217         // -?Chiusura cursore SQL?
052000121217         exsr  sr_CloseCursor;
052100151019
052200151019         // mi disconnetto da AS777
052300151019         exsr DisconnectFromAS777;
052400151019
052500151019         // mi riconnetto a SETRAS
052600151019         exsr ConnectToSetras;
052700141028
052800141028         // -?valorizza i parametri di output?
052900151019         kpjbu = UBTTCTLDS;
053000121217
053100121217         // -?Chiusura pgm?
053200141029         *inlr = *on;
053300121217         return;
053400121217
053500121217       ENDSR;
053600121218
053700151019       //--------------------------------------------------------------
053800151019       // Connessione
053900151019       //--------------------------------------------------------------
054000151020       BEGSR  ConnectToAS777;
054100151019
054200151020         // mi connetto ad AS777
054300151020         exec sql  connect to AS777;
054400151020
054500151020         // se per qualche motivo ero già connesso
054600151020         if SQLCod = ConnectionAlreadyExists;
054700151020           // risveglio la connessione
054800151020           exec sql  set connection AS777;
054900151020         endif;
055000151020
055100151020         // se in qualunque caso ho un errore SQL
055200151019         if SQLCode < 0;
055300151020           // esco direttamente tanto non ho dati da restituire, non ho cursori da chiudere e
055400151020           // non devo disconnettermi e riconnettermi
055500151020           *inlr = *on;
055600151020           return;
055700151019         endif;
055800151019
055900151019       ENDSR;
056000151019
056100151019       //--------------------------------------------------------------
056200151019       // Disconnessione
056300151019       //--------------------------------------------------------------
056400151020       BEGSR  DisconnectFromAS777;
056500151019
056600151020         // mi disconnetto da AS777
056700151020         exec sql  disconnect AS777;
056800151019         // se la disconnessione ha dato errore
056900151019         if SQLCode < 0;
057000151020           // non faccio nulla perché sono già in procinto di uscire
057100151019         endif;
057200151019
057300151019       ENDSR;
057400151019
057500151019       //--------------------------------------------------------------
057600151019       // Riconnessione a SETRAS
057700151019       //--------------------------------------------------------------
057800151019       BEGSR  ConnectToSetras;
057900151019
058000151019         // mi riconnetto a SETRAS
058100151020         // siccome in BRT gli SQLRPGLE si compilano col parametro RDBCNNMTH = *DUW, la connect ad
058200151020         // AS777 ha reso dormiente la connessione a SETRAS.
058300151020         // In questo caso NON serve un cmd CONNECT RESET perché darebbe:
058400151019         // errore SQL0842 = Connessione al database relazionale &1 già esistente.
058500151020         // Però, la connessione al DB locale (SETRAS) esiste se prima è stato eseguito almeno uno
058600151020         // statement SQL.
058700151020         // In caso contrario la SET CONNECTION darebbe sempre:
058800151020         // errore SQL0842 = Connessione al database relazionale &1 già esistente.
058900151020         // Per cui, dopo aver disconnesso AS777, fare il cmd SET CONNECTION per "risvegliare"
059000151020         // SETRAS e se non funziona provare con CONNECT RESET.
059100151019         exec sql  set connection SETRAS;
059200151019         if SQLCode < 0;
059300151019           // provo con CONNECT RESET
059400151019           exec sql  connect reset;
059500151020           // se anche in questo caso ho errore, non faccio nulla perché sto uscendo
059600151019           if SQLCode < 0;
059700151019           endif;
059800151019         endif;
059900151019
060000151019       ENDSR;
060100151020
060200151020      /end-free
060300151019
060400140925     C*------------------------------------------------------------------------*
060500140925     C* AggEmail: aggiunge rcd all'e-mail
060600140925     C*------------------------------------------------------------------------*
060700141217     C     AggEmail      BEGSR
060800140925     C*
060900141021     C* questa routine non fa niente perché pMsgV è già stato valorizzato prima
061000140925     C*
061100140925     C                   ENDSR
061200140925     C*------------------------------------------------------------------------*
061300140925     C* EmailInvia - Invio e-mail di conferma forzatura
061400140925     C*------------------------------------------------------------------------*
061500140925     C     EmailInvia    BEGSR
061600140925     C*
061700140925     C* invio e-mail
061800140925     C                   call      'TIS701C1'
061900140925     C                   parm                    peml            253
062000140925     C                   parm                    pcceml          253
062100140925     C                   parm                    pogg             44
062200140925     C                   parm                    pmsg           5000
062300140925     C                   parm                    pesito            1
062400140925     C*
062500140925     C                   ENDSR
062600121218** -?$iSystem / $Libraries:?Sistemi AS/400 & Librerie con entrambi i file?
062700121218SETRAS  GAITRAGRU FILTRAGRU
062800121218AS888   GAITRAGRPSFILTRAGRPF
