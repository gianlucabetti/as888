000100180209       //==============================================================
000200180209       //? TNSS16R - Statistica Colli Incompatibili Arrivati.           ?
000300180209       //==============================================================
000400180209
000500180209       //--------------------------------------------------------------
000600180209       //? Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700180209       //--------------------------------------------------------------
000800180209
000900180209     /*PRM  dbgview(*source)
001000180209     /*CMD  ovrdbf file(FNARB00F) tofile(FILTRAPRD/FNARB00F) +
001100180209     /*CMD         ovrscope(*calllvl)
001200180209     /*END  dltovr file(FNARB00F) lvl(*)
001300180209     /*END
001400180209
001500180209       //--------------------------------------------------------------
001600180209       //? Specifiche di controllo.                                     ?
001700180209       //--------------------------------------------------------------
001800180209
001900180209     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002000180209     h dftactgrp(*no)
002100180209     h alwnull(*inputonly)
002200180209
002300180209       //--------------------------------------------------------------
002400180209       //? Dichiarazione file.                                          ?
002500180209       //--------------------------------------------------------------
002600180209
002700180209       // - ?Wrkf Statistica Colli Incompatibili Arrivati?
002800180209     fWFSCI00F  o    e             disk
002900180209
003000180209       //--------------------------------------------------------------
003100180209       //? Definizione costanti.                                        ?
003200180209       //--------------------------------------------------------------
003300180209
003400180209
003500180209       //--------------------------------------------------------------
003600180209       //? Definizione schiere.                                         ?
003700180209       //--------------------------------------------------------------
003800180209
003900180209
004000180209       //--------------------------------------------------------------
004100180209       //? Definizione aree dati.                                       ?
004200180209       //--------------------------------------------------------------
004300180209
004400180209
004500180209       //--------------------------------------------------------------
004600180209       //? Definizione strutture dati.                                  ?
004700180209       //--------------------------------------------------------------
004800180209
004900180209       // - ?Status ds?
005000180209     d Status         sds
005100180209     d   SDSpgm          *proc
005200180209     d*//SDSprm          *parms
005300180209     d*//SDSdta              191    198
005400180209     d   SDSjob              244    253
005500180209     d   SDSusr              254    263
005600180209
005700180209       // - ?Parametri Ricevuti?
005800180209     d KPJBA         e ds
005900180209     d TNSS16ds      e ds                  inz   qualified
006000180209
006100180209       // - ?Dati estratti via SQL?
006200180209     d WFSCI_ds        ds                  inz   qualified
006300180209     d   WCItfa                            inz   like( WCItfa )
006400180209     d   WCIlna                            inz   like( WCIlna )
006500180212     d   WCItca                            inz   like( WCItca )
006600180209     d   WCItci                            inz   like( WCItci )
006700180209     d   WCIiss                            inz   like( WCIiss )
006800180213
006900180212     d FNART_ds        ds                  inz   qualified
007000180212     d   WCItfa                            inz   like( WCItfa )
007100180212     d   WCIlna                            inz   like( WCIlna )
007200180212     d   WCItca                            inz   like( WCItca )
007300180209
007400180209       //--------------------------------------------------------------
007500180209       //? Definizione variabili globali.                               ?
007600180209       //--------------------------------------------------------------
007700180209
007800180209       // - ?Flags booleani?
007900180209     d $EoF            s               n   inz
008000180212     d $EoF2           s               n   inz
008100180209     d $Where          s               n   inz
008200180209
008300180209       // - ?Indici di schiera?
008400180209     d xx              s              3  0 inz
008500180209
008600180213       // - ?Nome archivio FNARB00F (nel Sistema Informativo di Fil.)?
008700180212     d wLibFile1       s             21    inz
008800180213       // - ?Nome archivio FNART00F (nel Sistema Informativo di Fil.)?
008900180212     d wLibFile2       s             21    inz
009000180209
009100180209       // - ?Stringa SQL da eseguire?
009200180209     d wSql            s          32740    inz   varying
009300180212     d wSql2           s          32740    inz   varying
009400180209
009500180209       //--------------------------------------------------------------
009600180209       //? Definizione prototipi procedure usate.                       ?
009700180209       //--------------------------------------------------------------
009800180209
009900180209       // - ?API QCAPCMD (Process Commands)?
010000180209     d Qcmd            s           2048    inz   varying
010100180209      /copy qSysInc/qRpgleSrc,QCAPCMD
010200180209      /copy gaitrasrc/srcProtoPR,QCAPCMD
010300180209       // - ?Parametri gestione errori API.?
010400180209      /copy qsysinc/qRpgleSrc,QUSEC
010500180209
010600180209       //--------------------------------------------------------------
010700180209       //? Definizione key-list.                                        ?
010800180209       //--------------------------------------------------------------
010900180209
011000180209
011100180209       //--------------------------------------------------------------
011200180209       //? Riepilogo indicatori utilizzati.                             ?
011300180209       //--------------------------------------------------------------
011400180209
011500180209
011600180209       //--------------------------------------------------------------
011700180209       //? M A I N - L I N E                                            ?
011800180209       //--------------------------------------------------------------
011900180209
012000180209     c     *Entry        plist
012100180209     c                   parm                    KPJBA
012200180209
012300180209      /free
012400180209
012500180209       // - ?Operazioni iniziali?
012600180209       exsr  sr_RoutInz;
012700180209
012800180209       // - ?Preparazione stringa SQL?
012900180209       exsr  sr_PrepSQL;
013000180212       exsr  sr_PrepSQL2;
013100180209
013200180209       // - ?Apertura del cursore?
013300180209       exsr  sr_OpenCursor;
013400180209
013500180209       // - ?Lettura del file?
013600180209       DoW  Not $EoF;
013700180209         exsr  sr_ReadCursor;
013800180209       EndDo;
013900180209
014000180209       // - ?Chiusura del cursore?
014100180209       exsr  sr_CloseCursor;
014200180209
014300180209       // - ?Operazioni finali?
014400180209       exsr  sr_RoutEnd;
014500180209
014600180209       //--------------------------------------------------------------
014700180209       //? Operazioni iniziali.                                         ?
014800180209       //--------------------------------------------------------------
014900180209       BEGSR  sr_RoutInz;
015000180209
015100180209         // - ?Impostazione opzioni per SQL?
015200180209         exec sql   set option   DynUsrPrf = *Owner,
015300180209                                 CloSqlCsr = *EndMod;
015400180209
015500180209         // - ?Impostazione chiusura?
015600180209         *inLR = *on;
015700180209
015800180209         // - ?Ricezione Parametri?
015900180209         if  KPJBU <> *blank;
016000180209           TNSS16ds = KPJBU;
016100180209           //*·TNSS16ds.oSS16err = *off;
016200180209           //*·clear  TNSS16ds.oSS16msg;
016300180209         endif;
016400180209
016500180209         // - ?Impostazione Libreria per File di Filiale?
016600180209         if  %subst( kNSIf : 7 : 1 ) = 'P';
016700180212           wLibFile1 = 'FILTRAPRD/FNARB00F';
016800180212           wLibFile2 = 'FILTRAPRD/FNART00F';
016900180209         else;
017000180212           wLibFile1 = 'FILTRA201/FNARB00F';
017100180212           wLibFile2 = 'FILTRA201/FNART00F';
017200180209         endif;
017300180209
017400180209         // - ?Cancellazione dei dati già estratti per lo stesso periodo?
017500180209         if  KPJBU <> *blank;
017600180209           exsr  sr_DeleteWFSCI;
017700180209         endif;
017800180209
017900180209       ENDSR;
018000180209
018100180209       //--------------------------------------------------------------
018200180209       //? Cancellazione dati già registrati su WFSCI00F per lo stesso  ?
018300180209       //? periodo che si sta elaborando.                               ?
018400180209       //--------------------------------------------------------------
018500180209       BEGSR  sr_DeleteWFSCI;
018600180209
018700180209         clear  $Where;
018800180213
018900180213         $EoF  = *off;
019000180209
019100180209         // - ?Preparazione SQL da eseguire?
019200180209         wSQL =  'delete from WFSCI00F';
019300180209
019400180212         //*·// - ?Parzializzazione per Data Spunta?
019500180212         //*·if  TNSS16ds.iSS16dti > *zero  or
019600180212         //*·    TNSS16ds.iSS16dtf > *zero;
019700180212         //*·
019800180212         //*·  if  Not $Where;
019900180212         //*·    $Where = *on;
020000180212         //*·    wSQL  +=  ' where';
020100180212         //*·  else;
020200180212         //*·    wSQL  +=  ' and';
020300180212         //*·  endif;
020400180212         //*·
020500180212         //*·  wSQL +=     ' WCIdti = ' + %char( TNSS16ds.iSS16dti ) +
020600180212         //*·          ' and WCIdtf = ' + %char( TNSS16ds.iSS16dtf );
020700180212         //*·
020800180212         //*·endif;
020900180212         //*·
021000180212         //*·// - ?Parzializzazione (eventuale) per Linea di Arrivo?
021100180212         //*·if  TNSS16ds.iSS16lna > *zero;
021200180212         //*·
021300180212         //*·  if  Not $Where;
021400180212         //*·    $Where = *on;
021500180212         //*·    wSQL  +=  ' where';
021600180212         //*·  else;
021700180212         //*·    wSQL  +=  ' and';
021800180212         //*·  endif;
021900180212         //*·
022000180212         //*·  wSQL +=     ' WCIlna = ' + %char( TNSS16ds.iSS16lna );
022100180212         //*·
022200180212         //*·endif;
022300180209
022400180209
022500180213         // -? Cancellazione vecchi record dal file?
022600180209         exec SQL   execute immediate :wSQL;
022700180209
022800180209         if  SQLcode < *zero;
022900180209           $EoF  = *on;
023000180209           exsr  sr_PrintERR;
023100180209           exsr  sr_RoutEnd;
023200180209         endif;
023300180209
023400180209       ENDSR;
023500180209
023600180209       //--------------------------------------------------------------
023700180209       //? Preparazione stringa SQL per estrazione dati da              ?
023800180209       //?   TITAH*0F + FNARB00F.                                       ?
023900180209       //--------------------------------------------------------------
024000180209       BEGSR  sr_PrepSQL;
024100180209
024200180209         wSQL =   'with TITAH_J as +
024300180209
024400180209                       ( select TAHaas, TAHlnp, TAHnrs, TAHnsp, +
024500180213                                substr( TAHnot, 1, 3 ) as §ARSJPEST, +
024600180209                                count(*) as wTAHnclJ +
024700180209                           from TITAH00F +
024800180209                          where TAHtrc = ''J'' +
024900180209                            and substr( TAHnot, 5, 8 ) between ''' +
025000180209                                %editc( TNSS16ds.ISS16dti : '3' ) +
025100180209                                ''' and ''' +
025200180209                                %editc( TNSS16ds.iSS16dtf : '3' ) +
025300180209                                ''' +
025400180213                       group by TAHaas, TAHlnp, TAHnrs, TAHnsp, +
025500180213                                substr( TAHnot, 1, 3 ) +
025600180213                       order by TAHaas, TAHlnp, TAHnrs, TAHnsp, +
025700180213                                substr( TAHnot, 1, 3 ) ), +
025800180209
025900180209                      TITAH_J_S as +
026000180209
026100180209                       ( select TAHaas, TAHlnp, TAHnrs, TAHnsp, +
026200180213                                substr( TAHnot, 1, 3 ) as §ARSJPEST, +
026300180209                                count(*) as wTAHnclJ_S +
026400180209                           from TITAH00F +
026500180209                          where TAHtrc = ''J'' +
026600180209                            and substr( TAHnot, 15, 1 ) = ''S'' +
026700180209                            and substr( TAHnot, 5, 8 ) between ''' +
026800180209                                %editc( TNSS16ds.ISS16dti : '3' ) +
026900180209                                ''' and ''' +
027000180209                                %editc( TNSS16ds.iSS16dtf : '3' ) +
027100180209                                ''' +
027200180213                       group by TAHaas, TAHlnp, TAHnrs, TAHnsp, +
027300180213                                substr( TAHnot, 1, 3 ) +
027400180213                       order by TAHaas, TAHlnp, TAHnrs, TAHnsp, +
027500180213                                substr( TAHnot, 1, 3 ) ) +
027600180209
027700180213                  select TITAH_J.§ARSJPEST, ARBlna, +
027800180213                         sum( ARBncl ), +
027900180209                         sum( TITAH_J.wTAHnclJ ), +
028000180212                         coalesce( sum( TITAH_J_S.wTAHnclJ_S ), 0 ) +
028100180212
028200180212                    from TITAH_J inner join ' + %trimR( wLibFile1 ) +
028300180209                    ' on TITAH_J.TAHaas = ARBaas and +
028400180209                         TITAH_J.TAHlnp = ARBlnp and +
028500180209                         TITAH_J.TAHnrs = ARBnrs and +
028600180209                         TITAH_J.TAHnsp = ARBnsp +
028700180212
028800180209                                 left outer join TITAH_J_S +
028900180209                      on TITAH_J.TAHaas = TITAH_J_S.TAHaas and +
029000180209                         TITAH_J.TAHlnp = TITAH_J_S.TAHlnp and +
029100180209                         TITAH_J.TAHnrs = TITAH_J_S.TAHnrs and +
029200180213                         TITAH_J.TAHnsp = TITAH_J_S.TAHnsp and +
029300180213                         TITAH_J.§ARSJPEST = TITAH_J_S.§ARSJPEST';
029400180209
029500180209         // - ?Eventuale parzializzazione per Filiale Arrivo?
029600180209         if  TNSS16ds.iSS16lna <> *zero;
029700180209           wSQL +=
029800180209                 ' where ARBlna = ' + %editc( TNSS16ds.iSS16lna : 'X' );
029900180209         endif;
030000180209
030100180209         // - ?Raggruppamento / Ordinamento?
030200180213         wSQL += ' group by TITAH_J.§ARSJPEST, ARBlna +
030300180209
030400180213                   order by TITAH_J.§ARSJPEST, ARBlna +
030500180209
030600180209                     for fetch only';
030700180209
030800180209       ENDSR;
030900180212
031000180212       //--------------------------------------------------------------
031100180212       //? Preparazione stringa SQL per estrazione dati da FNART00F.    ?
031200180212       //--------------------------------------------------------------
031300180212       BEGSR  sr_PrepSQL2;
031400180212
031500180212         wSQL2 =  'select ARBtfa, ARBlna, count(*) +
031600180212
031700180212                     from ' + %trimR( wLibFile2 ) + ' +
031800180212                          inner join ' + %trimR( wLibFile1 ) + ' +
031900180212
032000180212                       on ARTaas = ARBaas and +
032100180212                          ARTlnp = ARBlnp and +
032200180212                          ARTnrs = ARBnrs and +
032300180212                          ARTnsp = ARBnsp +
032400180212
032500180212                    where ARTdam between ' +
032600180212                                %editc( TNSS16ds.ISS16dti : '3' ) +
032700180212                                ' and ' +
032800180212                                %editc( TNSS16ds.iSS16dtf : '3' ) +
032900180212                                ' +
033000180212
033100180212                    group by ARBtfa, ARBlna +
033200180212
033300180212                    order by ARBtfa, ARBlna +
033400180212
033500180212                     for fetch only';
033600180212
033700180212       ENDSR;
033800180209
033900180209       //--------------------------------------------------------------
034000180212       //? Dichiarazione / Apertura cursori.                            ?
034100180209       //--------------------------------------------------------------
034200180209       BEGSR  sr_OpenCursor;
034300180209
034400180213         $EoF  = *off;
034500180213
034600180212         // - ?Dichiarazione 1° cursore?
034700180209         exec sql   prepare S1   from :wSQL;
034800180209         exec sql   declare C1   cursor for S1;
034900180209
035000180209         if  SQLcode < *zero;
035100180209           $EoF  = *on;
035200180209           exsr  sr_PrintERR;
035300180209         endif;
035400180212
035500180212         // - ?Dichiarazione 2° cursore?
035600180212         exec sql   prepare S2   from :wSQL2;
035700180212         exec sql   declare C2   cursor for S2;
035800180212
035900180212         if  SQLcode < *zero;
036000180212           $EoF  = *on;
036100180212           exsr  sr_PrintERR;
036200180212         endif;
036300180209
036400180212         // - ?Apertura del 1° cursore?
036500180209         exec sql   open C1;
036600180209
036700180209         if  SQLcode < *zero;
036800180209           $EoF  = *on;
036900180209           exsr  sr_PrintERR;
037000180209         endif;
037100180212
037200180212         // - ?Apertura del 2° cursore?
037300180212         exec sql   open C2;
037400180212
037500180212         if  SQLcode < *zero;
037600180212           $EoF  = *on;
037700180212           exsr  sr_PrintERR;
037800180212         endif;
037900180209
038000180209       ENDSR;
038100180209
038200180209       //--------------------------------------------------------------
038300180212       //? Lettura cursori.                                             ?
038400180209       //--------------------------------------------------------------
038500180209       BEGSR  sr_ReadCursor;
038600180209
038700180209         $EoF  = *off;
038800180209         clear  WFSCI_ds;
038900180209
039000180209         exec sql   fetch next   from C1   into :WFSCI_ds;
039100180209
039200180209         select;
039300180209           when  SQLcode = 100;
039400180209             $EoF  = *on;
039500180209           when  SQLcode < *zero;
039600180209             $EoF  = *on;
039700180209             exsr  sr_PrintERR;
039800180209           other;
039900180212             exsr  sr_ReadCursor2;
040000180209             exsr  sr_Write_WFSCI;
040100180209         endsl;
040200180209
040300180209       ENDSR;
040400180209
040500180209       //--------------------------------------------------------------
040600180209       //? Chiusura cursore.                                            ?
040700180209       //--------------------------------------------------------------
040800180209       BEGSR  sr_CloseCursor;
040900180209
041000180212         // - ?Chiusura del 1° cursore?
041100180209         exec sql   close C1;
041200180212
041300180212         // - ?Chiusura del 2° cursore?
041400180212         exec sql   close C2;
041500180209
041600180209       ENDSR;
041700180212
041800180212       //--------------------------------------------------------------
041900180212       //? Reperimento Totale Segnacolli da FNART00F.                  ?
042000180212       //--------------------------------------------------------------
042100180212       BEGSR  sr_ReadCursor2;
042200180212
042300180212         $EoF2 = *off;
042400180212
042500180212         DoW  FNART_ds.WCItfa < WFSCI_ds.WCItfa  or
042600180212             (FNART_ds.WCItfa = WFSCI_ds.WCItfa  and
042700180212              FNART_ds.WCIlna < WFSCI_ds.WCIlna);
042800180212
042900180212           clear  FNART_ds;
043000180212           exec sql   fetch next   from C2   into :FNART_ds;
043100180212
043200180212         EndDo;
043300180212
043400180212         select;
043500180212           when  SQLcode = 100;
043600180212             $EoF2 = *on;
043700180212           when  SQLcode < *zero;
043800180212             $EoF2 = *on;
043900180212             exsr  sr_PrintERR;
044000180212         endsl;
044100180212
044200180212       ENDSR;
044300180209
044400180209       //--------------------------------------------------------------
044500180209       //? Scrittura del record nel Work-File WFSCI00F.                ?
044600180209       //--------------------------------------------------------------
044700180209       BEGSR  sr_Write_WFSCI;
044800180209
044900180209         clear  WFSCI000;
045000180209
045100180209         wciDTI  = TNSS16ds.iSS16dti;
045200180209         wciDTF  = TNSS16ds.iSS16dtf;
045300180212         wciTFA  = WFSCI_ds.WCItfa;
045400180209         wciLNA  = WFSCI_ds.WCIlna;
045500180212         if  FNART_ds.WCItfa = WFSCI_ds.WCItfa  and
045600180212             FNART_ds.WCIlna = WFSCI_ds.WCIlna;
045700180212           wciTCA  = FNART_ds.WCItca;
045800180212         endif;
045900180209         wciTCI  = WFSCI_ds.WCItci;
046000180209         wciIFI  = WFSCI_ds.WCItci - WFSCI_ds.WCIiss;
046100180209         wciISS  = WFSCI_ds.WCIiss;
046200180209
046300180209         //______________
046400180209         write  WFSCI000;
046500180209         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
046600180209
046700180209       ENDSR;
046800180209
046900180209       //--------------------------------------------------------------
047000180209       //? Stampa segnalazione dell'errore rilevato via SQL             ?
047100180209       //--------------------------------------------------------------
047200180209       BEGSR  sr_PrintERR;
047300180209
047400180209         //*·// - ?Impostazione dati di output?
047500180209         //*·if  KPJBU <> *blank;
047600180209         //*·  TNSS16ds.oSS16err = *on;
047700180209         //*·  TNSS16ds.oSS16msg = 'Rilevato errore - controllare stampe.';
047800180209         //*·endif;
047900180209
048000180209         // - ?Stampa del Dump?
048100180209         Dump(A);
048200180209
048300180209         // - ?Stampa del Job-Log?
048400180209         Qcmd = 'DSPJOBLOG job(*) output(*print)';
048500180209         exsr  sr_ExecCmd;
048600180209
048700180209         // - ?Chiusura del programma?
048800180209         exsr  sr_RoutEnd;
048900180209
049000180209       ENDSR;
049100180209
049200180209       //--------------------------------------------------------------
049300180209       //?Esecuzione del comando (già impostato).                      ?
049400180209       //--------------------------------------------------------------
049500180209       BEGSR  sr_ExecCmd;
049600180209
049700180209         clear Qcap0100;
049800180209         Qcabcsdh = *off;
049900180209         Qcapa    = *off;
050000180209         Qcacmdss = *off;
050100180209         Qcaerved = *allX'00';
050200180209
050300180209         clear Qusec;
050400180209         Qusbprv  = %size(Qusec);
050500180209
050600180209         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
050700180209                           %size(Qcap0100) : 'CPOP0100' : *omit :
050800180209                           0 : 0 : Qusec);
050900180209
051000180209         //*·if  Qusei <> *blank;
051100180209         //*·  ...;
051200180209         //*·endif;
051300180209
051400180209       ENDSR;
051500180209
051600180209       //--------------------------------------------------------------
051700180209       //?Operazioni finali.                                           ?
051800180209       //--------------------------------------------------------------
051900180209       BEGSR  sr_RoutEnd;
052000180209
052100180209         // -?Restituzione parametri al chiamante?
052200180209         if  KPJBU <> *blank;
052300180209           KPJBU = TNSS16ds;
052400180209         endif;
052500180209
052600180209         // -?Uscita dal *pgm?
052700180209         return;
052800180209
052900180209       ENDSR;
053000180209
053100180209      /end-free
