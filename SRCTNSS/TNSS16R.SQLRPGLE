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
003800180219
003900180219       // - ?Tab. "ETF": dettaglio "eccezioni" per singola LNA?
004000180219       //   ?(max 5 Terminal d'eccezione per la singola LNA)?
004100180219     d ds_ETF_flp      ds            15    inz
004200180219     d   sk_ETF_flp                   3    dim(05)
004300180209
004400180215       // - ?Nr. Max di elementi (LNA) per le schiere dalla tab. "ETF"?
004500180215       //   ?(vedi LNA della Sardegna)?
004600180215     d c_Max_ETF       c                   const(50)
004700180215       // - ?Tab. "ETF": LNA con "eccezioni"?
004800180215     d sk_ETF_ke1      s              3    dim(c_Max_ETF)
004900180219     d                                     inz
005000180215       // - ?Tab. "ETF": TFA "eccezioni" per LNA?
005100180215     d sk_ETF_uni      s                   like(ds_ETF_flp)
005200180215     d                                     dim(c_Max_ETF)
005300180219     d                                     inz
005400180219     d sk_ETF_memo     s                   like(ds_ETF_flp)
005500180219     d                                     dim(c_Max_ETF)
005600180219     d                                     inz
005700180209
005800180209       //--------------------------------------------------------------
005900180209       //? Definizione aree dati.                                       ?
006000180209       //--------------------------------------------------------------
006100180209
006200180209
006300180209       //--------------------------------------------------------------
006400180209       //? Definizione strutture dati.                                  ?
006500180209       //--------------------------------------------------------------
006600180209
006700180209       // - ?Status ds?
006800180209     d Status         sds
006900180209     d   SDSpgm          *proc
007000180209     d*//SDSprm          *parms
007100180209     d*//SDSdta              191    198
007200180209     d   SDSjob              244    253
007300180209     d   SDSusr              254    263
007400180209
007500180209       // - ?Parametri Ricevuti?
007600180209     d KPJBA         e ds
007700180209     d TNSS16ds      e ds                  inz   qualified
007800180209
007900180209       // - ?Dati estratti via SQL?
008000180209     d WFSCI_ds        ds                  inz   qualified
008100180209     d   WCItfa                            inz   like( WCItfa )
008200180209     d   WCIlna                            inz   like( WCIlna )
008300180212     d   WCItca                            inz   like( WCItca )
008400180209     d   WCItci                            inz   like( WCItci )
008500180209     d   WCIiss                            inz   like( WCIiss )
008600180213
008700180212     d FNART_ds        ds                  inz   qualified
008800180212     d   WCItfa                            inz   like( WCItfa )
008900180212     d   WCIlna                            inz   like( WCIlna )
009000180212     d   WCItca                            inz   like( WCItca )
009100180214
009200180219     d Eccezione_T_ds  ds                  inz   qualified
009300180216     d   WCItfa                            inz   like( WCItfa )
009400180216     d   WCIlna                            inz   like( WCIlna )
009500180216     d   WCItca                            inz   like( WCItca )
009600180215
009700180215     d TNTBE00F      e ds                  based(nullPtr)
009800180215     d                                     qualified
009900180215     d wKE1            s                   inz   like(TNTBE00F.TBEke1)
010000180215     d wUNI            s                   inz   like(TNTBE00F.TBEuni)
010100180209
010200180209       //--------------------------------------------------------------
010300180209       //? Definizione variabili globali.                               ?
010400180209       //--------------------------------------------------------------
010500180209
010600180209       // - ?Flags booleani?
010700180209     d $EoF            s               n   inz
010800180212     d $EoF2           s               n   inz
010900180209     d $Where          s               n   inz
011000180209
011100180209       // - ?Indici di schiera?
011200180209     d xx              s              3  0 inz
011300180215     d yy              s              3  0 inz
011400180209
011500180213       // - ?Nome archivio FNARB00F (nel Sistema Informativo di Fil.)?
011600180212     d wLibFile1       s             21    inz
011700180213       // - ?Nome archivio FNART00F (nel Sistema Informativo di Fil.)?
011800180212     d wLibFile2       s             21    inz
011900180209
012000180215       // - ?Stringhe SQL da eseguire?
012100180209     d wSql            s          32740    inz   varying
012200180212     d wSql2           s          32740    inz   varying
012300180214     d wSql3           s          32740    inz   varying
012400180219     d wSql4           s          32740    inz   varying
012500180219
012600180219       // - ?Comodo?
012700180219     d w_ETF_uni       s             20    inz
012800180209
012900180209       //--------------------------------------------------------------
013000180209       //? Definizione prototipi procedure usate.                       ?
013100180209       //--------------------------------------------------------------
013200180209
013300180209       // - ?API QCAPCMD (Process Commands)?
013400180209     d Qcmd            s           2048    inz   varying
013500180209      /copy qSysInc/qRpgleSrc,QCAPCMD
013600180209      /copy gaitrasrc/srcProtoPR,QCAPCMD
013700180209       // - ?Parametri gestione errori API.?
013800180209      /copy qsysinc/qRpgleSrc,QUSEC
013900180209
014000180209       //--------------------------------------------------------------
014100180209       //? Definizione key-list.                                        ?
014200180209       //--------------------------------------------------------------
014300180209
014400180209
014500180209       //--------------------------------------------------------------
014600180209       //? Riepilogo indicatori utilizzati.                             ?
014700180209       //--------------------------------------------------------------
014800180209
014900180209
015000180209       //--------------------------------------------------------------
015100180209       //? M A I N - L I N E                                            ?
015200180209       //--------------------------------------------------------------
015300180209
015400180209     c     *Entry        plist
015500180209     c                   parm                    KPJBA
015600180209
015700180209      /free
015800180209
015900180209       // - ?Operazioni iniziali?
016000180209       exsr  sr_RoutInz;
016100180209
016200180209       // - ?Preparazione stringa SQL?
016300180209       exsr  sr_PrepSQL;
016400180212       exsr  sr_PrepSQL2;
016500180209
016600180209       // - ?Apertura del cursore?
016700180209       exsr  sr_OpenCursor;
016800180209
016900180209       // - ?Lettura del file?
017000180209       DoW  Not $EoF;
017100180209         exsr  sr_ReadCursor;
017200180209       EndDo;
017300180209
017400180209       // - ?Chiusura del cursore?
017500180209       exsr  sr_CloseCursor;
017600180219
017700180209       // - ?Operazioni finali?
017800180209       exsr  sr_RoutEnd;
017900180209
018000180209       //--------------------------------------------------------------
018100180209       //? Operazioni iniziali.                                         ?
018200180209       //--------------------------------------------------------------
018300180209       BEGSR  sr_RoutInz;
018400180209
018500180209         // - ?Impostazione opzioni per SQL?
018600180209         exec sql   set option   DynUsrPrf = *Owner,
018700180209                                 CloSqlCsr = *EndMod;
018800180209
018900180209         // - ?Impostazione chiusura?
019000180209         *inLR = *on;
019100180209
019200180209         // - ?Ricezione Parametri?
019300180209         if  KPJBU <> *blank;
019400180209           TNSS16ds = KPJBU;
019500180209           //*·TNSS16ds.oSS16err = *off;
019600180209           //*·clear  TNSS16ds.oSS16msg;
019700180209         endif;
019800180213
019900180215         // - ?ECCEZIONI: Caricamento filiali dalla tab. "ETF"?
020000180215         exsr  sr_Tab_ETF;
020100180209
020200180209         // - ?Impostazione Libreria per File di Filiale?
020300180209         if  %subst( kNSIf : 7 : 1 ) = 'P';
020400180212           wLibFile1 = 'FILTRAPRD/FNARB00F';
020500180212           wLibFile2 = 'FILTRAPRD/FNART00F';
020600180209         else;
020700180212           wLibFile1 = 'FILTRA201/FNARB00F';
020800180212           wLibFile2 = 'FILTRA201/FNART00F';
020900180209         endif;
021000180209
021100180209         // - ?Cancellazione dei dati già estratti per lo stesso periodo?
021200180209         if  KPJBU <> *blank;
021300180209           exsr  sr_DeleteWFSCI;
021400180209         endif;
021500180209
021600180209       ENDSR;
021700180213
021800180213       //--------------------------------------------------------------
021900180215       //? Caricamento filiali "eccezione" dalla tab. "ETF".            ?
022000180213       //--------------------------------------------------------------
022100180215       BEGSR  sr_Tab_ETF;
022200180213
022300180215         wSQL = 'select TBEke1, TBEuni +
022400180215                   from TNTBE00F +
022500180215                  where TBEcod = ''ETF'' +
022600180215                    and TBEke2 = '' '' +
022700180215                    and TBEatb = '' '' +
022800180215                  order by TBEcod, TBEke1 +
022900180215                    for fetch only';
023000180213
023100180213         $EoF  = *off;
023200180213
023300180213         exec sql   prepare S0   from :wSQL;
023400180213         exec sql   declare C0   cursor for S0;
023500180213
023600180213         if  SQLcode < *zero;
023700180213           $EoF  = *on;
023800180213           exsr  sr_PrintERR;
023900180213         endif;
024000180213
024100180213         exec sql   open C0;
024200180213
024300180213         if  SQLcode < *zero;
024400180213           $EoF  = *on;
024500180213           exsr  sr_PrintERR;
024600180213         endif;
024700180213
024800180213         $EoF  = *off;
024900180213         clear  xx;
025000180215         clear  sk_ETF_ke1;
025100180215         clear  sk_ETF_uni;
025200180219         clear  sk_ETF_memo;
025300180215         clear  wKE1;
025400180215         clear  wUNI;
025500180213
025600180215         exec sql   fetch next   from C0   into :wKE1, :wUNI;
025700180213
025800180215         DoU  $EoF;
025900180215
026000180215           select;
026100180215
026200180215             when  SQLcode = 100;
026300180215               $EoF  = *on;
026400180215
026500180215             when  SQLcode < *zero;
026600180215               $EoF  = *on;
026700180215               exsr  sr_PrintERR;
026800180215
026900180215             other;
027000180215               xx += 1;
027100180219               sk_ETF_ke1(xx)  = wKE1;
027200180219               sk_ETF_uni(xx)  = wUNI;
027300180219               sk_ETF_memo(xx) = wUNI;
027400180215
027500180215               exec sql   fetch next   from C0   into :wKE1, :wUNI;
027600180215
027700180215           endsl;
027800180213
027900180213         EndDo;
028000180213
028100180213         exec sql   close C0;
028200180213
028300180213       ENDSR;
028400180209
028500180209       //--------------------------------------------------------------
028600180209       //? Cancellazione dati già registrati su WFSCI00F per lo stesso  ?
028700180209       //? periodo che si sta elaborando.                               ?
028800180209       //--------------------------------------------------------------
028900180209       BEGSR  sr_DeleteWFSCI;
029000180209
029100180209         clear  $Where;
029200180213
029300180213         $EoF  = *off;
029400180209
029500180209         // - ?Preparazione SQL da eseguire?
029600180209         wSQL =  'delete from WFSCI00F';
029700180209
029800180212         //*·// - ?Parzializzazione per Data Spunta?
029900180212         //*·if  TNSS16ds.iSS16dti > *zero  or
030000180212         //*·    TNSS16ds.iSS16dtf > *zero;
030100180212         //*·
030200180212         //*·  if  Not $Where;
030300180212         //*·    $Where = *on;
030400180212         //*·    wSQL  +=  ' where';
030500180212         //*·  else;
030600180212         //*·    wSQL  +=  ' and';
030700180212         //*·  endif;
030800180212         //*·
030900180212         //*·  wSQL +=     ' WCIdti = ' + %char( TNSS16ds.iSS16dti ) +
031000180212         //*·          ' and WCIdtf = ' + %char( TNSS16ds.iSS16dtf );
031100180212         //*·
031200180212         //*·endif;
031300180212         //*·
031400180212         //*·// - ?Parzializzazione (eventuale) per Linea di Arrivo?
031500180212         //*·if  TNSS16ds.iSS16lna > *zero;
031600180212         //*·
031700180212         //*·  if  Not $Where;
031800180212         //*·    $Where = *on;
031900180212         //*·    wSQL  +=  ' where';
032000180212         //*·  else;
032100180212         //*·    wSQL  +=  ' and';
032200180212         //*·  endif;
032300180212         //*·
032400180212         //*·  wSQL +=     ' WCIlna = ' + %char( TNSS16ds.iSS16lna );
032500180212         //*·
032600180212         //*·endif;
032700180209
032800180209
032900180213         // -? Cancellazione vecchi record dal file?
033000180209         exec SQL   execute immediate :wSQL;
033100180209
033200180209         if  SQLcode < *zero;
033300180209           $EoF  = *on;
033400180209           exsr  sr_PrintERR;
033500180209           exsr  sr_RoutEnd;
033600180209         endif;
033700180209
033800180209       ENDSR;
033900180209
034000180209       //--------------------------------------------------------------
034100180209       //? Preparazione stringa SQL per estrazione dati da              ?
034200180209       //?   TITAH*0F + FNARB00F.                                       ?
034300180209       //--------------------------------------------------------------
034400180209       BEGSR  sr_PrepSQL;
034500180209
034600180209         wSQL =   'with TITAH_J as +
034700180209
034800180209                       ( select TAHaas, TAHlnp, TAHnrs, TAHnsp, +
034900180213                                substr( TAHnot, 1, 3 ) as §ARSJPEST, +
035000180209                                count(*) as wTAHnclJ +
035100180209                           from TITAH00F +
035200180209                          where TAHtrc = ''J'' +
035300180209                            and substr( TAHnot, 5, 8 ) between ''' +
035400180209                                %editc( TNSS16ds.ISS16dti : '3' ) +
035500180209                                ''' and ''' +
035600180209                                %editc( TNSS16ds.iSS16dtf : '3' ) +
035700180209                                ''' +
035800180213                       group by TAHaas, TAHlnp, TAHnrs, TAHnsp, +
035900180213                                substr( TAHnot, 1, 3 ) +
036000180213                       order by TAHaas, TAHlnp, TAHnrs, TAHnsp, +
036100180213                                substr( TAHnot, 1, 3 ) ), +
036200180209
036300180209                      TITAH_J_S as +
036400180209
036500180209                       ( select TAHaas, TAHlnp, TAHnrs, TAHnsp, +
036600180213                                substr( TAHnot, 1, 3 ) as §ARSJPEST, +
036700180209                                count(*) as wTAHnclJ_S +
036800180209                           from TITAH00F +
036900180209                          where TAHtrc = ''J'' +
037000180209                            and substr( TAHnot, 15, 1 ) = ''S'' +
037100180209                            and substr( TAHnot, 5, 8 ) between ''' +
037200180209                                %editc( TNSS16ds.ISS16dti : '3' ) +
037300180209                                ''' and ''' +
037400180209                                %editc( TNSS16ds.iSS16dtf : '3' ) +
037500180209                                ''' +
037600180213                       group by TAHaas, TAHlnp, TAHnrs, TAHnsp, +
037700180213                                substr( TAHnot, 1, 3 ) +
037800180213                       order by TAHaas, TAHlnp, TAHnrs, TAHnsp, +
037900180213                                substr( TAHnot, 1, 3 ) ) +
038000180209
038100180213                  select TITAH_J.§ARSJPEST, ARBlna, +
038200180213                         sum( ARBncl ), +
038300180209                         sum( TITAH_J.wTAHnclJ ), +
038400180212                         coalesce( sum( TITAH_J_S.wTAHnclJ_S ), 0 ) +
038500180212
038600180212                    from TITAH_J inner join ' + %trimR( wLibFile1 ) +
038700180209                    ' on TITAH_J.TAHaas = ARBaas and +
038800180209                         TITAH_J.TAHlnp = ARBlnp and +
038900180209                         TITAH_J.TAHnrs = ARBnrs and +
039000180209                         TITAH_J.TAHnsp = ARBnsp +
039100180212
039200180209                                 left outer join TITAH_J_S +
039300180209                      on TITAH_J.TAHaas = TITAH_J_S.TAHaas and +
039400180209                         TITAH_J.TAHlnp = TITAH_J_S.TAHlnp and +
039500180209                         TITAH_J.TAHnrs = TITAH_J_S.TAHnrs and +
039600180213                         TITAH_J.TAHnsp = TITAH_J_S.TAHnsp and +
039700180213                         TITAH_J.§ARSJPEST = TITAH_J_S.§ARSJPEST';
039800180209
039900180209         // - ?Eventuale parzializzazione per Filiale Arrivo?
040000180209         if  TNSS16ds.iSS16lna <> *zero;
040100180209           wSQL +=
040200180209                 ' where ARBlna = ' + %editc( TNSS16ds.iSS16lna : 'X' );
040300180209         endif;
040400180209
040500180209         // - ?Raggruppamento / Ordinamento?
040600180213         wSQL += ' group by TITAH_J.§ARSJPEST, ARBlna +
040700180209
040800180213                   order by TITAH_J.§ARSJPEST, ARBlna +
040900180209
041000180209                     for fetch only';
041100180209
041200180209       ENDSR;
041300180212
041400180212       //--------------------------------------------------------------
041500180212       //? Preparazione stringa SQL per estrazione dati da FNART00F.    ?
041600180212       //--------------------------------------------------------------
041700180212       BEGSR  sr_PrepSQL2;
041800180212
041900180212         wSQL2 =  'select ARBtfa, ARBlna, count(*) +
042000180212
042100180212                     from ' + %trimR( wLibFile2 ) + ' +
042200180212                          inner join ' + %trimR( wLibFile1 ) + ' +
042300180212
042400180212                       on ARTaas = ARBaas and +
042500180212                          ARTlnp = ARBlnp and +
042600180212                          ARTnrs = ARBnrs and +
042700180212                          ARTnsp = ARBnsp +
042800180212
042900180212                    where ARTdam between ' +
043000180212                                %editc( TNSS16ds.ISS16dti : '3' ) +
043100180212                                ' and ' +
043200180212                                %editc( TNSS16ds.iSS16dtf : '3' ) +
043300180212                                ' +
043400180212
043500180212                    group by ARBtfa, ARBlna +
043600180212
043700180212                    order by ARBtfa, ARBlna +
043800180212
043900180212                     for fetch only';
044000180212
044100180212       ENDSR;
044200180209
044300180209       //--------------------------------------------------------------
044400180212       //? Dichiarazione / Apertura cursori.                            ?
044500180209       //--------------------------------------------------------------
044600180209       BEGSR  sr_OpenCursor;
044700180209
044800180213         $EoF  = *off;
044900180213
045000180212         // - ?Dichiarazione 1° cursore?
045100180209         exec sql   prepare S1   from :wSQL;
045200180209         exec sql   declare C1   cursor for S1;
045300180209
045400180209         if  SQLcode < *zero;
045500180209           $EoF  = *on;
045600180209           exsr  sr_PrintERR;
045700180209         endif;
045800180212
045900180212         // - ?Dichiarazione 2° cursore?
046000180212         exec sql   prepare S2   from :wSQL2;
046100180212         exec sql   declare C2   cursor for S2;
046200180212
046300180212         if  SQLcode < *zero;
046400180212           $EoF  = *on;
046500180212           exsr  sr_PrintERR;
046600180212         endif;
046700180209
046800180212         // - ?Apertura del 1° cursore?
046900180209         exec sql   open C1;
047000180209
047100180209         if  SQLcode < *zero;
047200180209           $EoF  = *on;
047300180209           exsr  sr_PrintERR;
047400180209         endif;
047500180212
047600180212         // - ?Apertura del 2° cursore?
047700180212         exec sql   open C2;
047800180212
047900180212         if  SQLcode < *zero;
048000180212           $EoF  = *on;
048100180212           exsr  sr_PrintERR;
048200180212         endif;
048300180209
048400180209       ENDSR;
048500180209
048600180209       //--------------------------------------------------------------
048700180212       //? Lettura cursori.                                             ?
048800180209       //--------------------------------------------------------------
048900180209       BEGSR  sr_ReadCursor;
049000180209
049100180209         $EoF  = *off;
049200180209         clear  WFSCI_ds;
049300180209
049400180209         exec sql   fetch next   from C1   into :WFSCI_ds;
049500180209
049600180209         select;
049700180209           when  SQLcode = 100;
049800180209             $EoF  = *on;
049900180209           when  SQLcode < *zero;
050000180209             $EoF  = *on;
050100180209             exsr  sr_PrintERR;
050200180209           other;
050300180215             xx = %lookup( %editc( WFSCI_ds.WCIlna : 'X' ) :
050400180215                           sk_ETF_ke1 );
050500180219             // - ?LNA non in KE1 della tab. "ETF"?
050600180215             if  xx = *zero;
050700180214               exsr  sr_ReadCursor2;
050800180214               exsr  sr_Write_WFSCI;
050900180219             // - ?LNA in KE1 della tab. "ETF"?
051000180214             else;
051100180219               ds_ETF_flp = sk_ETF_uni(xx);
051200180219               yy = %lookup( %editc( WFSCI_ds.WCItfa : 'X' ) :
051300180219                             sk_ETF_flp );
051400180219               select;
051500180219                 // - ?FLP in UNI della tab. "ETF"?
051600180219                 when  yy > *zero;
051700180219                   exsr  sr_Eccezione;
051800180219                 // - ?TFA = LNA?
051900180219                 when  WFSCI_ds.WCItfa = WFSCI_ds.WCIlna;
052000180219                   exsr  sr_Ges_Cursor4;
052100180219               endsl;
052200180214             endif;
052300180209         endsl;
052400180209
052500180209       ENDSR;
052600180209
052700180209       //--------------------------------------------------------------
052800180209       //? Chiusura cursore.                                            ?
052900180209       //--------------------------------------------------------------
053000180209       BEGSR  sr_CloseCursor;
053100180209
053200180212         // - ?Chiusura del 1° cursore?
053300180209         exec sql   close C1;
053400180212
053500180212         // - ?Chiusura del 2° cursore?
053600180212         exec sql   close C2;
053700180209
053800180209       ENDSR;
053900180212
054000180212       //--------------------------------------------------------------
054100180212       //? Reperimento Totale Segnacolli da FNART00F.                  ?
054200180212       //--------------------------------------------------------------
054300180212       BEGSR  sr_ReadCursor2;
054400180212
054500180212         $EoF2 = *off;
054600180212
054700180212         DoW  FNART_ds.WCItfa < WFSCI_ds.WCItfa  or
054800180212             (FNART_ds.WCItfa = WFSCI_ds.WCItfa  and
054900180212              FNART_ds.WCIlna < WFSCI_ds.WCIlna);
055000180212
055100180212           clear  FNART_ds;
055200180212           exec sql   fetch next   from C2   into :FNART_ds;
055300180212
055400180212         EndDo;
055500180212
055600180212         select;
055700180212           when  SQLcode = 100;
055800180212             $EoF2 = *on;
055900180212           when  SQLcode < *zero;
056000180212             $EoF2 = *on;
056100180212             exsr  sr_PrintERR;
056200180212         endsl;
056300180212
056400180212       ENDSR;
056500180214
056600180214       //--------------------------------------------------------------
056700180219       //? Reperimento Totale Segnacolli da FNART00F per TFA/LNA in     ?
056800180215       //? elaborazione (vedi schiere caricate dalla tab. "ETF").       ?
056900180214       //--------------------------------------------------------------
057000180219       BEGSR  sr_Eccezione;
057100180216
057200180219         // - ?Estrazione dati per il "vero" TFA in elaborazione?
057300180219         //   ?(dal cui totale andranno sottratte le spedizioni con?
057400180219         //   ?i colli incompatibili con LNA in KE1 della tab. "ETF"?
057500180219         //   ?e ARTFLP nella UNI corrispondente)?
057600180216         exsr  sr_ReadCursor2;
057700180215
057800180219         // - ?Estrazione dati per ognuna delle FLP reperite?
057900180215         $EoF2 = *off;
058000180215
058100180215         // - ?Preparazione stringa SQL per 3° cursore?
058200180215         exsr  sr_PrepSQL3;
058300180215
058400180215         // - ?Apertura del 3° cursore?
058500180215         exsr  sr_OpenCursor3;
058600180215
058700180215         // - ?Ciclo di lettura del file con il 3° cursore?
058800180215         DoW  Not $EoF2;
058900180215           exsr  sr_ReadCursor3;
059000180215         EndDo;
059100180215
059200180215         // - ?Chiusura del 3° cursore?
059300180215         exsr  sr_CloseCursor3;
059400180215
059500180215       ENDSR;
059600180214
059700180215       //--------------------------------------------------------------
059800180219       //? Preparazione stringa SQL per Totali di Eccezioni.            ?
059900180215       //--------------------------------------------------------------
060000180215       BEGSR  sr_PrepSQL3;
060100180214
060200180219         wSQL3 =  'select ARTflp, ARTlna, count(*) +
060300180219
060400180219                     from ' + %trimR( wLibFile2 ) + ' +
060500180219
060600180219                    where ARTlna = ' +
060700180219                          %editc( WFSCI_ds.WCIlna : 'X' ) + ' +
060800180219                      and ARTflp = ' +
060900180219                          %editc( WFSCI_ds.WCItfa : 'X' ) + ' +
061000180219                      and ARTdet between ' +
061100180219                          %editc( TNSS16ds.ISS16dti : '3' ) +
061200180219                                ' and ' +
061300180219                          %editc( TNSS16ds.iSS16dtf : '3' ) +
061400180219                                ' +
061500180219
061600180219                    group by ARTflp, ARTlna +
061700180219
061800180219                    order by ARTflp, ARTlna +
061900180219
062000180219                     for fetch only';
062100180216
062200180216       ENDSR;
062300180214
062400180215       //--------------------------------------------------------------
062500180215       //? Dichiarazione / Apertura Cursore.                            ?
062600180215       //--------------------------------------------------------------
062700180215       BEGSR  sr_OpenCursor3;
062800180215
062900180214         // - ?Dichiarazione 3° cursore?
063000180214         exec sql   prepare S3  from :wSQL3;
063100180214         exec sql   declare C3   cursor for S3;
063200180214
063300180214         if  SQLcode < *zero;
063400180214           $EoF2 = *on;
063500180214           exsr  sr_PrintERR;
063600180214         endif;
063700180214
063800180214         // - ?Apertura del 3° cursore?
063900180214         exec sql   open C3;
064000180214
064100180214         if  SQLcode < *zero;
064200180214           $EoF2 = *on;
064300180214           exsr  sr_PrintERR;
064400180214         endif;
064500180215
064600180215       ENDSR;
064700180215
064800180215       //--------------------------------------------------------------
064900180215       //? Lettura cursore.                                             ?
065000180215       //--------------------------------------------------------------
065100180215       BEGSR  sr_ReadCursor3;
065200180214
065300180214         // - ?Ciclo di lettura con il 3° cursore?
065400180219         clear  Eccezione_T_ds;
065500180219         exec sql   fetch next   from C3   into :Eccezione_T_ds;
065600180214
065700180214         select;
065800180214           when  SQLcode = 100;
065900180214             $EoF2 = *on;
066000180214           when  SQLcode < *zero;
066100180214             $EoF2 = *on;
066200180214             exsr  sr_PrintERR;
066300180219           other;
066400180219             exsr  sr_Upd_ETF_flp;
066500180219             exsr  sr_Write_WFSCI_Eccez;
066600180214         endsl;
066700180215
066800180215       ENDSR;
066900180215
067000180215       //--------------------------------------------------------------
067100180215       //? Chiusura cursore.                                            ?
067200180215       //--------------------------------------------------------------
067300180215       BEGSR  sr_CloseCursor3;
067400180214
067500180214         // - ?Chiusura del 3° cursore?
067600180214         exec sql   close C3;
067700180214
067800180214       ENDSR;
067900180219
068000180219       //--------------------------------------------------------------
068100180219       //? Estrazione dati per le LNA "eccezioni".                      ?
068200180219       //--------------------------------------------------------------
068300180219       BEGSR  sr_Ges_Cursor4;
068400180219
068500180219         ds_ETF_flp = sk_ETF_memo(xx);
068600180219         clear  w_ETF_uni;
068700180219
068800180219         For  yy = 1  To  %elem( sk_ETF_flp );
068900180219
069000180219           if  sk_ETF_flp(yy) > *zero;
069100180219
069200180219             if  w_ETF_uni = *blank;
069300180219               w_ETF_uni = sk_ETF_flp(yy);
069400180219             else;
069500180219               w_ETF_uni = %trimR( w_ETF_uni ) + ', ' + sk_ETF_flp(yy);
069600180219             endif;
069700180219
069800180219           endif;
069900180219
070000180219         EndFor;
070100180219
070200180219         wSQL4 =  'select count(*) +
070300180219
070400180219                     from ' + %trimR( wLibFile2 ) + ' +
070500180219
070600180219                    where ARTlna = ' +
070700180219                          %editc( WFSCI_ds.WCIlna : 'X' ) + ' +
070800180219                      and ARTflp not in (' +
070900180219                          %trimR( w_ETF_uni ) + ') +
071000180219                      and ARTdam between ' +
071100180219                          %editc( TNSS16ds.ISS16dti : '3' ) +
071200180219                                ' and ' +
071300180219                          %editc( TNSS16ds.iSS16dtf : '3' ) +
071400180219                                ' +
071500180219
071600180219                     for fetch only';
071700180219
071800180219         // - ?Dichiarazione 4° cursore?
071900180219         exec sql   prepare S4  from :wSQL4;
072000180219         exec sql   declare C4   cursor for S4;
072100180219
072200180219         if  SQLcode < *zero;
072300180219           $EoF2 = *on;
072400180219           exsr  sr_PrintERR;
072500180219         endif;
072600180219
072700180219         // - ?Apertura del 4° cursore?
072800180219         exec sql   open C4;
072900180219
073000180219         if  SQLcode < *zero;
073100180219           $EoF2 = *on;
073200180219           exsr  sr_PrintERR;
073300180219         endif;
073400180219
073500180219         // - ?Lettura con il 4° cursore?
073600180219         clear  FNART_ds;
073700180219         FNART_ds.wciTFA = WFSCI_ds.wciTFA;
073800180219         FNART_ds.wciLNA = WFSCI_ds.wciLNA;
073900180219         exec sql   fetch next   from C4   into :FNART_ds.wciTCA;
074000180219
074100180219         select;
074200180219           when  SQLcode = 100;
074300180219             $EoF2 = *on;
074400180219           when  SQLcode < *zero;
074500180219             $EoF2 = *on;
074600180219             exsr  sr_PrintERR;
074700180219           other;
074800180219             exsr  sr_Write_WFSCI;
074900180219         endsl;
075000180219
075100180219         // - ?Chiusura del 4° cursore?
075200180219         exec sql   close C4;
075300180219
075400180219       ENDSR;
075500180216
075600180216       //--------------------------------------------------------------
075700180219       //? Aggiornamento elenco filiali ancora da elaborare x LNA.      ?
075800180216       //--------------------------------------------------------------
075900180219       BEGSR  sr_Upd_ETF_flp;
076000180216
076100180219         // - ?Aggiornamento totale spedizioni già conteggiate?
076200180219         xx = %lookup( %editc( WFSCI_ds.WCIlna : 'X' ) :
076300180219                       sk_ETF_ke1 );
076400180219
076500180219         clear  ds_ETF_flp;
076600180219         clear  yy;
076700180219
076800180219         if  xx > *zero;
076900180219           ds_ETF_flp = sk_ETF_uni(xx);
077000180219           yy = %lookup( %editc( WFSCI_ds.WCItfa : 'X' ) :
077100180219                         sk_ETF_flp );
077200180219         endif;
077300180219
077400180219         if  yy > *zero;
077500180219           clear  sk_ETF_flp(yy);
077600180219           sk_ETF_uni(xx) = ds_ETF_flp;
077700180219         endif;
077800180216
077900180216       ENDSR;
078000180209
078100180209       //--------------------------------------------------------------
078200180209       //? Scrittura del record nel Work-File WFSCI00F.                ?
078300180209       //--------------------------------------------------------------
078400180209       BEGSR  sr_Write_WFSCI;
078500180209
078600180209         clear  WFSCI000;
078700180209
078800180209         wciDTI  = TNSS16ds.iSS16dti;
078900180209         wciDTF  = TNSS16ds.iSS16dtf;
079000180216         wciTFA  = WFSCI_ds.wciTFA;
079100180216         wciLNA  = WFSCI_ds.wciLNA;
079200180216         if  FNART_ds.wciTFA = WFSCI_ds.wciTFA  and
079300180216             FNART_ds.wciLNA = WFSCI_ds.wciLNA;
079400180216           wciTCA  = FNART_ds.wciTCA;
079500180212         endif;
079600180216         wciTCI  = WFSCI_ds.wciTCI;
079700180216         wciIFI  = WFSCI_ds.wciTCI - WFSCI_ds.wciISS;
079800180216         wciISS  = WFSCI_ds.wciISS;
079900180209
080000180209         //______________
080100180209         write  WFSCI000;
080200180209         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
080300180209
080400180209       ENDSR;
080500180214
080600180214       //--------------------------------------------------------------
080700180215       //? Scrittura del record nel Work-File WFSCI00F x singola ECCEZ. ?
080800180214       //--------------------------------------------------------------
080900180214
081000180219       BEGSR  sr_Write_WFSCI_Eccez;
081100180214
081200180214         clear  WFSCI000;
081300180214
081400180214         wciDTI  = TNSS16ds.iSS16dti;
081500180214         wciDTF  = TNSS16ds.iSS16dtf;
081600180219         wciTFA  = WFSCI_ds.wciTFA;
081700180219         wciLNA  = WFSCI_ds.wciLNA;
081800180216         wciTCA  = Eccezione_T_ds.wciTCA;
081900180219         wciTCI  = WFSCI_ds.wciTCI;
082000180219         wciIFI  = WFSCI_ds.wciTCI - WFSCI_ds.wciISS;
082100180219         wciISS  = WFSCI_ds.wciISS;
082200180214
082300180214         //______________
082400180214         write  WFSCI000;
082500180214         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
082600180214
082700180214       ENDSR;
082800180209
082900180209       //--------------------------------------------------------------
083000180209       //? Stampa segnalazione dell'errore rilevato via SQL             ?
083100180209       //--------------------------------------------------------------
083200180209       BEGSR  sr_PrintERR;
083300180209
083400180209         //*·// - ?Impostazione dati di output?
083500180209         //*·if  KPJBU <> *blank;
083600180209         //*·  TNSS16ds.oSS16err = *on;
083700180209         //*·  TNSS16ds.oSS16msg = 'Rilevato errore - controllare stampe.';
083800180209         //*·endif;
083900180209
084000180209         // - ?Stampa del Dump?
084100180209         Dump(A);
084200180209
084300180209         // - ?Stampa del Job-Log?
084400180209         Qcmd = 'DSPJOBLOG job(*) output(*print)';
084500180209         exsr  sr_ExecCmd;
084600180209
084700180209         // - ?Chiusura del programma?
084800180209         exsr  sr_RoutEnd;
084900180209
085000180209       ENDSR;
085100180209
085200180209       //--------------------------------------------------------------
085300180209       //?Esecuzione del comando (già impostato).                      ?
085400180209       //--------------------------------------------------------------
085500180209       BEGSR  sr_ExecCmd;
085600180209
085700180209         clear Qcap0100;
085800180209         Qcabcsdh = *off;
085900180209         Qcapa    = *off;
086000180209         Qcacmdss = *off;
086100180209         Qcaerved = *allX'00';
086200180209
086300180209         clear Qusec;
086400180209         Qusbprv  = %size(Qusec);
086500180209
086600180209         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
086700180209                           %size(Qcap0100) : 'CPOP0100' : *omit :
086800180209                           0 : 0 : Qusec);
086900180209
087000180209         //*·if  Qusei <> *blank;
087100180209         //*·  ...;
087200180209         //*·endif;
087300180209
087400180209       ENDSR;
087500180209
087600180209       //--------------------------------------------------------------
087700180209       //?Operazioni finali.                                           ?
087800180209       //--------------------------------------------------------------
087900180209       BEGSR  sr_RoutEnd;
088000180209
088100180209         // -?Restituzione parametri al chiamante?
088200180209         if  KPJBU <> *blank;
088300180209           KPJBU = TNSS16ds;
088400180209         endif;
088500180209
088600180209         // -?Uscita dal *pgm?
088700180209         return;
088800180209
088900180209       ENDSR;
089000180209
089100180209      /end-free
