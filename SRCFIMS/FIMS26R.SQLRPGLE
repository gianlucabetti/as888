000100170208       //==============================================================
000200170320       //?FIMS26R * Determinazione orari di Inizio/Fine picking        ?
000300170320       //?scartando le letture con Tipo Applicazione "I" = Postaz.Fisse?
000400170503       //?e "W" = WiFi e forzando le eccezioni valide per Filiale/Data.?
000500170208       //==============================================================
000600170208
000700170208       //--------------------------------------------------------------
000800170208       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000900170208       //--------------------------------------------------------------
001000170208
001100170208     /*PRM  dbgview(*source)
001200170208     /*END
001300170208
001400170208       //--------------------------------------------------------------
001500170208       //?Specifiche di controllo.                                     ?
001600170208       //--------------------------------------------------------------
001700170208
001800170208     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001900170208     h dftactgrp(*no)
002000170208     h alwnull(*inputonly)
002100170208
002200170208       //--------------------------------------------------------------
002300170208       //?Dichiarazione file.                                          ?
002400170208       //--------------------------------------------------------------
002500170405
002600170405       // -?Letture modalità Picking - ECCEZIONI?
002700170405     fFIMSE01L  if   e           k disk
002800170208
002900170405       // -?Letture modalità Picking - Orari di Inizio/Fine Picking?
003000170320       //  ?- SENZA letture di Tipo Impianto "I" -?
003100170208     fFIMSP10F  o    e             disk
003200170208
003300170208       //--------------------------------------------------------------
003400170208       //?Definizione costanti.                                        ?
003500170208       //--------------------------------------------------------------
003600170209
003700170209       // -?Numero minimo di Banchi da leggere in OGNI minuto?
003800170209     d c_NrMinimo_Banchi...
003900170209     d                 c                   const('2')
004000170217
004100170217       // -?Numero minimo di Minuti Consecutivi da elaborare?
004200170217     d c_NrMinimo_MinutiConsec...
004300170217     d                 c                   const(4)
004400170208
004500170209       // -?Numero minimo di Colli da leggere in 2 minuti consecutivi?
004600170209       //  ?(per lo stesso turno)?
004700170209     d c_NrMinimo_Colli...
004800170217     d                 c                   const(60)
004900170209
005000170209       // -?Costante per controllo "caratteri solo numerici"?
005100170209     d c_Digits        c                   const('0123456789')
005200170208
005300170208       //--------------------------------------------------------------
005400170208       //?Definizione schiere.                                         ?
005500170208       //--------------------------------------------------------------
005600170217
005700170217       // -?Dati delle ultime letture precedenti?
005800170217     d sk_FIMSP_P      s                   like(FIMSP_ds)
005900170217     d                                     dim(c_NrMinimo_MinutiConsec)
006000170217     d                                     inz
006100170208
006200170208       //--------------------------------------------------------------
006300170208       //?Definizione aree dati.                                       ?
006400170208       //--------------------------------------------------------------
006500170208
006600170208
006700170208       //--------------------------------------------------------------
006800170208       //?Definizione strutture dati.                                  ?
006900170208       //--------------------------------------------------------------
007000170208
007100170208       // -?Status ds?
007200170208     d Status         sds
007300170208     d   SDSpgm          *proc
007400170208     d*//SDSprm          *parms
007500170208     d*//SDSdta              191    198
007600170208     d   SDSjob              244    253
007700170208     d   SDSusr              254    263
007800170208
007900170208       // -?Parametri Ricevuti?
008000170208     d KPJBA         e ds
008100170512     d FIMS26ds      e ds                  inz   qualified
008200170406
008300170406       // -?Giorni della settimana in FIMSE00F?
008400170406     d FIMSE_ds      e ds                  inz
008500170406     d                                     extname( FIMSE00F )
008600170410     d   sk_MSEvs                     1    dim(7)
008700170410     d                                     overlay( FIMSE_ds : 14 )
008800170410     d                                     inz
008900170208
009000170209       // -?Dati del *file FIMSP00F:?
009100170209     d FIMSP00F      e ds                  based(nullPtr)
009200170209     d                                     qualified
009300170209       //  ?... relativi al minuto in elaborazione?
009400170209     d FIMSP_ds        ds                  inz   qualified
009500170209     d   MSPfil                            inz   like(FIMSP00F.MSPfil)
009600170209     d   MSPdtl                            inz   like(FIMSP00F.MSPdtl)
009700170209     d   MSPtur                            inz   like(FIMSP00F.MSPtur)
009800170209     d*//MSPimp                            inz   like(FIMSP00F.MSPimp)
009900170209     d   MSPhml                            inz   like(FIMSP00F.MSPhml)
010000170209     d   wTotPos                      3  0 inz
010100170209     d   MSPncl                     + 4    inz   like(FIMSP00F.MSPncl)
010200170217       //  ?... relativi ai 3 minuti precedenti?
010300170210     d FIMSP_P_ds      ds                  inz   likeds(FIMSP_ds)
010400170209       //  ?... relativi all'orario iniziale del turno picking?
010500170209     d FIMSP_I_ds      ds                  inz   likeds(FIMSP_ds)
010600170209       //  ?... relativi all'orario  finale  del turno picking?
010700170209     d FIMSP_F_ds      ds                  inz   likeds(FIMSP_ds)
010800170208
010900170208       //--------------------------------------------------------------
011000170208       //?Definizione variabili globali.                               ?
011100170208       //--------------------------------------------------------------
011200170208
011300170208       // -?Flags booleani?
011400170208     d $Fine           s               n   inz
011500170406     d $Eccezione      s               n   inz
011600170512     d $Where          s               n   inz
011700170217
011800170217       // -?Indici di schiera?
011900170217     d xx              s              3  0 inz
012000170208
012100170208       // -?Stringa SQL da eseguire?
012200170209     d wSql            s          32740    inz   varying
012300170209
012400170209       // -?Data da elaborare?
012500170209     d wDate           s              8s 0 inz
012600170406     d wData_Iso       s               d   inz   datfmt(*iso)
012700170406     d wDay_Of_Week    s              2  0 inz
012800170209
012900170209       // -?Minuti in elaborazione?
013000170209     d wTime_I         s               t   inz   timfmt(*hms)
013100170209     d wTime_F         s               t   inz   timfmt(*hms)
013200170217
013300170217       // -?Colli in elaborazione?
013400170217     d wTotNCL         s                   inz   like(FIMSP_ds.MSPncl)
013500170407
013600170407       // -?Comodo?
013700170407     d wSaveFIL        s                   inz   like(FIMSP_ds.MSPfil)
013800170407     d wSaveDTL        s                   inz   like(FIMSP_ds.MSPdtl)
013900170407     d wSaveTUR        s                   inz   like(FIMSP_ds.MSPtur)
014000170208
014100170208       //--------------------------------------------------------------
014200170208       //?Definizione prototipi procedure usate.                       ?
014300170208       //--------------------------------------------------------------
014400170208
014500170208       // -?API QCAPCMD (Process Commands)?
014600170208     d Qcmd            s           2048    inz   varying
014700170208      /copy qSysInc/qRpgleSrc,QCAPCMD
014800170208      /copy gaitrasrc/srcProtoPR,QCAPCMD
014900170208       // -?Parametri gestione errori API.?
015000170208      /copy qsysinc/qRpgleSrc,QUSEC
015100170208
015200170208       //--------------------------------------------------------------
015300170208       //?Definizione key-list.                                        ?
015400170208       //--------------------------------------------------------------
015500170208
015600170406       // -?File FIMSE01L?
015700170406     d keyFIMSE01    e ds                  extname( FIMSE01L : *key )
015800170406     d                                     prefix( k_ )   inz
015900170208
016000170208       //--------------------------------------------------------------
016100170208       //?Riepilogo indicatori utilizzati.                             ?
016200170208       //--------------------------------------------------------------
016300170208
016400170208
016500170208       //--------------------------------------------------------------
016600170208       //?M A I N - L I N E                                            ?
016700170208       //--------------------------------------------------------------
016800170208
016900170208     c     *Entry        plist
017000170208     c                   parm                    KPJBA
017100170208
017200170208      /free
017300170208
017400170208       // -?Operazioni iniziali?
017500170208       exsr  sr_RoutInz;
017600170208
017700170208       // -?Preparazione stringa SQL?
017800170208       exsr  sr_PrepSQL;
017900170208
018000170209       // -?Apertura del cursore?
018100170208       exsr  sr_OpenCursor;
018200170208
018300170208       // -?Lettura del file?
018400170208       DoW  Not $Fine;
018500170208         exsr  sr_ReadCursor;
018600170208       EndDo;
018700170208
018800170208       // -?Chiusura del cursore?
018900170208       exsr  sr_CloseCursor;
019000170208
019100170208       // -?Operazioni finali?
019200170208       exsr  sr_RoutEnd;
019300170208
019400170208       //--------------------------------------------------------------
019500170208       //?Operazioni iniziali.                                         ?
019600170208       //--------------------------------------------------------------
019700170208       BEGSR  sr_RoutInz;
019800170208
019900170208         // -?Impostazione opzioni per SQL?
020000170208         exec sql   set option   DynUsrPrf = *Owner,
020100170208                                 CloSqlCsr = *EndMod;
020200170208
020300170208         // -?Impostazione chiusura?
020400170208         *inLR = *on;
020500170208
020600170208         // -?Ricezione Parametri?
020700170512         if  KPJBU <> *blank;
020800170512           FIMS26ds = KPJBU;
020900170512           FIMS26ds.oMS26err = *off;
021000170512           clear  FIMS26ds.oMS26msg;
021100170512         endif;
021200170512         //*·if  %check( c_Digits : %subst( KPJBU : 1 :
021300170512         //*·                       %size( FIMS26ds.iMS26fil ) ) ) > *zero;
021400170512         //*·  clear  FIMS26ds.iMS26fil;
021500170512         //*·endif;
021600170512         //*·if  %check( c_Digits : %subst( KPJBU :
021700170512         //*·                       %size( FIMS26ds.iMS26fil ) + 1 :
021800170512         //*·                       %size( FIMS26ds.iMS26dti ) ) ) > *zero;
021900170512         //*·  clear  FIMS26ds.iMS26dti;
022000170512         //*·  clear  FIMS26ds.iMS26hmi;
022100170512         //*·endif;
022200170512         //*·if  %check( c_Digits : %subst( KPJBU :
022300170512         //*·                       %size( FIMS26ds.iMS26fil ) +
022400170512         //*·                       %size( FIMS26ds.iMS26dti ) +
022500170512         //*·                       %size( FIMS26ds.iMS26hmi ) + 1 :
022600170512         //*·                       %size( FIMS26ds.iMS26dtf ) ) ) > *zero;
022700170512         //*·  clear  FIMS26ds.iMS26dtf;
022800170512         //*·  clear  FIMS26ds.iMS26hmf;
022900170512         //*·endif;
023000170208
023100170208         //*·// -?Reperimento data e ora attuali?
023200170208         //*·wTime_ds = %subst( %char( %dec( %timestamp() ) )
023300170208         //*·                   : 1 : 14 );
023400170208
023500170208         // -?Impostazione data limite?
023600170515         wDate = %dec( %date() - %days(1) );
023700170208
023800170512         if  FIMS26ds.iMS26dti > *zero  and
023900170512             FIMS26ds.iMS26dtf = *zero;
024000170512           FIMS26ds.iMS26dtf = wDate;
024100170512           FIMS26ds.iMS26hmf = 9999;
024200170208         endif;
024300170512
024400170512         // -?SE ricevuti parametri: è per una RI-Determinazione orari?
024500170512         //  ?di Inizio/Fine picking - occorre quindi CANCELLARE i dati?
024600170512         //  ?corrispindenti già estratti in FIMSP10F.?
024700170512         if  KPJBU <> *blank;
024800170512           exsr  sr_DeleteFIMSP10;
024900170512         endif;
025000170406
025100170406         // -?Ricerca di eventuali ECCEZIONI previste nel *file FIMSE00F?
025200170406         // ·?Per ogni eccezione trovata: i suoi dati vanno a sostituire?
025300170406         //  ?quelli che saranno raccolti (per Filiale/Turno)?
025400170512         if  FIMS26ds.iMS26dti = *zero  and
025500170512             FIMS26ds.iMS26dtf = *zero;
025600170406           exsr  sr_CtrlFIMSE;
025700170406         else;
025800170512           wDate = FIMS26ds.iMS26dti;
025900170512           DoW  wDate <= FIMS26ds.iMS26dtf;
026000170406             exsr  sr_CtrlFIMSE;
026100170406             wDate = %dec( %date( wDate : *iso ) + %days(1) );
026200170406           EndDo;
026300170515           wDate = %dec( %date() - %days(1) );
026400170406         endif;
026500170208
026600170208       ENDSR;
026700170512
026800170512       //--------------------------------------------------------------
026900170512       //?Cancellazione dati già registrati su FIMSP10F prima di una   ?
027000170512       //?RI-Determinazione degli orari di Inizio/Fine picking.        ?
027100170512       //--------------------------------------------------------------
027200170512       BEGSR  sr_DeleteFIMSP10;
027300170512
027400170512         clear  $Where;
027500170512
027600170512         // -?Preparazione SQL da eseguire?
027700170512         wSQL =  'delete from FIMSP10F';
027800170512
027900170512         // -?Parzializzazione per Filiale?
028000170512         if  FIMS26ds.iMS26fil > *zero;
028100170512
028200170512           if  Not $Where;
028300170512             $Where = *on;
028400170512             wSQL  +=  ' where';
028500170512           else;
028600170512             wSQL  +=  ' and';
028700170512           endif;
028800170512
028900170512           wSQL +=     ' MSP1fil = ' + %char( FIMS26ds.iMS26fil );
029000170512
029100170512         endif;
029200170512
029300170512         // -?Parzializzazione per Data/Ora Lettura?
029400170512         if  ( FIMS26ds.iMS26dti + FIMS26ds.iMS26hmi ) > *zero  or
029500170512             ( FIMS26ds.iMS26dtf + FIMS26ds.iMS26hmf ) > *zero;
029600170512
029700170512           if  Not $Where;
029800170512             $Where = *on;
029900170512             wSQL  +=  ' where';
030000170512           else;
030100170512             wSQL  +=  ' and';
030200170512           endif;
030300170512
030400170512           wSQL +=     ' MSP1dtl between ' + %char( FIMS26ds.iMS26dti ) +
030500170512                                   ' and ' + %char( FIMS26ds.iMS26dtf );
030600170512
030700170512         endif;
030800170512
030900170512         // -?Parzializzazione per Turno?
031000170512         if  FIMS26ds.iMS26tur <> *blank;
031100170512
031200170512           if  Not $Where;
031300170512             $Where = *on;
031400170512             wSQL  +=  ' where';
031500170512           else;
031600170512             wSQL  +=  ' and';
031700170512           endif;
031800170512
031900170512           wSQL +=     ' MSP1tur = ''' + FIMS26ds.iMS26tur + '''';
032000170512
032100170512         endif;
032200170512
032300170512         // -?Parzializzazione per Impianto?
032400170512         if  FIMS26ds.iMS26imp <> *blank;
032500170512
032600170512           if  Not $Where;
032700170512             $Where = *on;
032800170512             wSQL  +=  ' where';
032900170512           else;
033000170512             wSQL  +=  ' and';
033100170512           endif;
033200170512
033300170512           wSQL +=     ' MSP1imp = ''' + FIMS26ds.iMS26imp + '''';
033400170512
033500170512         endif;
033600170512
033700170512
033800170512         // -?Cancellazione vecchi record dal file?
033900170512         exec SQL   execute immediate :wSQL;
034000170512
034100170512         if  SQLcode < *zero;
034200170512           $Fine = *on;
034300170512           exsr  sr_PrintERR;
034400170512           exsr  sr_RoutEnd;
034500170512         endif;
034600170512
034700170512       ENDSR;
034800170406
034900170406       //--------------------------------------------------------------
035000170406       //?Ricerca di eventuali ECCEZIONI perviste nel *file FIMSE00F e ?
035100170406       //?scrittura record nel *file FIMSP10F (al di là delle letture  ?
035200170406       //?riportate in FIMSP00F).                                      ?
035300170406       //--------------------------------------------------------------
035400170406       BEGSR  sr_CtrlFIMSE;
035500170406
035600170406         // -?Ciclo di lettura di FIMSE00F per un singolo giorno?
035700170406         clear  keyFIMSE01;
035800170512         if  FIMS26ds.iMS26fil > *zero;
035900170512           k_MSEfil = FIMS26ds.iMS26fil;
036000170420         endif;
036100170512         if  FIMS26ds.iMS26tur > *zero;
036200170512           k_MSEtur = FIMS26ds.iMS26tur;
036300170512         endif;
036400170406         setll  %kds( keyFIMSE01 )  FIMSE000;
036500170512         if  FIMS26ds.iMS26fil > *zero;
036600170420           readE  %kds( keyFIMSE01 : 1 )  FIMSE000;
036700170420         else;
036800170420           read   FIMSE000;
036900170420         endif;
037000170406
037100170406         DoW  NOT %eof( FIMSE01L );
037200170406
037300170512           If  ( FIMS26ds.iMS26tur = *blank    or
037400170512                 FIMS26ds.iMS26tur = MSEtur )  and
037500170512               ( wDate >= MSEdti             and
037600170512                 wDate <= MSEdtf );
037700170406
037800170406             // -?Reperimento giorno della settimana?
037900170406             //  ?(1=Lu/2=Ma/3=Me/4=Gi/5=Ve/6=Sa/7=Do)?
038000170406             wData_Iso = %date( wDate : *iso );
038100170406             clear wDay_Of_Week;
038200170406             exec sql   set   :wDay_Of_Week = DayOfWeek_Iso( :wData_Iso );
038300170406
038400170406             // -?Scrittura record Inizio/Fine per la singola eccezione?
038500170406             //  ?prevista nel giorno che si sta elaborando?
038600170406             if  wDay_Of_Week              > *zero  and
038700170406                 sk_MSEvs( wDay_Of_Week ) <> *blank;
038800170406               exsr  sr_WriteFIMSP_Eccezione;
038900170406             endif;
039000170406
039100170406           EndIf;
039200170406
039300170512           if  FIMS26ds.iMS26fil > *zero;
039400170420             readE  %kds( keyFIMSE01 : 1 )  FIMSE000;
039500170420           else;
039600170420             read  FIMSE000;
039700170420           endif;
039800170406
039900170406         EndDo;
040000170406
040100170406       ENDSR;
040200170406
040300170406       //--------------------------------------------------------------
040400170406       //?Scrittura dei 2 record in FIMSP10F (Inizio e Fine) per ogni  ?
040500170406       //?eccezione valida trovata in FIMSE00F (per i singoli Filiale, ?
040600170406       //?Turno e Data lettura).                                       ?
040700170406       //--------------------------------------------------------------
040800170406       BEGSR  sr_WriteFIMSP_Eccezione;
040900170406
041000170406         // -?Scrittura record di Avvio turno PKD?
041100170406         clear  FIMSP100;
041200170406         MSP1fil = MSEfil;
041300170406         MSP1dtl = wDate;
041400170406         MSP1tur = MSEtur;
041500170406         MSP1fif = 'I';
041600170406         MSP1hml = MSEhmi;
041700170410         //*·MSP1tim = %timestamp();
041800170410         MSP1tim = %timestamp( %date() );
041900170406         //______________
042000170406         write  FIMSP100;
042100170406         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
042200170406
042300170406         // -?Scrittura record di Fine turno PKD?
042400170406         clear  FIMSP100;
042500170406         MSP1fil = MSEfil;
042600170406         MSP1dtl = wDate;
042700170406         MSP1tur = MSEtur;
042800170406         MSP1fif = 'F';
042900170406         MSP1hml = MSEhmf;
043000170410         //*·MSP1tim = %timestamp();
043100170410         MSP1tim = %timestamp( %date() );
043200170406         //______________
043300170406         write  FIMSP100;
043400170406         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
043500170406
043600170406       ENDSR;
043700170208
043800170208       //--------------------------------------------------------------
043900170208       //?Preparazione stringa SQL per estrazione dati da FLTR700F.    ?
044000170208       //--------------------------------------------------------------
044100170208       BEGSR  sr_PrepSQL;
044200170208
044300170208         wSQL =  'select MSPfil, MSPdtl, MSPtur, MSPhml, +
044400170208                         count(*), sum(MSPncl) +
044500170208                    from FIMSP00F +
044600170320                   where MSPtur <> ''N'' +
044700170503                     and MSPimp not in (''I'', ''W'')';
044800170208
044900170512         // -?Parzializzazione per Filiale?
045000170512         if  FIMS26ds.iMS26fil > *zero;
045100170208
045200170512           wSQL += ' and MSPfil = ' + %editc( FIMS26ds.iMS26fil : 'X' );
045300170208
045400170208         endif;
045500170208
045600170512         // -?Parzializzazione per Data/Ora Lettura?
045700170512         if  ( FIMS26ds.iMS26dti + FIMS26ds.iMS26hmi ) > *zero  or
045800170512             ( FIMS26ds.iMS26dtf + FIMS26ds.iMS26hmf ) > *zero;
045900170208
046000170210           wSQL += ' and ( ( MSPdtl * 10000 ) + MSPhml ) +
046100170516                         between ' + %char( FIMS26ds.iMS26dti ) +
046200170516                                    %editc( FIMS26ds.iMS26hmi : 'X' ) +
046300170516                           ' and ' + %char( FIMS26ds.iMS26dtf ) +
046400170516                                    %editc( FIMS26ds.iMS26hmf : 'X' );
046500170208
046600170208         else;
046700170208
046800170208           wSQL += ' and MSPdtl = ' + %editc( wDate : 'X' );
046900170208
047000170208         endif;
047100170210
047200170512         // -?Parzializzazione per Turno?
047300170512         if  FIMS26ds.iMS26tur <> *blank;
047400170210
047500170512           wSQL += ' and MSPtur = ''' + FIMS26ds.iMS26tur + '''';
047600170210
047700170210         endif;
047800170210
047900170512         // -?Parzializzazione per Impianto?
048000170512         if  FIMS26ds.iMS26imp <> *blank;
048100170210
048200170512           wSQL += ' and MSPimp = ''' + FIMS26ds.iMS26imp + '''';
048300170210
048400170210         endif;
048500170208
048600170208         wSQL += ' group by MSPfil, MSPdtl, MSPtur, MSPhml +
048700170209                  having count(*) >= ' + c_NrMinimo_Banchi +
048800170209                 ' order by MSPfil, MSPdtl, MSPtur, MSPhml +
048900170208                     for fetch only';
049000170208
049100170208       ENDSR;
049200170208
049300170208       //--------------------------------------------------------------
049400170208       //?Apertura cursore.                                            ?
049500170208       //--------------------------------------------------------------
049600170208       BEGSR  sr_OpenCursor;
049700170208
049800170208         // -?Dichiarazione cursore?
049900170208         exec sql   prepare S1   from :wSQL;
050000170208         exec sql   declare C1   cursor for S1;
050100170208
050200170208         if  SQLcode < *zero;
050300170208           $Fine = *on;
050400170208           exsr  sr_PrintERR;
050500170208         endif;
050600170208
050700170208         // -?Apertura del cursore?
050800170208         exec sql   open C1;
050900170208
051000170208         if  SQLcode < *zero;
051100170208           $Fine = *on;
051200170208           exsr  sr_PrintERR;
051300170208         endif;
051400170208
051500170208       ENDSR;
051600170208
051700170208       //--------------------------------------------------------------
051800170208       //?Lettura cursore.                                             ?
051900170208       //--------------------------------------------------------------
052000170208       BEGSR  sr_ReadCursor;
052100170208
052200170208         $Fine = *off;
052300170209         clear  FIMSP_ds;
052400170208
052500170208         exec sql   fetch next   from C1   into :FIMSP_ds;
052600170208
052700170208         select;
052800170208           when  SQLcode = 100;
052900170208             exsr  sr_Write_FIMSP10F;
053000170208             $Fine = *on;
053100170208           when  SQLcode < *zero;
053200170208             $Fine = *on;
053300170208             exsr  sr_PrintERR;
053400170208           other;
053500170208             exsr  sr_Elab_FIMSP00F;
053600170208         endsl;
053700170208
053800170208       ENDSR;
053900170208
054000170208       //--------------------------------------------------------------
054100170208       //?Chiusura cursore.                                            ?
054200170208       //--------------------------------------------------------------
054300170208       BEGSR  sr_CloseCursor;
054400170208
054500170208         // -?Chiusura del cursore?
054600170208         exec sql   close C1;
054700170208
054800170208       ENDSR;
054900170208
055000170208       //--------------------------------------------------------------
055100170209       //?Controllo dati appena estratti da FIMSP00F.                  ?
055200170208       //--------------------------------------------------------------
055300170209       //?0) Ogni minuto devono essere elaborati almeno 2 banchi       ?
055400170209       //?   (ma questo filtro viene già eseguito via SQL).            ?
055500170209       //?1) I dati in input devono essere successivi agli ultimi letti?
055600170405       //?   di SOLO 1 minuto.                                         ?
055700170209       //?   In caso di differenza superiore: viene cancellata la let- ?
055800170209       //?   tura del 1° minuto e reimpostata con quella in input.     ?
055900170412       //?2) L'orario delle letture in input dev'essere successivo di  ?
056000170412       //?   solo 1 minuto a quello precedente, e devono risultare al- ?
056100170414       //?   meno 4 minuti (vedi costante "c_NrMinimo_MinutiConsec")   ?
056200170414       //?   consecutivi.                                              ?
056300170412       //?   In caso di "durata" inferiore viene cancellata la lettura ?
056400170412       //?   del 1° minuto e reimpostata con quella del minuto succes- ?
056500170412       //?   sivo.                                                     ?
056600170217       //?3) In questi 4 minuti (vedi costante "c_NrMinimo_MinutiConsec")?
056700170217       //?   devono risultare elaborati almeno 60 colli (vedi costante ?
056800170217       //?   "c_NrMinimo_Colli").                                      ?
056900170217       //?   In caso di sommatoria inferiore: viene rimossa la lettura ?
057000170412       //?   del 1° minuto e reimpostata con quella del minuto succes- ?
057100170412       //?   sivo).                                                    ?
057200170209       //--------------------------------------------------------------
057300170209       BEGSR  sr_Elab_FIMSP00F;
057400170209
057500170414         // -?"Rottura" di Filiale/Data/Turno?
057600170414         IF  ( FIMSP_ds.MSPfil <> wSaveFIL  or
057700170414               FIMSP_ds.MSPdtl <> wSaveDTL  or
057800170414               FIMSP_ds.MSPtur <> wSaveTUR );
057900170414
058000170414           wSaveFIL = FIMSP_ds.MSPfil;
058100170414           wSaveDTL = FIMSP_ds.MSPdtl;
058200170414           wSaveTUR = FIMSP_ds.MSPtur;
058300170414
058400170414
058500170414           // -?Eventuale scrittura record a rottura di Filiale/Data/Turno?
058600170414           //  ?e   Memorizzaz. dei dati relativi al nuovo Avvio turno PKD?
058700170414           If  FIMSP_I_ds.MSPfil <> *zero             and
058800170414               ( FIMSP_ds.MSPfil <> FIMSP_I_ds.MSPfil  or
058900170414                 FIMSP_ds.MSPdtl <> FIMSP_I_ds.MSPdtl  or
059000170414                 FIMSP_ds.MSPtur <> FIMSP_I_ds.MSPtur );
059100170414             exsr  sr_Write_FIMSP10F;
059200170414           EndIf;
059300170414
059400170414
059500170414           clear  FIMSP_I_ds;
059600170414           clear  FIMSP_F_ds;
059700170414           clear  FIMSP_P_ds;
059800170414           clear  sk_FIMSP_P;
059900170406
060000170406
060100170414           // -?Reperimento giorno della settimana in elaborazione?
060200170414           //  ?(1=Lu/2=Ma/3=Me/4=Gi/5=Ve/6=Sa/7=Do)?
060300170414           wData_Iso = %date( FIMSP_ds.MSPdtl : *iso );
060400170414           clear wDay_Of_Week;
060500170414           exec sql   set   :wDay_Of_Week = DayOfWeek_Iso( :wData_Iso );
060600170414
060700170414           // -?SE filiale/turno previsti come "eccezione" (in FIMSE00F)?
060800170414           //  ?inizio/fine sono già stati inseriti in FIMSP10F  e questi?
060900170414           //  ?dati NON sono da elaborare?
061000170414           clear  $Eccezione;
061100170414           clear  keyFIMSE01;
061200170414           k_MSEfil = FIMSP_ds.MSPfil;
061300170414           k_MSEtur = FIMSP_ds.MSPtur;
061400170414           k_MSEdti = FIMSP_ds.MSPdtl;
061500170414           setll  %kds( keyFIMSE01 )  FIMSE000;
061600170414           reade  %kds( keyFIMSE01 : 2 )  FIMSE000;
061700170414
061800170414           DoW  Not %eof( FIMSE01L )  and  wDay_Of_Week > *zero;
061900170414
062000170414             // -?Scrittura record Inizio/Fine già fatta per la singola?
062100170414             //  ?eccezione e per il giorno che si sta elaborando?
062200170414             if  FIMSP_ds.MSPdtl >= MSEdti  and
062300170414                 FIMSP_ds.MSPdtl <= MSEdtf  and
062400170414                 sk_MSEvs( wDay_Of_Week ) <> *blank;
062500170414               $Eccezione = *on;
062600170414               leave;
062700170414             endif;
062800170414
062900170414             reade  %kds( keyFIMSE01 : 2 )  FIMSE000;
063000170414
063100170414           EndDo;
063200170414
063300170414
063400170414           // -?Memorizzazione dati del 1° minuto per?
063500170414           //  ?la nuova filiale o il nuovo giorno o il nuovo turno?
063600170414           //  ?(SE non è stata rilevata come "eccezione")?
063700170414           if  NOT $Eccezione;
063800170414             FIMSP_P_ds = FIMSP_ds;
063900170414             sk_FIMSP_P(1) = FIMSP_ds;
064000170414             leavesr;
064100170414           endif;
064200170406
064300170414         ENDIF;
064400170414
064500170406
064600170414         // -?Si scartano le "eccezioni" - già inserite in FIMSP10F?
064700170406         if  $Eccezione;
064800170406           leavesr;
064900170406         endif;
065000170209
065100170209
065200170217         clear  FIMSP_P_ds;
065300170217         if  sk_FIMSP_P(1) <> *blank;
065400170217           FIMSP_P_ds = sk_FIMSP_P(1);
065500170217         endif;
065600170405
065700170217
065800170209         // -?1) Controllo sequenza dei minuti in lettura?
065900170210
066000170210         // -?Non ancora elaborato il minuto precedente:?
066100170210         //  ?sarà questo!?
066200170210         if  FIMSP_P_ds.MSPhml = *zero;
066300170210           FIMSP_P_ds = FIMSP_ds;
066400170217           clear  sk_FIMSP_P;
066500170217           sk_FIMSP_P(1) = FIMSP_ds;
066600170210           leavesr;
066700170210         endif;
066800170209
066900170210         // -?Impostazione del minuto iniziale (quello precedente)?
067000170210         wTime_I = %time( FIMSP_P_ds.MSPhml * 100 : *hms );
067100170209         // -?Impostazione del minuto finale (appena letto)?
067200170209         wTime_F = %time( FIMSP_ds.MSPhml * 100 : *hms );
067300170209
067400170217         // -?SE la differenza con la lettura precedente è oltre 1 minuto?
067500170217         //  ? & si sta cercando ancora il minuto Finale (e magari anche?
067600170217         //  ?   quello iniziale):?
067700170210         //  ?=> Si re-imposta la lettura in input come precedente?
067800170217         //  ? e si riparte da essa?
067900170217         if  wTime_I + %minutes(1) < wTime_F  and
068000170217             FIMSP_F_ds.MSPfil     = *zero;
068100170210           FIMSP_P_ds = FIMSP_ds;
068200170217           clear  FIMSP_I_ds;
068300170217           clear  FIMSP_F_ds;
068400170217           clear  sk_FIMSP_P;
068500170217           sk_FIMSP_P(1) = FIMSP_ds;
068600170217           leavesr;
068700170210         endif;
068800170209
068900170209
069000170210         // -?Impostazione dati relativi all'Avvio turno PKD?
069100170210         //  ?(se non ancora impostati)?
069200170210         if  FIMSP_I_ds.MSPfil = *zero;
069300170210           FIMSP_I_ds = FIMSP_P_ds;
069400170210         endif;
069500170217
069600170217         // -?Aggiornamento del n° colli letti in questi minuti?
069700170217         FIMSP_I_ds.MSPncl += FIMSP_ds.MSPncl;
069800170210
069900170217
070000170217         // -?Salvataggio dati degli ultimi minuti elaborati in?
070100170217         //  ?precedenza per controllare il minuto successivo?
070200170217         For  xx = c_NrMinimo_MinutiConsec  DownTo 2;
070300170217           sk_FIMSP_P( xx ) = sk_FIMSP_P( xx - 1 );
070400170217         EndFor;
070500170217
070600170217         // -?Salvataggio dati del minuto in corso di elaborazione?
070700170217         sk_FIMSP_P( 1 ) = FIMSP_ds;
070800170217         FIMSP_P_ds      = FIMSP_ds;
070900170217
071000170217
071100170217         // -?2) Controllo del numero minimo di minuti consecutivi?
071200170217         //     ?delle ultime letture?
071300170217
071400170217         // -?Impostazione dell'orario iniziale (già memorizzato)?
071500170217         wTime_I = %time( FIMSP_I_ds.MSPhml * 100 : *hms );
071600170217         // -?Impostazione dell'orario finale (appena letto)?
071700170217         wTime_F = %time( FIMSP_ds.MSPhml * 100 : *hms );
071800170217
071900170217         // -?SE la differenza tra le 2 letture è inferiore a 3 minuti?
072000170217         //  ?(che sono per forza consecutivi - vedi 1° controllo)?
072100170217         //  ?=> Non si memorizza la lettura in input come ultima?
072200170217         if  ( wTime_I + %minutes(c_NrMinimo_MinutiConsec)
072300170217                       - %minutes(1) ) > wTime_F;
072400170217           leavesr;
072500170217         endif;
072600170217
072700170217
072800170217         // -?3) Controllo del numero minimo di colli letti, finora?
072900170217         //     ?elaborati (negli ultimi almeno 4 minuti consecutivi)?
073000170217
073100170217         // -?SE la somma dei colli delle ultime almeno 4 letture?
073200170217         //  ?è inferione a 60:?
073300170217         //  ?=> Non si memorizza la lettura in input come Finale?
073400170217         //  ?   e   si memorizza la prima delle ultime memorizzate?
073500170217         //  ?       come quella di Avvio turno PKD?
073600170217         IF  FIMSP_I_ds.MSPncl < c_NrMinimo_Colli;
073700170217
073800170217           // -?Si ricerca il minuto successivo a quello attualmente?
073900170217           //  ?memorizzato?
074000170217           For  xx = c_NrMinimo_MinutiConsec  DownTo 1;
074100170217
074200170217             FIMSP_P_ds = sk_FIMSP_P(xx);
074300170217
074400170217             If  FIMSP_P_ds.MSPfil > *zero  and
074500170217                 FIMSP_P_ds.MSPhml > FIMSP_I_ds.MSPhml;
074600170217
074700170217               FIMSP_I_ds = FIMSP_P_ds;
074800170217               // -?Quando trovato: si sommano i colli lavorati da quel?
074900170217               //  ?minuto, fino a quello attuale?
075000170217               xx -= 1;
075100170217               doW  xx >= 1;
075200170217                 FIMSP_P_ds = sk_FIMSP_P(xx);
075300170217                 FIMSP_I_ds.MSPncl += FIMSP_P_ds.MSPncl;
075400170217                 xx -= 1;
075500170217               enddo;
075600170217
075700170217               leave;
075800170217
075900170217             EndIf;
076000170217
076100170217           EndFor;
076200170217
076300170217           leavesr;
076400170217
076500170217         ENDIF;
076600170217
076700170217
076800170217         // -?Impostazione dati relativi alla Fine turno PKD?
076900170217         //  ?Anche la lettura Finale deve avere 4 minuti di letture?
077000170217         //  ?consecutive:  SE la differenza tra ciascuna delle letture?
077100170217         //  ?precedenti è oltre 1 minuto => NON si imposta la lettura?
077200170217         //  ?in input come finale?
077300170217
077400170217         // -?Controllo minuto della Fine turno PKD?
077500170217         // -?Impostazione del minuto finale (appena letto) - in esame?
077600170217         wTime_F = %time( FIMSP_ds.MSPhml * 100 : *hms );
077700170217         // -?Impostazione del totale colli letti negli ultimi minuti?
077800170217         wTotNCL = FIMSP_ds.MSPncl;
077900170217
078000170217         For  xx = 2  To c_NrMinimo_MinutiConsec;
078100170217
078200170217           // -?Impostazione del minuto precedente quello in esame?
078300170217           FIMSP_P_ds = sk_FIMSP_P(xx);
078400170217           wTime_I = %time( FIMSP_P_ds.MSPhml * 100 : *hms );
078500170217
078600170217           // -?Se differente di ultre 1 minuto:?
078700170217           //  ?NON si memorizza l'orario di Fine turno?
078800170217           if  wTime_I + %minutes(xx-1) <> wTime_F;
078900170217             leavesr;
079000170217           endif;
079100170217
079200170217           // -?Aggiornamento del n° colli letti in questi minuti?
079300170217           wTotNCL += FIMSP_P_ds.MSPncl;
079400170217
079500170217         EndFor;
079600170217
079700170217         if  wTotNCL >= c_NrMinimo_Colli;
079800170217           FIMSP_F_ds = FIMSP_ds;
079900170217         endif;
080000170208
080100170208       ENDSR;
080200170208
080300170208       //--------------------------------------------------------------
080400170208       //?Scrittura del record nel *file FIMSP10F.                     ?
080500170208       //--------------------------------------------------------------
080600170208       BEGSR  sr_Write_FIMSP10F;
080700170208
080800170209         // -?Nessuna lettura consecutiva reperita dopo l'ultima?
080900170209         //  ?memorizzata come iniziale?
081000170209         // ·?Questo controllo è meglio farlo all'inizio di questa?
081100170209         //  ?subroutine perchè essa stessa viene eseguita in più punti?
081200170209         //  ?del programma?
081300170209         if  FIMSP_F_ds.MSPfil = *zero;
081400170208           leavesr;
081500170208         endif;
081600170208
081700170209         // -?Scrittura record di Avvio turno PKD?
081800170209         clear  FIMSP100;
081900170209         MSP1fil = FIMSP_I_ds.MSPfil;
082000170209         MSP1dtl = FIMSP_I_ds.MSPdtl;
082100170209         MSP1tur = FIMSP_I_ds.MSPtur;
082200170208         MSP1fif = 'I';
082300170209         MSP1hml = FIMSP_I_ds.MSPhml;
082400170209         MSP1tim = %timestamp();
082500170208         //______________
082600170209         write  FIMSP100;
082700170208         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
082800170208
082900170209         // -?Scrittura record di Fine turno PKD?
083000170210         clear  FIMSP100;
083100170210         MSP1fil = FIMSP_F_ds.MSPfil;
083200170210         MSP1dtl = FIMSP_F_ds.MSPdtl;
083300170210         MSP1tur = FIMSP_F_ds.MSPtur;
083400170208         MSP1fif = 'F';
083500170209         MSP1hml = FIMSP_F_ds.MSPhml;
083600170209         MSP1tim = %timestamp();
083700170208         //______________
083800170209         write  FIMSP100;
083900170208         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
084000170208
084100170208       ENDSR;
084200170208
084300170208       //--------------------------------------------------------------
084400170208       //?Stampa segnalazione dell'errore rilevato via SQL             ?
084500170208       //--------------------------------------------------------------
084600170208       BEGSR  sr_PrintERR;
084700170512
084800170512         // -?Impostazione dati di output?
084900170512         if  KPJBU <> *blank;
085000170512           FIMS26ds.oMS26err = *on;
085100170512           FIMS26ds.oMS26msg = 'Rilevato errore - controllare stampe.';
085200170512         endif;
085300170208
085400170208         // -?Stampa del Dump?
085500170208         Dump(A);
085600170208
085700170208         // -?Stampa del Job-Log?
085800170208         Qcmd = 'DSPJOBLOG job(*) output(*print)';
085900170208         exsr  sr_ExecCmd;
086000170208
086100170208         // -?Chiusura del programma?
086200170208         exsr  sr_RoutEnd;
086300170208
086400170208       ENDSR;
086500170208
086600170208       //--------------------------------------------------------------
086700170208       //?Esecuzione del comando (già impostato).                      ?
086800170208       //--------------------------------------------------------------
086900170208       BEGSR  sr_ExecCmd;
087000170208
087100170208         clear Qcap0100;
087200170208         Qcabcsdh = *off;
087300170208         Qcapa    = *off;
087400170208         Qcacmdss = *off;
087500170208         Qcaerved = *allX'00';
087600170208
087700170208         clear Qusec;
087800170208         Qusbprv  = %size(Qusec);
087900170208
088000170208         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
088100170208                           %size(Qcap0100) : 'CPOP0100' : *omit :
088200170208                           0 : 0 : Qusec);
088300170208
088400170208         //*·if  Qusei <> *blank;
088500170208         //*·  ...;
088600170208         //*·endif;
088700170208
088800170208       ENDSR;
088900170208
089000170208       //--------------------------------------------------------------
089100170208       //?Operazioni finali.                                           ?
089200170208       //--------------------------------------------------------------
089300170208       BEGSR  sr_RoutEnd;
089400170208
089500170512         // -?Restituzione parametri al chiamante?
089600170512         if  KPJBU <> *blank;
089700170512           KPJBU = FIMS26ds;
089800170512         endif;
089900170512
090000170208         // -?Uscita dal *pgm?
090100170208         return;
090200170208
090300170208       ENDSR;
090400170208
090500170208      /end-free
