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
008100170209     d FIMS26ds        ds                  inz   qualified
008200170208     d   prm_Fil                      3s 0 inz
008300170210     d   prm_Time_Ini                12s 0 inz
008400170210     d   prm_Time_End                12s 0 inz
008500170208     d   prm_Turno                    1a   inz
008600170208     d   prm_Impianto                 1a   inz
008700170406
008800170406       // -?Giorni della settimana in FIMSE00F?
008900170406     d FIMSE_ds      e ds                  inz
009000170406     d                                     extname( FIMSE00F )
009100170410     d   sk_MSEvs                     1    dim(7)
009200170410     d                                     overlay( FIMSE_ds : 14 )
009300170410     d                                     inz
009400170208
009500170209       // -?Dati del *file FIMSP00F:?
009600170209     d FIMSP00F      e ds                  based(nullPtr)
009700170209     d                                     qualified
009800170209       //  ?... relativi al minuto in elaborazione?
009900170209     d FIMSP_ds        ds                  inz   qualified
010000170209     d   MSPfil                            inz   like(FIMSP00F.MSPfil)
010100170209     d   MSPdtl                            inz   like(FIMSP00F.MSPdtl)
010200170209     d   MSPtur                            inz   like(FIMSP00F.MSPtur)
010300170209     d*//MSPimp                            inz   like(FIMSP00F.MSPimp)
010400170209     d   MSPhml                            inz   like(FIMSP00F.MSPhml)
010500170209     d   wTotPos                      3  0 inz
010600170209     d   MSPncl                     + 4    inz   like(FIMSP00F.MSPncl)
010700170217       //  ?... relativi ai 3 minuti precedenti?
010800170210     d FIMSP_P_ds      ds                  inz   likeds(FIMSP_ds)
010900170209       //  ?... relativi all'orario iniziale del turno picking?
011000170209     d FIMSP_I_ds      ds                  inz   likeds(FIMSP_ds)
011100170209       //  ?... relativi all'orario  finale  del turno picking?
011200170209     d FIMSP_F_ds      ds                  inz   likeds(FIMSP_ds)
011300170208
011400170208       //--------------------------------------------------------------
011500170208       //?Definizione variabili globali.                               ?
011600170208       //--------------------------------------------------------------
011700170208
011800170208       // -?Flags booleani?
011900170208     d $Fine           s               n   inz
012000170406     d $Eccezione      s               n   inz
012100170217
012200170217       // -?Indici di schiera?
012300170217     d xx              s              3  0 inz
012400170208
012500170208       // -?Stringa SQL da eseguire?
012600170209     d wSql            s          32740    inz   varying
012700170209
012800170209       // -?Data da elaborare?
012900170209     d wDate           s              8s 0 inz
013000170406     d wData_Iso       s               d   inz   datfmt(*iso)
013100170406     d wDay_Of_Week    s              2  0 inz
013200170209
013300170209       // -?Minuti in elaborazione?
013400170209     d wTime_I         s               t   inz   timfmt(*hms)
013500170209     d wTime_F         s               t   inz   timfmt(*hms)
013600170217
013700170217       // -?Colli in elaborazione?
013800170217     d wTotNCL         s                   inz   like(FIMSP_ds.MSPncl)
013900170407
014000170407       // -?Comodo?
014100170407     d wSaveFIL        s                   inz   like(FIMSP_ds.MSPfil)
014200170407     d wSaveDTL        s                   inz   like(FIMSP_ds.MSPdtl)
014300170407     d wSaveTUR        s                   inz   like(FIMSP_ds.MSPtur)
014400170208
014500170208       //--------------------------------------------------------------
014600170208       //?Definizione prototipi procedure usate.                       ?
014700170208       //--------------------------------------------------------------
014800170208
014900170208       // -?API QCAPCMD (Process Commands)?
015000170208     d Qcmd            s           2048    inz   varying
015100170208      /copy qSysInc/qRpgleSrc,QCAPCMD
015200170208      /copy gaitrasrc/srcProtoPR,QCAPCMD
015300170208       // -?Parametri gestione errori API.?
015400170208      /copy qsysinc/qRpgleSrc,QUSEC
015500170208
015600170208       //--------------------------------------------------------------
015700170208       //?Definizione key-list.                                        ?
015800170208       //--------------------------------------------------------------
015900170208
016000170406       // -?File FIMSE01L?
016100170406     d keyFIMSE01    e ds                  extname( FIMSE01L : *key )
016200170406     d                                     prefix( k_ )   inz
016300170208
016400170208       //--------------------------------------------------------------
016500170208       //?Riepilogo indicatori utilizzati.                             ?
016600170208       //--------------------------------------------------------------
016700170208
016800170208
016900170208       //--------------------------------------------------------------
017000170208       //?M A I N - L I N E                                            ?
017100170208       //--------------------------------------------------------------
017200170208
017300170208     c     *Entry        plist
017400170208     c                   parm                    KPJBA
017500170208
017600170208      /free
017700170208
017800170208       // -?Operazioni iniziali?
017900170208       exsr  sr_RoutInz;
018000170208
018100170208       // -?Preparazione stringa SQL?
018200170208       exsr  sr_PrepSQL;
018300170208
018400170209       // -?Apertura del cursore?
018500170208       exsr  sr_OpenCursor;
018600170208
018700170208       // -?Lettura del file?
018800170208       DoW  Not $Fine;
018900170208         exsr  sr_ReadCursor;
019000170208       EndDo;
019100170208
019200170208       // -?Chiusura del cursore?
019300170208       exsr  sr_CloseCursor;
019400170208
019500170208       // -?Operazioni finali?
019600170208       exsr  sr_RoutEnd;
019700170208
019800170208       //--------------------------------------------------------------
019900170208       //?Operazioni iniziali.                                         ?
020000170208       //--------------------------------------------------------------
020100170208       BEGSR  sr_RoutInz;
020200170208
020300170208         // -?Impostazione opzioni per SQL?
020400170208         exec sql   set option   DynUsrPrf = *Owner,
020500170208                                 CloSqlCsr = *EndMod;
020600170208
020700170208         // -?Impostazione chiusura?
020800170208         *inLR = *on;
020900170208
021000170208         // -?Ricezione Parametri?
021100170208         FIMS26ds = KPJBU;
021200170208         if  %check( c_Digits : %subst( KPJBU : 1 :
021300170208                                %size( FIMS26ds.prm_Fil ) ) )      > *zero;
021400170208           clear  FIMS26ds.prm_Fil;
021500170208         endif;
021600170208         if  %check( c_Digits : %subst( KPJBU :
021700170208                                %size( FIMS26ds.prm_Fil ) + 1 :
021800170208                                %size( FIMS26ds.prm_Time_Ini ) ) ) > *zero;
021900170208           clear  FIMS26ds.prm_Time_Ini;
022000170208         endif;
022100170208         if  %check( c_Digits : %subst( KPJBU :
022200170208                                %size( FIMS26ds.prm_Fil) +
022300170208                                %size( FIMS26ds.prm_Time_Ini ) + 1 :
022400170208                                %size( FIMS26ds.prm_Time_End ) ) ) > *zero;
022500170208           clear  FIMS26ds.prm_Time_End;
022600170208         endif;
022700170208
022800170208         //*·// -?Reperimento data e ora attuali?
022900170208         //*·wTime_ds = %subst( %char( %dec( %timestamp() ) )
023000170208         //*·                   : 1 : 14 );
023100170208
023200170208         // -?Impostazione data limite?
023300170209         wDate = %dec( %date() - %days(1) );
023400170208
023500170209         if  FIMS26ds.prm_Time_Ini > *zero  and
023600170209             FIMS26ds.prm_Time_End = *zero;
023700170209           FIMS26ds.prm_Time_End = ( wDate * 1000000 ) + 999999;
023800170208         endif;
023900170406
024000170406         // -?Ricerca di eventuali ECCEZIONI previste nel *file FIMSE00F?
024100170406         // ·?Per ogni eccezione trovata: i suoi dati vanno a sostituire?
024200170406         //  ?quelli che saranno raccolti (per Filiale/Turno)?
024300170406         if  FIMS26ds.prm_Time_Ini = *zero  and
024400170406             FIMS26ds.prm_Time_End = *zero;
024500170406           exsr  sr_CtrlFIMSE;
024600170406         else;
024700170406           wDate = ( FIMS26ds.prm_Time_Ini / 10000 );
024800170406           DoW  wDate * 10000 <= FIMS26ds.prm_Time_End;
024900170406             exsr  sr_CtrlFIMSE;
025000170406             wDate = %dec( %date( wDate : *iso ) + %days(1) );
025100170406           EndDo;
025200170406           wDate = %dec( %date() - %days(1) );
025300170406         endif;
025400170208
025500170208       ENDSR;
025600170406
025700170406       //--------------------------------------------------------------
025800170406       //?Ricerca di eventuali ECCEZIONI perviste nel *file FIMSE00F e ?
025900170406       //?scrittura record nel *file FIMSP10F (al di là delle letture  ?
026000170406       //?riportate in FIMSP00F).                                      ?
026100170406       //--------------------------------------------------------------
026200170406       BEGSR  sr_CtrlFIMSE;
026300170406
026400170406         // -?Ciclo di lettura di FIMSE00F per un singolo giorno?
026500170406         clear  keyFIMSE01;
026600170420         if  FIMS26ds.prm_Fil > *zero;
026700170420           k_MSEfil = FIMS26ds.prm_Fil;
026800170420         endif;
026900170406         setll  %kds( keyFIMSE01 )  FIMSE000;
027000170420         if  FIMS26ds.prm_Fil > *zero;
027100170420           readE  %kds( keyFIMSE01 : 1 )  FIMSE000;
027200170420         else;
027300170420           read   FIMSE000;
027400170420         endif;
027500170406
027600170406         DoW  NOT %eof( FIMSE01L );
027700170406
027800170406           If  ( wDate >= MSEdti  and
027900170406                 wDate <= MSEdtf );
028000170406
028100170406             // -?Reperimento giorno della settimana?
028200170406             //  ?(1=Lu/2=Ma/3=Me/4=Gi/5=Ve/6=Sa/7=Do)?
028300170406             wData_Iso = %date( wDate : *iso );
028400170406             clear wDay_Of_Week;
028500170406             exec sql   set   :wDay_Of_Week = DayOfWeek_Iso( :wData_Iso );
028600170406
028700170406             // -?Scrittura record Inizio/Fine per la singola eccezione?
028800170406             //  ?prevista nel giorno che si sta elaborando?
028900170406             if  wDay_Of_Week              > *zero  and
029000170406                 sk_MSEvs( wDay_Of_Week ) <> *blank;
029100170406               exsr  sr_WriteFIMSP_Eccezione;
029200170406             endif;
029300170406
029400170406           EndIf;
029500170406
029600170420           if  FIMS26ds.prm_Fil > *zero;
029700170420             readE  %kds( keyFIMSE01 : 1 )  FIMSE000;
029800170420           else;
029900170420             read  FIMSE000;
030000170420           endif;
030100170406
030200170406         EndDo;
030300170406
030400170406       ENDSR;
030500170406
030600170406       //--------------------------------------------------------------
030700170406       //?Scrittura dei 2 record in FIMSP10F (Inizio e Fine) per ogni  ?
030800170406       //?eccezione valida trovata in FIMSE00F (per i singoli Filiale, ?
030900170406       //?Turno e Data lettura).                                       ?
031000170406       //--------------------------------------------------------------
031100170406       BEGSR  sr_WriteFIMSP_Eccezione;
031200170406
031300170406         // -?Scrittura record di Avvio turno PKD?
031400170406         clear  FIMSP100;
031500170406         MSP1fil = MSEfil;
031600170406         MSP1dtl = wDate;
031700170406         MSP1tur = MSEtur;
031800170406         MSP1fif = 'I';
031900170406         MSP1hml = MSEhmi;
032000170410         //*·MSP1tim = %timestamp();
032100170410         MSP1tim = %timestamp( %date() );
032200170406         //______________
032300170406         write  FIMSP100;
032400170406         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
032500170406
032600170406         // -?Scrittura record di Fine turno PKD?
032700170406         clear  FIMSP100;
032800170406         MSP1fil = MSEfil;
032900170406         MSP1dtl = wDate;
033000170406         MSP1tur = MSEtur;
033100170406         MSP1fif = 'F';
033200170406         MSP1hml = MSEhmf;
033300170410         //*·MSP1tim = %timestamp();
033400170410         MSP1tim = %timestamp( %date() );
033500170406         //______________
033600170406         write  FIMSP100;
033700170406         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
033800170406
033900170406       ENDSR;
034000170208
034100170208       //--------------------------------------------------------------
034200170208       //?Preparazione stringa SQL per estrazione dati da FLTR700F.    ?
034300170208       //--------------------------------------------------------------
034400170208       BEGSR  sr_PrepSQL;
034500170208
034600170208         wSQL =  'select MSPfil, MSPdtl, MSPtur, MSPhml, +
034700170208                         count(*), sum(MSPncl) +
034800170208                    from FIMSP00F +
034900170320                   where MSPtur <> ''N'' +
035000170503                     and MSPimp not in (''I'', ''W'')';
035100170208
035200170208         if  FIMS26ds.prm_Fil > *zero;
035300170208
035400170210           wSQL += ' and MSPfil = ' + %editc( FIMS26ds.prm_Fil : 'X' );
035500170208
035600170208         endif;
035700170208
035800170208         if  FIMS26ds.prm_Time_Ini > *zero  or
035900170208             FIMS26ds.prm_Time_End > *zero;
036000170208
036100170210           wSQL += ' and ( ( MSPdtl * 10000 ) + MSPhml ) +
036200170210                         between ' +
036300170210                           %char( FIMS26ds.prm_Time_Ini ) +
036400170210                           ' and ' +
036500170210                           %char( FIMS26ds.prm_Time_End );
036600170208
036700170208         else;
036800170208
036900170208           wSQL += ' and MSPdtl = ' + %editc( wDate : 'X' );
037000170208
037100170208         endif;
037200170210
037300170210         if  FIMS26ds.prm_Turno <> *blank;
037400170210
037500170210           wSQL += ' and MSPtur = ''' + FIMS26ds.prm_Turno + '''';
037600170210
037700170210         endif;
037800170210
037900170210         if  FIMS26ds.prm_Impianto <> *blank;
038000170210
038100170210           wSQL += ' and MSPimp = ''' + FIMS26ds.prm_Impianto + '''';
038200170210
038300170210         endif;
038400170208
038500170208         wSQL += ' group by MSPfil, MSPdtl, MSPtur, MSPhml +
038600170209                  having count(*) >= ' + c_NrMinimo_Banchi +
038700170209                 ' order by MSPfil, MSPdtl, MSPtur, MSPhml +
038800170208                     for fetch only';
038900170208
039000170208       ENDSR;
039100170208
039200170208       //--------------------------------------------------------------
039300170208       //?Apertura cursore.                                            ?
039400170208       //--------------------------------------------------------------
039500170208       BEGSR  sr_OpenCursor;
039600170208
039700170208         // -?Dichiarazione cursore?
039800170208         exec sql   prepare S1   from :wSQL;
039900170208         exec sql   declare C1   cursor for S1;
040000170208
040100170208         if  SQLcode < *zero;
040200170208           $Fine = *on;
040300170208           exsr  sr_PrintERR;
040400170208         endif;
040500170208
040600170208         // -?Apertura del cursore?
040700170208         exec sql   open C1;
040800170208
040900170208         if  SQLcode < *zero;
041000170208           $Fine = *on;
041100170208           exsr  sr_PrintERR;
041200170208         endif;
041300170208
041400170208       ENDSR;
041500170208
041600170208       //--------------------------------------------------------------
041700170208       //?Lettura cursore.                                             ?
041800170208       //--------------------------------------------------------------
041900170208       BEGSR  sr_ReadCursor;
042000170208
042100170208         $Fine = *off;
042200170209         clear  FIMSP_ds;
042300170208
042400170208         exec sql   fetch next   from C1   into :FIMSP_ds;
042500170208
042600170208         select;
042700170208           when  SQLcode = 100;
042800170208             exsr  sr_Write_FIMSP10F;
042900170208             $Fine = *on;
043000170208           when  SQLcode < *zero;
043100170208             $Fine = *on;
043200170208             exsr  sr_PrintERR;
043300170208           other;
043400170208             exsr  sr_Elab_FIMSP00F;
043500170208         endsl;
043600170208
043700170208       ENDSR;
043800170208
043900170208       //--------------------------------------------------------------
044000170208       //?Chiusura cursore.                                            ?
044100170208       //--------------------------------------------------------------
044200170208       BEGSR  sr_CloseCursor;
044300170208
044400170208         // -?Chiusura del cursore?
044500170208         exec sql   close C1;
044600170208
044700170208       ENDSR;
044800170208
044900170208       //--------------------------------------------------------------
045000170209       //?Controllo dati appena estratti da FIMSP00F.                  ?
045100170208       //--------------------------------------------------------------
045200170209       //?0) Ogni minuto devono essere elaborati almeno 2 banchi       ?
045300170209       //?   (ma questo filtro viene già eseguito via SQL).            ?
045400170209       //?1) I dati in input devono essere successivi agli ultimi letti?
045500170405       //?   di SOLO 1 minuto.                                         ?
045600170209       //?   In caso di differenza superiore: viene cancellata la let- ?
045700170209       //?   tura del 1° minuto e reimpostata con quella in input.     ?
045800170412       //?2) L'orario delle letture in input dev'essere successivo di  ?
045900170412       //?   solo 1 minuto a quello precedente, e devono risultare al- ?
046000170414       //?   meno 4 minuti (vedi costante "c_NrMinimo_MinutiConsec")   ?
046100170414       //?   consecutivi.                                              ?
046200170412       //?   In caso di "durata" inferiore viene cancellata la lettura ?
046300170412       //?   del 1° minuto e reimpostata con quella del minuto succes- ?
046400170412       //?   sivo.                                                     ?
046500170217       //?3) In questi 4 minuti (vedi costante "c_NrMinimo_MinutiConsec")?
046600170217       //?   devono risultare elaborati almeno 60 colli (vedi costante ?
046700170217       //?   "c_NrMinimo_Colli").                                      ?
046800170217       //?   In caso di sommatoria inferiore: viene rimossa la lettura ?
046900170412       //?   del 1° minuto e reimpostata con quella del minuto succes- ?
047000170412       //?   sivo).                                                    ?
047100170209       //--------------------------------------------------------------
047200170209       BEGSR  sr_Elab_FIMSP00F;
047300170209
047400170414         // -?"Rottura" di Filiale/Data/Turno?
047500170414         IF  ( FIMSP_ds.MSPfil <> wSaveFIL  or
047600170414               FIMSP_ds.MSPdtl <> wSaveDTL  or
047700170414               FIMSP_ds.MSPtur <> wSaveTUR );
047800170414
047900170414           wSaveFIL = FIMSP_ds.MSPfil;
048000170414           wSaveDTL = FIMSP_ds.MSPdtl;
048100170414           wSaveTUR = FIMSP_ds.MSPtur;
048200170414
048300170414
048400170414           // -?Eventuale scrittura record a rottura di Filiale/Data/Turno?
048500170414           //  ?e   Memorizzaz. dei dati relativi al nuovo Avvio turno PKD?
048600170414           If  FIMSP_I_ds.MSPfil <> *zero             and
048700170414               ( FIMSP_ds.MSPfil <> FIMSP_I_ds.MSPfil  or
048800170414                 FIMSP_ds.MSPdtl <> FIMSP_I_ds.MSPdtl  or
048900170414                 FIMSP_ds.MSPtur <> FIMSP_I_ds.MSPtur );
049000170414             exsr  sr_Write_FIMSP10F;
049100170414           EndIf;
049200170414
049300170414
049400170414           clear  FIMSP_I_ds;
049500170414           clear  FIMSP_F_ds;
049600170414           clear  FIMSP_P_ds;
049700170414           clear  sk_FIMSP_P;
049800170406
049900170406
050000170414           // -?Reperimento giorno della settimana in elaborazione?
050100170414           //  ?(1=Lu/2=Ma/3=Me/4=Gi/5=Ve/6=Sa/7=Do)?
050200170414           wData_Iso = %date( FIMSP_ds.MSPdtl : *iso );
050300170414           clear wDay_Of_Week;
050400170414           exec sql   set   :wDay_Of_Week = DayOfWeek_Iso( :wData_Iso );
050500170414
050600170414           // -?SE filiale/turno previsti come "eccezione" (in FIMSE00F)?
050700170414           //  ?inizio/fine sono già stati inseriti in FIMSP10F  e questi?
050800170414           //  ?dati NON sono da elaborare?
050900170414           clear  $Eccezione;
051000170414           clear  keyFIMSE01;
051100170414           k_MSEfil = FIMSP_ds.MSPfil;
051200170414           k_MSEtur = FIMSP_ds.MSPtur;
051300170414           k_MSEdti = FIMSP_ds.MSPdtl;
051400170414           setll  %kds( keyFIMSE01 )  FIMSE000;
051500170414           reade  %kds( keyFIMSE01 : 2 )  FIMSE000;
051600170414
051700170414           DoW  Not %eof( FIMSE01L )  and  wDay_Of_Week > *zero;
051800170414
051900170414             // -?Scrittura record Inizio/Fine già fatta per la singola?
052000170414             //  ?eccezione e per il giorno che si sta elaborando?
052100170414             if  FIMSP_ds.MSPdtl >= MSEdti  and
052200170414                 FIMSP_ds.MSPdtl <= MSEdtf  and
052300170414                 sk_MSEvs( wDay_Of_Week ) <> *blank;
052400170414               $Eccezione = *on;
052500170414               leave;
052600170414             endif;
052700170414
052800170414             reade  %kds( keyFIMSE01 : 2 )  FIMSE000;
052900170414
053000170414           EndDo;
053100170414
053200170414
053300170414           // -?Memorizzazione dati del 1° minuto per?
053400170414           //  ?la nuova filiale o il nuovo giorno o il nuovo turno?
053500170414           //  ?(SE non è stata rilevata come "eccezione")?
053600170414           if  NOT $Eccezione;
053700170414             FIMSP_P_ds = FIMSP_ds;
053800170414             sk_FIMSP_P(1) = FIMSP_ds;
053900170414             leavesr;
054000170414           endif;
054100170406
054200170414         ENDIF;
054300170414
054400170406
054500170414         // -?Si scartano le "eccezioni" - già inserite in FIMSP10F?
054600170406         if  $Eccezione;
054700170406           leavesr;
054800170406         endif;
054900170209
055000170209
055100170217         clear  FIMSP_P_ds;
055200170217         if  sk_FIMSP_P(1) <> *blank;
055300170217           FIMSP_P_ds = sk_FIMSP_P(1);
055400170217         endif;
055500170405
055600170217
055700170209         // -?1) Controllo sequenza dei minuti in lettura?
055800170210
055900170210         // -?Non ancora elaborato il minuto precedente:?
056000170210         //  ?sarà questo!?
056100170210         if  FIMSP_P_ds.MSPhml = *zero;
056200170210           FIMSP_P_ds = FIMSP_ds;
056300170217           clear  sk_FIMSP_P;
056400170217           sk_FIMSP_P(1) = FIMSP_ds;
056500170210           leavesr;
056600170210         endif;
056700170209
056800170210         // -?Impostazione del minuto iniziale (quello precedente)?
056900170210         wTime_I = %time( FIMSP_P_ds.MSPhml * 100 : *hms );
057000170209         // -?Impostazione del minuto finale (appena letto)?
057100170209         wTime_F = %time( FIMSP_ds.MSPhml * 100 : *hms );
057200170209
057300170217         // -?SE la differenza con la lettura precedente è oltre 1 minuto?
057400170217         //  ? & si sta cercando ancora il minuto Finale (e magari anche?
057500170217         //  ?   quello iniziale):?
057600170210         //  ?=> Si re-imposta la lettura in input come precedente?
057700170217         //  ? e si riparte da essa?
057800170217         if  wTime_I + %minutes(1) < wTime_F  and
057900170217             FIMSP_F_ds.MSPfil     = *zero;
058000170210           FIMSP_P_ds = FIMSP_ds;
058100170217           clear  FIMSP_I_ds;
058200170217           clear  FIMSP_F_ds;
058300170217           clear  sk_FIMSP_P;
058400170217           sk_FIMSP_P(1) = FIMSP_ds;
058500170217           leavesr;
058600170210         endif;
058700170209
058800170209
058900170210         // -?Impostazione dati relativi all'Avvio turno PKD?
059000170210         //  ?(se non ancora impostati)?
059100170210         if  FIMSP_I_ds.MSPfil = *zero;
059200170210           FIMSP_I_ds = FIMSP_P_ds;
059300170210         endif;
059400170217
059500170217         // -?Aggiornamento del n° colli letti in questi minuti?
059600170217         FIMSP_I_ds.MSPncl += FIMSP_ds.MSPncl;
059700170210
059800170217
059900170217         // -?Salvataggio dati degli ultimi minuti elaborati in?
060000170217         //  ?precedenza per controllare il minuto successivo?
060100170217         For  xx = c_NrMinimo_MinutiConsec  DownTo 2;
060200170217           sk_FIMSP_P( xx ) = sk_FIMSP_P( xx - 1 );
060300170217         EndFor;
060400170217
060500170217         // -?Salvataggio dati del minuto in corso di elaborazione?
060600170217         sk_FIMSP_P( 1 ) = FIMSP_ds;
060700170217         FIMSP_P_ds      = FIMSP_ds;
060800170217
060900170217
061000170217         // -?2) Controllo del numero minimo di minuti consecutivi?
061100170217         //     ?delle ultime letture?
061200170217
061300170217         // -?Impostazione dell'orario iniziale (già memorizzato)?
061400170217         wTime_I = %time( FIMSP_I_ds.MSPhml * 100 : *hms );
061500170217         // -?Impostazione dell'orario finale (appena letto)?
061600170217         wTime_F = %time( FIMSP_ds.MSPhml * 100 : *hms );
061700170217
061800170217         // -?SE la differenza tra le 2 letture è inferiore a 3 minuti?
061900170217         //  ?(che sono per forza consecutivi - vedi 1° controllo)?
062000170217         //  ?=> Non si memorizza la lettura in input come ultima?
062100170217         if  ( wTime_I + %minutes(c_NrMinimo_MinutiConsec)
062200170217                       - %minutes(1) ) > wTime_F;
062300170217           leavesr;
062400170217         endif;
062500170217
062600170217
062700170217         // -?3) Controllo del numero minimo di colli letti, finora?
062800170217         //     ?elaborati (negli ultimi almeno 4 minuti consecutivi)?
062900170217
063000170217         // -?SE la somma dei colli delle ultime almeno 4 letture?
063100170217         //  ?è inferione a 60:?
063200170217         //  ?=> Non si memorizza la lettura in input come Finale?
063300170217         //  ?   e   si memorizza la prima delle ultime memorizzate?
063400170217         //  ?       come quella di Avvio turno PKD?
063500170217         IF  FIMSP_I_ds.MSPncl < c_NrMinimo_Colli;
063600170217
063700170217           // -?Si ricerca il minuto successivo a quello attualmente?
063800170217           //  ?memorizzato?
063900170217           For  xx = c_NrMinimo_MinutiConsec  DownTo 1;
064000170217
064100170217             FIMSP_P_ds = sk_FIMSP_P(xx);
064200170217
064300170217             If  FIMSP_P_ds.MSPfil > *zero  and
064400170217                 FIMSP_P_ds.MSPhml > FIMSP_I_ds.MSPhml;
064500170217
064600170217               FIMSP_I_ds = FIMSP_P_ds;
064700170217               // -?Quando trovato: si sommano i colli lavorati da quel?
064800170217               //  ?minuto, fino a quello attuale?
064900170217               xx -= 1;
065000170217               doW  xx >= 1;
065100170217                 FIMSP_P_ds = sk_FIMSP_P(xx);
065200170217                 FIMSP_I_ds.MSPncl += FIMSP_P_ds.MSPncl;
065300170217                 xx -= 1;
065400170217               enddo;
065500170217
065600170217               leave;
065700170217
065800170217             EndIf;
065900170217
066000170217           EndFor;
066100170217
066200170217           leavesr;
066300170217
066400170217         ENDIF;
066500170217
066600170217
066700170217         // -?Impostazione dati relativi alla Fine turno PKD?
066800170217         //  ?Anche la lettura Finale deve avere 4 minuti di letture?
066900170217         //  ?consecutive:  SE la differenza tra ciascuna delle letture?
067000170217         //  ?precedenti è oltre 1 minuto => NON si imposta la lettura?
067100170217         //  ?in input come finale?
067200170217
067300170217         // -?Controllo minuto della Fine turno PKD?
067400170217         // -?Impostazione del minuto finale (appena letto) - in esame?
067500170217         wTime_F = %time( FIMSP_ds.MSPhml * 100 : *hms );
067600170217         // -?Impostazione del totale colli letti negli ultimi minuti?
067700170217         wTotNCL = FIMSP_ds.MSPncl;
067800170217
067900170217         For  xx = 2  To c_NrMinimo_MinutiConsec;
068000170217
068100170217           // -?Impostazione del minuto precedente quello in esame?
068200170217           FIMSP_P_ds = sk_FIMSP_P(xx);
068300170217           wTime_I = %time( FIMSP_P_ds.MSPhml * 100 : *hms );
068400170217
068500170217           // -?Se differente di ultre 1 minuto:?
068600170217           //  ?NON si memorizza l'orario di Fine turno?
068700170217           if  wTime_I + %minutes(xx-1) <> wTime_F;
068800170217             leavesr;
068900170217           endif;
069000170217
069100170217           // -?Aggiornamento del n° colli letti in questi minuti?
069200170217           wTotNCL += FIMSP_P_ds.MSPncl;
069300170217
069400170217         EndFor;
069500170217
069600170217         if  wTotNCL >= c_NrMinimo_Colli;
069700170217           FIMSP_F_ds = FIMSP_ds;
069800170217         endif;
069900170208
070000170208       ENDSR;
070100170208
070200170208       //--------------------------------------------------------------
070300170208       //?Scrittura del record nel *file FIMSP10F.                     ?
070400170208       //--------------------------------------------------------------
070500170208       BEGSR  sr_Write_FIMSP10F;
070600170208
070700170209         // -?Nessuna lettura consecutiva reperita dopo l'ultima?
070800170209         //  ?memorizzata come iniziale?
070900170209         // ·?Questo controllo è meglio farlo all'inizio di questa?
071000170209         //  ?subroutine perchè essa stessa viene eseguita in più punti?
071100170209         //  ?del programma?
071200170209         if  FIMSP_F_ds.MSPfil = *zero;
071300170208           leavesr;
071400170208         endif;
071500170208
071600170209         // -?Scrittura record di Avvio turno PKD?
071700170209         clear  FIMSP100;
071800170209         MSP1fil = FIMSP_I_ds.MSPfil;
071900170209         MSP1dtl = FIMSP_I_ds.MSPdtl;
072000170209         MSP1tur = FIMSP_I_ds.MSPtur;
072100170208         MSP1fif = 'I';
072200170209         MSP1hml = FIMSP_I_ds.MSPhml;
072300170209         MSP1tim = %timestamp();
072400170208         //______________
072500170209         write  FIMSP100;
072600170208         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
072700170208
072800170209         // -?Scrittura record di Fine turno PKD?
072900170210         clear  FIMSP100;
073000170210         MSP1fil = FIMSP_F_ds.MSPfil;
073100170210         MSP1dtl = FIMSP_F_ds.MSPdtl;
073200170210         MSP1tur = FIMSP_F_ds.MSPtur;
073300170208         MSP1fif = 'F';
073400170209         MSP1hml = FIMSP_F_ds.MSPhml;
073500170209         MSP1tim = %timestamp();
073600170208         //______________
073700170209         write  FIMSP100;
073800170208         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
073900170208
074000170208       ENDSR;
074100170208
074200170208       //--------------------------------------------------------------
074300170208       //?Stampa segnalazione dell'errore rilevato via SQL             ?
074400170208       //--------------------------------------------------------------
074500170208       BEGSR  sr_PrintERR;
074600170208
074700170208         // -?Stampa del Dump?
074800170208         Dump(A);
074900170208
075000170208         // -?Stampa del Job-Log?
075100170208         Qcmd = 'DSPJOBLOG job(*) output(*print)';
075200170208         exsr  sr_ExecCmd;
075300170208
075400170208         // -?Chiusura del programma?
075500170208         exsr  sr_RoutEnd;
075600170208
075700170208       ENDSR;
075800170208
075900170208       //--------------------------------------------------------------
076000170208       //?Esecuzione del comando (già impostato).                      ?
076100170208       //--------------------------------------------------------------
076200170208       BEGSR  sr_ExecCmd;
076300170208
076400170208         clear Qcap0100;
076500170208         Qcabcsdh = *off;
076600170208         Qcapa    = *off;
076700170208         Qcacmdss = *off;
076800170208         Qcaerved = *allX'00';
076900170208
077000170208         clear Qusec;
077100170208         Qusbprv  = %size(Qusec);
077200170208
077300170208         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
077400170208                           %size(Qcap0100) : 'CPOP0100' : *omit :
077500170208                           0 : 0 : Qusec);
077600170208
077700170208         //*·if  Qusei <> *blank;
077800170208         //*·  ...;
077900170208         //*·endif;
078000170208
078100170208       ENDSR;
078200170208
078300170208       //--------------------------------------------------------------
078400170208       //?Operazioni finali.                                           ?
078500170208       //--------------------------------------------------------------
078600170208       BEGSR  sr_RoutEnd;
078700170208
078800170208         // -?Uscita dal *pgm?
078900170208         return;
079000170208
079100170208       ENDSR;
079200170208
079300170208      /end-free
