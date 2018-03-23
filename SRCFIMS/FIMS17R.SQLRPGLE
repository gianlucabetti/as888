000100170605       //==============================================================
000200170605       //?FiMS17R - Statistica Smistamento Merce Incompatibile: crea   ?
000300170605       //?          *WrkF WFMIT10F: Merce Incompatibile - Totale per   ?
000400170605       //?          Data_Spunta/Filiale/Categoria_FV/Postazione.       ?
000500170605       //==============================================================
000600170605
000700170605       //--------------------------------------------------------------
000800170605       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000900170605       //--------------------------------------------------------------
001000170605
001100170605     /*PRM dbgview(*source)
001200170605     /*END
001300170605
001400170605       //--------------------------------------------------------------
001500170605       //?Specifiche di controllo.                                     ?
001600170605       //--------------------------------------------------------------
001700170605
001800170605     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001900170605     h alwnull(*inputonly)
002000170605     h dftactgrp(*no)
002100170605
002200170605       //--------------------------------------------------------------
002300170605       //?Dichiarazione file.                                          ?
002400170605       //--------------------------------------------------------------
002500170605
002600170605       // -?BOLLE: Dettaglio Segnacolli?
002700170605     fFIART27L  if   e           k disk
002800170605
002900170605       // -?BOLLE ARRIVI: Testata?
003000170605     fFNARB01L  if   e           k disk
003100170605
003200170605       // -?Merce Incompatibile: Spedizioni?
003300170605     fWFMIT10F  o    e             disk
003400170605
003500170605       //--------------------------------------------------------------
003600170605       //?Definizione costanti.                                        ?
003700170605       //--------------------------------------------------------------
003800170605
003900170605       // -?Costante per controllo "caratteri solo numerici"?
004000170605     d c_Digits        c                   const('0123456789')
004100170605
004200170605       //--------------------------------------------------------------
004300170605       //?Definizione schiere.                                         ?
004400170605       //--------------------------------------------------------------
004500170605
004600170605
004700170605       //--------------------------------------------------------------
004800170605       //?Definizione aree dati.                                       ?
004900170605       //--------------------------------------------------------------
005000170605
005100170605       // -?Dati utente?
005200170605     d §AzUte        e ds                  extname(AZUTE00F)
005300170605     d                                     dtaara
005400170605     d §DatiUte      e ds                  extname(dDatiUte)
005500170605     d                                     dtaara
005600170605
005700170605       //--------------------------------------------------------------
005800170605       //?Definizione strutture dati.                                  ?
005900170605       //--------------------------------------------------------------
006000170605
006100170605       // -?Status ds?
006200170605     d Status         sds
006300170605     d   SDSpgmName      *proc
006400170605     d*//SDSdta              191    198
006500170605     d*//SDSjob              244    253
006600170605     d*//SDSusr              254    263
006700170605
006800170605       // -?Parametri ricevuti?
006900170605     d KPJBA         e ds
007000170605     d FIMS17ds        ds                  inz   qualified
007100170605     d   iMS17fil                     3s 0 inz
007200170605     d   iMS17dti                     8s 0 inz
007300170605     d   iMS17dtf                     8s 0 inz
007400170605     d   iMS17npg                     1s 0 inz
007500170605
007600170605       // -?Dati estratti via SQL?
007700170605     d FIMIP_ds      e ds                  extname(FIMIP00F)
007800170605     d                                     inz
007900170605
008000170605     d FIPSL00F      e ds                  based(nullPtr)
008100170605     d                                     qualified
008200170605     d wSQL_FIPSL_ds   ds                  inz   qualified
008300170605     d   pslFIL                            inz   like(FIPSL00F.PSLfil)
008400170605     d   pslNPG                            inz   like(FIPSL00F.PSLnpg)
008500170605     d   pslUSR                            inz   like(FIPSL00F.PSLusr)
008600170605     d   pslDToraR                         inz   like(FIPSL00F.PSLdtOraR)
008700170605     d   pslBARco                          inz   like(FIPSL00F.PSLbarCo)
008800170605
008900170605     d FLTR700F      e ds                  based(nullPtr)
009000170605     d                                     qualified
009100170605     d wSQL_FLTR7_ds   ds                  inz   qualified
009200170605     d   tr7FIL                            inz   like(FLTR700F.TR7fil)
009300170605     d   tr7NPG                            inz   like(FLTR700F.TR7ctl)
009400170605     d   tr7TIM                            inz   like(FLTR700F.TR7tim)
009500170605     d   tr7PEP                            inz   like(FLTR700F.TR7pep)
009600170605     d   tr7BAR                            inz   like(FLTR700F.TR7bar)
009700170605
009800170605       // -?BarCode Segnacollo?
009900170605     d wBarCode        ds            18    inz   qualified
010000170605     d   wBar_LNP                     3s 0 inz
010100170605     d   wBar_LNA                     3s 0 inz
010200170605     d   wBar_NRS                     2s 0 inz
010300170605     d   wBar_NSC                     7s 0 inz
010400170605     d   wBar_ZNC                     2s 0 inz
010500170605     d   wBar_CHK                     1s 0 inz
010600170605
010700170605       // -?Tabelle?
010800170605     d ds3A          e ds                  inz   qualified
010900170605
011000170605       //--------------------------------------------------------------
011100170605       //?Definizione variabili globali.                               ?
011200170605       //--------------------------------------------------------------
011300170605
011400170605       // -?Flags booleani?
011500170605     d $EoF            s               n   inz
011600170605     d $EoF2           s               n   inz
011700170605     d $EoF3           s               n   inz
011800170605
011900170605       // -?Stringhe SQL da eseguire?
012000170605     d wSQL            s          32740    inz   varying
012100170605     d wSQL2           s          32740    inz   varying
012200170605     d wSQL3           s          32740    inz   varying
012300170605
012400170605       // -?Data/Ora limite (dati alfanumerici)?
012500170605     d wDataOraIni     s             14    inz
012600170605     d wDataOraFin     s             14    inz
012700170605
012800170605      *//· // -?Data in formato *EUR?
012900170605     d*//· wDate_Eur       s               d   inz   datfmt(*EUR)
013000170605
013100170605       // -?Campi di Comodo?
013200170605     d wDateMax        s              8  0 inz
013300170605     d wDate           s              8  0 inz
013400170605     d*//· wTime           s              6  0 inz
013500170605
013600170605       //--------------------------------------------------------------
013700170605       //?Definizione prototipi procedure.                             ?
013800170605       //--------------------------------------------------------------
013900170605
014000170605       // -?Reperimento dati utente?
014100170605     d TIBS34ds      e ds                  inz
014200170605      /copy gaitrasrc/srcProtoPR,TIBS34R
014300170605
014400170605       // -?Parametri API QCAPCMD (Process Commands)?
014500170605     d Qcmd            s           2048    inz   varying
014600170605      /copy qSysInc/qRpgleSrc,QCAPCMD
014700170605       // -?API QCAPCMD (Process Commands)?
014800170605      /copy gaitrasrc/srcProtoPR,QCAPCMD
014900170605
015000170605       // -?Parametri gestione errori API.?
015100170605      /copy qsysinc/qrpglesrc,QUSEC
015200170605
015300170605       //--------------------------------------------------------------
015400170605       //?Definizione key-list.                                        ?
015500170605       //--------------------------------------------------------------
015600170605
015700170605       // -?File FIART27L?
015800170605     d keyFIART27    e ds                  extname( FIART27L : *key )
015900170605     d                                     prefix( k_ )  inz
016000170605
016100170605       // -?File FNARB01L?
016200170605     d keyFNARB01    e ds                  extname( FNARB01L : *key )
016300170605     d                                     prefix( k_ )  inz
016400170605
016500170605       //--------------------------------------------------------------
016600170605       //?Riepilogo indicatori utilizzati.                             ?
016700170605       //--------------------------------------------------------------
016800170605
016900170605
017000170605       //--------------------------------------------------------------
017100170605       //?M A I N - L I N E                                            ?
017200170605       //--------------------------------------------------------------
017300170605
017400170605     c     *Entry        plist
017500170605     c                   parm                    KPJBA
017600170605
017700170605      /free
017800170605
017900170605       // -?Operazioni iniziali?
018000170605       exsr  sr_RoutInz;
018100170605
018200170605       // -?Preparazione stringa SQL per estrazione dati?
018300170605       exsr  sr_PrepSQL;
018400170605
018500170605       // -?Apertura Cursore?
018600170605       exsr  sr_OpenCursor;
018700170605
018800170605       // -?Ciclo di elaborazione?
018900170605       DoW  $EoF = *off;
019000170605         exsr  sr_ReadCursor;
019100170605       EndDo;
019200170605
019300170605       // -?Chiusura Cursore?
019400170605       exsr  sr_CloseCursor;
019500170605
019600170605       // -?Operazioni finali?
019700170605       exsr sr_RoutEnd;
019800170605
019900170605       //--------------------------------------------------------------
020000170605       //?Operazioni iniziali.                                         ?
020100170605       //--------------------------------------------------------------
020200170605       BEGSR  sr_RoutInz;
020300170605
020400170605         // -?Impostazione opzioni per SQL?
020500170605         exec sql   set  option  DynUsrPrf = *Owner,
020600170605                                 CloSqlCsr = *EndMod;
020700170605
020800170605         // -?Impostazione chiusura?
020900170605         *inLR = *on;
021000170605
021100170605         // -?Reperimento dati job?
021200170605         exsr  sr_DatiJob;
021300170605
021400170605         // -?Reperimento parametri?
021500170605         clear  FIMS17ds;
021600170605         If  KPJBU <> *blank;
021700170605           FIMS17ds = KPJBU;
021800170605           if  %check( c_Digits : %subst( KPJBU : 1 :
021900170605                                  %size( FIMS17ds.iMS17fil ) ) )      > *zero;
022000170605             clear  FiMS17ds.iMS17fil;
022100170605           endif;
022200170605           if  %check( c_Digits : %subst( KPJBU :
022300170605                                  %size( FiMS17ds.iMS17fil ) + 1 :
022400170605                                  %size( FiMS17ds.iMS17dti ) ) ) > *zero;
022500170605             clear  FiMS17ds.iMS17dti;
022600170605           endif;
022700170605           if  %check( c_Digits : %subst( KPJBU :
022800170605                                  %size( FiMS17ds.iMS17fil ) +
022900170605                                  %size( FiMS17ds.iMS17dti ) + 1 :
023000170605                                  %size( FiMS17ds.iMS17dtf ) ) ) > *zero;
023100170605             clear  FiMS17ds.iMS17dtf;
023200170605           endif;
023300170605           if  %check( c_Digits : %subst( KPJBU :
023400170605                                  %size( FiMS17ds.iMS17fil ) +
023500170605                                  %size( FiMS17ds.iMS17dti ) +
023600170605                                  %size( FiMS17ds.iMS17dtf ) + 1 :
023700170605                                  %size( FiMS17ds.iMS17npg ) ) ) > *zero;
023800170605             clear  FiMS17ds.iMS17npg;
023900170605           endif;
024000170605           if  FiMS17ds.iMS17dti > *zero  and
024100170605               FiMS17ds.iMS17dtf = *zero;
024200170605             FiMS17ds.iMS17dtf = FiMS17ds.iMS17dti;
024300170605           endif;
024400170605         EndIf;
024500170605
024600170605         //*·// -?Reperimento data odierna in formato gg/mm/aaaa?
024700170605         //*·wDate_Eur = %date();
024800170605         //*·wDate = %dec( wDate_Eur );
024900170605         //*·
025000170605         //*·// -?Reperimento orario?
025100170605         //*·wTime = %dec( %time() );
025200170605
025300170605         // -?Impostazione data limite?
025400170605         wDateMax = %dec( %date() - %days(1) );
025500170605
025600170605       ENDSR;
025700170605
025800170605       //--------------------------------------------------------------
025900170605       //?Reperimento Dati del job (Utente/Operativi).                 ?
026000170605       //--------------------------------------------------------------
026100170605       BEGSR  sr_DatiJob;
026200170605
026300170605         in(e)  §AzUte;
026400170605         if  NOT  %error;
026500170605           in(e)  §DatiUte;
026600170605         endif;
026700170605         if  %error  or  RSut = *blank;
026800170605           tibs34r ( tibs34ds );
026900170605           in  §AzUte;
027000170605           in  §DatiUte;
027100170605         endif;
027200170605
027300170605       ENDSR;
027400170605
027500170605       //--------------------------------------------------------------
027600170605       //?Preparazione stringa SQL - per estrazione dati da FIMIP.     ?
027700170605       //--------------------------------------------------------------
027800170605       BEGSR  sr_PrepSQL;
027900170605
028000170605         wSQL  = 'select MIPfil, MIPnpg, MIPtip, MIPpep, +
028100170605                         MIPdti, MIPdtf, MIPhmi, MIPhmf +
028200170605                    from FIMIP00F';
028300170605
028400170605         if  FiMS17ds.iMS17fil > *zero;
028500170605           wSQL +=
028600170605                 ' where MIPfil = ' + %editc( FiMS17ds.iMS17fil : 'X' );
028700170605         endif;
028800170605
028900170605         wSQL += ' order by MIPfil, MIPnpg, MIPtip, MIPpep, +
029000170605                            MIPdti, MIPdtf, MIPhmi, MIPhmf +
029100170605                     for fetch only';
029200170605
029300170605       ENDSR;
029400170605
029500170605       //--------------------------------------------------------------
029600170605       //?Apertura cursore.                                            ?
029700170605       //--------------------------------------------------------------
029800170605       BEGSR  sr_OpenCursor;
029900170605
030000170605         $EoF  = *off;
030100170605
030200170605         // -?Dichiarazione cursore?
030300170605         exec sql   prepare S1   from :wSQL;
030400170605         exec sql   declare C1   cursor for S1;
030500170605
030600170605         if  SQLcode < *zero;
030700170605           $EoF  = *on;
030800170605           exsr  sr_PrintERR;
030900170605           leavesr;
031000170605         endif;
031100170605
031200170605         // -?Apertura del cursore?
031300170605         exec sql   open C1;
031400170605
031500170605         if  SQLcode < *zero;
031600170605           $EoF  = *on;
031700170605           exsr  sr_PrintERR;
031800170605           leavesr;
031900170605         endif;
032000170605
032100170605       ENDSR;
032200170605
032300170605       //--------------------------------------------------------------
032400170605       //?Chiusura cursore.                                            ?
032500170605       //--------------------------------------------------------------
032600170605       BEGSR  sr_CloseCursor;
032700170605
032800170605         // -?Chiusura del cursore?
032900170605         exec sql   close C1;
033000170605
033100170605       ENDSR;
033200170605
033300170605       //--------------------------------------------------------------
033400170605       //?Lettura cursore.                                             ?
033500170605       //--------------------------------------------------------------
033600170605       BEGSR  sr_ReadCursor;
033700170605
033800170605         clear  FIMIP_ds;
033900170605
034000170605         exec sql   fetch next   from C1   into :FIMIP_ds;
034100170605
034200170605         Select;
034300170605
034400170605           // -?E.o.F.?
034500170605           When  SQLcode = 100;
034600170605             $EoF = *on;
034700170605
034800170605           // -?Errore?
034900170605           When  SQLcode < *zero;
035000170605             $EoF = *on;
035100170605             exsr  sr_PrintERR;
035200170605
035300170605           // -?Elaborazione?
035400170605           Other;
035500170605             exsr  sr_Elab_FIMIP;
035600170605
035700170605         EndSl;
035800170605
035900170605       ENDSR;
036000170605
036100170605       //--------------------------------------------------------------
036200170605       //?Elaborazione singolo record del *file FIMIP00F.              ?
036300170605       //--------------------------------------------------------------
036400170605       BEGSR  sr_Elab_FIMIP;
036500170605
036600170605         // -?Impostazione date e orari limite per elaborazione?
036700170605         // ·?Data inizio elaborazione?
036800170605         select;
036900170605           when  FiMS17ds.iMS17dti >  *zero  and
037000170605                 FiMS17ds.iMS17dti >= MIPdti;
037100170605             wDataOraIni = %editc( FiMS17ds.iMS17dti : 'X' );
037200170605           when  FiMS17ds.iMS17dti <= *zero  and
037300170605                 wDateMax >= MIPdti;
037400170605             wDataOraIni = %editc( wDateMax : 'X' );
037500170605           other;
037600170605             leavesr;
037700170605         endsl;
037800170605         // ·?Orario inizio elaborazione?
037900170605         if  MIPhmi <> *zero;
038000170605           wDataOraIni = %trimR( wDataOraIni ) + %editc( MIPhmi : 'X' )
038100170605                                               + '00';
038200170605         else;
038300170605           wDataOraIni = %trimR( wDataOraIni ) + '000000';
038400170605         endif;
038500170605         // ·?Data fine elaborazione?
038600170605         select;
038700170605           when  FiMS17ds.iMS17dtf >  *zero  and
038800170605                 FiMS17ds.iMS17dtf <= MIPdtf;
038900170605             wDataOraFin = %editc( FiMS17ds.iMS17dtf : 'X' );
039000170605           when  FiMS17ds.iMS17dtf <= *zero  and
039100170605                 wDateMax <= MIPdtf;
039200170605             wDataOraFin = %editc( wDateMax : 'X' );
039300170605           other;
039400170605             leavesr;
039500170605         endsl;
039600170605         // ·?Orario fine elaborazione?
039700170605         if  MIPhmf <> *zero;
039800170605           wDataOraFin = %trimR( wDataOraFin ) + %editc( MIPhmf : 'X' )
039900170605                                               + '99';
040000170605         else;
040100170605           wDataOraFin = %trimR( wDataOraFin ) + '999999';
040200170605         endif;
040300170605
040400170605
040500170605         Select;
040600170605
040700170605           // -?Estrazione letture da Pistola da FIPSL?
040800170605           When  MIPtip = 'P';
040900170605             exsr  sr_Pistola;
041000170605
041100170605           // -?Estrazione letture da Banco da FLTR7?
041200170605           When  MIPtip = 'F';
041300170605             exsr  sr_Banco;
041400170605
041500170605         EndSl;
041600170605
041700170605       ENDSR;
041800170605
041900170605       //--------------------------------------------------------------
042000170605       //?Elaborazioni letture da Pistola.                             ?
042100170605       //--------------------------------------------------------------
042200170605       BEGSR  sr_Pistola;
042300170605
042400170605         $EoF2 = *off;
042500170605
042600170605         // -?Preparazione stringa SQL per estrazione dati?
042700170605         exsr  sr_PrepSQL_FIPSL;
042800170605
042900170605         // -?Apertura Cursore?
043000170605         exsr  sr_OpenCursor_FIPSL;
043100170605
043200170605         // -?Ciclo di elaborazione?
043300170605         DoW  $EoF2 = *off;
043400170605           exsr  sr_ReadCursor_FIPSL;
043500170605         EndDo;
043600170605
043700170605         // -?Chiusura Cursore?
043800170605         exsr  sr_CloseCursor_FIPSL;
043900170605
044000170605       ENDSR;
044100170605
044200170605       //--------------------------------------------------------------
044300170605       //?Elaborazioni letture da Banco.                               ?
044400170605       //--------------------------------------------------------------
044500170605       BEGSR  sr_Banco;
044600170605
044700170605         $EoF3 = *off;
044800170605
044900170605         // -?Preparazione stringa SQL per estrazione dati?
045000170605         exsr  sr_PrepSQL_FLTR7;
045100170605
045200170605         // -?Apertura Cursore?
045300170605         exsr  sr_OpenCursor_FLTR7;
045400170605
045500170605         // -?Ciclo di elaborazione?
045600170605         DoW  $EoF3 = *off;
045700170605           exsr  sr_ReadCursor_FLTR7;
045800170605         EndDo;
045900170605
046000170605         // -?Chiusura Cursore?
046100170605         exsr  sr_CloseCursor_FLTR7;
046200170605
046300170605       ENDSR;
046400170605
046500170605       //--------------------------------------------------------------
046600170605       //?Preparazione stringa SQL - per estrazione dati da FIPSL.     ?
046700170605       //--------------------------------------------------------------
046800170605       BEGSR  sr_PrepSQL_FIPSL;
046900170605
047000170605         wSQL2 = 'select PSLfil, PSLnpg, PSLusr, PSLdtOraR, PSLbarCo +
047100170605                    from FIPSL00F +
047200170605                   where PSLfil = ''' + %editc( MIPfil : 'X' ) + ''' +
047300170605                     and PSLnpg = ''' + %editc( MIPnpg : 'X' ) + ''' +
047400170605                     and PSLerr = ''  '' +
047500170605                     and PSLusr like ''%' + %editc( MIPfil : 'X' ) +
047600170605                                            %trimR( MIPpep ) + '%'' +
047700170605                     and PSLdtOraR between ''' + wDataOraIni + ''' +
047800170605                                       and ''' + wDataOraFin + ''' +
047900170605                order by PSLfil, PSLnpg, PSLusr, PSLdtOraR, PSLbarCo +
048000170605                     for fetch only';
048100170605
048200170605       ENDSR;
048300170605
048400170605       //--------------------------------------------------------------
048500170605       //?Apertura cursore - per estrazione dati da FIPSL.             ?
048600170605       //--------------------------------------------------------------
048700170605       BEGSR  sr_OpenCursor_FIPSL;
048800170605
048900170605         $EoF2 = *off;
049000170605
049100170605         // -?Dichiarazione cursore?
049200170605         exec sql   prepare S2   from :wSQL2;
049300170605         exec sql   declare C2   cursor for S2;
049400170605
049500170605         if  SQLcode < *zero;
049600170605           $EoF2 = *on;
049700170605           exsr  sr_PrintERR;
049800170605           leavesr;
049900170605         endif;
050000170605
050100170605         // -?Apertura del cursore?
050200170605         exec sql   open C2;
050300170605
050400170605         if  SQLcode < *zero;
050500170605           $EoF2 = *on;
050600170605           exsr  sr_PrintERR;
050700170605           leavesr;
050800170605         endif;
050900170605
051000170605       ENDSR;
051100170605
051200170605       //--------------------------------------------------------------
051300170605       //?Chiusura cursore - per estrazione dati da FIPSL.             ?
051400170605       //--------------------------------------------------------------
051500170605       BEGSR  sr_CloseCursor_FIPSL;
051600170605
051700170605         // -?Chiusura del cursore?
051800170605         exec sql   close C2;
051900170605
052000170605         if  SQLcode < *zero;
052100170605           $EoF2 = *on;
052200170605           exsr  sr_PrintERR;
052300170605           leavesr;
052400170605         endif;
052500170605
052600170605       ENDSR;
052700170605
052800170605       //--------------------------------------------------------------
052900170605       //?Lettura cursore - per estrazione dati da FIPSL.              ?
053000170605       //--------------------------------------------------------------
053100170605       BEGSR  sr_ReadCursor_FIPSL;
053200170605
053300170605         clear  wSQL_FIPSL_ds;
053400170605
053500170605         exec sql   fetch next   from C2   into :wSQL_FIPSL_ds;
053600170605
053700170605         Select;
053800170605
053900170605           // -?E.o.F.?
054000170605           When  SQLcode = 100;
054100170605             $EoF2 = *on;
054200170605             exsr  sr_Write_WFMIT10;
054300170605
054400170605           // -?Errore?
054500170605           When  SQLcode < *zero;
054600170605             $EoF2 = *on;
054700170605             exsr  sr_PrintERR;
054800170605
054900170605           // -?Elaborazione?
055000170605           Other;
055100170605             clear  wDate;
055200170605             clear  wBarCode;
055300170605             wDate    = %int( %subst( wSQL_FIPSL_ds.PSLdtOraR : 1 : 8 ) );
055400170605             wBarCode = wSQL_FIPSL_ds.PSLbarCo;
055500170605             exsr  sr_Elab_BarCode;
055600170605
055700170605         EndSl;
055800170605
055900170605       ENDSR;
056000170605
056100170605       //--------------------------------------------------------------
056200170605       //?Preparazione stringa SQL - per estrazione dati da FLTR7.     ?
056300170605       //--------------------------------------------------------------
056400170605       BEGSR  sr_PrepSQL_FLTR7;
056500170605
056600170605         wSQL3 = 'select TR7fil, TR7ctl, TR7tim, TR7pep, TR7bar +
056700170605                    from FLTR700F +
056800170605                   where TR7fil = ''' + %editc( MIPfil : 'X' ) + ''' +
056900170605                     and TR7ctl = ''' + %editc( MIPnpg : 'X' ) + ''' +
057000170605                     and TR7crc = ''  '' +
057100170605                     and TR7pep like ''%' + %trimR( MIPpep ) + ''' +
057200170605                     and TR7bar > ''00000000000000'' +
057300170605                     and TR7tim between ''' + wDataOraIni + ''' +
057400170605                                    and ''' + wDataOraFin + ''' +
057500170605                order by TR7fil, TR7ctl, TR7tim, TR7pep, TR7bar +
057600170605                     for fetch only';
057700170605
057800170605       ENDSR;
057900170605
058000170605       //--------------------------------------------------------------
058100170605       //?Apertura cursore - per estrazione dati da FLTR7.             ?
058200170605       //--------------------------------------------------------------
058300170605       BEGSR  sr_OpenCursor_FLTR7;
058400170605
058500170605         $EoF3 = *off;
058600170605
058700170605         // -?Dichiarazione cursore?
058800170605         exec sql   prepare S3   from :wSQL3;
058900170605         exec sql   declare C3   cursor for S3;
059000170605
059100170605         if  SQLcode < *zero;
059200170605           $EoF3 = *on;
059300170605           exsr  sr_PrintERR;
059400170605           leavesr;
059500170605         endif;
059600170605
059700170605         // -?Apertura del cursore?
059800170605         exec sql   open C3;
059900170605
060000170605         if  SQLcode < *zero;
060100170605           $EoF3 = *on;
060200170605           exsr  sr_PrintERR;
060300170605           leavesr;
060400170605         endif;
060500170605
060600170605       ENDSR;
060700170605
060800170605       //--------------------------------------------------------------
060900170605       //?Chiusura cursore - per estrazione dati da FLTR7.             ?
061000170605       //--------------------------------------------------------------
061100170605       BEGSR  sr_CloseCursor_FLTR7;
061200170605
061300170605         // -?Chiusura del cursore?
061400170605         exec sql   close C3;
061500170605
061600170605         if  SQLcode < *zero;
061700170605           $EoF3 = *on;
061800170605           exsr  sr_PrintERR;
061900170605           leavesr;
062000170605         endif;
062100170605
062200170605       ENDSR;
062300170605
062400170605       //--------------------------------------------------------------
062500170605       //?Lettura cursore - per estrazione dati da FLTR7.              ?
062600170605       //--------------------------------------------------------------
062700170605       BEGSR  sr_ReadCursor_FLTR7;
062800170605
062900170605         clear  wSQL_FLTR7_ds;
063000170605
063100170605         exec sql   fetch next   from C3   into :wSQL_FLTR7_ds;
063200170605
063300170605         Select;
063400170605
063500170605           // -?E.o.F.?
063600170605           When  SQLcode = 100;
063700170605             $EoF3 = *on;
063800170605             exsr  sr_Write_WFMIT10;
063900170605
064000170605           // -?Errore?
064100170605           When  SQLcode < *zero;
064200170605             $EoF3 = *on;
064300170605             exsr  sr_PrintERR;
064400170605
064500170605           // -?Elaborazione?
064600170605           Other;
064700170605             clear  wDate;
064800170605             clear  wBarCode;
064900170605             wDate    = %int( %subst( wSQL_FLTR7_ds.TR7tim : 1 : 8 ) );
065000170605             wBarCode = wSQL_FLTR7_ds.TR7bar;
065100170605             exsr  sr_Elab_BarCode;
065200170605
065300170605         EndSl;
065400170605
065500170605       ENDSR;
065600170605
065700170605       //--------------------------------------------------------------
065800170605       //?Reperimento dati del Segnacollo ed incremento totali.        ?
065900170605       //--------------------------------------------------------------
066000170605       BEGSR  sr_Elab_BarCode;
066100170605
066200170605         // -?Controllo "rottura di livello" per scrittura record?
066300170605         If  ( MIT1aas * 10000 ) + MIT1mgs <> wDate   or
066400170605             MIT1fil                       <> MIPfil  or
066500170605             MIT1npg                       <> MIPnpg  or
066600170605             MIT1pep                       <> MIPpep;
066700170605
066800170605           exsr  sr_Write_WFMIT10;
066900170605
067000170605           MIT1aas = ( wDate / 10000 );
067100170605           MIT1mgs = wDate - ( MIT1aas * 10000 );
067200170605           MIT1fil = MIPfil;
067300170605           MIT1npg = MIPnpg;
067400170605           MIT1pep = MIPpep;
067500170605
067600170605         EndIf;
067700170605
067800170605         // -?Reperimento n° spedizione dal segnacollo in esame?
067900170605         clear  keyFIART27;
068000170605         k_ARTfls = wBarCode.wBar_LNP;
068100170605         k_ARTlna = wBarCode.wBar_LNA;
068200170605         k_ARTnrs = wBarCode.wBar_NRS;
068300170605         k_ARTnsc = wBarCode.wBar_NSC;
068400170605         chain  %kds( keyFIART27 )  FIART000;
068500170605
068600170605         if  not %found( FIART27L );
068700170605           leavesr;
068800170605         endif;
068900170605
069000170605         // -?Aggancio FNARB00F?
069100170605         clear  keyFNARB01;
069200170605         k_ARBaas = ARTaas;
069300170605         k_ARBlnp = ARTlnp;
069400170605         k_ARBnrs = ARTnrs;
069500170605         k_ARBnsp = ARTnsp;
069600170605         chain  %kds( keyFNARB01 )  FNARB000;
069700170605
069800170605         if  not %found( FNARB01L );
069900170605           leavesr;
070000170605         endif;
070100170605
070200170605         // -?Aggiornamento n° Spunte Incompatibili?
070300170605         //  ?(suddiviso tra Partenze e Arrivi)?
070400170605         MIT1nsi += 1;
070500170605
070600170605       ENDSR;
070700170605
070800170605       //--------------------------------------------------------------
070900170605       //?Scrittura singolo record in WFMIT10F.                        ?
071000170605       //--------------------------------------------------------------
071100170605       BEGSR  sr_Write_WFMIT10;
071200170605
071300170605         if  MIT1nsi <> *zero;
071400170605           //______________
071500170605           write  WFMIT100;
071600170605           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
071700170605         endif;
071800170605
071900170605         clear  WFMIT100;
072000170605
072100170605       ENDSR;
072200170605
072300170605       //--------------------------------------------------------------
072400170605       //?Stampa segnalazione dell'errore rilevato via SQL.            ?
072500170605       //--------------------------------------------------------------
072600170605       BEGSR  sr_PrintERR;
072700170605
072800170605         // -?Stampa del Dump?
072900170605         Dump(A);
073000170605
073100170605         // -?Stampa del Job-Log?
073200170605         Qcmd = 'DSPJOBLOG job(*) output(*print)';
073300170605         exsr  sr_ExecCmd;
073400170605
073500170605       ENDSR;
073600170605
073700170605       //--------------------------------------------------------------
073800170605       //?Operazioni finali.                                           ?
073900170605       //--------------------------------------------------------------
074000170605       BEGSR  sr_RoutEnd;
074100170605
074200170605         // -?Uscita dal *pgm?
074300170605         return;
074400170605
074500170605       ENDSR;
074600170605
074700170605       //--------------------------------------------------------------
074800170605       //?Esecuzione del comando (già impostato).                      ?
074900170605       //--------------------------------------------------------------
075000170605       BEGSR  sr_ExecCmd;
075100170605
075200170605         clear Qcap0100;
075300170605         Qcabcsdh = *off;
075400170605         Qcapa    = *off;
075500170605         Qcacmdss = *off;
075600170605         Qcaerved = *allX'00';
075700170605
075800170605         clear Qusec;
075900170605         Qusbprv  = %size(Qusec);
076000170605
076100170605         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
076200170605                           %size(Qcap0100) : 'CPOP0100' : *omit :
076300170605                           0 : 0 : Qusec);
076400170605
076500170605         //if  Qusei <> *blank;
076600170605         //  ...;
076700170605         //endif;
076800170605
076900170605       ENDSR;
077000170605
077100170605      /end-free
