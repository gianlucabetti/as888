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
008600170605     d   pslBARco                          inz   like(FIPSL00F.PSLbarCo)
008700170605
008800170605     d FLTR700F      e ds                  based(nullPtr)
008900170605     d                                     qualified
009000170605     d wSQL_FLTR7_ds   ds                  inz   qualified
009100170605     d   tr7FIL                            inz   like(FLTR700F.TR7fil)
009200170605     d   tr7NPG                            inz   like(FLTR700F.TR7ctl)
009300170605     d   tr7PEP                            inz   like(FLTR700F.TR7pep)
009400170605     d   tr7BAR                            inz   like(FLTR700F.TR7bar)
009500170605
009600170605       // -?BarCode Segnacollo?
009700170605     d wBarCode        ds            18    inz   qualified
009800170605     d   wBar_LNP                     3s 0 inz
009900170605     d   wBar_LNA                     3s 0 inz
010000170605     d   wBar_NRS                     2s 0 inz
010100170605     d   wBar_NSC                     7s 0 inz
010200170605     d   wBar_ZNC                     2s 0 inz
010300170605     d   wBar_CHK                     1s 0 inz
010400170605
010500170605       // -?Tabelle?
010600170605     d ds3A          e ds                  inz   qualified
010700170605
010800170605       //--------------------------------------------------------------
010900170605       //?Definizione variabili globali.                               ?
011000170605       //--------------------------------------------------------------
011100170605
011200170605       // -?Flags booleani?
011300170605     d $EoF            s               n   inz
011400170605     d $EoF2           s               n   inz
011500170605     d $EoF3           s               n   inz
011600170605
011700170605       // -?Stringhe SQL da eseguire?
011800170605     d wSQL            s          32740    inz   varying
011900170605     d wSQL2           s          32740    inz   varying
012000170605     d wSQL3           s          32740    inz   varying
012100170605
012200170605       // -?Data/Ora limite (dati alfanumerici)?
012300170605     d wDataOraIni     s             14    inz
012400170605     d wDataOraFin     s             14    inz
012500170605
012600170605      *//· // -?Data in formato *EUR?
012700170605     d*//· wDate_Eur       s               d   inz   datfmt(*EUR)
012800170605
012900170605       // -?Campi di Comodo?
013000170605     d wDateMax        s              8  0 inz
013100170605     d wDate           s              8  0 inz
013200170605     d*//· wTime           s              6  0 inz
013300170605     d wFil            s                   inz   like(mipFIL)
013400170605     d wNpg            s                   inz   like(mipNPG)
013500170605     d wPep            s                   inz   like(mipPEP)
013600170605
013700170605       //--------------------------------------------------------------
013800170605       //?Definizione prototipi procedure.                             ?
013900170605       //--------------------------------------------------------------
014000170605
014100170605       // -?Reperimento dati utente?
014200170605     d TIBS34ds      e ds                  inz
014300170605      /copy gaitrasrc/srcProtoPR,TIBS34R
014400170605
014500170605       // -?Parametri API QCAPCMD (Process Commands)?
014600170605     d Qcmd            s           2048    inz   varying
014700170605      /copy qSysInc/qRpgleSrc,QCAPCMD
014800170605       // -?API QCAPCMD (Process Commands)?
014900170605      /copy gaitrasrc/srcProtoPR,QCAPCMD
015000170605
015100170605       // -?Parametri gestione errori API.?
015200170605      /copy qsysinc/qrpglesrc,QUSEC
015300170605
015400170605       //--------------------------------------------------------------
015500170605       //?Definizione key-list.                                        ?
015600170605       //--------------------------------------------------------------
015700170605
015800170605       // -?File FIART27L?
015900170605     d keyFIART27    e ds                  extname( FIART27L : *key )
016000170605     d                                     prefix( k_ )  inz
016100170605
016200170605       // -?File FNARB01L?
016300170605     d keyFNARB01    e ds                  extname( FNARB01L : *key )
016400170605     d                                     prefix( k_ )  inz
016500170605
016600170605       //--------------------------------------------------------------
016700170605       //?Riepilogo indicatori utilizzati.                             ?
016800170605       //--------------------------------------------------------------
016900170605
017000170605
017100170605       //--------------------------------------------------------------
017200170605       //?M A I N - L I N E                                            ?
017300170605       //--------------------------------------------------------------
017400170605
017500170605     c     *Entry        plist
017600170605     c                   parm                    KPJBA
017700170605
017800170605      /free
017900170605
018000170605       // -?Operazioni iniziali?
018100170605       exsr  sr_RoutInz;
018200170605
018300170605       // -?Preparazione stringa SQL per estrazione dati?
018400170605       exsr  sr_PrepSQL;
018500170605
018600170605       // -?Apertura Cursore?
018700170605       exsr  sr_OpenCursor;
018800170605
018900170605       // -?Ciclo di elaborazione?
019000170605       DoW  $EoF = *off;
019100170605         exsr  sr_ReadCursor;
019200170605       EndDo;
019300170605
019400170605       // -?Chiusura Cursore?
019500170605       exsr  sr_CloseCursor;
019600170605
019700170605       // -?Operazioni finali?
019800170605       exsr sr_RoutEnd;
019900170605
020000170605       //--------------------------------------------------------------
020100170605       //?Operazioni iniziali.                                         ?
020200170605       //--------------------------------------------------------------
020300170605       BEGSR  sr_RoutInz;
020400170605
020500170605         // -?Impostazione opzioni per SQL?
020600170605         exec sql   set  option  DynUsrPrf = *Owner,
020700170605                                 CloSqlCsr = *EndMod;
020800170605
020900170605         // -?Impostazione chiusura?
021000170605         *inLR = *on;
021100170605
021200170605         // -?Reperimento dati job?
021300170605         exsr  sr_DatiJob;
021400170605
021500170605         // -?Reperimento parametri?
021600170605         clear  FIMS17ds;
021700170605         If  KPJBU <> *blank;
021800170605           FIMS17ds = KPJBU;
021900170605           if  %check( c_Digits : %subst( KPJBU : 1 :
022000170605                                  %size( FIMS17ds.iMS17fil ) ) )      > *zero;
022100170605             clear  FiMS17ds.iMS17fil;
022200170605           endif;
022300170605           if  %check( c_Digits : %subst( KPJBU :
022400170605                                  %size( FiMS17ds.iMS17fil ) + 1 :
022500170605                                  %size( FiMS17ds.iMS17dti ) ) ) > *zero;
022600170605             clear  FiMS17ds.iMS17dti;
022700170605           endif;
022800170605           if  %check( c_Digits : %subst( KPJBU :
022900170605                                  %size( FiMS17ds.iMS17fil ) +
023000170605                                  %size( FiMS17ds.iMS17dti ) + 1 :
023100170605                                  %size( FiMS17ds.iMS17dtf ) ) ) > *zero;
023200170605             clear  FiMS17ds.iMS17dtf;
023300170605           endif;
023400170605           if  %check( c_Digits : %subst( KPJBU :
023500170605                                  %size( FiMS17ds.iMS17fil ) +
023600170605                                  %size( FiMS17ds.iMS17dti ) +
023700170605                                  %size( FiMS17ds.iMS17dtf ) + 1 :
023800170605                                  %size( FiMS17ds.iMS17npg ) ) ) > *zero;
023900170605             clear  FiMS17ds.iMS17npg;
024000170605           endif;
024100170605           if  FiMS17ds.iMS17dti > *zero  and
024200170605               FiMS17ds.iMS17dtf = *zero;
024300170605             FiMS17ds.iMS17dtf = FiMS17ds.iMS17dti;
024400170605           endif;
024500170605         EndIf;
024600170605
024700170605         //*·// -?Reperimento data odierna in formato gg/mm/aaaa?
024800170605         //*·wDate_Eur = %date();
024900170605         //*·wDate = %dec( wDate_Eur );
025000170605         //*·
025100170605         //*·// -?Reperimento orario?
025200170605         //*·wTime = %dec( %time() );
025300170605
025400170605         // -?Impostazione data limite?
025500170605         wDateMax = %dec( %date() - %days(1) );
025600170605
025700170605       ENDSR;
025800170605
025900170605       //--------------------------------------------------------------
026000170605       //?Reperimento Dati del job (Utente/Operativi).                 ?
026100170605       //--------------------------------------------------------------
026200170605       BEGSR  sr_DatiJob;
026300170605
026400170605         in(e)  §AzUte;
026500170605         if  NOT  %error;
026600170605           in(e)  §DatiUte;
026700170605         endif;
026800170605         if  %error  or  RSut = *blank;
026900170605           tibs34r ( tibs34ds );
027000170605           in  §AzUte;
027100170605           in  §DatiUte;
027200170605         endif;
027300170605
027400170605       ENDSR;
027500170605
027600170605       //--------------------------------------------------------------
027700170605       //?Preparazione stringa SQL - per estrazione dati da FIMIP.     ?
027800170605       //--------------------------------------------------------------
027900170605       BEGSR  sr_PrepSQL;
028000170605
028100170605         wSQL  = 'select MIPfil, MIPnpg, MIPtip, MIPpep, +
028200170605                         MIPdti, MIPdtf, MIPhmi, MIPhmf +
028300170605                    from FIMIP00F';
028400170605
028500170605         if  FiMS17ds.iMS17fil > *zero;
028600170605           wSQL +=
028700170605                 ' where MIPfil = ' + %editc( FiMS17ds.iMS17fil : 'X' );
028800170605         endif;
028900170605
029000170605         wSQL += ' order by MIPfil, MIPnpg, MIPtip, MIPpep, +
029100170605                            MIPdti, MIPdtf, MIPhmi, MIPhmf +
029200170605                     for fetch only';
029300170605
029400170605       ENDSR;
029500170605
029600170605       //--------------------------------------------------------------
029700170605       //?Apertura cursore.                                            ?
029800170605       //--------------------------------------------------------------
029900170605       BEGSR  sr_OpenCursor;
030000170605
030100170605         $EoF  = *off;
030200170605
030300170605         // -?Dichiarazione cursore?
030400170605         exec sql   prepare S1   from :wSQL;
030500170605         exec sql   declare C1   cursor for S1;
030600170605
030700170605         if  SQLcode < *zero;
030800170605           $EoF  = *on;
030900170605           exsr  sr_PrintERR;
031000170605           leavesr;
031100170605         endif;
031200170605
031300170605         // -?Apertura del cursore?
031400170605         exec sql   open C1;
031500170605
031600170605         if  SQLcode < *zero;
031700170605           $EoF  = *on;
031800170605           exsr  sr_PrintERR;
031900170605           leavesr;
032000170605         endif;
032100170605
032200170605       ENDSR;
032300170605
032400170605       //--------------------------------------------------------------
032500170605       //?Chiusura cursore.                                            ?
032600170605       //--------------------------------------------------------------
032700170605       BEGSR  sr_CloseCursor;
032800170605
032900170605         // -?Chiusura del cursore?
033000170605         exec sql   close C1;
033100170605
033200170605       ENDSR;
033300170605
033400170605       //--------------------------------------------------------------
033500170605       //?Lettura cursore.                                             ?
033600170605       //--------------------------------------------------------------
033700170605       BEGSR  sr_ReadCursor;
033800170605
033900170605         clear  FIMIP_ds;
034000170605
034100170605         exec sql   fetch next   from C1   into :FIMIP_ds;
034200170605
034300170605         Select;
034400170605
034500170605           // -?E.o.F.?
034600170605           When  SQLcode = 100;
034700170605             $EoF = *on;
034800170605
034900170605           // -?Errore?
035000170605           When  SQLcode < *zero;
035100170605             $EoF = *on;
035200170605             exsr  sr_PrintERR;
035300170605
035400170605           // -?Elaborazione?
035500170605           Other;
035600170605             exsr  sr_Elab_FIMIP;
035700170605
035800170605         EndSl;
035900170605
036000170605       ENDSR;
036100170605
036200170605       //--------------------------------------------------------------
036300170605       //?Elaborazione singolo record del *file FIMIP00F.              ?
036400170605       //--------------------------------------------------------------
036500170605       BEGSR  sr_Elab_FIMIP;
036600170605
036700170605         // -?Impostazione date e orari limite per elaborazione?
036800170605         // ·?Data inizio elaborazione?
036900170605         select;
037000170605           when  FiMS17ds.iMS17dti >  *zero  and
037100170605                 FiMS17ds.iMS17dti >= MIPdti;
037200170605             wDataOraIni = %editc( FiMS17ds.iMS17dti : 'X' );
037300170605           when  FiMS17ds.iMS17dti <= *zero  and
037400170605                 wDateMax >= MIPdti;
037500170605             wDataOraIni = %editc( wDateMax : 'X' );
037600170605           other;
037700170605             leavesr;
037800170605         endsl;
037900170605         // ·?Orario inizio elaborazione?
038000170605         if  MIPhmi <> *zero;
038100170605           wDataOraIni = %trimR( wDataOraIni ) + %editc( MIPhmi : 'X' )
038200170605                                               + '00';
038300170605         else;
038400170605           wDataOraIni = %trimR( wDataOraIni ) + '000000';
038500170605         endif;
038600170605         // ·?Data fine elaborazione?
038700170605         select;
038800170605           when  FiMS17ds.iMS17dtf >  *zero  and
038900170605                 FiMS17ds.iMS17dtf <= MIPdtf;
039000170605             wDataOraFin = %editc( FiMS17ds.iMS17dtf : 'X' );
039100170605           when  FiMS17ds.iMS17dtf <= *zero  and
039200170605                 wDateMax <= MIPdtf;
039300170605             wDataOraFin = %editc( wDateMax : 'X' );
039400170605           other;
039500170605             leavesr;
039600170605         endsl;
039700170605         // ·?Orario fine elaborazione?
039800170605         if  MIPhmf <> *zero;
039900170605           wDataOraFin = %trimR( wDataOraFin ) + %editc( MIPhmf : 'X' )
040000170605                                               + '99';
040100170605         else;
040200170605           wDataOraFin = %trimR( wDataOraFin ) + '999999';
040300170605         endif;
040400170605
040500170605
040600170605         Select;
040700170605
040800170605           // -?Estrazione letture da Pistola da FIPSL?
040900170605           When  MIPtip = 'P';
041000170605             exsr  sr_Pistola;
041100170605
041200170605           // -?Estrazione letture da Banco da FLTR7?
041300170605           When  MIPtip = 'F';
041400170605             exsr  sr_Banco;
041500170605
041600170605         EndSl;
041700170605
041800170605       ENDSR;
041900170605
042000170605       //--------------------------------------------------------------
042100170605       //?Elaborazioni letture da Pistola.                             ?
042200170605       //--------------------------------------------------------------
042300170605       BEGSR  sr_Pistola;
042400170605
042500170605         $EoF2 = *off;
042600170605
042700170605         // -?Preparazione stringa SQL per estrazione dati?
042800170605         exsr  sr_PrepSQL_FIPSL;
042900170605
043000170605         // -?Apertura Cursore?
043100170605         exsr  sr_OpenCursor_FIPSL;
043200170605
043300170605         // -?Ciclo di elaborazione?
043400170605         DoW  $EoF2 = *off;
043500170605           exsr  sr_ReadCursor_FIPSL;
043600170605         EndDo;
043700170605
043800170605         // -?Chiusura Cursore?
043900170605         exsr  sr_CloseCursor_FIPSL;
044000170605
044100170605       ENDSR;
044200170605
044300170605       //--------------------------------------------------------------
044400170605       //?Elaborazioni letture da Banco.                               ?
044500170605       //--------------------------------------------------------------
044600170605       BEGSR  sr_Banco;
044700170605
044800170605         $EoF3 = *off;
044900170605
045000170605         // -?Preparazione stringa SQL per estrazione dati?
045100170605         exsr  sr_PrepSQL_FLTR7;
045200170605
045300170605         // -?Apertura Cursore?
045400170605         exsr  sr_OpenCursor_FLTR7;
045500170605
045600170605         // -?Ciclo di elaborazione?
045700170605         DoW  $EoF3 = *off;
045800170605           exsr  sr_ReadCursor_FLTR7;
045900170605         EndDo;
046000170605
046100170605         // -?Chiusura Cursore?
046200170605         exsr  sr_CloseCursor_FLTR7;
046300170605
046400170605       ENDSR;
046500170605
046600170605       //--------------------------------------------------------------
046700170605       //?Preparazione stringa SQL - per estrazione dati da FIPSL.     ?
046800170605       //--------------------------------------------------------------
046900170605       BEGSR  sr_PrepSQL_FIPSL;
047000170605
047100170605         wSQL2 = 'select distinct PSLfil, PSLnpg, PSLusr, PSLbarCo +
047200170605                    from FIPSL00F +
047300170605                   where PSLfil = ''' + %editc( MIPfil : 'X' ) + ''' +
047400170605                     and PSLnpg = ''' + %editc( MIPnpg : 'X' ) + ''' +
047500170605                     and PSLerr = ''  '' +
047600170605                     and PSLusr like ''%' + %editc( MIPfil : 'X' ) +
047700170605                                            %trimR( MIPpep ) + ''' +
047800170605                     and PSLdtOraR between ''' + wDataOraIni + ''' +
047900170605                                       and ''' + wDataOraFin + ''' +
048000170605                order by PSLfil, PSLnpg, PSLusr, PSLbarCo +
048100170605                     for fetch only';
048200170605
048300170605       ENDSR;
048400170605
048500170605       //--------------------------------------------------------------
048600170605       //?Apertura cursore - per estrazione dati da FIPSL.             ?
048700170605       //--------------------------------------------------------------
048800170605       BEGSR  sr_OpenCursor_FIPSL;
048900170605
049000170605         $EoF2 = *off;
049100170605
049200170605         // -?Dichiarazione cursore?
049300170605         exec sql   prepare S2   from :wSQL2;
049400170605         exec sql   declare C2   cursor for S2;
049500170605
049600170605         if  SQLcode < *zero;
049700170605           $EoF2 = *on;
049800170605           exsr  sr_PrintERR;
049900170605           leavesr;
050000170605         endif;
050100170605
050200170605         // -?Apertura del cursore?
050300170605         exec sql   open C2;
050400170605
050500170605         if  SQLcode < *zero;
050600170605           $EoF2 = *on;
050700170605           exsr  sr_PrintERR;
050800170605           leavesr;
050900170605         endif;
051000170605
051100170605       ENDSR;
051200170605
051300170605       //--------------------------------------------------------------
051400170605       //?Chiusura cursore - per estrazione dati da FIPSL.             ?
051500170605       //--------------------------------------------------------------
051600170605       BEGSR  sr_CloseCursor_FIPSL;
051700170605
051800170605         // -?Chiusura del cursore?
051900170605         exec sql   close C2;
052000170605
052100170605         if  SQLcode < *zero;
052200170605           $EoF2 = *on;
052300170605           exsr  sr_PrintERR;
052400170605           leavesr;
052500170605         endif;
052600170605
052700170605       ENDSR;
052800170605
052900170605       //--------------------------------------------------------------
053000170605       //?Lettura cursore - per estrazione dati da FIPSL.              ?
053100170605       //--------------------------------------------------------------
053200170605       BEGSR  sr_ReadCursor_FIPSL;
053300170605
053400170605         clear  wSQL_FIPSL_ds;
053500170605
053600170605         exec sql   fetch next   from C2   into :wSQL_FIPSL_ds;
053700170605
053800170605         Select;
053900170605
054000170605           // -?E.o.F.?
054100170605           When  SQLcode = 100;
054200170605             $EoF2 = *on;
054300170605
054400170605           // -?Errore?
054500170605           When  SQLcode < *zero;
054600170605             $EoF2 = *on;
054700170605             exsr  sr_PrintERR;
054800170605
054900170605           // -?Elaborazione?
055000170605           Other;
055100170605             clear  wDate;
055200170605             clear  wFil;
055300170605             clear  wNpg;
055400170605             clear  wPep;
055500170605             clear  wBarCode;
055600170605             wDate    = %int( %subst( PSLdtOraR : 1 : 8 ) );
055700170605             wFil     = MIPfil;
055800170605             wNpg     = MIPnpg;
055900170605             wPep     = MIPpep;
056000170605             wBarCode = wSQL_FIPSL_ds.PSLbarCo;
056100170605             exsr  sr_Write_WFMIT10;
056200170605
056300170605         EndSl;
056400170605
056500170605       ENDSR;
056600170605
056700170605       //--------------------------------------------------------------
056800170605       //?Preparazione stringa SQL - per estrazione dati da FLTR7.     ?
056900170605       //--------------------------------------------------------------
057000170605       BEGSR  sr_PrepSQL_FLTR7;
057100170605
057200170605         wSQL3 = 'select distinct TR7fil, TR7ctl, TR7pep, TR7bar +
057300170605                    from FLTR700F +
057400170605                   where TR7fil = ''' + %editc( MIPfil : 'X' ) + ''' +
057500170605                     and TR7ctl = ''' + %editc( MIPnpg : 'X' ) + ''' +
057600170605                     and TR7crc = ''  '' +
057700170605                     and TR7pep like ''%' + %trimR( MIPpep ) + ''' +
057800170605                     and TR7bar > ''00000000000000000'' +
057900170605                     and TR7tim between ''' + wDataOraIni + ''' +
058000170605                                    and ''' + wDataOraFin + ''' +
058100170605                   order by TR7fil, TR7ctl, TR7pep, TR7bar +
058200170605                     for fetch only';
058300170605
058400170605       ENDSR;
058500170605
058600170605       //--------------------------------------------------------------
058700170605       //?Apertura cursore - per estrazione dati da FLTR7.             ?
058800170605       //--------------------------------------------------------------
058900170605       BEGSR  sr_OpenCursor_FLTR7;
059000170605
059100170605         $EoF3 = *off;
059200170605
059300170605         // -?Dichiarazione cursore?
059400170605         exec sql   prepare S3   from :wSQL3;
059500170605         exec sql   declare C3   cursor for S3;
059600170605
059700170605         if  SQLcode < *zero;
059800170605           $EoF3 = *on;
059900170605           exsr  sr_PrintERR;
060000170605           leavesr;
060100170605         endif;
060200170605
060300170605         // -?Apertura del cursore?
060400170605         exec sql   open C3;
060500170605
060600170605         if  SQLcode < *zero;
060700170605           $EoF3 = *on;
060800170605           exsr  sr_PrintERR;
060900170605           leavesr;
061000170605         endif;
061100170605
061200170605       ENDSR;
061300170605
061400170605       //--------------------------------------------------------------
061500170605       //?Chiusura cursore - per estrazione dati da FLTR7.             ?
061600170605       //--------------------------------------------------------------
061700170605       BEGSR  sr_CloseCursor_FLTR7;
061800170605
061900170605         // -?Chiusura del cursore?
062000170605         exec sql   close C3;
062100170605
062200170605         if  SQLcode < *zero;
062300170605           $EoF3 = *on;
062400170605           exsr  sr_PrintERR;
062500170605           leavesr;
062600170605         endif;
062700170605
062800170605       ENDSR;
062900170605
063000170605       //--------------------------------------------------------------
063100170605       //?Lettura cursore - per estrazione dati da FLTR7.              ?
063200170605       //--------------------------------------------------------------
063300170605       BEGSR  sr_ReadCursor_FLTR7;
063400170605
063500170605         clear  wSQL_FLTR7_ds;
063600170605
063700170605         exec sql   fetch next   from C3   into :wSQL_FLTR7_ds;
063800170605
063900170605         Select;
064000170605
064100170605           // -?E.o.F.?
064200170605           When  SQLcode = 100;
064300170605             $EoF3 = *on;
064400170605
064500170605           // -?Errore?
064600170605           When  SQLcode < *zero;
064700170605             $EoF3 = *on;
064800170605             exsr  sr_PrintERR;
064900170605
065000170605           // -?Elaborazione?
065100170605           Other;
065200170605             clear  wDate;
065300170605             clear  wFil;
065400170605             clear  wNpg;
065500170605             clear  wPep;
065600170605             clear  wBarCode;
065700170605             wDate    = %int( %subst( TR7tim : 1 : 8 ) );
065800170605             wFil     = MIPfil;
065900170605             wNpg     = MIPnpg;
066000170605             wPep     = MIPpep;
066100170605             wBarCode = wSQL_FLTR7_ds.TR7bar;
066200170605             exsr  sr_Write_WFMIT10;
066300170605
066400170605         EndSl;
066500170605
066600170605       ENDSR;
066700170605
066800170605       //--------------------------------------------------------------
066900170605       //?Controllo/Scrittura record in WFMIT10F.                      ?
067000170605       //--------------------------------------------------------------
067100170605       BEGSR  sr_Write_WFMIT10;
067200170605
067300170605         // -?Controllo "rottura di livello" per scrittura record?
067400170605         If  ( MIT1aas * 10000 ) + MIT1mgs <> wDate  or
067500170605             MIT1fil                       <> wFil   or
067600170605             MIT1npg                       <> wNpg   or
067700170605             MIT1pep                       <> wPep;
067800170605
067900170605           //______________
068000170605           write  WFMIT100;
068100170605           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
068200170605
068300170605           clear  WFMIT100;
068400170605           MIT1aas = ( wDate / 10000 );
068500170605           MIT1mgs = wDate - ( MIT1aas * 10000 );
068600170605           MIT1fil = wFil;
068700170605           MIT1npg = wNpg;
068800170605           MIT1pep = wPep;
068900170605
069000170605         EndIf;
069100170605
069200170605         // -?Reperimento n° spedizione dal segnacollo in esame?
069300170605         clear  keyFIART27;
069400170605         k_ARTfls = wBarCode.wBar_LNP;
069500170605         k_ARTlna = wBarCode.wBar_LNA;
069600170605         k_ARTnrs = wBarCode.wBar_NRS;
069700170605         k_ARTnsc = wBarCode.wBar_NSC;
069800170605         chain  %kds( keyFIART27 )  FIART000;
069900170605
070000170605         if  not %found( FIART27L );
070100170605           leavesr;
070200170605         endif;
070300170605
070400170605         // -?Aggancio FNARB00F?
070500170605         clear  keyFNARB01;
070600170605         k_ARBaas = ARTaas;
070700170605         k_ARBlnp = ARTlnp;
070800170605         k_ARBnrs = ARTnrs;
070900170605         k_ARBnsp = ARTnsp;
071000170605         chain  %kds( keyFNARB01 )  FNARB000;
071100170605
071200170605         if  not %found( FNARB01L );
071300170605           leavesr;
071400170605         endif;
071500170605
071600170605         // -?Aggiornamento n° Spunte Incompatibili?
071700170605         //  ?(suddiviso tra Partenze e Arrivi)?
071800170605         MIS1nsi += 1;
071900170605
072000170605       ENDSR;
072100170605
072200170605       //--------------------------------------------------------------
072300170605       //?Stampa segnalazione dell'errore rilevato via SQL.            ?
072400170605       //--------------------------------------------------------------
072500170605       BEGSR  sr_PrintERR;
072600170605
072700170605         // -?Stampa del Dump?
072800170605         Dump(A);
072900170605
073000170605         // -?Stampa del Job-Log?
073100170605         Qcmd = 'DSPJOBLOG job(*) output(*print)';
073200170605         exsr  sr_ExecCmd;
073300170605
073400170605       ENDSR;
073500170605
073600170605       //--------------------------------------------------------------
073700170605       //?Operazioni finali.                                           ?
073800170605       //--------------------------------------------------------------
073900170605       BEGSR  sr_RoutEnd;
074000170605
074100170605         // -?Uscita dal *pgm?
074200170605         return;
074300170605
074400170605       ENDSR;
074500170605
074600170605       //--------------------------------------------------------------
074700170605       //?Esecuzione del comando (già impostato).                      ?
074800170605       //--------------------------------------------------------------
074900170605       BEGSR  sr_ExecCmd;
075000170605
075100170605         clear Qcap0100;
075200170605         Qcabcsdh = *off;
075300170605         Qcapa    = *off;
075400170605         Qcacmdss = *off;
075500170605         Qcaerved = *allX'00';
075600170605
075700170605         clear Qusec;
075800170605         Qusbprv  = %size(Qusec);
075900170605
076000170605         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
076100170605                           %size(Qcap0100) : 'CPOP0100' : *omit :
076200170605                           0 : 0 : Qusec);
076300170605
076400170605         //if  Qusei <> *blank;
076500170605         //  ...;
076600170605         //endif;
076700170605
076800170605       ENDSR;
076900170605
077000170605      /end-free
