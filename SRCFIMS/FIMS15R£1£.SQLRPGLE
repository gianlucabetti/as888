000100170522       //==============================================================
000200170524       //?FIMS15R - Statistica Clienti con Merce Incompatibile: crea   ?
000300170524       //?          *file FIMIS00F: Merce Incompatibile - Spedizioni   ?
000400170524       //?          - batch -                                          ?
000500170522       //==============================================================
000600170522
000700170522       //--------------------------------------------------------------
000800170522       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000900170522       //--------------------------------------------------------------
001000170522
001100170522     /*PRM dbgview(*source)
001200170522     /*END
001300170522
001400170522       //--------------------------------------------------------------
001500170522       //?Specifiche di controllo.                                     ?
001600170522       //--------------------------------------------------------------
001700170522
001800170522     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001900170522     h alwnull(*inputonly)
002000170522     h dftactgrp(*no)
002100170524     h bnddir('TRUL')
002200170522
002300170522       //--------------------------------------------------------------
002400170522       //?Dichiarazione file.                                          ?
002500170522       //--------------------------------------------------------------
002600170522
002700170524       // -?BOLLE: Dettaglio Segnacolli?
002800170524     fFIART27L  if   e           k disk
002900170522
003000170524       // -?BOLLE ARRIVI: Testata?
003100170522     fFNARB01L  if   e           k disk
003200170522
003300170522       // -?Merce Incompatibile: Spedizioni?
003400170524     fFIMIS01L  if A e           k disk
003500170523
003600170523       // -?Stampa segnalazioni di errore?
003700170523     fQSYSPRT   o    f  132        printer usropn
003800170523     f                                     oflind(*inOF)
003900170522
004000170522       //--------------------------------------------------------------
004100170522       //?Definizione costanti.                                        ?
004200170522       //--------------------------------------------------------------
004300170522
004400170524       // -?Costante per controllo "caratteri solo numerici"?
004500170524     d c_Digits        c                   const('0123456789')
004600170522
004700170522       //--------------------------------------------------------------
004800170522       //?Definizione schiere.                                         ?
004900170522       //--------------------------------------------------------------
005000170522
005100170523       // -?Turni elaborati?
005200170522     d sk_Turno        s             10    dim( 2)  ctdata  perrcd(1)
005300170522     d sk_Npg          s              1s 0 dim( 2)  alt(sk_Turno)
005400170523
005500170523       // -?Messaggi di Errore?
005600170524     d sk_Msg          s             78    dim( 4)  ctdata  perrcd( 1)
005700170522
005800170522       //--------------------------------------------------------------
005900170522       //?Definizione aree dati.                                       ?
006000170522       //--------------------------------------------------------------
006100170522
006200170522       // -?Dati utente?
006300170522     d §AzUte        e ds                  extname(AZUTE00F)
006400170522     d                                     dtaara
006500170522     d §DatiUte      e ds                  extname(dDatiUte)
006600170522     d                                     dtaara
006700170522
006800170522       //--------------------------------------------------------------
006900170522       //?Definizione strutture dati.                                  ?
007000170522       //--------------------------------------------------------------
007100170522
007200170522       // -?Status ds?
007300170522     d Status         sds
007400170522     d   SDSpgmName      *proc
007500170522     d*//SDSdta              191    198
007600170522     d   SDSjob              244    253
007700170522     d   SDSusr              254    263
007800170522
007900170522       // -?Parametri ricevuti?
008000170523     d KPJBA         e ds
008100170522     d FIMS15ds        ds                  inz   qualified
008200170523     d   iMS15fil                     3s 0 inz
008300170524     d   iMS15dti                     8s 0 inz
008400170524     d   iMS15dtf                     8s 0 inz
008500170523     d*//iMS15npg                     1s 0 inz
008600170522
008700170522       // -?Dati estratti via SQL?
008800170524     d FIMIP_ds      e ds                  extname(FIMIP00F)
008900170524     d                                     inz
009000170523
009100170523     d FIPSL00F      e ds                  based(nullPtr)
009200170523     d                                     qualified
009300170523     d wSQL_FIPSL_ds   ds                  inz   qualified
009400170523     d   pslFIL                            inz   like(FIPSL00F.PSLfil)
009500170523     d   pslNPG                            inz   like(FIPSL00F.PSLnpg)
009600170523     d   pslUSR                            inz   like(FIPSL00F.PSLusr)
009700170523     d   pslBARco                          inz   like(FIPSL00F.PSLbarCo)
009800170523
009900170523     d FLTR700F      e ds                  based(nullPtr)
010000170523     d                                     qualified
010100170523     d wSQL_FLTR7_ds   ds                  inz   qualified
010200170523     d   tr7FIL                            inz   like(FLTR700F.TR7fil)
010300170523     d   tr7NPG                            inz   like(FLTR700F.TR7ctl)
010400170523     d   tr7PEP                            inz   like(FLTR700F.TR7pep)
010500170523     d   tr7BAR                            inz   like(FLTR700F.TR7bar)
010600170523
010700170523       // -?BarCode Segnacollo?
010800170524     d wBarCode        ds            18    inz   qualified
010900170524     d   wBar_LNP                     3s 0 inz
011000170524     d   wBar_LNA                     3s 0 inz
011100170524     d   wBar_NRS                     2s 0 inz
011200170524     d   wBar_NSC                     7s 0 inz
011300170524     d   wBar_ZNC                     2s 0 inz
011400170524     d   wBar_CHK                     1s 0 inz
011500170524
011600170524       // -?Tabelle?
011700170524     d ds3A          e ds                  inz   qualified
011800170522
011900170522       //--------------------------------------------------------------
012000170522       //?Definizione variabili globali.                               ?
012100170522       //--------------------------------------------------------------
012200170522
012300170522       // -?Flags booleani?
012400170522     d $EoF            s               n   inz
012500170524     d $EoF2           s               n   inz
012600170524     d $EoF3           s               n   inz
012700170522
012800170522       // -?Stringhe SQL da eseguire?
012900170522     d wSQL            s          32740    inz   varying
013000170523     d wSQL2           s          32740    inz   varying
013100170523     d wSQL3           s          32740    inz   varying
013200170523
013300170523       // -?Data/Ora limite (dati alfanumerici)?
013400170523     d wDataOraIni     s             14    inz
013500170523     d wDataOraFin     s             14    inz
013600170522
013700170523       // -?Data in formato *EUR?
013800170523     d wDate_Eur       s               d   inz   datfmt(*EUR)
013900170524
014000170524       // -?Errore rilevato?
014100170524     d wErr            s              1  0 inz
014200170524     d wErrMsg         s             80    inz
014300170523
014400170523       // -?Campi di Comodo?
014500170523     d wDateMax        s              8  0 inz
014600170523     d wDate           s              8  0 inz
014700170523     d wTime           s              6  0 inz
014800170522
014900170522       //--------------------------------------------------------------
015000170522       //?Definizione prototipi procedure.                             ?
015100170522       //--------------------------------------------------------------
015200170522
015300170522       // -?Reperimento dati utente?
015400170522     d TIBS34ds      e ds                  inz
015500170522      /copy gaitrasrc/srcProtoPR,TIBS34R
015600170522
015700170522       // -?Reperimento dati tabelle?
015800170522      /copy gaitrasrc/srcProtoPI,TRULTAB
015900170522      /copy gaitrasrc/srcProtoPR,TRULTAB
016000170522
016100170522       // -?Parametri API QCAPCMD (Process Commands)?
016200170522     d Qcmd            s           2048    inz   varying
016300170522      /copy qSysInc/qRpgleSrc,QCAPCMD
016400170522       // -?API QCAPCMD (Process Commands)?
016500170522      /copy gaitrasrc/srcProtoPR,QCAPCMD
016600170522
016700170522       // -?Parametri gestione errori API.?
016800170522      /copy qsysinc/qrpglesrc,QUSEC
016900170522
017000170522       //--------------------------------------------------------------
017100170522       //?Definizione key-list.                                        ?
017200170522       //--------------------------------------------------------------
017300170522
017400170524       // -?File FIART27L?
017500170524     d keyFIART27    e ds                  extname( FIART27L : *key )
017600170524     d                                     prefix( k_ )  inz
017700170524
017800170524       // -?File FNARB01L?
017900170524     d keyFNARB01    e ds                  extname( FNARB01L : *key )
018000170524     d                                     prefix( k_ )  inz
018100170524
018200170524       // -?File FIMIS01L?
018300170524     d keyFIMIS01    e ds                  extname( FIMIS01L : *key )
018400170524     d                                     prefix( k_ )  inz
018500170522
018600170522       //--------------------------------------------------------------
018700170522       //?Riepilogo indicatori utilizzati.                             ?
018800170522       //--------------------------------------------------------------
018900170522
019000170522
019100170522       //--------------------------------------------------------------
019200170522       //?M A I N - L I N E                                            ?
019300170522       //--------------------------------------------------------------
019400170522
019500170522     c     *Entry        plist
019600170522     c                   parm                    KPJBA
019700170522
019800170522      /free
019900170522
020000170522       // -?Operazioni iniziali?
020100170522       exsr  sr_RoutInz;
020200170522
020300170522       // -?Preparazione stringa SQL per estrazione dati?
020400170522       exsr  sr_PrepSQL;
020500170522
020600170522       // -?Apertura Cursore?
020700170522       exsr  sr_OpenCursor;
020800170522
020900170523       // -?Ciclo di elaborazione?
021000170522       DoW  $EoF = *off;
021100170522         exsr  sr_ReadCursor;
021200170522       EndDo;
021300170522
021400170522       // -?Chiusura Cursore?
021500170522       exsr  sr_CloseCursor;
021600170522
021700170522       // -?Operazioni finali?
021800170522       exsr sr_RoutEnd;
021900170522
022000170522       //--------------------------------------------------------------
022100170522       //?Operazioni iniziali.                                         ?
022200170522       //--------------------------------------------------------------
022300170522       BEGSR  sr_RoutInz;
022400170522
022500170522         // -?Impostazione opzioni per SQL?
022600170522         exec sql   set  option  DynUsrPrf = *Owner,
022700170522                                 CloSqlCsr = *EndMod;
022800170522
022900170522         // -?Impostazione chiusura?
023000170522         *inLR = *on;
023100170522
023200170522         // -?Reperimento dati job?
023300170522         exsr  sr_DatiJob;
023400170522
023500170522         // -?Reperimento parametri?
023600170523         clear  FIMS15ds;
023700170523         If  KPJBU <> *blank;
023800170523           FIMS15ds = KPJBU;
023900170523           if  %check( c_Digits : %subst( KPJBU : 1 :
024000170523                                  %size( FIMS15ds.iMS15fil ) ) )      > *zero;
024100170523             clear  FIMS15ds.iMS15fil;
024200170523           endif;
024300170523           if  %check( c_Digits : %subst( KPJBU :
024400170523                                  %size( FIMS15ds.iMS15fil ) + 1 :
024500170523                                  %size( FIMS15ds.iMS15dti ) ) ) > *zero;
024600170523             clear  FIMS15ds.iMS15dti;
024700170523           endif;
024800170523           if  %check( c_Digits : %subst( KPJBU :
024900170523                                  %size( FIMS15ds.iMS15fil ) +
025000170523                                  %size( FIMS15ds.iMS15dti ) + 1 :
025100170523                                  %size( FIMS15ds.iMS15dtf ) ) ) > *zero;
025200170523             clear  FIMS15ds.iMS15dtf;
025300170523           endif;
025400170523           if  FIMS15ds.iMS15dti > *zero  and
025500170523               FIMS15ds.iMS15dtf = *zero;
025600170523             FIMS15ds.iMS15dtf = FIMS15ds.iMS15dti;
025700170523           endif;
025800170523         EndIf;
025900170523
026000170523         // -?Reperimento data odierna in formato gg/mm/aaaa?
026100170523         wDate_Eur = %date();
026200170523         wDate = %dec( wDate_Eur );
026300170523
026400170523         // -?Impostazione data limite?
026500170523         wDateMax = %dec( %date() - %days(1) );
026600170523
026700170523         // -?Reperimento orario?
026800170523         wTime = %dec( %time() );
026900170522
027000170522       ENDSR;
027100170522
027200170522       //--------------------------------------------------------------
027300170522       //?Reperimento Dati del job (Utente/Operativi).                 ?
027400170522       //--------------------------------------------------------------
027500170522       BEGSR  sr_DatiJob;
027600170522
027700170522         in(e)  §AzUte;
027800170522         if  NOT  %error;
027900170522           in(e)  §DatiUte;
028000170522         endif;
028100170522         if  %error  or  RSut = *blank;
028200170522           tibs34r ( tibs34ds );
028300170522           in  §AzUte;
028400170522           in  §DatiUte;
028500170522         endif;
028600170522
028700170522       ENDSR;
028800170522
028900170522       //--------------------------------------------------------------
029000170523       //?Preparazione stringa SQL - per estrazione dati da FIMIP.     ?
029100170522       //--------------------------------------------------------------
029200170522       BEGSR  sr_PrepSQL;
029300170522
029400170524         wSQL  = 'select MIPfil, MIPnpg, MIPtip, MIPpep, +
029500170524                         MIPdti, MIPdtf, MIPhmi, MIPhmf +
029600170523                    from FIMIP00F';
029700170523
029800170523         if  FIMS15ds.iMS15fil > *zero;
029900170524           wSQL +=
030000170524                 ' where MIPfil = ' + %editc( FIMS15ds.iMS15fil : 'X' );
030100170524         endif;
030200170523
030300170524         wSQL += ' order by MIPfil, MIPnpg, MIPtip, MIPpep, +
030400170524                         MIPdti, MIPdtf, MIPhmi, MIPhmf +
030500170522                     for fetch only';
030600170522
030700170522       ENDSR;
030800170522
030900170522       //--------------------------------------------------------------
031000170522       //?Apertura cursore.                                            ?
031100170522       //--------------------------------------------------------------
031200170522       BEGSR  sr_OpenCursor;
031300170522
031400170524         $EoF  = *off;
031500170522
031600170522         // -?Dichiarazione cursore?
031700170522         exec sql   prepare S1   from :wSQL;
031800170522         exec sql   declare C1   cursor for S1;
031900170522
032000170522         if  SQLcode < *zero;
032100170524           $EoF  = *on;
032200170522           exsr  sr_PrintERR;
032300170522           leavesr;
032400170522         endif;
032500170522
032600170522         // -?Apertura del cursore?
032700170522         exec sql   open C1;
032800170522
032900170522         if  SQLcode < *zero;
033000170524           $EoF  = *on;
033100170522           exsr  sr_PrintERR;
033200170522           leavesr;
033300170522         endif;
033400170522
033500170522       ENDSR;
033600170522
033700170522       //--------------------------------------------------------------
033800170522       //?Chiusura cursore.                                            ?
033900170522       //--------------------------------------------------------------
034000170522       BEGSR  sr_CloseCursor;
034100170522
034200170522         // -?Chiusura del cursore?
034300170522         exec sql   close C1;
034400170522
034500170522       ENDSR;
034600170522
034700170522       //--------------------------------------------------------------
034800170523       //?Lettura cursore.                                             ?
034900170522       //--------------------------------------------------------------
035000170522       BEGSR  sr_ReadCursor;
035100170522
035200170524         clear  FIMIP_ds;
035300170522
035400170524         exec sql   fetch next   from C1   into :FIMIP_ds;
035500170522
035600170522         Select;
035700170522
035800170522           // -?E.o.F.?
035900170522           When  SQLcode = 100;
036000170524             $EoF = *on;
036100170522
036200170522           // -?Errore?
036300170522           When  SQLcode < *zero;
036400170522             $EoF = *on;
036500170522             exsr  sr_PrintERR;
036600170522
036700170522           // -?Elaborazione?
036800170522           Other;
036900170523             exsr  sr_Elab_FIMIP;
037000170522
037100170522         EndSl;
037200170522
037300170522       ENDSR;
037400170523
037500170523       //--------------------------------------------------------------
037600170523       //?Elaborazione singolo record del *file FIMIP00F.              ?
037700170523       //--------------------------------------------------------------
037800170523       BEGSR  sr_Elab_FIMIP;
037900170524
038000170524         // -?Impostazione date e orari limite per elaborazione?
038100170525         // ·?Data inizio elaborazione?
038200170524         select;
038300170524           when  FIMS15ds.iMS15dti >  *zero  and
038400170524                 FIMS15ds.iMS15dti >= MIPdti;
038500170524             wDataOraIni = %editc( FIMS15ds.iMS15dti : 'X' );
038600170524           when  FIMS15ds.iMS15dti <= *zero  and
038700170524                 wDateMax >= MIPdti;
038800170524             wDataOraIni = %editc( wDateMax : 'X' );
038900170524           other;
039000170524             leavesr;
039100170524         endsl;
039200170525         // ·?Orario inizio elaborazione?
039300170524         if  MIPhmi <> *zero;
039400170524           wDataOraIni = %trimR( wDataOraIni ) + %editc( MIPhmi : 'X' )
039500170524                                               + '00';
039600170524         else;
039700170524           wDataOraIni = %trimR( wDataOraIni ) + '000000';
039800170524         endif;
039900170525         // ·?Data fine elaborazione?
040000170524         select;
040100170524           when  FIMS15ds.iMS15dtf >  *zero  and
040200170524                 FIMS15ds.iMS15dtf <= MIPdtf;
040300170524             wDataOraFin = %editc( FIMS15ds.iMS15dtf : 'X' );
040400170524           when  FIMS15ds.iMS15dtf <= *zero  and
040500170524                 wDateMax <= MIPdtf;
040600170524             wDataOraFin = %editc( wDateMax : 'X' );
040700170524           other;
040800170524             leavesr;
040900170524         endsl;
041000170525         // ·?Orario fine elaborazione?
041100170524         if  MIPhmf <> *zero;
041200170524           wDataOraFin = %trimR( wDataOraFin ) + %editc( MIPhmf : 'X' )
041300170524                                               + '99';
041400170524         else;
041500170524           wDataOraFin = %trimR( wDataOraFin ) + '999999';
041600170524         endif;
041700170523
041800170525
041900170525         Select;
042000170525
042100170525           // -?Estrazione letture da Pistola da FIPSL?
042200170525           When  MIPtip = 'P';
042300170523             exsr  sr_Pistola;
042400170525
042500170525           // -?Estrazione letture da Banco da FLTR7?
042600170525           When  MIPtip = 'F';
042700170523             exsr  sr_Banco;
042800170525
042900170524           // -?? ? ??
043000170525           Other;
043100170524             clear  wErr;
043200170523             exsr  sr_Errore;
043300170523             leavesr;
043400170525
043500170525         EndSl;
043600170523
043700170523       ENDSR;
043800170522
043900170522       //--------------------------------------------------------------
044000170523       //?Elaborazioni letture da Pistola.                             ?
044100170522       //--------------------------------------------------------------
044200170523       BEGSR  sr_Pistola;
044300170523
044400170524         $EoF2 = *off;
044500170523
044600170523         // -?Preparazione stringa SQL per estrazione dati?
044700170523         exsr  sr_PrepSQL_FIPSL;
044800170523
044900170523         // -?Apertura Cursore?
045000170523         exsr  sr_OpenCursor_FIPSL;
045100170523
045200170523         // -?Ciclo di elaborazione?
045300170524         DoW  $EoF2 = *off;
045400170523           exsr  sr_ReadCursor_FIPSL;
045500170523         EndDo;
045600170523
045700170523         // -?Chiusura Cursore?
045800170523         exsr  sr_CloseCursor_FIPSL;
045900170522
046000170522       ENDSR;
046100170523
046200170523       //--------------------------------------------------------------
046300170523       //?Elaborazioni letture da Banco.                               ?
046400170523       //--------------------------------------------------------------
046500170523       BEGSR  sr_Banco;
046600170523
046700170524         $EoF3 = *off;
046800170523
046900170523         // -?Preparazione stringa SQL per estrazione dati?
047000170523         exsr  sr_PrepSQL_FLTR7;
047100170523
047200170523         // -?Apertura Cursore?
047300170523         exsr  sr_OpenCursor_FLTR7;
047400170523
047500170523         // -?Ciclo di elaborazione?
047600170524         DoW  $EoF3 = *off;
047700170523           exsr  sr_ReadCursor_FLTR7;
047800170523         EndDo;
047900170523
048000170523         // -?Chiusura Cursore?
048100170523         exsr  sr_CloseCursor_FLTR7;
048200170523
048300170523       ENDSR;
048400170523
048500170523       //--------------------------------------------------------------
048600170523       //?Preparazione stringa SQL - per estrazione dati da FIPSL.     ?
048700170523       //--------------------------------------------------------------
048800170523       BEGSR  sr_PrepSQL_FIPSL;
048900170524
049000170523
049100170523         wSQL2 = 'select distinct PSLfil, PSLnpg, PSLusr, PSLbarCo +
049200170523                    from FIPSL00F +
049300170523                   where PSLfil = ''' + %editc( MIPfil : 'X' ) + ''' +
049400170524                     and PSLnpg = ''' + %editc( MIPnpg : 'X' ) + ''' +
049500170523                     and PSLerr = ''  '' +
049600170524                     and PSLusr like ''%' + %editc( MIPfil : 'X' ) +
049700170524                                            %trimR( MIPpep ) + ''' +
049800170523                     and PSLdtOraR between ''' + wDataOraIni + ''' +
049900170523                                       and ''' + wDataOraFin + ''' +
050000170523                order by PSLfil, PSLnpg, PSLusr, PSLbarCo +
050100170523                     for fetch only';
050200170523
050300170523       ENDSR;
050400170523
050500170523       //--------------------------------------------------------------
050600170523       //?Apertura cursore - per estrazione dati da FIPSL.             ?
050700170523       //--------------------------------------------------------------
050800170523       BEGSR  sr_OpenCursor_FIPSL;
050900170523
051000170524         $EoF2 = *off;
051100170523
051200170523         // -?Dichiarazione cursore?
051300170523         exec sql   prepare S2   from :wSQL2;
051400170523         exec sql   declare C2   cursor for S2;
051500170523
051600170523         if  SQLcode < *zero;
051700170524           $EoF2 = *on;
051800170523           exsr  sr_PrintERR;
051900170523           leavesr;
052000170523         endif;
052100170523
052200170523         // -?Apertura del cursore?
052300170523         exec sql   open C2;
052400170523
052500170523         if  SQLcode < *zero;
052600170524           $EoF2 = *on;
052700170523           exsr  sr_PrintERR;
052800170523           leavesr;
052900170523         endif;
053000170523
053100170523       ENDSR;
053200170523
053300170523       //--------------------------------------------------------------
053400170523       //?Chiusura cursore - per estrazione dati da FIPSL.             ?
053500170523       //--------------------------------------------------------------
053600170523       BEGSR  sr_CloseCursor_FIPSL;
053700170523
053800170523         // -?Chiusura del cursore?
053900170523         exec sql   close C2;
054000170525
054100170525         if  SQLcode < *zero;
054200170525           $EoF2 = *on;
054300170525           exsr  sr_PrintERR;
054400170525           leavesr;
054500170525         endif;
054600170523
054700170523       ENDSR;
054800170523
054900170523       //--------------------------------------------------------------
055000170523       //?Lettura cursore - per estrazione dati da FIPSL.              ?
055100170523       //--------------------------------------------------------------
055200170523       BEGSR  sr_ReadCursor_FIPSL;
055300170523
055400170523         clear  wSQL_FIPSL_ds;
055500170523
055600170523         exec sql   fetch next   from C2   into :wSQL_FIPSL_ds;
055700170523
055800170523         Select;
055900170523
056000170523           // -?E.o.F.?
056100170523           When  SQLcode = 100;
056200170524             $EoF2 = *on;
056300170523
056400170523           // -?Errore?
056500170523           When  SQLcode < *zero;
056600170524             $EoF2 = *on;
056700170523             exsr  sr_PrintERR;
056800170523
056900170523           // -?Elaborazione?
057000170523           Other;
057100170524             clear  wBarCode;
057200170524             wBarCode = wSQL_FIPSL_ds.PSLbarCo;
057300170524             exsr  sr_Write_FIMIS;
057400170523
057500170523         EndSl;
057600170523
057700170523       ENDSR;
057800170523
057900170523       //--------------------------------------------------------------
058000170523       //?Preparazione stringa SQL - per estrazione dati da FLTR7.     ?
058100170523       //--------------------------------------------------------------
058200170523       BEGSR  sr_PrepSQL_FLTR7;
058300170523
058400170523         wSQL3 = 'select distinct TR7fil, TR7ctl, TR7pep, TR7bar +
058500170523                    from FLTR700F +
058600170523                   where TR7fil = ''' + %editc( MIPfil : 'X' ) + ''' +
058700170524                     and TR7ctl = ''' + %editc( MIPnpg : 'X' ) + ''' +
058800170523                     and TR7crc = ''  '' +
058900170524                     and TR7pep like ''%' + %trimR( MIPpep ) + ''' +
059000170523                     and TR7bar > ''00000000000000000'' +
059100170523                     and TR7tim between ''' + wDataOraIni + ''' +
059200170523                                    and ''' + wDataOraFin + ''' +
059300170523                   order by TR7fil, TR7ctl, TR7pep, TR7bar +
059400170523                     for fetch only';
059500170523
059600170523       ENDSR;
059700170523
059800170523       //--------------------------------------------------------------
059900170523       //?Apertura cursore - per estrazione dati da FLTR7.             ?
060000170523       //--------------------------------------------------------------
060100170523       BEGSR  sr_OpenCursor_FLTR7;
060200170523
060300170524         $EoF3 = *off;
060400170523
060500170523         // -?Dichiarazione cursore?
060600170523         exec sql   prepare S3   from :wSQL3;
060700170523         exec sql   declare C3   cursor for S3;
060800170523
060900170523         if  SQLcode < *zero;
061000170524           $EoF3 = *on;
061100170523           exsr  sr_PrintERR;
061200170523           leavesr;
061300170523         endif;
061400170523
061500170523         // -?Apertura del cursore?
061600170523         exec sql   open C3;
061700170523
061800170523         if  SQLcode < *zero;
061900170524           $EoF3 = *on;
062000170523           exsr  sr_PrintERR;
062100170523           leavesr;
062200170523         endif;
062300170523
062400170523       ENDSR;
062500170523
062600170523       //--------------------------------------------------------------
062700170523       //?Chiusura cursore - per estrazione dati da FLTR7.             ?
062800170523       //--------------------------------------------------------------
062900170524       BEGSR  sr_CloseCursor_FLTR7;
063000170523
063100170523         // -?Chiusura del cursore?
063200170523         exec sql   close C3;
063300170525
063400170525         if  SQLcode < *zero;
063500170525           $EoF3 = *on;
063600170525           exsr  sr_PrintERR;
063700170525           leavesr;
063800170525         endif;
063900170523
064000170523       ENDSR;
064100170523
064200170523       //--------------------------------------------------------------
064300170523       //?Lettura cursore - per estrazione dati da FLTR7.              ?
064400170523       //--------------------------------------------------------------
064500170523       BEGSR  sr_ReadCursor_FLTR7;
064600170523
064700170523         clear  wSQL_FLTR7_ds;
064800170523
064900170523         exec sql   fetch next   from C3   into :wSQL_FLTR7_ds;
065000170523
065100170523         Select;
065200170523
065300170523           // -?E.o.F.?
065400170523           When  SQLcode = 100;
065500170524             $EoF3 = *on;
065600170523
065700170523           // -?Errore?
065800170523           When  SQLcode < *zero;
065900170524             $EoF3 = *on;
066000170523             exsr  sr_PrintERR;
066100170523
066200170523           // -?Elaborazione?
066300170523           Other;
066400170524             clear  wBarCode;
066500170524             wBarCode = wSQL_FLTR7_ds.TR7bar;
066600170524             exsr  sr_Write_FIMIS;
066700170523
066800170523         EndSl;
066900170523
067000170523       ENDSR;
067100170522
067200170522       //--------------------------------------------------------------
067300170522       //?Controllo/Scrittura record in FIMIS00F.                      ?
067400170522       //--------------------------------------------------------------
067500170522       BEGSR  sr_Write_FIMIS;
067600170524
067700170524         // -?Reperimento n° spedizione dal segnacollo in esame?
067800170524         clear  keyFIART27;
067900170524         k_ARTfls = wBarCode.wBar_LNP;
068000170524         k_ARTlna = wBarCode.wBar_LNA;
068100170524         k_ARTnrs = wBarCode.wBar_NRS;
068200170524         k_ARTnsc = wBarCode.wBar_NSC;
068300170524         chain  %kds( keyFIART27 )  FIART000;
068400170524
068500170524         if  not %found( FIART27L );
068600170524           wErr = 1;
068700170524           exsr  sr_Errore;
068800170524           leavesr;
068900170524         endif;
069000170522
069100170522         // -?Controllo se esiste già un rec. per la spedizione?
069200170524         clear  keyFIMIS01;
069300170524         k_MISaas = ARTaas;
069400170524         k_MISlnp = ARTlnp;
069500170524         k_MISnrs = ARTnrs;
069600170524         k_MISnsp = ARTnsp;
069700170524         setll  %kds( keyFIMIS01 )  FIMIS000;
069800170524
069900170524         if  %equal( FIMIS01L );
070000170524           leavesr;
070100170524         endif;
070200170524
070300170524         // -?Aggancio FNARB00F?
070400170524         clear  keyFNARB01;
070500170524         k_ARBaas = ARTaas;
070600170524         k_ARBlnp = ARTlnp;
070700170524         k_ARBnrs = ARTnrs;
070800170524         k_ARBnsp = ARTnsp;
070900170524         chain  %kds( keyFNARB01 )  FNARB000;
071000170524
071100170524         if  not %found( FNARB01L );
071200170524           wErr = 2;
071300170524           exsr  sr_Errore;
071400170524           leavesr;
071500170524         endif;
071600170524
071700170524         // -?Reperimento dati del Codice Bolla?
071800170524         If  ARBcbo <> ds_TNTBE.TBEke1;
071900170524           clear  ds3A;
072000170524           ds_TNTBE.TBEke1 = ARBcbo;
072100170524           if  getTabella ( c_Tabel : '3A'  : ds_TNTBE.TBEke1 :
072200170524                            *omit : *omit : *omit :
072300170524                            *omit : *omit :
072400170524                            *omit : *omit : *omit : *omit :
072500170524                            *omit : *omit : *omit : *omit :
072600170524                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
072700170524                          = *zero      AND
072800170524               ds_TNTBE.TBEatb = *blank;
072900170524             ds3A   = ds_TNTBE.TBEuni;
073000170524           endif;
073100170524         EndIf;
073200170524
073300170524         // -?Scrittura record in FIMIS00F?
073400170524         clear  FIMIS000;
073500170524         MISaas = ARBaas;
073600170524         MISmgs = ARBmgs;
073700170524         MISlnp = ARBlnp;
073800170524         MISnrs = ARBnrs;
073900170524         MISnsp = ARBnsp;
074000170524         select;
074100170524           when  ARBccm <> *zero;
074200170524             MISccm = ARBccm;
074300170524           when  %subst( ds3A.§3Atb1 : 1 : 1 ) = 'F';
074400170524             MISccm = ARBksc;
074500170524         endsl;
074600170524         MISctr = ARBctr;
074700170524         MISpkf = ARBpkf;
074800170524         MISncl = ARBncl;
074900170524
075000170524         //______________
075100170524         write  FIMIS000;
075200170524         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
075300170522
075400170522       ENDSR;
075500170523
075600170523       //--------------------------------------------------------------
075700170523       //?Stampa segnalazione dell'errore rilevato via SQL.            ?
075800170523       //--------------------------------------------------------------
075900170523       BEGSR  sr_Errore;
076000170523
076100170523         // -?Apertura *file?
076200170523         if  Not  %open(QSYSPRT);
076300170523           open  QSYSPRT;
076400170523           *inOF = *on;
076500170523         endif;
076600170523
076700170523         // -?Stampa testata?
076800170523         if  *inOF;
076900170523           except ErrTXT;
077000170523           *inOF = *off;
077100170523         endif;
077200170523
077300170523         // -?Stampa dettaglio?
077400170524         clear  wErrMsg;
077500170524         select;
077600170524           when  wErr = *zero;
077700170524             wErrMsg = sk_Msg(1);
077800170524           when  wErr = 1;
077900170524             wErrMsg = %trimR( sk_Msg(2) ) + ' ' + wBarCode;
078000170524           when  wErr = 2;
078100170524             wErrMsg = %trimR( sk_Msg(3) )    + ' ' +
078200170524                       %editc( ARTaas : '3' ) + '-' +
078300170524                       %editc( ARTlnp : 'X' ) + '-' +
078400170524                       %editc( ARTnrs : '3' ) + '-' +
078500170524                       %editc( ARTnsp : '3' );
078600170524           other;
078700170524             wErrMsg = sk_Msg(4);
078800170524         endsl;
078900170524
079000170524         except  ErrDET;
079100170524
079200170524         clear  wErr;
079300170523
079400170523       ENDSR;
079500170522
079600170522       //--------------------------------------------------------------
079700170522       //?Stampa segnalazione dell'errore rilevato via SQL.            ?
079800170522       //--------------------------------------------------------------
079900170522       BEGSR  sr_PrintERR;
080000170522
080100170522         // -?Stampa del Dump?
080200170522         Dump(A);
080300170522
080400170522         // -?Stampa del Job-Log?
080500170522         Qcmd = 'DSPJOBLOG job(*) output(*print)';
080600170522         exsr  sr_ExecCmd;
080700170522
080800170522       ENDSR;
080900170522
081000170522       //--------------------------------------------------------------
081100170522       //?Operazioni finali.                                           ?
081200170522       //--------------------------------------------------------------
081300170522       BEGSR  sr_RoutEnd;
081400170523
081500170523         // -?Chiusura file di stampa errori?
081600170523         if  %open(QSYSPRT);
081700170523           except  ErrEND;
081800170523           close   QSYSPRT;
081900170523         endif;
082000170522
082100170522         // -?Uscita dal *pgm?
082200170522         return;
082300170522
082400170522       ENDSR;
082500170522
082600170522       //--------------------------------------------------------------
082700170522       //?Esecuzione del comando (già impostato).                      ?
082800170522       //--------------------------------------------------------------
082900170522       BEGSR  sr_ExecCmd;
083000170522
083100170522         clear Qcap0100;
083200170522         Qcabcsdh = *off;
083300170522         Qcapa    = *off;
083400170522         Qcacmdss = *off;
083500170522         Qcaerved = *allX'00';
083600170522
083700170522         clear Qusec;
083800170522         Qusbprv  = %size(Qusec);
083900170522
084000170522         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
084100170522                           %size(Qcap0100) : 'CPOP0100' : *omit :
084200170522                           0 : 0 : Qusec);
084300170522
084400170522         //if  Qusei <> *blank;
084500170522         //  ...;
084600170522         //endif;
084700170522
084800170522       ENDSR;
084900170522
085000170522      /end-free
085100170523
085200170523       //--------------------------------------------------------------
085300170523       //?S P O O L - F I L E S                                        ?
085400170523       //--------------------------------------------------------------
085500170523
085600170523     oQSYSPRT   e            ErrTXT            1
085700170523     o                       RSUT
085800170523     o                                        +   6 'LISTA ERRORI RILE-
085900170523     o                                              VATI IN FASE DI CR-
086000170523     o                                              EAZIONE FILE FIMIS-
086100170523     o                                              00F'
086200170524     o                       SDSpgmName       +   6
086300170523     o                       wDate         y  +   3
086400170523     o          e            ErrTXT      0
086500170523     o                                          +26 'LISTA ERRORI RILE-
086600170523     o                                              VATI IN FASE DI CR-
086700170523     o                                              EAZIONE FILE FIMIS-
086800170523     o                                              00F'
086900170523     o          e            ErrTXT      1
087000170523     o                       SDSusr           +  11
087100170523     o                                        +   5 '------------------
087200170523     o                                              -------------------
087300170523     o                                              -------------------
087400170523     o                                              ---'
087500170523     o                                        +   6 'Pag.'
087600170523     o                       Page          z  +   0
087700170523     o                       wTime            +   5 '  :  :  '
087800170523      *
087900170523     o          e            ErrTXT      2
088000170523     o                                              'Fil'
088100170523     o                                        +   1 'Cat.Foglio'
088200170523     o                                        +   1 'Tipo Post.'
088300170523     o                                        +   1 'Postazione    '
088400170523     o                                        +   1 'Errore    '
088500170523     o          e            ErrTXT      0
088600170523     o                                              'Fil'
088700170523     o                                        +   1 'Cat.Foglio'
088800170523     o                                        +   1 'Tipo Post.'
088900170523     o                                        +   1 'Postazione    '
089000170523     o                                        +   1 'Errore    '
089100170523     o          e            ErrTXT      1
089200170523     o                                              '----
089300170523     o                                              ------------
089400170523     o                                              ------------
089500170523     o                                              ----------------
089600170523     o                                              -----------'
089700170523     o          e            ErrTXT      0  1
089800170523     o                                              '----
089900170523     o                                              ------------
090000170523     o                                              ------------
090100170523     o                                              ----------------
090200170523     o                                              -----------'
090300170523      *
090400170523     o          e            ErrDET      1
090500170524     o                       MIPfil        x      3
090600170524     o                       MIPnpg              10
090700170524     o                       MIPtip              22
090800170524     o                       MIPpep              43
090900170524     o                       wErrMsg            125
091000170523      *
091100170523     o          e            ErrEND      2
091200170523     o                                        +  10 '***  Fine Lista  -
091300170523     o                                              ***'
091400170523     o          e            ErrEND      0
091500170523     o                                        +  10 '***  Fine Lista  -
091600170523     o                                              ***'
091700170522
091800170522       //--------------------------------------------------------------
091900170522       //?Definizione schiere a tempo di compilazione                  ?
092000170522       //--------------------------------------------------------------
092100170522
092200170522** --?sk_Npg:?Turni elaborati?
092300170522PARTENZE  5
092400170522ARRIVI    2
092500170524** --?sk_Msg:?Errori in stampa?----------------------------------------------*
092600170524Rilevati dati ERRATI nel file FIMIP00F                                           1
092700170524Non trovato segnacollo                                                           2
092800170524Non trovata spedizione                                                           3
092900170524? ? ? ? ?                                                                        4
