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
003500170522
003600170522       //--------------------------------------------------------------
003700170522       //?Definizione costanti.                                        ?
003800170522       //--------------------------------------------------------------
003900170522
004000170524       // -?Costante per controllo "caratteri solo numerici"?
004100170524     d c_Digits        c                   const('0123456789')
004200170522
004300170522       //--------------------------------------------------------------
004400170522       //?Definizione schiere.                                         ?
004500170522       //--------------------------------------------------------------
004600170522
004700170522
004800170522       //--------------------------------------------------------------
004900170522       //?Definizione aree dati.                                       ?
005000170522       //--------------------------------------------------------------
005100170522
005200170522       // -?Dati utente?
005300170522     d §AzUte        e ds                  extname(AZUTE00F)
005400170522     d                                     dtaara
005500170522     d §DatiUte      e ds                  extname(dDatiUte)
005600170522     d                                     dtaara
005700170522
005800170522       //--------------------------------------------------------------
005900170522       //?Definizione strutture dati.                                  ?
006000170522       //--------------------------------------------------------------
006100170522
006200170522       // -?Status ds?
006300170522     d Status         sds
006400170522     d   SDSpgmName      *proc
006500170522     d*//SDSdta              191    198
006600170526     d*//SDSjob              244    253
006700170526     d*//SDSusr              254    263
006800170522
006900170522       // -?Parametri ricevuti?
007000170523     d KPJBA         e ds
007100170522     d FIMS15ds        ds                  inz   qualified
007200170523     d   iMS15fil                     3s 0 inz
007300170524     d   iMS15dti                     8s 0 inz
007400170524     d   iMS15dtf                     8s 0 inz
007500170523     d*//iMS15npg                     1s 0 inz
007600170522
007700170522       // -?Dati estratti via SQL?
007800170524     d FIMIP_ds      e ds                  extname(FIMIP00F)
007900170524     d                                     inz
008000170523
008100170523     d FIPSL00F      e ds                  based(nullPtr)
008200170523     d                                     qualified
008300170523     d wSQL_FIPSL_ds   ds                  inz   qualified
008400170523     d   pslFIL                            inz   like(FIPSL00F.PSLfil)
008500170523     d   pslNPG                            inz   like(FIPSL00F.PSLnpg)
008600170523     d   pslUSR                            inz   like(FIPSL00F.PSLusr)
008700170523     d   pslBARco                          inz   like(FIPSL00F.PSLbarCo)
008800170523
008900170523     d FLTR700F      e ds                  based(nullPtr)
009000170523     d                                     qualified
009100170523     d wSQL_FLTR7_ds   ds                  inz   qualified
009200170523     d   tr7FIL                            inz   like(FLTR700F.TR7fil)
009300170523     d   tr7NPG                            inz   like(FLTR700F.TR7ctl)
009400170523     d   tr7PEP                            inz   like(FLTR700F.TR7pep)
009500170523     d   tr7BAR                            inz   like(FLTR700F.TR7bar)
009600170523
009700170523       // -?BarCode Segnacollo?
009800170524     d wBarCode        ds            18    inz   qualified
009900170524     d   wBar_LNP                     3s 0 inz
010000170524     d   wBar_LNA                     3s 0 inz
010100170524     d   wBar_NRS                     2s 0 inz
010200170524     d   wBar_NSC                     7s 0 inz
010300170524     d   wBar_ZNC                     2s 0 inz
010400170524     d   wBar_CHK                     1s 0 inz
010500170524
010600170524       // -?Tabelle?
010700170524     d ds3A          e ds                  inz   qualified
010800170522
010900170522       //--------------------------------------------------------------
011000170522       //?Definizione variabili globali.                               ?
011100170522       //--------------------------------------------------------------
011200170522
011300170522       // -?Flags booleani?
011400170522     d $EoF            s               n   inz
011500170524     d $EoF2           s               n   inz
011600170524     d $EoF3           s               n   inz
011700170522
011800170522       // -?Stringhe SQL da eseguire?
011900170522     d wSQL            s          32740    inz   varying
012000170523     d wSQL2           s          32740    inz   varying
012100170523     d wSQL3           s          32740    inz   varying
012200170523
012300170523       // -?Data/Ora limite (dati alfanumerici)?
012400170523     d wDataOraIni     s             14    inz
012500170523     d wDataOraFin     s             14    inz
012600170522
012700170523       // -?Data in formato *EUR?
012800170523     d wDate_Eur       s               d   inz   datfmt(*EUR)
012900170523
013000170523       // -?Campi di Comodo?
013100170523     d wDateMax        s              8  0 inz
013200170523     d wDate           s              8  0 inz
013300170523     d wTime           s              6  0 inz
013400170522
013500170522       //--------------------------------------------------------------
013600170522       //?Definizione prototipi procedure.                             ?
013700170522       //--------------------------------------------------------------
013800170522
013900170522       // -?Reperimento dati utente?
014000170522     d TIBS34ds      e ds                  inz
014100170522      /copy gaitrasrc/srcProtoPR,TIBS34R
014200170522
014300170522       // -?Reperimento dati tabelle?
014400170522      /copy gaitrasrc/srcProtoPI,TRULTAB
014500170522      /copy gaitrasrc/srcProtoPR,TRULTAB
014600170522
014700170522       // -?Parametri API QCAPCMD (Process Commands)?
014800170522     d Qcmd            s           2048    inz   varying
014900170522      /copy qSysInc/qRpgleSrc,QCAPCMD
015000170522       // -?API QCAPCMD (Process Commands)?
015100170522      /copy gaitrasrc/srcProtoPR,QCAPCMD
015200170522
015300170522       // -?Parametri gestione errori API.?
015400170522      /copy qsysinc/qrpglesrc,QUSEC
015500170522
015600170522       //--------------------------------------------------------------
015700170522       //?Definizione key-list.                                        ?
015800170522       //--------------------------------------------------------------
015900170522
016000170524       // -?File FIART27L?
016100170524     d keyFIART27    e ds                  extname( FIART27L : *key )
016200170524     d                                     prefix( k_ )  inz
016300170524
016400170524       // -?File FNARB01L?
016500170524     d keyFNARB01    e ds                  extname( FNARB01L : *key )
016600170524     d                                     prefix( k_ )  inz
016700170524
016800170524       // -?File FIMIS01L?
016900170524     d keyFIMIS01    e ds                  extname( FIMIS01L : *key )
017000170524     d                                     prefix( k_ )  inz
017100170522
017200170522       //--------------------------------------------------------------
017300170522       //?Riepilogo indicatori utilizzati.                             ?
017400170522       //--------------------------------------------------------------
017500170522
017600170522
017700170522       //--------------------------------------------------------------
017800170522       //?M A I N - L I N E                                            ?
017900170522       //--------------------------------------------------------------
018000170522
018100170522     c     *Entry        plist
018200170522     c                   parm                    KPJBA
018300170522
018400170522      /free
018500170522
018600170522       // -?Operazioni iniziali?
018700170522       exsr  sr_RoutInz;
018800170522
018900170522       // -?Preparazione stringa SQL per estrazione dati?
019000170522       exsr  sr_PrepSQL;
019100170522
019200170522       // -?Apertura Cursore?
019300170522       exsr  sr_OpenCursor;
019400170522
019500170523       // -?Ciclo di elaborazione?
019600170522       DoW  $EoF = *off;
019700170522         exsr  sr_ReadCursor;
019800170522       EndDo;
019900170522
020000170522       // -?Chiusura Cursore?
020100170522       exsr  sr_CloseCursor;
020200170522
020300170522       // -?Operazioni finali?
020400170522       exsr sr_RoutEnd;
020500170522
020600170522       //--------------------------------------------------------------
020700170522       //?Operazioni iniziali.                                         ?
020800170522       //--------------------------------------------------------------
020900170522       BEGSR  sr_RoutInz;
021000170522
021100170522         // -?Impostazione opzioni per SQL?
021200170522         exec sql   set  option  DynUsrPrf = *Owner,
021300170522                                 CloSqlCsr = *EndMod;
021400170522
021500170522         // -?Impostazione chiusura?
021600170522         *inLR = *on;
021700170522
021800170522         // -?Reperimento dati job?
021900170522         exsr  sr_DatiJob;
022000170522
022100170522         // -?Reperimento parametri?
022200170523         clear  FIMS15ds;
022300170523         If  KPJBU <> *blank;
022400170523           FIMS15ds = KPJBU;
022500170523           if  %check( c_Digits : %subst( KPJBU : 1 :
022600170523                                  %size( FIMS15ds.iMS15fil ) ) )      > *zero;
022700170523             clear  FIMS15ds.iMS15fil;
022800170523           endif;
022900170523           if  %check( c_Digits : %subst( KPJBU :
023000170523                                  %size( FIMS15ds.iMS15fil ) + 1 :
023100170523                                  %size( FIMS15ds.iMS15dti ) ) ) > *zero;
023200170523             clear  FIMS15ds.iMS15dti;
023300170523           endif;
023400170523           if  %check( c_Digits : %subst( KPJBU :
023500170523                                  %size( FIMS15ds.iMS15fil ) +
023600170523                                  %size( FIMS15ds.iMS15dti ) + 1 :
023700170523                                  %size( FIMS15ds.iMS15dtf ) ) ) > *zero;
023800170523             clear  FIMS15ds.iMS15dtf;
023900170523           endif;
024000170523           if  FIMS15ds.iMS15dti > *zero  and
024100170523               FIMS15ds.iMS15dtf = *zero;
024200170523             FIMS15ds.iMS15dtf = FIMS15ds.iMS15dti;
024300170523           endif;
024400170523         EndIf;
024500170523
024600170523         // -?Reperimento data odierna in formato gg/mm/aaaa?
024700170523         wDate_Eur = %date();
024800170523         wDate = %dec( wDate_Eur );
024900170523
025000170523         // -?Impostazione data limite?
025100170523         wDateMax = %dec( %date() - %days(1) );
025200170523
025300170523         // -?Reperimento orario?
025400170523         wTime = %dec( %time() );
025500170522
025600170522       ENDSR;
025700170522
025800170522       //--------------------------------------------------------------
025900170522       //?Reperimento Dati del job (Utente/Operativi).                 ?
026000170522       //--------------------------------------------------------------
026100170522       BEGSR  sr_DatiJob;
026200170522
026300170522         in(e)  §AzUte;
026400170522         if  NOT  %error;
026500170522           in(e)  §DatiUte;
026600170522         endif;
026700170522         if  %error  or  RSut = *blank;
026800170522           tibs34r ( tibs34ds );
026900170522           in  §AzUte;
027000170522           in  §DatiUte;
027100170522         endif;
027200170522
027300170522       ENDSR;
027400170522
027500170522       //--------------------------------------------------------------
027600170523       //?Preparazione stringa SQL - per estrazione dati da FIMIP.     ?
027700170522       //--------------------------------------------------------------
027800170522       BEGSR  sr_PrepSQL;
027900170522
028000170524         wSQL  = 'select MIPfil, MIPnpg, MIPtip, MIPpep, +
028100170524                         MIPdti, MIPdtf, MIPhmi, MIPhmf +
028200170523                    from FIMIP00F';
028300170523
028400170523         if  FIMS15ds.iMS15fil > *zero;
028500170524           wSQL +=
028600170524                 ' where MIPfil = ' + %editc( FIMS15ds.iMS15fil : 'X' );
028700170524         endif;
028800170523
028900170524         wSQL += ' order by MIPfil, MIPnpg, MIPtip, MIPpep, +
029000170524                         MIPdti, MIPdtf, MIPhmi, MIPhmf +
029100170522                     for fetch only';
029200170522
029300170522       ENDSR;
029400170522
029500170522       //--------------------------------------------------------------
029600170522       //?Apertura cursore.                                            ?
029700170522       //--------------------------------------------------------------
029800170522       BEGSR  sr_OpenCursor;
029900170522
030000170524         $EoF  = *off;
030100170522
030200170522         // -?Dichiarazione cursore?
030300170522         exec sql   prepare S1   from :wSQL;
030400170522         exec sql   declare C1   cursor for S1;
030500170522
030600170522         if  SQLcode < *zero;
030700170524           $EoF  = *on;
030800170522           exsr  sr_PrintERR;
030900170522           leavesr;
031000170522         endif;
031100170522
031200170522         // -?Apertura del cursore?
031300170522         exec sql   open C1;
031400170522
031500170522         if  SQLcode < *zero;
031600170524           $EoF  = *on;
031700170522           exsr  sr_PrintERR;
031800170522           leavesr;
031900170522         endif;
032000170522
032100170522       ENDSR;
032200170522
032300170522       //--------------------------------------------------------------
032400170522       //?Chiusura cursore.                                            ?
032500170522       //--------------------------------------------------------------
032600170522       BEGSR  sr_CloseCursor;
032700170522
032800170522         // -?Chiusura del cursore?
032900170522         exec sql   close C1;
033000170522
033100170522       ENDSR;
033200170522
033300170522       //--------------------------------------------------------------
033400170523       //?Lettura cursore.                                             ?
033500170522       //--------------------------------------------------------------
033600170522       BEGSR  sr_ReadCursor;
033700170522
033800170524         clear  FIMIP_ds;
033900170522
034000170524         exec sql   fetch next   from C1   into :FIMIP_ds;
034100170522
034200170522         Select;
034300170522
034400170522           // -?E.o.F.?
034500170522           When  SQLcode = 100;
034600170524             $EoF = *on;
034700170522
034800170522           // -?Errore?
034900170522           When  SQLcode < *zero;
035000170522             $EoF = *on;
035100170522             exsr  sr_PrintERR;
035200170522
035300170522           // -?Elaborazione?
035400170522           Other;
035500170523             exsr  sr_Elab_FIMIP;
035600170522
035700170522         EndSl;
035800170522
035900170522       ENDSR;
036000170523
036100170523       //--------------------------------------------------------------
036200170523       //?Elaborazione singolo record del *file FIMIP00F.              ?
036300170523       //--------------------------------------------------------------
036400170523       BEGSR  sr_Elab_FIMIP;
036500170524
036600170524         // -?Impostazione date e orari limite per elaborazione?
036700170525         // ·?Data inizio elaborazione?
036800170524         select;
036900170524           when  FIMS15ds.iMS15dti >  *zero  and
037000170524                 FIMS15ds.iMS15dti >= MIPdti;
037100170524             wDataOraIni = %editc( FIMS15ds.iMS15dti : 'X' );
037200170524           when  FIMS15ds.iMS15dti <= *zero  and
037300170524                 wDateMax >= MIPdti;
037400170524             wDataOraIni = %editc( wDateMax : 'X' );
037500170524           other;
037600170524             leavesr;
037700170524         endsl;
037800170525         // ·?Orario inizio elaborazione?
037900170524         if  MIPhmi <> *zero;
038000170524           wDataOraIni = %trimR( wDataOraIni ) + %editc( MIPhmi : 'X' )
038100170524                                               + '00';
038200170524         else;
038300170524           wDataOraIni = %trimR( wDataOraIni ) + '000000';
038400170524         endif;
038500170525         // ·?Data fine elaborazione?
038600170524         select;
038700170524           when  FIMS15ds.iMS15dtf >  *zero  and
038800170524                 FIMS15ds.iMS15dtf <= MIPdtf;
038900170524             wDataOraFin = %editc( FIMS15ds.iMS15dtf : 'X' );
039000170524           when  FIMS15ds.iMS15dtf <= *zero  and
039100170524                 wDateMax <= MIPdtf;
039200170524             wDataOraFin = %editc( wDateMax : 'X' );
039300170524           other;
039400170524             leavesr;
039500170524         endsl;
039600170525         // ·?Orario fine elaborazione?
039700170524         if  MIPhmf <> *zero;
039800170524           wDataOraFin = %trimR( wDataOraFin ) + %editc( MIPhmf : 'X' )
039900170524                                               + '99';
040000170524         else;
040100170524           wDataOraFin = %trimR( wDataOraFin ) + '999999';
040200170524         endif;
040300170523
040400170525
040500170525         Select;
040600170525
040700170525           // -?Estrazione letture da Pistola da FIPSL?
040800170525           When  MIPtip = 'P';
040900170523             exsr  sr_Pistola;
041000170525
041100170525           // -?Estrazione letture da Banco da FLTR7?
041200170525           When  MIPtip = 'F';
041300170523             exsr  sr_Banco;
041400170525
041500170525         EndSl;
041600170523
041700170523       ENDSR;
041800170522
041900170522       //--------------------------------------------------------------
042000170523       //?Elaborazioni letture da Pistola.                             ?
042100170522       //--------------------------------------------------------------
042200170523       BEGSR  sr_Pistola;
042300170523
042400170524         $EoF2 = *off;
042500170523
042600170523         // -?Preparazione stringa SQL per estrazione dati?
042700170523         exsr  sr_PrepSQL_FIPSL;
042800170523
042900170523         // -?Apertura Cursore?
043000170523         exsr  sr_OpenCursor_FIPSL;
043100170523
043200170523         // -?Ciclo di elaborazione?
043300170524         DoW  $EoF2 = *off;
043400170523           exsr  sr_ReadCursor_FIPSL;
043500170523         EndDo;
043600170523
043700170523         // -?Chiusura Cursore?
043800170523         exsr  sr_CloseCursor_FIPSL;
043900170522
044000170522       ENDSR;
044100170523
044200170523       //--------------------------------------------------------------
044300170523       //?Elaborazioni letture da Banco.                               ?
044400170523       //--------------------------------------------------------------
044500170523       BEGSR  sr_Banco;
044600170523
044700170524         $EoF3 = *off;
044800170523
044900170523         // -?Preparazione stringa SQL per estrazione dati?
045000170523         exsr  sr_PrepSQL_FLTR7;
045100170523
045200170523         // -?Apertura Cursore?
045300170523         exsr  sr_OpenCursor_FLTR7;
045400170523
045500170523         // -?Ciclo di elaborazione?
045600170524         DoW  $EoF3 = *off;
045700170523           exsr  sr_ReadCursor_FLTR7;
045800170523         EndDo;
045900170523
046000170523         // -?Chiusura Cursore?
046100170523         exsr  sr_CloseCursor_FLTR7;
046200170523
046300170523       ENDSR;
046400170523
046500170523       //--------------------------------------------------------------
046600170523       //?Preparazione stringa SQL - per estrazione dati da FIPSL.     ?
046700170523       //--------------------------------------------------------------
046800170523       BEGSR  sr_PrepSQL_FIPSL;
046900170524
047000170523         wSQL2 = 'select distinct PSLfil, PSLnpg, PSLusr, PSLbarCo +
047100170523                    from FIPSL00F +
047200170523                   where PSLfil = ''' + %editc( MIPfil : 'X' ) + ''' +
047300170524                     and PSLnpg = ''' + %editc( MIPnpg : 'X' ) + ''' +
047400170523                     and PSLerr = ''  '' +
047500170524                     and PSLusr like ''%' + %editc( MIPfil : 'X' ) +
047600170524                                            %trimR( MIPpep ) + ''' +
047700170523                     and PSLdtOraR between ''' + wDataOraIni + ''' +
047800170523                                       and ''' + wDataOraFin + ''' +
047900170523                order by PSLfil, PSLnpg, PSLusr, PSLbarCo +
048000170523                     for fetch only';
048100170523
048200170523       ENDSR;
048300170523
048400170523       //--------------------------------------------------------------
048500170523       //?Apertura cursore - per estrazione dati da FIPSL.             ?
048600170523       //--------------------------------------------------------------
048700170523       BEGSR  sr_OpenCursor_FIPSL;
048800170523
048900170524         $EoF2 = *off;
049000170523
049100170523         // -?Dichiarazione cursore?
049200170523         exec sql   prepare S2   from :wSQL2;
049300170523         exec sql   declare C2   cursor for S2;
049400170523
049500170523         if  SQLcode < *zero;
049600170524           $EoF2 = *on;
049700170523           exsr  sr_PrintERR;
049800170523           leavesr;
049900170523         endif;
050000170523
050100170523         // -?Apertura del cursore?
050200170523         exec sql   open C2;
050300170523
050400170523         if  SQLcode < *zero;
050500170524           $EoF2 = *on;
050600170523           exsr  sr_PrintERR;
050700170523           leavesr;
050800170523         endif;
050900170523
051000170523       ENDSR;
051100170523
051200170523       //--------------------------------------------------------------
051300170523       //?Chiusura cursore - per estrazione dati da FIPSL.             ?
051400170523       //--------------------------------------------------------------
051500170523       BEGSR  sr_CloseCursor_FIPSL;
051600170523
051700170523         // -?Chiusura del cursore?
051800170523         exec sql   close C2;
051900170525
052000170525         if  SQLcode < *zero;
052100170525           $EoF2 = *on;
052200170525           exsr  sr_PrintERR;
052300170525           leavesr;
052400170525         endif;
052500170523
052600170523       ENDSR;
052700170523
052800170523       //--------------------------------------------------------------
052900170523       //?Lettura cursore - per estrazione dati da FIPSL.              ?
053000170523       //--------------------------------------------------------------
053100170523       BEGSR  sr_ReadCursor_FIPSL;
053200170523
053300170523         clear  wSQL_FIPSL_ds;
053400170523
053500170523         exec sql   fetch next   from C2   into :wSQL_FIPSL_ds;
053600170523
053700170523         Select;
053800170523
053900170523           // -?E.o.F.?
054000170523           When  SQLcode = 100;
054100170524             $EoF2 = *on;
054200170523
054300170523           // -?Errore?
054400170523           When  SQLcode < *zero;
054500170524             $EoF2 = *on;
054600170523             exsr  sr_PrintERR;
054700170523
054800170523           // -?Elaborazione?
054900170523           Other;
055000170524             clear  wBarCode;
055100170524             wBarCode = wSQL_FIPSL_ds.PSLbarCo;
055200170524             exsr  sr_Write_FIMIS;
055300170523
055400170523         EndSl;
055500170523
055600170523       ENDSR;
055700170523
055800170523       //--------------------------------------------------------------
055900170523       //?Preparazione stringa SQL - per estrazione dati da FLTR7.     ?
056000170523       //--------------------------------------------------------------
056100170523       BEGSR  sr_PrepSQL_FLTR7;
056200170523
056300170523         wSQL3 = 'select distinct TR7fil, TR7ctl, TR7pep, TR7bar +
056400170523                    from FLTR700F +
056500170523                   where TR7fil = ''' + %editc( MIPfil : 'X' ) + ''' +
056600170524                     and TR7ctl = ''' + %editc( MIPnpg : 'X' ) + ''' +
056700170523                     and TR7crc = ''  '' +
056800170524                     and TR7pep like ''%' + %trimR( MIPpep ) + ''' +
056900170523                     and TR7bar > ''00000000000000000'' +
057000170523                     and TR7tim between ''' + wDataOraIni + ''' +
057100170523                                    and ''' + wDataOraFin + ''' +
057200170523                   order by TR7fil, TR7ctl, TR7pep, TR7bar +
057300170523                     for fetch only';
057400170523
057500170523       ENDSR;
057600170523
057700170523       //--------------------------------------------------------------
057800170523       //?Apertura cursore - per estrazione dati da FLTR7.             ?
057900170523       //--------------------------------------------------------------
058000170523       BEGSR  sr_OpenCursor_FLTR7;
058100170523
058200170524         $EoF3 = *off;
058300170523
058400170523         // -?Dichiarazione cursore?
058500170523         exec sql   prepare S3   from :wSQL3;
058600170523         exec sql   declare C3   cursor for S3;
058700170523
058800170523         if  SQLcode < *zero;
058900170524           $EoF3 = *on;
059000170523           exsr  sr_PrintERR;
059100170523           leavesr;
059200170523         endif;
059300170523
059400170523         // -?Apertura del cursore?
059500170523         exec sql   open C3;
059600170523
059700170523         if  SQLcode < *zero;
059800170524           $EoF3 = *on;
059900170523           exsr  sr_PrintERR;
060000170523           leavesr;
060100170523         endif;
060200170523
060300170523       ENDSR;
060400170523
060500170523       //--------------------------------------------------------------
060600170523       //?Chiusura cursore - per estrazione dati da FLTR7.             ?
060700170523       //--------------------------------------------------------------
060800170524       BEGSR  sr_CloseCursor_FLTR7;
060900170523
061000170523         // -?Chiusura del cursore?
061100170523         exec sql   close C3;
061200170525
061300170525         if  SQLcode < *zero;
061400170525           $EoF3 = *on;
061500170525           exsr  sr_PrintERR;
061600170525           leavesr;
061700170525         endif;
061800170523
061900170523       ENDSR;
062000170523
062100170523       //--------------------------------------------------------------
062200170523       //?Lettura cursore - per estrazione dati da FLTR7.              ?
062300170523       //--------------------------------------------------------------
062400170523       BEGSR  sr_ReadCursor_FLTR7;
062500170523
062600170523         clear  wSQL_FLTR7_ds;
062700170523
062800170523         exec sql   fetch next   from C3   into :wSQL_FLTR7_ds;
062900170523
063000170523         Select;
063100170523
063200170523           // -?E.o.F.?
063300170523           When  SQLcode = 100;
063400170524             $EoF3 = *on;
063500170523
063600170523           // -?Errore?
063700170523           When  SQLcode < *zero;
063800170524             $EoF3 = *on;
063900170523             exsr  sr_PrintERR;
064000170523
064100170523           // -?Elaborazione?
064200170523           Other;
064300170524             clear  wBarCode;
064400170524             wBarCode = wSQL_FLTR7_ds.TR7bar;
064500170524             exsr  sr_Write_FIMIS;
064600170523
064700170523         EndSl;
064800170523
064900170523       ENDSR;
065000170522
065100170522       //--------------------------------------------------------------
065200170522       //?Controllo/Scrittura record in FIMIS00F.                      ?
065300170522       //--------------------------------------------------------------
065400170522       BEGSR  sr_Write_FIMIS;
065500170524
065600170524         // -?Reperimento n° spedizione dal segnacollo in esame?
065700170524         clear  keyFIART27;
065800170524         k_ARTfls = wBarCode.wBar_LNP;
065900170524         k_ARTlna = wBarCode.wBar_LNA;
066000170524         k_ARTnrs = wBarCode.wBar_NRS;
066100170524         k_ARTnsc = wBarCode.wBar_NSC;
066200170524         chain  %kds( keyFIART27 )  FIART000;
066300170524
066400170524         if  not %found( FIART27L );
066500170524           leavesr;
066600170524         endif;
066700170522
066800170522         // -?Controllo se esiste già un rec. per la spedizione?
066900170524         clear  keyFIMIS01;
067000170524         k_MISaas = ARTaas;
067100170524         k_MISlnp = ARTlnp;
067200170524         k_MISnrs = ARTnrs;
067300170524         k_MISnsp = ARTnsp;
067400170524         setll  %kds( keyFIMIS01 )  FIMIS000;
067500170524
067600170524         if  %equal( FIMIS01L );
067700170524           leavesr;
067800170524         endif;
067900170524
068000170524         // -?Aggancio FNARB00F?
068100170524         clear  keyFNARB01;
068200170524         k_ARBaas = ARTaas;
068300170524         k_ARBlnp = ARTlnp;
068400170524         k_ARBnrs = ARTnrs;
068500170524         k_ARBnsp = ARTnsp;
068600170524         chain  %kds( keyFNARB01 )  FNARB000;
068700170524
068800170524         if  not %found( FNARB01L );
068900170524           leavesr;
069000170524         endif;
069100170524
069200170524         // -?Reperimento dati del Codice Bolla?
069300170524         If  ARBcbo <> ds_TNTBE.TBEke1;
069400170524           clear  ds3A;
069500170524           ds_TNTBE.TBEke1 = ARBcbo;
069600170524           if  getTabella ( c_Tabel : '3A'  : ds_TNTBE.TBEke1 :
069700170524                            *omit : *omit : *omit :
069800170524                            *omit : *omit :
069900170524                            *omit : *omit : *omit : *omit :
070000170524                            *omit : *omit : *omit : *omit :
070100170524                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
070200170524                          = *zero      AND
070300170524               ds_TNTBE.TBEatb = *blank;
070400170524             ds3A   = ds_TNTBE.TBEuni;
070500170524           endif;
070600170524         EndIf;
070700170524
070800170524         // -?Scrittura record in FIMIS00F?
070900170524         clear  FIMIS000;
071000170524         MISaas = ARBaas;
071100170524         MISmgs = ARBmgs;
071200170524         MISlnp = ARBlnp;
071300170524         MISnrs = ARBnrs;
071400170524         MISnsp = ARBnsp;
071500170524         select;
071600170524           when  ARBccm <> *zero;
071700170524             MISccm = ARBccm;
071800170524           when  %subst( ds3A.§3Atb1 : 1 : 1 ) = 'F';
071900170524             MISccm = ARBksc;
072000170526           other;
072100170526             MISccm = ( ARBlnp * 10000 ) + 8888;
072200170524         endsl;
072300170524         MISctr = ARBctr;
072400170524         MISpkf = ARBpkf;
072500170529         MISvlf = ARBvlf;
072600170524         MISncl = ARBncl;
072700170524
072800170524         //______________
072900170524         write  FIMIS000;
073000170524         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
073100170522
073200170522       ENDSR;
073300170522
073400170522       //--------------------------------------------------------------
073500170522       //?Stampa segnalazione dell'errore rilevato via SQL.            ?
073600170522       //--------------------------------------------------------------
073700170522       BEGSR  sr_PrintERR;
073800170522
073900170522         // -?Stampa del Dump?
074000170522         Dump(A);
074100170522
074200170522         // -?Stampa del Job-Log?
074300170522         Qcmd = 'DSPJOBLOG job(*) output(*print)';
074400170522         exsr  sr_ExecCmd;
074500170522
074600170522       ENDSR;
074700170522
074800170522       //--------------------------------------------------------------
074900170522       //?Operazioni finali.                                           ?
075000170522       //--------------------------------------------------------------
075100170522       BEGSR  sr_RoutEnd;
075200170522
075300170522         // -?Uscita dal *pgm?
075400170522         return;
075500170522
075600170522       ENDSR;
075700170522
075800170522       //--------------------------------------------------------------
075900170522       //?Esecuzione del comando (già impostato).                      ?
076000170522       //--------------------------------------------------------------
076100170522       BEGSR  sr_ExecCmd;
076200170522
076300170522         clear Qcap0100;
076400170522         Qcabcsdh = *off;
076500170522         Qcapa    = *off;
076600170522         Qcacmdss = *off;
076700170522         Qcaerved = *allX'00';
076800170522
076900170522         clear Qusec;
077000170522         Qusbprv  = %size(Qusec);
077100170522
077200170522         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
077300170522                           %size(Qcap0100) : 'CPOP0100' : *omit :
077400170522                           0 : 0 : Qusec);
077500170522
077600170522         //if  Qusei <> *blank;
077700170522         //  ...;
077800170522         //endif;
077900170522
078000170522       ENDSR;
078100170522
078200170522      /end-free
