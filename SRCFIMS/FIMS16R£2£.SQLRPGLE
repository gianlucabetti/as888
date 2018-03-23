000100170525       //==============================================================
000200170525       //?FIMS16R - Statistica Clienti con Merce Incompatibile: crea   ?
000300170525       //?          *file WFMIS00F: Merce Incompatibile - Totali per   ?
000400170525       //?          Cliente / Data Spedizione              - batch -   ?
000500170525       //==============================================================
000600170525
000700170525       //--------------------------------------------------------------
000800170525       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000900170525       //--------------------------------------------------------------
001000170525
001100170525     /*PRM dbgview(*source)
001200170525     /*END
001300170525
001400170525       //--------------------------------------------------------------
001500170525       //?Specifiche di controllo.                                     ?
001600170525       //--------------------------------------------------------------
001700170525
001800170525     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001900170525     h alwnull(*inputonly)
002000170525     h dftactgrp(*no)
002100170525
002200170525       //--------------------------------------------------------------
002300170525       //?Dichiarazione file.                                          ?
002400170525       //--------------------------------------------------------------
002500170525
002600170525       // -?Merce Incompatibile: Totale per Cliente / Data Spedizione?
002700170525     fWFMIT00F  o    e             disk
002800170525
002900170525       //--------------------------------------------------------------
003000170525       //?Definizione costanti.                                        ?
003100170525       //--------------------------------------------------------------
003200170525
003300170525       // -?Costante per controllo "caratteri solo numerici"?
003400170525     d c_Digits        c                   const('0123456789')
003500170525
003600170525       //--------------------------------------------------------------
003700170525       //?Definizione schiere.                                         ?
003800170525       //--------------------------------------------------------------
003900170525
004000170525
004100170525       //--------------------------------------------------------------
004200170525       //?Definizione aree dati.                                       ?
004300170525       //--------------------------------------------------------------
004400170525
004500170525       // -?Dati utente?
004600170525     d §AzUte        e ds                  extname(AZUTE00F)
004700170525     d                                     dtaara
004800170525     d §DatiUte      e ds                  extname(dDatiUte)
004900170525     d                                     dtaara
005000170525
005100170525       //--------------------------------------------------------------
005200170525       //?Definizione strutture dati.                                  ?
005300170525       //--------------------------------------------------------------
005400170525
005500170525       // -?Status ds?
005600170525     d Status         sds
005700170525     d   SDSpgmName      *proc
005800170525     d*//SDSdta              191    198
005900170525     d   SDSjob              244    253
006000170525     d   SDSusr              254    263
006100170525
006200170525       // -?Parametri ricevuti?
006300170525     d KPJBA         e ds
006400170525     d FIMS16ds        ds                  inz   qualified
006500170525     d   iMS16dti                     8s 0 inz
006600170525     d   iMS16dtf                     8s 0 inz
006700170525     d   iMS16ccm                     7s 0 inz
006800170525
006900170525       // -?Dati estratti via SQL?
007000170525     d FIMIS00F      e ds                  based(nullPtr)
007100170525     d                                     qualified
007200170525     d wSQL_FIMIS_ds   ds                  inz   qualified
007300170525     d   misAAS                            inz   like(FIMIS00F.MISaas)
007400170525     d   misMGS                            inz   like(FIMIS00F.MISmgs)
007500170531     d   misTFP                            inz   like(FIMIS00F.MIStfp)
007600170525     d   misCCM                            inz   like(FIMIS00F.MISccm)
007700170529     d*//misCTR                            inz   like(FIMIS00F.MISctr)
007800170525     d   misPKF                     + 4    inz   like(FIMIS00F.MISpkf)
007900170525     d   misVLF                     + 4    inz   like(FIMIS00F.MISvlf)
008000170525     d   misNCL                     + 4    inz   like(FIMIS00F.MISncl)
008100170530     d   misNSIP                    + 4    inz   like(FIMIS00F.MISnsiP)
008200170530     d   misNSIA                    + 4    inz   like(FIMIS00F.MISnsiA)
008300170525     d   misTSP                            inz   like(FIMIS00F.MISnsp)
008400170530
008500170530     d TITAS00F      e ds                  based(nullPtr)
008600170530     d                                     qualified
008700170530     d wSQL_TITAS_ds   ds                  inz   qualified
008800170530     d   tasAAS                            inz   like(TITAS00F.TASaas)
008900170530     d   tasMGS                            inz   like(TITAS00F.TASmgs)
009000170531     d   tasTFP                            inz   like(TITAS00F.TAStfp)
009100170530     d   tasCCM                            inz   like(TITAS00F.TASccm)
009200170530     d*//tasCTR                            inz   like(TITAS00F.TASctr)
009300170530     d   tasPKF                     + 4    inz   like(TITAS00F.TASpkf)
009400170530     d   tasVLF                     + 4    inz   like(TITAS00F.TASvlf)
009500170530     d   tasNCL                     + 4    inz   like(TITAS00F.TASncl)
009600170530     d   tasTSP                            inz   like(TITAS00F.TASnsp)
009700170525
009800170525       //--------------------------------------------------------------
009900170525       //?Definizione variabili globali.                               ?
010000170525       //--------------------------------------------------------------
010100170525
010200170525       // -?Flags booleani?
010300170525     d $EoF            s               n   inz
010400170530     d $EoF2           s               n   inz
010500170525     d $Where          s               n   inz
010600170525
010700170525       // -?Stringhe SQL da eseguire?
010800170525     d wSQL            s          32740    inz   varying
010900170530     d wSQL2           s          32740    inz   varying
011000170525
011100170525       //--------------------------------------------------------------
011200170525       //?Definizione prototipi procedure.                             ?
011300170525       //--------------------------------------------------------------
011400170525
011500170525       // -?Reperimento dati utente?
011600170525     d TIBS34ds      e ds                  inz
011700170525      /copy gaitrasrc/srcProtoPR,TIBS34R
011800170525
011900170525       // -?Ricerca unificante padre?
012000170525      /copy gaitrasrc/srcProtoPI,TIBS10R
012100170525      /copy gaitrasrc/srcProtoPR,TIBS10R
012200170525
012300170525       // -?Parametri API QCAPCMD (Process Commands)?
012400170525     d Qcmd            s           2048    inz   varying
012500170525      /copy qSysInc/qRpgleSrc,QCAPCMD
012600170525       // -?API QCAPCMD (Process Commands)?
012700170525      /copy gaitrasrc/srcProtoPR,QCAPCMD
012800170525
012900170525       // -?Parametri gestione errori API.?
013000170525      /copy qsysinc/qrpglesrc,QUSEC
013100170525
013200170525       //--------------------------------------------------------------
013300170525       //?Definizione key-list.                                        ?
013400170525       //--------------------------------------------------------------
013500170525
013600170525
013700170525       //--------------------------------------------------------------
013800170525       //?Riepilogo indicatori utilizzati.                             ?
013900170525       //--------------------------------------------------------------
014000170525
014100170525
014200170525       //--------------------------------------------------------------
014300170525       //?M A I N - L I N E                                            ?
014400170525       //--------------------------------------------------------------
014500170525
014600170525     c     *Entry        plist
014700170525     c                   parm                    KPJBA
014800170525
014900170525      /free
015000170525
015100170525       // -?Operazioni iniziali?
015200170525       exsr  sr_RoutInz;
015300170525
015400170525       // -?Preparazione stringa SQL per estrazione dati?
015500170525       exsr  sr_PrepSQL;
015600170525
015700170525       // -?Apertura Cursore?
015800170525       exsr  sr_OpenCursor;
015900170525
016000170525       // -?Ciclo di elaborazione?
016100170525       DoW  $EoF = *off;
016200170525         exsr  sr_ReadCursor;
016300170525       EndDo;
016400170525
016500170525       // -?Chiusura Cursore?
016600170525       exsr  sr_CloseCursor;
016700170525
016800170525       // -?Operazioni finali?
016900170525       exsr sr_RoutEnd;
017000170525
017100170525       //--------------------------------------------------------------
017200170525       //?Operazioni iniziali.                                         ?
017300170525       //--------------------------------------------------------------
017400170525       BEGSR  sr_RoutInz;
017500170525
017600170525         // -?Impostazione opzioni per SQL?
017700170525         exec sql   set  option  DynUsrPrf = *Owner,
017800170525                                 CloSqlCsr = *EndMod;
017900170525
018000170525         // -?Impostazione chiusura?
018100170525         *inLR = *on;
018200170525
018300170525         // -?Reperimento dati job?
018400170525         exsr  sr_DatiJob;
018500170525
018600170525         // -?Reperimento parametri?
018700170525         clear  FIMS16ds;
018800170525         If  KPJBU <> *blank;
018900170525           FIMS16ds = KPJBU;
019000170525           if  %check( c_Digits : %subst( KPJBU : 1 :
019100170525                                  %size( FIMS16ds.iMS16dti ) ) )      > *zero;
019200170525             clear  FIMS16ds.iMS16dti;
019300170525           endif;
019400170525           if  %check( c_Digits : %subst( KPJBU :
019500170525                                  %size( FIMS16ds.iMS16dti ) + 1 :
019600170525                                  %size( FIMS16ds.iMS16dtf ) ) ) > *zero;
019700170525             clear  FIMS16ds.iMS16dtf;
019800170525           endif;
019900170525           if  %check( c_Digits : %subst( KPJBU :
020000170525                                  %size( FIMS16ds.iMS16dti ) +
020100170525                                  %size( FIMS16ds.iMS16dtf ) + 1 :
020200170525                                  %size( FIMS16ds.iMS16ccm ) ) ) > *zero;
020300170525             clear  FIMS16ds.iMS16ccm;
020400170525           endif;
020500170525           if  FIMS16ds.iMS16dti > *zero  and
020600170525               FIMS16ds.iMS16dtf = *zero;
020700170525             FIMS16ds.iMS16dtf = FIMS16ds.iMS16dti;
020800170525           endif;
020900170525         EndIf;
021000170525
021100170525       ENDSR;
021200170525
021300170525       //--------------------------------------------------------------
021400170525       //?Reperimento Dati del job (Utente/Operativi).                 ?
021500170525       //--------------------------------------------------------------
021600170525       BEGSR  sr_DatiJob;
021700170525
021800170525         in(e)  §AzUte;
021900170525         if  NOT  %error;
022000170525           in(e)  §DatiUte;
022100170525         endif;
022200170525         if  %error  or  RSut = *blank;
022300170525           tibs34r ( tibs34ds );
022400170525           in  §AzUte;
022500170525           in  §DatiUte;
022600170525         endif;
022700170525
022800170525       ENDSR;
022900170525
023000170525       //--------------------------------------------------------------
023100170525       //?Preparazione stringa SQL - per estrazione dati da FIMIS.     ?
023200170525       //--------------------------------------------------------------
023300170525       BEGSR  sr_PrepSQL;
023400170525
023500170525         clear  $Where;
023600170525
023700170531         wSQL  = 'select MISaas, MISmgs, MIStfp, MISccm, +
023800170526                         sum( MISpkf ), sum( MISvlf ), sum( MISncl ), +
023900170529                         sum( MISnsiP ), sum( MISnsiA ), count(*) +
024000170526                    from FIMIS00F';
024100170525
024200170525         if  ( FIMS16ds.iMS16dti + FIMS16ds.iMS16dtf ) > *zero;
024300170525
024400170525           if  Not $Where;
024500170525             $Where = *on;
024600170525             wSQL  +=  ' where';
024700170525           else;
024800170525             wSQL  +=  ' and';
024900170525           endif;
025000170525
025100170525           wSQL +=     ' ( MISaas * 10000 ) + MISmgs between '
025200170525                       + %editc( FIMS16ds.iMS16dti : 'X' ) + ' and '
025300170525                       + %editc( FIMS16ds.iMS16dtf : 'X' );
025400170525
025500170525         endif;
025600170525
025700170525         if  FIMS16ds.iMS16ccm > *zero;
025800170525
025900170525           if  Not $Where;
026000170525             $Where = *on;
026100170525             wSQL  +=  ' where';
026200170525           else;
026300170525             wSQL  +=  ' and';
026400170525           endif;
026500170525
026600170525           wSQL +=     ' MISccm = ' + %editc( FIMS16ds.iMS16ccm : 'X' );
026700170525
026800170525         endif;
026900170525
027000170531         wSQL += ' group by MISaas, MISmgs, MIStfp, MISccm +
027100170531                   order by MISaas, MISmgs, MIStfp, MISccm +
027200170525                     for fetch only';
027300170525
027400170525       ENDSR;
027500170525
027600170525       //--------------------------------------------------------------
027700170525       //?Apertura cursore.                                            ?
027800170525       //--------------------------------------------------------------
027900170525       BEGSR  sr_OpenCursor;
028000170525
028100170525         $EoF  = *off;
028200170525
028300170525         // -?Dichiarazione cursore?
028400170525         exec sql   prepare S1   from :wSQL;
028500170525         exec sql   declare C1   cursor for S1;
028600170525
028700170525         if  SQLcode < *zero;
028800170525           $EoF  = *on;
028900170525           exsr  sr_PrintERR;
029000170525           leavesr;
029100170525         endif;
029200170525
029300170525         // -?Apertura del cursore?
029400170525         exec sql   open C1;
029500170525
029600170525         if  SQLcode < *zero;
029700170525           $EoF  = *on;
029800170525           exsr  sr_PrintERR;
029900170525           leavesr;
030000170525         endif;
030100170525
030200170525       ENDSR;
030300170525
030400170525       //--------------------------------------------------------------
030500170525       //?Chiusura cursore.                                            ?
030600170525       //--------------------------------------------------------------
030700170525       BEGSR  sr_CloseCursor;
030800170525
030900170525         // -?Chiusura del cursore?
031000170525         exec sql   close C1;
031100170525
031200170525       ENDSR;
031300170525
031400170525       //--------------------------------------------------------------
031500170525       //?Lettura cursore.                                             ?
031600170525       //--------------------------------------------------------------
031700170525       BEGSR  sr_ReadCursor;
031800170525
031900170525         clear  wSQL_FIMIS_ds;
032000170525
032100170525         exec sql   fetch next   from C1   into :wSQL_FIMIS_ds;
032200170525
032300170525         Select;
032400170525
032500170525           // -?E.o.F.?
032600170525           When  SQLcode = 100;
032700170525             $EoF = *on;
032800170525
032900170525           // -?Errore?
033000170525           When  SQLcode < *zero;
033100170525             $EoF = *on;
033200170525             exsr  sr_PrintERR;
033300170525
033400170525           // -?Elaborazione?
033500170525           Other;
033600170525             exsr  sr_Write_WFMIT;
033700170525
033800170525         EndSl;
033900170525
034000170525       ENDSR;
034100170525
034200170525       //--------------------------------------------------------------
034300170525       //?Elaborazione totale di FIMS00F e scrittura rec. di WFMIT00F. ?
034400170525       //--------------------------------------------------------------
034500170525       BEGSR  sr_Write_WFMIT;
034600170525
034700170525         clear  WFMIT000;
034800170525
034900170530         MITaas  = wSQL_FIMIS_ds.MISaas;
035000170530         MITmgs  = wSQL_FIMIS_ds.MISmgs;
035100170531         MITtfp  = wSQL_FIMIS_ds.MIStfp;
035200170530         MITccm  = wSQL_FIMIS_ds.MISccm;
035300170530         //*·MITctr  = wSQL_FIMIS_ds.MISctr;
035400170530         MITpkfI = wSQL_FIMIS_ds.MISpkf;
035500170530         MITvlfI = wSQL_FIMIS_ds.MISvlf;
035600170530         MITnclI = wSQL_FIMIS_ds.MISncl;
035700170530         MITtspI = wSQL_FIMIS_ds.MIStsp;
035800170530         MITnsiP = wSQL_FIMIS_ds.MISnsiP;
035900170530         MITnsiA = wSQL_FIMIS_ds.MISnsiA;
036000170525
036100170525         // -?Reperimento codice cliente Unificante?
036200170525         clear  TIBS10ds;
036300170525         TIBS10ds.D10drf = ( wSQL_FIMIS_ds.MISaas * 10000 ) +
036400170525                             wSQL_FIMIS_ds.MISmgs;
036500170525         TIBS10ds.D10tle = 'ST';
036600170525         TIBS10ds.D10paf = 'P';   //?Verifica se è figlio e ne reperisce il padre?
036700170525         TIBS10ds.D10cod = wSQL_FIMIS_ds.MISccm;
036800170525         GetKSCfamily ( TIBS10ds : 'R' );
036900170525         if  TIBS10ds.D10err = *blank  and
037000170525             TIBS10ds.D10cop > *zero;
037100170525           MITksu = TIBS10ds.D10cop;
037200170525         else;
037300170525           MITksu = TIBS10ds.D10cod;
037400170525         endif;
037500170530
037600170530         // -?Reperimento totali da TITAS00F/10F/P0F?
037700170530         exsr  sr_Elab_TITAS;
037800170525
037900170525         // -?Scrittura record in WFMIT00F?
038000170525         //______________
038100170525         write  WFMIT000;
038200170525         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
038300170525
038400170525       ENDSR;
038500170530
038600170530       //--------------------------------------------------------------
038700170530       //?Estrazione totale Spedizioni per giorno/cliente da TITAS.    ?
038800170530       //--------------------------------------------------------------
038900170530       BEGSR  sr_Elab_TITAS;
039000170530
039100170530         // -?Preparazione stringa SQL per estrazione dati?
039200170530         exsr  sr_PrepSQL_TITAS;
039300170530
039400170530         // -?Apertura Cursore?
039500170530         exsr  sr_OpenCursor_TITAS;
039600170530
039700170530         // -?Ciclo di elaborazione?
039800170530         DoW  $EoF2 = *off;
039900170530           exsr  sr_ReadCursor_TITAS;
040000170530         EndDo;
040100170530
040200170530         // -?Chiusura Cursore?
040300170530         exsr  sr_CloseCursor_TITAS;
040400170530
040500170530       ENDSR;
040600170530
040700170530       //--------------------------------------------------------------
040800170530       //?Preparazione stringa SQL per estrazione totali da TITAS.     ?
040900170530       //--------------------------------------------------------------
041000170530       BEGSR  sr_PrepSQL_TITAS;
041100170530
041200170530
041300170530         wSQL2 = 'with TITASP as ( +
041400170531                       select TASaas, TASmgs, TAStfp, TASccm, +
041500170530                              TASpkf, TASvlf, TASncl +
041600170530                         from TITASP0F +
041700170530                        where TASccm = ' + %trim(
041800170530                              %editc( wSQL_FIMIS_ds.MISccm : '3' ) ) +
041900170530                        ' and TASaas = ' + %trim(
042000170530                              %editc( wSQL_FIMIS_ds.MISaas : '3' ) ) +
042100170530                        ' and TASmgs = ' + %trim(
042200170530                              %editc( wSQL_FIMIS_ds.MISmgs : '3' ) ) +
042300170531                        ' and TAStfp = ' + %trim(
042400170531                              %editc( wSQL_FIMIS_ds.MIStfp : '3' ) ) +
042500170530                        ' and TAStbl in (''F1'', ''A2'') +
042600170530                                 ), +
042700170530                       TITAS1 as ( +
042800170531                       select TASaas, TASmgs, TAStfp, TASccm, +
042900170530                              TASpkf, TASvlf, TASncl +
043000170530                         from TITAS10F +
043100170530                        where TASccm = ' + %trim(
043200170530                              %editc( wSQL_FIMIS_ds.MISccm : '3' ) ) +
043300170530                        ' and TASaas = ' + %trim(
043400170530                              %editc( wSQL_FIMIS_ds.MISaas : '3' ) ) +
043500170530                        ' and TASmgs = ' + %trim(
043600170530                              %editc( wSQL_FIMIS_ds.MISmgs : '3' ) ) +
043700170531                        ' and TAStfp = ' + %trim(
043800170531                              %editc( wSQL_FIMIS_ds.MIStfp : '3' ) ) +
043900170530                        ' and TAStbl in (''F1'', ''A2'') +
044000170530                                 ), +
044100170530                       TITAS0 as ( +
044200170531                       select TASaas, TASmgs, TAStfp, TASccm, +
044300170530                              TASpkf, TASvlf, TASncl +
044400170530                         from TITAS00F +
044500170530                        where TASccm = ' + %trim(
044600170530                              %editc( wSQL_FIMIS_ds.MISccm : '3' ) ) +
044700170530                        ' and TASaas = ' + %trim(
044800170530                              %editc( wSQL_FIMIS_ds.MISaas : '3' ) ) +
044900170530                        ' and TASmgs = ' + %trim(
045000170530                              %editc( wSQL_FIMIS_ds.MISmgs : '3' ) ) +
045100170531                        ' and TAStfp = ' + %trim(
045200170531                              %editc( wSQL_FIMIS_ds.MIStfp : '3' ) ) +
045300170530                        ' and TAStbl in (''F1'', ''A2'') +
045400170530                                 ) +
045500170530
045600170531                  select TASaas, TASmgs, TAStfp, TASccm, +
045700170530                         sum( TASpkf ), sum( TASvlf ), sum( TASncl ), +
045800170530                         count(*) +
045900170530                    from TITASP +
046000170531                group by TASccm, TASaas, TAStfp, TASmgs +
046100170530                   union +
046200170531                  select TASaas, TASmgs, TAStfp, TASccm, +
046300170530                         sum( TASpkf ), sum( TASvlf ), sum( TASncl ), +
046400170530                         count(*) +
046500170530                    from TITAS1 +
046600170531                group by TASccm, TASaas, TAStfp, TASmgs +
046700170530                   union +
046800170531                  select TASaas, TASmgs, TAStfp, TASccm, +
046900170530                         sum( TASpkf ), sum( TASvlf ), sum( TASncl ), +
047000170530                         count(*) +
047100170530                    from TITAS0 +
047200170531                group by TASccm, TASaas, TAStfp, TASmgs +
047300170530
047400170530               for fetch only';
047500170530
047600170530       ENDSR;
047700170530
047800170530       //--------------------------------------------------------------
047900170530       //?Apertura cursore.                                            ?
048000170530       //--------------------------------------------------------------
048100170530       BEGSR  sr_OpenCursor_TITAS;
048200170530
048300170530         $EoF2 = *off;
048400170530
048500170530         // -?Dichiarazione cursore?
048600170530         exec sql   prepare S2   from :wSQL2;
048700170530         exec sql   declare C2   cursor for S2;
048800170530
048900170530         if  SQLcode < *zero;
049000170530           $EoF2 = *on;
049100170530           exsr  sr_PrintERR;
049200170530           leavesr;
049300170530         endif;
049400170530
049500170530         // -?Apertura del cursore?
049600170530         exec sql   open C2;
049700170530
049800170530         if  SQLcode < *zero;
049900170530           $EoF2 = *on;
050000170530           exsr  sr_PrintERR;
050100170530           leavesr;
050200170530         endif;
050300170530
050400170530       ENDSR;
050500170530
050600170530       //--------------------------------------------------------------
050700170530       //?Chiusura cursore.                                            ?
050800170530       //--------------------------------------------------------------
050900170530       BEGSR  sr_CloseCursor_TITAS;
051000170530
051100170530         // -?Chiusura del cursore?
051200170530         exec sql   close C2;
051300170530
051400170530       ENDSR;
051500170530
051600170530       //--------------------------------------------------------------
051700170530       //?Lettura cursore.                                             ?
051800170530       //--------------------------------------------------------------
051900170530       BEGSR  sr_ReadCursor_TITAS;
052000170530
052100170530         clear  wSQL_TITAS_ds;
052200170530
052300170530         exec sql   fetch next   from C2   into :wSQL_TITAS_ds;
052400170530
052500170530         Select;
052600170530
052700170530           // -?E.o.F.?
052800170530           When  SQLcode = 100;
052900170530             $EoF2 = *on;
053000170530
053100170530           // -?Errore?
053200170530           When  SQLcode < *zero;
053300170530             $EoF2 = *on;
053400170530             exsr  sr_PrintERR;
053500170530
053600170530           // -?Elaborazione?
053700170530           Other;
053800170530             MITpkfT += wSQL_TITAS_ds.TASpkf;
053900170530             MITvlfT += wSQL_TITAS_ds.TASvlf;
054000170530             MITnclT += wSQL_TITAS_ds.TASncl;
054100170530             MITtspT += wSQL_TITAS_ds.TAStsp;
054200170530
054300170530         EndSl;
054400170530
054500170530       ENDSR;
054600170525
054700170525       //--------------------------------------------------------------
054800170525       //?Stampa segnalazione dell'errore rilevato via SQL.            ?
054900170525       //--------------------------------------------------------------
055000170525       BEGSR  sr_PrintERR;
055100170525
055200170525         // -?Stampa del Dump?
055300170525         Dump(A);
055400170525
055500170525         // -?Stampa del Job-Log?
055600170525         Qcmd = 'DSPJOBLOG job(*) output(*print)';
055700170525         exsr  sr_ExecCmd;
055800170530
055900170530         // -?Uscita dal *pgm?
056000170530         exsr  sr_RoutEnd;
056100170525
056200170525       ENDSR;
056300170525
056400170525       //--------------------------------------------------------------
056500170525       //?Operazioni finali.                                           ?
056600170525       //--------------------------------------------------------------
056700170525       BEGSR  sr_RoutEnd;
056800170525
056900170525         // -?Uscita dal *pgm?
057000170525         return;
057100170525
057200170525       ENDSR;
057300170525
057400170525       //--------------------------------------------------------------
057500170525       //?Esecuzione del comando (già impostato).                      ?
057600170525       //--------------------------------------------------------------
057700170525       BEGSR  sr_ExecCmd;
057800170525
057900170525         clear Qcap0100;
058000170525         Qcabcsdh = *off;
058100170525         Qcapa    = *off;
058200170525         Qcacmdss = *off;
058300170525         Qcaerved = *allX'00';
058400170525
058500170525         clear Qusec;
058600170525         Qusbprv  = %size(Qusec);
058700170525
058800170525         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
058900170525                           %size(Qcap0100) : 'CPOP0100' : *omit :
059000170525                           0 : 0 : Qusec);
059100170525
059200170525         //if  Qusei <> *blank;
059300170525         //  ...;
059400170525         //endif;
059500170525
059600170525       ENDSR;
059700170525
059800170525      /end-free
