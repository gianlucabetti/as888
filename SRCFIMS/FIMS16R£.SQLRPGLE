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
008400170609     d   misPOR                     + 4    inz   like(FIMIS00F.MISpor)
008500170609     d   misIMV                     + 4    inz   like(FIMIS00F.MISimv)
008600170530
008700170530     d TITAS00F      e ds                  based(nullPtr)
008800170530     d                                     qualified
008900170530     d wSQL_TITAS_ds   ds                  inz   qualified
009000170530     d   tasAAS                            inz   like(TITAS00F.TASaas)
009100170530     d   tasMGS                            inz   like(TITAS00F.TASmgs)
009200170531     d   tasTFP                            inz   like(TITAS00F.TAStfp)
009300170530     d   tasCCM                            inz   like(TITAS00F.TASccm)
009400170530     d*//tasCTR                            inz   like(TITAS00F.TASctr)
009500170530     d   tasPKF                     + 4    inz   like(TITAS00F.TASpkf)
009600170530     d   tasVLF                     + 4    inz   like(TITAS00F.TASvlf)
009700170530     d   tasNCL                     + 4    inz   like(TITAS00F.TASncl)
009800170530     d   tasTSP                            inz   like(TITAS00F.TASnsp)
009900170608     d   tasPOR                     + 4    inz   like(TITAS00F.TASpor)
010000170608     d   tasIMV                     + 4    inz   like(TITAS00F.TASimv)
010100170525
010200170525       //--------------------------------------------------------------
010300170525       //?Definizione variabili globali.                               ?
010400170525       //--------------------------------------------------------------
010500170525
010600170525       // -?Flags booleani?
010700170525     d $EoF            s               n   inz
010800170530     d $EoF2           s               n   inz
010900170608     d $EoF3           s               n   inz
011000170525
011100170525       // -?Stringhe SQL da eseguire?
011200170525     d wSQL            s          32740    inz   varying
011300170530     d wSQL2           s          32740    inz   varying
011400170608     d wSQL3           s          32740    inz   varying
011500170608
011600170608       // -?Campi di totale (a parte)?
011700170608     d wTotPOR         s             15  3 inz
011800170608     d wTotIMV         s             15  3 inz
011900170608
012000170608       // -?Campo Data (di comodo)?
012100170608     d wDate           s              8  0 inz
012200170525
012300170525       //--------------------------------------------------------------
012400170525       //?Definizione prototipi procedure.                             ?
012500170525       //--------------------------------------------------------------
012600170525
012700170525       // -?Reperimento dati utente?
012800170525     d TIBS34ds      e ds                  inz
012900170525      /copy gaitrasrc/srcProtoPR,TIBS34R
013000170525
013100170525       // -?Ricerca unificante padre?
013200170525      /copy gaitrasrc/srcProtoPI,TIBS10R
013300170525      /copy gaitrasrc/srcProtoPR,TIBS10R
013400170525
013500170525       // -?Parametri API QCAPCMD (Process Commands)?
013600170525     d Qcmd            s           2048    inz   varying
013700170525      /copy qSysInc/qRpgleSrc,QCAPCMD
013800170525       // -?API QCAPCMD (Process Commands)?
013900170525      /copy gaitrasrc/srcProtoPR,QCAPCMD
014000170525
014100170525       // -?Parametri gestione errori API.?
014200170525      /copy qsysinc/qrpglesrc,QUSEC
014300170525
014400170525       //--------------------------------------------------------------
014500170525       //?Definizione key-list.                                        ?
014600170525       //--------------------------------------------------------------
014700170525
014800170525
014900170525       //--------------------------------------------------------------
015000170525       //?Riepilogo indicatori utilizzati.                             ?
015100170525       //--------------------------------------------------------------
015200170525
015300170525
015400170525       //--------------------------------------------------------------
015500170525       //?M A I N - L I N E                                            ?
015600170525       //--------------------------------------------------------------
015700170525
015800170525     c     *Entry        plist
015900170525     c                   parm                    KPJBA
016000170525
016100170525      /free
016200170525
016300170525       // -?Operazioni iniziali?
016400170525       exsr  sr_RoutInz;
016500170525
016600170525       // -?Preparazione stringa SQL per estrazione dati?
016700170525       exsr  sr_PrepSQL;
016800170525
016900170525       // -?Apertura Cursore?
017000170525       exsr  sr_OpenCursor;
017100170525
017200170525       // -?Ciclo di elaborazione?
017300170525       DoW  $EoF = *off;
017400170525         exsr  sr_ReadCursor;
017500170525       EndDo;
017600170525
017700170525       // -?Chiusura Cursore?
017800170525       exsr  sr_CloseCursor;
017900170525
018000170525       // -?Operazioni finali?
018100170525       exsr sr_RoutEnd;
018200170525
018300170525       //--------------------------------------------------------------
018400170525       //?Operazioni iniziali.                                         ?
018500170525       //--------------------------------------------------------------
018600170525       BEGSR  sr_RoutInz;
018700170525
018800170525         // -?Impostazione opzioni per SQL?
018900170525         exec sql   set  option  DynUsrPrf = *Owner,
019000170525                                 CloSqlCsr = *EndMod;
019100170525
019200170525         // -?Impostazione chiusura?
019300170525         *inLR = *on;
019400170525
019500170525         // -?Reperimento dati job?
019600170525         exsr  sr_DatiJob;
019700170525
019800170525         // -?Reperimento parametri?
019900170525         clear  FIMS16ds;
020000170525         If  KPJBU <> *blank;
020100170525           FIMS16ds = KPJBU;
020200170525           if  %check( c_Digits : %subst( KPJBU : 1 :
020300170525                                  %size( FIMS16ds.iMS16dti ) ) )      > *zero;
020400170525             clear  FIMS16ds.iMS16dti;
020500170525           endif;
020600170525           if  %check( c_Digits : %subst( KPJBU :
020700170525                                  %size( FIMS16ds.iMS16dti ) + 1 :
020800170525                                  %size( FIMS16ds.iMS16dtf ) ) ) > *zero;
020900170525             clear  FIMS16ds.iMS16dtf;
021000170525           endif;
021100170525           if  %check( c_Digits : %subst( KPJBU :
021200170525                                  %size( FIMS16ds.iMS16dti ) +
021300170525                                  %size( FIMS16ds.iMS16dtf ) + 1 :
021400170525                                  %size( FIMS16ds.iMS16ccm ) ) ) > *zero;
021500170525             clear  FIMS16ds.iMS16ccm;
021600170525           endif;
021700170525           if  FIMS16ds.iMS16dti > *zero  and
021800170525               FIMS16ds.iMS16dtf = *zero;
021900170525             FIMS16ds.iMS16dtf = FIMS16ds.iMS16dti;
022000170525           endif;
022100170525         EndIf;
022200170609
022300170609         // -?Impostazione data limite (ultimo giorno del mese precedente)?
022400170609         //  ?per il calcolo dei Totali Incomaptibili?
022500170609         clear  wDate;
022600170609         wDate = %dec( %date() - %days( %subdt( %date() : *days ) ) );
022700170525
022800170525       ENDSR;
022900170525
023000170525       //--------------------------------------------------------------
023100170525       //?Reperimento Dati del job (Utente/Operativi).                 ?
023200170525       //--------------------------------------------------------------
023300170525       BEGSR  sr_DatiJob;
023400170525
023500170525         in(e)  §AzUte;
023600170525         if  NOT  %error;
023700170525           in(e)  §DatiUte;
023800170525         endif;
023900170525         if  %error  or  RSut = *blank;
024000170525           tibs34r ( tibs34ds );
024100170525           in  §AzUte;
024200170525           in  §DatiUte;
024300170525         endif;
024400170525
024500170525       ENDSR;
024600170525
024700170525       //--------------------------------------------------------------
024800170525       //?Preparazione stringa SQL - per estrazione dati da FIMIS.     ?
024900170525       //--------------------------------------------------------------
025000170525       BEGSR  sr_PrepSQL;
025100170525
025200170531         wSQL  = 'select MISaas, MISmgs, MIStfp, MISccm, +
025300170526                         sum( MISpkf ), sum( MISvlf ), sum( MISncl ), +
025400170609                         sum( MISnsiP ), sum( MISnsiA ), count(*), +
025500170609                         sum( MISpor ), sum( MISimv ) +
025600170608                    from FIMIS00F +
025700170608                   where MIStbl in (''F1'', ''A2'')';
025800170525
025900170525         if  ( FIMS16ds.iMS16dti + FIMS16ds.iMS16dtf ) > *zero;
026000170525
026100170608           wSQL += ' and ( MISaas * 10000 ) + MISmgs between '
026200170525                       + %editc( FIMS16ds.iMS16dti : 'X' ) + ' and '
026300170525                       + %editc( FIMS16ds.iMS16dtf : 'X' );
026400170525
026500170525         endif;
026600170525
026700170525         if  FIMS16ds.iMS16ccm > *zero;
026800170525
026900170608           wSQL += ' and MISccm = ' + %editc( FIMS16ds.iMS16ccm : 'X' );
027000170525
027100170525         endif;
027200170525
027300170531         wSQL += ' group by MISaas, MISmgs, MIStfp, MISccm +
027400170531                   order by MISaas, MISmgs, MIStfp, MISccm +
027500170525                     for fetch only';
027600170525
027700170525       ENDSR;
027800170525
027900170525       //--------------------------------------------------------------
028000170525       //?Apertura cursore.                                            ?
028100170525       //--------------------------------------------------------------
028200170525       BEGSR  sr_OpenCursor;
028300170525
028400170525         $EoF  = *off;
028500170525
028600170525         // -?Dichiarazione cursore?
028700170525         exec sql   prepare S1   from :wSQL;
028800170525         exec sql   declare C1   cursor for S1;
028900170525
029000170525         if  SQLcode < *zero;
029100170525           $EoF  = *on;
029200170525           exsr  sr_PrintERR;
029300170525           leavesr;
029400170525         endif;
029500170525
029600170525         // -?Apertura del cursore?
029700170525         exec sql   open C1;
029800170525
029900170525         if  SQLcode < *zero;
030000170525           $EoF  = *on;
030100170525           exsr  sr_PrintERR;
030200170525           leavesr;
030300170525         endif;
030400170525
030500170525       ENDSR;
030600170525
030700170525       //--------------------------------------------------------------
030800170525       //?Chiusura cursore.                                            ?
030900170525       //--------------------------------------------------------------
031000170525       BEGSR  sr_CloseCursor;
031100170525
031200170525         // -?Chiusura del cursore?
031300170525         exec sql   close C1;
031400170525
031500170525       ENDSR;
031600170525
031700170525       //--------------------------------------------------------------
031800170525       //?Lettura cursore.                                             ?
031900170525       //--------------------------------------------------------------
032000170525       BEGSR  sr_ReadCursor;
032100170525
032200170525         clear  wSQL_FIMIS_ds;
032300170525
032400170525         exec sql   fetch next   from C1   into :wSQL_FIMIS_ds;
032500170525
032600170525         Select;
032700170525
032800170525           // -?E.o.F.?
032900170525           When  SQLcode = 100;
033000170525             $EoF = *on;
033100170525
033200170525           // -?Errore?
033300170525           When  SQLcode < *zero;
033400170525             $EoF = *on;
033500170525             exsr  sr_PrintERR;
033600170525
033700170525           // -?Elaborazione?
033800170525           Other;
033900170525             exsr  sr_Write_WFMIT;
034000170525
034100170525         EndSl;
034200170525
034300170525       ENDSR;
034400170525
034500170525       //--------------------------------------------------------------
034600170525       //?Elaborazione totale di FIMS00F e scrittura rec. di WFMIT00F. ?
034700170525       //--------------------------------------------------------------
034800170525       BEGSR  sr_Write_WFMIT;
034900170525
035000170525         clear  WFMIT000;
035100170525
035200170530         MITaas  = wSQL_FIMIS_ds.MISaas;
035300170530         MITmgs  = wSQL_FIMIS_ds.MISmgs;
035400170531         MITtfp  = wSQL_FIMIS_ds.MIStfp;
035500170530         MITccm  = wSQL_FIMIS_ds.MISccm;
035600170530         //*·MITctr  = wSQL_FIMIS_ds.MISctr;
035700170530         MITpkfI = wSQL_FIMIS_ds.MISpkf;
035800170530         MITvlfI = wSQL_FIMIS_ds.MISvlf;
035900170530         MITnclI = wSQL_FIMIS_ds.MISncl;
036000170530         MITtspI = wSQL_FIMIS_ds.MIStsp;
036100170530         MITnsiP = wSQL_FIMIS_ds.MISnsiP;
036200170530         MITnsiA = wSQL_FIMIS_ds.MISnsiA;
036300170609         MITporI = wSQL_FIMIS_ds.MISpor;
036400170609         MITimvI = wSQL_FIMIS_ds.MISimv;
036500170525
036600170525         // -?Reperimento codice cliente Unificante?
036700170525         clear  TIBS10ds;
036800170525         TIBS10ds.D10drf = ( wSQL_FIMIS_ds.MISaas * 10000 ) +
036900170525                             wSQL_FIMIS_ds.MISmgs;
037000170525         TIBS10ds.D10tle = 'ST';
037100170525         TIBS10ds.D10paf = 'P';   //?Verifica se è figlio e ne reperisce il padre?
037200170525         TIBS10ds.D10cod = wSQL_FIMIS_ds.MISccm;
037300170525         GetKSCfamily ( TIBS10ds : 'R' );
037400170525         if  TIBS10ds.D10err = *blank  and
037500170525             TIBS10ds.D10cop > *zero;
037600170525           MITksu = TIBS10ds.D10cop;
037700170525         else;
037800170525           MITksu = TIBS10ds.D10cod;
037900170525         endif;
038000170530
038100170530         // -?Reperimento totali da TITAS00F/10F/P0F?
038200170530         exsr  sr_Elab_TITAS;
038300170525
038400170525         // -?Scrittura record in WFMIT00F?
038500170525         //______________
038600170525         write  WFMIT000;
038700170525         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
038800170525
038900170525       ENDSR;
039000170530
039100170530       //--------------------------------------------------------------
039200170530       //?Estrazione totale Spedizioni per giorno/cliente da TITAS.    ?
039300170530       //--------------------------------------------------------------
039400170530       BEGSR  sr_Elab_TITAS;
039500170530
039600170608         // -? Calcolo TOTALI x CLIENTE ?
039700170608
039800170608         // -?Preparazione stringa SQL per estrazione?
039900170608         exsr  sr_PrepSQL_TITAS_TOT_CLI;
040000170530
040100170530         // -?Apertura Cursore?
040200170608         exsr  sr_OpenCursor_TITAS_TOT_CLI;
040300170530
040400170530         // -?Ciclo di elaborazione?
040500170530         DoW  $EoF2 = *off;
040600170608           exsr  sr_ReadCursor_TITAS_TOT_CLI;
040700170530         EndDo;
040800170530
040900170530         // -?Chiusura Cursore?
041000170608         exsr  sr_CloseCursor_TITAS_TOT_CLI;
041100170530
041200170530       ENDSR;
041300170530
041400170530       //--------------------------------------------------------------
041500170530       //?Preparazione stringa SQL per estrazione totali da TITAS.     ?
041600170530       //--------------------------------------------------------------
041700170608       BEGSR  sr_PrepSQL_TITAS_TOT_CLI;
041800170530
041900170530         wSQL2 = 'with TITASP as ( +
042000170531                       select TASaas, TASmgs, TAStfp, TASccm, +
042100170608                              TASpkf, TASvlf, TASncl, TASpor, TASimv +
042200170530                         from TITASP0F +
042300170530                        where TASccm = ' + %trim(
042400170530                              %editc( wSQL_FIMIS_ds.MISccm : '3' ) ) +
042500170530                        ' and TASaas = ' + %trim(
042600170530                              %editc( wSQL_FIMIS_ds.MISaas : '3' ) ) +
042700170530                        ' and TASmgs = ' + %trim(
042800170530                              %editc( wSQL_FIMIS_ds.MISmgs : '3' ) ) +
042900170531                        ' and TAStfp = ' + %trim(
043000170531                              %editc( wSQL_FIMIS_ds.MIStfp : '3' ) ) +
043100170530                        ' and TAStbl in (''F1'', ''A2'') +
043200170530                                 ), +
043300170530                       TITAS1 as ( +
043400170531                       select TASaas, TASmgs, TAStfp, TASccm, +
043500170608                              TASpkf, TASvlf, TASncl, TASpor, TASimv +
043600170530                         from TITAS10F +
043700170530                        where TASccm = ' + %trim(
043800170530                              %editc( wSQL_FIMIS_ds.MISccm : '3' ) ) +
043900170530                        ' and TASaas = ' + %trim(
044000170530                              %editc( wSQL_FIMIS_ds.MISaas : '3' ) ) +
044100170530                        ' and TASmgs = ' + %trim(
044200170530                              %editc( wSQL_FIMIS_ds.MISmgs : '3' ) ) +
044300170531                        ' and TAStfp = ' + %trim(
044400170531                              %editc( wSQL_FIMIS_ds.MIStfp : '3' ) ) +
044500170530                        ' and TAStbl in (''F1'', ''A2'') +
044600170530                                 ), +
044700170530                       TITAS0 as ( +
044800170531                       select TASaas, TASmgs, TAStfp, TASccm, +
044900170608                              TASpkf, TASvlf, TASncl, TASpor, TASimv +
045000170530                         from TITAS00F +
045100170530                        where TASccm = ' + %trim(
045200170530                              %editc( wSQL_FIMIS_ds.MISccm : '3' ) ) +
045300170530                        ' and TASaas = ' + %trim(
045400170530                              %editc( wSQL_FIMIS_ds.MISaas : '3' ) ) +
045500170530                        ' and TASmgs = ' + %trim(
045600170530                              %editc( wSQL_FIMIS_ds.MISmgs : '3' ) ) +
045700170531                        ' and TAStfp = ' + %trim(
045800170531                              %editc( wSQL_FIMIS_ds.MIStfp : '3' ) ) +
045900170530                        ' and TAStbl in (''F1'', ''A2'') +
046000170530                                 ) +
046100170530
046200170531                  select TASaas, TASmgs, TAStfp, TASccm, +
046300170530                         sum( TASpkf ), sum( TASvlf ), sum( TASncl ), +
046400170608                         count(*), sum( TASpor ), sum( TASimv ) +
046500170530                    from TITASP +
046600170531                group by TASccm, TASaas, TAStfp, TASmgs +
046700170530                   union +
046800170531                  select TASaas, TASmgs, TAStfp, TASccm, +
046900170530                         sum( TASpkf ), sum( TASvlf ), sum( TASncl ), +
047000170608                         count(*), sum( TASpor ), sum( TASimv ) +
047100170530                    from TITAS1 +
047200170531                group by TASccm, TASaas, TAStfp, TASmgs +
047300170530                   union +
047400170531                  select TASaas, TASmgs, TAStfp, TASccm, +
047500170530                         sum( TASpkf ), sum( TASvlf ), sum( TASncl ), +
047600170608                         count(*), sum( TASpor ), sum( TASimv ) +
047700170530                    from TITAS0 +
047800170531                group by TASccm, TASaas, TAStfp, TASmgs +
047900170530
048000170530               for fetch only';
048100170530
048200170530       ENDSR;
048300170530
048400170530       //--------------------------------------------------------------
048500170530       //?Apertura cursore.                                            ?
048600170530       //--------------------------------------------------------------
048700170608       BEGSR  sr_OpenCursor_TITAS_TOT_CLI;
048800170530
048900170530         $EoF2 = *off;
049000170530
049100170530         // -?Dichiarazione cursore?
049200170530         exec sql   prepare S2   from :wSQL2;
049300170530         exec sql   declare C2   cursor for S2;
049400170530
049500170530         if  SQLcode < *zero;
049600170530           $EoF2 = *on;
049700170530           exsr  sr_PrintERR;
049800170530           leavesr;
049900170530         endif;
050000170530
050100170530         // -?Apertura del cursore?
050200170530         exec sql   open C2;
050300170530
050400170530         if  SQLcode < *zero;
050500170530           $EoF2 = *on;
050600170530           exsr  sr_PrintERR;
050700170530           leavesr;
050800170530         endif;
050900170530
051000170530       ENDSR;
051100170530
051200170530       //--------------------------------------------------------------
051300170530       //?Chiusura cursore.                                            ?
051400170530       //--------------------------------------------------------------
051500170608       BEGSR  sr_CloseCursor_TITAS_TOT_CLI;
051600170530
051700170530         // -?Chiusura del cursore?
051800170530         exec sql   close C2;
051900170530
052000170530       ENDSR;
052100170530
052200170530       //--------------------------------------------------------------
052300170530       //?Lettura cursore.                                             ?
052400170530       //--------------------------------------------------------------
052500170608       BEGSR  sr_ReadCursor_TITAS_TOT_CLI;
052600170530
052700170530         clear  wSQL_TITAS_ds;
052800170530
052900170530         exec sql   fetch next   from C2   into :wSQL_TITAS_ds;
053000170530
053100170530         Select;
053200170530
053300170530           // -?E.o.F.?
053400170530           When  SQLcode = 100;
053500170530             $EoF2 = *on;
053600170530
053700170530           // -?Errore?
053800170530           When  SQLcode < *zero;
053900170608             $EoF2 = *on;
054000170530             exsr  sr_PrintERR;
054100170530
054200170530           // -?Elaborazione?
054300170530           Other;
054400170530             MITpkfT += wSQL_TITAS_ds.TASpkf;
054500170530             MITvlfT += wSQL_TITAS_ds.TASvlf;
054600170530             MITnclT += wSQL_TITAS_ds.TASncl;
054700170530             MITtspT += wSQL_TITAS_ds.TAStsp;
054800170608             MITporT += wSQL_TITAS_ds.TASpor;
054900170608             MITimvT += wSQL_TITAS_ds.TASimv;
055000170530
055100170530         EndSl;
055200170530
055300170530       ENDSR;
055400170525
055500170525       //--------------------------------------------------------------
055600170525       //?Stampa segnalazione dell'errore rilevato via SQL.            ?
055700170525       //--------------------------------------------------------------
055800170525       BEGSR  sr_PrintERR;
055900170525
056000170525         // -?Stampa del Dump?
056100170525         Dump(A);
056200170525
056300170525         // -?Stampa del Job-Log?
056400170525         Qcmd = 'DSPJOBLOG job(*) output(*print)';
056500170525         exsr  sr_ExecCmd;
056600170530
056700170530         // -?Uscita dal *pgm?
056800170530         exsr  sr_RoutEnd;
056900170525
057000170525       ENDSR;
057100170525
057200170525       //--------------------------------------------------------------
057300170525       //?Operazioni finali.                                           ?
057400170525       //--------------------------------------------------------------
057500170525       BEGSR  sr_RoutEnd;
057600170525
057700170525         // -?Uscita dal *pgm?
057800170525         return;
057900170525
058000170525       ENDSR;
058100170525
058200170525       //--------------------------------------------------------------
058300170525       //?Esecuzione del comando (già impostato).                      ?
058400170525       //--------------------------------------------------------------
058500170525       BEGSR  sr_ExecCmd;
058600170525
058700170525         clear Qcap0100;
058800170525         Qcabcsdh = *off;
058900170525         Qcapa    = *off;
059000170525         Qcacmdss = *off;
059100170525         Qcaerved = *allX'00';
059200170525
059300170525         clear Qusec;
059400170525         Qusbprv  = %size(Qusec);
059500170525
059600170525         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
059700170525                           %size(Qcap0100) : 'CPOP0100' : *omit :
059800170525                           0 : 0 : Qusec);
059900170525
060000170525         //if  Qusei <> *blank;
060100170525         //  ...;
060200170525         //endif;
060300170525
060400170525       ENDSR;
060500170525
060600170525      /end-free
