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
013100170612       // -?Reperimento cliente Unificante?
013200170525      /copy gaitrasrc/srcProtoPI,TIBS10R
013300170525      /copy gaitrasrc/srcProtoPR,TIBS10R
013400170612
013500170612       // -?Controllo/Decodifica cliente?
013600170612      /copy gaitrasrc/srcProtoPI,TIBS69R
013700170612      /copy gaitrasrc/srcProtoPR,TIBS69R
013800170525
013900170525       // -?Parametri API QCAPCMD (Process Commands)?
014000170525     d Qcmd            s           2048    inz   varying
014100170525      /copy qSysInc/qRpgleSrc,QCAPCMD
014200170525       // -?API QCAPCMD (Process Commands)?
014300170525      /copy gaitrasrc/srcProtoPR,QCAPCMD
014400170525
014500170525       // -?Parametri gestione errori API.?
014600170525      /copy qsysinc/qrpglesrc,QUSEC
014700170525
014800170525       //--------------------------------------------------------------
014900170525       //?Definizione key-list.                                        ?
015000170525       //--------------------------------------------------------------
015100170525
015200170525
015300170525       //--------------------------------------------------------------
015400170525       //?Riepilogo indicatori utilizzati.                             ?
015500170525       //--------------------------------------------------------------
015600170525
015700170525
015800170525       //--------------------------------------------------------------
015900170525       //?M A I N - L I N E                                            ?
016000170525       //--------------------------------------------------------------
016100170525
016200170525     c     *Entry        plist
016300170525     c                   parm                    KPJBA
016400170525
016500170525      /free
016600170525
016700170525       // -?Operazioni iniziali?
016800170525       exsr  sr_RoutInz;
016900170525
017000170525       // -?Preparazione stringa SQL per estrazione dati?
017100170525       exsr  sr_PrepSQL;
017200170525
017300170525       // -?Apertura Cursore?
017400170525       exsr  sr_OpenCursor;
017500170525
017600170525       // -?Ciclo di elaborazione?
017700170525       DoW  $EoF = *off;
017800170525         exsr  sr_ReadCursor;
017900170525       EndDo;
018000170525
018100170525       // -?Chiusura Cursore?
018200170525       exsr  sr_CloseCursor;
018300170525
018400170525       // -?Operazioni finali?
018500170525       exsr sr_RoutEnd;
018600170525
018700170525       //--------------------------------------------------------------
018800170525       //?Operazioni iniziali.                                         ?
018900170525       //--------------------------------------------------------------
019000170525       BEGSR  sr_RoutInz;
019100170525
019200170525         // -?Impostazione opzioni per SQL?
019300170525         exec sql   set  option  DynUsrPrf = *Owner,
019400170525                                 CloSqlCsr = *EndMod;
019500170525
019600170525         // -?Impostazione chiusura?
019700170525         *inLR = *on;
019800170525
019900170525         // -?Reperimento dati job?
020000170525         exsr  sr_DatiJob;
020100170525
020200170525         // -?Reperimento parametri?
020300170525         clear  FIMS16ds;
020400170525         If  KPJBU <> *blank;
020500170525           FIMS16ds = KPJBU;
020600170525           if  %check( c_Digits : %subst( KPJBU : 1 :
020700170525                                  %size( FIMS16ds.iMS16dti ) ) )      > *zero;
020800170525             clear  FIMS16ds.iMS16dti;
020900170525           endif;
021000170525           if  %check( c_Digits : %subst( KPJBU :
021100170525                                  %size( FIMS16ds.iMS16dti ) + 1 :
021200170525                                  %size( FIMS16ds.iMS16dtf ) ) ) > *zero;
021300170525             clear  FIMS16ds.iMS16dtf;
021400170525           endif;
021500170525           if  %check( c_Digits : %subst( KPJBU :
021600170525                                  %size( FIMS16ds.iMS16dti ) +
021700170525                                  %size( FIMS16ds.iMS16dtf ) + 1 :
021800170525                                  %size( FIMS16ds.iMS16ccm ) ) ) > *zero;
021900170525             clear  FIMS16ds.iMS16ccm;
022000170525           endif;
022100170525           if  FIMS16ds.iMS16dti > *zero  and
022200170525               FIMS16ds.iMS16dtf = *zero;
022300170525             FIMS16ds.iMS16dtf = FIMS16ds.iMS16dti;
022400170525           endif;
022500170525         EndIf;
022600170609
022700170609         // -?Impostazione data limite (ultimo giorno del mese precedente)?
022800170609         //  ?per il calcolo dei Totali Incomaptibili?
022900170609         clear  wDate;
023000170609         wDate = %dec( %date() - %days( %subdt( %date() : *days ) ) );
023100170525
023200170525       ENDSR;
023300170525
023400170525       //--------------------------------------------------------------
023500170525       //?Reperimento Dati del job (Utente/Operativi).                 ?
023600170525       //--------------------------------------------------------------
023700170525       BEGSR  sr_DatiJob;
023800170525
023900170525         in(e)  §AzUte;
024000170525         if  NOT  %error;
024100170525           in(e)  §DatiUte;
024200170525         endif;
024300170525         if  %error  or  RSut = *blank;
024400170525           tibs34r ( tibs34ds );
024500170525           in  §AzUte;
024600170525           in  §DatiUte;
024700170525         endif;
024800170525
024900170525       ENDSR;
025000170525
025100170525       //--------------------------------------------------------------
025200170525       //?Preparazione stringa SQL - per estrazione dati da FIMIS.     ?
025300170525       //--------------------------------------------------------------
025400170525       BEGSR  sr_PrepSQL;
025500170525
025600170531         wSQL  = 'select MISaas, MISmgs, MIStfp, MISccm, +
025700170526                         sum( MISpkf ), sum( MISvlf ), sum( MISncl ), +
025800170609                         sum( MISnsiP ), sum( MISnsiA ), count(*), +
025900170609                         sum( MISpor ), sum( MISimv ) +
026000170608                    from FIMIS00F +
026100170608                   where MIStbl in (''F1'', ''A2'')';
026200170525
026300170525         if  ( FIMS16ds.iMS16dti + FIMS16ds.iMS16dtf ) > *zero;
026400170525
026500170608           wSQL += ' and ( MISaas * 10000 ) + MISmgs between '
026600170525                       + %editc( FIMS16ds.iMS16dti : 'X' ) + ' and '
026700170525                       + %editc( FIMS16ds.iMS16dtf : 'X' );
026800170525
026900170525         endif;
027000170525
027100170525         if  FIMS16ds.iMS16ccm > *zero;
027200170525
027300170608           wSQL += ' and MISccm = ' + %editc( FIMS16ds.iMS16ccm : 'X' );
027400170525
027500170525         endif;
027600170525
027700170531         wSQL += ' group by MISaas, MISmgs, MIStfp, MISccm +
027800170531                   order by MISaas, MISmgs, MIStfp, MISccm +
027900170525                     for fetch only';
028000170525
028100170525       ENDSR;
028200170525
028300170525       //--------------------------------------------------------------
028400170525       //?Apertura cursore.                                            ?
028500170525       //--------------------------------------------------------------
028600170525       BEGSR  sr_OpenCursor;
028700170525
028800170525         $EoF  = *off;
028900170525
029000170525         // -?Dichiarazione cursore?
029100170525         exec sql   prepare S1   from :wSQL;
029200170525         exec sql   declare C1   cursor for S1;
029300170525
029400170525         if  SQLcode < *zero;
029500170525           $EoF  = *on;
029600170525           exsr  sr_PrintERR;
029700170525           leavesr;
029800170525         endif;
029900170525
030000170525         // -?Apertura del cursore?
030100170525         exec sql   open C1;
030200170525
030300170525         if  SQLcode < *zero;
030400170525           $EoF  = *on;
030500170525           exsr  sr_PrintERR;
030600170525           leavesr;
030700170525         endif;
030800170525
030900170525       ENDSR;
031000170525
031100170525       //--------------------------------------------------------------
031200170525       //?Chiusura cursore.                                            ?
031300170525       //--------------------------------------------------------------
031400170525       BEGSR  sr_CloseCursor;
031500170525
031600170525         // -?Chiusura del cursore?
031700170525         exec sql   close C1;
031800170525
031900170525       ENDSR;
032000170525
032100170525       //--------------------------------------------------------------
032200170525       //?Lettura cursore.                                             ?
032300170525       //--------------------------------------------------------------
032400170525       BEGSR  sr_ReadCursor;
032500170525
032600170525         clear  wSQL_FIMIS_ds;
032700170525
032800170525         exec sql   fetch next   from C1   into :wSQL_FIMIS_ds;
032900170525
033000170525         Select;
033100170525
033200170525           // -?E.o.F.?
033300170525           When  SQLcode = 100;
033400170525             $EoF = *on;
033500170525
033600170525           // -?Errore?
033700170525           When  SQLcode < *zero;
033800170525             $EoF = *on;
033900170525             exsr  sr_PrintERR;
034000170525
034100170525           // -?Elaborazione?
034200170525           Other;
034300170525             exsr  sr_Write_WFMIT;
034400170525
034500170525         EndSl;
034600170525
034700170525       ENDSR;
034800170525
034900170525       //--------------------------------------------------------------
035000170525       //?Elaborazione totale di FIMS00F e scrittura rec. di WFMIT00F. ?
035100170525       //--------------------------------------------------------------
035200170525       BEGSR  sr_Write_WFMIT;
035300170525
035400170525         clear  WFMIT000;
035500170525
035600170530         MITaas  = wSQL_FIMIS_ds.MISaas;
035700170530         MITmgs  = wSQL_FIMIS_ds.MISmgs;
035800170531         MITtfp  = wSQL_FIMIS_ds.MIStfp;
035900170530         MITccm  = wSQL_FIMIS_ds.MISccm;
036000170530         //*·MITctr  = wSQL_FIMIS_ds.MISctr;
036100170530         MITpkfI = wSQL_FIMIS_ds.MISpkf;
036200170530         MITvlfI = wSQL_FIMIS_ds.MISvlf;
036300170530         MITnclI = wSQL_FIMIS_ds.MISncl;
036400170530         MITtspI = wSQL_FIMIS_ds.MIStsp;
036500170530         MITnsiP = wSQL_FIMIS_ds.MISnsiP;
036600170530         MITnsiA = wSQL_FIMIS_ds.MISnsiA;
036700170609         MITporI = wSQL_FIMIS_ds.MISpor;
036800170609         MITimvI = wSQL_FIMIS_ds.MISimv;
036900170525
037000170525         // -?Reperimento codice cliente Unificante?
037100170525         clear  TIBS10ds;
037200170525         TIBS10ds.D10drf = ( wSQL_FIMIS_ds.MISaas * 10000 ) +
037300170525                             wSQL_FIMIS_ds.MISmgs;
037400170525         TIBS10ds.D10tle = 'ST';
037500170525         TIBS10ds.D10paf = 'P';   //?Verifica se è figlio e ne reperisce il padre?
037600170525         TIBS10ds.D10cod = wSQL_FIMIS_ds.MISccm;
037700170525         GetKSCfamily ( TIBS10ds : 'R' );
037800170525         if  TIBS10ds.D10err = *blank  and
037900170525             TIBS10ds.D10cop > *zero;
038000170525           MITksu = TIBS10ds.D10cop;
038100170525         else;
038200170525           MITksu = TIBS10ds.D10cod;
038300170525         endif;
038400170612
038500170612         // -?Decodifica cliente Mittente?
038600170612         clear  TIBS69ds;
038700170612         clear  ds_CNACO;
038800170612         clear  ds_CNIND;
038900170612         clear  ds_CNCLP;
039000170612         clear  ds_FNCLS;
039100170612         i69kac = wSQL_FIMIS_ds.MISccm;
039200170612         tibs69r ( TIBS69ds :
039300170612                   ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
039400170612         if  O69err = *blank;
039500170612           MITrsm  = ACOrag;
039600170612         endif;
039700170530
039800170530         // -?Reperimento totali da TITAS00F/10F/P0F?
039900170530         exsr  sr_Elab_TITAS;
040000170525
040100170525         // -?Scrittura record in WFMIT00F?
040200170525         //______________
040300170525         write  WFMIT000;
040400170525         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
040500170525
040600170525       ENDSR;
040700170530
040800170530       //--------------------------------------------------------------
040900170530       //?Estrazione totale Spedizioni per giorno/cliente da TITAS.    ?
041000170530       //--------------------------------------------------------------
041100170530       BEGSR  sr_Elab_TITAS;
041200170530
041300170608         // -? Calcolo TOTALI x CLIENTE ?
041400170608
041500170608         // -?Preparazione stringa SQL per estrazione?
041600170608         exsr  sr_PrepSQL_TITAS_TOT_CLI;
041700170530
041800170530         // -?Apertura Cursore?
041900170608         exsr  sr_OpenCursor_TITAS_TOT_CLI;
042000170530
042100170530         // -?Ciclo di elaborazione?
042200170530         DoW  $EoF2 = *off;
042300170608           exsr  sr_ReadCursor_TITAS_TOT_CLI;
042400170530         EndDo;
042500170530
042600170530         // -?Chiusura Cursore?
042700170608         exsr  sr_CloseCursor_TITAS_TOT_CLI;
042800170530
042900170530       ENDSR;
043000170530
043100170530       //--------------------------------------------------------------
043200170530       //?Preparazione stringa SQL per estrazione totali da TITAS.     ?
043300170530       //--------------------------------------------------------------
043400170608       BEGSR  sr_PrepSQL_TITAS_TOT_CLI;
043500170530
043600170530         wSQL2 = 'with TITASP as ( +
043700170531                       select TASaas, TASmgs, TAStfp, TASccm, +
043800170608                              TASpkf, TASvlf, TASncl, TASpor, TASimv +
043900170530                         from TITASP0F +
044000170530                        where TASccm = ' + %trim(
044100170530                              %editc( wSQL_FIMIS_ds.MISccm : '3' ) ) +
044200170530                        ' and TASaas = ' + %trim(
044300170530                              %editc( wSQL_FIMIS_ds.MISaas : '3' ) ) +
044400170530                        ' and TASmgs = ' + %trim(
044500170530                              %editc( wSQL_FIMIS_ds.MISmgs : '3' ) ) +
044600170531                        ' and TAStfp = ' + %trim(
044700170531                              %editc( wSQL_FIMIS_ds.MIStfp : '3' ) ) +
044800170530                        ' and TAStbl in (''F1'', ''A2'') +
044900170530                                 ), +
045000170530                       TITAS1 as ( +
045100170531                       select TASaas, TASmgs, TAStfp, TASccm, +
045200170608                              TASpkf, TASvlf, TASncl, TASpor, TASimv +
045300170530                         from TITAS10F +
045400170530                        where TASccm = ' + %trim(
045500170530                              %editc( wSQL_FIMIS_ds.MISccm : '3' ) ) +
045600170530                        ' and TASaas = ' + %trim(
045700170530                              %editc( wSQL_FIMIS_ds.MISaas : '3' ) ) +
045800170530                        ' and TASmgs = ' + %trim(
045900170530                              %editc( wSQL_FIMIS_ds.MISmgs : '3' ) ) +
046000170531                        ' and TAStfp = ' + %trim(
046100170531                              %editc( wSQL_FIMIS_ds.MIStfp : '3' ) ) +
046200170530                        ' and TAStbl in (''F1'', ''A2'') +
046300170530                                 ), +
046400170530                       TITAS0 as ( +
046500170531                       select TASaas, TASmgs, TAStfp, TASccm, +
046600170608                              TASpkf, TASvlf, TASncl, TASpor, TASimv +
046700170530                         from TITAS00F +
046800170530                        where TASccm = ' + %trim(
046900170530                              %editc( wSQL_FIMIS_ds.MISccm : '3' ) ) +
047000170530                        ' and TASaas = ' + %trim(
047100170530                              %editc( wSQL_FIMIS_ds.MISaas : '3' ) ) +
047200170530                        ' and TASmgs = ' + %trim(
047300170530                              %editc( wSQL_FIMIS_ds.MISmgs : '3' ) ) +
047400170531                        ' and TAStfp = ' + %trim(
047500170531                              %editc( wSQL_FIMIS_ds.MIStfp : '3' ) ) +
047600170530                        ' and TAStbl in (''F1'', ''A2'') +
047700170530                                 ) +
047800170530
047900170531                  select TASaas, TASmgs, TAStfp, TASccm, +
048000170530                         sum( TASpkf ), sum( TASvlf ), sum( TASncl ), +
048100170608                         count(*), sum( TASpor ), sum( TASimv ) +
048200170530                    from TITASP +
048300170531                group by TASccm, TASaas, TAStfp, TASmgs +
048400170530                   union +
048500170531                  select TASaas, TASmgs, TAStfp, TASccm, +
048600170530                         sum( TASpkf ), sum( TASvlf ), sum( TASncl ), +
048700170608                         count(*), sum( TASpor ), sum( TASimv ) +
048800170530                    from TITAS1 +
048900170531                group by TASccm, TASaas, TAStfp, TASmgs +
049000170530                   union +
049100170531                  select TASaas, TASmgs, TAStfp, TASccm, +
049200170530                         sum( TASpkf ), sum( TASvlf ), sum( TASncl ), +
049300170608                         count(*), sum( TASpor ), sum( TASimv ) +
049400170530                    from TITAS0 +
049500170531                group by TASccm, TASaas, TAStfp, TASmgs +
049600170530
049700170530               for fetch only';
049800170530
049900170530       ENDSR;
050000170530
050100170530       //--------------------------------------------------------------
050200170530       //?Apertura cursore.                                            ?
050300170530       //--------------------------------------------------------------
050400170608       BEGSR  sr_OpenCursor_TITAS_TOT_CLI;
050500170530
050600170530         $EoF2 = *off;
050700170530
050800170530         // -?Dichiarazione cursore?
050900170530         exec sql   prepare S2   from :wSQL2;
051000170530         exec sql   declare C2   cursor for S2;
051100170530
051200170530         if  SQLcode < *zero;
051300170530           $EoF2 = *on;
051400170530           exsr  sr_PrintERR;
051500170530           leavesr;
051600170530         endif;
051700170530
051800170530         // -?Apertura del cursore?
051900170530         exec sql   open C2;
052000170530
052100170530         if  SQLcode < *zero;
052200170530           $EoF2 = *on;
052300170530           exsr  sr_PrintERR;
052400170530           leavesr;
052500170530         endif;
052600170530
052700170530       ENDSR;
052800170530
052900170530       //--------------------------------------------------------------
053000170530       //?Chiusura cursore.                                            ?
053100170530       //--------------------------------------------------------------
053200170608       BEGSR  sr_CloseCursor_TITAS_TOT_CLI;
053300170530
053400170530         // -?Chiusura del cursore?
053500170530         exec sql   close C2;
053600170530
053700170530       ENDSR;
053800170530
053900170530       //--------------------------------------------------------------
054000170530       //?Lettura cursore.                                             ?
054100170530       //--------------------------------------------------------------
054200170608       BEGSR  sr_ReadCursor_TITAS_TOT_CLI;
054300170530
054400170530         clear  wSQL_TITAS_ds;
054500170530
054600170530         exec sql   fetch next   from C2   into :wSQL_TITAS_ds;
054700170530
054800170530         Select;
054900170530
055000170530           // -?E.o.F.?
055100170530           When  SQLcode = 100;
055200170530             $EoF2 = *on;
055300170530
055400170530           // -?Errore?
055500170530           When  SQLcode < *zero;
055600170608             $EoF2 = *on;
055700170530             exsr  sr_PrintERR;
055800170530
055900170530           // -?Elaborazione?
056000170530           Other;
056100170530             MITpkfT += wSQL_TITAS_ds.TASpkf;
056200170530             MITvlfT += wSQL_TITAS_ds.TASvlf;
056300170530             MITnclT += wSQL_TITAS_ds.TASncl;
056400170530             MITtspT += wSQL_TITAS_ds.TAStsp;
056500170608             MITporT += wSQL_TITAS_ds.TASpor;
056600170608             MITimvT += wSQL_TITAS_ds.TASimv;
056700170530
056800170530         EndSl;
056900170530
057000170530       ENDSR;
057100170525
057200170525       //--------------------------------------------------------------
057300170525       //?Stampa segnalazione dell'errore rilevato via SQL.            ?
057400170525       //--------------------------------------------------------------
057500170525       BEGSR  sr_PrintERR;
057600170525
057700170525         // -?Stampa del Dump?
057800170525         Dump(A);
057900170525
058000170525         // -?Stampa del Job-Log?
058100170525         Qcmd = 'DSPJOBLOG job(*) output(*print)';
058200170525         exsr  sr_ExecCmd;
058300170530
058400170530         // -?Uscita dal *pgm?
058500170530         exsr  sr_RoutEnd;
058600170525
058700170525       ENDSR;
058800170525
058900170525       //--------------------------------------------------------------
059000170525       //?Operazioni finali.                                           ?
059100170525       //--------------------------------------------------------------
059200170525       BEGSR  sr_RoutEnd;
059300170525
059400170525         // -?Uscita dal *pgm?
059500170525         return;
059600170525
059700170525       ENDSR;
059800170525
059900170525       //--------------------------------------------------------------
060000170525       //?Esecuzione del comando (già impostato).                      ?
060100170525       //--------------------------------------------------------------
060200170525       BEGSR  sr_ExecCmd;
060300170525
060400170525         clear Qcap0100;
060500170525         Qcabcsdh = *off;
060600170525         Qcapa    = *off;
060700170525         Qcacmdss = *off;
060800170525         Qcaerved = *allX'00';
060900170525
061000170525         clear Qusec;
061100170525         Qusbprv  = %size(Qusec);
061200170525
061300170525         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
061400170525                           %size(Qcap0100) : 'CPOP0100' : *omit :
061500170525                           0 : 0 : Qusec);
061600170525
061700170525         //if  Qusei <> *blank;
061800170525         //  ...;
061900170525         //endif;
062000170525
062100170525       ENDSR;
062200170525
062300170525      /end-free
