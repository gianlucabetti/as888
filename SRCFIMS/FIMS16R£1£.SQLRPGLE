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
007500170525     d   misCCM                            inz   like(FIMIS00F.MISccm)
007600170525     d   misCTR                            inz   like(FIMIS00F.MISctr)
007700170525     d   misPKF                     + 4    inz   like(FIMIS00F.MISpkf)
007800170525     d   misVLF                     + 4    inz   like(FIMIS00F.MISvlf)
007900170525     d   misNCL                     + 4    inz   like(FIMIS00F.MISncl)
008000170525     d   misTSP                            inz   like(FIMIS00F.MISnsp)
008100170525
008200170525       //--------------------------------------------------------------
008300170525       //?Definizione variabili globali.                               ?
008400170525       //--------------------------------------------------------------
008500170525
008600170525       // -?Flags booleani?
008700170525     d $EoF            s               n   inz
008800170525     d $Where          s               n   inz
008900170525
009000170525       // -?Stringhe SQL da eseguire?
009100170525     d wSQL            s          32740    inz   varying
009200170525
009300170525       //--------------------------------------------------------------
009400170525       //?Definizione prototipi procedure.                             ?
009500170525       //--------------------------------------------------------------
009600170525
009700170525       // -?Reperimento dati utente?
009800170525     d TIBS34ds      e ds                  inz
009900170525      /copy gaitrasrc/srcProtoPR,TIBS34R
010000170525
010100170525       // -?Ricerca unificante padre?
010200170525      /copy gaitrasrc/srcProtoPI,TIBS10R
010300170525      /copy gaitrasrc/srcProtoPR,TIBS10R
010400170525
010500170525       // -?Parametri API QCAPCMD (Process Commands)?
010600170525     d Qcmd            s           2048    inz   varying
010700170525      /copy qSysInc/qRpgleSrc,QCAPCMD
010800170525       // -?API QCAPCMD (Process Commands)?
010900170525      /copy gaitrasrc/srcProtoPR,QCAPCMD
011000170525
011100170525       // -?Parametri gestione errori API.?
011200170525      /copy qsysinc/qrpglesrc,QUSEC
011300170525
011400170525       //--------------------------------------------------------------
011500170525       //?Definizione key-list.                                        ?
011600170525       //--------------------------------------------------------------
011700170525
011800170525
011900170525       //--------------------------------------------------------------
012000170525       //?Riepilogo indicatori utilizzati.                             ?
012100170525       //--------------------------------------------------------------
012200170525
012300170525
012400170525       //--------------------------------------------------------------
012500170525       //?M A I N - L I N E                                            ?
012600170525       //--------------------------------------------------------------
012700170525
012800170525     c     *Entry        plist
012900170525     c                   parm                    KPJBA
013000170525
013100170525      /free
013200170525
013300170525       // -?Operazioni iniziali?
013400170525       exsr  sr_RoutInz;
013500170525
013600170525       // -?Preparazione stringa SQL per estrazione dati?
013700170525       exsr  sr_PrepSQL;
013800170525
013900170525       // -?Apertura Cursore?
014000170525       exsr  sr_OpenCursor;
014100170525
014200170525       // -?Ciclo di elaborazione?
014300170525       DoW  $EoF = *off;
014400170525         exsr  sr_ReadCursor;
014500170525       EndDo;
014600170525
014700170525       // -?Chiusura Cursore?
014800170525       exsr  sr_CloseCursor;
014900170525
015000170525       // -?Operazioni finali?
015100170525       exsr sr_RoutEnd;
015200170525
015300170525       //--------------------------------------------------------------
015400170525       //?Operazioni iniziali.                                         ?
015500170525       //--------------------------------------------------------------
015600170525       BEGSR  sr_RoutInz;
015700170525
015800170525         // -?Impostazione opzioni per SQL?
015900170525         exec sql   set  option  DynUsrPrf = *Owner,
016000170525                                 CloSqlCsr = *EndMod;
016100170525
016200170525         // -?Impostazione chiusura?
016300170525         *inLR = *on;
016400170525
016500170525         // -?Reperimento dati job?
016600170525         exsr  sr_DatiJob;
016700170525
016800170525         // -?Reperimento parametri?
016900170525         clear  FIMS16ds;
017000170525         If  KPJBU <> *blank;
017100170525           FIMS16ds = KPJBU;
017200170525           if  %check( c_Digits : %subst( KPJBU : 1 :
017300170525                                  %size( FIMS16ds.iMS16dti ) ) )      > *zero;
017400170525             clear  FIMS16ds.iMS16dti;
017500170525           endif;
017600170525           if  %check( c_Digits : %subst( KPJBU :
017700170525                                  %size( FIMS16ds.iMS16dti ) + 1 :
017800170525                                  %size( FIMS16ds.iMS16dtf ) ) ) > *zero;
017900170525             clear  FIMS16ds.iMS16dtf;
018000170525           endif;
018100170525           if  %check( c_Digits : %subst( KPJBU :
018200170525                                  %size( FIMS16ds.iMS16dti ) +
018300170525                                  %size( FIMS16ds.iMS16dtf ) + 1 :
018400170525                                  %size( FIMS16ds.iMS16ccm ) ) ) > *zero;
018500170525             clear  FIMS16ds.iMS16ccm;
018600170525           endif;
018700170525           if  FIMS16ds.iMS16dti > *zero  and
018800170525               FIMS16ds.iMS16dtf = *zero;
018900170525             FIMS16ds.iMS16dtf = FIMS16ds.iMS16dti;
019000170525           endif;
019100170525         EndIf;
019200170525
019300170525       ENDSR;
019400170525
019500170525       //--------------------------------------------------------------
019600170525       //?Reperimento Dati del job (Utente/Operativi).                 ?
019700170525       //--------------------------------------------------------------
019800170525       BEGSR  sr_DatiJob;
019900170525
020000170525         in(e)  §AzUte;
020100170525         if  NOT  %error;
020200170525           in(e)  §DatiUte;
020300170525         endif;
020400170525         if  %error  or  RSut = *blank;
020500170525           tibs34r ( tibs34ds );
020600170525           in  §AzUte;
020700170525           in  §DatiUte;
020800170525         endif;
020900170525
021000170525       ENDSR;
021100170525
021200170525       //--------------------------------------------------------------
021300170525       //?Preparazione stringa SQL - per estrazione dati da FIMIS.     ?
021400170525       //--------------------------------------------------------------
021500170525       BEGSR  sr_PrepSQL;
021600170525
021700170525         clear  $Where;
021800170525
021900170525         wSQL  = 'select MISaas, MISmgs, MISccm, MISctr, +
022000170526                         sum( MISpkf ), sum( MISvlf ), sum( MISncl ), +
022100170526                         count(*) +
022200170526                    from FIMIS00F';
022300170525
022400170525         if  ( FIMS16ds.iMS16dti + FIMS16ds.iMS16dtf ) > *zero;
022500170525
022600170525           if  Not $Where;
022700170525             $Where = *on;
022800170525             wSQL  +=  ' where';
022900170525           else;
023000170525             wSQL  +=  ' and';
023100170525           endif;
023200170525
023300170525           wSQL +=     ' ( MISaas * 10000 ) + MISmgs between '
023400170525                       + %editc( FIMS16ds.iMS16dti : 'X' ) + ' and '
023500170525                       + %editc( FIMS16ds.iMS16dtf : 'X' );
023600170525
023700170525         endif;
023800170525
023900170525         if  FIMS16ds.iMS16ccm > *zero;
024000170525
024100170525           if  Not $Where;
024200170525             $Where = *on;
024300170525             wSQL  +=  ' where';
024400170525           else;
024500170525             wSQL  +=  ' and';
024600170525           endif;
024700170525
024800170525           wSQL +=     ' MISccm = ' + %editc( FIMS16ds.iMS16ccm : 'X' );
024900170525
025000170525         endif;
025100170525
025200170525         wSQL += ' group by MISaas, MISmgs, MISccm, MISctr +
025300170525                   order by MISaas, MISmgs, MISccm, MISctr +
025400170525                     for fetch only';
025500170525
025600170525       ENDSR;
025700170525
025800170525       //--------------------------------------------------------------
025900170525       //?Apertura cursore.                                            ?
026000170525       //--------------------------------------------------------------
026100170525       BEGSR  sr_OpenCursor;
026200170525
026300170525         $EoF  = *off;
026400170525
026500170525         // -?Dichiarazione cursore?
026600170525         exec sql   prepare S1   from :wSQL;
026700170525         exec sql   declare C1   cursor for S1;
026800170525
026900170525         if  SQLcode < *zero;
027000170525           $EoF  = *on;
027100170525           exsr  sr_PrintERR;
027200170525           leavesr;
027300170525         endif;
027400170525
027500170525         // -?Apertura del cursore?
027600170525         exec sql   open C1;
027700170525
027800170525         if  SQLcode < *zero;
027900170525           $EoF  = *on;
028000170525           exsr  sr_PrintERR;
028100170525           leavesr;
028200170525         endif;
028300170525
028400170525       ENDSR;
028500170525
028600170525       //--------------------------------------------------------------
028700170525       //?Chiusura cursore.                                            ?
028800170525       //--------------------------------------------------------------
028900170525       BEGSR  sr_CloseCursor;
029000170525
029100170525         // -?Chiusura del cursore?
029200170525         exec sql   close C1;
029300170525
029400170525       ENDSR;
029500170525
029600170525       //--------------------------------------------------------------
029700170525       //?Lettura cursore.                                             ?
029800170525       //--------------------------------------------------------------
029900170525       BEGSR  sr_ReadCursor;
030000170525
030100170525         clear  wSQL_FIMIS_ds;
030200170525
030300170525         exec sql   fetch next   from C1   into :wSQL_FIMIS_ds;
030400170525
030500170525         Select;
030600170525
030700170525           // -?E.o.F.?
030800170525           When  SQLcode = 100;
030900170525             $EoF = *on;
031000170525
031100170525           // -?Errore?
031200170525           When  SQLcode < *zero;
031300170525             $EoF = *on;
031400170525             exsr  sr_PrintERR;
031500170525
031600170525           // -?Elaborazione?
031700170525           Other;
031800170525             exsr  sr_Write_WFMIT;
031900170525
032000170525         EndSl;
032100170525
032200170525       ENDSR;
032300170525
032400170525       //--------------------------------------------------------------
032500170525       //?Elaborazione totale di FIMS00F e scrittura rec. di WFMIT00F. ?
032600170525       //--------------------------------------------------------------
032700170525       BEGSR  sr_Write_WFMIT;
032800170525
032900170525         clear  WFMIT000;
033000170525
033100170525         MITaas = wSQL_FIMIS_ds.MISaas;
033200170525         MITmgs = wSQL_FIMIS_ds.MISmgs;
033300170525         MITccm = wSQL_FIMIS_ds.MISccm;
033400170525         MITctr = wSQL_FIMIS_ds.MISctr;
033500170525         MITpkf = wSQL_FIMIS_ds.MISpkf;
033600170525         MITvlf = wSQL_FIMIS_ds.MISvlf;
033700170525         MITncl = wSQL_FIMIS_ds.MISncl;
033800170525         MITtsp = wSQL_FIMIS_ds.MIStsp;
033900170525
034000170525         // -?Reperimento codice cliente Unificante?
034100170525         clear  TIBS10ds;
034200170525         TIBS10ds.D10drf = ( wSQL_FIMIS_ds.MISaas * 10000 ) +
034300170525                             wSQL_FIMIS_ds.MISmgs;
034400170525         TIBS10ds.D10tle = 'ST';
034500170525         TIBS10ds.D10paf = 'P';   //?Verifica se è figlio e ne reperisce il padre?
034600170525         TIBS10ds.D10cod = wSQL_FIMIS_ds.MISccm;
034700170525         GetKSCfamily ( TIBS10ds : 'R' );
034800170525         if  TIBS10ds.D10err = *blank  and
034900170525             TIBS10ds.D10cop > *zero;
035000170525           MITksu = TIBS10ds.D10cop;
035100170525         else;
035200170525           MITksu = TIBS10ds.D10cod;
035300170525         endif;
035400170525
035500170525         // -?Scrittura record in WFMIT00F?
035600170525         //______________
035700170525         write  WFMIT000;
035800170525         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
035900170525
036000170525       ENDSR;
036100170525
036200170525       //--------------------------------------------------------------
036300170525       //?Stampa segnalazione dell'errore rilevato via SQL.            ?
036400170525       //--------------------------------------------------------------
036500170525       BEGSR  sr_PrintERR;
036600170525
036700170525         // -?Stampa del Dump?
036800170525         Dump(A);
036900170525
037000170525         // -?Stampa del Job-Log?
037100170525         Qcmd = 'DSPJOBLOG job(*) output(*print)';
037200170525         exsr  sr_ExecCmd;
037300170525
037400170525       ENDSR;
037500170525
037600170525       //--------------------------------------------------------------
037700170525       //?Operazioni finali.                                           ?
037800170525       //--------------------------------------------------------------
037900170525       BEGSR  sr_RoutEnd;
038000170525
038100170525         // -?Uscita dal *pgm?
038200170525         return;
038300170525
038400170525       ENDSR;
038500170525
038600170525       //--------------------------------------------------------------
038700170525       //?Esecuzione del comando (già impostato).                      ?
038800170525       //--------------------------------------------------------------
038900170525       BEGSR  sr_ExecCmd;
039000170525
039100170525         clear Qcap0100;
039200170525         Qcabcsdh = *off;
039300170525         Qcapa    = *off;
039400170525         Qcacmdss = *off;
039500170525         Qcaerved = *allX'00';
039600170525
039700170525         clear Qusec;
039800170525         Qusbprv  = %size(Qusec);
039900170525
040000170525         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
040100170525                           %size(Qcap0100) : 'CPOP0100' : *omit :
040200170525                           0 : 0 : Qusec);
040300170525
040400170525         //if  Qusei <> *blank;
040500170525         //  ...;
040600170525         //endif;
040700170525
040800170525       ENDSR;
040900170525
041000170525      /end-free
